extern crate combine;
extern crate walkdir;

use combine::{Parser, ParseResult, Stream, skip_many, satisfy, many};
use combine::char::{string, char, spaces, letter, alpha_num};
use std::io;
use std::path::Path;

#[derive(Debug)]
pub enum Error {
    IoError(io::Error),
    ParseError(std::path::PathBuf, String),
    GccError(),
    BadOutputError(std::string::FromUtf8Error)
}

use self::Error::*;

impl From<io::Error> for Error {
    fn from(e : io::Error) -> Self { IoError(e) }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq, Eq)]
struct ConstDef {
    name : String,
    ctype : ConstType
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub enum ConstType {
    i32
}

fn file_str(path : &std::path::Path) -> Result<String> {
    use std::io::Read;
    let mut r = std::io::BufReader::new(try!(std::fs::File::open(path)));
    let mut s = String::new();
    try!(r.read_to_string(&mut s));
    Ok(s)
}

fn rest_of_line<'a, S>() -> Box<Parser<Input=S, Output=()> + 'a> where S : Stream<Item=char> + 'a {
    skip_many(satisfy(|c| c != '\n')).with(char('\n')).map(|_| ()).boxed()
}

fn ident<'a, S>() -> Box<Parser<Input=S, Output=String> + 'a> where S : Stream<Item=char> + 'a {
    letter().and(many::<Vec<_>,_>(alpha_num().or(char('_'))))
        .map(|(c, v)| Some(c).into_iter().chain(v.into_iter()).collect())
        .boxed()
}

fn ctype<'a, S>() -> Box<Parser<Input=S, Output=ConstType> + 'a> where S : Stream<Item=char> + 'a {
    string("i32").map(|_| ConstType::i32).boxed()
}

fn const_def<'a, S>() -> Box<Parser<Input=S, Output=ConstDef> + 'a> where S : Stream<Item=char> + 'a {
    string("const").with(spaces()).with(ident()).with(spaces()).with(char(':')).with(spaces())
        .with(ctype())
    .and(spaces().with(char('=')).with(spaces()).with(string("magic::"))
        .with(ident()))
    .skip(string(";"))
    .map(|(t, i)| ConstDef { name : i, ctype : t })
    .boxed()
}

macro_rules! next_line_matching {
    ( $p:expr, $rest:ident ) => {
        (|| {
            loop {
                match try!(combine::optional(combine::try($p)).parse_stream($rest)) {
                    (None, r) => {
                        $rest = r.into_inner();
                        $rest = try!(combine::try(::rest_of_line()).parse_stream($rest)).1.into_inner();
                    },
                    (Some(o), r) => return Ok((o, r))
                }
            }
        })()
    };
}

fn next_const<I>(input : I) -> ParseResult<ConstDef, I> where I : Stream<Item=char> {
    let mut rest = input;
    next_line_matching!(const_def(), rest)
}

fn file_consts<R>(path : &std::path::Path) -> Result<R> where R : std::iter::FromIterator<ConstDef> {
    let content = try!(file_str(path));
    let mut p = many(combine::parser(next_const));
    match p.parse(content.as_str()) {
        Ok((v, _remaining_input)) => Ok(v),
        Err(e) => Err(ParseError(path.to_path_buf(), format!("{}", e)))
    }
}

fn is_rust_source(e : walkdir::DirEntry) -> Option<walkdir::DirEntry> {
    if e.file_type().is_file() && e.file_name().to_str().map(|s| s.ends_with(".rs")).unwrap_or(false) {
        Some(e)
    } else {
        None
    }
}

fn mk_c<'a, I, R>(i : I) -> R where I : IntoIterator<Item=&'a ConstDef>, R : std::iter::FromIterator<String> {
    Some("#include <stdio.h>\n#include \"/vagrant/litc/myconsts.h\"\nint main() {\n".to_owned()).into_iter()
        .chain(i.into_iter().map(|cd| format!("  printf(\"%d\", {});\n", cd.name)))
        .chain(Some("}\n".to_owned()))
        .collect()
}

fn write(p : &Path, s : &str) -> Result<()> {
    use std::io::Write;
    std::fs::File::create(p).and_then(|mut f| f.write_all(s.as_bytes())).map_err(IoError)
}

fn get_values(consts : &Vec<ConstDef>, out_dir : &Path) -> Result<Vec<String>> {
    let c_src = out_dir.join(Path::new("test.c"));
    let c_out = out_dir.join(Path::new("test.out"));
    write(&c_src, String::as_str(&mk_c(consts))).unwrap();
    if !try!(std::process::Command::new("gcc").args(&[c_src.as_ref() as &std::ffi::OsStr, "-o".as_ref(), c_out.as_ref()]).status()).success() {
        return Err(GccError());
    }
    let c_res = try!(String::from_utf8(try!(std::process::Command::new(c_out).output()).stdout).map_err(BadOutputError));
    Ok(c_res.lines().map(|l| l.to_owned()).collect())
}

fn mk_rs<'a, I>(i : I) -> String where I : IntoIterator<Item=(&'a ConstDef, String)> {
    Some("mod magic {\n".to_owned()).into_iter()
        .chain(i.into_iter().map(|(cd, v)| format!("  pub const {} : {:?} = {};\n", cd.name, cd.ctype, v)))
        .chain(Some("}\n".to_owned()))
        .collect()
}

pub fn litc<P>(src_dir : P, out_dir : P) -> Result<String> where P : AsRef<Path> + Sized {
    let consts = walkdir::WalkDir::new(src_dir).into_iter()
        .filter_map(|em| em.ok().and_then(is_rust_source).and_then(|e| file_consts(e.path()).ok()))
        .flat_map::<Vec<_>,_>(|v| v)
        .collect::<Vec<_>>();
    let values = try!(get_values(&consts, out_dir.as_ref()));
    Ok(mk_rs(consts.iter().zip(values)))
}

#[cfg(test)]
mod tests {
    use combine;
    use combine::{Parser};

    fn number() -> ::ConstDef {
        ::ConstDef { name : "NUMBER".to_owned(), ctype : ::ConstType::i32 }
    }

    #[test]
    fn match_const() {
        let res = combine::parser(::next_const).parse("const NUMBER : i32 = magic::NUMBER;").unwrap().0;
        assert_eq!(res, number());
    }

    #[test]
    fn lines_matching() {
        // Rust has terrible multi-line syntax
        let input = "
a
1
b
3";
        let p = combine::parser(|input| {
            let mut rest = input;
            next_line_matching!(combine::char::digit(), rest)
        });
        let res = combine::many::<Vec<_>, _>(p).parse(input).unwrap().0;
        assert_eq!(res, vec!['1', '3'])
    }

    #[test]
    fn match_consts() {
        let input = "
const NUMBER : i32 = magic::NUMBER;
const THING : &'static str = \"hello\";
const ABRA : i32 = magic::MAGIC;";
        let expected = vec![number(), ::ConstDef { name : "MAGIC".to_owned(), ctype : ::ConstType::i32 }];
        let res = combine::many::<Vec<_>, _>(combine::parser(::next_const)).parse(input).unwrap().0;
        assert_eq!(res, expected)
    }
}
