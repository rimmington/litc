extern crate combine;
extern crate walkdir;

use combine::{Parser, ParseResult, Stream, skip_many, satisfy, many, sep_by1};
use combine::char::{string, char, spaces, letter, alpha_num};
use std::io;
use std::path::Path;

#[derive(Debug)]
pub enum Error {
    IoError(io::Error),
    ParseError(std::path::PathBuf, String),
    GccError(),
    BadOutputError(std::string::FromUtf8Error),
    TypeMismatch(String)
}

use self::Error::*;

impl From<io::Error> for Error {
    fn from(e : io::Error) -> Self { IoError(e) }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq, Eq)]
struct ConstDef<N> {
    name : N,
    ctype : ConstType
}

type InitConstDef = ConstDef<Vec<String>>;

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub enum ConstType {
    i32
}

struct ConstModule<V> {
    name : String,
    modules : Vec<ConstModule<V>>,
    values : Vec<(ConstDef<String>, V)>
}

type InitConstModule = ConstModule<()>;
type FilledConstModule = ConstModule<String>;

impl <V> ConstModule<V> {
    fn get_or_insert_module(&mut self, idx : &[&str]) -> &mut ConstModule<V> {
        match idx.split_first() {
            Some((n, rest)) => {
                let pos = {
                    self.modules.iter().enumerate().find(|&(_, m)| m.name == *n).map(|(loc, _)| loc)
                };
                if let Some(loc) = pos {
                    self.modules.get_mut(loc).unwrap()
                } else {
                    let new = ConstModule { name : n.to_string(), modules : vec![], values : vec![] };
                    self.modules.push(new);
                    let len = {
                        self.modules.len()
                    };
                    self.modules.get_mut(len - 1).unwrap().get_or_insert_module(rest)
                }
            }
            None => self
        }
    }

    fn get_or_insert_value(&mut self, cd : ConstDef<String>, value : V) -> Result<&(ConstDef<String>, V)> {
        let pos = {
            self.values.iter().enumerate().find(|&(_, &(ref m, _))| m.name == cd.name).map(|(loc, _)| loc)
        };
        if let Some(loc) = pos {
            let found = self.values.get(loc).unwrap();
            if found.0.ctype == cd.ctype {
                Ok(found)
            } else {
                Err(TypeMismatch(cd.name))
            }
        } else {
            self.values.push((cd, value));
            let len = {
                self.values.len()
            };
            Ok(self.values.get(len - 1).unwrap())
        }
    }

    fn cata<F, R>(self, f : &mut F) -> R where F : FnMut(&[String], String, Vec<R>, Vec<(ConstDef<String>, V)>) -> R {
        self.cata_(&vec![], f)
    }

    fn cata_<F, R>(self, ctx : &Vec<String>, mut f : &mut F) -> R where F : FnMut(&[String], String, Vec<R>, Vec<(ConstDef<String>, V)>) -> R {
        let mut nctx = ctx.clone();
        let c = self.name.clone();
        nctx.push(c);
        let mr = self.modules.into_iter().map(|m| m.cata_(&nctx, f)).collect();
        f(ctx.as_slice(), self.name, mr, self.values)
    }
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

fn const_def<'a, S>() -> Box<Parser<Input=S, Output=InitConstDef> + 'a> where S : Stream<Item=char> + 'a {
    string("const").with(spaces()).with(ident()).with(spaces()).with(char(':')).with(spaces())
        .with(ctype())
    .and(spaces().with(char('=')).with(spaces()).with(string("magic::"))
        .with(sep_by1(ident(), string("::"))))
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

fn next_const<I>(input : I) -> ParseResult<InitConstDef, I> where I : Stream<Item=char> {
    let mut rest = input;
    next_line_matching!(const_def(), rest)
}

fn file_consts<R>(path : &std::path::Path) -> Result<R> where R : std::iter::FromIterator<InitConstDef> {
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

fn write(p : &Path, s : &str) -> Result<()> {
    use std::io::Write;
    std::fs::File::create(p).and_then(|mut f| f.write_all(s.as_bytes())).map_err(IoError)
}

fn get_values(cm : InitConstModule, out_dir : &Path) -> Result<FilledConstModule> {
    let c_src = out_dir.join(Path::new("test.c"));
    let c_out = out_dir.join(Path::new("test.out"));
    cm.cata(&mut |ctx, name, acc : Vec<Result<FilledConstModule>>, ns| {
        let ac = try!(acc.into_iter().collect());
        if ns.len() == 0 {
            return Ok(ConstModule { name : name, modules : ac, values : vec![] })
        }

        let import = if ctx.len() == 0 {
            "".to_owned()
        } else {
            let proper_ctx : Vec<String> = ctx.split_first().unwrap().1.iter().chain(Some(&name)).map(|s| s.to_owned()).collect();
            format!("#include <{}.h>", proper_ctx.join("/"))
        };
        let reqs : Vec<String> = ns.iter().map(|&(ref cd, _)| format!("printf(\"%d\", {});", &cd.name)).collect();
        // TODO: cast to type?
        let src = format!("// Autogenerated
#include <stdio.h>
{}
int main() {{
{}
}}
", import, reqs.join("\n"));
        try!(write(&c_src, src.as_str()));
        if !try!(std::process::Command::new("gcc").args(&[c_src.as_ref() as &std::ffi::OsStr, "-o".as_ref(), c_out.as_ref()]).status()).success() {
            return Err(GccError());
        }
        let c_res = try!(String::from_utf8(try!(std::process::Command::new(&c_out).output()).stdout).map_err(BadOutputError));
        let vs = c_res.lines().zip(ns.into_iter()).map(|(o, (cd, _))| (cd, o.to_owned())).collect();
        Ok(ConstModule { name : name, modules : ac, values : vs })
    })
}

fn collect_modules<I>(cds : I) -> Result<InitConstModule> where I : IntoIterator<Item=InitConstDef> {
    let mut top = ConstModule { name : "magic".to_owned(), modules : vec![], values : vec![] };
    for cd in cds {
        let (vname, midx) = cd.name.split_last().unwrap();
        let cmod = top.get_or_insert_module(&*midx.iter().map(AsRef::as_ref).collect::<Vec<_>>());
        try!(cmod.get_or_insert_value(ConstDef { name : vname.to_owned(), ctype : cd.ctype }, ()));
    }
    Ok(top)
}

fn mk_rs(cm : FilledConstModule) -> String {
    cm.cata(&mut |_ctx, name, acc : Vec<String>, vs : Vec<(ConstDef<String>, String)>| {
        let submods : String = acc.into_iter().collect();
        let consts : String = vs.into_iter().map(|(cd, v)| format!("  pub const {} : {:?} = {};\n", cd.name, cd.ctype, v)).collect();
        format!("pub mod {} {{\n{}{}}}\n", name, consts, submods)
    })
}

pub fn litc<P>(src_dir : P, out_dir : P) -> Result<String> where P : AsRef<Path> + Sized {
    let consts = walkdir::WalkDir::new(src_dir).into_iter()
        .filter_map(|em| em.ok().and_then(is_rust_source).and_then(|e| file_consts(e.path()).ok()))
        .flat_map::<Vec<_>,_>(|v| v)
        .collect::<Vec<_>>();
    let cm = try!(collect_modules(consts));
    let values = try!(get_values(cm, out_dir.as_ref()));
    Ok(mk_rs(values))
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
