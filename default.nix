{rustPlatform, linuxHeaders}:

rustPlatform.buildRustPackage {
  name = "litc";
  src = ./.;
  depsSha256 = "084rn20wrjba2yi4fpf3frvr0d1snfpazcqilhcrpcc3dh9qsmvm";

  checkPhase = "cargo test --lib"; # No exe tests

  shellHook = "unset SSL_CERT_FILE";  # https://github.com/NixOS/nixpkgs/issues/13744

  C_INCLUDE_PATH="${linuxHeaders}/include";
}
