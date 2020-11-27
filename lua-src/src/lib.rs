use std::fs;
use std::io::Cursor;
use std::path::{Path, PathBuf};
use std::process::Command;

pub const LUA_VERSION: &str = "5.4.1";
pub const LUA_URL: &str = "https://www.lua.org/ftp/lua-5.4.1.tar.gz";
pub const LUA_TAR: &str = "lua-5.4.1.tar.gz";

pub const LUA_TAR_SHA1: &str = "88961e7d4fda58ca2c6163938fd48db8880e803d";
pub const LUA_TAR_MD1: &str = "1d575faef1c907292edd79e7a2784d30";

#[derive(Debug)]
pub struct Artifacts
{
  include_dir: PathBuf,
  lib_dir: PathBuf,
  lib: String,
}

fn download(url: &str) -> Vec<u8>
{
  let mut handle = curl::easy::Easy::new();
  let mut buf = Vec::new();
  handle.url(url).unwrap();
  handle.fail_on_error(true).unwrap();
  {
    let mut transfer = handle.transfer();
    transfer
      .write_function(|data| {
        buf.extend_from_slice(data);
        Ok(data.len())
      })
      .unwrap();
    transfer.perform().unwrap();
  }
  buf
}

fn verify(buf: &[u8], sha1: &str, md1: &str) -> bool
{
  let buf_sha1 = hex::encode(openssl::sha::sha1(&buf));
  let buf_md1 = hex::encode(openssl::hash::hash(openssl::hash::MessageDigest::md5(), &buf).unwrap());
  buf_sha1.eq(sha1) && buf_md1.eq(md1)
}

pub struct Build
{
  out_dir: PathBuf,
  tar_file: PathBuf,
  src_dir: PathBuf,
  artifact_dir: PathBuf,
}

impl Build
{
  pub fn new<P: AsRef<Path>>(out_dir: P) -> Self
  {
    Self {
      out_dir: out_dir.as_ref().to_path_buf(),
      tar_file: out_dir.as_ref().join(LUA_TAR),
      src_dir: out_dir.as_ref().join(format!("lua-{}", LUA_VERSION)),
      artifact_dir: out_dir.as_ref().join("lua-artifacts"),
    }
  }

  pub fn download(&self)
  {
    if !self.out_dir.is_dir() {
      fs::create_dir(&self.out_dir).unwrap();
    }
    let exists = self.tar_file.is_file();
    let buf = if exists {
      fs::read(&self.tar_file).unwrap()
    } else {
      download(LUA_URL)
    };
    if !verify(&buf, LUA_TAR_SHA1, LUA_TAR_MD1) {
      panic!("Lua tar file couldn't be verified.");
    }
    if !exists {
      fs::write(&self.tar_file, &buf).unwrap();
    }
    if self.src_dir.is_dir() {
      fs::remove_dir_all(&self.src_dir).unwrap();
    }
    let decompressed = flate2::read::GzDecoder::new(Cursor::new(&buf));
    tar::Archive::new(decompressed).unpack(&self.out_dir).unwrap();
  }

  pub fn build(&self) -> Artifacts
  {
    if !self.artifact_dir.is_dir() {
      fs::create_dir(&self.artifact_dir).unwrap();
    }
    let artifact_dir = self.artifact_dir.canonicalize().unwrap();

    let mut make = Command::new("make");

    make.arg("all");
    make.arg("test");
    make.arg("install");
    make.arg(format!("INSTALL_TOP={}", artifact_dir.display()));

    make.current_dir(&self.src_dir);

    assert!(make.status().unwrap().success());

    Artifacts {
      include_dir: artifact_dir.join("include"),
      lib_dir: artifact_dir.join("lib"),
      lib: String::from("lua"),
    }
  }
}

impl Artifacts
{
  pub fn include_dir(&self) -> &Path
  {
    &self.include_dir
  }

  pub fn lib_dir(&self) -> &Path
  {
    &self.lib_dir
  }

  pub fn lib(&self) -> &str
  {
    &self.lib
  }

  pub fn print_cargo_metadata(&self)
  {
    println!("cargo:rustc-link-search=native={}", self.lib_dir.display());
    println!("cargo:rustc-link-lib=static={}", self.lib);
    println!("cargo:include={}", self.include_dir.display());
    println!("cargo:lib={}", self.lib_dir.display());
  }
}
