use std::env;
use std::path::{Path, PathBuf};

enum BuildMode
{
  Dynamic(pkg_config::Library),
  Static,
}

fn determine_mode() -> BuildMode
{
  if cfg!(feature = "static") {
    BuildMode::Static
  } else {
    let lib = pkg_config::Config::new().exactly_version("5.4.1").probe("lua");
    if lib.is_err() {
      BuildMode::Static
    } else {
      BuildMode::Dynamic(lib.unwrap())
    }
  }
}

fn generate_bindings(include: &Path, out: &Path)
{
  bindgen::builder()
    .header(include.join("luaconf.h").to_str().unwrap())
    .header(include.join("lua.h").to_str().unwrap())
    .header(include.join("lualib.h").to_str().unwrap())
    .header(include.join("lauxlib.h").to_str().unwrap())
    .whitelist_var("[Ll][Uu][Aa].*")
    .whitelist_type("[Ll][Uu][Aa].*")
    .whitelist_function("[Ll][Uu][Aa].*")
    .generate()
    .unwrap()
    .write_to_file(out.join("bindings.rs"))
    .unwrap();
}

fn main()
{
  let out: PathBuf = env::var("OUT_DIR").unwrap().parse().unwrap();
  let include = match determine_mode() {
    BuildMode::Dynamic(lib) => lib.include_paths[0].clone(),
    BuildMode::Static => {
      let build = lua_src::Build::new(&out);
      build.download();
      let artifact = build.build();
      artifact.print_cargo_metadata();
      artifact.include_dir().to_path_buf()
    }
  };
  generate_bindings(&include, &out);
}
