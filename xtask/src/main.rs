#![allow(clippy::missing_errors_doc)]
#![allow(clippy::must_use_candidate)]

use std::{env, ffi::OsString, path::PathBuf};

use anyhow::Result;
use xshell::Shell;

mod install;
mod sourcegen;
mod wasm;

fn main() -> Result<()> {
    let mode = App::from_env()?.subcommand;

    let sh = Shell::new()?;
    sh.change_dir(project_root());

    match mode {
        AppCmd::Generate(_) => sourcegen::generate_ungrammar(),
        AppCmd::Install(opts) => install::install(&sh, &opts),
        AppCmd::BuildWasm(opts) => wasm::build(&sh, &opts),
    }
}

xflags::xflags! {
    src "./src/main.rs"
    cmd app {
        /// Generate AST from ungrammar definition
        cmd generate {}
        /// Install the language server and vscode extension
        cmd install {
            /// The path to the vscode executable, defaults to 'code'
            optional --code-path path: OsString
        }
        cmd build-wasm {
            optional --release
            optional --debug
        }
    }
}
// generated start
// The following code is generated by `xflags` macro.
// Run `env UPDATE_XFLAGS=1 cargo build` to regenerate.
#[derive(Debug)]
pub struct App {
    pub subcommand: AppCmd,
}

#[derive(Debug)]
pub enum AppCmd {
    Generate(Generate),
    Install(Install),
    BuildWasm(BuildWasm),
}

#[derive(Debug)]
pub struct Generate;

#[derive(Debug)]
pub struct Install {
    pub code_path: Option<OsString>,
}

#[derive(Debug)]
pub struct BuildWasm {
    pub release: bool,
    pub debug: bool,
}

impl App {
    #[allow(dead_code)]
    pub fn from_env_or_exit() -> Self {
        Self::from_env_or_exit_()
    }

    #[allow(dead_code)]
    pub fn from_env() -> xflags::Result<Self> {
        Self::from_env_()
    }

    #[allow(dead_code)]
    pub fn from_vec(args: Vec<std::ffi::OsString>) -> xflags::Result<Self> {
        Self::from_vec_(args)
    }
}
// generated end

fn project_root() -> PathBuf {
    let dir =
        env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_owned());
    PathBuf::from(dir).parent().unwrap().to_owned()
}
