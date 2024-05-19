use std::{env, path::PathBuf};

use anyhow::{bail, Result};
use xshell::Shell;

mod install;
mod sourcegen;

fn main() -> Result<()> {
    let args = std::env::args().collect::<Vec<_>>();

    let mode = Mode::from_args(&args)?;

    let sh = Shell::new()?;
    sh.change_dir(project_root());

    match mode {
        Mode::Generate => sourcegen::generate_ungrammar(),
        Mode::Install => install::install(&sh),
    }
}

enum Mode {
    Generate,
    Install,
}

impl Mode {
    fn from_args(args: &[String]) -> Result<Self> {
        if args.len() == 1 || args.len() > 2 {
            bail!("Usage: xtask <generate|install>")
        } else if args[1] == "generate" {
            Ok(Mode::Generate)
        } else if args[1] == "install" {
            Ok(Mode::Install)
        } else {
            bail!("Usage: xtask <generate|install>")
        }
    }
}

fn project_root() -> PathBuf {
    let dir =
        env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_owned());
    PathBuf::from(dir).parent().unwrap().to_owned()
}
