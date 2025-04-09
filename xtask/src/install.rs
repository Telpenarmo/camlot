use std::ffi::OsStr;

use anyhow::{Context, Ok, Result};
use xshell::{cmd, Shell};

use crate::Install;

pub(crate) fn install(sh: &Shell, opts: &Install) -> Result<()> {
    let vscode_path = OsStr::new("code").to_os_string();
    let vscode_path = opts.code_path.as_ref().unwrap_or(&vscode_path);
    install_client(sh, vscode_path)?;
    Ok(())
}

fn install_client(sh: &Shell, vscode_path: &OsStr) -> Result<()> {
    let _dir = sh.push_dir("editors/vscode");

    cmd!(sh, "pnpm --version")
        .run()
        .context("`pnpm` is required to build the VS Code plugin")?;

    cmd!(sh, "pnpm install").run()?;
    cmd!(sh, "pnpm run package").run()?;

    cmd!(sh, "{vscode_path}")
        .arg("--install-extension")
        .arg("out/camlot-analyzer.vsix")
        .run()?;

    Ok(())
}
