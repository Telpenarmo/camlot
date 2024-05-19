use anyhow::{Context, Ok, Result};
use xshell::{cmd, Shell};

pub(crate) fn install(sh: &Shell) -> Result<()> {
    install_server(sh)?;
    install_client(sh)?;
    Ok(())
}

fn install_server(sh: &Shell) -> Result<()> {
    let cmd = cmd!(sh, "cargo install --path crates/server --locked");
    cmd.run()?;
    Ok(())
}

fn install_client(sh: &Shell) -> Result<()> {
    let _dir = sh.push_dir("editors/vscode");

    cmd!(sh, "pnpm --version")
        .run()
        .context("`pnpm` is required to build the VS Code plugin")?;

    cmd!(sh, "pnpm install").run()?;
    cmd!(sh, "pnpm run package").run()?;

    cmd!(sh, "code --install-extension out/rideml-analyzer.vsix").run()?;

    Ok(())
}
