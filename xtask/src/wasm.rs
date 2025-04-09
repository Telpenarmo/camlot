use std::path::Path;

use anyhow::Result;
use xshell::{cmd, Shell};

use crate::BuildWasm;

pub(crate) fn build(sh: &Shell, opts: &BuildWasm) -> Result<()> {
    let profile = if opts.release { "release-wasm" } else { "dev" };
    let target_triple = "wasm32-wasip1-threads";
    let target_dir = "target/";
    cmd!(sh, "cargo build")
        .arg("--profile")
        .arg(profile)
        .arg("--target")
        .arg(target_triple)
        .arg("--target-dir")
        .arg(target_dir)
        .arg("--bin")
        .arg("camlot-server")
        .env(
            "RUSTFLAGS",
            "-Clink-arg=--initial-memory=10485760 -Clink-arg=--max-memory=10485760",
        )
        .run()?;

    let profile_dir = if opts.release {
        profile
    } else {
        "debug"
    };

    let artifact_path = Path::new(target_dir)
        .join(target_triple)
        .join(profile_dir)
        .join("camlot-server.wasm");

    cmd!(sh, "cp")
        .arg(artifact_path)
        .arg("editors/vscode/out/")
        .run()?;
    Ok(())
}
