use std::env;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::Path;

fn main() {
    let mut dest = open_destination_file();

    writeln!(&dest, "use crate::check::check;\n").unwrap();

    let examples_path = Path::new("../../examples/ok");
    fs::read_dir(examples_path)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| is_example(p))
        .for_each(|example| {
            let canonicalized = example.canonicalize().unwrap();
            generate_test(&mut dest, &canonicalized);
        });
    println!("cargo::rerun-if-changed=build.rs");
    println!("cargo::rerun-if-changed=../examples/");
}

fn open_destination_file() -> File {
    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("examples_tests.rs");
    std::fs::OpenOptions::new()
        .write(true)
        .truncate(true)
        .open(&dest_path)
        .unwrap()
}

fn is_example(path: &Path) -> bool {
    path.extension().unwrap() == "cml"
        && !path
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .ends_with(".hir.cml")
}

fn generate_test(dest: &mut File, path: &Path) {
    let name = path.file_stem().unwrap().to_str().unwrap();
    let absolute = path.to_string_lossy();
    writeln!(
        dest,
        "#[test]\nfn check_{name}() {{\n\tcheck(\"{absolute}\");\n}}"
    )
    .unwrap();
}
