use core::{infer, Interner, Module, ModuleAndNames, Type};
use std::{
    fs,
    path::{Path, PathBuf},
};

use expect_test::expect_file;

#[allow(unused)]
pub(crate) fn check(path: &str) {
    let text = fs::read_to_string(path).unwrap();
    check_program(&text, Path::new(path));
}

fn check_program(text: &str, path: &Path) {
    let ast = test_parsing(text, path);

    let mut types = Interner::new();
    let mut names = Interner::new();
    let mut module = Module::new(&mut names, &mut types);
    module.lower_module(&mut names, &ast.module());

    test_lowering(&module, &mut names, path);

    ensure_no_type_errors(&module, &mut names, &mut types);

    test_pretty_printing(&module, &mut names, &mut types);
}

fn test_parsing(text: &str, path: &Path) -> parser::Parse {
    let ast = parser::parse(text);
    expect_file![cst_path(path)].assert_eq(&ast.debug_tree());
    assert!(ast.errors.is_empty(), "Parse errors: {:#?}", ast.errors);
    ast
}

fn test_lowering(module: &Module, names: &mut Interner<String>, path: &Path) {
    let module_str = format!("{}", ModuleAndNames { module, names });
    expect_file![&hir_path(path)].assert_eq(&module_str);
}

fn test_pretty_printing(module: &Module, names: &mut Interner<String>, types: &mut Interner<Type>) {
    let module_str = format!("{}", ModuleAndNames { module, names });
    let reparsed = parser::parse(&module_str);
    let mut module_from_hir = Module::new(names, types);
    module_from_hir.lower_module(names, &reparsed.module());

    assert!(
        &module_from_hir == module,
        "Pretty-printed HIR does not match original HIR"
    );
}

fn ensure_no_type_errors(
    module: &Module,
    names: &mut Interner<String>,
    types: &mut Interner<Type>,
) {
    let result = infer(module, names, types);
    assert!(
        result.diagnostics.is_empty(),
        "Type errors: {:?}",
        result.diagnostics
    );
}

fn cst_path(path: &Path) -> PathBuf {
    path.with_extension("ccst")
}

fn hir_path(path: &Path) -> PathBuf {
    path.with_extension("hir.cml")
}
