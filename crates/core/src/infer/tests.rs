use super::{InferenceResult, TypeInference};
use crate::{hir, infer::arrow, intern::Interner, types::Type, TypeIdx};

fn infer_from_str(code: &str) -> (hir::Module, Interner<Type>, InferenceResult) {
    let module_ast = parser::parse(code).module();
    let mut types = Interner::new();
    let mut module = hir::Module::new(&mut types);
    let infer = TypeInference::new();
    module.lower_module(&module_ast);
    let result = infer.infer(&module, &mut types);
    (module, types, result)
}

#[track_caller]
#[inline]
fn assert_empty<T: std::fmt::Debug>(vec: &[T]) {
    assert!(vec.is_empty(), "{vec:?}");
}

#[track_caller]
#[inline]
fn get_first_defn_types(module: &hir::Module, result: &InferenceResult) -> (TypeIdx, TypeIdx) {
    let (idx, defn) = module.iter_definitions().next().unwrap();
    (result.defn_types[idx], result.expr_types[defn.defn])
}

#[track_caller]
#[inline]
fn assert_types_eq(actual: TypeIdx, expected: TypeIdx, types: &Interner<Type>) {
    assert_eq!(types.lookup(actual), types.lookup(expected), "{types:?}");
}

#[test]
fn test_infer_def_with_annotated_param() {
    let (module, mut types, result) = infer_from_str("def f(x: int) = x;");

    assert_empty(&result.diagnostics);

    let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

    let int = types.intern(Type::Int);
    let expected = arrow(&mut types, int, int);

    assert_types_eq(actual_defn, expected, &types);
    assert_types_eq(actual_body, int, &types);
}

#[test]
fn test_infer_def_lambda_with_annotated_param() {
    let (module, mut types, result) = infer_from_str("def f = \\(x: int) -> x;");

    assert_empty(&result.diagnostics);

    let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

    let int = types.intern(Type::Int);
    let expected = arrow(&mut types, int, int);

    assert_types_eq(actual_defn, expected, &types);
    assert_types_eq(actual_body, expected, &types);
}

#[test]
fn test_infer_def_lambda_with_return_type() {
    let (module, mut types, result) = infer_from_str("def f = \\x : int -> x;");

    assert_empty(&result.diagnostics);

    let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

    let int = types.intern(Type::Int);
    let expected = arrow(&mut types, int, int);

    assert_types_eq(actual_defn, expected, &types);
    assert_types_eq(actual_body, expected, &types);
}

#[test]
fn test_infer_annotated_def_lambda() {
    let (module, mut types, result) = infer_from_str("def f : int -> int = \\x -> x;");

    assert_empty(&result.diagnostics);

    let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

    let int = types.intern(Type::Int);
    let expected = arrow(&mut types, int, int);

    assert_types_eq(actual_defn, expected, &types);
    assert_types_eq(actual_body, expected, &types);
}

#[test]
fn test_infer_unannotated_id() {
    let (module, types, result) = infer_from_str("def f x = x;");

    assert_empty(&result.diagnostics);

    let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

    assert!(
        matches!(types.lookup(actual_defn), Type::Arrow(from, to) if matches!(types.lookup(*from), Type::Unifier(_)) && matches!(types.lookup(*to), Type::Unifier(_))),
        "actual: {actual_defn:?}\n{types:?}"
    );
    assert!(
        matches!(types.lookup(actual_body), Type::Unifier(_)),
        "actual: {actual_body:?}\n{types:?}"
    );
}

#[test]
fn test_infer_empty_def() {
    let (module, mut types, result) = infer_from_str("def f { }");

    assert_empty(&result.diagnostics);

    let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

    let unit = types.intern(Type::Unit);

    assert_types_eq(actual_defn, unit, &types);
    assert_types_eq(actual_body, unit, &types);
}

#[test]
fn test_infer_empty_def_with_annotated_param() {
    let (module, mut types, result) = infer_from_str("def f (x: int) { }");

    assert_empty(&result.diagnostics);

    let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

    let unit = types.intern(Type::Unit);
    let int = types.intern(Type::Int);

    assert_types_eq(actual_body, unit, &types);
    assert_types_eq(actual_defn, arrow(&mut types, int, unit), &types);
}

#[test]
fn test_infer_block_returning_let() {
    let (module, mut types, result) = infer_from_str("def f (x: int) { let y = x; y }");

    assert_empty(&result.diagnostics);

    let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

    let int = types.intern(Type::Int);

    assert_types_eq(actual_defn, arrow(&mut types, int, int), &types);
    assert_types_eq(actual_body, int, &types);
}

#[test]
fn infer_endless_recursion() {
    let (module, types, result) = infer_from_str("def f = f;");

    assert_empty(&result.diagnostics);

    let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

    assert!(
        matches!(types.lookup(actual_body), Type::Unifier(_)),
        "actual: {:?}\n{types:?}",
        types.lookup(actual_body)
    );
    assert!(
        matches!(types.lookup(actual_defn), Type::Unifier(_)),
        "actual: {:?}\n{types:?}",
        types.lookup(actual_defn)
    );
}

#[test]
fn test_infer_annotated_int() {
    let (module, mut types, result) = infer_from_str("def f : int = 1;");

    assert_empty(&result.diagnostics);

    let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

    let int = types.intern(Type::Int);

    assert_types_eq(actual_defn, int, &types);
    assert_types_eq(actual_body, int, &types);
}

#[test]
fn infer_application() {
    let (module, mut types, result) = infer_from_str("def f (y: int) = ((\\x -> x) y));");

    assert_empty(&result.diagnostics);

    let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

    let int = types.intern(Type::Int);

    assert_types_eq(actual_defn, arrow(&mut types, int, int), &types);
    assert_types_eq(actual_body, int, &types);
}

#[test]
fn test_infer_int_by_using_add() {
    let (module, mut types, result) = infer_from_str("def f x = (add x x);");

    assert_empty(&result.diagnostics);

    let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

    let int = types.intern(Type::Int);
    let expected = arrow(&mut types, int, int);

    assert_types_eq(actual_defn, expected, &types);
    assert_types_eq(actual_body, int, &types);
}

#[test]
fn type_aliases_are_associative() {
    let (module, mut types, result) = infer_from_str("type A = int; type B = A; def i: B = 1;");

    assert_empty(&result.diagnostics);

    let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

    let int = types.intern(Type::Int);

    assert_types_eq(actual_defn, int, &types);
    assert_types_eq(actual_body, int, &types);
}

#[test]
fn types_declarations_are_order_sensitive() {
    let (_module, _types, result) = infer_from_str("type A = B; type B = int; def i: A = 1;");

    assert_eq!(result.diagnostics.len(), 2);

    assert!(matches!(
        &result.diagnostics[0],
        &crate::TypeError::UnboundTypeVariable { .. }
    ));

    assert!(matches!(
        &result.diagnostics[1],
        &crate::TypeError::TypeMismatch { .. }
    ));
}

#[test]
fn test_infer_let_function_with_return_type() {
    let (module, mut types, result) = infer_from_str("def f { let f y : int = y; f }");

    assert_empty(&result.diagnostics);

    let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

    let int = types.intern(Type::Int);

    let int_to_int = arrow(&mut types, int, int);
    assert_types_eq(actual_defn, int_to_int, &types);
    assert_types_eq(actual_body, int_to_int, &types);
}

#[test]
fn test_infer_let_function_with_param_type() {
    let (module, mut types, result) = infer_from_str("def f { let f (y : int) = y; f }");

    assert_empty(&result.diagnostics);

    let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

    let int = types.intern(Type::Int);

    let int_to_int = arrow(&mut types, int, int);
    assert_types_eq(actual_defn, int_to_int, &types);
    assert_types_eq(actual_body, int_to_int, &types);
}
