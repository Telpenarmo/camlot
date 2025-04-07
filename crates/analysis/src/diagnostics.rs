use core::{display_type, TypeError};

use line_index::TextRange;
use parser::{SyntaxKind, SyntaxNodePtr};

impl crate::Document {
    /// # Panics
    ///
    /// Panics if parser produced different number of error nodes than messages.
    #[must_use]
    pub fn get_diagnostics(&self) -> Vec<lsp_types::Diagnostic> {
        let mut diagnostics = self.parsing_errors();
        diagnostics.extend(self.lsp_type_errors());
        diagnostics
    }

    fn parsing_errors(&self) -> Vec<lsp_types::Diagnostic> {
        let parsed = &self.parsed();
        let mut error_idx = 0;
        parsed
            .syntax()
            .descendants_with_tokens()
            .filter(|node| {
                node.kind() == SyntaxKind::ERROR || node.kind() == SyntaxKind::LEXING_ERROR
            })
            .map(|node| {
                let msg = &parsed
                    .errors
                    .get(error_idx)
                    .expect("Missing error message")
                    .message;
                error_idx += 1;
                self.diagnostic(msg, node.text_range())
            })
            .collect()
    }

    fn diagnostic(&self, message: &str, range: TextRange) -> lsp_types::Diagnostic {
        let range = self.text_range_to_lsp_range(range);
        let mut diagnostic = lsp_types::Diagnostic::new_simple(range, message.to_string());
        diagnostic.source = Some("Camlot".into());
        diagnostic
    }

    fn range_of_expr<T: core::HirNode + std::fmt::Debug>(
        &self,
        hir: core::ArenaIdx<T>,
    ) -> TextRange {
        let whole = SyntaxNodePtr::new(&self.parsed().syntax().clone());
        self.hir().syntax(hir).unwrap_or(whole).text_range()
    }

    fn lsp_type_errors(&self) -> Vec<lsp_types::Diagnostic> {
        self.type_errors()
            .iter()
            .map(|error| self.type_error_to_diagnostic(error))
            .collect()
    }

    #[must_use]
    pub fn type_error_message(&self, error: &TypeError) -> String {
        let types = self.types();
        let names = self.names();
        let generalized_labels = self.generalized_labels();
        match *error {
            TypeError::UnboundVariable { name, .. } => {
                format!("Unbound variable {}", names.get_name(name))
            }
            TypeError::UnboundTypeVariable { name, .. } => {
                format!("Unbound type {}", names.get_name(name))
            }
            TypeError::CyclicType { typ, var, .. } => {
                let typ = display_type(types, names, generalized_labels, typ);
                let var = display_type(types, names, generalized_labels, var);
                format!("Cyclic type: {var} appears in {typ}",)
            }
            TypeError::WrongArgument {
                expected, actual, ..
            } => {
                let expected = display_type(types, names, generalized_labels, expected);
                let actual = display_type(types, names, generalized_labels, actual);
                format!("Type mismatch: this value has type {actual}, but is given to a function expecting type {expected}.")
            }
            TypeError::TypeMismatch {
                expected, actual, ..
            } => {
                let expected = display_type(types, names, generalized_labels, expected);
                let actual = display_type(types, names, generalized_labels, actual);
                format!("Type mismatch: expected {expected} but got {actual}.")
            }
            TypeError::UnexpectedUnitPattern { expected, .. } => {
                let expected = display_type(types, names, generalized_labels, expected);
                format!("This pattern constraints the value to type @unit, but it is expected to have type {expected}.")
            }
            TypeError::AppliedToNonFunction {
                lhs_type: func_type,
                ..
            } => {
                let func_type = display_type(types, names, generalized_labels, func_type);
                format!("This expression has a type {func_type}. It is not a function and cannot be applied.")
            }
            TypeError::WrongAnnotation {
                actual, expected, ..
            } => {
                let actual = display_type(types, names, generalized_labels, actual);
                let expected = display_type(types, names, generalized_labels, expected);
                format!("This parameter was expected to have type {expected}, but was annotated as {actual}.")
            }
            TypeError::ExpectedDueToAnnotation {
                actual, expected, ..
            } => {
                let actual = display_type(types, names, generalized_labels, actual);
                let expected = display_type(types, names, generalized_labels, expected);
                format!(
                "This expression was expected to have type {expected}, but it has type {actual}."
            )
            }
            TypeError::DuplicatedDefinition { double: _, name } => {
                format!("Definition `{}` already exists", names.get_name(name))
            }
            TypeError::DuplicatedTypeDefinition { double: _, name } => {
                format!("Type alias `{}` already exists", names.get_name(name))
            }
        }
    }

    #[must_use]
    #[allow(clippy::missing_panics_doc)]
    pub fn type_error_range(&self, error: &TypeError) -> TextRange {
        match *error {
            TypeError::AppliedToNonFunction { lhs: expr, .. }
            | TypeError::WrongArgument { arg: expr, .. }
            | TypeError::UnboundVariable { expr, .. }
            | TypeError::CyclicType { expr, .. }
            | TypeError::ExpectedDueToAnnotation { expr, .. }
            | TypeError::TypeMismatch { expr, .. } => self.range_of_expr(expr),
            TypeError::UnexpectedUnitPattern { pattern, .. } => self.range_of_expr(pattern),
            TypeError::UnboundTypeVariable { type_expr, .. }
            | TypeError::WrongAnnotation {
                annotation: type_expr,
                ..
            } => self.range_of_expr(type_expr),
            TypeError::DuplicatedDefinition { double, name: _ } => {
                let syntax: parser::nodes::Definition = self
                    .syntax(double)
                    .expect("Duplicated definition error in nonexistent definition");
                syntax
                    .ident_lit()
                    .expect("Duplicated definition error in nameless definition")
                    .text_range()
            }
            TypeError::DuplicatedTypeDefinition { double, name: _ } => {
                let syntax: parser::nodes::TypeDefinition = self
                    .syntax(double)
                    .expect("Duplicated type definition error in nonexistent type definition");
                syntax
                    .ident_lit()
                    .expect("Duplicated type definition error in nameless type definition")
                    .text_range()
            }
        }
    }

    fn type_error_to_diagnostic(&self, error: &TypeError) -> lsp_types::Diagnostic {
        let msg = self.type_error_message(error);
        let range = self.type_error_range(error);
        self.diagnostic(&msg, range)
    }
}
