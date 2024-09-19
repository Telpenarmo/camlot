use core::{infer, InferenceResult, Module, TypeIdx};

pub struct Document {
    line_index: line_index::LineIndex,
    text: String,
    parsed: parser::Parse,
    hir: Module,
    inference_result: InferenceResult,
}

#[allow(unused)]
impl Document {
    #[must_use]
    pub fn new(text: String) -> Document {
        let line_index = line_index::LineIndex::new(&text);
        let parsed = parser::parse(&text);
        let mut hir = core::Module::new();

        hir.lower_module(&parsed.module());

        let inference_result = core::infer(&hir);

        Document {
            line_index,
            text,
            parsed,
            hir,
            inference_result,
        }
    }

    pub fn update(&mut self, text: String) {
        self.line_index = line_index::LineIndex::new(&text);
        let parse = parser::parse(&text);
        self.hir = core::Module::new();
        self.hir.lower_module(&parse.module());
        self.inference_result = infer(&self.hir);
        self.parsed = parse;
        self.text = text;
    }

    #[must_use]
    pub fn parsed(&self) -> &parser::Parse {
        &self.parsed
    }

    pub(crate) fn get_line_index(&self) -> &line_index::LineIndex {
        &self.line_index
    }

    #[must_use]
    pub fn hir(&self) -> &core::Module {
        &self.hir
    }

    pub(crate) fn types(&self) -> &la_arena::ArenaMap<core::ExprIdx, TypeIdx> {
        &self.inference_result.expr_types
    }

    pub(crate) fn diagnostics(&self) -> Vec<lsp_types::Diagnostic> {
        self.inference_result
            .diagnostics
            .iter()
            .map(|diag| todo!(""))
            .collect()
    }
}
