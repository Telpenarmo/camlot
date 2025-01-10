use core::{infer, InferenceResult, Interner, Module, Type, TypeIdx};

pub struct Document {
    line_index: line_index::LineIndex,
    text: String,
    types: Interner<Type>,
    parsed: parser::Parse,
    hir: Module,
    inference_result: InferenceResult,
}

#[allow(unused)]
impl Document {
    #[must_use]
    pub fn new(text: String) -> Document {
        let mut types = Interner::new();
        let line_index = line_index::LineIndex::new(&text);
        let parsed = parser::parse(&text);
        let mut hir = core::Module::new(&mut types);

        hir.lower_module(&parsed.module());

        let inference_result = core::infer(&hir, &mut types);

        Document {
            line_index,
            text,
            types,
            parsed,
            hir,
            inference_result,
        }
    }

    pub fn update(&mut self, text: String) {
        self.line_index = line_index::LineIndex::new(&text);
        let parse = parser::parse(&text);
        self.hir = core::Module::new(&mut self.types);
        self.hir.lower_module(&parse.module());
        self.inference_result = infer(&self.hir, &mut self.types);
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

    #[must_use]
    pub fn types(&self) -> &Interner<Type> {
        &self.types
    }

    pub(crate) fn expr_types(&self) -> &la_arena::ArenaMap<core::ExprIdx, TypeIdx> {
        &self.inference_result.expr_types
    }

    pub(crate) fn defn_types(&self) -> &la_arena::ArenaMap<core::DefinitionIdx, TypeIdx> {
        &self.inference_result.defn_types
    }

    pub(crate) fn type_errors(&self) -> &Vec<core::TypeError> {
        &self.inference_result.diagnostics
    }

    pub(crate) fn display_type(&self, idx: core::TypeIdx) -> String {
        core::display_type(&self.types, self.hir(), idx)
    }

    pub(crate) fn get_type(&self, idx: core::TypeIdx) -> &Type {
        self.types.lookup(idx)
    }

    pub(crate) fn syntax<T: core::StoredInArena, N: parser::AstNode>(
        &self,
        idx: core::ArenaIdx<T>,
    ) -> Option<N> {
        let ptr = self.hir.syntax(idx)?;
        let ptr = ptr.cast::<N>()?;
        Some(ptr.to_node(&self.parsed.syntax()))
    }
}
