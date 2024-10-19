use crate::{Module, TypeExpr, TypeExprIdx};

impl Module {
    pub(super) fn fmt_type_expr(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ty: TypeExprIdx,
    ) -> std::fmt::Result {
        match self.get_type_expr(ty) {
            TypeExpr::Missing => f.write_str("_"),
            &TypeExpr::IdentTypeExpr { name } => f.write_str(self.get_name(name)),
            &TypeExpr::TypeArrow { from, to } => {
                match self.get_type_expr(from) {
                    TypeExpr::TypeArrow { .. } => {
                        f.write_str("(")?;
                        self.fmt_type_expr(f, from)?;
                        f.write_str(")")?;
                    }
                    _ => self.fmt_type_expr(f, from)?,
                }
                f.write_str(" -> ")?;
                self.fmt_type_expr(f, to)?;
                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::hir::pprint::InModule;

    use super::*;

    impl std::fmt::Debug for InModule<TypeExprIdx> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            self.0.fmt_type_expr(f, self.1)
        }
    }

    #[test]
    fn test_fmt_type_expr() {
        let mut module = Module::new();
        let name = module.name("a");
        let a = module.alloc_type_expr(TypeExpr::IdentTypeExpr { name });
        let name = module.name("b");
        let b = module.alloc_type_expr(TypeExpr::IdentTypeExpr { name });
        let ab = module.alloc_type_expr(TypeExpr::TypeArrow { from: a, to: b });

        assert_eq!(format!("{:?}", InModule(module, ab)), "a -> b");
    }

    #[test]
    fn test_fmt_type_expr_three() {
        let mut module = Module::new();
        let name = module.name("a");
        let a = module.alloc_type_expr(TypeExpr::IdentTypeExpr { name });
        let name = module.name("b");
        let b = module.alloc_type_expr(TypeExpr::IdentTypeExpr { name });
        let ab = module.alloc_type_expr(TypeExpr::TypeArrow { from: a, to: b });

        let bab = module.alloc_type_expr(TypeExpr::TypeArrow { from: b, to: ab });

        assert_eq!(format!("{:?}", InModule(module, bab)), "b -> a -> b");
    }

    #[test]
    fn test_fmt_type_expr_associativity() {
        let mut module = Module::new();
        let name = module.name("a");
        let a = module.alloc_type_expr(TypeExpr::IdentTypeExpr { name });
        let name = module.name("b");
        let b = module.alloc_type_expr(TypeExpr::IdentTypeExpr { name });
        let ab = module.alloc_type_expr(TypeExpr::TypeArrow { from: a, to: b });
        let abc = module.alloc_type_expr(TypeExpr::TypeArrow { from: ab, to: a });

        assert_eq!(format!("{:?}", InModule(module, abc)), "(a -> b) -> a");
    }
}
