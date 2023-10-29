use super::diagnostic::{Span, Spanned};
use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::hash::Hash;

#[derive(Clone, Debug, Default)]
pub struct VariableLevelContext<LVar: Clone + PartialEq + Eq + Hash> {
    pub map: RefCell<HashMap<LVar, (Option<Span>, usize)>>,
}
impl<LVar: Clone + PartialEq + Eq + Hash> VariableLevelContext<LVar> {
    pub fn inner_map(&self) -> Ref<HashMap<LVar, (Option<Span>, usize)>> {
        self.map.borrow()
    }
    pub fn make_stack<'a: 'b, 'b: 'a>(&'a mut self) -> VariableLevelContextStack<'b, LVar> {
        VariableLevelContextStack {
            stack: RefCell::new(Vec::new()),
            original: self,
        }
    }
}

type VariableLevelStackInner<LVar> = RefCell<Vec<(LVar, Option<(Option<Span>, usize)>)>>;
#[derive(Debug)]
pub struct VariableLevelContextStack<'a, LVar: Clone + PartialEq + Eq + Hash> {
    stack: VariableLevelStackInner<LVar>,
    original: &'a mut VariableLevelContext<LVar>,
}
impl<'a, LVar: Clone + PartialEq + Eq + Hash> VariableLevelContextStack<'a, LVar> {
    pub fn push(
        &'a self,
        id: Spanned<LVar>,
        level: usize,
    ) -> ContextStackPopper<'a, Self, impl Fn(&VariableLevelContextStack<'a, LVar>)> {
        let saved = self
            .original
            .map
            .borrow_mut()
            .insert(id.value.clone(), (id.span, level));
        self.stack.borrow_mut().push((id.value, saved));
        ContextStackPopper {
            stack: self,
            f: |stack| stack.pop(),
        }
    }
    fn pop(&self) {
        let (name, saved) = self.stack.borrow_mut().pop().unwrap();
        if let Some(saved) = saved {
            self.original.map.borrow_mut().insert(name, saved);
        } else {
            self.original.map.borrow_mut().remove(&name);
        }
    }
    pub fn get(&self, id: &LVar) -> Option<(Option<Span>, usize)> {
        self.original.map.borrow().get(id).copied()
    }

    pub fn fork(&self) -> VariableLevelContext<LVar> {
        self.original.clone()
    }
}

pub struct ContextStackPopper<'a, Stack, F>
where
    F: Fn(&Stack),
{
    stack: &'a Stack,
    f: F,
}
impl<'a, Stack, F> ContextStackPopper<'a, Stack, F>
where
    F: Fn(&Stack),
{
    pub fn new(stack: &'a Stack, f: F) -> Self {
        Self { stack, f }
    }
}
impl<'a, Stack, F> Drop for ContextStackPopper<'a, Stack, F>
where
    F: Fn(&Stack),
{
    fn drop(&mut self) {
        (self.f)(self.stack);
    }
}
