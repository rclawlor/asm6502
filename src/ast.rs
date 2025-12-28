use std::sync::atomic::AtomicUsize;

/// ID of AST node
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NodeId(usize);

static NEXT_NODE_ID: AtomicUsize = AtomicUsize::new(0);

/// Get next available AST node ID
pub fn next_node_id() -> NodeId {
    NodeId(NEXT_NODE_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
}

/// Span of AST node in source
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    /// Get span up to end
    pub fn up_to(&self, end: Span) -> Span {
        Span {
            start: self.start,
            end: end.end,
        }
    }
}
