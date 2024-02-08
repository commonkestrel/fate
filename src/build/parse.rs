use std::{ops::{Range, RangeInclusive}, sync::Arc};

use super::lexer::{self, SingleSpan};

#[derive(Debug,  Clone)]
pub struct Span {
    source_name: Arc<String>,
    source: lexer::Source,
    lines: RangeInclusive<usize>,
    columns: Range<usize>,
}

impl From<SingleSpan> for Span {
    fn from(value: SingleSpan) -> Self {
        Span {
            source_name: value.source_name,
            source: value.source,
            columns: value.columns,
            lines: value.line..=value.line,
        }
    }
}
