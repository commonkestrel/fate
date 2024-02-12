use std::{
    ops::{Range, RangeBounds},
    sync::Arc,
};

use colored::{Color, Colorize};

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    source_name: Arc<String>,
    lookup: Arc<Lookup>,
    location: Range<usize>,
}

impl Span {
    pub fn new(source_name: Arc<String>, lookup: Arc<Lookup>, location: Range<usize>) -> Self {
        Span {
            source_name,
            lookup,
            location,
        }
    }

    pub fn to(&self, other: &Span) -> Span {
        debug_assert_eq!(self.source_name, other.source_name);
        debug_assert_eq!(self.lookup, other.lookup);

        Span {
            source_name: self.source_name.clone(),
            lookup: self.lookup.clone(),
            location: self.location.start.min(other.location.start)
                ..self.location.end.max(other.location.end),
        }
    }

    pub fn line_col(&self) -> (usize, usize) {
        self.lookup.line_col(self.location.start)
    }

    pub fn pointer(&self, arrow_color: Color) -> String {
        let lines = self.lookup.lines(self.location.clone());
        let line_n = lines.start + 1;
        let col_n = self.lookup.col_from_line(lines.start, self.location.start) + 1;

        if lines.len() > 1 {
            todo!()
        } else {
            let line = self.lookup.line(lines.start).trim_end();
            let offset = (lines.start+1).ilog10() as usize + 2;

            format!(
                "\
                {arrow:>arr_space$} {name}:{line_n}:{col_n}\n\
                {cap:>width$}\n\
                {n} {line}\n\
                {cap:>width$} {pointer}\
                ",
                arrow = "-->".bright_blue().bold(),
                name = self.source_name,
                cap = "|".bright_blue().bold(),
                width = offset + 1,
                arr_space = offset + 2,
                n = format!("{line_n:<offset$}|").bright_blue().bold(),
                pointer = format!(
                    "{blank:>start$}{blank:^>length$}",
                    blank = "",
                    start = col_n - 1,
                    length = self.location.end - self.location.start,
                ).color(arrow_color),
            )
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lookup {
    source: Arc<String>,
    heads: Box<[usize]>,
}

impl Lookup {
    pub fn new(source: Arc<String>) -> Self {
        // Tabs should be replaced with spaces in order to keep character spacing the same
        debug_assert!(!source.contains('\t'));

        let heads = std::iter::once(0)
            .chain(
                source
                    .char_indices()
                    .filter_map(|(i, c)| if c == '\n' { Some(i + 1) } else { None }),
            )
            .collect();

        Lookup { source, heads }
    }

    pub fn line_n(&self, index: usize) -> usize {
        match self.heads.binary_search(&index) {
            Ok(line) => line,
            Err(insert) => insert - 1,
        }
    }

    #[inline]
    pub fn line_col(&self, index: usize) -> (usize, usize) {
        let line = self.line_n(index);
        let col = self.col_from_line(line, index);

        (line, col)
    }

    #[inline]
    pub fn col_from_line(&self, line: usize, index: usize) -> usize {
        index - self.heads[line]
    }

    pub fn multiline(&self, range: Range<usize>) -> bool {
        let starting_line = self.line_n(range.start);
        let next_start = self.heads[starting_line + 1];

        range.end <= next_start
    }

    pub fn line(&self, index: usize) -> &str {
        let range = self.heads[index]..self.heads[index + 1];

        &self.source[range]
    }

    pub fn lines(&self, span: Range<usize>) -> Range<usize> {
        let start_line = self.line_n(span.start);
        let next_start = self.heads[start_line + 1];

        if span.end <= next_start {
            // Check if the span ends on the same line
            start_line..start_line + 1
        } else {
            // Otherwise perform a binary search through the rest of the lines.
            match self.heads[start_line + 1..].binary_search(&(span.end - 1)) {
                Ok(end_line) => start_line..end_line + 1,
                Err(insert) => start_line..insert,
            }
        }
    }
}
