//! Manipulate lines of text and groups of lines.

use crate::{positions::SingleLineSpan, syntax::MatchedPos};
use lazy_static::lazy_static;
use regex::Regex;
use std::{
    cmp::{max, Ordering},
    fmt,
};

/// A distinct number type for line numbers, to prevent confusion with
/// other numerical data.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LineNumber(pub usize);

impl fmt::Debug for LineNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("LineNumber: {}", self.0))
    }
}

impl From<usize> for LineNumber {
    fn from(number: usize) -> Self {
        Self(number)
    }
}

/// Compare two MatchedPos to see which starts earlier (on either
/// side).
pub fn compare_matched_pos(lhs: &MatchedPos, rhs: &MatchedPos) -> Ordering {
    let lhs_line = lhs.pos.line;
    let rhs_line = rhs.pos.line;
    lhs_line.cmp(&rhs_line)
}

pub fn format_line_num(line_num: LineNumber) -> String {
    format!("{:>2} ", line_num.0 + 1)
}

/// A position in a single line of a string.
#[derive(Debug, PartialEq, Clone, Copy)]
struct LinePosition {
    /// Both zero-indexed.
    pub line: LineNumber,
    column: usize,
}

/// A struct for efficiently converting absolute string positions to
/// line-relative positions.
#[derive(Debug)]
pub struct NewlinePositions {
    /// A vector of the start positions of all the lines in `s`.
    positions: Vec<usize>,
    str_length: usize,
}

impl From<&str> for NewlinePositions {
    fn from(s: &str) -> Self {
        lazy_static! {
            static ref NEWLINE_RE: Regex = Regex::new("\n").unwrap();
        }
        let mut positions: Vec<_> = NEWLINE_RE.find_iter(s).map(|mat| mat.end()).collect();
        positions.insert(0, 0);

        NewlinePositions {
            positions,
            str_length: s.len(),
        }
    }
}

impl NewlinePositions {
    /// Convert to single-line spans. If the original span crosses a
    /// newline, the vec will contain multiple items.
    pub fn from_offsets(&self, region_start: usize, region_end: usize) -> Vec<SingleLineSpan> {
        let mut res = vec![];
        for (line_num, line_start) in self.positions.iter().enumerate() {
            let line_end = match self.positions.get(line_num + 1) {
                // TODO: this assumes lines terminate with \n, not \r\n.
                Some(v) => *v - 1,
                None => self.str_length,
            };

            if region_start > line_end {
                continue;
            }
            if *line_start > region_end {
                break;
            }

            res.push(SingleLineSpan {
                line: line_num.into(),
                start_col: if *line_start > region_start {
                    0
                } else {
                    region_start - line_start
                },
                end_col: if region_end < line_end {
                    region_end - line_start
                } else {
                    line_end - line_start
                },
            });
        }
        res
    }

    pub fn from_offsets_relative_to(
        &self,
        start: SingleLineSpan,
        region_start: usize,
        region_end: usize,
    ) -> Vec<SingleLineSpan> {
        let mut res = vec![];
        for pos in self.from_offsets(region_start, region_end) {
            if pos.line.0 == 0 {
                res.push(SingleLineSpan {
                    line: start.line,
                    start_col: start.start_col + pos.start_col,
                    end_col: start.start_col + pos.end_col,
                });
            } else {
                res.push(SingleLineSpan {
                    line: (start.line.0 + pos.line.0).into(),
                    start_col: pos.start_col,
                    end_col: pos.end_col,
                });
            }
        }

        res
    }
}

/// Return the length of `s` in codepoints. This ensures that it's
/// safe to slice `s` at this boundary.
pub fn codepoint_len(s: &str) -> usize {
    s.chars().count()
}

/// The first `len` codepoints of `s`. This is safer than slicing by
/// bytes, which panics if the byte isn't on a codepoint boundary.
pub fn substring_by_codepoint(s: &str, start: usize, end: usize) -> &str {
    let byte_start = s.char_indices().nth(start).unwrap().0;
    match s.char_indices().nth(end) {
        Some(byte_end) => &s[byte_start..byte_end.0],
        None => &s[byte_start..],
    }
}

/// Ensure that every line in `s` has this length. Pad short lines and
/// truncate long lines.
pub fn enforce_exact_length(s: &str, line_length: usize) -> String {
    let mut result = String::with_capacity(s.len());
    for line in s.lines() {
        if codepoint_len(line) > line_length {
            result.push_str(substring_by_codepoint(line, 0, line_length));
            result.push('\n');
        } else {
            // Pad with spaces.
            result.push_str(&format!("{:width$}\n", line, width = line_length));
        }
    }

    result
}

/// Truncate any lines in `s` that are longer than `line_length`.
pub fn enforce_max_length(s: &str, line_length: usize) -> String {
    let mut result = String::with_capacity(s.len());
    for line in s.lines() {
        // TODO: use length in chars not bytes.
        if line.len() > line_length {
            result.push_str(substring_by_codepoint(line, 0, line_length));
            result.push('\n');
        } else {
            result.push_str(&format!("{}\n", line));
        }
    }

    result
}

pub trait MaxLine {
    fn max_line(&self) -> LineNumber;
}

impl<S: AsRef<str>> MaxLine for S {
    fn max_line(&self) -> LineNumber {
        (max(1, self.as_ref().lines().count()) - 1).into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn from_offsets_first_line() {
        let newline_positions: NewlinePositions = "foo".into();
        let line_spans = newline_positions.from_offsets(1, 3);
        assert_eq!(
            line_spans,
            vec![SingleLineSpan {
                line: 0.into(),
                start_col: 1,
                end_col: 3
            }]
        );
    }

    #[test]
    fn from_offsets_first_char() {
        let newline_positions: NewlinePositions = "foo".into();
        let line_spans = newline_positions.from_offsets(0, 0);
        assert_eq!(
            line_spans,
            vec![SingleLineSpan {
                line: 0.into(),
                start_col: 0,
                end_col: 0
            }]
        );
    }

    #[test]
    fn from_offsets_split_over_multiple_lines() {
        let newline_positions: NewlinePositions = "foo\nbar\nbaz\naaaaaaaaaaa".into();
        let line_spans = newline_positions.from_offsets(5, 10);

        assert_eq!(
            line_spans,
            vec![
                (SingleLineSpan {
                    line: 1.into(),
                    start_col: 1,
                    end_col: 3
                }),
                (SingleLineSpan {
                    line: 2.into(),
                    start_col: 0,
                    end_col: 2
                })
            ]
        );
    }

    #[test]
    fn enforce_length_short() {
        let result = enforce_exact_length("foo\nbar\n", 5);
        assert_eq!(result, "foo  \nbar  \n");
    }

    #[test]
    fn enforce_length_long() {
        let result = enforce_exact_length("foobar\nbarbaz\n", 3);
        assert_eq!(result, "foo\nbar\n");
    }

    #[test]
    fn str_max_line() {
        let line: String = "foo\nbar".into();
        assert_eq!(line.max_line().0, 1);
    }

    #[test]
    fn empty_str_max_line() {
        let line: String = "".into();
        assert_eq!(line.max_line().0, 0);
    }

    #[test]
    fn from_offsets_relative_to() {
        let newline_positions: NewlinePositions = "foo\nbar".into();

        let pos = SingleLineSpan {
            line: 1.into(),
            start_col: 1,
            end_col: 1,
        };

        let line_spans = newline_positions.from_offsets_relative_to(pos, 1, 2);
        assert_eq!(
            line_spans,
            vec![SingleLineSpan {
                line: 1.into(),
                start_col: 2,
                end_col: 3
            }]
        );
    }

    #[test]
    fn codepoint_len_non_ascii() {
        assert_eq!(codepoint_len("ƒoo"), 3);
    }
}
