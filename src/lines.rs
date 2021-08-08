//! Manipulate lines of text and groups of lines.

use crate::intervals::Interval;
use crate::positions::SingleLineSpan;
use crate::syntax::{MatchKind, MatchedPos};
use lazy_static::lazy_static;
use regex::Regex;
use std::cmp::{max, min, Ordering};
use std::collections::HashSet;
use std::fmt;
use std::iter::FromIterator;

const MAX_GAP: usize = 2;

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
        LineNumber(number)
    }
}

/// A group of lines that are displayed together.
///
/// This is called a 'hunk' in some diff tools.
#[derive(Debug, PartialEq, Eq)]
pub struct LineGroup {
    pub lhs_lines: Option<Interval<LineNumber>>,
    pub rhs_lines: Option<Interval<LineNumber>>,
}

impl LineGroup {
    fn new() -> Self {
        Self {
            lhs_lines: None,
            rhs_lines: None,
        }
    }

    pub fn lhs_lines(&self) -> Vec<LineNumber> {
        let mut res = vec![];
        match &self.lhs_lines {
            Some(lhs_lines) => {
                if lhs_lines.is_empty() {
                    return vec![];
                }

                for line in lhs_lines.start.0..lhs_lines.end.0 {
                    res.push(line.into());
                }
            }
            None => {}
        }
        res
    }

    pub fn rhs_lines(&self) -> Vec<LineNumber> {
        let mut res = vec![];
        match &self.rhs_lines {
            Some(rhs_lines) => {
                if rhs_lines.end.0 == 0 {
                    return vec![];
                }

                for line in rhs_lines.start.0..rhs_lines.end.0 {
                    res.push(line.into());
                }
            }
            None => {}
        }
        res
    }

    /// Does `mp` overlap with self? Assumes that `mp` either overlaps
    /// or occurs after self.
    fn next_overlaps(&self, is_lhs: bool, mp: &MatchedPos, max_gap: usize) -> bool {
        let (group_lines, opposite_group_lines) = if is_lhs {
            (&self.lhs_lines, &self.rhs_lines)
        } else {
            (&self.rhs_lines, &self.lhs_lines)
        };

        if let Some(group_lines) = group_lines {
            let last_group_line = group_lines.end;

            let match_lines = &mp.pos;
            assert!(!match_lines.is_empty());
            let first_match_line = match_lines[0].line.0;

            if first_match_line <= last_group_line.0 + max_gap {
                return true;
            }
        }

        if let MatchKind::Unchanged { opposite_pos } = &mp.kind {
            let first_opposite = opposite_pos.0.first();
            if let (Some(first_opposite), Some(opposite_group_lines)) =
                (first_opposite, opposite_group_lines)
            {
                if first_opposite.line.0 <= opposite_group_lines.end.0 + max_gap {
                    return true;
                }
            }
        }

        false
    }

    fn add_lhs_pos(&mut self, line_spans: &[SingleLineSpan]) {
        if let (Some(first), Some(last)) = (line_spans.first(), line_spans.last()) {
            if let Some(lhs_lines) = &self.lhs_lines {
                let start = min(lhs_lines.start, first.line);
                let end = max(lhs_lines.end, last.line);
                self.lhs_lines = Some(Interval { start, end });
            } else {
                self.lhs_lines = Some(Interval {
                    start: first.line,
                    end: last.line,
                });
            }
        }
    }
    fn add_rhs_pos(&mut self, line_spans: &[SingleLineSpan]) {
        if let (Some(first), Some(last)) = (line_spans.first(), line_spans.last()) {
            if let Some(rhs_lines) = &self.rhs_lines {
                let start = min(rhs_lines.start, first.line);
                let end = max(rhs_lines.end, last.line);
                self.rhs_lines = Some(Interval { start, end });
            } else {
                self.rhs_lines = Some(Interval {
                    start: first.line,
                    end: last.line,
                });
            }
        }
    }

    fn add_lhs(&mut self, mp: &MatchedPos) {
        self.add_lhs_pos(&mp.pos);

        match &mp.kind {
            MatchKind::Unchanged { opposite_pos } => {
                self.add_rhs_pos(&opposite_pos.0);
            }
            MatchKind::UnchangedCommentPart { opposite_pos } => {
                self.add_rhs_pos(opposite_pos);
            }
            _ => {}
        }
    }
    fn add_rhs(&mut self, mp: &MatchedPos) {
        self.add_rhs_pos(&mp.pos);

        match &mp.kind {
            MatchKind::Unchanged { opposite_pos } => {
                self.add_lhs_pos(&opposite_pos.0);
            }
            MatchKind::UnchangedCommentPart { opposite_pos } => {
                self.add_lhs_pos(opposite_pos);
            }
            _ => {}
        }
    }

    pub fn max_visible_lhs(&self) -> LineNumber {
        match &self.lhs_lines {
            Some(lhs_lines) => lhs_lines.end,
            None => 0.into(),
        }
    }
    pub fn max_visible_rhs(&self) -> LineNumber {
        match &self.rhs_lines {
            Some(rhs_lines) => rhs_lines.end,
            None => 0.into(),
        }
    }
}

/// Compare two MatchedPos to see which starts earlier (on either
/// side).
fn compare_matched_pos(lhs: &MatchedPos, rhs: &MatchedPos) -> Ordering {
    let lhs_line = lhs.pos[0].line;
    let rhs_line = rhs.pos[0].line;
    lhs_line.cmp(&rhs_line)
}

/// Add `amount` lines of context around the line numbers given.
pub fn pad(lines: &[LineNumber], amount: usize, max_line: LineNumber) -> Vec<LineNumber> {
    let mut seen: HashSet<usize> = HashSet::new();
    let mut res: Vec<LineNumber> = vec![];

    for line in lines {
        for line_num in max(line.0 as isize - amount as isize, 0) as usize..=(line.0 + amount) {
            if line_num > max_line.0 {
                break;
            }

            if !seen.contains(&line_num) {
                res.push(line_num.into());
                seen.insert(line_num);
            }
        }
    }

    res
}

pub fn groups_from_lines(
    lhs_padded_lines: &[LineNumber],
    rhs_padded_lines: &[LineNumber],
    lhs_mps: &[MatchedPos],
    rhs_mps: &[MatchedPos],
) -> Vec<LineGroup> {
    let lhs_padded_lines: HashSet<LineNumber> =
        HashSet::from_iter(lhs_padded_lines.iter().cloned());
    let rhs_padded_lines: HashSet<LineNumber> =
        HashSet::from_iter(rhs_padded_lines.iter().cloned());

    let mut mps = vec![];
    mps.extend(lhs_mps.iter().map(|p| (true, p)));
    mps.extend(rhs_mps.iter().map(|p| (false, p)));

    mps.sort_unstable_by(|(_, x), (_, y)| compare_matched_pos(x, y));

    let mut groups = vec![];
    let mut current: Option<LineGroup> = None;

    // TODO: what about blank lines that don't have any mp on them?
    for (is_lhs, mp) in mps {
        match mp.pos.first() {
            Some(pos) => {
                if is_lhs && !lhs_padded_lines.contains(&pos.line)
                    || !is_lhs && !rhs_padded_lines.contains(&pos.line)
                {
                    // This MatchedPos does not occur on the lines we
                    // want to display.
                    continue;
                }
            }
            None => continue,
        }

        if let Some(group) = current.take() {
            if group.next_overlaps(is_lhs, mp, MAX_GAP) {
                // Continue with this group.
                current = Some(group);
            } else {
                // Start new group
                groups.push(group);
                current = None;
            }
        }

        let mut group = current.unwrap_or_else(LineGroup::new);
        if is_lhs {
            group.add_lhs(mp);
        } else {
            group.add_rhs(mp);
        }
        current = Some(group);
    }

    if let Some(group) = current {
        groups.push(group);
    }
    groups
}

pub fn format_line_num(line_num: LineNumber) -> String {
    format!("{:>2} ", line_num.0 + 1)
}

/// A position in a single line of a string.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct LinePosition {
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

impl MaxLine for String {
    fn max_line(&self) -> LineNumber {
        (max(1, self.lines().count()) - 1).into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn from_ranges_first_line() {
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
    fn from_ranges_first_char() {
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
    fn from_ranges_split_over_multiple_lines() {
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
    fn line_group_uses_interval_correctly() {
        let group = LineGroup {
            lhs_lines: Some(Interval {
                start: 1.into(),
                end: 3.into(),
            }),
            rhs_lines: None,
        };

        // Intervals are inclusive of `start` but exclusive of `end`.
        assert_eq!(group.lhs_lines(), vec![1.into(), 2.into()])
    }

    #[test]
    fn codepoint_len_non_ascii() {
        assert_eq!(codepoint_len("ƒoo"), 3);
    }
}
