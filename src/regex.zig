// Simple regex engine for FHIRPath
// Supports: literals, ^$, [charset], *, +, ?, {n,m}, ., |, \d\w\s\D\W\S\b
// Capture groups for replaceMatches
const std = @import("std");

pub const Regex = struct {
    pattern: []const u8,
    flags: Flags,

    pub const Flags = struct {
        case_insensitive: bool = false,
        multiline: bool = false,
    };

    pub fn init(pattern: []const u8, flags: Flags) Regex {
        return .{ .pattern = pattern, .flags = flags };
    }

    /// Returns true if the pattern matches anywhere in the text
    pub fn matches(self: Regex, text: []const u8) bool {
        // Try to match at each position
        var i: usize = 0;
        while (i <= text.len) : (i += 1) {
            var match_ctx = MatchContext{
                .pattern = self.pattern,
                .text = text,
                .text_pos = i,
                .flags = self.flags,
            };
            if (matchPattern(&match_ctx, 0)) {
                return true;
            }
            if (i == text.len) break;
        }
        return false;
    }

    /// Returns true if the pattern matches the entire text
    pub fn matchesFull(self: Regex, text: []const u8) bool {
        var match_ctx = MatchContext{
            .pattern = self.pattern,
            .text = text,
            .text_pos = 0,
            .flags = self.flags,
        };
        if (matchPattern(&match_ctx, 0)) {
            return match_ctx.text_pos == text.len;
        }
        return false;
    }

    /// Replace all matches with substitution (supports $1, $2, etc.)
    pub fn replaceAll(self: Regex, allocator: std.mem.Allocator, text: []const u8, substitution: []const u8) ![]const u8 {
        var result = std.ArrayList(u8).empty;
        var pos: usize = 0;

        while (pos <= text.len) {
            var match_ctx = MatchContext{
                .pattern = self.pattern,
                .text = text,
                .text_pos = pos,
                .flags = self.flags,
            };

            if (matchPattern(&match_ctx, 0) and match_ctx.text_pos > pos) {
                // Apply substitution with capture references
                try applySubstitution(&result, allocator, text, substitution, &match_ctx);
                pos = match_ctx.text_pos;
            } else {
                if (pos < text.len) {
                    try result.append(allocator, text[pos]);
                }
                pos += 1;
            }
        }

        return result.toOwnedSlice(allocator);
    }
};

fn applySubstitution(result: *std.ArrayList(u8), allocator: std.mem.Allocator, text: []const u8, substitution: []const u8, ctx: *MatchContext) !void {
    var i: usize = 0;
    while (i < substitution.len) {
        if (substitution[i] == '$' and i + 1 < substitution.len) {
            // Check for $0-$9
            if (substitution[i + 1] >= '0' and substitution[i + 1] <= '9') {
                const group_num = substitution[i + 1] - '0';
                if (group_num < ctx.capture_count) {
                    const cap = ctx.captures[group_num];
                    try result.appendSlice(allocator, text[cap.start..cap.end]);
                }
                i += 2;
                continue;
            }
            // Check for ${n} or ${name}
            if (substitution[i + 1] == '{') {
                if (std.mem.indexOf(u8, substitution[i + 2 ..], "}")) |end_brace| {
                    const name = substitution[i + 2 .. i + 2 + end_brace];
                    if (name.len == 1 and name[0] >= '0' and name[0] <= '9') {
                        // Numeric reference ${0}-${9}
                        const group_num = name[0] - '0';
                        if (group_num < ctx.capture_count) {
                            const cap = ctx.captures[group_num];
                            try result.appendSlice(allocator, text[cap.start..cap.end]);
                        }
                    } else {
                        // Named capture reference ${name}
                        if (ctx.getCaptureByName(name)) |cap| {
                            try result.appendSlice(allocator, text[cap.start..cap.end]);
                        }
                    }
                    i += 3 + end_brace;
                    continue;
                }
            }
        }
        try result.append(allocator, substitution[i]);
        i += 1;
    }
}

const Capture = struct {
    start: usize,
    end: usize,
    name: ?[]const u8 = null, // For named capture groups
};

const MatchContext = struct {
    pattern: []const u8,
    text: []const u8,
    text_pos: usize,
    flags: Regex.Flags,
    captures: [10]Capture = undefined,
    capture_count: usize = 0,
    match_start: usize = 0,

    fn addCapture(self: *MatchContext, start: usize, end: usize) void {
        if (self.capture_count < 10) {
            self.captures[self.capture_count] = .{ .start = start, .end = end, .name = null };
            self.capture_count += 1;
        }
    }

    fn addNamedCapture(self: *MatchContext, start: usize, end: usize, name: []const u8) void {
        if (self.capture_count < 10) {
            self.captures[self.capture_count] = .{ .start = start, .end = end, .name = name };
            self.capture_count += 1;
        }
    }

    fn getCaptureByName(self: *const MatchContext, name: []const u8) ?Capture {
        for (self.captures[0..self.capture_count]) |cap| {
            if (cap.name) |cap_name| {
                if (std.mem.eql(u8, cap_name, name)) {
                    return cap;
                }
            }
        }
        return null;
    }
};

fn matchPattern(ctx: *MatchContext, pat_pos: usize) bool {
    ctx.match_start = ctx.text_pos;

    // Handle alternation by trying each branch
    var alt_start = pat_pos;
    var alt_end = findAlternative(ctx.pattern, pat_pos);

    while (true) {
        const branch = ctx.pattern[alt_start..alt_end];
        const saved_pos = ctx.text_pos;
        const saved_captures = ctx.capture_count;

        if (matchBranch(ctx, branch)) {
            // Record full match as capture 0
            // Shift existing captures to make room for full match at index 0
            var i = ctx.capture_count;
            while (i > 0) : (i -= 1) {
                if (i < 10) ctx.captures[i] = ctx.captures[i - 1];
            }
            ctx.captures[0] = .{ .start = ctx.match_start, .end = ctx.text_pos };
            if (ctx.capture_count < 10) ctx.capture_count += 1;
            return true;
        }

        ctx.text_pos = saved_pos;
        ctx.capture_count = saved_captures;

        // Try next alternative
        if (alt_end >= ctx.pattern.len or ctx.pattern[alt_end] != '|') break;
        alt_start = alt_end + 1;
        alt_end = findAlternative(ctx.pattern, alt_start);
    }

    return false;
}

fn findAlternative(pattern: []const u8, start: usize) usize {
    var pos = start;
    var depth: usize = 0;

    while (pos < pattern.len) {
        const c = pattern[pos];
        if (c == '\\' and pos + 1 < pattern.len) {
            pos += 2;
            continue;
        }
        if (c == '[') {
            // Skip character class
            pos += 1;
            while (pos < pattern.len and pattern[pos] != ']') {
                if (pattern[pos] == '\\' and pos + 1 < pattern.len) pos += 1;
                pos += 1;
            }
        } else if (c == '(') {
            depth += 1;
        } else if (c == ')') {
            if (depth > 0) depth -= 1;
        } else if (c == '|' and depth == 0) {
            return pos;
        }
        pos += 1;
    }

    return pattern.len;
}

fn matchBranch(ctx: *MatchContext, branch: []const u8) bool {
    return matchBranchAt(ctx, branch, 0);
}

/// Recursive backtracking matcher: matches branch starting at pat_pos
fn matchBranchAt(ctx: *MatchContext, branch: []const u8, start_pat_pos: usize) bool {
    var pat_pos = start_pat_pos;

    while (pat_pos < branch.len) {
        const c = branch[pat_pos];

        // Handle anchors
        if (c == '^') {
            if (ctx.text_pos != 0) {
                if (ctx.flags.multiline and ctx.text_pos > 0 and ctx.text[ctx.text_pos - 1] == '\n') {
                    pat_pos += 1;
                    continue;
                }
                return false;
            }
            pat_pos += 1;
            continue;
        }

        if (c == '$') {
            if (ctx.text_pos != ctx.text.len) {
                if (ctx.flags.multiline and ctx.text_pos < ctx.text.len and ctx.text[ctx.text_pos] == '\n') {
                    pat_pos += 1;
                    continue;
                }
                return false;
            }
            pat_pos += 1;
            continue;
        }

        // Handle groups
        if (c == '(') {
            const group_end = findGroupEnd(branch, pat_pos);
            var group_content = branch[pat_pos + 1 .. group_end];
            var group_name: ?[]const u8 = null;

            // Check for named group: (?<name>...)
            if (group_content.len >= 3 and group_content[0] == '?' and group_content[1] == '<') {
                // Find the closing >
                if (std.mem.indexOf(u8, group_content[2..], ">")) |name_end| {
                    group_name = group_content[2 .. 2 + name_end];
                    group_content = group_content[3 + name_end ..];
                }
            }

            // Get quantifier after group
            const quant = parseQuantifier(branch, group_end + 1);

            // For groups, we use a special backtracking that records captures
            if (!matchGroupBacktrackNamed(ctx, group_content, quant, branch, quant.end, group_name)) {
                return false;
            }

            return true; // Successfully matched rest via backtracking
        }

        // Parse atom and quantifier
        const atom_end = parseAtomEnd(branch, pat_pos);
        const atom = branch[pat_pos..atom_end];
        const quant = parseQuantifier(branch, atom_end);

        if (!matchQuantifiedBacktrack(ctx, atom, quant, false, branch, quant.end)) {
            return false;
        }

        return true; // Successfully matched rest via backtracking
    }

    return true;
}

fn findGroupEnd(pattern: []const u8, start: usize) usize {
    var depth: usize = 1;
    var pos = start + 1;

    while (pos < pattern.len and depth > 0) {
        const c = pattern[pos];
        if (c == '\\' and pos + 1 < pattern.len) {
            pos += 2;
            continue;
        }
        if (c == '(') depth += 1;
        if (c == ')') depth -= 1;
        pos += 1;
    }

    return pos - 1;
}

fn parseAtomEnd(pattern: []const u8, pos: usize) usize {
    if (pos >= pattern.len) return pos;

    const c = pattern[pos];

    // Escape sequence
    if (c == '\\' and pos + 1 < pattern.len) {
        return pos + 2;
    }

    // Character class
    if (c == '[') {
        var end = pos + 1;
        if (end < pattern.len and pattern[end] == '^') end += 1;
        while (end < pattern.len) {
            if (pattern[end] == '\\' and end + 1 < pattern.len) {
                end += 2;
                continue;
            }
            if (pattern[end] == ']') {
                return end + 1;
            }
            end += 1;
        }
        return end;
    }

    // Single character (including .)
    return pos + 1;
}

const Quantifier = struct {
    min: usize,
    max: ?usize, // null = unlimited
    end: usize,
};

fn parseQuantifier(pattern: []const u8, pos: usize) Quantifier {
    if (pos >= pattern.len) return .{ .min = 1, .max = 1, .end = pos };

    const c = pattern[pos];

    if (c == '*') return .{ .min = 0, .max = null, .end = pos + 1 };
    if (c == '+') return .{ .min = 1, .max = null, .end = pos + 1 };
    if (c == '?') return .{ .min = 0, .max = 1, .end = pos + 1 };

    if (c == '{') {
        var end = pos + 1;
        var min: usize = 0;
        var max: ?usize = null;
        var has_comma = false;

        while (end < pattern.len and pattern[end] >= '0' and pattern[end] <= '9') {
            min = min * 10 + (pattern[end] - '0');
            end += 1;
        }

        if (end < pattern.len and pattern[end] == ',') {
            has_comma = true;
            end += 1;
            if (end < pattern.len and pattern[end] >= '0' and pattern[end] <= '9') {
                max = 0;
                while (end < pattern.len and pattern[end] >= '0' and pattern[end] <= '9') {
                    max = max.? * 10 + (pattern[end] - '0');
                    end += 1;
                }
            }
        } else {
            max = min;
        }

        if (end < pattern.len and pattern[end] == '}') {
            return .{
                .min = min,
                .max = if (has_comma and max == null) null else (max orelse min),
                .end = end + 1,
            };
        }
    }

    return .{ .min = 1, .max = 1, .end = pos };
}

/// Match atom with backtracking: try greedy match, then backtrack if rest fails
fn matchQuantifiedBacktrack(ctx: *MatchContext, atom: []const u8, quant: Quantifier, is_group: bool, branch: []const u8, next_pat_pos: usize) bool {
    _ = is_group; // Groups handled separately by matchGroupBacktrack
    var count: usize = 0;
    var positions: [256]usize = undefined; // Track text positions for backtracking
    positions[0] = ctx.text_pos; // Position before any matches

    // Match minimum required
    while (count < quant.min) {
        if (!matchAtom(ctx, atom)) return false;
        count += 1;
        if (count < positions.len) {
            positions[count] = ctx.text_pos;
        }
    }

    // Match up to maximum (greedy)
    const max = quant.max orelse 1000; // Arbitrary large number
    while (count < max) {
        const saved_pos = ctx.text_pos;
        if (!matchAtom(ctx, atom)) {
            ctx.text_pos = saved_pos;
            break;
        }
        count += 1;
        if (count < positions.len) {
            positions[count] = ctx.text_pos;
        }
    }

    // Try to match rest of pattern; backtrack if necessary
    while (true) {
        const saved_captures = ctx.capture_count;
        if (matchBranchAt(ctx, branch, next_pat_pos)) {
            return true; // Found a match!
        }
        ctx.capture_count = saved_captures;

        // Backtrack: reduce count by 1 if possible
        if (count == quant.min) {
            return false; // Can't backtrack further
        }
        count -= 1;
        if (count < positions.len) {
            ctx.text_pos = positions[count];
        } else {
            return false; // Too many matches to track
        }
    }
}

/// Match a group with backtracking, recording captures properly
fn matchGroupBacktrack(ctx: *MatchContext, group_content: []const u8, quant: Quantifier, branch: []const u8, next_pat_pos: usize) bool {
    return matchGroupBacktrackNamed(ctx, group_content, quant, branch, next_pat_pos, null);
}

fn matchGroupBacktrackNamed(ctx: *MatchContext, group_content: []const u8, quant: Quantifier, branch: []const u8, next_pat_pos: usize, group_name: ?[]const u8) bool {
    var count: usize = 0;
    // Track both text positions and capture info for backtracking
    var positions: [256]usize = undefined;
    var capture_starts: [256]usize = undefined;
    var capture_ends: [256]usize = undefined;
    var capture_counts: [256]usize = undefined;

    positions[0] = ctx.text_pos;
    capture_counts[0] = ctx.capture_count;

    // Match minimum required
    while (count < quant.min) {
        const group_start = ctx.text_pos;
        if (!matchBranchAt(ctx, group_content, 0)) return false;
        const group_end = ctx.text_pos;

        // Record capture info for this iteration
        if (count < capture_starts.len) {
            capture_starts[count] = group_start;
            capture_ends[count] = group_end;
        }

        count += 1;
        if (count < positions.len) {
            positions[count] = ctx.text_pos;
            capture_counts[count] = ctx.capture_count;
        }
    }

    // Match up to maximum (greedy)
    const max = quant.max orelse 1000;
    while (count < max) {
        const saved_pos = ctx.text_pos;
        const group_start = ctx.text_pos;
        if (!matchBranchAt(ctx, group_content, 0)) {
            ctx.text_pos = saved_pos;
            break;
        }
        const group_end = ctx.text_pos;

        if (count < capture_starts.len) {
            capture_starts[count] = group_start;
            capture_ends[count] = group_end;
        }

        count += 1;
        if (count < positions.len) {
            positions[count] = ctx.text_pos;
            capture_counts[count] = ctx.capture_count;
        }
    }

    // Try to match rest of pattern; backtrack if necessary
    while (true) {
        // Add captures for current successful match count
        const saved_capture_count = if (count > 0 and count - 1 < capture_counts.len)
            capture_counts[count - 1]
        else
            ctx.capture_count;

        // Restore to capture state from before last successful match, then add the captures
        ctx.capture_count = if (count > 0 and count < capture_counts.len) capture_counts[0] else ctx.capture_count;
        // Add all the captures we've collected
        var i: usize = 0;
        while (i < count and i < capture_starts.len) : (i += 1) {
            if (group_name) |name| {
                ctx.addNamedCapture(capture_starts[i], capture_ends[i], name);
            } else {
                ctx.addCapture(capture_starts[i], capture_ends[i]);
            }
        }

        if (matchBranchAt(ctx, branch, next_pat_pos)) {
            return true; // Found a match!
        }

        // Restore captures before trying backtrack
        ctx.capture_count = saved_capture_count;

        // Backtrack: reduce count by 1 if possible
        if (count == quant.min) {
            return false; // Can't backtrack further
        }
        count -= 1;
        if (count < positions.len) {
            ctx.text_pos = positions[count];
        } else {
            return false; // Too many matches to track
        }
    }
}

fn matchAtom(ctx: *MatchContext, atom: []const u8) bool {
    if (atom.len == 0) return true;

    const c = atom[0];

    // Escape sequence - handle zero-width assertions before bounds check
    if (c == '\\' and atom.len >= 2) {
        const ec = atom[1];

        // Word boundary - zero-width, can match at end of string
        if (ec == 'b') {
            const prev_word = ctx.text_pos > 0 and isWordChar(ctx.text[ctx.text_pos - 1]);
            const curr_word = ctx.text_pos < ctx.text.len and isWordChar(ctx.text[ctx.text_pos]);
            return prev_word != curr_word;
        }
        if (ec == 'B') {
            const prev_word = ctx.text_pos > 0 and isWordChar(ctx.text[ctx.text_pos - 1]);
            const curr_word = ctx.text_pos < ctx.text.len and isWordChar(ctx.text[ctx.text_pos]);
            return prev_word == curr_word;
        }

        // All other escape sequences need a character to match
        if (ctx.text_pos >= ctx.text.len) return false;
        const tc = ctx.text[ctx.text_pos];

        const matched = switch (ec) {
            'd' => tc >= '0' and tc <= '9',
            'D' => !(tc >= '0' and tc <= '9'),
            'w' => isWordChar(tc),
            'W' => !isWordChar(tc),
            's' => tc == ' ' or tc == '\t' or tc == '\n' or tc == '\r',
            'S' => !(tc == ' ' or tc == '\t' or tc == '\n' or tc == '\r'),
            'n' => tc == '\n',
            'r' => tc == '\r',
            't' => tc == '\t',
            else => charMatch(tc, ec, ctx.flags.case_insensitive),
        };

        if (matched) {
            ctx.text_pos += 1;
            return true;
        }
        return false;
    }

    // All non-escape atoms need a character to match
    if (ctx.text_pos >= ctx.text.len) return false;
    const tc = ctx.text[ctx.text_pos];

    // Dot - any char (FHIRPath requires single-line mode where dot matches newline)
    if (c == '.') {
        ctx.text_pos += 1;
        return true;
    }

    // Character class [...]
    if (c == '[') {
        const matched = matchCharset(atom, tc, ctx.flags.case_insensitive);
        if (matched) {
            ctx.text_pos += 1;
            return true;
        }
        return false;
    }

    // Literal character
    if (charMatch(tc, c, ctx.flags.case_insensitive)) {
        ctx.text_pos += 1;
        return true;
    }
    return false;
}

fn matchCharset(charset: []const u8, c: u8, case_insensitive: bool) bool {
    if (charset.len < 2) return false;

    var pos: usize = 1;
    var negated = false;

    if (pos < charset.len and charset[pos] == '^') {
        negated = true;
        pos += 1;
    }

    var matched = false;
    while (pos < charset.len) {
        if (charset[pos] == ']') break;

        const match_char = charset[pos];

        // Handle escape
        if (match_char == '\\' and pos + 1 < charset.len) {
            pos += 1;
            const ec = charset[pos];
            const char_matched = switch (ec) {
                'd' => c >= '0' and c <= '9',
                'D' => !(c >= '0' and c <= '9'),
                'w' => isWordChar(c),
                'W' => !isWordChar(c),
                's' => c == ' ' or c == '\t' or c == '\n' or c == '\r',
                'S' => !(c == ' ' or c == '\t' or c == '\n' or c == '\r'),
                'n' => c == '\n',
                'r' => c == '\r',
                't' => c == '\t',
                else => charMatch(c, ec, case_insensitive),
            };
            if (char_matched) matched = true;
            pos += 1;
            continue;
        }

        // Check for range a-z
        if (pos + 2 < charset.len and charset[pos + 1] == '-' and charset[pos + 2] != ']') {
            const range_end = charset[pos + 2];
            if (inRange(c, match_char, range_end, case_insensitive)) {
                matched = true;
            }
            pos += 3;
            continue;
        }

        // Single character
        if (charMatch(c, match_char, case_insensitive)) {
            matched = true;
        }
        pos += 1;
    }

    return if (negated) !matched else matched;
}

fn charMatch(a: u8, b: u8, case_insensitive: bool) bool {
    if (a == b) return true;
    if (case_insensitive) {
        return toLower(a) == toLower(b);
    }
    return false;
}

fn toLower(c: u8) u8 {
    if (c >= 'A' and c <= 'Z') return c + 32;
    return c;
}

fn inRange(c: u8, start: u8, end: u8, case_insensitive: bool) bool {
    if (case_insensitive) {
        const lc = toLower(c);
        return lc >= toLower(start) and lc <= toLower(end);
    }
    return c >= start and c <= end;
}

fn isWordChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_';
}

/// Parse flags string (e.g., "i", "im", "m")
pub fn parseFlags(flags_str: ?[]const u8) Regex.Flags {
    var flags = Regex.Flags{};
    if (flags_str) |fs| {
        for (fs) |c| {
            switch (c) {
                'i' => flags.case_insensitive = true,
                'm' => flags.multiline = true,
                else => {},
            }
        }
    }
    return flags;
}

test "basic literal match" {
    const r = Regex.init("hello", .{});
    try std.testing.expect(r.matches("hello world"));
    try std.testing.expect(r.matches("say hello"));
    try std.testing.expect(!r.matches("goodbye"));
}

test "anchors" {
    const r1 = Regex.init("^hello", .{});
    try std.testing.expect(r1.matches("hello world"));
    try std.testing.expect(!r1.matches("say hello"));

    const r2 = Regex.init("world$", .{});
    try std.testing.expect(r2.matches("hello world"));
    try std.testing.expect(!r2.matches("world hello"));
}

test "character classes" {
    const r = Regex.init("[0-9]+", .{});
    try std.testing.expect(r.matches("abc123def"));
    try std.testing.expect(!r.matches("abcdef"));
}

test "quantifiers" {
    const r1 = Regex.init("a+", .{});
    try std.testing.expect(r1.matches("aaa"));
    try std.testing.expect(!r1.matches("bbb"));

    const r2 = Regex.init("^N[0-9]{8}$", .{});
    try std.testing.expect(r2.matchesFull("N12345678"));
    try std.testing.expect(!r2.matchesFull("N123456789")); // 9 digits
}

test "case insensitive" {
    const r = Regex.init("hello", .{ .case_insensitive = true });
    try std.testing.expect(r.matches("HELLO"));
    try std.testing.expect(r.matches("Hello"));
}

test "dot wildcard" {
    const r = Regex.init("a.c", .{});
    try std.testing.expect(r.matches("abc"));
    try std.testing.expect(r.matches("aXc"));
    try std.testing.expect(!r.matches("ac"));
}

test "matchesFull" {
    const r = Regex.init("hello", .{});
    try std.testing.expect(r.matchesFull("hello"));
    try std.testing.expect(!r.matchesFull("hello world"));
}

test "alternation" {
    const r = Regex.init("cat|dog", .{});
    try std.testing.expect(r.matches("cat"));
    try std.testing.expect(r.matches("dog"));
    try std.testing.expect(!r.matches("bird"));
}

test "replaceAll basic" {
    const allocator = std.testing.allocator;
    const r = Regex.init("aa", .{});
    const result = try r.replaceAll(allocator, "aaabaa", "\"aa\"");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("\"aa\"ab\"aa\"", result);
}

test "word boundary" {
    const r = Regex.init("\\bworld\\b", .{});
    try std.testing.expect(r.matches("hello world"));
    try std.testing.expect(!r.matches("helloworld"));
}

test "capture groups" {
    const allocator = std.testing.allocator;
    const r = Regex.init("(.)(.)(.)", .{});
    const result = try r.replaceAll(allocator, "abc", "$3$2$1");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("cba", result);
}
