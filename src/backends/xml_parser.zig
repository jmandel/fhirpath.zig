//! Minimal XML DOM parser for FHIR XML documents.
//!
//! Parses well-formed XML into a tree of elements with:
//! - Element names (local name, no prefix)
//! - Attributes (name + value pairs)
//! - Text content
//! - Child elements
//!
//! Arena-allocated — fits the engine's arena pattern.
//! No support for: DTD, processing instructions, CDATA (FHIR XML doesn't use these meaningfully).

const std = @import("std");

pub const Attribute = struct {
    name: []const u8, // local name (no prefix for xmlns attrs, full "xmlns:prefix" for xmlns decls)
    value: []const u8,
};

pub const XmlNode = struct {
    tag: []const u8, // local name (no prefix)
    attributes: []const Attribute, // name=value pairs (excludes xmlns declarations)
    children: []const *XmlNode, // child elements
    text: ?[]const u8, // concatenated text content (null if none)
};

pub const ParseError = error{
    UnexpectedEof,
    InvalidSyntax,
    MismatchedTag,
    OutOfMemory,
};

pub fn parse(allocator: std.mem.Allocator, input: []const u8) ParseError!*XmlNode {
    var parser = Parser{
        .allocator = allocator,
        .input = input,
        .pos = 0,
    };
    // Skip XML declaration if present
    parser.skipProlog();
    return parser.parseElement();
}

const Parser = struct {
    allocator: std.mem.Allocator,
    input: []const u8,
    pos: usize,

    fn skipProlog(self: *Parser) void {
        self.skipWhitespace();
        // Skip XML declaration: <?xml ... ?>
        if (self.startsWith("<?")) {
            if (std.mem.indexOf(u8, self.input[self.pos..], "?>")) |end| {
                self.pos += end + 2;
            }
        }
        self.skipWhitespace();
        // Skip any comments before root element
        while (self.startsWith("<!--")) {
            if (std.mem.indexOf(u8, self.input[self.pos..], "-->")) |end| {
                self.pos += end + 3;
            } else break;
            self.skipWhitespace();
        }
    }

    fn parseElement(self: *Parser) ParseError!*XmlNode {
        self.skipWhitespace();

        // Expect '<'
        if (self.pos >= self.input.len or self.input[self.pos] != '<') {
            return ParseError.InvalidSyntax;
        }
        self.pos += 1;

        // Read tag name (may include prefix)
        const full_tag = self.readName() orelse return ParseError.InvalidSyntax;
        const tag = stripPrefix(full_tag);

        // Parse attributes
        var attrs = std.ArrayListUnmanaged(Attribute){};
        defer attrs.deinit(self.allocator);

        while (true) {
            self.skipWhitespace();
            if (self.pos >= self.input.len) return ParseError.UnexpectedEof;
            if (self.input[self.pos] == '/' or self.input[self.pos] == '>') break;

            const attr_name = self.readName() orelse return ParseError.InvalidSyntax;
            self.skipWhitespace();
            if (self.pos >= self.input.len or self.input[self.pos] != '=') return ParseError.InvalidSyntax;
            self.pos += 1;
            self.skipWhitespace();
            const attr_value = self.readQuotedValue() orelse return ParseError.InvalidSyntax;

            // Skip xmlns declarations but keep all other attributes
            if (std.mem.eql(u8, attr_name, "xmlns") or std.mem.startsWith(u8, attr_name, "xmlns:")) {
                continue;
            }
            // Strip prefix from attribute names too
            const local_attr_name = stripPrefix(attr_name);
            try attrs.append(self.allocator, .{ .name = local_attr_name, .value = attr_value });
        }

        if (self.pos >= self.input.len) return ParseError.UnexpectedEof;

        const node = try self.allocator.create(XmlNode);

        // Self-closing tag: <tag ... />
        if (self.input[self.pos] == '/') {
            self.pos += 1;
            if (self.pos >= self.input.len or self.input[self.pos] != '>') return ParseError.InvalidSyntax;
            self.pos += 1;
            node.* = .{
                .tag = tag,
                .attributes = try attrs.toOwnedSlice(self.allocator),
                .children = &.{},
                .text = null,
            };
            return node;
        }

        // Opening tag: <tag ...>
        if (self.input[self.pos] != '>') return ParseError.InvalidSyntax;
        self.pos += 1;

        // Parse children and text content
        var children = std.ArrayListUnmanaged(*XmlNode){};
        defer children.deinit(self.allocator);
        var text_parts = std.ArrayListUnmanaged([]const u8){};
        defer text_parts.deinit(self.allocator);

        while (true) {
            if (self.pos >= self.input.len) return ParseError.UnexpectedEof;

            if (self.startsWith("</")) {
                // Closing tag
                self.pos += 2;
                const close_full_tag = self.readName() orelse return ParseError.InvalidSyntax;
                const close_tag = stripPrefix(close_full_tag);
                self.skipWhitespace();
                if (self.pos >= self.input.len or self.input[self.pos] != '>') return ParseError.InvalidSyntax;
                self.pos += 1;

                if (!std.mem.eql(u8, tag, close_tag)) {
                    return ParseError.MismatchedTag;
                }
                break;
            } else if (self.startsWith("<!--")) {
                // Skip comment
                if (std.mem.indexOf(u8, self.input[self.pos..], "-->")) |end| {
                    self.pos += end + 3;
                } else {
                    return ParseError.UnexpectedEof;
                }
            } else if (self.startsWith("<![CDATA[")) {
                // Handle CDATA section
                self.pos += 9; // skip "<![CDATA["
                if (std.mem.indexOf(u8, self.input[self.pos..], "]]>")) |end| {
                    const cdata_text = self.input[self.pos .. self.pos + end];
                    try text_parts.append(self.allocator, cdata_text);
                    self.pos += end + 3;
                } else {
                    return ParseError.UnexpectedEof;
                }
            } else if (self.input[self.pos] == '<') {
                // Child element
                const child = try self.parseElement();
                try children.append(self.allocator, child);
            } else {
                // Text content
                const text_start = self.pos;
                while (self.pos < self.input.len and self.input[self.pos] != '<') {
                    self.pos += 1;
                }
                if (self.pos > text_start) {
                    const raw_text = self.input[text_start..self.pos];
                    // Only add non-whitespace text
                    const trimmed = std.mem.trim(u8, raw_text, " \t\n\r");
                    if (trimmed.len > 0) {
                        const decoded = try self.decodeEntities(raw_text);
                        try text_parts.append(self.allocator, decoded);
                    }
                }
            }
        }

        // Concatenate text parts
        var text: ?[]const u8 = null;
        if (text_parts.items.len > 0) {
            if (text_parts.items.len == 1) {
                text = text_parts.items[0];
            } else {
                var total_len: usize = 0;
                for (text_parts.items) |part| total_len += part.len;
                const buf = try self.allocator.alloc(u8, total_len);
                var offset: usize = 0;
                for (text_parts.items) |part| {
                    @memcpy(buf[offset .. offset + part.len], part);
                    offset += part.len;
                }
                text = buf;
            }
        }

        node.* = .{
            .tag = tag,
            .attributes = try attrs.toOwnedSlice(self.allocator),
            .children = try children.toOwnedSlice(self.allocator),
            .text = text,
        };
        return node;
    }

    fn readName(self: *Parser) ?[]const u8 {
        const start = self.pos;
        while (self.pos < self.input.len) {
            const c = self.input[self.pos];
            if (c == ' ' or c == '\t' or c == '\n' or c == '\r' or
                c == '=' or c == '>' or c == '/' or c == '<')
            {
                break;
            }
            self.pos += 1;
        }
        if (self.pos == start) return null;
        return self.input[start..self.pos];
    }

    fn readQuotedValue(self: *Parser) ?[]const u8 {
        if (self.pos >= self.input.len) return null;
        const quote = self.input[self.pos];
        if (quote != '"' and quote != '\'') return null;
        self.pos += 1;

        const start = self.pos;
        while (self.pos < self.input.len and self.input[self.pos] != quote) {
            self.pos += 1;
        }
        if (self.pos >= self.input.len) return null;
        const value = self.input[start..self.pos];
        self.pos += 1; // skip closing quote

        // Decode entities in attribute values
        if (std.mem.indexOf(u8, value, "&") != null) {
            return self.decodeEntities(value) catch value;
        }
        return value;
    }

    fn decodeEntities(self: *Parser, input: []const u8) ParseError![]const u8 {
        if (std.mem.indexOf(u8, input, "&") == null) return input;

        var buf = std.ArrayListUnmanaged(u8){};
        defer buf.deinit(self.allocator);

        var i: usize = 0;
        while (i < input.len) {
            if (input[i] == '&') {
                if (std.mem.indexOf(u8, input[i..], ";")) |end| {
                    const entity = input[i + 1 .. i + end];
                    if (std.mem.eql(u8, entity, "amp")) {
                        try buf.append(self.allocator, '&');
                    } else if (std.mem.eql(u8, entity, "lt")) {
                        try buf.append(self.allocator, '<');
                    } else if (std.mem.eql(u8, entity, "gt")) {
                        try buf.append(self.allocator, '>');
                    } else if (std.mem.eql(u8, entity, "quot")) {
                        try buf.append(self.allocator, '"');
                    } else if (std.mem.eql(u8, entity, "apos")) {
                        try buf.append(self.allocator, '\'');
                    } else if (entity.len > 1 and entity[0] == '#') {
                        // Numeric character reference
                        const num_str = entity[1..];
                        const codepoint = if (num_str.len > 1 and (num_str[0] == 'x' or num_str[0] == 'X'))
                            std.fmt.parseInt(u21, num_str[1..], 16) catch 0xFFFD
                        else
                            std.fmt.parseInt(u21, num_str, 10) catch 0xFFFD;
                        var utf8_buf: [4]u8 = undefined;
                        const len = std.unicode.utf8Encode(codepoint, &utf8_buf) catch 0;
                        if (len > 0) {
                            try buf.appendSlice(self.allocator, utf8_buf[0..len]);
                        }
                    } else {
                        // Unknown entity — pass through
                        try buf.append(self.allocator, '&');
                        try buf.appendSlice(self.allocator, entity);
                        try buf.append(self.allocator, ';');
                    }
                    i += end + 1;
                } else {
                    try buf.append(self.allocator, input[i]);
                    i += 1;
                }
            } else {
                try buf.append(self.allocator, input[i]);
                i += 1;
            }
        }
        return try buf.toOwnedSlice(self.allocator);
    }

    fn skipWhitespace(self: *Parser) void {
        while (self.pos < self.input.len) {
            const c = self.input[self.pos];
            if (c != ' ' and c != '\t' and c != '\n' and c != '\r') break;
            self.pos += 1;
        }
    }

    fn startsWith(self: *Parser, prefix: []const u8) bool {
        if (self.pos + prefix.len > self.input.len) return false;
        return std.mem.eql(u8, self.input[self.pos .. self.pos + prefix.len], prefix);
    }
};

fn stripPrefix(name: []const u8) []const u8 {
    if (std.mem.indexOfScalar(u8, name, ':')) |colon| {
        return name[colon + 1 ..];
    }
    return name;
}

/// Serialize an XmlNode and its children back to an XML string.
/// Used for XHTML div serialization.
pub fn serializeNode(allocator: std.mem.Allocator, xml_node: *const XmlNode) error{OutOfMemory}![]const u8 {
    var buf = std.ArrayListUnmanaged(u8){};
    defer buf.deinit(allocator);
    try serializeNodeInto(allocator, &buf, xml_node);
    return try buf.toOwnedSlice(allocator);
}

fn serializeNodeInto(allocator: std.mem.Allocator, buf: *std.ArrayListUnmanaged(u8), xml_node: *const XmlNode) error{OutOfMemory}!void {
    try buf.appendSlice(allocator, "<");
    try buf.appendSlice(allocator, xml_node.tag);
    for (xml_node.attributes) |attr| {
        try buf.appendSlice(allocator, " ");
        try buf.appendSlice(allocator, attr.name);
        try buf.appendSlice(allocator, "=\"");
        try appendEscaped(allocator, buf, attr.value);
        try buf.appendSlice(allocator, "\"");
    }

    const has_content = xml_node.children.len > 0 or xml_node.text != null;
    if (!has_content) {
        try buf.appendSlice(allocator, "/>");
        return;
    }

    try buf.appendSlice(allocator, ">");
    if (xml_node.text) |t| {
        try appendEscaped(allocator, buf, t);
    }
    for (xml_node.children) |child| {
        try serializeNodeInto(allocator, buf, child);
    }
    try buf.appendSlice(allocator, "</");
    try buf.appendSlice(allocator, xml_node.tag);
    try buf.appendSlice(allocator, ">");
}

fn appendEscaped(allocator: std.mem.Allocator, buf: *std.ArrayListUnmanaged(u8), text: []const u8) error{OutOfMemory}!void {
    for (text) |c| {
        switch (c) {
            '&' => try buf.appendSlice(allocator, "&amp;"),
            '<' => try buf.appendSlice(allocator, "&lt;"),
            '>' => try buf.appendSlice(allocator, "&gt;"),
            '"' => try buf.appendSlice(allocator, "&quot;"),
            else => try buf.append(allocator, c),
        }
    }
}

// ── Tests ────────────────────────────────────────────────────────────

test "parse simple element" {
    const input = "<Patient><name value=\"John\"/></Patient>";
    const doc = try parse(std.testing.allocator, input);
    // We can't easily free arena-style, but for tests with testing.allocator
    // we accept leaks in these small tests. The real usage is arena-allocated.
    defer {
        std.testing.allocator.free(doc.children[0].attributes);
        std.testing.allocator.destroy(doc.children[0]);
        std.testing.allocator.free(doc.children);
        std.testing.allocator.destroy(doc);
    }
    try std.testing.expectEqualStrings("Patient", doc.tag);
    try std.testing.expectEqual(@as(usize, 1), doc.children.len);
    try std.testing.expectEqualStrings("name", doc.children[0].tag);
    try std.testing.expectEqualStrings("value", doc.children[0].attributes[0].name);
    try std.testing.expectEqualStrings("John", doc.children[0].attributes[0].value);
}

test "parse with namespace prefix" {
    const input = "<f:Patient xmlns:f=\"http://hl7.org/fhir\"><f:id value=\"123\"/></f:Patient>";
    const doc = try parse(std.testing.allocator, input);
    defer {
        std.testing.allocator.free(doc.children[0].attributes);
        std.testing.allocator.destroy(doc.children[0]);
        std.testing.allocator.free(doc.children);
        std.testing.allocator.destroy(doc);
    }
    try std.testing.expectEqualStrings("Patient", doc.tag);
    try std.testing.expectEqual(@as(usize, 1), doc.children.len);
    try std.testing.expectEqualStrings("id", doc.children[0].tag);
}

test "parse self-closing element" {
    const input = "<active value=\"true\"/>";
    const doc = try parse(std.testing.allocator, input);
    defer {
        std.testing.allocator.free(doc.attributes);
        std.testing.allocator.destroy(doc);
    }
    try std.testing.expectEqualStrings("active", doc.tag);
    try std.testing.expectEqualStrings("true", doc.attributes[0].value);
    try std.testing.expectEqual(@as(usize, 0), doc.children.len);
}

test "parse text content" {
    const input = "<div>Hello World</div>";
    const doc = try parse(std.testing.allocator, input);
    defer {
        std.testing.allocator.destroy(doc);
    }
    try std.testing.expectEqualStrings("div", doc.tag);
    try std.testing.expectEqualStrings("Hello World", doc.text.?);
}

test "parse xml declaration" {
    const input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Patient/>";
    const doc = try parse(std.testing.allocator, input);
    defer {
        std.testing.allocator.destroy(doc);
    }
    try std.testing.expectEqualStrings("Patient", doc.tag);
}

test "parse entity decoding" {
    const input = "<text value=\"a &amp; b &lt; c\"/>";
    const doc = try parse(std.testing.allocator, input);
    defer {
        std.testing.allocator.free(doc.attributes[0].value);
        std.testing.allocator.free(doc.attributes);
        std.testing.allocator.destroy(doc);
    }
    try std.testing.expectEqualStrings("a & b < c", doc.attributes[0].value);
}

test "serialize node roundtrip" {
    const input = "<div><p>Hello</p></div>";
    const doc = try parse(std.testing.allocator, input);
    defer {
        std.testing.allocator.destroy(doc.children[0]);
        std.testing.allocator.free(doc.children);
        std.testing.allocator.destroy(doc);
    }
    const serialized = try serializeNode(std.testing.allocator, doc);
    defer std.testing.allocator.free(serialized);
    try std.testing.expectEqualStrings("<div><p>Hello</p></div>", serialized);
}
