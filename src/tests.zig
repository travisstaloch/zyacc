const std = @import("std");
const bnf = @import("main.zig");

const t = std.testing;
test "zig grammar" {
    const src = @embedFile("../../grammarz/samples/zig.grammar");
    // const src = @embedFile("../samples/test.grammar");

    // comptime var prods: []const Production = &.{};
    var prods = try bnf.parse(t.allocator, t.failing_allocator, src);
    defer bnf.parseFree(t.allocator, prods);

    for (prods) |prod| {
        std.debug.print("'{s}' <- '{}'\n", .{ prod.name, prod.rule.root });
    }
}

test "TopLevelDecl" {
    const src =
        \\TopLevelDecl
        \\    <- (KEYWORD_export / KEYWORD_extern STRINGLITERALSINGLE? / (KEYWORD_inline / KEYWORD_noinline))? FnProto (SEMICOLON / Block)
        \\     / (KEYWORD_export / KEYWORD_extern STRINGLITERALSINGLE?)? KEYWORD_threadlocal? VarDecl
        \\     / KEYWORD_usingnamespace Expr SEMICOLON
    ;
    var prods = try bnf.parse(t.allocator, t.failing_allocator, src);
    defer bnf.parseFree(t.allocator, prods);

    try t.expectEqual(@as(usize, 1), prods.len);
    try t.expectEqual(@as(usize, 3), prods[0].rule.root.choice.len);
    try t.expect(prods[0].rule.root.choice[0] == .seq);
    try t.expectEqual(@as(usize, 3), prods[0].rule.root.choice[0].seq.len);
    try t.expect(prods[0].rule.root.choice[0].seq[0] == .optional);
    try t.expect(prods[0].rule.root.choice[0].seq[0].optional.* == .group);
    try t.expectEqual(@as(usize, 1), prods[0].rule.root.choice[0].seq[0].optional.group.len);
    try t.expect(prods[0].rule.root.choice[0].seq[0].optional.group[0] == .choice);
    try t.expectEqual(@as(usize, 3), prods[0].rule.root.choice[0].seq[0].optional.group[0].choice.len);
    try t.expect(prods[0].rule.root.choice[0].seq[0].optional.group[0].choice[0] == .name);
    try t.expect(prods[0].rule.root.choice[0].seq[0].optional.group[0].choice[1] == .seq);
    try t.expect(prods[0].rule.root.choice[0].seq[0].optional.group[0].choice[2] == .group);
    try t.expectEqual(@as(usize, 1), prods[0].rule.root.choice[0].seq[0].optional.group[0].choice[2].group.len);
    try t.expect(prods[0].rule.root.choice[0].seq[0].optional.group[0].choice[2].group[0] == .choice);
    try t.expectEqual(@as(usize, 2), prods[0].rule.root.choice[0].seq[0].optional.group[0].choice[2].group[0].choice.len);
    try t.expectEqual(@as(usize, 3), prods[0].rule.root.choice[1].seq.len);
    try t.expectEqual(@as(usize, 3), prods[0].rule.root.choice[2].seq.len);
}

test "keyword" {
    const src =
        \\keyword <- KEYWORD_align / KEYWORD_allowzero
        \\         / KEYWORD_anytype / KEYWORD_asm / KEYWORD_async / KEYWORD_await
    ;

    var prods = try bnf.parse(t.allocator, t.failing_allocator, src);
    defer bnf.parseFree(t.allocator, prods);

    // std.debug.print("{any}\n", .{prods[0].rule.root.choice});
    try t.expectEqual(@as(usize, 6), prods[0].rule.root.choice.len);

    try t.expect(prods[0].rule.root.choice[0] == .name);
    try t.expect(prods[0].rule.root.choice[1] == .name);
    try t.expect(prods[0].rule.root.choice[2] == .name);
    try t.expect(prods[0].rule.root.choice[3] == .name);
    try t.expect(prods[0].rule.root.choice[4] == .name);
    try t.expect(prods[0].rule.root.choice[5] == .name);
}

test "literals" {
    {
        const src =
            \\optchar0 <- '0'?
            \\nsqbkt0 <- ![0-9]
            \\str0 <- "asdf"
            \\str1 <- "asdf"'asdf'[asdf]
        ;
        var prods = try bnf.parse(t.allocator, t.failing_allocator, src);
        defer bnf.parseFree(t.allocator, prods);
        // for (prods) |p|
        //     std.debug.print("{s} <- {}\n", .{ p.name, p.rule.root });
        try t.expectEqual(@as(usize, 4), prods.len);

        try t.expect(prods[0].rule.root == .choice);
        try t.expect(prods[0].rule.root.choice.len == 1);
        try t.expect(prods[0].rule.root.choice[0] == .optional);
        try t.expect(prods[0].rule.root.choice[0].optional.* == .char_lit);
        try t.expect(prods[0].rule.root.choice[0].optional.char_lit.len == 1);

        try t.expect(prods[1].rule.root == .choice);
        try t.expect(prods[1].rule.root.choice.len == 1);
        try t.expect(prods[1].rule.root.choice[0] == .not);
        try t.expect(prods[1].rule.root.choice[0].not.* == .sqbkt_lit);
        try t.expect(prods[1].rule.root.choice[0].not.sqbkt_lit.len == 3);

        try t.expect(prods[2].rule.root == .choice);
        try t.expect(prods[2].rule.root.choice.len == 1);
        try t.expect(prods[2].rule.root.choice[0] == .str_lit);
        try t.expect(prods[2].rule.root.choice[0].str_lit.len == 4);

        try t.expect(prods[3].rule.root == .choice);
        try t.expect(prods[3].rule.root.choice.len == 1);
        try t.expect(prods[3].rule.root.choice[0] == .seq);
        try t.expect(prods[3].rule.root.choice[0].seq.len == 3);
        try t.expect(prods[3].rule.root.choice[0].seq[0] == .str_lit);
        try t.expect(prods[3].rule.root.choice[0].seq[1] == .char_lit);
        try t.expect(prods[3].rule.root.choice[0].seq[2] == .sqbkt_lit);
    }
}

test "literal escapes" {
    {
        const src =
            \\asdf <- ('//!' '\n' '\\')+
        ;
        var prods = try bnf.parse(t.allocator, t.failing_allocator, src);
        defer bnf.parseFree(t.allocator, prods);
        for (prods) |p|
            std.debug.print("{s} <- {}\n", .{ p.name, p.rule.root });
        try t.expect(prods.len == 1);
        try t.expect(prods[0].rule.root == .choice);
        try t.expect(prods[0].rule.root.choice.len == 1);
        try t.expect(prods[0].rule.root.choice[0] == .some);
        try t.expect(prods[0].rule.root.choice[0].some.* == .group);
        try t.expect(prods[0].rule.root.choice[0].some.group.len == 3);
        try t.expect(prods[0].rule.root.choice[0].some.group[1] == .char_lit);
        // TODO: this should be len == 1 w/ value == 10, not .{'\\', 'n'}
        try t.expect(prods[0].rule.root.choice[0].some.group[1].char_lit.len == 2);
    }
}
