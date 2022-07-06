const std = @import("std");
const g = @import("main.zig");
const assert = std.debug.assert;
const t = std.testing;
// const allr = t.allocator;
const fallr = t.failing_allocator;
var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
const allr = arena.allocator();

test "tokenize" {
    // const src = @embedFile("../samples/zig.grammar");
    const src =
        \\Root <- skip container_doc_comment? ContainerMembers eof
        \\
        \\# *** Top level ***
        \\ContainerMembers <- ContainerDeclarations (ContainerField COMMA)* (ContainerField / ContainerDeclarations)
        \\
        \\keyword <- KEYWORD_align / KEYWORD_allowzero / KEYWORD_and / KEYWORD_anyframe
    ;
    var grammar = try g.Grammar.init(allr, fallr, src);
    defer grammar.deinit(allr);
    // for (grammar.productions.items) |p, i| {
    //     std.debug.print("{}: {s}-{}\n", .{ i, grammar.name(p.name.id), p.name.id });
    // }

    const productions = grammar.productions;
    try t.expectEqual(@as(usize, 6), productions.items.len);
    {
        // skip container_doc_comment? ContainerMembers eof
        const prod = productions.items[0];
        try t.expectEqualStrings("Root", grammar.name(prod.name.id).?);
        try t.expectEqual(@as(usize, 5), prod.rule.items.len);
        try t.expectEqualStrings("skip", grammar.name(prod.rule.items[0].sym.token.id).?);
        try t.expectEqualStrings("eof", grammar.name(prod.rule.items[3].sym.token.id).?);
        const expected_tags: []const g.Token.Tag = &.{ .name, .name, .name, .name, .comment };
        try t.expectEqual(expected_tags.len, prod.rule.items.len);
        for (prod.rule.items) |tok, i| {
            if (i < expected_tags.len)
                try t.expectEqual(expected_tags[i], tok.sym.token.tag);
        }
        try t.expect(prod.rule.items[1].flags.contains(.optional));
    }
    {
        // ContainerDeclarations (ContainerField COMMA)* (ContainerField / ContainerDeclarations)
        const prod = productions.items[1];
        try t.expectEqual(@as(usize, 3), prod.rule.items.len);
        try t.expectEqual(g.Symbol.Sym.Tag.token, prod.rule.items[0].sym);
        try t.expectEqual(g.Token.Tag.name, prod.rule.items[0].sym.token.tag);

        try t.expectEqual(g.Symbol.Sym.Tag.group, prod.rule.items[1].sym);
        try t.expectEqual(@as(usize, 2), prod.rule.items[1].sym.group.items.len);
        try t.expect(prod.rule.items[1].flags.contains(.many));

        try t.expectEqual(g.Symbol.Sym.Tag.group, prod.rule.items[2].sym);
        // std.debug.print("{}\n", .{prod.rule.items[2].sym.group.items[0].sym});
        try t.expectEqual(@as(usize, 1), prod.rule.items[2].sym.group.items.len);
    }
    {
        // KEYWORD_align / KEYWORD_allowzero / KEYWORD_and / KEYWORD_anyframe
        var i: usize = 2;
        while (i < 6) : (i += 1) {
            const prod = productions.items[i];
            try t.expectEqual(@as(usize, 1), prod.rule.items.len);
        }
    }
}

test "itemSymbolId" {
    const src = @embedFile("../samples/xz.bnf");
    const expectItemSymId = struct {
        fn func(expected_id: ?g.SymbolId, id: g.SymbolId, pos: g.SymbolId, start: g.Production, grammar: g.Grammar) !void {
            const msym = grammar.itemSymbolId(g.Item.init(id, pos), start);
            try t.expectEqual(expected_id, msym);
        }
    }.func;
    var grammar = try g.Grammar.init(allr, fallr, src);
    defer grammar.deinit(allr);
    const start = grammar.start();
    const Sid = grammar.name_ids.get("S").?;
    const Eid = grammar.name_ids.get("E").?;
    const zid = grammar.name_ids.get("z").?;
    const xid = grammar.name_ids.get("x").?;
    try expectItemSymId(Sid, g.Automaton.augmented_id, 0, start, grammar);
    try expectItemSymId(null, g.Automaton.augmented_id, 1, start, grammar);
    try expectItemSymId(Eid, 0, 0, start, grammar);
    try expectItemSymId(null, 0, 1, start, grammar);
    try expectItemSymId(Eid, 1, 0, start, grammar);
    try expectItemSymId(xid, 1, 1, start, grammar);
    try expectItemSymId(Eid, 1, 2, start, grammar);
    try expectItemSymId(null, 1, 3, start, grammar);
    try expectItemSymId(zid, 2, 0, start, grammar);
    try expectItemSymId(null, 2, 1, start, grammar);
}

test "lr0 items" {
    const src = @embedFile("../samples/xz.bnf");
    var grammar = try g.Grammar.init(allr, fallr, src);
    defer grammar.deinit(allr);

    try t.expectEqual(@as(usize, 3), grammar.productions.items.len);

    var a = try g.lr0_automaton(allr, grammar);
    defer a.deinit(allr);

    const states = a.transitions.items;
    // std.debug.print("states {any}\n", .{g.Automaton.TransitionsFmt.init(a.transitions, grammar)});
    try t.expectEqual(@as(usize, 6), states.len);
    // std.debug.print("a.transitions count {any}\n", .{states.len});
    try t.expectEqualStrings("S'", grammar.name(grammar.augprod.name.id).?);
    const Spid = grammar.name_ids.get("S'").?;
    const Sid = grammar.name_ids.get("S").?;
    const Eid = grammar.name_ids.get("E").?;

    // 0 :: S':.S
    try t.expectEqual(@as(usize, 1), states[0].kernel.count());
    try t.expectEqual(g.Item.init(Spid, 0), states[0].kernel.keys()[0]);
    // 1 :: S':S.
    try t.expectEqual(@as(usize, 1), states[1].kernel.count());
    try t.expectEqual(g.Item.init(Spid, 1), states[1].kernel.keys()[0]);
    // 2 :: S:E. E:E.xE
    try t.expectEqual(@as(usize, 2), states[2].kernel.count());
    try t.expectEqual(g.Item.init(Sid, 1), states[2].kernel.keys()[0]);
    try t.expectEqual(g.Item.init(Eid, 1), states[2].kernel.keys()[1]);
    // 3 :: E:z.
    try t.expectEqual(@as(usize, 1), states[3].kernel.count());
    try t.expectEqual(g.Item.init(2, 1), states[3].kernel.keys()[0]);
    // 4 :: E:Ex.E
    try t.expectEqual(@as(usize, 1), states[4].kernel.count());
    try t.expectEqual(g.Item.init(Eid, 2), states[4].kernel.keys()[0]);
    // 5 :: E:ExE. E:E.xE
    try t.expectEqual(@as(usize, 2), states[5].kernel.count());
    try t.expectEqual(g.Item.init(Eid, 3), states[5].kernel.keys()[0]);
    try t.expectEqual(g.Item.init(Eid, 1), states[5].kernel.keys()[1]);
}

test "firsts" {
    const src = @embedFile("../samples/xz.bnf");
    var grammar = try g.Grammar.init(allr, fallr, src);
    defer grammar.deinit(allr);
    var firsts = try grammar.first(allr);
    defer firsts.map.deinit(allr);

    const Sid = grammar.name_ids.get("S").?;
    const Eid = grammar.name_ids.get("E").?;
    const zid = grammar.name_ids.get("z").?;

    try t.expectEqual(@as(usize, 2), firsts.map.count());
    try t.expect(firsts.map.contains(Sid));
    const firstS = firsts.map.get(Sid) orelse unreachable;
    // std.debug.print("{any}\n", .{firstS.keys()});
    try t.expectEqual(@as(usize, 1), firstS.count());
    try t.expect(firstS.contains(zid));

    try t.expect(firsts.map.contains(Eid));
    const firstE = firsts.map.get(Eid) orelse unreachable;
    try t.expectEqual(@as(usize, 1), firstE.count());
    try t.expect(firstE.contains(zid));
}

test "follows" {
    const src = @embedFile("../samples/xz.bnf");
    var grammar = try g.Grammar.init(allr, fallr, src);
    defer grammar.deinit(allr);
    var follows = try grammar.follow(allr);
    defer follows.map.deinit(allr);

    const Sid = grammar.name_ids.get("S").?;
    const Eid = grammar.name_ids.get("E").?;
    const xid = grammar.name_ids.get("x").?;
    // std.debug.print("follows {any}\n", .{follows.map.keys()});
    try t.expectEqual(@as(usize, 2), follows.map.count());
    {
        try t.expect(follows.map.contains(Sid));
        const followS = follows.map.get(Sid) orelse unreachable;
        try t.expectEqual(@as(usize, 1), followS.count());
        try t.expect(followS.contains(g.Grammar.END));
    }
    {
        try t.expect(follows.map.contains(Eid));
        const followE = follows.map.get(Eid) orelse unreachable;
        // for (followE.keys()) |k| std.debug.print("{s}\n", .{grammar.name(k)});
        try t.expectEqual(@as(usize, 2), followE.count());
        try t.expect(followE.contains(xid));
        try t.expect(followE.contains(g.Grammar.END));
    }
}

test "display productions" {
    const src = @embedFile("../samples/xz.bnf");
    var grammar = try g.Grammar.init(allr, fallr, src);
    defer grammar.deinit(allr);
    var l = std.ArrayList(u8).init(allr);
    defer l.deinit();
    const writer = l.writer();
    // for (grammar.productions.items) |p|
    try writer.print("{}", .{g.Production.Fmt.init(grammar.productions.items[0], grammar)});
    try t.expectEqualStrings("S <- E ", l.items);
    l.items.len = 0;
    try writer.print("{}", .{g.Production.Fmt.init(grammar.productions.items[1], grammar)});
    try t.expectEqualStrings("E <- E x E ", l.items);
    l.items.len = 0;
    try writer.print("{}", .{g.Production.Fmt.init(grammar.productions.items[2], grammar)});
    try t.expectEqualStrings("E <- z ", l.items);
    l.items.len = 0;
}

test "slr1Table" {
    const src = @embedFile("../samples/xz.bnf");
    var grammar = try g.Grammar.init(allr, fallr, src);
    defer grammar.deinit(allr);
    var table = try g.slr1Table(allr, grammar);
    defer g.tableFree(allr, &table);
    // std.debug.print("table {}\n", .{g.TableFmt.init(table, grammar)});
    const tablesrc =
        \\0-S-s1, 0-E-s2, 0-z-s3, 
        \\1-$-$accept, 
        \\2-x-s4, 2-$-r0, 
        \\3-x-r2, 3-$-r2, 
        \\4-E-s5, 4-z-s3, 
        \\5-x-s4, 5-x-r1, 5-$-r1, 
    ;
    var extable = try g.parseSlr1Table(allr, t.failing_allocator, tablesrc, grammar);
    defer g.tableFree(allr, &extable);
    try t.expectEqual(@as(usize, 6), extable.items.len);
    try t.expect(g.tablesEql(extable, table));
}
