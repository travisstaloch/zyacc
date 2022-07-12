const std = @import("std");
const pkgs = @import("deps.zig").pkgs;

pub fn build(b: *std.build.Builder) void {
    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();
    const lib = b.addStaticLibrary("zyacc", "src/main.zig");
    pkgs.addAllTo(lib);
    lib.setBuildMode(mode);
    lib.install();

    const main_tests = b.addTest("src/tests.zig");
    pkgs.addAllTo(main_tests);
    main_tests.setBuildMode(mode);
    // main_tests.linkLibC();

    var filter: []const u8 = "";
    // filter = "slr1Table parser";
    main_tests.filter = filter;

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);
}
