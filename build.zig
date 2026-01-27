const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lib_module = b.createModule(.{
        .root_source_file = b.path("src/lib.zig"),
        .target = target,
        .optimize = optimize,
    });

    const tests = b.addTest(.{
        .root_module = lib_module,
    });
    const run_tests = b.addRunArtifact(tests);
    b.step("test", "Run library tests").dependOn(&run_tests.step);

    const harness_module = b.createModule(.{
        .root_source_file = b.path("src/harness.zig"),
        .target = target,
        .optimize = optimize,
    });
    const harness = b.addExecutable(.{
        .name = "harness",
        .root_module = harness_module,
    });
    b.installArtifact(harness);

    const run_harness = b.addRunArtifact(harness);
    if (b.args) |args| run_harness.addArgs(args);
    b.step("harness", "Run artisinal test harness").dependOn(&run_harness.step);

    const cli_module = b.createModule(.{
        .root_source_file = b.path("src/cli.zig"),
        .target = target,
        .optimize = optimize,
    });
    const cli = b.addExecutable(.{
        .name = "fhirpath",
        .root_module = cli_module,
    });
    b.installArtifact(cli);

    const run_cli = b.addRunArtifact(cli);
    if (b.args) |args| run_cli.addArgs(args);
    b.step("run", "Run FHIRPath CLI").dependOn(&run_cli.step);

    const official_module = b.createModule(.{
        .root_source_file = b.path("src/official_tests.zig"),
        .target = target,
        .optimize = optimize,
    });
    const official = b.addExecutable(.{
        .name = "official-tests",
        .root_module = official_module,
    });
    b.installArtifact(official);

    const run_official = b.addRunArtifact(official);
    if (b.args) |args| run_official.addArgs(args);
    b.step("official-tests", "Run official FHIRPath test runner").dependOn(&run_official.step);

    const build_model_module = b.createModule(.{
        .root_source_file = b.path("scripts/build_model.zig"),
        .target = target,
        .optimize = optimize,
    });
    const build_model = b.addExecutable(.{
        .name = "build-model",
        .root_module = build_model_module,
    });
    b.installArtifact(build_model);

    const run_build_model = b.addRunArtifact(build_model);
    if (b.args) |args| run_build_model.addArgs(args);
    b.step("build-model", "Build schema model from FHIR StructureDefinitions").dependOn(&run_build_model.step);
}
