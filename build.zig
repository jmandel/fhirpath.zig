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

    // Benchmark executable
    const bench_module = b.createModule(.{
        .root_source_file = b.path("src/bench.zig"),
        .target = target,
        .optimize = .ReleaseFast,
    });
    const bench = b.addExecutable(.{
        .name = "bench",
        .root_module = bench_module,
    });
    b.installArtifact(bench);

    const run_bench = b.addRunArtifact(bench);
    if (b.args) |args| run_bench.addArgs(args);
    b.step("bench", "Run performance benchmarks").dependOn(&run_bench.step);

    // Adapter verification test
    const verify_module = b.createModule(.{
        .root_source_file = b.path("src/verify_adapters.zig"),
        .target = target,
        .optimize = optimize,
    });
    const verify = b.addExecutable(.{
        .name = "verify-adapters",
        .root_module = verify_module,
    });
    b.installArtifact(verify);
    const run_verify = b.addRunArtifact(verify);
    b.step("verify-adapters", "Verify adapter correctness").dependOn(&run_verify.step);

    // Adapter overhead benchmark
    const overhead_module = b.createModule(.{
        .root_source_file = b.path("src/bench_overhead.zig"),
        .target = target,
        .optimize = .ReleaseFast,
    });
    const overhead = b.addExecutable(.{
        .name = "bench-overhead",
        .root_module = overhead_module,
    });
    b.installArtifact(overhead);
    const run_overhead = b.addRunArtifact(overhead);
    b.step("bench-overhead", "Benchmark adapter overhead vs direct access").dependOn(&run_overhead.step);

    // WebAssembly build (ABI exports)
    const wasm_target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding,
    });
    const wasm_module = b.createModule(.{
        .root_source_file = b.path("src/wasm.zig"),
        .target = wasm_target,
        .optimize = .ReleaseSmall,
    });
    const wasm = b.addExecutable(.{
        .name = "fhirpath",
        .root_module = wasm_module,
    });
    wasm.export_memory = true;
    wasm.rdynamic = true;
    wasm.entry = .disabled;
    const wasm_install = b.addInstallArtifact(wasm, .{});
    b.step("wasm", "Build WebAssembly module").dependOn(&wasm_install.step);
}
