open Assemblage

let lib_pkgs = [
  pkg "xmldiff";
]
let cli_pkgs = [pkg "cmdliner"]
let bin_pkgs = [pkg "cmdliner"]

let xedInput = unit "xedInput" (`Path ["lib"])
let xedPath = unit "xedPath" (`Path ["lib"])
let xedSchema = unit "xedSchema" (`Path ["lib"])
let xedDiff = unit "xedDiff" (`Path ["lib"])

let xedTest = unit "xedTest" (`Path ["test"])

let xedCli = unit "xedCli" (`Path ["cli"])
let xed = unit "xed" (`Path ["cli"])

let l = lib ~deps:lib_pkgs "xed" (`Units [
  xedInput;
  xedPath;
  xedSchema;
  xedDiff;
])
let xed_test_bin = bin ~deps:[l] "xed-test" (`Units [
  xedTest;
])
let t = test "xed-test" [test_bin xed_test_bin ()]
let c = lib ~deps:(l :: cli_pkgs) "cli" (`Units [
  xedCli;
])
let b = bin ~deps:(c :: bin_pkgs) "xed" (`Units [
  xed;
])

let () = assemble (project "xed" [l;t;c;b])
