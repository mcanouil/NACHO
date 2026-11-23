# Temporary diagnostic for the R-devel check, where S7.rdb vanishes mid-run.
# testthat compares this state before and after each test file and warns,
# naming the file, when it changes.
testthat::set_state_inspector(function() {
  s7_path <- getNamespaceInfo("S7", "path")
  list(
    s7_path = s7_path,
    s7_rdb_exists = file.exists(file.path(s7_path, "R", "S7.rdb")),
    s7_link_target = Sys.readlink(s7_path),
    s7_lib_entries = length(list.files(dirname(s7_path))),
    lib_paths = .libPaths(),
    r_libs = Sys.getenv("R_LIBS"),
    tempdir_exists = dir.exists(tempdir())
  )
})
