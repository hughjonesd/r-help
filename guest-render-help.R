#! Rscript

# This script is run by versions of R which use compiled help databases.

rv <- getRVersion()
version <- paste(rv$major, rv$minor, sep = ".")
libraryDir <- R.home("library")
versionDir <- file.path("site", version)

for (pkg in list.files(libraryDir)) {
  helpDir <- file.path(libraryDir, pkg, "help")
  indexFile <- file.path(helpDir, "AnIndex")
  database <- file.path(helpDir, paste(pkg, "rdb", sep = "."))
  if (! file.exists(indexFile) || ! file.exists(database)) next

  indexLines <- readLines(indexFile, warn = FALSE)
  indexBits <- strsplit(indexLines, "\t", fixed = TRUE)
  indexBits <- indexBits[unlist(lapply(indexBits, length)) >= 2L]
  topics <- unique(unlist(lapply(indexBits, function(x) x[2L])))

  rdDatabase <- try(
    tools:::fetchRdDB(file.path(helpDir, pkg)),
    silent = TRUE
  )
  if (inherits(rdDatabase, "try-error")) {
    warning("Could not read help for ", pkg)
    next
  }
  topics <- topics[topics %in% names(rdDatabase)]

  packageDir <- file.path(versionDir, pkg)
  dir.create(packageDir, recursive = TRUE, showWarnings = FALSE)
  for (topic in topics) {
    outputFile <- file.path(packageDir, paste(topic, "html", sep = "."))
    rendered <- try(
      tools::Rd2HTML(
        rdDatabase[[topic]],
        out = outputFile,
        package = pkg
      ),
      silent = TRUE
    )
    if (inherits(rendered, "try-error")) {
      if (file.exists(outputFile)) unlink(outputFile)
      warning("Could not render ", pkg, "::", topic)
    }
  }
}
