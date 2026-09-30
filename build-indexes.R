#! Rscript

htmlEscape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}

versionDirs <- list.dirs("site", recursive = FALSE, full.names = TRUE)
indexedVersions <- character()

for (versionDir in versionDirs) {
  aliasFile <- file.path(versionDir, "aliases.tsv")
  if (! file.exists(aliasFile) || file.size(aliasFile) == 0L) next

  aliases <- read.delim(
    aliasFile,
    header = FALSE,
    quote = "",
    col.names = c("package", "name", "topic")
  )
  aliases <- unique(aliases[order(aliases$package, aliases$name), ])
  links <- sprintf(
    '<a data-package="%s" data-name="%s" href="%s/%s.html">%s::%s</a><br>',
    htmlEscape(aliases$package),
    htmlEscape(aliases$name),
    htmlEscape(aliases$package),
    htmlEscape(aliases$topic),
    htmlEscape(aliases$package),
    htmlEscape(aliases$name)
  )
  packages <- unique(aliases$package)
  for (package in packages) {
    first <- match(package, aliases$package)
    links[first] <- paste0('<h2 id="package-', htmlEscape(package), '">',
      htmlEscape(package), '</h2>', links[first])
  }

  version <- basename(versionDir)
  writeLines(c(
    "<!doctype html>",
    '<html lang="en"><head><meta charset="utf-8">',
    paste0("<title>R ", version, " help</title>"),
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    '<style>body { margin: 1.5rem 2rem; line-height: 1.4; } h2 { margin-bottom: .3rem; }</style>',
    '<script defer src="../navigation.js"></script></head><body>',
    paste0("<h1>R ", version, " help</h1>"),
    '<p id="missing" hidden>No help page was found.</p>',
    '<label>Package <select onchange="if (this.value) location.hash = this.value">',
    '<option value="">Select a package</option>',
    paste0('<option value="package-', htmlEscape(packages), '">',
      htmlEscape(packages), '</option>'),
    '</select></label>',
    links,
    "</body></html>"
  ), file.path(versionDir, "00index.html"))
  unlink(aliasFile)
  indexedVersions <- c(indexedVersions, version)

  # Keep historical markup, correcting paths for our version/package layout.
  for (page in list.files(versionDir, pattern = "\\.html$", recursive = TRUE,
      full.names = TRUE)) {
    if (dirname(page) == versionDir) next
    html <- paste(readLines(page, warn = FALSE), collapse = "\n")
    html <- gsub('(href=["\x27])\\.\\./\\.\\./([^/"\x27]+)/html/',
      '\\1../\\2/', html, ignore.case = TRUE)
    html <- sub('(<body\\b[^>]*>)',
      '\\1<script defer src="../../navigation.js"></script>',
      html, ignore.case = TRUE, perl = TRUE)
    writeLines(html, page, useBytes = TRUE)
  }
}

indexedVersions <- indexedVersions[
  order(as.package_version(indexedVersions), decreasing = TRUE)
]
writeLines(indexedVersions, "site/versions.txt")
file.copy("navigation.js", "site/navigation.js", overwrite = TRUE)
links <- paste0(
  '<a href="', indexedVersions, '/00index.html">R ',
  indexedVersions, "</a><br>"
)
writeLines(c(
  "<!doctype html>",
  '<html lang="en"><head><meta charset="utf-8">',
  '<title>Historical R help</title><style>body { margin: 1.5rem 2rem; line-height: 1.4; }</style></head><body>',
  "<h1>Historical R help</h1>",
  '<p><a href="https://github.com/hughjonesd/r-help">GitHub</a></p>',
  links,
  "</body></html>"
), "site/index.html")
