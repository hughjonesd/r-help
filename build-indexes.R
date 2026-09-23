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
  aliases <- unique(aliases[order(aliases$name, aliases$package), ])
  links <- sprintf(
    '<a data-package="%s" data-name="%s" href="%s/%s.html">%s::%s</a><br>',
    htmlEscape(aliases$package),
    htmlEscape(aliases$name),
    htmlEscape(aliases$package),
    htmlEscape(aliases$topic),
    htmlEscape(aliases$package),
    htmlEscape(aliases$name)
  )

  version <- basename(versionDir)
  writeLines(c(
    "<!doctype html>",
    '<html lang="en"><head><meta charset="utf-8">',
    paste0("<title>R ", version, " help</title>"),
    "<script>window.addEventListener('DOMContentLoaded', function () {",
    "var query = new URLSearchParams(window.location.search);",
    "var links = document.querySelectorAll('a[data-package]');",
    "for (var i = 0; i < links.length; i++) {",
    "if (links[i].dataset.package === query.get('package') && links[i].dataset.name === query.get('name')) {",
    "window.location.replace(links[i].href); return;",
    "}}",
    "if (query.get('name')) document.getElementById('missing').hidden = false;",
    "});</script></head><body>",
    paste0("<h1>R ", version, " help</h1>"),
    '<p id="missing" hidden>No help page was found.</p>',
    links,
    "</body></html>"
  ), file.path(versionDir, "00index.html"))
  unlink(aliasFile)
  indexedVersions <- c(indexedVersions, version)
}

indexedVersions <- indexedVersions[
  order(as.package_version(indexedVersions), decreasing = TRUE)
]
links <- paste0(
  '<a href="', indexedVersions, '/00index.html">R ',
  indexedVersions, "</a><br>"
)
writeLines(c(
  "<!doctype html>",
  '<html lang="en"><head><meta charset="utf-8">',
  "<title>Historical R help</title></head><body>",
  "<h1>Historical R help</h1>",
  links,
  "</body></html>"
), "site/index.html")
