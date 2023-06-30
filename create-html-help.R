
library(gert)
options(nwarnings = 1e5)

r_source_dir <- "../r-source"
html_dir <- "docs"

branches <- gert::git_branch_list(repo = r_source_dir)
branches <- branches$name
is_R_version <- grepl("tags/R-[0-9]", branches)
branches <- branches[is_R_version]
branches <- gsub("^origin/", "", branches)
branches <- unique(branches) # don't duplicate for remote branches

for (branch_name in branches) {
  message("=== Processing R ", branch_name, " ===" )
  R_version <- gsub(".*tags/R-([0-9]+)-([0-9]+)-*([0-9]*)", 
                             "\\1.\\2.\\3", 
                             branch_name)
  R_version <- gsub("\\.$", "", R_version)
  
  gert::git_branch_checkout(branch = branch_name, repo = r_source_dir)   
  rd_files <- list.files(path = r_source_dir, 
                         pattern = "Rd$", recursive = TRUE, full.names = TRUE)
  # some Rd files are for tests:
  rd_files <- grep("/man/", rd_files, value = TRUE) 
  rd_files <- grep("/tests/", rd_files, invert = TRUE, value = TRUE)
  
  rd_files <- data.frame(path = rd_files)
  rd_files$package <- gsub(".*?([[:alnum:]]+)/man/.*", "\\1", rd_files$path)
  rd_files$topic <- gsub(".*?([^/]*)\\.Rd$", "\\1", rd_files$path)
  rd_files$platform <- ifelse(grepl("/(windows|unix)/", rd_files$path),
                          gsub(".*/(windows|unix)/.*", "\\1", rd_files$path),
                          NA)
  for (r in seq_len(nrow(rd_files))) {
    rd_path <- rd_files[r, "path"]
    rd_package <- rd_files[r, "package"]
    rd_platform <- rd_files[r, "platform"]
    rd_topic <- rd_files[r, "topic"]
    
    output_file <- paste0(rd_topic, ".html")
    
    subdir <- file.path(html_dir, R_version, rd_package)
    if (! is.na(rd_platform)) subdir <- file.path(subdir, rd_platform)
    
    dir.create(subdir, recursive = TRUE, showWarnings = FALSE)
    output_path <- file.path(subdir, output_file)
    
    tryCatch(
      tools::Rd2HTML(rd_path, out = output_path, package = rd_package),
      error = function (e) warning("tools::Rd2HTML failed on ", rd_path, 
                                   " for R ", R_version)
    )
    
    html <- readLines(output_path)
    current_url <- paste0("https://stat.ethz.ch/R-manual/R-patched/library/",
                          rd_package, "/html/", rd_topic, ".html")
    version_html <- paste0("<div style='padding: 20pt; border: 2px solid black; text-align:center;'><b>This help topic is for R version ", R_version,
                           ". For the corresponding topic in the current version of R, see ",
                           "<a href='", current_url, "'>", current_url, "</a></b></div>")
    html <- sub("<body>", paste("<body>", version_html), html)
    html <- sub("Package <em>(.*?)</em> version \\S+",
                paste0("<a href='/", R_version, 
                       "/00index.html'>Package <em>\\1</em> version ", 
                       R_version, "</a>"), 
                html)
    writeLines(html, output_path)
  }
  
  # Build ultra-simple '00index' files
  package_links <- ifelse(is.na(rd_files$platform), 
                      rd_files$package, 
                      paste(rd_files$package, rd_files$platform, sep = "/"))
  urls <- paste(package_links, paste0(rd_files$topic, ".html"), sep = "/")
  index_lines <- paste0("<a href='", urls,"'>", rd_files$package, "::", 
                        rd_files$topic, "</a><br>\n")
  index_lines <- paste(index_lines, collapse = "")
  index_html <- paste0("<html><body style='margin: 10em 10em; text-align: center;'>",
                       "<div style='margin: 2em;'>",
                       "<b>Help topics for R ", R_version, "</b></div>",
                      index_lines, "</body></html>")
  cat(index_html, file = file.path(html_dir, R_version, "00index.html"))
}
