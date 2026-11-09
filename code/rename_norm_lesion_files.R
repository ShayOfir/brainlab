rename_norm_lesion_files <- function(path) {
  require(stringr)
  pattern <- "^norm\\.lesion"
  files <- list.files(path, pattern = pattern, full.names = FALSE)
  
  renamed <- character(0)
  for (fn in files) {
    number <- str_extract(fn, "(?<=\\.)\\d+(?=\\.)")
    new_fn <- paste0(number, ".nii.gz")
    file.rename(
      from = file.path(path, fn),
      to   = file.path(path, new_fn)
    )
    renamed <- c(renamed, new_fn)
  }
  
  invisible(renamed)
}

remove_leading_zero <- function(path) {
  pattern <- "^0(.+)$"
  files <- list.files(path, pattern = pattern, full.names = FALSE)
  
  renamed <- character(0)
  for (fn in files) {
    new_fn <- sub(pattern, "\\1", fn)
    file.rename(
      from = file.path(path, fn),
      to   = file.path(path, new_fn)
    )
    renamed <- c(renamed, new_fn)
  }
  
  invisible(renamed)
}