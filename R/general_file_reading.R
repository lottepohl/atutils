#' Read files with specific extensions into the global environment
#'
#' Reads supported file types from a directory and assigns each file
#' to the global environment using its filename (without extension).
#'
#' @param path Directory containing the files to load.
#' @param file_ext File extension(s). Supported: gpkg, csv, tsv, xlsx,
#'   xls, json, rds.
#' @param subdir Logical. If TRUE, include files in child directories.
#'
#' @returns Invisibly returns the names of the objects loaded into
#'   the global environment. Returns NULL if no matching files are found.
#'
#' @export
load_files_to_env <- function(path, file_ext, subdir = FALSE) {

  # Remove leading dots if provided
  file_ext <- sub("^\\.", "", file_ext)

  # Map file extensions to read functions
  get_read_func <- function(ext) {
    switch(ext,
           "gpkg"  = function(file_path) sf::st_read(file_path, quiet = TRUE),
           "csv"   = readr::read_csv,
           "tsv"   = readr::read_tsv,
           "xlsx"  = readxl::read_excel,
           "xls"   = readxl::read_excel,
           "json"  = jsonlite::fromJSON,
           "rds"   = readRDS,
           stop("Unsupported file extension: '", ext, "'. Supported: gpkg, csv, tsv, xlsx, xls, json, rds")
    )
  }

  # Validate all extensions upfront before doing any work
  purrr::walk(file_ext, get_read_func)

  # Build regex pattern matching any of the provided extensions
  pattern <- paste0("\\.(", paste(file_ext, collapse = "|"), ")$")

  # List files, optionally recursing into subdirectories
  files <- list.files(path, pattern = pattern, full.names = TRUE, recursive = subdir)

  if (length(files) == 0) {
    message("No ", paste(file_ext, collapse = "/"), " files found in: ", path,
            if (subdir) " (including subdirectories)")
    return(invisible(NULL))
  }

  # Derive names: always just the bare filename without extension
  names(files) <- tools::file_path_sans_ext(basename(files))

  # Warn about name collisions (last file wins)
  dupes <- names(files)[duplicated(names(files))]
  if (length(dupes) > 0) {
    warning("Duplicate object names detected (last file wins): ",
            paste(unique(dupes), collapse = ", "))
  }

  # Load each file using its extension-specific read function
  purrr::walk2(files, names(files), function(file_path, obj_name) {
    ext  <- tools::file_ext(file_path)
    read <- get_read_func(ext)
    assign(obj_name, read(file_path), envir = .GlobalEnv)
  })

  message("Loaded ", length(files), " object(s): ", paste(names(files), collapse = ", "))
  invisible(names(files))
}
