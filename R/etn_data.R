#' download or read etn data from file
#'
#' @param name string, filename of the file
#' @param expr etn function, e.g. etn::get_animals()
#' @param folder folder where the file is saved or will be saved
#' @param refresh boolean. if refresh = T, then data will be downloaded
#'
#' @returns obj in the global environment, and saved obj in the specified path
#' @export
#'
etn_get_or_download <- function(name, expr, folder,
                            refresh = TRUE) {

  file <- file.path(folder, paste0(name, ".rds"))

  # 1. file exists -> read it from disk
  if (file.exists(file) && !refresh) {
    message("Loading '", name, "' from ", file)
    return(readRDS(file))
  }

  # 2. otherwise download (this is where `expr` is evaluated)
  message("Downloading '", name, "' from ETN ...")
  obj <- expr

  # 3. save it. save_data() names the file after the object it receives, so the
  #    object is first created under its real name in the calling environment.
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  save_data(obj, folder = folder, name = name)

  obj
}
