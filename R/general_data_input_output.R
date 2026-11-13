#' Save obj as .rds data
#'
#' @param data R obj, its name is the filename in the saved .rds obj
#' @param folder directory to save the .rds file to
#' @param name filename for obj to be saved as
#'
#' @returns saved .rds obj
#' @export
save_data <- function(data, folder, name = deparse(substitute(data))) {
  base::saveRDS(
    data,
    file = file.path(folder, paste0(name, ".rds"))
  )
}

#' Read .rds obj into R global environment
#'
#' @param filestring filename
#' @param folder directory of the saved .rds obj
#'
#' @returns an obj in the global environment
#' @export
#'
load_data <- function(filestring, folder){
  data <- base::readRDS(file = paste0(folder, "/", filestring, ".rds"))
  return(data)
}
