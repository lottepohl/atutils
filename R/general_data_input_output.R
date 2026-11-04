#' Save obj as .rds data
#'
#' @param data R obj, its name is the filename in the saved .rds obj
#' @param folder directory to save the .rds file to
#'
#' @returns saved .rds obj
#' @export
#'
save_data <- function(data, folder){
  base::saveRDS(data, file = paste0(folder, "/" , deparse(substitute(data)), ".rds"))
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
