#' Keep first entry of comma-separated cell entries of etn animals df
#'
#' @param animals animals df from etn database
#'
#' @returns df with first entry of columns tag_serial_number, tag_type, tag_subtype, acoustic_tag_id
#' @export
#'
etn_remove_double_cols <- function(animals){
  animals$tag_serial_number <- gsub(",.*","", animals$tag_serial_number)
  animals$tag_type <- gsub(",.*","", animals$tag_type)
  animals$tag_subtype <- gsub(",.*","", animals$tag_subtype)
  animals$acoustic_tag_id <- gsub(",.*","", animals$acoustic_tag_id)
  return(animals)
}
