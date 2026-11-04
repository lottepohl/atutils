# save_plot <- function(plot, folder, file_ext = "pdf", height = 15, width = 18, units = "cm"){
#   ggplot2::ggsave(filename = paste0(folder, "/", deparse(substitute(plot)),".", file_ext),
#                   height = height, width = width, units = units)
# }
