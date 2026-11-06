# save_plot <- function(plot, folder, file_ext = "pdf", height = 15, width = 18, units = "cm"){
#   ggplot2::ggsave(filename = paste0(folder, "/", deparse(substitute(plot)),".", file_ext),
#                   height = height, width = width, units = units)
# }
#' Save plot() or ggplot() objects as pdf or png
#' The filename will be the R obj name
#' @param plot plot() or ggplot() obj
#' @param folder target dir
#' @param file_ext "pdf" or "png"
#' @param height plot height
#' @param width plot width
#' @param units inuts, e.g 'cm'
#' @param ...
#'
#' @returns a saved plot in the target directory
#' @export
save_plot <- function(plot, folder,
                      name = NULL,
                      file_ext = "pdf",
                      height = 15, width = 18,
                      units = "cm",
                      ...) {

  plot_expr <- substitute(plot)

  # Use the supplied name, or derive one from the expression
  if (is.null(name)) {
    name <- if (is.call(plot_expr)) {
      deparse(plot_expr[[2]])
    } else {
      deparse(plot_expr)
    }
  }

  filename <- file.path(folder, paste0(name, ".", file_ext))

  # Direct base R plot() call
  if (is.call(plot_expr) && identical(plot_expr[[1]], quote(plot))) {

    grDevices::pdf(
      file = filename,
      height = height / if (units == "cm") 2.54 else 1,
      width = width / if (units == "cm") 2.54 else 1
    )

    tryCatch(
      eval(plot_expr, envir = parent.frame()),
      finally = grDevices::dev.off()
    )

  } else {

    # ggplot object
    plot_obj <- eval(plot_expr, envir = parent.frame())

    if (!inherits(plot_obj, "ggplot")) {
      stop("Input must be a ggplot object or a direct plot() call.")
    }

    ggplot2::ggsave(
      filename = filename,
      plot = plot_obj,
      height = height,
      width = width,
      units = units,
      ...
    )
  }

  invisible(filename)
}
