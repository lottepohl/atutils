# save_plot <- function(plot, folder, file_ext = "pdf", height = 15, width = 18, units = "cm"){
#   ggplot2::ggsave(filename = paste0(folder, "/", deparse(substitute(plot)),".", file_ext),
#                   height = height, width = width, units = units)
# }
#' Save plot() or ggplot() objects as pdf or png
#' The filename will be the R obj name
#' @param plot plot() or ggplot() obj
#' @param name optional filename (without file extension)
#' @param folder target dir
#' @param file_ext "pdf" or "png"
#' @param height plot height
#' @param width plot width
#' @param units inuts, e.g 'cm'
#' @param res resolution, in dpi
#' @param ... additional parameters for ggplot
#'
#' @returns a saved plot in the target directory
#' @export
save_plot <- function(plot, folder,
                      name = NULL,
                      file_ext = "pdf",
                      height = 15, width = 18,
                      units = "cm",
                      res = 300,
                      ...) {

  plot_expr <- substitute(plot)
  plot_env  <- parent.frame()

  file_ext <- tolower(file_ext)

  # Determine filename
  if (is.null(name)) {
    name <- if (is.call(plot_expr)) {
      deparse(plot_expr[[2]])
    } else {
      deparse(plot_expr)
    }
  }

  filename <- file.path(folder, paste0(name, ".", file_ext))

  # Create folder if necessary
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)

  # Open graphics device
  if (file_ext == "pdf") {

    grDevices::pdf(
      file = filename,
      width = if (units == "cm") width / 2.54 else width,
      height = if (units == "cm") height / 2.54 else height,
      ...
    )

  } else if (file_ext == "png") {

    grDevices::png(
      filename = filename,
      width = width,
      height = height,
      units = units,
      res = res,
      ...
    )

  } else {
    stop("Unsupported file extension: ", file_ext)
  }

  # Evaluate and draw plot
  tryCatch({

    result <- eval(plot_expr, envir = plot_env)

    # Explicitly print ggplot and trellis objects
    if (inherits(result, c("ggplot", "trellis"))) {
      print(result)
    }

  }, finally = {
    grDevices::dev.off()
  })

  message("Plot saved: ", filename)

  invisible(filename)
}
