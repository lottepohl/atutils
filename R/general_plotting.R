#' Save plot() or ggplot() objects as PDF and PNG
#'
#' Saves a plot in both PDF and PNG formats using the same filename.
#' By default, the filename is derived from the supplied R object name.
#'
#' @param plot plot() or ggplot() object
#' @param folder target directory
#' @param name optional filename (without file extension)
#' @param height plot height
#' @param width plot width
#' @param units units for dimensions, e.g. "cm" or "in"
#' @param res resolution for PNG output, in dpi
#' @param ... additional arguments passed to the graphics devices
#'
#' @returns Invisibly returns a named character vector containing the paths
#'   to the saved PDF and PNG files.
#' @export
save_plot <- function(plot, folder,
                      name = NULL,
                      height = 15,
                      width = 18,
                      units = "cm",
                      res = 300,
                      ...) {

  plot_expr <- substitute(plot)
  plot_env <- parent.frame()

  # Determine filename -----------------------------------------------------

  if (is.null(name)) {
    name <- if (is.call(plot_expr)) {
      deparse(plot_expr[[2]])
    } else {
      deparse(plot_expr)
    }
  }

  # Create folder if necessary --------------------------------------------

  dir.create(folder, recursive = TRUE, showWarnings = FALSE)

  # Evaluate plot ----------------------------------------------------------

  result <- eval(plot_expr, envir = plot_env)

  # Draw plot on a graphics device ----------------------------------------

  draw_plot <- function(device, filename) {

    if (device == "pdf") {

      grDevices::pdf(
        file = filename,
        width = if (units == "cm") width / 2.54 else width,
        height = if (units == "cm") height / 2.54 else height,
        ...
      )

    } else if (device == "png") {

      grDevices::png(
        filename = filename,
        width = width,
        height = height,
        units = units,
        res = res,
        ...
      )
    }

    tryCatch({

      if (inherits(result, c("ggplot", "trellis"))) {
        print(result)
      } else {
        # Base R plots are drawn when the expression is evaluated.
        # Re-evaluate the expression to draw them on this device.
        eval(plot_expr, envir = plot_env)
      }

    }, finally = {
      grDevices::dev.off()
    })
  }

  # Save both formats ------------------------------------------------------

  pdf_file <- file.path(folder, paste0(name, ".pdf"))
  png_file <- file.path(folder, paste0(name, ".png"))

  draw_plot("pdf", pdf_file)
  draw_plot("png", png_file)

  message(
    "Plots saved:\n",
    "  PDF: ", pdf_file, "\n",
    "  PNG: ", png_file
  )

  invisible(c(pdf = pdf_file, png = png_file))
}
