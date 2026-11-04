#' make_run_id(): "<label>_<first 6 characters of the hash of the settings>"
#'
#' @param label readable description, e.g. "NRT_setup1_thelma_test_ctrl2"
#' @param settings named list with all settings that influence the result, e.g. settings, date, kaltoa version, git commit of the code, R session info.
#'
#' @returns
#' @export
#'
#' @examples
make_run_id <- function(label, settings) {
  paste0(label, "_", substr(rlang::hash(settings), 1, 6))
}

#' write_run_info(): document a run in its folder (run_info.rds + run_info.txt)
#'
#' @param settings named list with all settings that influence the result, e.g. settings, date, kaltoa version, git commit of the code, R session info.
#' @param folder run folder, e.g. file.path(path_positioning, run_id)
#' @param note optional free text, e.g. why this run was made
#'
#' @returns
#' @export
#'
#' @examples
write_run_info <- function(settings, folder, note = NULL) {

  dir.create(folder, recursive = TRUE, showWarnings = FALSE)

  # git commit of the project (NA if the project is not a git repository)
  git_commit <- tryCatch(
    system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE),
    error   = function(e) NA_character_,
    warning = function(w) NA_character_)[1]

  info <- list(
    run_id         = basename(folder),
    created        = Sys.time(),
    note           = note,
    kaltoa_version = as.character(utils::packageVersion("kaltoa")),
    git_commit     = git_commit,
    settings       = settings,
    session        = utils::sessionInfo())

  saveRDS(info, file.path(folder, "run_info.rds"))

  writeLines(c(
    paste("Run ID:        ", info$run_id),
    paste("Created:       ", format(info$created, "%Y-%m-%d %H:%M:%S %Z")),
    paste("Note:          ", if (is.null(note)) "-" else note),
    paste("kaltoa version:", info$kaltoa_version),
    paste("git commit:    ", info$git_commit),
    "",
    "Settings:",
    utils::capture.output(utils::str(settings)),
    "",
    utils::capture.output(print(info$session))),
    file.path(folder, "run_info.txt"))

  invisible(info)
}
