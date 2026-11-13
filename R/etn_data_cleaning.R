#' Clean comma-separated duplicate values in character columns
#'
#' 1. Renames `acoustic_tag_id_alternative` to `acoustic_tag_id_2`.
#' 2. For every character column containing a comma, splits the value on ",",
#'    trims whitespace, drops empty parts and removes duplicates within the cell.
#'    - one unique value left  -> the cell keeps just that value
#'      ("090Z,090Z" -> "090Z", "A69-1303-1966," -> "A69-1303-1966", "," -> NA)
#'    - several unique values  -> the first stays in the original column, the
#'      others go into new columns with the next free counter
#'      (acoustic_tag_id, acoustic_tag_id_2 exist -> acoustic_tag_id_3)
#'
#' @param df      data frame / tibble (e.g. the animal table)
#' @param exclude character columns to leave untouched (free text where commas
#'                are legitimate, e.g. "comments")
#' @export
etn_remove_double_cols <- function(df, exclude = "comments") {

  # 1) rename alternative tag id
  if ("acoustic_tag_id_alternative" %in% names(df)) {
    if ("acoustic_tag_id_2" %in% names(df)) {
      stop("Both acoustic_tag_id_alternative and acoustic_tag_id_2 exist.")
    }
    names(df)[names(df) == "acoustic_tag_id_alternative"] <- "acoustic_tag_id_2"
  }

  char_cols <- setdiff(names(df)[vapply(df, is.character, logical(1))], exclude)

  for (col in char_cols) {
    x <- df[[col]]
    has_comma <- !is.na(x) & grepl(",", x, fixed = TRUE)
    if (!any(has_comma)) next

    # unique, non-empty parts per row (rows without a comma are left as is)
    parts <- lapply(seq_along(x), function(i) {
      if (!has_comma[i]) return(x[i])
      p <- trimws(strsplit(x[i], ",", fixed = TRUE)[[1]])
      unique(p[nzchar(p)])
    })
    n_parts <- lengths(parts)

    # 2) keep the first unique value in the original column
    x[has_comma] <- vapply(
      parts[has_comma],
      function(p) if (length(p)) p[1] else NA_character_,
      character(1)
    )
    df[[col]] <- x

    # 3) additional distinct values -> new numbered columns
    max_n <- max(c(1L, n_parts[has_comma]))
    if (max_n >= 2) {
      base <- sub("_\\d+$", "", col)            # acoustic_tag_id_2 -> acoustic_tag_id
      fam_pat <- paste0("^", base, "(_\\d+)?$")
      nums <- suppressWarnings(as.integer(
        sub(paste0("^", base, "_(\\d+)$"), "\\1",
            grep(paste0("^", base, "_\\d+$"), names(df), value = TRUE))
      ))
      next_n <- max(c(1L, nums), na.rm = TRUE) + 1L

      new_names <- character(0)
      for (k in 2:max_n) {
        new_name <- paste0(base, "_", next_n)
        df[[new_name]] <- vapply(
          parts,
          function(p) if (length(p) >= k) p[k] else NA_character_,
          character(1)
        )
        new_names <- c(new_names, new_name)
        next_n <- next_n + 1L
      }

      # place new columns right after the last column of this "family"
      rest <- setdiff(names(df), new_names)
      after <- max(grep(fam_pat, rest))
      df <- df[append(rest, new_names, after = after)]
    }
  }

  df
}

# Usage:
# animal_clean <- clean_comma_cols(animal)
# animal_clean <- clean_comma_cols(animal, exclude = c("comments", "tagging_methodology"))

#' #' Keep first entry of comma-separated cell entries of etn animals df
#' #'
#' #' @param animals animals df from etn database
#' #'
#' #' @returns df with first entry of columns tag_serial_number, tag_type, tag_subtype, acoustic_tag_id
#' #' @export
#' #'
#' etn_remove_double_cols <- function(animals){
#'   animals$tag_serial_number <- gsub(",.*","", animals$tag_serial_number)
#'   animals$tag_type <- gsub(",.*","", animals$tag_type)
#'   animals$tag_subtype <- gsub(",.*","", animals$tag_subtype)
#'   animals$acoustic_tag_id <- gsub(",.*","", animals$acoustic_tag_id)
#'   return(animals)
#' }
