#' Iteratively query etn detections per tag_serial_number with informative messages
#'
#' @param animals animal df from the etn database
#' @param output_file file to store detections in
#' @param save_interval after how many tag_serial_numbers the output_file should be overwritten
#'
#' @returns detections df from the etn database
#' @export
#'
#' @examples
etn_det_per_tag <- function(animals, output_file, save_interval = 5) {

  # Resume if exists
  if(file.exists(output_file)) {
    detections_all <- readRDS(output_file)
    # Extract already processed tag serial numbers
    processed_tags <- unique(detections_all$tag_serial_number)
    message("🔄 Resuming from ", nrow(detections_all), " existing detections")
    message("📋 Already processed tags: ", length(processed_tags))
  } else {
    detections_all <- tibble()
    processed_tags <- character(0)
  }

  # Save on exit (even if interrupted)
  on.exit({
    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
    saveRDS(detections_all, output_file)
    message("💾 Final progress saved to: ", output_file)
  })

  # get unique tag_serial_numbers
  animals <- animals %>% distinct(tag_serial_number)

  for(i in seq_len(nrow(animals))) {
    animal_row <- animals[i, ]
    serial_num <- animal_row$tag_serial_number

    # Print the tag being processed
    message("🔍 Processing tag: ", serial_num, " (iteration ", i, "/", nrow(animals), ")")

    # Skip if already processed
    if(serial_num %in% processed_tags) {
      message("⏭️  Skipping (already processed): ", serial_num)
      next
    }

    # Try to fetch data
    detections_animal <- tryCatch({
      etn::get_acoustic_detections(tag_serial_number = serial_num)
    }, error = function(e) {
      message("⚠️  Failed for tag: ", serial_num, " | Error: ", e$message)
      return(tibble())
    })

    # Combine if successful
    if(nrow(detections_animal) > 0) {
      detections_all <- rbind(detections_all, detections_animal)
      # Update processed tags list
      processed_tags <- c(processed_tags, serial_num)
    }

    # Save periodically
    if(i %% save_interval == 0) {
      saveRDS(detections_all, output_file)
      message("💾 Progress saved at iteration ", i, " (", nrow(detections_all), " detections)")
    }
  }

  return(detections_all)
}

#' Iteratively query etn detections for a receiver station name with informative messages
#'
#' @param stations list of station names
#' @param animals animal df from the etn database
#' @param scientific_name scientific name of the species
#' @param output_file filename where detections should be saved
#' @param save_interval after how many stations the output file should be overwritten
#'
#' @returns detections df from the etn database
#' @export
#'
#' @examples
etn_det_per_station <- function(stations, animals = NULL, scientific_name = NULL,
                                output_file = NULL,
                                save_interval = 5) {

  # Build a unique identifier for resume tracking
  resume_key <- paste0(
    "stations=", paste(sort(stations), collapse = ";"),
    "|species=", scientific_name %||% "ALL"
  )

  # Resume if exists
  if(file.exists(output_file)) {
    detections_all <- readRDS(output_file)
    # Extract already processed stations
    processed_stations <- unique(detections_all$station_name)
    message("🔄 Resuming from ", nrow(detections_all), " existing detections")
    message("📋 Already processed stations: ", length(processed_stations))
    if(!is.null(scientific_name)) {
      message("🧬 Filtering by species: ", scientific_name)
    }
  } else {
    detections_all <- tibble()
    processed_stations <- character(0)
  }

  # Save on exit (even if interrupted)
  on.exit({
    saveRDS(detections_all, output_file)
    message("💾 Final progress saved to: ", output_file)
  })

  for(i in seq_along(stations)) {
    station <- stations[i]

    # Print the station being processed
    message("🔍 Processing station: ", station, " (iteration ", i, "/", length(stations), ")")

    # Skip if already processed
    if(station %in% processed_stations) {
      message("⏭️  Skipping (already processed): ", station)
      next
    }

    # Try to fetch data for this station with scientific_name filter
    detections_station <- tryCatch({
      etn::get_acoustic_detections(
        station_name = station,
        tag_serial_number = animals$tag_serial_number,
        scientific_name = scientific_name
      )
    }, error = function(e) {
      message("⚠️  Failed for station: ", station, " | Error: ", e$message)
      return(tibble())
    })

    # Combine if successful
    if(nrow(detections_station) > 0) {
      detections_all <- rbind(detections_all, detections_station)
      # Update processed stations list
      processed_stations <- c(processed_stations, station)
    }

    # Save periodically
    if(i %% save_interval == 0) {
      saveRDS(detections_all, output_file)
      message("💾 Progress saved at iteration ", i, " (", nrow(detections_all), " detections)")
    }
  }

  return(detections_all)
}


# find_tag_matches <- function(acoustic_tag_ids, animals_df, tags_df) {
#   results_list <- vector("list", nrow(acoustic_tag_ids))
#
#   for (i in seq_len(nrow(acoustic_tag_ids))) {
#     # Ensure tag_id is a single character string
#     tag_id <- acoustic_tag_ids$acoustic_tag_id[i]
#
#     # Define the columns to search
#     cols_to_search <- c("tag_serial_number", "acoustic_tag_id", "acoustic_tag_id_alternative")
#
#     # --- Search Animals ---
#     # We assume animals might have similar columns, or we search everything if unsure.
#     # If animals also has specific columns, replace 'everything()' below with the specific vector.
#     animal_match <- animals_df %>%
#       filter(grepl(.env$tag_id, tag_serial_number, fixed = TRUE) |
#                grepl(.env$tag_id, acoustic_tag_id, fixed = TRUE) |
#                grepl(.env$tag_id, acoustic_tag_id_alternative, fixed = TRUE))
#
#     # --- Search Tags (Specific Columns Only) ---
#     # We build the filter condition manually to avoid if_any vectorization issues
#     tag_match <- tags_df %>%
#       mutate(
#         match_serial = grepl(pattern = .env$tag_id, x = tag_serial_number, fixed = TRUE),
#         match_acoustic = grepl(.env$tag_id, acoustic_tag_id, fixed = TRUE),
#         match_alternative = grepl(.env$tag_id, acoustic_tag_id_alternative, fixed = TRUE)
#       ) %>%
#       filter(
#         match_serial | match_acoustic | match_alternative,
#         tag_subtype != 'built-in'
#       ) %>%
#       select(-match_serial, -match_acoustic, -match_alternative)
#
#     # # Check what dplyr sees
#     # str(tags_df$tag_serial_number)
#     # str(tags_df$acoustic_tag_id)
#     # str(tags_df$acoustic_tag_id_alternative)
#     #
#     # # Test grepl directly
#     # grepl(tag_id, tags_df$tag_serial_number, fixed = TRUE)  # Should return logical vector
#
#     # Store results
#     results_list[[i]] <- tibble(
#       acoustic_tag_id = tag_id,
#       n_animal_matches = nrow(animal_match),
#       n_tag_matches = nrow(tag_match),
#       animal_data = list(animal_match),
#       tag_data = list(tag_match)
#     )
#   }
#
#   bind_rows(results_list)
# }
