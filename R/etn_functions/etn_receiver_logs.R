#' Retrieve raw acoustic receiver logs from the ETN database
#'
#' Retrieves receiver log records associated with one or more deployment IDs.
#' The query also returns receiver and manufacturer information.
#'
#' @param deployment_id Integer or numeric vector of deployment IDs.
#' @param con Database connection created with DBI.
#'
#' @return A data frame containing the raw receiver logs, including receiver
#'   name, manufacturer information, deployment ID and all columns from
#'   `acoustic.receiver_logs`.
#'
#' @examples
#' \dontrun{
#' raw_logs <- get_raw_receiver_logs(
#'   deployment_id = c(96818, 96820),
#'   con = con_ETNdb
#' )
#' }
#'
#' @export
get_raw_receiver_logs <- function(deployment_id, con) {

  # Ensure deployment IDs are unique integers
  deployment_id <- unique(as.integer(deployment_id))

  # Check that valid deployment IDs have been provided
  if (length(deployment_id) == 0 || anyNA(deployment_id)) {
    stop("Please provide valid deployment IDs.")
  }

  # Build SQL query:
  # - Retrieve receiver and manufacturer information
  # - Join deployments to receivers and receiver logs
  # - Retrieve logs for all requested deployment IDs
  # - Order results by deployment and timestamp
  query <- glue::glue_sql("
    SELECT
      r.receiver,
      r.manufacturer_fk,
      cm.id_pk AS manufacturer_id,
      cm.project AS manufacturer,
      d.id_pk AS deployment_id,
      rl.*
    FROM acoustic.deployments d
    JOIN acoustic.receivers r
      ON d.receiver_fk = r.id_pk
    LEFT JOIN common.manufacturer cm
      ON r.manufacturer_fk = cm.id_pk
    JOIN acoustic.receiver_logs rl
      ON d.id_pk = rl.deployment_fk
    WHERE d.id_pk IN ({deployment_id*})
    ORDER BY d.id_pk, rl.timestamp
  ", .con = con)

  # Execute query and return raw logs
  result <- DBI::dbGetQuery(con, query)

  return(result)
}


# 2. JSON decoding helper function --------------------------------------------

#' Decode receiver log JSON data
#'
#' Decodes JSON strings stored in receiver log data. Some receiver records
#' contain double-encoded JSON, so the function attempts a second decoding
#' step when necessary.
#'
#' Empty, missing or invalid JSON values are returned as empty lists.
#'
#' @param x Character vector containing JSON strings.
#'
#' @return A list containing the decoded JSON objects.
#'
#' @keywords internal
decode_receiver_json <- function(x) {

  purrr::map(x, function(value) {

    # Return an empty list for missing or empty values
    if (is.na(value) || !nzchar(value)) {
      return(list())
    }

    # Attempt to decode the JSON string
    tryCatch({

      json <- jsonlite::fromJSON(
        value,
        simplifyVector = FALSE
      )

      # Decode again if the result is still a JSON string
      if (is.character(json) && length(json) == 1) {
        json <- jsonlite::fromJSON(
          json,
          simplifyVector = FALSE
        )
      }

      json

    }, error = function(e) {

      # Return an empty list if decoding fails
      list()
    })
  })
}


# 3. Manufacturer-specific parsing functions ---------------------------------


#' Parse Thelma Biotel receiver logs
#'
#' Decodes and expands JSON data from Thelma Biotel receiver logs.
#' Only records marked as valid are retained. JSON fields are expanded
#' into individual columns using their descriptive field names.
#'
#' @param logs Data frame containing raw receiver logs from Thelma Biotel.
#'
#' @return A data frame with decoded JSON fields expanded into columns.
#'
#' @keywords internal
parse_thelma <- function(logs) {

  logs %>%

    # Retain only valid receiver log records
    dplyr::filter(data_is_valid == TRUE) %>%

    # Decode JSON data into a list-column
    dplyr::mutate(
      parsed_data = decode_receiver_json(data)
    ) %>%

    # Remove original JSON column
    dplyr::select(-data) %>%

    # Expand JSON fields into individual columns
    tidyr::unnest_wider(
      parsed_data,
      names_repair = "unique"
    )
}


#' Parse Innovasea receiver logs
#'
#' Decodes Innovasea receiver log data and creates one column for each
#' unique record type. The value stored under JSON key "6" is extracted
#' for each record and placed in the column corresponding to its record type.
#'
#' Only records marked as valid are retained.
#'
#' @param logs Data frame containing raw receiver logs from Innovasea.
#'
#' @return A data frame containing the original log metadata and one
#'   additional column per record type.
#'
#' @keywords internal
parse_innovasea <- function(logs) {

  # Retain valid records and decode JSON data
  logs <- logs %>%
    dplyr::filter(data_is_valid == TRUE) %>%
    dplyr::mutate(
      parsed_data = decode_receiver_json(data)
    )

  # Identify all unique record types in the dataset
  record_types <- unique(logs$record_type)

  # Create one column per record type
  for (rt in record_types) {

    # Extract the value from JSON key "6" for records
    # matching the current record type
    logs[[rt]] <- purrr::map_chr(
      seq_len(nrow(logs)),
      function(i) {

        if (logs$record_type[i] == rt) {

          value <- logs$parsed_data[[i]][["6"]]

          if (is.null(value)) {
            NA_character_
          } else {
            as.character(value)
          }

        } else {

          # Leave other record types as missing
          NA_character_
        }
      }
    )
  }

  # Remove the original JSON data and temporary parsed column
  logs %>%
    dplyr::select(-data, -parsed_data)
}


#' Parse receiver logs according to manufacturer
#'
#' Automatically selects the appropriate manufacturer-specific parser
#' based on the manufacturer column in the supplied log data.
#'
#' Currently supported manufacturers:
#' \itemize{
#'   \item Innovasea
#'   \item Thelma Biotel
#' }
#'
#' All records supplied to this function must belong to the same manufacturer.
#'
#' @param logs Data frame containing raw receiver logs, including a
#'   `manufacturer` column.
#'
#' @return A data frame containing parsed receiver logs.
#'
#' @examples
#' \dontrun{
#' parsed_logs <- parse_receiver_logs(raw_logs)
#' }
#'
#' @keywords internal
parse_receiver_logs <- function(logs) {

  # Identify manufacturers represented in the supplied logs
  manufacturers <- unique(stats::na.omit(logs$manufacturer))

  # Ensure that logs belong to exactly one manufacturer
  if (length(manufacturers) != 1) {
    stop("Logs must contain records from only one manufacturer.")
  }

  # Select the appropriate parser based on manufacturer
  switch(
    tolower(manufacturers),
    "innovasea" = parse_innovasea(logs),
    "thelma biotel" = parse_thelma(logs),
    stop("Unknown manufacturer: ", manufacturers)
  )
}


# 4. Main wrapper function ----------------------------------------------------


#' Retrieve and parse acoustic receiver logs
#'
#' Retrieves receiver logs from the ETN database and automatically parses
#' their JSON data according to the receiver manufacturer.
#'
#' Multiple deployment IDs can be supplied, provided that all deployments
#' belong to receivers from the same manufacturer.
#'
#' @param deployment_id Integer or numeric vector of deployment IDs.
#' @param con Database connection created with DBI.
#'
#' @return A data frame containing parsed receiver logs. The original
#'   receiver and deployment metadata are retained.
#'
#' @examples
#' \dontrun{
#' # Retrieve and parse logs from one deployment
#' logs <- get_receiver_logs(
#'   deployment_id = 96818,
#'   con = con_ETNdb
#' )
#'
#' # Retrieve and parse logs from multiple deployments
#' logs <- get_receiver_logs(
#'   deployment_id = c(96818, 96820),
#'   con = con_ETNdb
#' )
#' }
#'
#' @export
get_receiver_logs <- function(deployment_id, con) {

  # Step 1: Retrieve raw logs from the database
  logs <- get_raw_receiver_logs(
    deployment_id = deployment_id,
    con = con
  )

  # Step 2: Parse JSON data according to receiver manufacturer
  parsed_logs <- parse_receiver_logs(logs)

  # Return parsed logs
  return(parsed_logs)
}


# 5. Tests --------------------------------------------------------------------


# # Retrieve and parse logs from multiple Thelma Biotel deployments
# logs_multiple_t_test <- get_receiver_logs(
#   deployment_id = c(29200, 58442),
#   con = con_ETNdb
# )
#
# # Retrieve and parse logs from multiple Innovasea deployments
# logs_multiple_i_test <- get_receiver_logs(
#   deployment_id = c(96818, 96820),
#   con = con_ETNdb
# )
