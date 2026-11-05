#' Build a DetectionsList object from deployments and detections dfs
#'
#' @param deployments deployments df from etn database
#' @param detections detections df from etn database
#' @param utm_epsg coordinate reference system (e.g. 4326, 32631)
#' @param tz time zone, e.g. "UTC"
#' @param default_depth " depth of the acoustic receivers
#'
#' @returns a 'DetectionsList' object to be further used in the kaltoa package
#' @export
#'
kaltoa_build_detections_list <- function(deployments,
                                         detections,
                                         utm_epsg = 32631,   # UTM 31N, Belgian North Sea
                                         tz = "UTC",         # time zone of date_time
                                         default_depth = NULL) {

  # Check required columns
  req_dep <- c("receiver_id", "station_name", "built_in_acoustic_tag_id",
               "deploy_latitude", "deploy_longitude", "deploy_depth")
  req_det <- c("receiver_id", "acoustic_tag_id", "date_time", "millisecond")
  stopifnot(all(req_dep %in% names(deployments)),
            all(req_det %in% names(detections)))

  # Easting/northing in metres
  xy <- sf::st_coordinates(
    sf::st_transform(
      sf::st_as_sf(deployments,
                   coords = c("deploy_longitude", "deploy_latitude"),
                   crs = 4326, remove = FALSE),
      utm_epsg
    )
  )
  deployments$easting  <- xy[, 1]
  deployments$northing <- xy[, 2]

  # Numeric depth (deploy_depth is character)
  deployments$depth_m <- readr::parse_number(as.character(deployments$deploy_depth))

  # Optional fallback: a single number, e.g. default_depth = 5
  if (!is.null(default_depth)) {
    stopifnot(is.numeric(default_depth), length(default_depth) == 1)
    deployments$depth_m[is.na(deployments$depth_m)] <- default_depth
  }
  if (anyNA(deployments$depth_m)) {
    stop("Missing depth_m for: ",
         paste(deployments$station_name[is.na(deployments$depth_m)], collapse = ", "),
         ". Fix deploy_depth or set default_depth.")
  }

  flagged <- character(0)   # receivers without a usable sync tag ID

  # One Detections object per receiver, matched on receiver_id
  build_one <- function(rec) {
    d <- detections[detections$receiver_id == rec$receiver_id, ]
    d <- d[order(d$date_time, d$millisecond), ]

    if (nrow(d) == 0) {
      warning("No detections for ", rec$station_name, " (", rec$receiver_id, ")")
      return(NULL)
    }

    iso <- paste0(format(d$date_time, "%Y-%m-%dT%H:%M:%S", tz = tz),
                  sprintf(".%03d", as.integer(d$millisecond)))

    # Treat NA, "" and the string "NA" as missing
    sync_tag <- rec$built_in_acoustic_tag_id
    has_sync_tag <- !is.na(sync_tag) && nzchar(trimws(sync_tag)) && sync_tag != "NA"

    if (has_sync_tag) {
      is_self <- !is.na(d$acoustic_tag_id) & d$acoustic_tag_id == sync_tag
      if (!any(is_self)) {
        warning(rec$station_name, ": no detections of its own sync tag (",
                sync_tag, ")")
      }
    } else {
      is_self <- rep(FALSE, nrow(d))
      flagged <<- c(flagged, rec$station_name)
      warning(rec$station_name, ": no sync tag ID (emit_id) in deployments; ",
              "built without emit_id and without sync emissions")
    }

    args <- list(
      detect_times   = kaltoa::iso2DetectionTime(iso, tz = tz),
      detect_id      = ifelse(is_self, "PPM_SELF", d$acoustic_tag_id),
      coords         = c(rec$easting, rec$northing),
      depth          = rec$depth_m,
      name           = rec$station_name,
      type           = ifelse(is_self, "PPM_SELF", "PPM"),
      sync_emissions = is_self
    )
    if (has_sync_tag) args$emit_id <- sync_tag   # only supplied when known

    tryCatch(
      do.call(kaltoa::Detections, args),
      error = function(e) {
        warning(rec$station_name, ": Detections() failed, receiver skipped: ",
                conditionMessage(e))
        NULL
      }
    )
  }

  rec_list <- split(deployments, deployments$receiver_id)
  names(rec_list) <- vapply(rec_list, function(x) x$station_name[1], character(1))

  # make DetectionsList obj
  out <- purrr::compact(purrr::map(rec_list, build_one))

  if (length(flagged) > 0) {
    warning("Receivers without a sync tag ID: ",
            paste(flagged, collapse = ", "))
  }

  # Convert to a kaltoa DetectionsList
  det_list <- tryCatch(
    kaltoa::DetectionsList(out),                       # style 1: one list
    error = function(e1) {
      tryCatch(
        do.call(kaltoa::DetectionsList, unname(out)),  # style 2: objects as arguments
        error = function(e2) {
          warning("Could not build a DetectionsList (", conditionMessage(e2),
                  "); returning a plain list instead.")
          out
        }
      )
    }
  )

  attr(det_list, "no_sync_tag") <- flagged   # may be dropped on S4 objects
  det_list
}
