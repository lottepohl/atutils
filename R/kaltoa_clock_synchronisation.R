#' Automatically get the optimal receiver synchronisation order
#'
#' @param sync_conn synchronisation model obj
#' @param n number of acoustic receivers to be control receivers
#' @param min_detections min sync-tag detections for a usable link
#' @param exclude receivers to leave out entirely
#' @param max_exhaustive number of cross detections
#'
#' @returns df of control and target receivers to be further used in kaltoa workflow
#' @export
kaltoa_auto_sync_order <- function(sync_conn,
                            n = 1,                 # number of control receivers
                            min_detections = 1000, # min sync-tag detections for a usable link
                            exclude = NULL,        # receivers to leave out entirely
                            max_exhaustive = 20000) {

  M <- sync_conn$connectivity_matrix
  M[is.na(M)] <- 0                      # diagonal is NaN
  rec <- rownames(M)
  stopifnot(setequal(rec, colnames(M)))
  M <- M[rec, rec]

  # A pair is only as strong as its weaker direction (toa_ct and toa_tc are both used)
  S <- pmin(M, t(M))
  diag(S) <- 0
  W <- log1p(S)                         # log scale: no single huge count dominates

  # Receivers that can't be synced at all (best link below the threshold)
  usable <- setdiff(rec[apply(S, 1, max) >= min_detections], exclude)
  unconnected <- setdiff(rec, usable)
  if (length(unconnected) > 0) {
    warning("Left out (no link with >= ", min_detections, " detections): ",
            paste(unconnected, collapse = ", "))
  }
  if (n > length(usable)) stop("n is larger than the number of usable receivers")

  # Backbone: connect the controllers to each other (maximum spanning tree, Prim)
  backbone <- function(ctrl) {
    if (length(ctrl) == 1) {
      return(list(root = ctrl, edges = NULL, score = 0))
    }
    root <- ctrl[which.max(rowSums(W[ctrl, usable, drop = FALSE]))]
    tree <- root
    from <- to <- character(0)
    w <- numeric(0)
    while (length(tree) < length(ctrl)) {
      rest <- setdiff(ctrl, tree)
      sub  <- W[tree, rest, drop = FALSE]
      k    <- which(sub == max(sub), arr.ind = TRUE)[1, ]
      from <- c(from, tree[k[1]])
      to   <- c(to, rest[k[2]])
      w    <- c(w, sub[k[1], k[2]])
      tree <- c(tree, rest[k[2]])
    }
    list(root = root,
         edges = data.frame(controller = from, target = to, stringsAsFactors = FALSE),
         score = sum(w))
  }

  # Score of a controller set: best link of every target + strength of the backbone
  objective <- function(ctrl) {
    others <- setdiff(usable, ctrl)
    tgt <- if (length(others) > 0) {
      sum(apply(W[ctrl, others, drop = FALSE], 2, max))
    } else 0
    tgt + backbone(ctrl)$score
  }

  # Choose controllers: exhaustive if feasible, otherwise greedy
  if (choose(length(usable), n) <= max_exhaustive) {
    combos <- utils::combn(usable, n, simplify = FALSE)
    best <- combos[[which.max(vapply(combos, objective, numeric(1)))]]
  } else {
    best <- character(0)
    for (i in seq_len(n)) {
      cand <- setdiff(usable, best)
      best <- c(best, cand[which.max(vapply(cand, function(x) objective(c(best, x)),
                                            numeric(1)))])
    }
  }

  bb <- backbone(best)
  others <- setdiff(usable, best)
  assigned <- vapply(others, function(o) best[which.max(W[best, o])], character(1))

  # Build the sync_order. Backbone rows come first so every controller is
  # synchronised before it is used to synchronise others.
  pairs <- list()
  if (!is.null(bb$edges)) {
    for (i in seq_len(nrow(bb$edges))) {
      pairs[[length(pairs) + 1]] <- list(controller = bb$edges$controller[i],
                                         target     = bb$edges$target[i])
    }
  }
  for (ctrl in best) {
    tg <- names(assigned)[assigned == ctrl]
    if (length(tg) > 0) pairs[[length(pairs) + 1]] <- list(controller = ctrl, target = tg)
  }

  sync_order <- tryCatch(
    Reduce(`+`, lapply(pairs, function(p)
      kaltoa::sync_pair(controller = p$controller, target = p$target))),
    error = function(e) {   # fallback: plain data.frame with the same columns
      do.call(rbind, lapply(pairs, function(p)
        data.frame(controller = p$controller, target = p$target,
                   toa_ct = TRUE, toa_tc = TRUE, tdoa_ct = TRUE,
                   stringsAsFactors = FALSE)))
    }
  )

  # Link summary: detections in the weaker direction for each chosen pair
  all_pairs <- do.call(rbind, lapply(pairs, function(p)
    data.frame(controller = p$controller, target = p$target, stringsAsFactors = FALSE)))
  all_pairs$min_detections <- S[cbind(all_pairs$controller, all_pairs$target)]
  weak <- all_pairs[all_pairs$min_detections < min_detections, ]
  if (nrow(weak) > 0) warning("Weak links in sync_order: see attr(, 'links')")

  attr(sync_order, "links")       <- all_pairs
  attr(sync_order, "unconnected") <- unconnected
  sync_order
}

#
# It’s a sensible method, but treat it as a good starting heuristic, not a proven optimum.
#
# Why it’s reasonable
#
# It uses the quantity that limits synchronisation, which is how many sync-tag detections link two receivers.
# Counting only the weaker direction of each pair avoids picking links that look strong but only work one way.
# For n of 1 to 3 it checks every combination, so within its own scoring it does find the best set.
# It flags weak links and receivers it can’t connect.
#
# Where it can mislead you
#
# Total counts hide gaps. The drift is estimated per hourly window, so a pair with 100,000 detections concentrated in a few weeks is worse than one with fewer detections spread evenly. The function doesn’t look at coverage over time.
# It ignores timing quality. It doesn’t check whether the detections have milliseconds. You saw partial ms coverage at NRT-05, NRT-07 and NRT-10, and a controller with weak ms data hurts every target linked to it.
# It ignores geometry. Distance and reflection-prone paths aren’t considered, though they affect how clean the latencies are.
# Targets use one controller only. Each target is tied to its single best controller, even if a second link would help.
# Scores aren’t comparable across n. You can’t pick n by score, so your approach of reading the weakest link in attr(, "links") is the right one.
#
# How to check it for your data
#
# For the chosen pairs, count sync detections per hour and check there are no long gaps (your earlier median was 34 per hour).
# Compare sync_order_2 and sync_order_3 after the full workflow. The one with the flatter latency and smaller error plot wins, whatever the function scored.
# Look at which receivers end up with large offsets or error. NRT-10 and NRT-15 (about +12.9 ms) are the ones to look at first.
#
# If you want, I can add an hourly-coverage check, and an option to leave out receivers with poor ms coverage, to kaltoa_auto_sync_order().
