#' Automatically get the optimal receiver synchronisation order
#'
#' @param sync_conn
#' @param n
#' @param min_detections
#' @param exclude
#' @param max_exhaustive
#'
#' @returns
#' @export
#'
#' @examples
auto_kaltoa_sync_order <- function(sync_conn,
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
