# validate_anchors.R
#
# validate_anchors(): full anchor-set evaluation following Boutyline & Johnston
# (2023) and the text2map vignette.
#
# Workflow:
#   1. Compute PairDir for all anchor pairs (test_anchors, method = "pairdir").
#   2. Flag the weakest pair (lowest individual PairDir).
#   3. For each side of the weakest pair, iterate through candidate words and
#      find which replacement yields the highest AVERAGE PairDir for the full set.
#   4. Return a tidy list with the pairdir table, the weakest pair, and the
#      top-N replacement candidates for each side.
#
# Usage:
#   pole_a <- c("hug", "kissed", "cuddle", "affection", "companionship")
#   pole_b <- c("blackpill", "blackpilled", "cope", "looksmax", "smv")
#   res    <- validate_anchors(pole_a, pole_b, wv = local_glove)
#   print(res)

validate_anchors <- function(
    pole_a,
    pole_b,
    wv,
    find_replacements  = TRUE,
    candidate_pool     = NULL,   # custom word pool; NULL = sample from full vocab
    n_candidates       = 200L,
    n_top              = 15L,
    seed               = 42L
) {

  # ── 1. input checks ─────────────────────────────────────────────────────────
  pole_a <- pole_a[pole_a %in% rownames(wv)]
  pole_b <- pole_b[pole_b %in% rownames(wv)]

  if (length(pole_a) == 0L) stop("No pole_a words found in wv.")
  if (length(pole_b) == 0L) stop("No pole_b words found in wv.")

  n_pairs <- min(length(pole_a), length(pole_b))
  if (n_pairs < 2L) stop("Need at least 2 pairs for PairDir.")

  anchors_df <- data.frame(
    a = pole_a[seq_len(n_pairs)],
    b = pole_b[seq_len(n_pairs)]
  )

  # ── 2. baseline PairDir ──────────────────────────────────────────────────────
  pd_full <- text2map::test_anchors(anchors_df, wv = wv, method = "pairdir")

  avg_baseline <- pd_full$pair_dir[pd_full$anchor_pair == "AVERAGE"]

  # individual pair scores (drop AVERAGE row)
  pd_pairs <- pd_full[pd_full$anchor_pair != "AVERAGE", ]
  weakest_idx  <- which.min(pd_pairs$pair_dir)
  weakest_pair <- pd_pairs$anchor_pair[weakest_idx]
  weakest_score <- pd_pairs$pair_dir[weakest_idx]

  weak_a <- anchors_df$a[weakest_idx]
  weak_b <- anchors_df$b[weakest_idx]

  # ── 3. candidate replacement search ─────────────────────────────────────────
  replacements <- NULL

  if (find_replacements) {

    all_anchor_words <- unique(c(pole_a, pole_b))
    pool <- if (!is.null(candidate_pool)) {
      candidate_pool[candidate_pool %in% rownames(wv)]
    } else {
      rownames(wv)
    }
    pool <- pool[!pool %in% all_anchor_words]
    pool <- pool[grepl("^[a-z]+$", pool)]   # lowercase unigrams only

    set.seed(seed)
    candidates <- sample(pool, min(n_candidates, length(pool)))

    # helper: test one substitution, return new AVERAGE PairDir
    .test_sub <- function(new_word, side) {
      tmp <- anchors_df
      if (side == "a") tmp$a[weakest_idx] <- new_word
      else             tmp$b[weakest_idx] <- new_word
      res <- text2map::test_anchors(tmp, wv = wv, method = "pairdir")
      res$pair_dir[res$anchor_pair == "AVERAGE"]
    }

    message(sprintf(
      "Weakest pair: %s (PairDir = %.3f). Testing %d replacement candidates...",
      weakest_pair, weakest_score, n_candidates
    ))

    # replace a-side (keeping weak_b fixed)
    scores_a <- vapply(candidates, .test_sub, numeric(1L), side = "a")
    top_a <- data.frame(
      candidate  = candidates,
      fixes_side = "a",
      fixed_word = weak_b,
      avg_pairdir = scores_a
    ) |>
      dplyr::arrange(dplyr::desc(avg_pairdir)) |>
      dplyr::slice_head(n = n_top)

    # replace b-side (keeping weak_a fixed)
    scores_b <- vapply(candidates, .test_sub, numeric(1L), side = "b")
    top_b <- data.frame(
      candidate  = candidates,
      fixes_side = "b",
      fixed_word = weak_a,
      avg_pairdir = scores_b
    ) |>
      dplyr::arrange(dplyr::desc(avg_pairdir)) |>
      dplyr::slice_head(n = n_top)

    replacements <- list(
      replace_a_side = top_a,
      replace_b_side = top_b
    )
  }

  # ── 4. return ─────────────────────────────────────────────────────────────
  structure(
    list(
      pairdir_table   = pd_full,
      avg_pairdir     = avg_baseline,
      n_pairs         = n_pairs,
      weakest_pair    = weakest_pair,
      weakest_score   = weakest_score,
      anchors_used    = anchors_df,
      replacements    = replacements
    ),
    class = "validate_anchors"
  )
}

# =============================================================================
# validate_centroid()
#
# Centroid equivalent of validate_anchors(). For unipolar anchor sets where
# all words define one semantic pole (no bipolar contrast).
#
# Workflow:
#   1. Compute baseline RELCO (avg pairwise cosine) and per-word cosine to centroid.
#   2. Flag the weakest word (lowest cosine to centroid).
#   3. Search candidates: (a) best word to ADD to the set, (b) best word to
#      REPLACE the weakest word — both ranked by resulting RELCO.
#
# Usage:
#   res <- validate_centroid(
#     anchors = c("femoid","roastie","whore","slut","subhuman","vile","degenerate"),
#     wv      = local_glove
#   )
#   print(res)
# =============================================================================

validate_centroid <- function(
    anchors,
    wv,
    find_replacements = TRUE,
    candidate_pool    = NULL,
    n_candidates      = 200L,
    n_top             = 15L,
    seed              = 42L
) {

  # ── 1. vocab filter (preserve duplicates intentionally) ───────────────────
  anchors_in <- anchors[anchors %in% rownames(wv)]
  if (length(anchors_in) < 2L) stop("Need at least 2 anchor words in wv.")

  # ── 2. helpers ─────────────────────────────────────────────────────────────
  .avg_paircos <- function(words) {
    mat   <- wv[words, , drop = FALSE]
    norms <- sqrt(rowSums(mat^2))
    norms[norms < 1e-12] <- 1e-12
    mn    <- mat / norms
    cm    <- tcrossprod(mn)
    mean(cm[upper.tri(cm)])
  }

  # ── 3. baseline RELCO ──────────────────────────────────────────────────────
  relco_baseline <- .avg_paircos(anchors_in)

  # ── 4. per-word cosine to centroid ─────────────────────────────────────────
  centroid_vec  <- as.numeric(text2map::get_centroid(anchors_in, wv))
  centroid_norm <- centroid_vec / sqrt(sum(centroid_vec^2))
  mat_a <- wv[anchors_in, , drop = FALSE]
  norms_a <- sqrt(rowSums(mat_a^2)); norms_a[norms_a < 1e-12] <- 1e-12
  sims_to_centroid <- as.numeric(mat_a %*% centroid_norm) / norms_a
  names(sims_to_centroid) <- anchors_in

  weakest_word  <- names(which.min(sims_to_centroid))
  weakest_score <- min(sims_to_centroid)

  # ── 5. candidate search ────────────────────────────────────────────────────
  replacements <- NULL

  if (find_replacements) {
    pool <- if (!is.null(candidate_pool)) {
      candidate_pool[candidate_pool %in% rownames(wv)]
    } else {
      rownames(wv)
    }
    pool <- pool[!pool %in% unique(anchors_in)]
    pool <- pool[grepl("^[a-z]+$", pool)]

    set.seed(seed)
    candidates <- sample(pool, min(n_candidates, length(pool)))

    message(sprintf(
      "Weakest word: '%s' (cos-to-centroid = %.3f). Testing %d candidates...",
      weakest_word, weakest_score, length(candidates)
    ))

    # (a) ADD candidate: RELCO of (anchors + candidate)
    scores_add <- vapply(candidates, function(w) .avg_paircos(c(anchors_in, w)),
                         numeric(1L))
    top_add <- data.frame(candidate = candidates, relco_if_added = scores_add) |>
      dplyr::arrange(dplyr::desc(relco_if_added)) |>
      dplyr::slice_head(n = n_top)

    # (b) REPLACE weakest: RELCO of (anchors - weakest + candidate)
    anchors_minus <- anchors_in[anchors_in != weakest_word]
    scores_rep <- vapply(candidates,
                         function(w) .avg_paircos(c(anchors_minus, w)),
                         numeric(1L))
    top_rep <- data.frame(
      candidate        = candidates,
      replaces         = weakest_word,
      relco_if_replaced = scores_rep
    ) |>
      dplyr::arrange(dplyr::desc(relco_if_replaced)) |>
      dplyr::slice_head(n = n_top)

    replacements <- list(add = top_add, replace_weakest = top_rep)
  }

  # ── 6. return ──────────────────────────────────────────────────────────────
  structure(
    list(
      relco_baseline   = relco_baseline,
      sims_to_centroid = sort(sims_to_centroid, decreasing = TRUE),
      weakest_word     = weakest_word,
      weakest_score    = weakest_score,
      anchors_used     = anchors_in,
      replacements     = replacements
    ),
    class = "validate_centroid"
  )
}
