# Extended get_cos_sim with target_embedding support and factor-group safety.
#
# Changes vs conText original:
#   1. New `target_embedding` param: a numeric vector or 1×d matrix (centroid /
#      direction). When supplied, cosine similarity is computed against this
#      vector directly — no `features` / GloVe lookup needed.
#   2. New `direction_name` param: label for the feature column when using
#      `target_embedding` (default: "target_embedding").
#   3. Factor `groups` are auto-coerced to character, preventing the
#      `table(factor)` vs `rowsum(factor)` dimension mismatch that produces NaN.
#   4. Mutual-exclusion check: exactly one of `features` / `target_embedding`
#      must be provided.
#
# Drop-in replacement — all original arguments are preserved.

get_cos_sim <- function(x,
                        groups           = NULL,
                        features         = character(0),
                        target_embedding = NULL,
                        direction_name   = "target_embedding",
                        pre_trained,
                        transform        = TRUE,
                        transform_matrix,
                        bootstrap        = TRUE,
                        num_bootstraps   = 100,
                        confidence_level = 0.95,
                        stem             = FALSE,
                        language         = "porter",
                        as_list          = TRUE) {

  # ── validation ────────────────────────────────────────────────────────────
  if (bootstrap && (confidence_level >= 1 || confidence_level <= 0))
    stop('"confidence_level" must be a numeric value between 0 and 1.',
         call. = FALSE)
  if (bootstrap && num_bootstraps < 100)
    stop("num_bootstraps must be at least 100")
  if (!inherits(x, "tokens"))
    stop("data must be of class tokens", call. = FALSE)
  if (length(features) == 0 && is.null(target_embedding))
    stop('Provide either "features" or "target_embedding".', call. = FALSE)
  if (length(features) > 0 && !is.null(target_embedding))
    stop('Provide either "features" or "target_embedding", not both.',
         call. = FALSE)

  # ── factor-safe groups ────────────────────────────────────────────────────
  group_levels <- NULL
  if (!is.null(groups) && is.factor(groups)) {
    group_levels <- levels(groups)   # preserve order before coercion
    groups <- as.character(groups)
  }

  # ── normalise target_embedding to unit vector ─────────────────────────────
  if (!is.null(target_embedding)) {
    target_vec  <- as.numeric(target_embedding)
    tvec_norm   <- sqrt(sum(target_vec^2))
    if (tvec_norm < 1e-12)
      stop("target_embedding has zero norm.", call. = FALSE)
    target_vec  <- target_vec / tvec_norm
  } else {
    target_vec <- NULL
  }

  # ── stemming check ────────────────────────────────────────────────────────
  if (stem) {
    if (requireNamespace("SnowballC", quietly = TRUE)) {
      cat('Using', language,
          'for stemming. To check available languages run',
          '"SnowballC::getStemLanguages()"\n')
    } else {
      stop('"SnowballC (>= 0.7.0)" package must be installed for stemming.')
    }
  }

  # ── build DEM ─────────────────────────────────────────────────────────────
  if (!is.null(groups)) quanteda::docvars(x) <- NULL
  quanteda::docvars(x, "group") <- groups

  x_dfm <- quanteda::dfm(x, tolower = FALSE)
  x_dem  <- conText::dem(
    x = x_dfm, pre_trained = pre_trained,
    transform = transform, transform_matrix = transform_matrix,
    verbose = FALSE
  )

  # ── bootstrap path ────────────────────────────────────────────────────────
  if (bootstrap) {
    cat("starting bootstraps \n")
    cossimdf_bs <- replicate(
      num_bootstraps,
      .cos_sim_bootstrap_internal(
        x                = x_dem,
        by               = x_dem@docvars$group,
        features         = features,
        target_vec       = target_vec,
        direction_name   = direction_name,
        pre_trained      = pre_trained,
        stem             = stem,
        language         = language
      ),
      simplify = FALSE
    )
    result <- do.call(rbind, cossimdf_bs) |>
      dplyr::group_by(target, feature) |>
      dplyr::mutate(
        lower.ci = dplyr::nth(value,
                              round((1 - confidence_level) * num_bootstraps),
                              order_by = value),
        upper.ci = dplyr::nth(value,
                              round(confidence_level * num_bootstraps),
                              order_by = value)
      ) |>
      dplyr::summarise(
        std.error = sd(value),
        value     = mean(value),
        lower.ci  = mean(lower.ci),
        upper.ci  = mean(upper.ci),
        .groups   = "keep"
      ) |>
      dplyr::ungroup() |>
      dplyr::select("target", "feature", "value",
                    "std.error", "lower.ci", "upper.ci")
    cat("done with bootstraps \n")

  # ── non-bootstrap path ────────────────────────────────────────────────────
  } else {
    wvs <- if (!is.null(groups)) {
      conText::dem_group(x = x_dem, groups = x_dem@docvars$group)
    } else {
      matrix(colMeans(x_dem), nrow = 1)
    }

    if (!is.null(target_vec)) {
      result <- .cos_to_vec(wvs, target_vec, direction_name)
    } else {
      result <- conText::cos_sim(
        x = wvs, pre_trained = pre_trained,
        features = features, stem = stem, language = language,
        as_list = FALSE, show_language = FALSE
      )
    }
  }

  # ── reshape to list if requested ──────────────────────────────────────────
  if (as_list) {
    if (is.null(groups)) {
      cat("NOTE: as_list cannot be TRUE when groups is NULL; returning data frame.\n")
    } else {
      targets_ordered <- if (!is.null(group_levels)) {
        intersect(group_levels, unique(result$target))  # factor level order, drop absent levels
      } else {
        unique(result$target)
      }
      result <- lapply(
        targets_ordered,
        function(i) dplyr::filter(result, target == i) |>
          dplyr::mutate(target = as.character(target))
      ) |> setNames(targets_ordered)
    }
  }

  result
}

# ── internal bootstrap sub-function ──────────────────────────────────────────
.cos_sim_bootstrap_internal <- function(x,
                                        by,
                                        features,
                                        target_vec,
                                        direction_name,
                                        pre_trained,
                                        stem,
                                        language) {
  x_sample <- conText::dem_sample(x = x, size = 1, replace = TRUE, by = by)
  wvs      <- if (!is.null(by)) {
    conText::dem_group(x = x_sample, groups = x_sample@docvars$group)
  } else {
    matrix(colMeans(x_sample), nrow = 1)
  }

  if (!is.null(target_vec)) {
    .cos_to_vec(wvs, target_vec, direction_name)
  } else {
    conText::cos_sim(
      x = wvs, pre_trained = pre_trained,
      features = features, stem = stem, language = language,
      as_list = FALSE, show_language = FALSE
    )
  }
}

# ── cosine similarity: group embeddings × one target vector ──────────────────
# target_vec must already be L2-normalised (unit vector).
.cos_to_vec <- function(wvs_mat, target_vec, direction_name) {
  row_ids <- rownames(wvs_mat)
  row_norms <- sqrt(Matrix::rowSums(wvs_mat ^ 2))
  row_norms[row_norms < 1e-12] <- NA_real_
  sims <- as.numeric(wvs_mat %*% target_vec) / row_norms
  data.frame(
    target  = row_ids,
    feature = direction_name,
    value   = sims,
    stringsAsFactors = FALSE
  )
}
# ── get_proj_dist_ci ──────────────────────────────────────────────────────────
# Bootstrap CI for pairwise Euclidean distance in z-scored projection space.
#
# For each bootstrap iteration and each YM:
#   1. Resample documents within each (category × YM) cell with replacement
#   2. Compute scalar projections onto all directions
#   3. Z-score using reference params from the full dataset (held fixed)
#   4. Compute pairwise Euclidean distance between communities
#
# The z-score reference is fixed so the bootstrap captures only document-level
# sampling uncertainty, not variation in the standardisation reference.
#
# Arguments:
#   toks_list        : named list of tokens objects (e.g. list(incel=..., normie=..., women=...))
#   directions       : named list of direction vectors from get_direction()
#   groups_var       : docvar name to group by, e.g. "ym"
#   pre_trained      : local GloVe matrix
#   transform_matrix : local transform matrix
#   n_bootstrap      : number of bootstrap iterations (>= 100)
#   confidence_level : CI width, default 0.95
#
# Returns: data frame with columns ym, pair, mean_dist, lower.ci, upper.ci

get_proj_dist_ci <- function(toks_list,
                             directions,
                             groups_var       = "ym",
                             pre_trained,
                             transform_matrix,
                             n_bootstrap      = 500,
                             confidence_level = 0.95) {

  stopifnot(n_bootstrap >= 100)
  stopifnot(confidence_level > 0, confidence_level < 1)

  # ── precompute: build DEMs and convert to dense matrices ──────────────────
  cat("Building DEMs...\n")
  dir_vecs  <- lapply(directions, as.numeric)
  dir_norms <- sapply(dir_vecs, function(d) sqrt(sum(d^2)))

  mats <- lapply(toks_list, function(toks) {
    # Attach groups to toks docvars so dem inherits them
    quanteda::docvars(toks, ".__grp__") <- as.character(
      quanteda::docvars(toks, groups_var)
    )
    dfm  <- quanteda::dfm(toks, tolower = FALSE)
    dem  <- conText::dem(x = dfm, pre_trained = pre_trained,
                         transform_matrix = transform_matrix, verbose = FALSE)
    # Use groups from dem@docvars to match rows that survived DEM construction
    list(mat    = as.matrix(dem),
         groups = as.character(dem@docvars$`.__grp__`))
  })

  all_yms <- sort(unique(unlist(lapply(mats, `[[`, "groups"))))
  cat_names <- names(toks_list)

  # ── point estimates: projections for all (ym × category × dimension) ──────
  point_proj <- do.call(rbind, lapply(cat_names, function(cat) {
    m <- mats[[cat]]
    do.call(rbind, lapply(all_yms, function(ym) {
      idx <- which(m$groups == ym)
      if (length(idx) == 0) return(NULL)
      emb <- colMeans(m$mat[idx, , drop = FALSE])
      data.frame(
        ym        = ym,
        category  = cat,
        dimension = names(dir_vecs),
        proj      = sapply(seq_along(dir_vecs), function(j)
          sum(emb * dir_vecs[[j]]) / dir_norms[[j]])
      )
    }))
  }))

  # ── z-score reference: pooled mean/SD per dimension (fixed for bootstrap) ─
  z_ref <- point_proj |>
    dplyr::group_by(dimension) |>
    dplyr::summarise(mu = mean(proj), sigma = sd(proj), .groups = "drop")

  # ── bootstrap ──────────────────────────────────────────────────────────────
  cat("Starting bootstraps...\n")
  boot_list <- replicate(n_bootstrap, {

    # For each YM: resample each category, project, z-score, distance
    do.call(rbind, lapply(all_yms, function(ym) {

      # Resample and project for each category
      proj_by_cat <- sapply(cat_names, function(cat) {
        m   <- mats[[cat]]
        idx <- which(m$groups == ym)
        if (length(idx) == 0) return(rep(NA_real_, length(dir_vecs)))
        boot_idx <- sample(idx, length(idx), replace = TRUE)
        emb <- colMeans(m$mat[boot_idx, , drop = FALSE])
        sapply(seq_along(dir_vecs), function(j)
          sum(emb * dir_vecs[[j]]) / dir_norms[[j]])
      })
      # proj_by_cat: dims × categories

      # Z-score using fixed reference
      z_by_cat <- sapply(seq_along(dir_vecs), function(j) {
        mu    <- z_ref$mu[z_ref$dimension == names(dir_vecs)[j]]
        sigma <- z_ref$sigma[z_ref$dimension == names(dir_vecs)[j]]
        (proj_by_cat[j, ] - mu) / sigma
      }) |> t()
      # z_by_cat: dims × categories

      # Pairwise distances + per-dimension sq_prop
      pairs <- utils::combn(cat_names, 2, simplify = FALSE)
      do.call(rbind, lapply(pairs, function(p) {
        diff_vec  <- z_by_cat[, p[1]] - z_by_cat[, p[2]]
        sq_diff   <- diff_vec^2
        total_sq  <- sum(sq_diff, na.rm = TRUE)
        data.frame(
          ym        = ym,
          pair      = paste0(p[1], "_", p[2]),
          dimension = names(dir_vecs),
          dist      = sqrt(total_sq),
          sq_prop   = sq_diff / total_sq
        )
      }))
    }))

  }, simplify = FALSE)

  cat("Done with bootstraps.\n")

  # ── aggregate CI ──────────────────────────────────────────────────────────
  alpha <- (1 - confidence_level) / 2
  do.call(rbind, boot_list) |>
    dplyr::group_by(ym, pair, dimension) |>
    dplyr::summarise(
      mean_dist    = mean(dist,    na.rm = TRUE),
      dist_lower   = quantile(dist,    alpha,     na.rm = TRUE),
      dist_upper   = quantile(dist,    1 - alpha, na.rm = TRUE),
      mean_sq_prop = mean(sq_prop, na.rm = TRUE),
      prop_lower   = quantile(sq_prop, alpha,     na.rm = TRUE),
      prop_upper   = quantile(sq_prop, 1 - alpha, na.rm = TRUE),
      .groups      = "drop"
    )
}

