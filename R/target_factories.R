# R/target_factories.R ─────────────────────────────────────────────────────────
# Target factory functions for semantic dimension targets.
#
# Adding a new semantic direction requires ONLY editing centroid_specs or
# direction_specs in _targets.R.  No other changes are needed.
#
# Two factories:
#   centroid_factory()  → centroid, centroid_non_anchors,
#                         centroid_validation, centroid_search
#   direction_factory() → direction, pairdir, direction_non_anchors,
#                         relco_a, relco_z, direction_search
#
# Each factory returns a flat list of tar_target_raw() objects.
# Invoke via purrr::pmap(spec_tibble, factory_fn) |> unlist(recursive = FALSE).
# ──────────────────────────────────────────────────────────────────────────────


centroid_factory <- function(dim_name, anchors, dfm_obj, ...) {
  nm_centroid    <- paste0("centroid_",             dim_name)
  nm_non_anchors <- paste0("centroid_non_anchors_", dim_name)
  nm_validation  <- paste0("centroid_validation_",  dim_name)
  nm_search      <- paste0("centroid_search_",      dim_name)
  nm_sample      <- paste0("anchor_docs_",          dim_name)

  list(
    targets::tar_target_raw(
      nm_centroid,
      bquote(
        text2map::get_centroid(
          anchors = .(anchors)[.(anchors) %in% rownames(local_glove)],
          wv      = local_glove
        )
      )
    ),

    targets::tar_target_raw(
      nm_non_anchors,
      bquote({
        pool <- setdiff(quanteda::featnames(.(dfm_obj)), .(anchors))
        pool <- pool[pool %in% rownames(local_glove)]
        pool <- pool[grepl("^[a-z]+$", pool)]
        set.seed(42L)
        sample(pool, min(200L, length(pool)))
      })
    ),

    targets::tar_target_raw(
      nm_validation,
      bquote(
        text2map::test_anchors(
          anchors     = .(anchors)[.(anchors) %in% rownames(local_glove)],
          wv          = local_glove,
          non_anchors = .(as.name(nm_non_anchors)),
          method      = "relco",
          type        = "centroid",
          conf        = 0.95,
          n_runs      = 100L,
          seed        = 42L
        )
      )
    ),

    targets::tar_target_raw(
      nm_search,
      bquote(
        validate_centroid(
          anchors           = .(anchors),
          wv                = local_glove,
          find_replacements = TRUE,
          candidate_pool    = quanteda::featnames(.(dfm_obj)),
          n_candidates      = 500L,
          n_top             = 15L,
          seed              = 42L
        )
      )
    ),

    # Qualitative reading aid: sample documents containing each anchor word
    targets::tar_target_raw(
      nm_sample,
      bquote(
        sample_anchor_docs(.(anchors), docs_for_tokens, n_per_word = 3L, seed = 42L)
      )
    )
  )
}


direction_factory <- function(dim_name, pole_a, pole_z, dfm_obj, search_dfm_obj, ...) {
  nm_dir         <- paste0("direction_",             dim_name)
  nm_pairdir     <- paste0("pairdir_",               dim_name)
  nm_non_anchors <- paste0("direction_non_anchors_", dim_name)
  nm_relco_a     <- paste0("relco_a_",               dim_name)
  nm_relco_z     <- paste0("relco_z_",               dim_name)
  nm_search      <- paste0("direction_search_",      dim_name)
  nm_sample      <- paste0("anchor_docs_",           dim_name)

  list(
    targets::tar_target_raw(
      nm_dir,
      bquote(
        get_direction(
          data.frame(a = .(pole_a), z = .(pole_z)),
          local_glove
        )
      )
    ),

    targets::tar_target_raw(
      nm_pairdir,
      bquote(
        text2map::test_anchors(
          anchors = data.frame(a = .(pole_a), z = .(pole_z)),
          wv      = local_glove,
          method  = "pairdir"
        )
      )
    ),

    targets::tar_target_raw(
      nm_non_anchors,
      bquote({
        anchor_words <- c(.(pole_a), .(pole_z))
        pool <- setdiff(quanteda::featnames(.(dfm_obj)), anchor_words)
        pool <- pool[pool %in% rownames(local_glove)]
        pool <- pool[grepl("^[a-z]+$", pool)]
        set.seed(42L)
        sample(pool, min(200L, length(pool)))
      })
    ),

    targets::tar_target_raw(
      nm_relco_a,
      bquote(
        text2map::test_anchors(
          anchors     = unique(.(pole_a)[.(pole_a) %in% rownames(local_glove)]),
          wv          = local_glove,
          non_anchors = .(as.name(nm_non_anchors)),
          method      = "relco",
          type        = "centroid",
          conf        = 0.95,
          n_runs      = 100L,
          seed        = 42L
        )
      )
    ),

    targets::tar_target_raw(
      nm_relco_z,
      bquote(
        text2map::test_anchors(
          anchors     = unique(.(pole_z)[.(pole_z) %in% rownames(local_glove)]),
          wv          = local_glove,
          non_anchors = .(as.name(nm_non_anchors)),
          method      = "relco",
          type        = "centroid",
          conf        = 0.95,
          n_runs      = 100L,
          seed        = 42L
        )
      )
    ),

    targets::tar_target_raw(
      nm_search,
      bquote(
        validate_anchors(
          .(pole_a),
          .(pole_z),
          wv                = local_glove,
          find_replacements = TRUE,
          candidate_pool    = quanteda::featnames(.(search_dfm_obj))[
            quanteda::featnames(.(search_dfm_obj)) %in% rownames(local_glove)
          ],
          n_candidates      = min(
            2000L,
            sum(quanteda::featnames(.(search_dfm_obj)) %in% rownames(local_glove))
          ),
          n_top             = 15L
        )
      )
    ),

    # Qualitative reading aid: sample documents for each pole
    targets::tar_target_raw(
      nm_sample,
      bquote(
        sample_anchor_docs(
          data.frame(a = .(pole_a), z = .(pole_z)),
          docs_for_tokens,
          n_per_word = 3L,
          seed       = 42L
        )
      )
    )
  )
}

