# _targets.R -- Symbolic Boundaries in r/incels
# Author: Yukun Jiao
#
# WORKFLOW STRUCTURE -- mapped to Research Questions and Hypotheses
# ---
#
# NOTE:
# - Focal categories are treated as discursively constructed categories
#   (*incel*, *women*, *normies*), not verified speaker groups.
# - Boundary strength is operationalized as semantic association distance (Euclidean distance)
#   between target-specific association coordinates across shared dimensions.
# ---

library(targets)
library(tarchetypes)

tar_option_set(
  packages = c(
    "quanteda",
    "dplyr",
    "data.table",
    "ggplot2",
    "stringr",
    "textutils",
    "readr",
    "tibble",
    "tidyr",
    "purrr",
    "word2vec",
    "text2vec",
    "text2map",
    "conText",
    "ggrepel",
    "rsample",
    "textclean",
    "stringi",
    "quanteda.textstats",
    "scales",
    "jsonlite"
  ),
  seed = 42
)

tar_source()

# --- Centroid specs: add a new centroid by appending a row here only ------
# --- centroid_specs is Deprecate, not used in this study ---
# anchors and dfm_obj are placed side by side for readability.
# dfm_obj: quoted DFM target name used for non-anchor pool sampling.
centroid_specs <- tibble::tribble(
  ~dim_name, ~boundary_type, ~is_core, ~anchors, ~dfm_obj,

  "blackpill", "ideological", TRUE,
  c("blackpill", "blackpilled", "cope", "coping", "hypergamy", "lookism", "smv", "chad", "stacy"),
  quote(incel_dfm)

  # "appearance", "status", TRUE,
  # c("lookism", "smv", "ugly", "manlet"),
  # quote(incel_dfm)
)

# --- Direction specs: add a new direction by appending one row here only 
# pole_a / pole_z are placed side by side so each semantic contrast is readable.
# dfm_obj: DFM for non-anchor pool sampling.
# search_dfm_obj: DFM used for validate_anchors() candidate pool.
direction_specs <- tibble::tribble(
  ~dim_name, ~boundary_type, ~is_core, ~pole_a, ~pole_z, ~dfm_obj, ~search_dfm_obj,
  
  "gender", "gender", TRUE,
  c("man", "men", "he", "him", "boy", "boys", "male", "males"),
  c("woman", "women", "she", "her", "girl", "girls", "female", "females"),
  quote(incel_dfm), quote(incel_dfm),
  
  "moral", "moral", TRUE,
  c("hatred", "contempt", "loathing", "resentment", "hostility", "bitterness", "anger", "frustration", "despair", "sadness"),
  c("love", "respect", "affection", "empathy", "kindness", "acceptance", "care", "passion", "humanity", "decency"),
  quote(women_dfm), quote(women_dfm),

  "cultural", "cultural", TRUE,
  c("blackpill", "ldar", "ldaring", "rope", "suicide", "sui", "rot", "fuel", "it_is_over", "subhuman", "genetics"),
  c("attitude", "improvement", "skills", "confidence", "friendly", "humor", "advice", "skill", "personalities", "personality", "intelligence"),
  quote(normie_dfm), quote(normie_dfm),
  
  "status", "status", TRUE,
  c("virgin", "ugly", "short", "skinny", "awkward", "unattractive", "autistic"),
  c("slayer", "handsome", "tall", "muscular", "chiseled", "ripped", "assertive"),
  quote(incel_dfm), quote(incel_dfm)
)

category_specs <- tibble::tribble(
  ~cat_name,    ~toks_obj,              ~role,
  "incel",      quote(incel_toks),      "core",
  "normie",     quote(normie_toks),     "core",
  "women",      quote(women_toks),      "core"
)




all_dim_specs <- dplyr::bind_rows(
  centroid_specs |>
    dplyr::transmute(
      dim_name,
      dim_type = "centroid",
      boundary_type,
      is_core
    ),
  direction_specs |>
    dplyr::transmute(
      dim_name,
      dim_type = "direction",
      boundary_type,
      is_core
    )
) |>
  dplyr::mutate(
    series_label = paste0(dim_name, "_", dim_type)
  )

# Add one cos_{cat} column per category -- derived from category_specs.
# Adding a new row to category_specs auto-extends all_dim_specs here.
for (.cn in category_specs$cat_name) {
  all_dim_specs[[paste0("cos_", .cn)]] <- purrr::map(
    all_dim_specs$dim_name,
    ~ as.name(paste0("cos_sim_ym_", .cn, "_", .x))
  )
}

# Core/appendix dimension lists are derived from the `is_core` flag above.
# To move a dimension between the main text and appendix, edit only `is_core`
# in `centroid_specs` or `direction_specs`.
main_dims <- all_dim_specs |>
  dplyr::filter(is_core) |>
  dplyr::pull(dim_name)

main_dim_specs <- all_dim_specs |>
  dplyr::filter(is_core)

# --- Cross-pole anchor contamination check 
.all_a <- unlist(direction_specs$pole_a)
.all_z <- unlist(direction_specs$pole_z)
.conflicts <- intersect(.all_a, .all_z)
if (length(.conflicts) > 0) {
  warning(
    "Anchors appearing on both A-pole and Z-pole across directions: ",
    paste(.conflicts, collapse = ", ")
  )
}

# --- Target factory invocations ---------
#
# To add a new semantic direction: edit centroid_specs or direction_specs above.
# The factories generate direction/centroid embeddings and validation helpers
# used by the manuscript and appendix tables.

centroid_tgts <- purrr::pmap(centroid_specs, centroid_factory) |>
  unlist(recursive = FALSE)

direction_tgts <- purrr::pmap(direction_specs, direction_factory) |>
  unlist(recursive = FALSE)

.qa_core_anchor_doc_calls <- purrr::map(
  main_dims,
  ~ bquote(
    dplyr::mutate(
      .(as.name(paste0("anchor_docs_", .x))),
      dim_name = .(.x),
      .before = 1
    )
  )
)

qualitative_core_anchor_appendix_table_tgt <- rlang::inject(
  tar_target(
    qualitative_core_anchor_appendix_table,
    {
      dim_lookup <- all_dim_specs |>
        dplyr::select(dim_name, boundary_type, dim_type, is_core)

      dplyr::bind_rows(list(!!!.qa_core_anchor_doc_calls)) |>
        tibble::as_tibble() |>
        dplyr::left_join(dim_lookup, by = "dim_name") |>
        dplyr::filter(is_core) |>
        dplyr::mutate(
          dimension = factor(dim_name, levels = main_dims)
        ) |>
        dplyr::select(
          dimension, boundary_type, dim_type,
          pole, keyword, period, ym, doc_id, snippet
        )
    }
  )
)

list(

  # ====================================================================
  # SECTION 1: DATA LOADING & TEXT PREPARATION
  # ====================================================================

  tar_target(submissions_raw_file, "submissions_raw.rds", format = "file"),
  tar_target(comments_raw_file,    "comments_raw.rds",    format = "file"),
  tar_target(bot_authors_file,     "bot_accounts.csv",    format = "file"),

  tar_target(submissions_raw, readRDS(submissions_raw_file)),
  tar_target(comments_raw,    readRDS(comments_raw_file)),

  tar_target(
    data,
    prepare_text_dataset(
      submissions_raw = submissions_raw,
      comments_raw    = comments_raw,
      bot_authors_csv = bot_authors_file
    )
  ),

  tar_target(cleaned_data,    clean_texts(data)),
  tar_target(simplified_docs, simplify_texts(cleaned_data)),

  tar_target(
    docs_for_tokens,
    clean_docs_for_tokens(simplified_docs,
                          extra_protected_phrases = c("80 20 rule", "q3.14"),
                          extra_protected_patterns = c(
                            "\\bit(?:'s|s|\\s+is)\\s+over\\b" = "it_is_over"
                          )
                          ) |>
      label_period() |>
      add_interaction_docvars()
  ),

  # ====================================================================
  # SECTION 2: DESCRIPTIVE STATISTICS
  # ====================================================================
  tar_target(
    desc_docs_per_ym,
    docs_for_tokens |>
      dplyr::filter(!is.na(ym), !period %in% c("pre", "ban_month")) |>
      dplyr::count(ym) |>
      dplyr::mutate(
        ym = factor(ym, levels = sort(unique(ym)))
      )
  ),
  
  tar_target(
    plot_docs_per_ym,
    desc_docs_per_ym |>
      ggplot2::ggplot(ggplot2::aes(x = ym, y = n)) +
      ggplot2::geom_col(fill = "#2c3e50") +
      ggplot2::geom_text(
        ggplot2::aes(label = scales::comma(n)),
        vjust = -0.5,
        size = 3.5
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::comma,
        expand = ggplot2::expansion(mult = c(0, 0.1))
      ) +
      ggplot2::labs(x = "Year-Month", y = "N") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
  ),
  tar_target(
    desc_tokens_per_ym,
    docs_for_tokens |>
      dplyr::filter(!is.na(ym), !period %in% c("pre", "ban_month")) |>
      dplyr::mutate(n_tokens = stringr::str_count(text, "\\S+")) |>
      dplyr::group_by(ym) |>
      dplyr::summarise(n_tokens = sum(n_tokens, na.rm = TRUE), .groups = "drop")
  ),
  tar_target(
    desc_users_per_ym,
    docs_for_tokens |>
      dplyr::filter(
        !is.na(ym),
        !is.na(period),
        !period %in% c("pre", "ban_month"),
        !is.na(author),
        author != "",
        author != "[deleted]"
      ) |>
      dplyr::group_by(author, ym) |>
      dplyr::summarise(n_docs = dplyr::n(), .groups = "drop") |>
      dplyr::filter(n_docs >= 5) |>
      dplyr::group_by(ym) |>
      dplyr::summarise(n_unique_users = dplyr::n_distinct(author), .groups = "drop") |>
      dplyr::mutate(
        ym = factor(ym, levels = sort(unique(ym)))
      )
  ),
  tar_target(
    plot_tokens_per_ym,
    desc_tokens_per_ym |>
      ggplot2::ggplot(ggplot2::aes(x = ym, y = n_tokens)) +
      ggplot2::geom_col(fill = "#2c3e50", width = 0.8) +
      ggplot2::scale_y_continuous(
        labels = function(x) paste0(x / 1e6, "M"),
        expand = ggplot2::expansion(mult = c(0, 0.08))
      ) +
      ggplot2::labs(
        x = "Year-Month",
        y = "Number of tokens (millions)"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
  ),
  
  tar_target(
    plot_users_per_ym,
    desc_users_per_ym |>
      ggplot2::ggplot(ggplot2::aes(x = ym, y = n_unique_users)) +
      ggplot2::geom_col(fill = "#2c3e50") +
      ggplot2::geom_text(
        ggplot2::aes(label = scales::comma(n_unique_users)),
        vjust = -0.5,
        size = 3.5
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::comma,
        expand = ggplot2::expansion(mult = c(0, 0.1))
      ) +
      ggplot2::labs(
        x = "Year-Month",
        y = "Unique Users"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
  ),
  
  tar_target(
    desc_doc_type_per_ym,
    docs_for_tokens |>
      dplyr::filter(!is.na(ym), !period %in% c("pre", "ban_month")) |>
      dplyr::mutate(
        doc_type = dplyr::case_when(
          is.na(doc_type) | doc_type == "" ~ "unknown",
          TRUE ~ as.character(doc_type)
        )
      ) |>
      dplyr::count(ym, doc_type) |>
      tidyr::pivot_wider(
        names_from = doc_type,
        values_from = n,
        values_fill = 0
      ) |>
      dplyr::arrange(ym)
  ),
  
  tar_target(
    desc_token_features_per_ym,
    docs_for_tokens |>
      dplyr::filter(!is.na(ym), !period %in% c("pre", "ban_month")) |>
      dplyr::mutate(
        token_list = stringr::str_split(text, "\\s+"),
        token_list = purrr::map(token_list, ~ .x[.x != ""]),
        n_tokens   = purrr::map_int(token_list, length)
      ) |>
      tidyr::unnest_longer(token_list, values_to = "token", keep_empty = FALSE) |>
      dplyr::filter(!is.na(token), token != "") |>
      dplyr::group_by(ym) |>
      dplyr::summarise(
        total_tokens     = dplyr::n(),
        unique_tokens    = dplyr::n_distinct(token),
        type_token_ratio = unique_tokens / total_tokens,
        .groups = "drop"
      ) |>
      dplyr::arrange(ym)
  ),
  
  tar_target(
    desc_focal_hits_per_ym,
    docs_for_tokens |>
      dplyr::filter(!is.na(ym), !period %in% c("pre", "ban_month")) |>
      dplyr::mutate(
        incel_hits  = stringr::str_count(text, "\\bincels?\\b"),
        women_hits  = stringr::str_count(text, "\\b(woman|women|female|females|girls?)\\b"),
        normie_hits = stringr::str_count(text, "\\bnormies?\\b")
      ) |>
      dplyr::group_by(ym) |>
      dplyr::summarise(
        `Incel hits`  = sum(incel_hits,  na.rm = TRUE),
        `Women hits`  = sum(women_hits,  na.rm = TRUE),
        `Normie hits` = sum(normie_hits, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(ym)
  ),
  tar_target(
    custom_stops,
    c(
      "t", "s", "re", "m", "ve", "d", "ll",
      "don", "doesn", "didn", "isn",
      "just", "like", "can", "even", "one",
      "really", "much", "still", "also", "now", "well", "right",
      "probably", "though", "ever", "always",
      "something", "anything", "nothing", "thing", "things",
      "going", "getting", "got",
      "lol", "__url__",
      "get", "think", "know", "want",
      "good", "time", "make", "go",
      "see", "say", "look", "find",
      "said", "mean", "maybe", "dont", "people", "actually", "someone", "lot", "many",
      "yeah", "sure", "day", "first", "around",
      "everyone", "back", "yes", "anyone",
      "since", "different", "literally", "matter",
      "long", "else", "need", "take", "give", "try", "help",
      "care", "tell", "come", "talk", "made",
      "makes", "stop", "do_not", "can_not", "does_not"
    )
  ),

  tar_target(
    desc_topwords,
    {
      tf <- toks |>
        tokens_select(
          pattern = unique(c(stopwords::stopwords("en"), custom_stops)),
          selection = "remove",
          min_nchar = 2
        ) |>
        dfm() |>
        topfeatures(30)
      
      tibble(
        feature = names(tf),
        frequency = as.numeric(tf),
        rank = seq_along(tf)
      )
    }
  ),
  
  # ====================================================================
  # SECTION 3: GloVe EMBEDDINGS
  # ====================================================================

  tar_target(
    corp,
    corpus(docs_for_tokens, text_field = "text", docid_field = "doc_id")
  ),

  tar_target(toks, tokens(corp)),
  
  tar_target(
    custom_stopwords,
    setdiff(
      stopwords("en"),
      c(
        "no", "not", "nor", "never", "without",
        "you", "your", "yours",
        "he", "him", "his",
        "she", "her", "hers",
        "we", "us", "our", "ours", "ourselves",
        "they", "them", "their", "theirs", "themselves"
      )
    )
  ),

  tar_target(
    toks_nostop,
    tokens_select(toks, pattern = custom_stopwords, selection = "remove", min_nchar = 2)
  ),

  tar_target(
    feats,
    dfm(toks_nostop, tolower = TRUE, verbose = FALSE) |>
      dfm_trim(min_termfreq = 5) |>
      featnames()
  ),

  tar_target(
    toks_nostop_feats,
    tokens_select(toks_nostop, feats, padding = TRUE)
  ),

  tar_target(
    toks_fcm,
    fcm(toks_nostop_feats, context = "window", window = 6,
        count = "frequency", tri = FALSE)
  ),

  tar_target(
    glove_fit,
    {
      glove    <- GlobalVectors$new(rank = 300, x_max = 10, learning_rate = 0.05)
      wv_main  <- glove$fit_transform(toks_fcm, n_iter = 10,
                                       convergence_tol = 1e-3, n_threads = 2)
      list(wv_main = wv_main, wv_context = glove$components)
    }
  ),

  tar_target(local_glove,     glove_fit$wv_main + t(glove_fit$wv_context)),
  tar_target(local_transform, compute_transform(x = toks_fcm, pre_trained = local_glove, weighting = "log")),

  tar_target(
    transform_sanity_checks,
    check_transform_holdout(
      toks_fcm    = toks_fcm,
      local_glove = local_glove,
      min_freq    = 1000,
      holdout_n   = 200
    )
  ),

  # --- Transform validation: frequency-stratified holdout (appendix) ---
  # Samples 10 words from each of 5 log-frequency quintiles (N = 50 total)
  # among words with freq >= 500 that are present in the GloVe vocab.
  # Excludes holdout words from transform training, recomputes ALC embeddings,
  # then measures cosine similarity between ALC and original GloVe vectors.
  # The frequency-stratified design reveals whether reconstruction quality
  # depends on word frequency (expected: yes, higher freq -> higher cosine).
  tar_target(
    transform_sanity_stratified,
    {
      feats_all   <- quanteda::featnames(toks_fcm)
      freq_all    <- quanteda::featfreq(toks_fcm)
      glove_vocab <- rownames(local_glove)

      # Eligible: in GloVe vocab and freq >= 500
      elig_names <- intersect(names(freq_all[freq_all >= 500]), glove_vocab)
      elig_freq  <- freq_all[elig_names]

      # Five equal-count log-frequency quintiles
      log_q <- quantile(log10(elig_freq), probs = seq(0, 1, by = 0.2))
      quintile_labels <- c("Q1 (lowest)", "Q2", "Q3", "Q4", "Q5 (highest)")
      bins <- cut(log10(elig_freq),
                  breaks         = log_q,
                  include.lowest = TRUE,
                  labels         = quintile_labels)
      names(bins) <- elig_names

      # Sample 10 per quintile
      holdout_by_bin <- tapply(elig_names, bins, function(w) sample(w, 10L))
      holdout        <- unlist(holdout_by_bin, use.names = FALSE)
      train_feats    <- setdiff(feats_all, holdout)

      # Retrain transform on holdout-excluded vocabulary
      fcm_train    <- toks_fcm[train_feats, train_feats]
      fcm_test     <- toks_fcm[holdout, train_feats]
      glove_train  <- local_glove[intersect(train_feats, glove_vocab), ,
                                  drop = FALSE]

      local_transform_train <- conText::compute_transform(
        x            = fcm_train,
        pre_trained  = glove_train,
        weighting    = "log"
      )

      holdout_alc <- conText::fem(
        x                = fcm_test,
        pre_trained      = glove_train,
        transform        = TRUE,
        transform_matrix = local_transform_train,
        verbose          = FALSE
      )

      alc_mat  <- as.matrix(holdout_alc)
      true_mat <- local_glove[rownames(alc_mat), , drop = FALSE]

      cosine_sim <- function(a, b) sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
      sims <- sapply(seq_len(nrow(alc_mat)),
                     function(i) cosine_sim(alc_mat[i, ], true_mat[i, ]))
      names(sims) <- rownames(alc_mat)

      per_word <- tibble(
        feature       = rownames(alc_mat),
        freq          = as.integer(elig_freq[rownames(alc_mat)]),
        log10_freq    = round(log10(freq), 2),
        freq_quintile = as.character(bins[rownames(alc_mat)]),
        cosine_sim    = round(sims, 4)
      ) |>
        dplyr::mutate(
          freq_quintile = factor(freq_quintile, levels = quintile_labels)
        ) |>
        dplyr::arrange(freq_quintile, dplyr::desc(cosine_sim))

      by_quintile <- per_word |>
        dplyr::group_by(freq_quintile) |>
        dplyr::summarise(
          n_words      = dplyr::n(),
          freq_range   = paste0(
            scales::comma(min(freq)), "\u2013", scales::comma(max(freq))
          ),
          mean_cosine  = round(mean(cosine_sim, na.rm = TRUE),   3),
          median_cosine= round(median(cosine_sim, na.rm = TRUE), 3),
          sd_cosine    = round(sd(cosine_sim, na.rm = TRUE),     3),
          min_cosine   = round(min(cosine_sim, na.rm = TRUE),    3),
          max_cosine   = round(max(cosine_sim, na.rm = TRUE),    3),
          .groups      = "drop"
        )

      overall <- tibble(
        n_evaluated       = nrow(per_word),
        min_eligible_freq = 500L,
        mean_cosine       = round(mean(sims, na.rm = TRUE),             3),
        median_cosine     = round(median(sims, na.rm = TRUE),           3),
        sd_cosine         = round(sd(sims, na.rm = TRUE),               3),
        q25_cosine        = round(stats::quantile(sims, .25, na.rm = TRUE), 3),
        q75_cosine        = round(stats::quantile(sims, .75, na.rm = TRUE), 3),
        min_cosine        = round(min(sims, na.rm = TRUE),              3),
        max_cosine        = round(max(sims, na.rm = TRUE),              3),
        r_logfreq_cosine  = round(cor(log10(per_word$freq), sims),      3)
      )

      list(per_word = per_word, by_quintile = by_quintile, overall = overall)
    }
  ),

  # Restrict to analysis periods (drop pre / ban_month)
  tar_target(
    toks_main,
    {
      keep_ids <- docs_for_tokens |>
        dplyr::filter(!period %in% c("pre", "ban_month")) |>
        dplyr::pull(doc_id) |>
        as.character()
      toks_nostop_feats[keep_ids, ]
    }
  ),

  # ====================================================================
  # SECTION 4: CONTEXT TOKEN EXTRACTION
  # ALC embeddings for three anchor-term groups.
  # ====================================================================
  tar_target(
    incel_toks,
    tokens_context(x = toks_main,
                   pattern = c("incel", "incels"),
                   window  = 6L)
  ),

  tar_target(incel_dfm, dfm(incel_toks)),

  tar_target(
    incel_dem,
    dem(x = incel_dfm, pre_trained = local_glove,
        transform = TRUE, transform_matrix = local_transform, verbose = TRUE)
  ),

  tar_target(
    incel_ym_nns,
    get_nns(
      x = incel_toks, N = 10,
      groups           = as.character(docvars(incel_toks, "ym")),
      candidates       = featnames(incel_dfm),
      pre_trained      = local_glove,
      transform        = TRUE, transform_matrix = local_transform,
      bootstrap        = TRUE, num_bootstraps   = 100,
      confidence_level = 0.95, as_list          = TRUE
    )
  ),
  tar_target(
    normie_toks,
    tokens_context(x = toks_main, pattern = c("normie", "normies"), window = 6L)
  ),
  
  tar_target(normie_dfm, dfm(normie_toks)),
  
  tar_target(
    normie_dem,
    dem(x = normie_dfm, pre_trained = local_glove,
        transform = TRUE, transform_matrix = local_transform, verbose = TRUE)
  ),
  
  tar_target(
    normie_ym_nns,
    get_nns(
      x = normie_toks, N = 10,
      groups           = as.character(docvars(normie_toks, "ym")),
      candidates       = featnames(normie_dfm),
      pre_trained      = local_glove,
      transform        = TRUE, transform_matrix = local_transform,
      bootstrap        = TRUE, num_bootstraps   = 100,
      confidence_level = 0.95, as_list          = TRUE
    )
  ),
  
  tar_target(
    women_toks,
    tokens_context(x = toks_main,
                   pattern = c("women", "woman", "females", "female", "girl", "girls"),
                   window  = 6L)
  ),
  
  tar_target(women_dfm, dfm(women_toks)),
  
  tar_target(
    women_dem,
    dem(x = women_dfm, pre_trained = local_glove,
        transform = TRUE, transform_matrix = local_transform, verbose = TRUE)
  ),
  
  tar_target(
    women_ym_nns,
    get_nns(
      x = women_toks, N = 10,
      groups           = as.character(docvars(women_toks, "ym")),
      candidates       = featnames(women_dfm),
      pre_trained      = local_glove,
      transform        = TRUE, transform_matrix = local_transform,
      bootstrap        = TRUE, num_bootstraps   = 100,
      confidence_level = 0.95, as_list          = TRUE
    )
  ),
# ---══
# SECTION 5 -- CONSTRUCT DEFINITIONS, VALIDATION, AND COSINE SIMILARITY
# Construct and validation targets are generated by target factories
# from centroid_specs / direction_specs (defined at the top of this file).
# ---══
centroid_tgts,
direction_tgts,

# Anchor tables -- read directly from spec tibbles at definition time and split
# by `is_core`. `*_manuscript` targets contain only core dimensions for the
# main text; `*_appendix` targets contain exploratory dimensions.
tar_target(
  anchors_table_centroid_manuscript,
  centroid_specs |>
    dplyr::filter(is_core) |>
    dplyr::transmute(
      Construct = tools::toTitleCase(gsub("_", " ", dim_name)),
      `Boundary type` = boundary_type,
      `Anchor words` = purrr::map_chr(anchors, ~ paste(.x, collapse = "; "))
    )
),
tar_target(
  anchors_table_direction_manuscript,
  direction_specs |>
    dplyr::filter(is_core) |>
    dplyr::transmute(
      Construct = tools::toTitleCase(gsub("_", " ", dim_name)),
      `Boundary type` = boundary_type,
      `Pole A` = purrr::map_chr(pole_a, ~ paste(.x, collapse = "; ")),
      `Pole Z` = purrr::map_chr(pole_z, ~ paste(.x, collapse = "; "))
    )
),
qualitative_core_anchor_appendix_table_tgt,

tar_target(
  incel_dem_ym,
  dem_group(
    x      = incel_dem,
    groups = as.character(incel_dem@docvars$ym)
  )
),

tar_target(
  normie_dem_ym,
  dem_group(
    x      = normie_dem,
    groups = as.character(normie_dem@docvars$ym)
  )
),


tar_target(
  women_dem_ym,
  dem_group(
    x      = women_dem,
    groups = as.character(women_dem@docvars$ym)
  )
),

tar_target(
  target_words_table_manuscript,
  tibble::tibble(
    Category = c("incel", "normie", "women"),
    `Target words` = c(
      paste(c("incel", "incels"), collapse = "; "),
      paste(c("normie", "normies"), collapse = "; "),
      paste(c("women", "woman", "females", "female", "girl", "girls"), collapse = "; ")
    )
  )
),

tar_target(
  h1_emb_incel,
  colMeans(incel_dem_ym)
),
tar_target(
  h1_emb_normie,
  colMeans(normie_dem_ym)
),
tar_target(
  h1_emb_women,
  colMeans(women_dem_ym)
),

tar_target(
  dirs,
  list(
    gender = direction_gender,
    moral = direction_moral,
    cultural = direction_cultural,
    status = direction_status)
),
tar_target(
  embs,
  list(
    incel  = h1_emb_incel,
    normie = h1_emb_normie,
    women  = h1_emb_women
)
),

tar_target(
  h1_cos_sim,
  lapply(dirs, function(d) {
    sim2(d, do.call(rbind, embs), method = "cosine", norm = "l2")[1, ]
  })
),


tar_target(
  h1_cos_sim_df,
  purrr::imap_dfr(h1_cos_sim, ~ data.frame(
    dimension = factor(.y, levels = c("status", "cultural", "moral", "gender")),
    category  = names(.x),
    cosim     = as.numeric(.x)
  ))
),

tar_target(
  h1_cos_sim_plot,
  ggplot(h1_cos_sim_df, aes(x = cosim, y = dimension, color = category)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_point(size = 3, position = position_dodge(width = 0.0)) +
    labs(x = "Cosine similarity", y = NULL, color = NULL) +
    theme_minimal()
),

tar_target(
  h1_cos_sim_ci,
  {
    # 每个 category × dimension 已有 16 个月的点估计
    # 用跨月分布作为 H1 整体估计的不确定性度量
    purrr::imap_dfr(dirs, function(d, dim_name) {
      purrr::imap_dfr(list(incel = incel_dem_ym,
                           normie = normie_dem_ym,
                           women  = women_dem_ym), function(dem_ym, cat_name) {
                             # cos sim for each ym
                             vals <- sim2(d, as.matrix(dem_ym), method = "cosine", norm = "l2")[1, ]
                             data.frame(
                               dimension = dim_name,
                               category  = cat_name,
                               estimate  = mean(vals),
                               lower.ci  = quantile(vals, 0.025),
                               upper.ci  = quantile(vals, 0.975)
                             )
                           })
    })
  }
),

tar_target(
  toks_list,
  list(
    incel  = incel_toks,
    normie = normie_toks,
    women  = women_toks
    )
),

tar_target(
  h3_cos_sim,
  lapply(toks_list, function(toks) {
    
    res <- lapply(names(dirs), function(dir_name) {
      get_cos_sim(
        toks,
        groups           = docvars(toks, "ym"),
        direction_name   = dir_name,
        target_embedding = dirs[[dir_name]],
        pre_trained      = local_glove,
        transform_matrix = local_transform,
        bootstrap = TRUE
      )
    })
    
    names(res) <- names(dirs)
    
    res
  })
),
tar_target(
  h3_cos_sim_gender,
  extract_dir_target(h3_cos_sim, "gender")
),
tar_target(
  h3_cos_sim_moral,
  extract_dir_target(h3_cos_sim, "moral")
),
tar_target(
  h3_cos_sim_cultural,
  extract_dir_target(h3_cos_sim, "cultural")
),

tar_target(
  h3_cos_sim_status,
  extract_dir_target(h3_cos_sim, "status")
),

tar_target(
  df_all,
  {
    dplyr::bind_rows(
    h3_cos_sim_gender |> dplyr::mutate(dimension = "gender"),
    h3_cos_sim_moral |> dplyr::mutate(dimension = "moral"),
    h3_cos_sim_cultural |> dplyr::mutate(dimension = "cultural"),
    h3_cos_sim_status   |> dplyr::mutate(dimension = "status")
  )}
),

# plots based on proj_dist_ci are included in the manuscript qmd file
tar_target(
  proj_dist_ci,
  get_proj_dist_ci(
    toks_list        = list(incel = incel_toks, normie = normie_toks, women = women_toks),
    directions       = dirs,
    groups_var       = "ym",
    pre_trained      = local_glove,
    transform_matrix = local_transform,
    n_bootstrap      = 100,
    confidence_level = 0.95
  )
)
)
