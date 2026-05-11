check_transform_holdout <- function(toks_fcm,
                                    local_glove,
                                    min_freq = 10000,
                                    holdout_n = 20,
                                    weighting = "log") {
  feats <- featnames(toks_fcm)
  freq <- featfreq(toks_fcm)

  eligible <- names(freq[freq >= min_freq])
  eligible <- intersect(eligible, rownames(local_glove))

  if (length(eligible) < holdout_n) {
    stop("Not enough eligible features for the requested holdout_n.")
  }

  holdout <- sample(eligible, holdout_n)
  train_feats <- setdiff(feats, holdout)

  toks_fcm_train <- toks_fcm[train_feats, train_feats]
  toks_fcm_test  <- toks_fcm[holdout, train_feats]

  local_transform <- compute_transform(
    x = toks_fcm_train,
    pre_trained = local_glove[train_feats, , drop = FALSE],
    weighting = "log"
  )

  holdout_alc <- fem(
    x = toks_fcm_test,
    pre_trained = local_glove[train_feats, , drop = FALSE],
    transform = TRUE,
    transform_matrix = local_transform,
    verbose = FALSE
  )

  alc_mat <- as.matrix(holdout_alc)
  true_mat <- local_glove[holdout, , drop = FALSE]
  true_mat <- true_mat[rownames(alc_mat), , drop = FALSE]

  cosine_sim <- function(x, y) {
    sum(x * y) / (sqrt(sum(x * x)) * sqrt(sum(y * y)))
  }

  sims <- sapply(seq_len(nrow(alc_mat)), function(i) {
    cosine_sim(alc_mat[i, ], true_mat[i, ])
  })

  results <- data.frame(
    feature = rownames(alc_mat),
    cosine = sims,
    row.names = NULL
  )

  list(
    summary = summary(sims),
    mean = mean(sims, na.rm = TRUE),
    median = median(sims, na.rm = TRUE),
    results = results |> dplyr::arrange(desc(cosine)),
    sims = sims,
    holdout = holdout,
    train_feats = train_feats,
    transform = local_transform
  )
}
