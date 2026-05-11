clean_docs_for_tokens <- function(
    docs,
    extra_protected_phrases = character(),
    extra_protected_patterns = character()
) {
  # Only protect expressions whose meaning would be damaged by the later
  # punctuation / digit cleanup. Most ordinary multiword phrases should remain
  # split and can be tracked analytically without forced compounding.
  protected_phrases <- unique(c(
    "80 20 rule",
    extra_protected_phrases
  ))

  # Sentinel tokens used only inside this function. They temporarily shield
  # already-protected tokens from underscore cleanup.
  phrase_sep <- "COMPOUNDTOKEN"

  normalize_encoding <- function(x) {
    x |>
      replace_curly_quote() |>
      stringi::stri_trans_general(id = "Any-Latin; Latin-ASCII")
  }

  protect_existing_tokens <- function(x) {
    # These were already standardized upstream in 1_text_pre_wrangling and must
    # survive intact.
    x <- gsub("__rate_(\\d+)_(\\d+)__", " RATETOKEN\\1X\\2 ", x, perl = TRUE)
    x <- gsub("__ratio_(\\d+)_(\\d+)__", " RATIOTOKEN\\1X\\2 ", x, perl = TRUE)
    x <- gsub("__URL__", " URLTOKEN ", x, fixed = TRUE)
    x
  }

  normalize_negation_contractions <- function(x) {
    # Keep negation from fragmenting into tokens such as "wouldn t". We
    # normalize negative contractions to *_not so the negation remains explicit
    # and survives later punctuation cleanup.
    contraction_map <- c(
      "aren't" = "are_not",
      "isn't" = "is_not",
      "wasn't" = "was_not",
      "weren't" = "were_not",
      "don't" = "do_not",
      "doesn't" = "does_not",
      "didn't" = "did_not",
      "haven't" = "have_not",
      "hasn't" = "has_not",
      "hadn't" = "had_not",
      "can't" = "can_not",
      "couldn't" = "could_not",
      "won't" = "will_not",
      "wouldn't" = "would_not",
      "shan't" = "shall_not",
      "shouldn't" = "should_not",
      "mustn't" = "must_not",
      "mightn't" = "might_not",
      "mayn't" = "may_not",
      "needn't" = "need_not",
      "oughtn't" = "ought_not",
      "daren't" = "dare_not",
      "ain't" = "is_not",
      "cannot" = "can_not"
    )

    for (contraction_i in names(contraction_map)) {
      replacement_i <- gsub("_", phrase_sep, contraction_map[[contraction_i]], fixed = TRUE)
      x <- gsub(
        paste0("\\b", contraction_i, "\\b"),
        replacement_i,
        x,
        perl = TRUE,
        ignore.case = TRUE
      )
    }

    # Fallback for rare regular forms not covered above. This is less
    # semantically polished than the explicit map, but still prevents "t"
    # fragments from entering the vocabulary.
    x <- gsub(
      "([[:alpha:]]+)n't\\b",
      paste0("\\1", phrase_sep, "not"),
      x,
      perl = TRUE,
      ignore.case = TRUE
    )
    x
  }

  protect_new_patterns <- function(x) {
    # Protect only expressions whose meaning would be broken by later cleanup.
    if (length(protected_phrases) > 0) {
      for (phrase_i in protected_phrases) {
        # Turn a literal expression such as "80 20 rule" or "q3.14" into a
        # case-insensitive regex that allows flexible whitespace between parts
        # but otherwise matches the literal characters the user supplied.
        escaped_i <- stringr::str_replace_all(
          phrase_i,
          "([.|()\\^{}+$*?\\[\\]\\\\])",
          "\\\\\\1"
        )
        pattern_i <- stringr::str_replace_all(escaped_i, "\\s+", "\\\\s+")
        replacement_i <- gsub("[[:space:][:punct:]]+", phrase_sep, phrase_i)

        x <- stringr::str_replace_all(
          x,
          stringr::regex(
            paste0("(?<![[:alnum:]_])", pattern_i, "(?![[:alnum:]_])"),
            ignore_case = TRUE
          ),
          replacement_i
        )
      }
    }

    # Optional explicit protections supplied by the caller.
    # Expected format:
    #   c("q3\\\\.14" = "q3_14", "iso\\\\s*9001" = "iso_9001")
    # Names are regex patterns matched against the raw text; values are the
    # exact protected token to keep through later cleanup.
    if (length(extra_protected_patterns) > 0) {
      for (i in seq_along(extra_protected_patterns)) {
        pattern_i <- names(extra_protected_patterns)[i]
        replacement_i <- extra_protected_patterns[[i]]
        if (!is.na(pattern_i) && nzchar(pattern_i) &&
            !is.na(replacement_i) && nzchar(replacement_i)) {
          replacement_i <- gsub("_", phrase_sep, replacement_i, fixed = TRUE)
          x <- gsub(
            pattern_i,
            replacement_i,
            x,
            perl = TRUE,
            ignore.case = TRUE
          )
        }
      }
    }
    x
  }

  clean_underscores <- function(x) {
    # At this point, only ordinary underscores should be flattened. Protected
    # upstream tokens have already been replaced by sentinels.
    gsub("_+", " ", x)
  }

  restore_protected_tokens <- function(x) {
    x <- gsub("\\bRATETOKEN(\\d+)X(\\d+)\\b", " rate_\\1_\\2 ", x, perl = TRUE)
    x <- gsub("\\bRATIOTOKEN(\\d+)X(\\d+)\\b", " ratio_\\1_\\2 ", x, perl = TRUE)
    x <- gsub("\\bURLTOKEN\\b", "__url__", x, perl = TRUE)
    x <- gsub(phrase_sep, "_", x, fixed = TRUE)
    x
  }

  finalize_text <- function(x) {
    x |>
      tolower() |>
      # Remove punctuation, but keep underscores because protected tokens are
      # now represented with underscores.
      gsub("[^[:alnum:]_[:space:]]+", " ", x = _) |>
      # Remove standalone numbers only. Numbers inside protected forms such as
      # 80_20_rule or q3_14 should remain untouched.
      gsub("(?<=^|\\s)\\d+(?=\\s|$)", " ", x = _, perl = TRUE) |>
      gsub("\\s+", " ", x = _) |>
      trimws()
  }

  docs |>
    dplyr::mutate(
      text = ifelse(is.na(text), "", text),
      text = normalize_encoding(text),
      text = protect_existing_tokens(text),
      text = normalize_negation_contractions(text),
      text = protect_new_patterns(text),
      text = clean_underscores(text),
      text = restore_protected_tokens(text),
      text = finalize_text(text)
    ) |>
    dplyr::filter(!is.na(text) & text != "")
}

collapse_consecutive_repeats <- function(text, min_run = 2, keep = 1) {
  if (is.na(text) || !nzchar(text)) return(text)

  tokens <- strsplit(text, "\\s+")[[1]]
  tokens <- tokens[tokens != ""]

  if (length(tokens) <= 1) return(text)

  r <- rle(tokens)

  new_tokens <- unlist(
    Map(
      f = function(word, n) {
        if (n >= min_run) {
          rep(word, keep)
        } else {
          rep(word, n)
        }
      },
      word = r$values,
      n = r$lengths
    ),
    use.names = FALSE
  )

  paste(new_tokens, collapse = " ")
}

label_period <- function(df,
                         ym_col = "ym",
                         period_col = "period") {
  
  stopifnot(ym_col %in% names(df))
  
  ym <- as.character(df[[ym_col]])
  
  period <- rep(NA_character_, length(ym))
  
  period[ym >= "2016-02" & ym <= "2016-06"] <- "pre"
  period[ym >= "2016-07" & ym <= "2016-12"] <- "early"
  period[ym >= "2017-01" & ym <= "2017-07"] <- "mid"
  period[ym >= "2017-08" & ym <= "2017-10"] <- "late"
  period[ym == "2017-11"] <- "ban_month"
  
  df[[period_col]] <- factor(
    period,
    levels = c("pre", "early", "mid", "late", "ban_month"),
    ordered = TRUE
  )
  
  df
}
