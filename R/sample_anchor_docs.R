# sample_anchor_docs.R
#
# sample_anchor_docs(): qualitative reading aid for construct validation.
#
# Searches docs_for_tokens (raw text data frame) for documents containing
# anchor words, returns a readable sample with keyword-in-context snippets.
#
# Usage:
#   tar_load(c(docs_for_tokens, md_direction_anchors))
#
#   # direction anchors (data.frame with a / z columns)
#   res <- sample_anchor_docs(md_direction_anchors, docs_for_tokens)
#   print(res)
#
#   # centroid anchors (character vector)
#   res <- sample_anchor_docs(c("femoid","roastie","subhuman"), docs_for_tokens)
#
#   # filter by period, more samples per word
#   res <- sample_anchor_docs(cultural_exclusion_direction_anchors, docs_for_tokens,
#                             n_per_word = 5, period = "early")

sample_anchor_docs <- function(
    anchors,
    docs,           # docs_for_tokens: data.frame with doc_id, text, period, ym
    n_per_word  = 3L,    # documents sampled per anchor word
    chars_each  = 200L,  # characters of context shown each side of the keyword
    period      = NULL,  # NULL = all; e.g. "early" or c("early","mid")
    seed        = 42L
) {

  # ── 1. normalise anchors ─────────────────────────────────────────────────────
  if (is.data.frame(anchors)) {
    words_a  <- as.character(anchors$a[!is.na(anchors$a)])
    words_z  <- as.character(anchors$z[!is.na(anchors$z)])
    word_vec <- c(
      stats::setNames(rep("pole_a", length(words_a)), words_a),
      stats::setNames(rep("pole_z", length(words_z)), words_z)
    )
  } else {
    words    <- as.character(anchors[!is.na(anchors)])
    word_vec <- stats::setNames(rep("centroid", length(words)), words)
  }

  # ── 2. optional period filter ────────────────────────────────────────────────
  if (!is.null(period) && "period" %in% names(docs)) {
    docs <- docs[docs$period %in% period, ]
  }
  docs <- docs[!is.na(docs$text) & nchar(docs$text) > 0L, ]

  # ── 3. helper: extract a short snippet around the first match ────────────────
  .snippet <- function(text, word, chars_each) {
    m <- regexpr(word, text, ignore.case = TRUE, perl = TRUE)
    if (m == -1L) return(substr(text, 1L, chars_each * 2L))
    kw_end <- m + attr(m, "match.length") - 1L
    start  <- max(1L, m - chars_each)
    end    <- min(nchar(text), kw_end + chars_each)
    snip   <- substr(text, start, end)
    # bracket the keyword
    kw     <- substr(text, m, kw_end)
    snip   <- sub(kw, paste0("[", kw, "]"), snip, fixed = TRUE)
    if (start > 1L)          snip <- paste0("\u2026", snip)
    if (end < nchar(text))   snip <- paste0(snip, "\u2026")
    snip
  }

  # ── 4. search per anchor word ────────────────────────────────────────────────
  set.seed(seed)

  results <- lapply(names(word_vec), function(w) {
    # whole-word, case-insensitive match
    pat  <- paste0("\\b", w, "\\b")
    hits <- docs[grepl(pat, docs$text, ignore.case = TRUE, perl = TRUE), ]

    if (nrow(hits) == 0L) {
      message(sprintf("  '%s' \u2014 no hits", w))
      return(NULL)
    }

    idx  <- sample(nrow(hits), min(n_per_word, nrow(hits)))
    hits <- hits[idx, , drop = FALSE]

    tibble::tibble(
      doc_id  = hits$doc_id,
      pole    = word_vec[[w]],
      keyword = w,
      period  = if ("period" %in% names(hits)) hits$period else NA_character_,
      ym      = if ("ym"     %in% names(hits)) hits$ym     else NA_character_,
      snippet = vapply(hits$text, .snippet, character(1L),
                       word = w, chars_each = chars_each,
                       USE.NAMES = FALSE)
    )
  })

  out <- dplyr::bind_rows(results)

  if (nrow(out) == 0L) {
    message("No anchor words found.")
    return(invisible(out))
  }

  structure(out, class = c("sample_anchor_docs", "tbl_df", "tbl", "data.frame"))
}
