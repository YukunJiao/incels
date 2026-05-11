# R/browse_docs.R ─────────────────────────────────────────────────────────────
# browse_docs(): export docs_for_tokens to a local HTML file and optionally open in browser.
#
# Usage:
#   tar_load(docs_for_tokens)
#
#   browse_docs(docs_for_tokens)                        # all docs
#   browse_docs(docs_for_tokens, ym = "2017-06")        # one month
#   browse_docs(docs_for_tokens, ym = c("2017-01","2017-06"), n = 50)
#   browse_docs(docs_for_tokens, pattern = "blackpill") # keyword filter + highlight
#   browse_docs(docs_for_tokens, pattern = "chad|stacy", n = 30, raw = TRUE)
#   browse_docs(docs_for_tokens, out_dir = "browse_docs_html")
#   browse_docs(docs_for_tokens, ym = "2017-06", view = "thread")
#   browse_docs(docs_for_tokens, view = "thread", sample_mode = "top_scored", n = 10)
#   browse_docs(docs_for_tokens, view = "thread", sample_mode = "top_scored_by_ym", n = 10)
#   browse_docs(docs_for_tokens, pattern = "advice", view = "thread", sample_mode = "most_commented_threads_by_ym", n = 5)
#   browse_docs(docs_for_tokens, doc_id = 12345, view = "thread")  # full thread for one document
# ─────────────────────────────────────────────────────────────────────────────

.browse_docs_is_port_open <- function(host, port) {
  con <- try(
    suppressWarnings(
      socketConnection(
        host = host,
        port = port,
        open = "r+",
        blocking = TRUE,
        timeout = 1
      )
    ),
    silent = TRUE
  )

  if (inherits(con, "try-error")) return(FALSE)
  close(con)
  TRUE
}

.browse_docs_ensure_server <- function(dir, host, port) {
  if (.browse_docs_is_port_open(host, port)) return(invisible(TRUE))

  py <- Sys.which("python3")
  if (!nzchar(py)) {
    stop("python3 not found; cannot open via localhost.")
  }

  log_file <- tempfile(pattern = "browse_docs_server_", fileext = ".log")
  system2(
    py,
    args = c(
      "-m", "http.server",
      as.character(port),
      "--bind", host,
      "--directory", normalizePath(dir, winslash = "/", mustWork = TRUE)
    ),
    wait = FALSE,
    stdout = log_file,
    stderr = log_file
  )

  for (i in seq_len(20L)) {
    Sys.sleep(0.15)
    if (.browse_docs_is_port_open(host, port)) return(invisible(TRUE))
  }

  stop(sprintf("Failed to start localhost server at http://%s:%s", host, port))
}

.browse_docs_open <- function(path, browser, open_mode, server_host, server_port) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  open_mode <- match.arg(open_mode, c("file", "localhost"))

  if (identical(open_mode, "localhost")) {
    .browse_docs_ensure_server(dirname(path), server_host, server_port)
    url <- sprintf(
      "http://%s:%s/%s",
      server_host,
      server_port,
      utils::URLencode(basename(path), reserved = TRUE)
    )
  } else {
    url <- paste0("file://", path)
  }

  utils::browseURL(url, browser = browser)
  invisible(url)
}

browse_docs <- function(
    docs,
    doc_id    = NULL,      # integer/numeric/character doc_id(s); when set, show matching thread(s)
    ym       = NULL,      # character vector of "YYYY-MM" to keep; NULL = all
    pattern  = NULL,      # regex to filter rows AND highlight in text
    n        = NULL,      # integer: random sample after filtering; NULL = all
    raw      = TRUE,      # TRUE → show text_raw instead of text
    show_both = FALSE,    # TRUE -> show both text and text_raw
    view     = c("flat", "thread"),
    sample_mode = c(
      "random", "top_scored", "top_scored_by_ym",
      "most_commented_threads", "most_commented_threads_by_ym",
      "edge_cases", "time_stratified"
    ),
    seed     = 42L,
    out_dir  = getwd(),   # local directory for HTML output
    file_name = NULL,     # optional explicit output filename
    overwrite = FALSE,    # FALSE -> skip existing matching file
    open_file = TRUE,     # TRUE -> open HTML after write/skip
    open_mode = c("file", "localhost"),
    server_host = "127.0.0.1",
    server_port = 8765L,
    browser  = getOption("browser")
) {
  view <- match.arg(view)
  sample_mode <- match.arg(sample_mode)
  open_mode <- match.arg(open_mode)

  .slug <- function(x, max_len = 60L) {
    x <- paste(x, collapse = "_")
    x <- tolower(as.character(x))
    x <- gsub("[^a-z0-9]+", "_", x)
    x <- gsub("^_+|_+$", "", x)
    x <- gsub("_+", "_", x)
    if (!nzchar(x)) x <- "all"
    substr(x, 1L, max_len)
  }

  .default_file_name <- function(doc_id, ym, pattern, n, raw, show_both, seed, view, sample_mode) {
    parts <- c(
      "browse_docs",
      paste0("view_", view),
      paste0("sample_", sample_mode),
      if (!is.null(doc_id)) paste0("docid_", .slug(doc_id)) else NULL,
      if (!is.null(ym)) paste0("ym_", .slug(ym)) else "ym_all",
      if (!is.null(pattern)) paste0("pattern_", .slug(pattern)) else "pattern_all",
      if (!is.null(n)) paste0("n_", n) else "n_all",
      paste0("raw_", if (raw) "true" else "false"),
      paste0("both_", if (show_both) "true" else "false"),
      if (!is.null(n)) paste0("seed_", seed) else NULL
    )
    paste0(paste(parts, collapse = "__"), ".html")
  }

  .require_cols <- function(df, cols, what) {
    missing_cols <- setdiff(cols, names(df))
    if (length(missing_cols) > 0L) {
      stop(sprintf(
        "%s requires columns: %s",
        what,
        paste(missing_cols, collapse = ", ")
      ))
    }
  }

  # ── 1. Filter ──────────────────────────────────────────────────────────────
  all_docs <- as.data.frame(docs)
  all_docs <- all_docs[!is.na(all_docs$text) & nchar(all_docs$text) > 0L, ]
  d <- all_docs

  if (!is.null(doc_id)) {
    .require_cols(all_docs, c("doc_id", "link_id"), "doc_id lookup")
    doc_id_chr <- as.character(doc_id)
    hit_rows <- all_docs[as.character(all_docs$doc_id) %in% doc_id_chr, , drop = FALSE]
    if (nrow(hit_rows) == 0L) {
      stop("No documents match the requested doc_id values.")
    }
    hit_link_ids <- unique(as.character(stats::na.omit(hit_rows$link_id)))
    if (length(hit_link_ids) == 0L) {
      stop("Matched doc_id values do not have link_id values, so no thread can be shown.")
    }
    d <- all_docs[as.character(all_docs$link_id) %in% hit_link_ids, , drop = FALSE]
    view <- "thread"
    sample_mode <- "random"
    n <- NULL
  }

  if (is.null(doc_id) && !is.null(ym)) {
    d <- d[as.character(d$ym) %in% ym, ]
    if (nrow(d) == 0L) stop("No documents match the requested ym values.")
  }

  if (is.null(doc_id) && !is.null(pattern)) {
    hits <- grepl(pattern, d$text, ignore.case = TRUE, perl = TRUE) |
            grepl(pattern, d$text_raw, ignore.case = TRUE, perl = TRUE)
    d <- d[hits, ]
    if (nrow(d) == 0L) stop("No documents match the pattern.")
  }

  filtered_docs <- d

  .safe_max <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0L) NA_real_ else max(as.numeric(x))
  }

  .safe_min <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0L) NA_real_ else min(as.numeric(x))
  }

  .require_cols <- function(df, cols, what) {
    missing_cols <- setdiff(cols, names(df))
    if (length(missing_cols) > 0L) {
      stop(sprintf(
        "%s requires columns: %s",
        what,
        paste(missing_cols, collapse = ", ")
      ))
    }
  }

  .sample_rows_by_ym <- function(df, n, seed) {
    if (is.null(n)) {
      stop("sample_mode = 'time_stratified' requires n (per-month sample size).")
    }
    .require_cols(df, "ym", "sample_mode = 'time_stratified'")

    ym_values <- sort(unique(as.character(stats::na.omit(df$ym))))
    out <- lapply(seq_along(ym_values), function(i) {
      ym_i <- ym_values[i]
      sub <- df[as.character(df$ym) == ym_i, , drop = FALSE]
      if (nrow(sub) <= n) {
        sub
      } else {
        set.seed(seed + i - 1L)
        sub[sample(nrow(sub), n), , drop = FALSE]
      }
    })

    do.call(rbind, out)
  }

  .top_rows_by_ym <- function(df, n, order_df = df, key_col = NULL, mode = "top_rows_by_ym") {
    if (is.null(n)) {
      stop(sprintf("sample_mode = '%s' requires n (top N per month).", mode))
    }
    .require_cols(df, "ym", sprintf("sample_mode = '%s'", mode))

    ym_values <- sort(unique(as.character(stats::na.omit(df$ym))))
    out <- lapply(ym_values, function(ym_i) {
      sub <- df[as.character(df$ym) == ym_i, , drop = FALSE]
      sub_order <- order_df[as.character(order_df$ym) == ym_i, , drop = FALSE]
      keep_n <- min(n, nrow(sub_order))
      if (keep_n == 0L) return(sub[0, , drop = FALSE])
      sub_order <- head(sub_order, keep_n)

      if (!is.null(key_col) && key_col %in% names(sub) && key_col %in% names(sub_order)) {
        keep_keys <- unique(as.character(sub_order[[key_col]]))
        sub[as.character(sub[[key_col]]) %in% keep_keys, , drop = FALSE]
      } else {
        head(sub, keep_n)
      }
    })

    do.call(rbind, out)
  }

  .thread_meta <- function(filtered_docs, all_docs) {
    .require_cols(filtered_docs, c("link_id", "ym"), "thread metadata")
    .require_cols(all_docs, c("link_id", "doc_type", "text"), "thread metadata")

    filtered_link_id <- as.character(filtered_docs$link_id)
    all_link_id <- as.character(all_docs$link_id)
    thread_ids <- unique(stats::na.omit(filtered_link_id))
    if (length(thread_ids) == 0L) {
      return(data.frame())
    }

    hit_docs_by_thread <- split(
      filtered_docs[!is.na(filtered_link_id) & filtered_link_id %in% thread_ids, , drop = FALSE],
      filtered_link_id[!is.na(filtered_link_id) & filtered_link_id %in% thread_ids]
    )

    full_threads_by_id <- split(
      all_docs[!is.na(all_link_id) & all_link_id %in% thread_ids, , drop = FALSE],
      all_link_id[!is.na(all_link_id) & all_link_id %in% thread_ids]
    )

    meta_list <- lapply(thread_ids, function(thread_id) {
      hit_docs <- hit_docs_by_thread[[thread_id]]
      full_thread <- full_threads_by_id[[thread_id]]
      sub <- full_thread[as.character(full_thread$doc_type) == "submission", , drop = FALSE]

      thread_score <- if (nrow(sub) > 0L && "score" %in% names(sub)) {
        .safe_max(sub$score)
      } else if ("score" %in% names(full_thread)) {
        .safe_max(full_thread$score)
      } else {
        NA_real_
      }

      thread_comments <- if (nrow(sub) > 0L && "num_comments" %in% names(sub)) {
        .safe_max(sub$num_comments)
      } else {
        sum(as.character(full_thread$doc_type) == "comment", na.rm = TRUE)
      }

      data.frame(
        link_id = thread_id,
        ym = as.character(hit_docs$ym[1]),
        thread_score = thread_score,
        num_comments = thread_comments,
        n_docs = nrow(full_thread),
        total_chars = sum(nchar(as.character(full_thread$text)), na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    })

    do.call(rbind, meta_list)
  }

  .sample_docs <- function(df, n, sample_mode, seed) {
    if (nrow(df) == 0L) return(df)

    if (identical(sample_mode, "random")) {
      if (!is.null(n) && nrow(df) > n) {
        set.seed(seed)
        df <- df[sample(nrow(df), n), , drop = FALSE]
      }
      return(df)
    }

    if (identical(sample_mode, "top_scored")) {
      .require_cols(df, "score", "sample_mode = 'top_scored'")
      ord <- order(
        -ifelse(is.na(df$score), -Inf, as.numeric(df$score)),
        if ("num_comments" %in% names(df)) -ifelse(is.na(df$num_comments), -Inf, as.numeric(df$num_comments)) else 0,
        if ("created_time_utc" %in% names(df)) -as.numeric(as.POSIXct(df$created_time_utc)) else 0,
        na.last = TRUE
      )
      df <- df[ord, , drop = FALSE]
      if (!is.null(n)) df <- head(df, n)
      return(df)
    }

    if (identical(sample_mode, "top_scored_by_ym")) {
      .require_cols(df, c("score", "ym"), "sample_mode = 'top_scored_by_ym'")
      ord <- order(
        as.character(df$ym),
        -ifelse(is.na(df$score), -Inf, as.numeric(df$score)),
        if ("num_comments" %in% names(df)) -ifelse(is.na(df$num_comments), -Inf, as.numeric(df$num_comments)) else 0,
        if ("created_time_utc" %in% names(df)) -as.numeric(as.POSIXct(df$created_time_utc)) else 0,
        na.last = TRUE
      )
      ordered_df <- df[ord, , drop = FALSE]
      return(.top_rows_by_ym(
        df = ordered_df,
        n = n,
        order_df = ordered_df,
        key_col = "doc_id",
        mode = "top_scored_by_ym"
      ))
    }

    if (identical(sample_mode, "most_commented_threads")) {
      .require_cols(df, c("link_id", "num_comments", "doc_type"), "sample_mode = 'most_commented_threads'")
      sub_df <- df[as.character(df$doc_type) == "submission", , drop = FALSE]
      if (nrow(sub_df) == 0L) stop("No submission rows available for most_commented_threads.")
      ord <- order(
        -ifelse(is.na(sub_df$num_comments), -Inf, as.numeric(sub_df$num_comments)),
        if ("score" %in% names(sub_df)) -ifelse(is.na(sub_df$score), -Inf, as.numeric(sub_df$score)) else 0,
        na.last = TRUE
      )
      top_threads <- unique(as.character(sub_df$link_id[ord]))
      if (!is.null(n)) top_threads <- head(top_threads, n)
      return(df[as.character(df$link_id) %in% top_threads, , drop = FALSE])
    }

    if (identical(sample_mode, "most_commented_threads_by_ym")) {
      .require_cols(df, c("link_id", "num_comments", "doc_type", "ym"), "sample_mode = 'most_commented_threads_by_ym'")
      sub_df <- df[as.character(df$doc_type) == "submission", , drop = FALSE]
      if (nrow(sub_df) == 0L) stop("No submission rows available for most_commented_threads_by_ym.")
      ord <- order(
        as.character(sub_df$ym),
        -ifelse(is.na(sub_df$num_comments), -Inf, as.numeric(sub_df$num_comments)),
        if ("score" %in% names(sub_df)) -ifelse(is.na(sub_df$score), -Inf, as.numeric(sub_df$score)) else 0,
        na.last = TRUE
      )
      ordered_sub <- sub_df[ord, , drop = FALSE]
      sampled_sub <- .top_rows_by_ym(
        df = ordered_sub,
        n = n,
        order_df = ordered_sub,
        key_col = "link_id",
        mode = "most_commented_threads_by_ym"
      )
      top_threads <- unique(as.character(sampled_sub$link_id))
      return(df[as.character(df$link_id) %in% top_threads, , drop = FALSE])
    }

    if (identical(sample_mode, "edge_cases")) {
      idx_parts <- list(
        shortest = order(nchar(as.character(df$text)), na.last = TRUE),
        longest = order(-nchar(as.character(df$text)), na.last = TRUE)
      )

      if ("score" %in% names(df)) {
        idx_parts$lowest_score <- order(ifelse(is.na(df$score), Inf, as.numeric(df$score)), na.last = TRUE)
        idx_parts$highest_score <- order(-ifelse(is.na(df$score), -Inf, as.numeric(df$score)), na.last = TRUE)
      }

      if ("created_time_utc" %in% names(df)) {
        ts <- as.numeric(as.POSIXct(df$created_time_utc))
        idx_parts$earliest <- order(ts, na.last = TRUE)
        idx_parts$latest <- order(-ts, na.last = TRUE)
      }

      if (is.null(n)) n <- min(20L, nrow(df))
      per_bucket <- max(1L, ceiling(n / max(1L, length(idx_parts))))
      idx <- unique(unlist(lapply(idx_parts, head, n = per_bucket)))
      idx <- idx[seq_len(min(length(idx), n))]
      return(df[idx, , drop = FALSE])
    }

    if (identical(sample_mode, "time_stratified")) {
      return(.sample_rows_by_ym(df, n = n, seed = seed))
    }

    df
  }

  .sample_threads <- function(filtered_docs, all_docs, n, sample_mode, seed) {
    .require_cols(filtered_docs, c("link_id", "id", "doc_type"), "thread view")
    meta <- .thread_meta(filtered_docs, all_docs)
    if (nrow(meta) == 0L) stop("No threads match the requested filters.")

    if (identical(sample_mode, "random")) {
      thread_ids <- meta$link_id
      if (!is.null(n) && length(thread_ids) > n) {
        set.seed(seed)
        thread_ids <- sample(thread_ids, n)
      }
      return(thread_ids)
    }

    if (identical(sample_mode, "top_scored")) {
      ord <- order(
        -ifelse(is.na(meta$thread_score), -Inf, meta$thread_score),
        -ifelse(is.na(meta$num_comments), -Inf, meta$num_comments),
        -meta$n_docs,
        na.last = TRUE
      )
      thread_ids <- meta$link_id[ord]
      if (!is.null(n)) thread_ids <- head(thread_ids, n)
      return(thread_ids)
    }

    if (identical(sample_mode, "top_scored_by_ym")) {
      .require_cols(meta, c("ym", "thread_score"), "sample_mode = 'top_scored_by_ym'")
      ord <- order(
        as.character(meta$ym),
        -ifelse(is.na(meta$thread_score), -Inf, meta$thread_score),
        -ifelse(is.na(meta$num_comments), -Inf, meta$num_comments),
        -meta$n_docs,
        na.last = TRUE
      )
      ordered_meta <- meta[ord, , drop = FALSE]
      sampled_meta <- .top_rows_by_ym(
        df = ordered_meta,
        n = n,
        order_df = ordered_meta,
        key_col = "link_id",
        mode = "top_scored_by_ym"
      )
      return(unique(sampled_meta$link_id))
    }

    if (identical(sample_mode, "most_commented_threads")) {
      ord <- order(
        -ifelse(is.na(meta$num_comments), -Inf, meta$num_comments),
        -ifelse(is.na(meta$thread_score), -Inf, meta$thread_score),
        -meta$n_docs,
        na.last = TRUE
      )
      thread_ids <- meta$link_id[ord]
      if (!is.null(n)) thread_ids <- head(thread_ids, n)
      return(thread_ids)
    }

    if (identical(sample_mode, "most_commented_threads_by_ym")) {
      .require_cols(meta, c("ym", "num_comments"), "sample_mode = 'most_commented_threads_by_ym'")
      ord <- order(
        as.character(meta$ym),
        -ifelse(is.na(meta$num_comments), -Inf, meta$num_comments),
        -ifelse(is.na(meta$thread_score), -Inf, meta$thread_score),
        -meta$n_docs,
        na.last = TRUE
      )
      ordered_meta <- meta[ord, , drop = FALSE]
      sampled_meta <- .top_rows_by_ym(
        df = ordered_meta,
        n = n,
        order_df = ordered_meta,
        key_col = "link_id",
        mode = "most_commented_threads_by_ym"
      )
      return(unique(sampled_meta$link_id))
    }

    if (identical(sample_mode, "edge_cases")) {
      idx_parts <- list(
        smallest_thread = order(meta$n_docs, na.last = TRUE),
        largest_thread = order(-meta$n_docs, na.last = TRUE),
        shortest_thread = order(meta$total_chars, na.last = TRUE),
        longest_thread = order(-meta$total_chars, na.last = TRUE),
        lowest_score = order(ifelse(is.na(meta$thread_score), Inf, meta$thread_score), na.last = TRUE),
        highest_score = order(-ifelse(is.na(meta$thread_score), -Inf, meta$thread_score), na.last = TRUE),
        lowest_comments = order(ifelse(is.na(meta$num_comments), Inf, meta$num_comments), na.last = TRUE),
        highest_comments = order(-ifelse(is.na(meta$num_comments), -Inf, meta$num_comments), na.last = TRUE)
      )
      if (is.null(n)) n <- min(20L, nrow(meta))
      per_bucket <- max(1L, ceiling(n / max(1L, length(idx_parts))))
      idx <- unique(unlist(lapply(idx_parts, head, n = per_bucket)))
      idx <- idx[seq_len(min(length(idx), n))]
      return(meta$link_id[idx])
    }

    if (identical(sample_mode, "time_stratified")) {
      sampled_meta <- .sample_rows_by_ym(meta, n = n, seed = seed)
      return(sampled_meta$link_id)
    }

    meta$link_id
  }

  if (identical(view, "thread")) {
    selected_thread_ids <- .sample_threads(
      filtered_docs = filtered_docs,
      all_docs = all_docs,
      n = n,
      sample_mode = sample_mode,
      seed = seed
    )
    all_link_id <- as.character(all_docs$link_id)
    d <- all_docs[!is.na(all_link_id) & all_link_id %in% selected_thread_ids, , drop = FALSE]
    d <- d[order(match(as.character(d$link_id), selected_thread_ids), d$created_time_utc, d$doc_id), , drop = FALSE]
  } else {
    d <- .sample_docs(
      df = filtered_docs,
      n = n,
      sample_mode = sample_mode,
      seed = seed
    )
    if (sample_mode %in% c("most_commented_threads", "most_commented_threads_by_ym")) {
      sub_df <- d[as.character(d$doc_type) == "submission", , drop = FALSE]
      ordered_thread_ids <- unique(as.character(sub_df$link_id[order(
        if ("ym" %in% names(sub_df) && identical(sample_mode, "most_commented_threads_by_ym")) as.character(sub_df$ym) else 0,
        -ifelse(is.na(sub_df$num_comments), -Inf, as.numeric(sub_df$num_comments)),
        if ("score" %in% names(sub_df)) -ifelse(is.na(sub_df$score), -Inf, as.numeric(sub_df$score)) else 0,
        na.last = TRUE
      )]))
      d <- d[order(match(as.character(d$link_id), ordered_thread_ids), d$created_time_utc, d$doc_id), , drop = FALSE]
    } else if (sample_mode %in% c("top_scored", "top_scored_by_ym")) {
      d <- d[order(
        if ("ym" %in% names(d) && identical(sample_mode, "top_scored_by_ym")) as.character(d$ym) else 0,
        -ifelse(is.na(d$score), -Inf, as.numeric(d$score)),
        if ("num_comments" %in% names(d)) -ifelse(is.na(d$num_comments), -Inf, as.numeric(d$num_comments)) else 0,
        na.last = TRUE
      ), , drop = FALSE]
    }
  }

  if (nrow(d) == 0L) {
    stop("No documents available after sampling.")
  }

  # ── 2. Build per-card HTML ─────────────────────────────────────────────────
  .hl <- function(txt, pat) {
    # Wrap each match in a <mark> tag
    if (is.null(pat)) return(txt)
    gsub(
      paste0("(", pat, ")"),
      "<mark>\\1</mark>",
      txt,
      ignore.case = TRUE,
      perl = TRUE
    )
  }

  .esc <- function(x) {
    x <- ifelse(is.na(x), "", as.character(x))
    x <- gsub("&",  "&amp;",  x, fixed = TRUE)
    x <- gsub("<",  "&lt;",   x, fixed = TRUE)
    x <- gsub(">",  "&gt;",   x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    x
  }

  .meta_parts <- function(row) {
    score_val <- if ("score" %in% names(row)) row$score else NA
    n_cmt_val <- if ("num_comments" %in% names(row)) row$num_comments else NA

    Filter(nchar, c(
      if (!is.na(score_val))  paste0("\u2191 ", score_val),
      if (!is.na(n_cmt_val)) paste0("\U1F4AC ", n_cmt_val),
      if ("author" %in% names(row) && !is.na(row$author)) paste0("u/", row$author),
      if ("created_time_utc" %in% names(row) && !is.na(row$created_time_utc))
        format(as.POSIXct(row$created_time_utc), "%Y-%m-%d %H:%M")
    ))
  }

  .render_body <- function(row) {
    body_col <- if (raw) "text_raw" else "text"
    body <- .esc(as.character(row[[body_col]]))
    body <- .hl(body, pattern)
    body_extra <- if (isTRUE(show_both) && all(c("text", "text_raw") %in% names(row))) {
      paste0(
        '<div class="raw-block"><div class="raw-label">',
        if (raw) "cleaned text" else "raw text",
        '</div><p class="raw-text">',
        .hl(.esc(as.character(row[[if (raw) "text" else "text_raw"]])), pattern),
        "</p></div>"
      )
    } else ""

    paste0('<p class="body-text">', body, '</p>', body_extra)
  }

  .render_flat_card <- function(row) {
    dtype     <- if ("doc_type" %in% names(row)) as.character(row$doc_type) else ""
    dtype_cls <- if (identical(dtype, "submission")) "badge badge-sub" else "badge badge-cmt"
    meta_parts <- .meta_parts(row)

    sprintf('
  <div class="card">
    <div class="card-header">
      <span class="ym-badge">%s</span>
      <span class="period-badge">%s</span>
      <span class="%s">%s</span>
      <span class="meta">%s</span>
      <span class="doc-id">#%s</span>
    </div>
    <div class="card-body">
      %s
    </div>
  </div>',
      as.character(row$ym),
      as.character(row$period),
      dtype_cls, dtype,
      paste(meta_parts, collapse = " &nbsp;|&nbsp; "),
      row$doc_id,
      .render_body(row)
    )
  }

  .thread_depths <- function(thread_df) {
    ids <- as.character(thread_df$id)
    parents <- as.character(thread_df$parent_id)
    names(parents) <- ids

    vapply(ids, function(cur_id) {
      depth <- 0L
      seen <- character()
      parent <- parents[[cur_id]]

      while (!is.na(parent) && nzchar(parent) && startsWith(parent, "t1_") && parent %in% ids && !(parent %in% seen)) {
        depth <- depth + 1L
        seen <- c(seen, parent)
        parent <- parents[[parent]]
      }

      depth
    }, integer(1L))
  }

  .render_thread <- function(thread_df) {
    thread_df <- thread_df[order(thread_df$created_time_utc, thread_df$doc_id), , drop = FALSE]
    submission_idx <- which(as.character(thread_df$doc_type) == "submission")
    submission <- if (length(submission_idx) > 0L) thread_df[submission_idx[1], , drop = FALSE] else NULL
    comments <- thread_df[as.character(thread_df$doc_type) != "submission", , drop = FALSE]

    submission_html <- ""
    if (!is.null(submission)) {
      sub_row <- submission[1, ]
      submission_html <- sprintf(
        '<div class="thread-submission">
          <div class="thread-submission-header">
            <span class="badge badge-sub">submission</span>
            <span class="meta">%s</span>
            <span class="doc-id">#%s</span>
          </div>
          <div class="thread-submission-body">%s</div>
        </div>',
        paste(.meta_parts(sub_row), collapse = " &nbsp;|&nbsp; "),
        sub_row$doc_id,
        .render_body(sub_row)
      )
    }

    comments_html <- ""
    if (nrow(comments) > 0L) {
      depths <- .thread_depths(comments)
      comments_html <- paste(vapply(seq_len(nrow(comments)), function(i) {
        row <- comments[i, ]
        depth <- depths[i]
        indent_px <- min(depth, 8L) * 22L
        sprintf(
          '<div class="thread-comment depth-%s">
            <div class="thread-comment-header">
              <span class="badge badge-cmt">comment</span>
              <span class="reply-depth">depth %s</span>
              <span class="meta">%s</span>
              <span class="doc-id">#%s</span>
            </div>
            <div class="thread-comment-body">%s</div>
          </div>',
          min(depth, 8L),
          depth,
          paste(.meta_parts(row), collapse = " &nbsp;|&nbsp; "),
          row$doc_id,
          .render_body(row)
        )
      }, character(1L)), collapse = "\n")
    }

    thread_id <- if (!is.null(submission)) as.character(submission$id[1]) else as.character(thread_df$link_id[1])
    thread_label <- if (!is.null(submission)) {
      body_col <- if (raw) "text_raw" else "text"
      substr(gsub("\\s+", " ", as.character(submission[[body_col]][1])), 1L, 120L)
    } else {
      paste("Thread", thread_id)
    }

    sprintf(
      '<section class="thread">
        <div class="thread-header">
          <span class="thread-id">%s</span>
          <span class="thread-size">%s docs</span>
          <span class="thread-label">%s</span>
        </div>
        %s
        <div class="thread-comments">%s</div>
      </section>',
      .esc(thread_id),
      nrow(thread_df),
      .esc(thread_label),
      submission_html,
      comments_html
    )
  }

  cards <- if (identical(view, "thread")) {
    threads <- split(d, as.character(d$link_id))
    vapply(threads, .render_thread, character(1L))
  } else {
    vapply(seq_len(nrow(d)), function(i) .render_flat_card(d[i, , drop = FALSE]), character(1L))
  }

  # ── 3. Assemble full HTML page ─────────────────────────────────────────────
  title_str <- paste0(
    "docs_for_tokens — ",
    "view: ", view, " — ",
    "sample: ", sample_mode, " — ",
    if (!is.null(doc_id)) paste0("doc_id: ", paste(doc_id, collapse = ", "), " | ") else "",
    if (!is.null(ym))      paste("ym:", paste(ym, collapse = ", ")) else "all months",
    if (!is.null(pattern)) paste0(" | pattern: \"", pattern, "\""),
    " (", nrow(d), " docs)"
  )

  html <- paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>', .esc(title_str), '</title>
<style>
  *, *::before, *::after { box-sizing: border-box; }
  body {
    font-family: "Segoe UI", system-ui, sans-serif;
    background: #f4f5f7;
    color: #24292f;
    margin: 0;
    padding: 1rem 1.5rem 3rem;
  }
  h1 {
    font-size: 1rem;
    font-weight: 600;
    color: #57606a;
    margin: 0 0 1.2rem;
    padding-bottom: .5rem;
    border-bottom: 1px solid #d0d7de;
  }
  .card {
    background: #fff;
    border: 1px solid #d0d7de;
    border-radius: 8px;
    margin-bottom: 1rem;
    overflow: hidden;
    box-shadow: 0 1px 3px rgba(0,0,0,.06);
  }
  .card-header {
    background: #f6f8fa;
    padding: .45rem .8rem;
    border-bottom: 1px solid #d0d7de;
    display: flex;
    align-items: center;
    gap: .5rem;
    flex-wrap: wrap;
    font-size: .8rem;
  }
  .ym-badge {
    font-weight: 700;
    color: #0969da;
  }
  .period-badge {
    color: #6e7781;
    font-style: italic;
  }
  .badge {
    border-radius: 4px;
    padding: .1em .45em;
    font-size: .72rem;
    font-weight: 600;
  }
  .badge-sub { background: #ddf4ff; color: #0550ae; }
  .badge-cmt { background: #fff8c5; color: #7d4e00; }
  .meta { color: #57606a; margin-left: auto; }
  .doc-id { color: #adb5bd; font-size: .7rem; }
  .card-body { padding: .75rem 1rem; }
  .body-text {
    margin: 0;
    line-height: 1.65;
    font-size: .92rem;
    white-space: pre-wrap;
    word-break: break-word;
  }
  .body-text,
  .raw-text,
  .thread-submission-body,
  .thread-comment-body {
    user-select: text;
  }
  mark {
    background: #fff3a3;
    color: inherit;
    border-radius: 2px;
    padding: 0 1px;
  }
  .raw-block { margin-top: .6rem; }
  .raw-label {
    font-size: .78rem;
    color: #6e7781;
    margin-bottom: .35rem;
  }
  .raw-text {
    margin: .4rem 0 0;
    font-size: .82rem;
    color: #57606a;
    line-height: 1.55;
    background: #f6f8fa;
    border-left: 3px solid #d0d7de;
    padding: .4rem .7rem;
    white-space: pre-wrap;
    word-break: break-word;
  }
  .thread {
    background: #fff;
    border: 1px solid #d0d7de;
    border-radius: 10px;
    margin-bottom: 1.25rem;
    overflow: hidden;
    box-shadow: 0 1px 3px rgba(0,0,0,.06);
  }
  .thread-header {
    display: flex;
    gap: .6rem;
    flex-wrap: wrap;
    align-items: center;
    padding: .7rem .9rem;
    background: #eef6ff;
    border-bottom: 1px solid #d0d7de;
    font-size: .82rem;
  }
  .thread-id { font-weight: 700; color: #0550ae; }
  .thread-size { color: #57606a; }
  .thread-label { color: #24292f; }
  .thread-submission,
  .thread-comment {
    padding: .8rem .95rem;
    border-top: 1px solid #eef2f6;
  }
  .thread-submission { background: #fbfdff; }
  .thread-submission-header,
  .thread-comment-header {
    display: flex;
    gap: .5rem;
    flex-wrap: wrap;
    align-items: center;
    font-size: .8rem;
    margin-bottom: .45rem;
  }
  .reply-depth {
    font-size: .72rem;
    color: #6e7781;
    background: #f6f8fa;
    border-radius: 999px;
    padding: .08rem .4rem;
  }
  .depth-1 { margin-left: 22px; }
  .depth-2 { margin-left: 44px; }
  .depth-3 { margin-left: 66px; }
  .depth-4 { margin-left: 88px; }
  .depth-5 { margin-left: 110px; }
  .depth-6 { margin-left: 132px; }
  .depth-7 { margin-left: 154px; }
  .depth-8 { margin-left: 176px; }
</style>
</head>
<body>
<h1>', .esc(title_str), '</h1>
', paste(cards, collapse = "\n"), '
</body>
</html>')

  # ── 4. Write locally and optionally open ───────────────────────────────────
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (is.null(file_name)) {
    file_name <- .default_file_name(
      doc_id = doc_id,
      ym = ym,
      pattern = pattern,
      n = n,
      raw = raw,
      show_both = show_both,
      seed = seed,
      view = view,
      sample_mode = sample_mode
    )
  }

  out_path <- normalizePath(file.path(out_dir, file_name), winslash = "/", mustWork = FALSE)

  if (file.exists(out_path) && !isTRUE(overwrite)) {
    message(sprintf(
      "HTML already exists for these parameters, skipping write: %s",
      out_path
    ))
    if (isTRUE(open_file)) {
      .browse_docs_open(
        path = out_path,
        browser = browser,
        open_mode = open_mode,
        server_host = server_host,
        server_port = server_port
      )
    }
    return(invisible(out_path))
  }

  writeLines(html, out_path, useBytes = FALSE)
  message(sprintf("Wrote %d cards → %s", nrow(d), out_path))
  if (isTRUE(open_file)) {
    .browse_docs_open(
      path = out_path,
      browser = browser,
      open_mode = open_mode,
      server_host = server_host,
      server_port = server_port
    )
  }
  invisible(out_path)
}


# browse_docs_plain(): minimal HTML for browser translation plugins.
browse_docs_plain <- function(
    docs,
    doc_id    = NULL,
    ym       = NULL,
    pattern  = NULL,
    n        = NULL,
    raw      = TRUE,
    show_both = FALSE,
    view     = c("flat", "thread"),
    sample_mode = c(
      "random", "top_scored", "top_scored_by_ym",
      "most_commented_threads", "most_commented_threads_by_ym",
      "time_stratified"
    ),
    seed     = 42L,
    out_dir  = getwd(),
    file_name = NULL,
    overwrite = FALSE,
    open_file = TRUE,
    open_mode = c("file", "localhost"),
    server_host = "127.0.0.1",
    server_port = 8765L,
    browser  = getOption("browser")
) {
  view <- match.arg(view)
  sample_mode <- match.arg(sample_mode)
  open_mode <- match.arg(open_mode)

  .slug <- function(x, max_len = 60L) {
    x <- paste(x, collapse = "_")
    x <- tolower(as.character(x))
    x <- gsub("[^a-z0-9]+", "_", x)
    x <- gsub("^_+|_+$", "", x)
    x <- gsub("_+", "_", x)
    if (!nzchar(x)) x <- "all"
    substr(x, 1L, max_len)
  }

  .esc <- function(x) {
    x <- ifelse(is.na(x), "", as.character(x))
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    x
  }

  .hl <- function(txt, pat) {
    if (is.null(pat)) return(txt)
    gsub(paste0("(", pat, ")"), "<mark>\\1</mark>", txt, ignore.case = TRUE, perl = TRUE)
  }

  .require_cols <- function(df, cols, what) {
    missing_cols <- setdiff(cols, names(df))
    if (length(missing_cols) > 0L) {
      stop(sprintf("%s requires columns: %s", what, paste(missing_cols, collapse = ", ")))
    }
  }

  .default_file_name <- function() {
    parts <- c(
      "browse_docs_plain",
      paste0("view_", view),
      paste0("sample_", sample_mode),
      if (!is.null(doc_id)) paste0("docid_", .slug(doc_id)) else NULL,
      if (!is.null(ym)) paste0("ym_", .slug(ym)) else "ym_all",
      if (!is.null(pattern)) paste0("pattern_", .slug(pattern)) else "pattern_all",
      if (!is.null(n)) paste0("n_", n) else "n_all",
      paste0("raw_", if (raw) "true" else "false"),
      paste0("both_", if (show_both) "true" else "false")
    )
    paste0(paste(parts, collapse = "__"), ".html")
  }

  .sample_rows_by_ym <- function(df, n, mode) {
    if (is.null(n)) stop(sprintf("sample_mode = '%s' requires n.", mode))
    .require_cols(df, "ym", sprintf("sample_mode = '%s'", mode))
    ym_values <- sort(unique(as.character(stats::na.omit(df$ym))))

    out <- lapply(seq_along(ym_values), function(i) {
      ym_i <- ym_values[i]
      sub <- df[as.character(df$ym) == ym_i, , drop = FALSE]

      if (identical(mode, "time_stratified")) {
        if (nrow(sub) <= n) return(sub)
        set.seed(seed + i - 1L)
        return(sub[sample(nrow(sub), n), , drop = FALSE])
      }

      ord <- if (identical(mode, "most_commented_threads_by_ym")) {
        .require_cols(sub, "num_comments", "sample_mode = 'most_commented_threads_by_ym'")
        order(
          -ifelse(is.na(sub$num_comments), -Inf, as.numeric(sub$num_comments)),
          if ("score" %in% names(sub)) -ifelse(is.na(sub$score), -Inf, as.numeric(sub$score)) else 0,
          if ("created_time_utc" %in% names(sub)) -as.numeric(as.POSIXct(sub$created_time_utc)) else 0,
          na.last = TRUE
        )
      } else {
        order(
          -ifelse(is.na(sub$score), -Inf, as.numeric(sub$score)),
          if ("num_comments" %in% names(sub)) -ifelse(is.na(sub$num_comments), -Inf, as.numeric(sub$num_comments)) else 0,
          if ("created_time_utc" %in% names(sub)) -as.numeric(as.POSIXct(sub$created_time_utc)) else 0,
          na.last = TRUE
        )
      }
      head(sub[ord, , drop = FALSE], n)
    })

    do.call(rbind, out)
  }

  .sample_threads <- function(df) {
    .require_cols(df, c("link_id", "doc_type"), "thread view")
    link_id <- as.character(df$link_id)
    df <- df[!is.na(link_id) & nzchar(link_id), , drop = FALSE]
    link_id <- as.character(df$link_id)
    threads <- split(df, link_id)

    meta <- do.call(rbind, lapply(names(threads), function(id) {
      x <- threads[[id]]
      sub <- x[as.character(x$doc_type) == "submission", , drop = FALSE]
      data.frame(
        link_id = id,
        ym = as.character(x$ym[1]),
        score = if (nrow(sub) > 0L && "score" %in% names(sub)) as.numeric(sub$score[1]) else suppressWarnings(max(as.numeric(x$score), na.rm = TRUE)),
        num_comments = if (nrow(sub) > 0L && "num_comments" %in% names(sub)) as.numeric(sub$num_comments[1]) else sum(as.character(x$doc_type) == "comment", na.rm = TRUE),
        created_time_utc = if ("created_time_utc" %in% names(x)) min(as.POSIXct(x$created_time_utc), na.rm = TRUE) else as.POSIXct(NA),
        stringsAsFactors = FALSE
      )
    }))

    if (identical(sample_mode, "random")) {
      ids <- meta$link_id
      if (!is.null(n) && length(ids) > n) {
        set.seed(seed)
        ids <- sample(ids, n)
      }
    } else if (identical(sample_mode, "top_scored")) {
      ord <- order(-ifelse(is.na(meta$score), -Inf, meta$score), na.last = TRUE)
      ids <- meta$link_id[ord]
      if (!is.null(n)) ids <- head(ids, n)
    } else if (identical(sample_mode, "top_scored_by_ym")) {
      .require_cols(meta, c("ym", "score"), "sample_mode = 'top_scored_by_ym'")
      ym_values <- sort(unique(as.character(stats::na.omit(meta$ym))))
      ids <- unlist(lapply(ym_values, function(ym_i) {
        sub <- meta[as.character(meta$ym) == ym_i, , drop = FALSE]
        ord <- order(-ifelse(is.na(sub$score), -Inf, sub$score), na.last = TRUE)
        head(sub$link_id[ord], n)
      }), use.names = FALSE)
    } else if (identical(sample_mode, "most_commented_threads")) {
      .require_cols(meta, "num_comments", "sample_mode = 'most_commented_threads'")
      ord <- order(
        -ifelse(is.na(meta$num_comments), -Inf, meta$num_comments),
        -ifelse(is.na(meta$score), -Inf, meta$score),
        na.last = TRUE
      )
      ids <- meta$link_id[ord]
      if (!is.null(n)) ids <- head(ids, n)
    } else if (identical(sample_mode, "most_commented_threads_by_ym")) {
      .require_cols(meta, c("ym", "num_comments"), "sample_mode = 'most_commented_threads_by_ym'")
      ym_values <- sort(unique(as.character(stats::na.omit(meta$ym))))
      ids <- unlist(lapply(ym_values, function(ym_i) {
        sub <- meta[as.character(meta$ym) == ym_i, , drop = FALSE]
        ord <- order(
          -ifelse(is.na(sub$num_comments), -Inf, sub$num_comments),
          -ifelse(is.na(sub$score), -Inf, sub$score),
          na.last = TRUE
        )
        head(sub$link_id[ord], n)
      }), use.names = FALSE)
    } else if (identical(sample_mode, "time_stratified")) {
      .require_cols(meta, "ym", "sample_mode = 'time_stratified'")
      ym_values <- sort(unique(as.character(stats::na.omit(meta$ym))))
      ids <- unlist(lapply(seq_along(ym_values), function(i) {
        ym_i <- ym_values[i]
        sub <- meta[as.character(meta$ym) == ym_i, , drop = FALSE]
        if (is.null(n) || nrow(sub) <= n) return(sub$link_id)
        set.seed(seed + i - 1L)
        sub$link_id[sample(nrow(sub), n)]
      }), use.names = FALSE)
    }

    df[as.character(df$link_id) %in% ids, , drop = FALSE]
  }

  d <- as.data.frame(docs)
  d <- d[!is.na(d$text) & nchar(d$text) > 0L, , drop = FALSE]

  if (!is.null(doc_id)) {
    .require_cols(d, c("doc_id", "link_id"), "doc_id lookup")
    doc_id_chr <- as.character(doc_id)
    hit_rows <- d[as.character(d$doc_id) %in% doc_id_chr, , drop = FALSE]
    if (nrow(hit_rows) == 0L) stop("No documents match the requested doc_id values.")
    hit_link_ids <- unique(as.character(stats::na.omit(hit_rows$link_id)))
    if (length(hit_link_ids) == 0L) stop("Matched doc_id values do not have link_id values, so no thread can be shown.")
    d <- d[as.character(d$link_id) %in% hit_link_ids, , drop = FALSE]
    view <- "thread"
    sample_mode <- "random"
    n <- NULL
  }

  if (is.null(doc_id) && !is.null(ym)) {
    d <- d[as.character(d$ym) %in% ym, , drop = FALSE]
    if (nrow(d) == 0L) stop("No documents match the requested ym values.")
  }

  if (is.null(doc_id) && !is.null(pattern)) {
    hits <- grepl(pattern, d$text, ignore.case = TRUE, perl = TRUE) |
      grepl(pattern, d$text_raw, ignore.case = TRUE, perl = TRUE)
    d <- d[hits, , drop = FALSE]
    if (nrow(d) == 0L) stop("No documents match the pattern.")
  }

  if (identical(view, "thread")) {
    d <- .sample_threads(d)
  } else if (identical(sample_mode, "random")) {
    if (!is.null(n) && nrow(d) > n) {
      set.seed(seed)
      d <- d[sample(nrow(d), n), , drop = FALSE]
    }
  } else if (identical(sample_mode, "top_scored")) {
    .require_cols(d, "score", "sample_mode = 'top_scored'")
    ord <- order(
      -ifelse(is.na(d$score), -Inf, as.numeric(d$score)),
      if ("created_time_utc" %in% names(d)) -as.numeric(as.POSIXct(d$created_time_utc)) else 0,
      na.last = TRUE
    )
    d <- d[ord, , drop = FALSE]
    if (!is.null(n)) d <- head(d, n)
  } else if (sample_mode %in% c("top_scored_by_ym", "most_commented_threads_by_ym", "time_stratified")) {
    if (identical(sample_mode, "top_scored_by_ym")) .require_cols(d, "score", "sample_mode = 'top_scored_by_ym'")
    if (identical(sample_mode, "most_commented_threads_by_ym")) .require_cols(d, "num_comments", "sample_mode = 'most_commented_threads_by_ym'")
    d <- .sample_rows_by_ym(d, n = n, mode = sample_mode)
  } else if (identical(sample_mode, "most_commented_threads")) {
    .require_cols(d, c("link_id", "num_comments", "doc_type"), "sample_mode = 'most_commented_threads'")
    sub_df <- d[as.character(d$doc_type) == "submission", , drop = FALSE]
    if (nrow(sub_df) == 0L) stop("No submission rows available for most_commented_threads.")
    ord <- order(
      -ifelse(is.na(sub_df$num_comments), -Inf, as.numeric(sub_df$num_comments)),
      if ("score" %in% names(sub_df)) -ifelse(is.na(sub_df$score), -Inf, as.numeric(sub_df$score)) else 0,
      na.last = TRUE
    )
    top_threads <- unique(as.character(sub_df$link_id[ord]))
    if (!is.null(n)) top_threads <- head(top_threads, n)
    d <- d[as.character(d$link_id) %in% top_threads, , drop = FALSE]
  }

  if (nrow(d) == 0L) stop("No documents available after sampling.")

  body_col <- if (raw) "text_raw" else "text"

  meta_line <- function(row) {
    paste(
      Filter(nzchar, c(
        if ("ym" %in% names(row)) paste0("ym=", as.character(row$ym)),
        if ("period" %in% names(row)) paste0("period=", as.character(row$period)),
        if ("doc_type" %in% names(row)) paste0("type=", as.character(row$doc_type)),
        if ("author" %in% names(row) && !is.na(row$author)) paste0("author=", as.character(row$author)),
        if ("score" %in% names(row) && !is.na(row$score)) paste0("score=", as.character(row$score)),
        if ("num_comments" %in% names(row) && !is.na(row$num_comments)) paste0("num_comments=", as.character(row$num_comments)),
        if ("id" %in% names(row) && !is.na(row$id)) paste0("id=", as.character(row$id)),
        if ("parent_id" %in% names(row) && !is.na(row$parent_id)) paste0("parent_id=", as.character(row$parent_id)),
        if ("link_id" %in% names(row) && !is.na(row$link_id)) paste0("link_id=", as.character(row$link_id))
      )),
      collapse = " | "
    )
  }

  render_flat <- function(df) {
    paste(vapply(seq_len(nrow(df)), function(i) {
      row <- df[i, , drop = FALSE]
      text_val <- .hl(.esc(as.character(row[[body_col]])), pattern)
      extra_val <- if (isTRUE(show_both) && all(c("text", "text_raw") %in% names(row))) {
        paste0(
          "<p>",
          .esc(if (raw) "cleaned text" else "raw text"),
          "</p>\n<div>",
          .hl(.esc(as.character(row[[if (raw) "text" else "text_raw"]])), pattern),
          "</div>\n"
        )
      } else ""
      paste0(
        "<article>\n",
        "<h2>Document ", .esc(as.character(row$doc_id)), "</h2>\n",
        "<p>", .esc(meta_line(row)), "</p>\n",
        "<div>", text_val, "</div>\n",
        extra_val,
        "</article>\n"
      )
    }, character(1L)), collapse = "\n<hr>\n")
  }

  render_thread <- function(df) {
    threads <- split(df, as.character(df$link_id))
    paste(vapply(threads, function(thread_df) {
      thread_df <- thread_df[order(thread_df$created_time_utc, thread_df$doc_id), , drop = FALSE]
      blocks <- vapply(seq_len(nrow(thread_df)), function(i) {
        row <- thread_df[i, , drop = FALSE]
        label <- if ("doc_type" %in% names(row)) as.character(row$doc_type) else "document"
        text_val <- .hl(.esc(as.character(row[[body_col]])), pattern)
        extra_val <- if (isTRUE(show_both) && all(c("text", "text_raw") %in% names(row))) {
          paste0(
            "<p>",
            .esc(if (raw) "cleaned text" else "raw text"),
            "</p>\n<div>",
            .hl(.esc(as.character(row[[if (raw) "text" else "text_raw"]])), pattern),
            "</div>\n"
          )
        } else ""
        paste0(
          "<section>\n",
          "<h3>", .esc(label), " ", .esc(as.character(row$doc_id)), "</h3>\n",
          "<p>", .esc(meta_line(row)), "</p>\n",
          "<div>", text_val, "</div>\n",
          extra_val,
          "</section>\n"
        )
      }, character(1L))

      paste0(
        "<article>\n",
        "<h2>Thread ", .esc(as.character(thread_df$link_id[1])), "</h2>\n",
        paste(blocks, collapse = "\n"),
        "</article>\n"
      )
    }, character(1L)), collapse = "\n<hr>\n")
  }

  title_str <- paste0(
    "browse_docs_plain | view=", view,
    " | sample=", sample_mode,
    if (!is.null(doc_id)) paste0(" | doc_id=", paste(doc_id, collapse = ",")) else "",
    if (!is.null(ym)) paste0(" | ym=", paste(ym, collapse = ",")) else "",
    if (!is.null(pattern)) paste0(" | pattern=", pattern) else "",
    " | docs=", nrow(d)
  )

  html <- paste0(
    "<!DOCTYPE html>\n",
    "<html lang=\"en\">\n",
    "<head>\n",
    "<meta charset=\"UTF-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<title>", .esc(title_str), "</title>\n",
    "<style>\n",
    "body { font-family: serif; line-height: 1.5; margin: 24px; }\n",
    "article, section, div, p, h1, h2, h3 { display: block; }\n",
    "div, p { white-space: pre-wrap; }\n",
    "hr { margin: 24px 0; }\n",
    "mark { background: #fff3a3; }\n",
    "</style>\n",
    "</head>\n",
    "<body>\n",
    "<h1>", .esc(title_str), "</h1>\n",
    if (identical(view, "thread")) render_thread(d) else render_flat(d),
    "\n</body>\n</html>\n"
  )

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (is.null(file_name)) file_name <- .default_file_name()
  out_path <- normalizePath(file.path(out_dir, file_name), winslash = "/", mustWork = FALSE)

  if (file.exists(out_path) && !isTRUE(overwrite)) {
    message(sprintf("HTML already exists for these parameters, skipping write: %s", out_path))
    if (isTRUE(open_file)) {
      .browse_docs_open(
        path = out_path,
        browser = browser,
        open_mode = open_mode,
        server_host = server_host,
        server_port = server_port
      )
    }
    return(invisible(out_path))
  }

  writeLines(html, out_path, useBytes = FALSE)
  message(sprintf("Wrote %d rows -> %s", nrow(d), out_path))
  if (isTRUE(open_file)) {
    .browse_docs_open(
      path = out_path,
      browser = browser,
      open_mode = open_mode,
      server_host = server_host,
      server_port = server_port
    )
  }
  invisible(out_path)
}
