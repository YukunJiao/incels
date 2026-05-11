# select columns and remove comments from bots and texts shown as deleted and removed
prepare_text_dataset <- function(submissions_raw, comments_raw, bot_authors_csv = "bot_accounts.csv"){
  bot_authors <- read.csv(bot_authors_csv)
  
  submissions_selected <- submissions_raw |>
    dplyr::select(
      id,
      author,
      title,
      selftext,
      created_utc,
      score,
      num_comments
    ) |> 
    mutate(
      id = paste0("t3_", id),
    )
  
  comments_selected <- comments_raw |>
    dplyr::select(
      id,
      author,
      body,
      created_utc,
      score,
      parent_id,
      link_id
    ) |> 
    mutate(
      id = paste0("t1_", id),
    )
  
  submissions_labeled <- submissions_selected |>
    dplyr::mutate(
      is_bot  = author %in% bot_authors$author,
      deleted = as.integer(selftext == "[deleted]"),
      removed = as.integer(selftext == "[removed]")
    )
  
  comments_labeled <- comments_selected |>
    dplyr::mutate(
      is_bot  = author %in% bot_authors$author,
      deleted = as.integer(body == "[deleted]"),
      removed = as.integer(body == "[removed]")
    )
  
  list(
    submissions = submissions_labeled,
    comments    = comments_labeled
  )
}

clean_texts <- function(data) {
  fix_technical_errors <- function(x) {
    x <- ifelse(is.na(x), "", x)
    x <- textutils::HTMLdecode(x)  # decode HTML entities
    x <- stringr::str_replace_all(
      x,
      fixed("[This comment has been overwritten to protect the user's privacy.]"),
      " "
    )  # remove privacy-overwrite placeholder text
    x <- stringr::str_replace_all(
      x,
      "(?m)^\\s*>+\\s*.*(?:\\n|$)",
      " "
    )  # remove quoted forum lines
    x <- stringr::str_replace_all(x, ">{2,}", " ")  # remove residual quote markers
    x <- stringr::str_replace_all(x, "[\r\n\t]+", " ")  # normalize line breaks/tabs
    stringr::str_squish(x)
  }
  
  wrangle_vec <- function(x) fix_technical_errors(x)
  
  submissions_text <- data$submissions |>
    dplyr::mutate(
      title = wrangle_vec(title),
      selftext = wrangle_vec(selftext)
    )
  
  comments_text <- data$comments |>
    dplyr::mutate(
      body = wrangle_vec(body)
    )
  
  sub_docs <- submissions_text |>
    dplyr::transmute(
      id,
      link_id = id,
      parent_id = NA_character_,
      doc_type = "submission",
      created_utc,
      author,
      text = stringr::str_squish(
        paste(
          title,
          ifelse(selftext %in% c("[deleted]", "[removed]"), "", selftext)
        )
      ),
      num_comments,
      score
    )
  
  com_docs <- comments_text |>
    dplyr::filter(
      !is_bot,
      body != "[deleted]",
      body != "[removed]"
    ) |>
    dplyr::transmute(
      id,
      link_id,
      parent_id,
      doc_type = "comment",
      created_utc,
      author,
      text = body,
      score
    )
  
  docs <- dplyr::bind_rows(sub_docs, com_docs) |>
    dplyr::mutate(
      created_time_utc = as.POSIXct(
        as.numeric(created_utc),
        origin = "1970-01-01",
        tz = "UTC"
      ),
      ym = format(as.Date(created_time_utc), "%Y-%m"),
      n_tokens = stringr::str_count(text, "\\S+")
    ) |>
    dplyr::filter(n_tokens > 1) |>
    dplyr::select(-n_tokens)
  
  docs$ym <- factor(docs$ym, levels = sort(unique(docs$ym)))
  docs <- docs |>
    dplyr::filter(!stringr::str_starts(ym, "2014"))
  
  data.table::setDT(docs)
  docs[, doc_id := .I]
  docs[, text_raw := text]
  
  docs
}

simplify_texts <- function(docs,
                           text_col = "text",
                           out_col = "text",
                           min_run = 2,
                           keep = 1) {
  x <- docs[[text_col]]
  x <- ifelse(is.na(x), "", x)
  
  # replace URLs with a placeholder
  x <- stringr::str_replace_all(
    x,
    "(https?://\\S+|www\\.\\S+)",
    " __URL__ "
  )
  
  # normalize x/10 ratings
  x <- gsub(
    "\\b([0-9]{1,2})\\s*/\\s*10\\b",
    " __rate_\\1_10__ ",
    x,
    perl = TRUE
  )
  
  # normalize other ratios
  x <- gsub(
    "\\b([0-9]{1,4})\\s*/\\s*([0-9]{1,6})\\b",
    " __ratio_\\1_\\2__ ",
    x,
    perl = TRUE
  )
  
  # collapse consecutive repeats
  x <- vapply(
    x,
    collapse_consecutive_repeats,
    FUN.VALUE = character(1),
    min_run = min_run,
    keep = keep
  )
  
  x <- stringr::str_squish(x)
  
  docs[[out_col]] <- x
  docs$has_url <- as.integer(stringr::str_detect(x, "__URL__"))
  
  docs
}
