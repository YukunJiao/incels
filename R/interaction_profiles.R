add_interaction_docvars <- function(docs) {
  docs <- docs |>
    dplyr::mutate(
      author_for_activity = dplyr::if_else(
        is.na(author) | author == "" | author == "[deleted]",
        NA_character_,
        as.character(author)
      )
    )

  author_counts <- docs |>
    dplyr::filter(!is.na(author_for_activity)) |>
    dplyr::count(author_for_activity, name = "author_n_docs")

  thread_counts <- docs |>
    dplyr::group_by(link_id) |>
    dplyr::summarise(
      thread_n_docs = dplyr::n(),
      thread_n_comments = sum(doc_type == "comment", na.rm = TRUE),
      thread_submission_score = {
        x <- score[doc_type == "submission"]
        if (length(x) == 0 || all(is.na(x))) NA_real_ else max(as.numeric(x), na.rm = TRUE)
      },
      thread_reported_num_comments = {
        x <- num_comments[doc_type == "submission"]
        if (length(x) == 0 || all(is.na(x))) NA_real_ else max(as.numeric(x), na.rm = TRUE)
      },
      .groups = "drop"
    )

  child_counts <- docs |>
    dplyr::filter(!is.na(parent_id), parent_id != "") |>
    dplyr::count(parent_id, name = "direct_child_count") |>
    dplyr::rename(id = parent_id)

  docs |>
    dplyr::left_join(author_counts, by = "author_for_activity") |>
    dplyr::left_join(thread_counts, by = "link_id") |>
    dplyr::left_join(child_counts, by = "id") |>
    dplyr::mutate(
      author_n_docs = dplyr::coalesce(author_n_docs, 0L),
      direct_child_count = dplyr::coalesce(direct_child_count, 0L),
      reply_type = dplyr::case_when(
        doc_type == "submission" ~ "submission",
        startsWith(as.character(parent_id), "t3_") ~ "top_level_comment",
        startsWith(as.character(parent_id), "t1_") ~ "nested_comment",
        TRUE ~ "comment_unknown"
      ),
      author_activity_bin = dplyr::case_when(
        is.na(author_for_activity) ~ "unknown",
        author_n_docs == 1 ~ "1",
        author_n_docs <= 5 ~ "2-5",
        author_n_docs <= 20 ~ "6-20",
        TRUE ~ "21+"
      )
    ) |>
    dplyr::group_by(ym) |>
    dplyr::mutate(
      score_pct_ym = dplyr::if_else(
        !is.na(score),
        dplyr::percent_rank(score),
        NA_real_
      ),
      submission_score_pct_ym = dplyr::if_else(
        doc_type == "submission" & !is.na(score),
        dplyr::percent_rank(score),
        NA_real_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      author_activity_bin = factor(
        author_activity_bin,
        levels = c("unknown", "1", "2-5", "6-20", "21+"),
        ordered = TRUE
      ),
      reply_type = factor(
        reply_type,
        levels = c("submission", "top_level_comment", "nested_comment", "comment_unknown")
      ),
      score_tier_ym = dplyr::case_when(
        is.na(score_pct_ym) ~ "missing",
        score_pct_ym >= 0.9 ~ "top10",
        score_pct_ym <= 0.1 ~ "bottom10",
        TRUE ~ "middle80"
      ),
      submission_score_tier_ym = dplyr::case_when(
        doc_type == "submission" & !is.na(submission_score_pct_ym) &
          submission_score_pct_ym >= 0.9 ~ paste0(as.character(ym), "__top10"),
        doc_type == "submission" & !is.na(submission_score_pct_ym) ~
          paste0(as.character(ym), "__other"),
        TRUE ~ NA_character_
      ),
      thread_comment_bin = dplyr::case_when(
        is.na(thread_n_comments) ~ NA_character_,
        thread_n_comments == 0 ~ "0",
        thread_n_comments <= 10 ~ "1-10",
        thread_n_comments <= 50 ~ "11-50",
        TRUE ~ "51+"
      ),
      direct_child_bin = dplyr::case_when(
        direct_child_count == 0 ~ "0",
        direct_child_count == 1 ~ "1",
        direct_child_count <= 5 ~ "2-5",
        TRUE ~ "6+"
      )
    ) |>
    dplyr::mutate(
      score_tier_ym = factor(score_tier_ym, levels = c("missing", "bottom10", "middle80", "top10")),
      thread_comment_bin = factor(thread_comment_bin, levels = c("0", "1-10", "11-50", "51+"), ordered = TRUE),
      direct_child_bin = factor(direct_child_bin, levels = c("0", "1", "2-5", "6+"), ordered = TRUE)
    )
}
