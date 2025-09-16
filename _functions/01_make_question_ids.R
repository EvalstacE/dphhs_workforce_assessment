

# Pipe-friendly helper: returns a vector the same length as nrow(.data)
make_question_ids <- function(.data, prefix = "SecA", reps = 448) {
  n_questions <- nrow(.data) / reps
  if (n_questions %% 1 != 0) {
    stop(sprintf(
      "%s rows not divisible by reps=%s (got n_rows=%s).",
      prefix, reps, nrow(.data)
    ))
  }
  rep(paste0(prefix, seq_len(n_questions)), each = reps)
}

