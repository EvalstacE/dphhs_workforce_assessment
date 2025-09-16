# Infer "SecX" from a path like ".../A_comm.csv"
infer_prefix <- function(path) {
  letter <- toupper(substr(basename(path), 1, 1))
  paste0("Sec", letter)
}



# Main function: read, add question_id, and stack
build_master_topics <- function(paths, reps = 448, use_here = TRUE) {
  paths <- as.character(paths)
  
  map_dfr(paths, function(p) {
    file_path <- if (use_here) here(p) else p
    pref <- infer_prefix(p)
    
    df <- read_csv(file_path, show_col_types = FALSE)
    
    df %>%
      mutate(
        question_id = make_question_ids(., prefix = pref, reps = reps),
        section = pref,
        source  = basename(p)
      )
  })
}
