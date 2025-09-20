#################################################
#####   recode agree/disagree to 2 bins     #####
#################################################
recode_agree <- function(x) {
  x_norm <- str_squish(str_to_lower(as.character(x)))
  case_when(
    x_norm %in% c("strongly disagree", "disagree") ~ "Disagree",
    x_norm %in% c("agree", "strongly agree")       ~ "Agree",
    TRUE                                           ~ NA_character_
  )
}


#################################################
#####   recode frequency scale to 3 bins    #####
#################################################
recode_freq <- function(x) {
  x_norm <- str_squish(str_to_lower(as.character(x)))
  case_when(
    x_norm %in% c("always", "almost always", "usually") ~ "Usually/Always",
    x_norm %in% c("sometimes")                          ~ "Sometimes",
    x_norm %in% c("rarely", "never")                    ~ "Rarely/Never",
    TRUE                                                ~ NA_character_
  ) %>%
    factor(levels = c("Rarely/Never", "Sometimes", "Usually/Always"), ordered = TRUE)
}