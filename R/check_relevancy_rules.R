#' Check Relevancy Rule
#'
#' The function looks up for any value violate the relevancy rules defined in the tool.
#' However, the tool_relevancy parameter of the function is dependent to another function
#' called create_relevancy_file by which the parameter can get the suitable input
#'
#' @param data The data set for which the relevancy would be checked
#' @param tool_relevancy The output of create_relevancy_file function, a file in which all the relevancy formulas are converted to what is understandable by R
#' @param sheet_name The sheet name of data set in case it holds multiple sheets. Default value to "data"
#' @param KEY The Unique ID of observations. Default value to "KEY"
#'
#' @usage check_relevancy_rules(data,
#'                              tool_relevancy,
#'                              sheet_name = "data",
#'                              KEY = "KEY")
#'
#' @return Data frame, containing the relevancy rules broken
#' @import dplyr
#' @import stringr
#' @export
check_relevancy_rules <- function(data, tool_relevancy, sheet_name = "data", KEY="KEY"){
  # initiate Log
  `%notin%` <- Negate(`%in%`)
  relevancy_log <- data.frame()
  missing_cols <- c()
  missing_relev_cols <- c()

  # Filter the rules
  tool_relevancy <- tool_relevancy %>% filter(sheet==sheet_name)

  # Loop through relevancy rules
  questions <- tool_relevancy$name
  for(question_i in 1:length(questions)){
    question <- questions[question_i]
    relevant_question <- str_split(tool_relevancy$relevant_question[question_i], " - ")[[1]] %>% unique()
    check_reverse <- tool_relevancy$check_reverse[question_i]
    relevancy_sub <- tool_relevancy[tool_relevancy$name == question,]

    # Skip if question is missing
    if(question %notin% names(data)){
      missing_cols <- c(missing_cols, question)
      next
    } else if(any(!relevant_question %in% names(data))){
      missing_relev_cols <- c(missing_relev_cols, relevant_question[!relevant_question %in% names(data)])
    }

    # Conditional string
    conditional_string <- relevancy_sub$Rcondition
    conditional_str_negated <- relevancy_sub$Rcondition %>% paste0("!(", ., ")") #Negate

    ## Flag issues
    # Rows where Question has a value but relevant question does not apply
    flagged_rows <- which(data[[question]] %notin% c(NA, "", NaN) & eval(parse(text=conditional_str_negated)))
    # Rows where Relevant Question applies but the actual question is null
    if(check_reverse){
      flagged_rows <- c(
        flagged_rows,
        which(data[[question]] %in% c(NA, "", NaN) & eval(parse(text=conditional_string)))
      )}

    # Log if rows are flagged
    len_flagged <- length(flagged_rows)
    if(len_flagged > 0){
      # Get the values of relevant questions
      relevant_values <- data[flagged_rows, c(KEY,relevant_question)] %>%
        pivot_longer(-all_of(KEY), names_to = "cols", values_to = "value", values_transform=as.character) %>%
        group_by(across(KEY)) %>% mutate(total = paste0(value, collapse = " - "), value=NULL, cols=NULL) %>% # Summarize messed up the group order
        ungroup() %>% unique() %>% pull(total)

      log <- data.frame(KEY=data[[KEY]][flagged_rows],
                        question=rep(question, len_flagged),
                        value=data[[question]][flagged_rows],
                        relevancy_rule=rep(relevancy_sub$relevance_rule[1], len_flagged),
                        relevant_question=rep(paste0(relevant_question, collapse = " - "), len_flagged),
                        relev_value=relevant_values,
                        sheet=sheet_name)
      # qa_status=data$qa_status[flagged_rows])
      # Rbind
      relevancy_log <- rbind(relevancy_log, log)
    }
  }
  ## Print Columns missing from dataset
  if(length(missing_cols) != 0){
    message("Column missing from dataset: ")
    print(unique(missing_cols))
  }
  if(length(missing_relev_cols) != 0){
    message("Relevant column missing from dataset: ")
    print(unique(missing_relev_cols))
  }

  ## Print if no relevancy issues found
  if (nrow(relevancy_log) == 0) {
    print(paste0("No relevancy issues found in: ", sheet_name))
  }
  # End
  return(relevancy_log)
}
