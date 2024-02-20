#' Creates the relevancy file
#'
#' #' @usage
#' library(atRfunctions)
#' create_relevancy_file(kobo_tool = kobo_tool,
#'                       ignore_reverse_check = c("Q1","Q2","Q10")
#'           )
#'
#' @param kobo_tool The data collection tool's survey sheet
#' @param ignore_reverse_check The list of question names to be ignored for reverse checking
#'
#' @return The input for check_relevancy_rules.R function
#' @import stringr
#'
#' @export
create_relevancy_file <-  function(kobo_tool, ignore_reverse_check = NULL){
  # source("./functions/convert_relevancy_to_R.R")


  # Imports required packages
  if(!require(stringr)) install.packages("stringr")

  # Checks if the required columns exist in Data Collection Tool
  if(!all(c("type", "name", "relevance") %in% names(kobo_tool))){
    stop("Required variable(s) not found in Kobo Tool")
  }

  # Unifies the relevancy column name
  if (!"relevant" %in% names(kobo_tool)) kobo_tool <- kobo_tool |> rename(relevant = relevance)

  # Takes care of group relevancy
  kobo_tool <- process_group_relevancies(kobo_tool)

  # source("functions/add_repeat_sheet_names_to_tool.R")

  # Add Repeat sheet name to Tool's row/question
  questions_sheet_classified <- add_repeat_sheet_names_to_questions(kobo_tool = kobo_tool)

  # Specify the column types for which the relevancy check won't be applied
  empty_cells = c(NA, "", "NA")
  to_be_excluded = c(unique(kobo_tool$type[grepl("[Bb]egin[_ ]", kobo_tool$type)]),
                     unique(kobo_tool$type[grepl("[Ee]nd[_ ]", kobo_tool$type)]),
                     "note", "start", "end", "deviceid", "xml-external", "audit", "background-audio")

  # Exclude question does not have relevancy
  kobo_tool <- kobo_tool[!kobo_tool$relevant %in% empty_cells,]
  kobo_tool <- kobo_tool[!kobo_tool$type %in% to_be_excluded,]

  # Start generating the relevancy file
  relevancy_file = data.frame(type = unlist(lapply(strsplit(kobo_tool$type, " "), function(x) x[1]))) # Type of question
  relevancy_file$name = kobo_tool[, "name"] |> unlist() |> unname() # Name of question
  relevancy_file$relevance_rule <- kobo_tool$relevant # Original Relevancy Rules
  relevancy_file$relevant_question <- questions_from_relevancy(relevancy_string = kobo_tool$relevant) # Extract only question names involved in Relevancy formula
  relevancy_file$relevant_value <- choices_from_relevancy(relevancy_string = kobo_tool$relevant) # Extract only values involved in Relevancy Formula
  relevancy_file$Rcondition <- convert_relevancy_to_R(kobo_tool) # Converts original formula to R statements
  relevancy_file$Rcondition <- gsub("\\\\", "\\\\\\\\", relevancy_file$Rcondition) # Handle ignore characters for grab function
  relevancy_file$Remarks <- "" # Blank Remark
  relevancy_file$check_reverse <- "TRUE" # Set TRUE for checking reverse case value
  if(!is.null(ignore_reverse_check) & length(ignore_reverse_check) > 0){
    relevancy_file$check_reverse[relevancy_file$name %in% ignore_reverse_check] <- "FALSE" # Set FALSE for checking reverse case
  }

  relevancy_file <- relevancy_file %>%
    left_join(questions_sheet_classified, by = "name") # Adding the sheet name for which question its belonged

  return(relevancy_file)
}
