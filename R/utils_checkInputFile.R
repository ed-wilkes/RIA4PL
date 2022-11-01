#' checkInputFile
#'
#' @param input_env shiny input object
#' @param input_id character string for inputId
#'
#' @return shinyFeedback object
#' @export
#'
#' @examples
#' 
checkInputFile <- function(input_env, input_id) {

  file <- input_env[[input_id]]

  if (!is.null(file)) {

    # Check input file extension
    if (!grepl(".csv|.xlsx|.xls|.CSV", file$datapath)) {
      
      shinyFeedback::showFeedbackDanger(
        inputId = input_id
        ,text = "File must be .csv, .xlsx, or .xls format!"
      )
      
    } else {
      
      df <- readFile(input_env[[input_id]])
      column_vector <- c("ReactionTube", "Name", "Counts")
      name_vector <- c("ZA", paste0("STD", 1:9))
      
      # Check column headers 
      if (all(column_vector %in% colnames(df)) == FALSE) {
        shinyFeedback::showFeedbackDanger(
          inputId = input_id
          ,text = "File must contain 'ReactionTube', 'Name', and 'Counts' columns!"
        )
      } else if (any(name_vector %in% df$Name) == FALSE) { 
        shinyFeedback::showFeedbackDanger(
          inputId = input_id
          ,text = "Column 'Name' must contain 'ZA', 'STD1-9'!"
        )
      } else {
        shinyFeedback::hideFeedback(input_id)
      }
      
    }

  } else {
    shinyFeedback::hideFeedback(input_id)
  }

}
