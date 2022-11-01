#' readFile
#'
#' @param file character string fileInput
#' @param headings boolean denoting whether to use headings
#'
#' @return data.frame
#' @export
#'
#' @examples
#' 
readFile <- function(file){
  
  inFile <- file
  if (is.null(inFile)) return(NULL)

  # Check file type
  if (grepl(".csv|.CSV", inFile$datapath)) {
    df <- read.csv(inFile$datapath, header = TRUE)
  } else if (grepl(".xls", inFile$datapath)) {
    df <- readxl::read_excel(inFile$datapath, sheet = 1)
  } else {
    stop("Please upload your data in .csv/.xlsx/.xls format.")
  }
  
  return(df)
}

