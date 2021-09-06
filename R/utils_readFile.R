readFile <- function(file, headings, sheet = NULL){
  
  inFile <- file
  if (is.null(inFile)) return(NULL)
  
  # Check file type
  if (stringr::str_detect(inFile$datapath, ".csv") |stringr::str_detect(inFile$datapath, ".CSV") ) {
    df <- read.csv(inFile$datapath, header = TRUE)
  } else if (stringr::str_detect(inFile$datapath, ".xls")) {
    df <- readxl::read_excel(inFile$datapath, sheet = 1)
  } else {
    stop("Please upload your data in .csv/.xlsx/.xls format.")
  }
  
}