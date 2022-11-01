#' predictCurve
#'
#' @param params numeric vector giving model parameters
#' @param data data.frame containing x values to predict
#'
#' @return numeric vector of predicted y values
#' @export
#'
#' @examples
#' 
predictCurve <- function(params, data) {
  a <- params[1]
  b <- abs(params[2])
  c <- params[3]
  d <- params[4]
  x <- data$Conc
  predicted <- d + ((a - d) / (1 + ((x/c)^b)))
  return(predicted)
}
