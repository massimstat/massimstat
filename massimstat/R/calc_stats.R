
#' Calculate basic statistics
#'
#' @param x A numeric vector
#' @return A vector with mean, median, standard deviation, and coefficient of variation
#' @export
calc_stats <- function(x) {
  mean_value <- mean(x, na.rm = TRUE)
  median_value <- median(x, na.rm = TRUE)
  sd_value <- sd(x, na.rm = TRUE)
  cv_value <- (sd_value / mean_value) * 100
  return(c(mean = mean_value, median = median_value, sd = sd_value, cv = cv_value))
}

