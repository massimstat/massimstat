
#' Calcula estatísticas básicas
#'
#' @param x Um vetor numérico.
#' @return Um vetor com média, mediana, desvio padrão e coeficiente de variação.
#' @export
calc_stats <- function(x) {
  mean_value <- mean(x, na.rm = TRUE)
  median_value <- median(x, na.rm = TRUE)
  sd_value <- sd(x, na.rm = TRUE)
  cv_value <- (sd_value / mean_value) * 100  # Coeficiente de variação em porcentagem
  
  return(c(mean = mean_value, median = median_value, sd = sd_value, cv = cv_value))
}

