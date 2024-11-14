
#' Outlier Detection
#'
#' Identifies outliers based on the base variable and replaces them with NA
#'
#' @param data Data frame containing the variables
#' @param variaveis Vector with the names of the variables to be analyzed
#' @param variavel_base Base independent variable for the analysis
#' @return A list with the outlier results and the modified data frame
#' @export
getOutliers <- function(data, variaveis, variavel_base) {

  pdf_file <- 'outliers_plots.pdf'
  resultados_outliers <- list()

  pdf(file = pdf_file, width = 8, height = 6)

  for (var in variaveis) {
    modelo <- lm(data[[var]] ~ data[[variavel_base]])
    previsao <- predict(modelo)
    residuos <- data[[var]] - previsao
    desvio_padrao <- sd(residuos, na.rm = TRUE)
    limiar_outlier <- 1.5 * desvio_padrao
    outliers <- which(abs(residuos) > limiar_outlier)
    resultados_outliers[[var]] <- list(
      modelo = modelo,
      residuos = residuos,
      outliers = outliers,
      limiar = limiar_outlier,
      outliers_valores = data[outliers, c(variavel_base, var)]
    )
    plot(data[[variavel_base]], data[[var]], main = paste(variavel_base, 'vs', var),
         xlab = variavel_base, ylab = var, pch = 19, col = 'blue')
    abline(modelo, col = 'red', lwd = 2)
    intervalo_conf <- predict(modelo, interval = 'confidence')
    matlines(data[[variavel_base]], intervalo_conf[, c('lwr', 'upr')], col = 'gray', lty = 2)
    points(data[[variavel_base]][outliers], data[[var]][outliers], col = 'red', pch = 19, cex = 1.5)
    text(data[[variavel_base]][outliers], data[[var]][outliers], labels = outliers, pos = 3, col = 'black', cex = 0.8)
    data[outliers, var] <- NA
  }
  dev.off()
  return(list(resultados_outliers = resultados_outliers, data_com_na = data))
}

