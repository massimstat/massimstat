# If the devtools package is not installed, install it first
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Load devtools for package creation
library(devtools)

# Package name
package_name <- "massimstat"

# Create the basic package structure
create_package(package_name)

# Function getOutliers for outlier detection and replacement
getOutliers_code <- "
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
"
writeLines(getOutliers_code, con = file.path(package_name, "R", "getOutliers.R"))

# Function aovAncov for testing the influence of sex and SVL on variables
aovAncov_code <- "
#' ANOVA and ANCOVA for analysis of the influence of sex and SVL
#'
#' Compares models with and without interaction and selects the best based on AIC
#'
#' @param data Data frame containing the variables
#' @param variaveis Vector with names of dependent variables
#' @param variavel_sexo Name of the sex variable
#' @param variavel_svl Name of the SVL variable
#' @return A list with the results of the interaction models, sum models, and selected model
#' @export
aovAncov <- function(data, variaveis, variavel_sexo, variavel_svl) {
  resultados_interacao <- list()
  resultados_soma <- list()
  resultados_selecionados <- list()

  for (var in variaveis) {
    formula_interacao <- as.formula(paste(var, '~', variavel_svl, '*', variavel_sexo))
    modelo_interacao <- aov(formula_interacao, data = data)
    aic_interacao <- AIC(modelo_interacao)
    resumo_interacao <- summary(modelo_interacao)

    formula_soma <- as.formula(paste(var, '~', variavel_svl, '+', variavel_sexo))
    modelo_soma <- aov(formula_soma, data = data)
    aic_soma <- AIC(modelo_soma)
    resumo_soma <- summary(modelo_soma)

    if (aic_interacao < aic_soma) {
      modelo_selecionado <- 'Interaction'
      aic_selecionado <- aic_interacao
      resumo_selecionado <- resumo_interacao
    } else {
      modelo_selecionado <- 'Sum'
      aic_selecionado <- aic_soma
      resumo_selecionado <- resumo_soma
    }

    resultados_interacao[[var]] <- list(modelo = modelo_interacao, resumo = resumo_interacao, AIC = aic_interacao)
    resultados_soma[[var]] <- list(modelo = modelo_soma, resumo = resumo_soma, AIC = aic_soma)
    resultados_selecionados[[var]] <- list(modelo = modelo_selecionado, AIC = aic_selecionado, resumo = resumo_selecionado)
  }

  return(list(
    resultados_interacao = resultados_interacao,
    resultados_soma = resultados_soma,
    resultados_selecionados = resultados_selecionados
  ))
}
"
writeLines(aovAncov_code, con = file.path(package_name, "R", "aovAncov.R"))

# Function calc_stats for statistical calculations
calc_stats_code <- "
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
"
writeLines(calc_stats_code, con = file.path(package_name, "R", "calc_stats.R"))

# Function remove_outliers for outlier removal
remove_outliers_code <- "
#' Remove outliers by interpolation
#'
#' @param data Data frame with dependent and independent variables
#' @param dependent_vars Vector with names of dependent variables
#' @param independent_var Name of the independent variable
#' @param distance_threshold Distance threshold to identify outliers
#' @return A list with the data frame without outliers and a list of found outliers
#' @export
remove_outliers <- function(data, dependent_vars, independent_var, distance_threshold) {
  outliers_list <- list()
  for (var in dependent_vars) {
    model <- lm(data[[var]] ~ data[[independent_var]])
    predictions <- predict(model)
    residuals <- abs(data[[var]] - predictions)
    outliers <- which(residuals > distance_threshold)
    outliers_list[[var]] <- outliers
    data[outliers, var] <- NA
  }
  return(list(data = data, outliers = outliers_list))
}
"
writeLines(remove_outliers_code, con = file.path(package_name, "R", "remove_outliers.R"))

# Document and install the package locally
document(package_name)  # This will generate the man folder and .Rd files
install(package_name, reload = TRUE)

# Load the package
library(massimstat)

# Now the massimstat package is ready, and the functions can be used directly!

