# Se não tiver o pacote devtools, instale-o primeiro
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Carrega o devtools para criação do pacote
library(devtools)

# Nome do pacote
package_name <- "massimstat"

# Cria a estrutura básica do pacote
create_package(package_name)

# Função getOutliers para detecção e substituição de outliers
getOutliers_code <- "
#' Detecção de Outliers
#'
#' Identifica outliers com base na variável base e substitui por NA
#'
#' @param data Data frame contendo as variáveis
#' @param variaveis Vetor com nomes das variáveis a serem analisadas
#' @param variavel_base Variável independente base para a análise
#' @return Uma lista com os resultados dos outliers e o data frame modificado
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

# Função aovAncov para teste de influência do sexo e SVL nas variáveis
aovAncov_code <- "
#' ANOVA e ANCOVA para análise de influência do sexo e SVL
#'
#' Compara modelos com e sem interação e seleciona o melhor baseado no AIC
#'
#' @param data Data frame contendo as variáveis
#' @param variaveis Vetor com nomes das variáveis dependentes
#' @param variavel_sexo Nome da variável sexo
#' @param variavel_svl Nome da variável SVL
#' @return Lista com os resultados dos modelos de interação, soma e modelo selecionado
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
      modelo_selecionado <- 'Interação'
      aic_selecionado <- aic_interacao
      resumo_selecionado <- resumo_interacao
    } else {
      modelo_selecionado <- 'Soma'
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

# Função calc_stats para cálculos estatísticos
calc_stats_code <- "
#' Calcula estatísticas básicas
#'
#' @param x Um vetor numérico
#' @return Vetor com média, mediana, desvio padrão e coeficiente de variação
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

# Função remove_outliers para remoção de outliers
remove_outliers_code <- "
#' Remove outliers por interpolação
#'
#' @param data Data frame com variáveis dependentes e independentes
#' @param dependent_vars Vetor com nomes das variáveis dependentes
#' @param independent_var Nome da variável independente
#' @param distance_threshold Limite de distância para identificar outliers
#' @return Lista com data frame sem outliers e lista de outliers encontrados
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

# Documenta e instala o pacote localmente
document(package_name)
install(package_name, reload = TRUE)

# Carrega o pacote
library(massimstat)

# Agora o pacote massimstat está pronto e as funções podem ser usadas diretamente!

