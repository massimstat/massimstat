
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

