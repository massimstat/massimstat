#' Chekup: Normality Analysis with Histograms
#'
#' This function performs normality analysis for specified variables in a dataset,
#' creates histograms, and saves the results in separate directories for raw and log-transformed data.
#'
#' @param data A data frame containing the variables to analyze.
#' @param variaveis A character vector with the names of the variables to analyze.
#' @param pasta_raw Directory to save the results for raw data.
#' @param pasta_log Directory to save the results for log-transformed data.
#' @return A list with paths to the directories where results were saved.
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(var1 = rnorm(100), var2 = runif(100, 1, 10))
#' Chekup(data, c("var1", "var2"), "raw_results", "log_results")
#' }
Chekup <- function(data, variaveis, pasta_raw, pasta_log) {

  # Create directories to save histograms and tables
  if (!dir.exists(pasta_raw)) dir.create(pasta_raw)
  if (!dir.exists(pasta_log)) dir.create(pasta_log)

  # Function to save histograms and tables in PDF/CSV format
  salvar_histogramas_e_tabela <- function(variaveis, data, pasta, prefixo) {
    resultados <- data.frame(Variavel = character(0), P_value = numeric(0), Normalidade = character(0))

    # Open PDF for histograms
    pdf(file = paste0(pasta, "/histograms_", prefixo, ".pdf"), width = 8, height = 6)

    for (var in variaveis) {
      # Normality test
      p_value <- shapiro.test(data[[var]])$p.value

      # Normality classification
      normalidade <- ifelse(p_value < 0.05, "Not normal", "Normal")

      # Add results to the table
      resultados <- rbind(resultados, data.frame(Variavel = var, P_value = p_value, Normalidade = normalidade))

      # Add histogram
      hist(data[[var]], main = paste("Histogram of", var, "-", normalidade), xlab = var)
    }

    # Close the PDF
    dev.off()

    # Save the table with results
    write.csv(resultados, file = paste0(pasta, "/table_", prefixo, ".csv"), row.names = FALSE)
  }

  # Normality test and histograms for RAW (original) variables
  salvar_histogramas_e_tabela(variaveis, data, pasta_raw, "raw")

  # Logarithmic transformation for variables (only variables > 0)
  data_log <- data
  for (var in variaveis) {
    data_log[[var]] <- ifelse(data[[var]] > 0, log(data[[var]]), NA)
  }

  # Normality test and histograms for LOG variables
  salvar_histogramas_e_tabela(variaveis, data_log, pasta_log, "log")

  # Return results as a list
  return(list(raw = pasta_raw, log = pasta_log))
}
