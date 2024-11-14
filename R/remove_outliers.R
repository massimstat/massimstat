
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

