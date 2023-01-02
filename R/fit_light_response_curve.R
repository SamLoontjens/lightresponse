#' A function that fits a light response curve.
#'
#' @description
#'   A function that fits a light response curve.
#'   The input is a dataframe.
#'   It extracts the columns A, elapsed and Qin.
#'   (These have to be columns in the dataframe or it will result in an error)
#'   It sets the initial parameters Rd, alpha, Pmax, curvature and the
#'   light_compensation_point.
#'   If manual check is on it will plot the data.
#'   It will try to run the model.
#'   The formula is: -Rd + (alpha * PPF + Pmax - sqrt((alpha * PPF + Pmax)^2
#'   - 4 * curvature * PPF * Pmax * alpha))/(2 * curvature).
#'   If manual check is on it will ask the user if the fit is acceptable.
#'   If there is an error or warning it will return a list of NA values.
#'   If the fit is accepted it will return a list with the fitted parameters.
#' @author Sam Loontjens
#' @param dataframe The dataframe that will be analysed.
#' @param title A string for the title of the plot.
#' @param subtitle A string of the subtitle to use. Filename recommended.
#' @param manual_check A boolean that regulates if the fits are checked.
#' @param save_plot A boolean that regulates if the plots get saved.
#' @param save_path A string of the pathname where the plots get saved.
#' @export
#' @return Returns a list of the fitted light response parameters
#' @examples
#' fitted_parameters <- fit_light_response_curve(mydata)
#'
fit_light_response_curve <- function(dataframe,
                                     title = "Light response curve",
                                     subtitle = "",
                                     manual_check = TRUE,
                                     save_plot = FALSE,
                                     save_path = "output_directory_licorfiles/light_response_plots/") {

  #get data
  PPF <- dataframe$Qin
  Pn <- dataframe$A

  #select start parameters
  Rd = 0.66
  alpha = 0.076
  Pmax = max(Pn)
  curvature = 0.67
  start_parameter_list <- list(Rd = Rd, alpha = alpha, Pmax = Pmax, curvature = curvature)
  lower_bounds <- c(-20, 0.01, 1, 0.001)
  upper_bounds <- c(20, 10, 100, 0.999)

  #make formula for fitting
  LRCformula <- as.formula(Pn ~ -Rd + (alpha * PPF + Pmax - sqrt((alpha * PPF + Pmax)^2 - 4 * curvature * PPF * Pmax * alpha))/(2 * curvature))

  #fit the parameters
  fit_parameters <- fit_any_curve(x = PPF,
                                  y = Pn,
                                  formula = LRCformula,
                                  variable_name = "PPF",
                                  list_of_start_parameters = start_parameter_list,
                                  title = title,
                                  subtitle = subtitle,
                                  manual_check = manual_check,
                                  save_plot = save_plot,
                                  save_path = save_path,
                                  lower_bounds = lower_bounds,
                                  upper_bounds = upper_bounds)

  #get  the parameters from the fit to calculate the light_compensation point
  fit_state = fit_parameters[[1]]
  Rd = fit_parameters[[5]]
  alpha = fit_parameters[[6]]
  Pmax = fit_parameters[[7]]
  curvature = fit_parameters[[8]]
  if (fit_state == "accepted" | fit_state == "not checked") {
    light_compensation_point <- (curvature * Rd^2 - Rd * Pmax) / (Rd * alpha - alpha * Pmax)
  } else {
    light_compensation_point <- NA
  }

  #add the parameters to a list
  fit_parameters <- c(fit_parameters, list(lcp = light_compensation_point))

  return(fit_parameters)
}
