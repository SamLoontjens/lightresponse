#' A function that calculates all light response curve parameters of a file.
#'
#' @description
#'   A function that calculates all light response curve parameters of a file.
#'   The input is a pathname.
#'   It slits the path into directories and the filename.
#'   It splits the filename into name parameters.
#'   It makes a title for the plots.
#'   It fits the data using other functions.
#'   It returns a list of the list of name parameters and the list of fitted
#'   parameters.
#' @author Sam Loontjens
#' @param pathname The licor file that will be analysed.
#' @param manual_check A boolean that regulates if the fits are checked.
#' @param name_parameters
#' A character vector of the parameters in the filename.
#' Default is a list of date description, light,
#' relative humidity, CO2, species, measurement and plant.
#' @export
#' @return
#' Returns a list with a list of the name parameters and a list of
#' the fitted light response parameters
#' @examples
#' pathname <- "input_directory_licorfiles/light_response_data/
#'              20210226 LRC NA 75RH 400CO2 T.xlsx"
#' list_of_parameters <- calculate_light_response_parameters(pathname)
#'
calculate_light_response_parameters <- function(pathname,
                                                manual_check = TRUE,
                                                save_plot = FALSE,
                                                save_path = "output_directory_licorfiles/light_response_plots/",
                                                name_parameters = c("date",
                                                                    "description",
                                                                    "light",
                                                                    "relative humidity",
                                                                    "CO2",
                                                                    "species",
                                                                    "measurement",
                                                                    "plant")) {

  #split pathname to find filename
  directory_list <- split_filepath(pathname)
  filename <- directory_list[[length(directory_list)]]

  #make the subtitle the filename without the extension
  subtitle <- strsplit(x = filename, split = "\\.")[[1]][1]

  #read the licorfile with required parameters
  parameters <- c('elapsed','A', 'Ci', 'Qin')
  sheetnumber <- 1
  print(paste("reading:", filename))
  dataframe <- read_licorfile(filepath = pathname,
                              sheetnumber = sheetnumber,
                              parameters = parameters)

  #fit data
  fit_parameters_LRC <- fit_light_response_curve(dataframe = dataframe,
                                                 subtitle = subtitle,
                                                 manual_check = manual_check,
                                                 save_plot = save_plot,
                                                 save_path = save_path)

  #get name parameters from filename
  name_parameters <- split_licor_filename(filename = filename,
                                          name_parameters = name_parameters)
  name_parameters <- name_parameters[names(name_parameters) != "filetype"]

  #add parameter lists together
  induction_parameter_list <- c(name_parameters, fit_parameters_LRC)

  return(induction_parameter_list)
}
