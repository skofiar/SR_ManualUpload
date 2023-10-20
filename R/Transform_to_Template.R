#################################################################################
# Functions - Fileinp/ Read-in Funciton:
#################################################################################
#' Function that loads all the information coming from the Shiny-widget
#' fileInput() into a list and prepares/processes all the respective elements.
#' Using this function, one could also upload multiple files using the named widget
#' and gets a nested list of all the information.
#'
#' @param dattab Data table that is uploaded from the user and needs to be renamed
#' to the corresponding SPIRE template column names. Further the missing columns
#' that are needed are added.
#'
#' @return List with the following information:
#' - df              = Uploaded data frame
#' - extension       = Ending of the data file (e.g. .xlsx)
#' - filepath        = Temporary file path
#' - fliename        = Complete file name
#'
#' @importFrom tools file_ext
#' @importFrom readr read_csv
#' @importFrom readxl excel_sheets read_xls read_xlsx read_excel
#' @export
transform_to_template <- function(dattab){

}
