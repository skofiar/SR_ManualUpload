#################################################################################
# Functions
#################################################################################
#' This Function contains all the preparation work in order to convert any kind
#' of input template to the desired/needed template format for spire.
#' The following tasks are done here:
#' 1. SPIRE starts with a development period of 0 (for all possible development
#'      period frequencies). Depending on the selected Development Period frequency
#'      we shift the values.
#' 2. Get rid of all 0 amounts in the data table.  This is not a required SPIRE
#'      functionality, but makes data handling like exporting the data table,
#'      uploading the data table to SPIRE faster and easier
#' 3. All Origin Periods that are given as only the year (For example 2023) are
#'      converted to Origin Period + January in numbers (202301)
#' 4. The SPIRE template needs to have a unique key
#'      (Key = Portfolio name + Origin Period + Development Period + Type of Amount).
#'       Using the function "remove_duplicates_by_sum()", we summarize the duplicates
#'       and remove the duplicated rows.
#' 5. If Case Reserves are not given in the data table, then we need to add it
#'      "manually" by calculating:
#'      Case Reserves = Claims Reported excl. ACR - Claims Paid
#'
#' @param datamat Data frame/table that has already the right number and type of
#'                of columns but does not satisfy the/one of the 5 conditions above.
#' @return datamat, in the template format needed by SPIRE
#' @export
template_prep <- function(datamat){
  # Development Period should start with 0 and therefore if there is no 0 element
  ## we need to shift it:
  if ( min(as.numeric(datamat$`Development Period`)) >= 3 &&
       unique(datamat$`Development Period frequency`) == "Quarterly") {
    datamat$`Development Period` <- as.numeric(datamat$`Development Period`) - 3
  }else if ( min(as.numeric(datamat$`Development Period`)) >= 1 &&
             unique(datamat$`Development Period frequency`) == "Annual") {
    datamat$`Development Period` <- as.numeric(datamat$`Development Period`) - 1
  }else if ( min(as.numeric(datamat$`Development Period`)) >= 1 &&
             unique(datamat$`Development Period frequency`) == "Monthly") {
    datamat$`Development Period` <- as.numeric(datamat$`Development Period`) - 1
  }else if ( min(as.numeric(datamat$`Development Period`)) >= 6 &&
             unique(datamat$`Development Period frequency`) == "Half-yearly") {
    datamat$`Development Period` <- as.numeric(datamat$`Development Period`) - 6
  }

  # Get rid of all Amount that are 0 in the data table:
  ## Important that this is done here, as there could be some
  datamat <- datamat[which(datamat$Amount != 0),]

  # If the Origin Period has only 4 digits, we add "01" to it
  ## We check only the first one, as this should be sufficient for the rest
  if (as.numeric(datamat$`Origin Period`[1]) < 100000) {
    datamat$`Origin Period` <- as.numeric(paste0(datamat$`Origin Period`, "01"))
  }

  # Make Uniqueness of the rows save:
  datamat <- remove_duplicates_bysum(df_to_manipulate = datamat)

  ## Do Cumulative Manipulation!
  ## Need first to transform everything to triangles and then the cum2incr and then the creation of the type of amount
  ## Change also the naming above!

  # Calculation of Claims Reported if there is no Claims Reported in Type of Amount:
  # unique_toas <- unique(datamat$`Type of Amount`)
  # if (!("Case Reserves" %in% unique_toas) &&
  #     (("Claims Reported excl. ACR" %in% unique_toas) || ("Claims Reported incl. ACR" %in% unique_toas))) {
  #   # Check whether PAID values are given or not:
  #   if ("Claims Paid" %in% unique_toas) {
  #
  #   }else{
  #
  #   }
  #
  # }

  return(datamat)
}
