#################################################################################
# Functions
#################################################################################
#' This Function takes a data table (data) and creates from it for each
#' type of amount and portfolio name triangles, so that one could do triangle
#' manipulations (as differences between triangles or conversion from cumulative
#' to incremental triangles)
#'
#' @param datamat Data frame/table that contains at least following columns:
#'                - Portfolio Name
#'                - Type of Amount
#'                - Development Period
#'                - Origin Period
#'                - Amount
#' @param cumorinc Boolean (T/F), which tells us if the data is given as
#'      cumulative values (cumorinc = T) or as incremental values (cumorinc = F)
#' @return triangle_list, list of all triangles which is structured
#'      by [[portfolio_name]][[type_of_amount]]
#' @import ChainLadder
#' @import dplyr
#' @import DT
#' @export
create_triangle_fromdata <- function(datamat, cumorinc){
  # First convert the data columns "Origin Period", "Development Period", "Amount"
  ## to needed numeric form:
  datamat[, "Amount"] <- round(as.numeric(datamat[, "Amount" ]), digits = 2)
  unique_portfolios <- unique(datamat$`Portfolio Name`)
  unique_types <- unique(datamat$`Type of Amount`)

  # Now create the biggest possible data frame using information comming from datamat
  rowlength <- max(as.numeric(substr(datamat$`Origin Period`,1,4))) - min(as.numeric(substr(datamat$`Origin Period`, 1,4))) + 1

  sorted_dev_per <- sort(unique(as.numeric(datamat$`Development Period`)))
  # We take always the 2nd element, as the first one will be 0 in each case (Annual, Half-yearly, ...)!
  collength <- length(seq(0,sorted_dev_per[length(sorted_dev_per)], sorted_dev_per[2]) )

  #Create the prototype:
  prototype_df <- as.data.frame(matrix(rep(NA, rowlength*collength), ncol = collength, nrow = rowlength))
  colnames(prototype_df) <- seq(0,sorted_dev_per[length(sorted_dev_per)], sorted_dev_per[2])
  rownames(prototype_df) <- seq(min(as.numeric(datamat$`Origin Period`)), max(as.numeric(datamat$`Origin Period`)), 100)


  triangle_list <- lapply(unique_portfolios, function(i) {
    datamat_cur <- datamat[datamat$`Portfolio Name` == i,]
    type_list <- lapply(unique_types, function(j) {
      datamat_type <- datamat_cur[datamat_cur$`Type of Amount` == j,]
      if (cumorinc) {
        triangle_type_list <-
          cum2incr(
            as.triangle(
              as.data.frame(datamat_type),
              origin = "Origin Period", dev = "Development Period", value = "Amount"
            )
          )
      }else{
        triangle_type_list <-
          as.triangle(
            as.data.frame(datamat_type),
            origin = "Origin Period", dev = "Development Period", value = "Amount"
          )
      }
      return(triangle_type_list)
    })
    # Name the nested elements (2nd level) of the list
    names(type_list) <- unique_types
    return(type_list)
  })
  # Name the elements of the list (1st level)
  names(triangle_list) <- unique_portfolios

  return(triangle_list)
}


