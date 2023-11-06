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

  # Generate the list of triangles:
  triangle_list <- lapply(unique_portfolios, function(i) {
    datamat_cur <- datamat[datamat$`Portfolio Name` == i,]
    type_list <- lapply(unique_types, function(j) {
      datamat_type <- datamat_cur[datamat_cur$`Type of Amount` == j,]
      # In case there is no type represented, we skipp the triangle creation
      if (dim(datamat_type)[1] > 0) {
        if (cumorinc && (length(unique(datamat_type$`Development Period`)) > 2)) {
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
      }else{
        return(NULL)
      }
    })
    # Name the nested elements (2nd level) of the list
    names(type_list) <- unique_types
    # Calculate the Case Reserves and add it to the type_list list:
    type_list <-
      append(type_list, list("Case Reserves" =
            create_CaseReserve(ReportedTriangle = type_list$`Claims Reported excl. ACR`,
                               PaidTriangle = type_list$`Claims Paid`)))
    return(type_list)
  })
  # Name the elements of the list (1st level)
  names(triangle_list) <- unique_portfolios

  return(triangle_list)
}


create_CaseReserve <- function(ReportedTriangle, PaidTriangle){
  # Create DF out of these matrices:
  df_ReportedTriangle <- as.data.frame(ReportedTriangle) %>%
    mutate(key = paste0(`Origin Period`, "_", `Development Period`))
  df_PaidTriangle <- as.data.frame(PaidTriangle) %>%
    mutate(key = paste0(`Origin Period`, "_", `Development Period`))

  # Create a total data frame:
  df_CaseReserve <- merge(df_ReportedTriangle, df_PaidTriangle, by = "key") %>%
    mutate(value = value.x - value.y) %>%
    select(`Origin Period.x`, `Development Period.x`, value) %>%
    rename("Development Period" = `Development Period.x`,
           "Origin Period" = `Origin Period.x`)

  CaseReserve <- as.triangle(df_CaseReserve, ,
                             origin = "Origin Period", dev = "Development Period", value = "value")
  return(CaseReserve)
}

