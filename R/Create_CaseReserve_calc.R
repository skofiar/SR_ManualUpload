#################################################################################
# Functions
#################################################################################
#' This Function takes two triangles (Reported and Paid) and calculates the Case
#'  Reserve triangle by taking the difference reported and paid triangle.
#'
#' @param ReportedTriangle The reported triangle (as a ChainLaddder package triangle)
#' @param PaidTriangle The paid triangle (as a ChainLaddder package triangle)
#'
#' @return CaseReserve, triangle (ChainLadder package type)
#' @import ChainLadder
#' @import dplyr
#' @import DT
create_CaseReserve <- function(ReportedTriangle, PaidTriangle){
  # Create DF out of these matrices:
  df_ReportedTriangle <- as.data.frame(ReportedTriangle) %>%
    mutate(key = paste0(`Origin Period`, "_", `Development Period`))

  # Load the Paid Triangles and get rid of the NA's by replacing them with 0
  ## This has no influence on the triangle itself or the calcution, we only want
  ## to make sure that if there is no Paid value, we have Case Reserve = Reported
  df_PaidTriangle <- as.data.frame(PaidTriangle) %>%
    mutate(key = paste0(`Origin Period`, "_", `Development Period`)) %>%
    mutate(across(everything(), ~replace(., is.na(.), 0)))

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
