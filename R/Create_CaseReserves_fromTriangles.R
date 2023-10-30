#################################################################################
# Functions
#################################################################################
#' This Function takes a nested list of the following structure
#' [[portfolio name]][[type of names]] which should include any kind of reported
#' value and possibly paid values (if paid is missing, we would just take
#' reported == case reserves)
#'
#' @param triangle_list Nested list of the following structure
#' [[portfolio name]][[type of names]] having at least reported triangle values in it.
#' @return triangle_list, list of all triangles which is structured
#'      by [[portfolio_name]][[type_of_amount]]. Additionally, the Case Reserve
#'      values are included
#' @import ChainLadder
#' @export
create_casereserves_fromtriangles <- function(nested_list){
  # Create an empty list to store the results
  # difference_list <- list()

  # Iterate through each Portfolio Name in nested_list
  for (i in names(nested_list)) {

    # Access the nested list for the current Portfolio Name
    portfolio_list <- nested_list[[i]]

    # Access the 'Reported' and 'Paid' triangles
    reported_triangle <- portfolio_list[[which(grepl("Reported", names(portfolio_list)) == T)[1]]]
    paid_triangle <- portfolio_list[[which(grepl("Paid", names(portfolio_list)) == T)[1]]]

    # Check if both triangles exist for the current Portfolio Name
    if(!is.null(reported_triangle)) {
      if (!is.null(paid_triangle)) {
        print(paste0(dim(reported_triangle)," vs. ", dim(paid_triangle)))
        # Ensure the triangles have matching dimensions
        if (all(dim(reported_triangle) == dim(paid_triangle))) {

          # Subtract the 'Paid' triangle from the 'Reported' triangle
          case_reserves <- reported_triangle - paid_triangle

          # Add the "Case Reserves" to the current portfolio list:
          portfolio_list[["Case Reserves"]] <- case_reserves

          # Store the difference triangle in difference_list under the current Portfolio Name
          nested_list[[i]] <- portfolio_list

        } else {
          # This case should never happen as we predefine the size of the
          #     nested element spaces in "CreateTriangles_from_DataTable.R"
          ## --> If you come to this place, then
          warning(paste("Mismatched dimensions for Portfolio Name:", i))
        }
      }else{
        # In this case we set the Case reserves and the Reported Values equal:
        case_reserves <- reported_triangle

        # Add the "Case Reserves" to the current portfolio list:
        portfolio_list[["Case Reserves"]] <- case_reserves

        # Store the difference triangle in difference_list under the current Portfolio Name
        nested_list[[i]] <- portfolio_list
      }


    } else {
      warning(paste("Missing Reported Values for Portfolio Name:", i))
    }


    # Still need to create the additional df which will be added in the end
    ## to the final data frame:
    additional_casereserves_df <- c()
    # --> This needs to be adapted at the end!
    return(list(nested_list, additional_casereserves_df))
  }


}
