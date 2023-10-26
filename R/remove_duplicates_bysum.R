#' Reads in a dataframe and checks for duplicates with respective to SPIREs uniqueness key.
#' This uniqueness key consists of the following columns (separated by "_")
#' - Portfolio Name
#' - Origin Period
#' - Development Period
#' - Type of Amount
#' In case some duplicated rows are detected, the function sums up the duplicated values and
#' return a version of the initial dataframe with only one representative of the duplicated rows.
#' This representative contains as amount the sum of the amounts.
#'
#' @param df_to_manipulate Dataframe in which we are looking for the duplicates.
#' @return result_df Dataframe with no duplicated rows.
#' @examples
#' result_df <- remove_duplicates_bysum(df_to_manipulate = result_df)
#' @export
remove_duplicates_bysum <- function(df_to_manipulate){
  # Add a uniqueness variable in order to find any duplicates:
  result_df <- df_to_manipulate %>%
    mutate(uniqueness_key = paste0(`Portfolio Name`,"_",`Origin Period`,"_", `Development Period`, "_", `Type of Amount`))

  # If any of the uniqueness_key elements are duplicated, then...
  if (any(duplicated(result_df$uniqueness_key))) {

    # Summarise the amounts over all duplicates
    result_df_tosummarize <- suppressWarnings({
      result_df %>% group_by(uniqueness_key) %>%
        summarise(sum(as.numeric(Amount))) %>% unique()
    })
    # Take only the unique rows:
    uniquerows_result_df <- result_df[!duplicated(result_df$uniqueness_key),]

    # Match the rows and write them into the case reserve df:
    match_index <- match(uniquerows_result_df$uniqueness_key, result_df_tosummarize$uniqueness_key)
    uniquerows_result_df$Amount <- result_df_tosummarize$`sum(as.numeric(Amount))`[match_index]

    # The result matrix is the unique row matrix without the uniqueness_key
    result_df <- uniquerows_result_df %>% select(-uniqueness_key)

  }

  return(result_df)
}
