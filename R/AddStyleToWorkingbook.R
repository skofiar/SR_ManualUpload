#################################################################################
# Functions - Fileinp/ Read-in Funciton:
#################################################################################
#' Function that adds a row-alternating Excel-style to the Excel.
#' This should help the user to better understand/be able to read the rows.
#' We do this, by defining the Excel styles in the function itself and then write
#' to the corresponding places the style!
#'
#' @param wb Current WorkingBook.
#' @param sheetnm Excel-Sheet name, in which the data should be written in and
#' the style should be applied to
#' @param data Data table to which the style should be applied to. We are not using
#' the data table content itself but more the characteristics of it like
#' the nrow and ncol.
#'
#' @return Nothing is going to be returned.
#' Only the style is added in this function
#'
#' @importFrom openxlsx addStyle createStyle
#' @export
addingstyletowb <- function(wb, sheetnm, data){

  # Excel styles:
  grybckgrndclr <- openxlsx::createStyle(fgFill = "gray93")
  insideBorders <- openxlsx::createStyle( border = c("top", "bottom"), borderStyle = "thin", fgFill = "white")
  header_st <- openxlsx::createStyle(textDecoration = "Bold", fgFill = "yellow", border = c("top", "left", "bottom", "right"))

  # addStyles to the corresponding places
  openxlsx::addStyle(wb,sheetnm, style = header_st, rows = 1, cols = 1:ncol(data),gridExpand = T)
  openxlsx::addStyle(wb,sheetnm, style = insideBorders, rows = 1:nrow(data) + 1, cols = 1:ncol(data),gridExpand = T)
  openxlsx::addStyle(wb,sheetnm, style = grybckgrndclr, rows = seq(2,nrow(data) + 1,2), cols = 1:ncol(data),gridExpand = T)
}
