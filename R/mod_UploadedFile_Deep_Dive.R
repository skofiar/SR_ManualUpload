#' UploadedFile_Deep_Dive UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_UploadedFile_Deep_Dive_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' UploadedFile_Deep_Dive Server Functions
#'
#' @noRd 
mod_UploadedFile_Deep_Dive_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_UploadedFile_Deep_Dive_ui("UploadedFile_Deep_Dive_1")
    
## To be copied in the server
# mod_UploadedFile_Deep_Dive_server("UploadedFile_Deep_Dive_1")
