#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @import shinydashboard
#' @import htmltools
#' @import ChainLadder
#' @import DT
#' @importFrom shinyWidgets switchInput
#'
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

    ### Create the UI Part of the shiny:
    shinyUI(
      dashboardPage(skin = "blue",
                    dashboardHeader( title = "Scheduel P - Prep"),
                    dashboardSidebar(
                      sidebarMenu(id = "overall_sidebar_view",
                                  menuItem("Manual Upload Datatable:", tabName = "MU_funcs", startExpanded = T,
                                           menuSubItem("Upload Data", tabName = "MU_data_upload"),
                                           menuSubItem("Data Display", tabName = "MU_data_display")
                                  ),
                                  menuItem("Manual Upload Triangle:", tabName = "MU_triangle_funcs", startExpanded = F,
                                           menuSubItem("Upload Data Triangle", tabName = "MU_data_upload_triangle"),
                                           menuSubItem("Data Display Triangle", tabName = "MU_data_display_triangle")
                                  ),
                                  menuItem("Schedule Data Prep:", tabName = "upload_funcs", startExpanded = F,
                                           menuSubItem("Upload Schedule Data", tabName = "SP_data_upload"),
                                           menuSubItem("Data Display", tabName = "SP_data_display")
                                  )
                      )
                    ),
                    dashboardBody(
                      # Load JS functions for Shiny:
                      shinyjs::useShinyjs(),

                      # Load tabsitems:
                      tabItems(
                        #------------------------------------------------------#
                        #################################
                        ###   Upload Manual Data      ###
                        #################################
                        tabItem(tabName = "MU_data_upload",
                                h1("Upload Data - Data Table Version:"),
                                helpText("Please upload data and provide us with the following
                                         information in order to create the needed SPIRE template:"),
                                box(title = "Upload Guide", solidHeader = TRUE, status = "warning", collapsible = T, width = "100%",
                                    helpText("Define the minimum data type (Premiums are needed for sure, do we need also Paid and Reported?)"),
                                    helpText("Massimo mentioned that he had the case once, where he uploaded halfyearly data and then the
                                                      diagnostics of the the portfolio view was not working (wrong periodicity was used).
                                                      Make sure that this works also here, otherwise flag it!")
                                ),
                                fluidRow(
                                  column(4,
                                         box(title = "Data Upload", solidHeader = TRUE, status = "info", collapsible = T, width = "100%",
                                             helpText("Please upload the file with the data that should be converted to the SPIRE template:"),
                                             fileInput(inputId = "MU_fileupload",
                                                       label = "Upload data:",
                                                       multiple = T, accept = c(".csv",'.xlsx', '.xls', '.xlsm')
                                             ),
                                             uiOutput("MU_sheetname_selectInput")
                                         ),
                                         uiOutput("MU_column_selection")
                                  ),
                                  column(8,
                                         uiOutput("MU_upload_wizard"),

                                  )
                                ),
                                uiOutput("MU_upload_rawtable_view")
                        ),

                        #################################
                        ###   Manual Data Display     ###
                        #################################
                        tabItem(tabName = "MU_data_display",
                                h1("Upload Data - Display of uploaded Data Table:"),
                                helpText("As soon as you have updated the data table and given us the
                                         needed information you'll find the prepared SPIRE template here.
                                         After checking the table, you can export the template."),
                                fluidRow(
                                  col_4(
                                    uiOutput("MU_data_display_exportbox"),
                                    uiOutput("MU_data_display_totalbox")
                                  ),
                                  col_8(
                                    uiOutput("MU_data_display_tablecheck")
                                  )
                                )
                        ),

                        #------------------------------------------------------#
                        #------------------------------------------------------#
                        #################################
                        ###   Upload Manual Data      ###
                        #################################
                        tabItem(tabName = "MU_data_upload_triangle",
                                h1("Upload Data - Triangle Data:"),
                                helpText("Please upload data and provide us with the following
                                         information in order to create the needed SPIRE template:"),
                                fluidRow(
                                  col_4(
                                    box(title = "Data Upload", solidHeader = TRUE, status = "info",
                                        collapsible = T, width = "100%",
                                          helpText("Please upload the file/s which contain the data triangles
                                                   and should be converted to the SPIRE template:"),
                                          fileInput(inputId = "MU_triangle_fileupload",
                                                    label = "Upload data:",
                                                    multiple = T, accept = c(".csv",'.xlsx', '.xls', '.xlsm')
                                          ),
                                          fluidRow(
                                            col_8(
                                              htmltools::h5("Is the data given incremental?",
                                                            style = "font-weight: bold")),
                                            col_4(
                                              switchInput(inputId = "MU_triangle_cumorinc",
                                                          label = " ", value = F, onLabel = "Yes", offLabel = "No"),)
                                          ),
                                          numericInput(inputId = "MU_triangle_numtri", label = "Number of triangles of interest:",
                                                       value = 3, min = 1, max = 10, step = 1),

                                          actionButton(inputId = "MU_triangle_load_button",
                                                       label = "Load Data", width = "100%"),
                                    ),
                                    uiOutput("MU_triangle_informationbox"),
                                  ),
                                  col_8(
                                    uiOutput("MU_triangle_trianglebox")
                                  )
                                )
                        ),

                        #################################
                        ###   Manual Data Display     ###
                        #################################
                        tabItem(tabName = "MU_data_display_triangle",
                                h1("Upload Data - Display of uploaded Triangle:"),
                                helpText("As soon as you have updated the triangle data and given us the
                                         needed information you'll find the prepared SPIRE template here.
                                         After checking the table, you can export the template."),
                                uiOutput("MU_triangle_displaygenerator")

                        ),


                        #------------------------------------------------------#
                        #################################
                        ###   Upload Schedule Data    ###
                        #################################
                        tabItem(tabName = "SP_data_upload",
                                h1("Upload Schedule Data:"),
                                helpText("Please upload Schedule-P data and provide us with the following
                                         information in order to create the needed SPIRE template:"),
                                fluidRow(
                                  column(4,
                                    box(title = "Schedule P - Data Upload", solidHeader = TRUE, status = "info", collapsible = T, width = "100%",
                                        helpText("Please upload the corresponding version of the following data files:"),
                                        # tags$ol(
                                        #   tags$li("Power BI data_IBNR_05 22.xlsx"),
                                        #   tags$li("Power BI data_Paid Loss_05 22.xlsx"),
                                        #   tags$li("Power BI data_Ult Loss_05 22.xlsx"),
                                        # ),
                                        fileInput(inputId = "SP_data_upload_fileupload",
                                                  label = "Upload Schedule P data:",
                                                  multiple = T, accept = c(".csv",'.xlsx', '.xls', '.xlsm')
                                        )
                                    )
                                  ),
                                  column(8,
                                    uiOutput("SP_data_upload_deepdive_tabbox")
                                  )
                                )
                        ),
                        ###################################
                        ###   Schedule Data Display     ###
                        ###################################
                        tabItem(tabName = "SP_data_display",
                                h1("Processed Data - From raw to processed:"),
                                helpText("After uploading the data and all the needed information for the computation,
                                         you will find the raw data and the processed data in this section:"),
                                tabsetPanel(id = "SP_all_data",
                                  tabPanel(
                                    title = "Raw Data",
                                    # uiOutput(outputId = "SP_raw_data_ui"),
                                    br(),
                                    tabBox( width = 12,
                                      tabPanel(title = "Reported",
                                         box(title = "Schedule P - Raw Reported Data", solidHeader = TRUE,
                                            status = "info", collapsible = T, width = "100%",
                                            DT::dataTableOutput("SP_data_display_rawreported")
                                          )
                                      ),
                                      tabPanel(title = "Paid",
                                           box(title = "Schedule P - Raw Paid Data", solidHeader = TRUE,
                                               status = "info", collapsible = T, width = "100%",
                                               DT::dataTableOutput("SP_data_display_rawpaid")
                                           )
                                      ),
                                      tabPanel(title = "IBNR",
                                           box(title = "Schedule P - Raw IBNR Data", solidHeader = TRUE,
                                               status = "info", collapsible = T, width = "100%",
                                               DT::dataTableOutput("SP_data_display_rawibnr")
                                           )
                                      ),
                                      tabPanel(title = "Premium",
                                           box(title = "Schedule P - Raw Premium Data", solidHeader = TRUE,
                                               status = "info", collapsible = T, width = "100%",
                                               DT::dataTableOutput("SP_data_display_rawprem")
                                           )
                                      )
                                    ),

                                  ),
                                  tabPanel(
                                    title = "Triangle Data",
                                    br(),
                                    tabBox( width = 12,
                                      tabPanel(title = "Reported",
                                        uiOutput("SP_data_display_reported_triangle"),
                                      ),
                                      tabPanel(title = "Paid",
                                         uiOutput("SP_data_display_paid_triangle"),
                                      ),
                                      tabPanel(title = "IBNR",
                                         uiOutput("SP_data_display_ibnr_triangle"),
                                      ),
                                      tabPanel(title = "Premium",
                                         uiOutput("SP_data_display_prem_triangle"),
                                      )
                                    ),
                                  ),
                                  tabPanel(
                                    title = "Processed Data",
                                    uiOutput(outputId = "SP_processed_data_ui")
                                  )
                                )
                        )
                      )
                    )
      )
    )


    #### Delete this part later: ####
    # fluidPage(
    #   h1("SPIRE.Sandbox")
    # )
    #################################
  )
}





#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ManualUpload"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
