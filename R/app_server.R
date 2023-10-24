

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @import shinydashboard
#' @import htmltools
#' @import ChainLadder
#' @import DT
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook addStyle createStyle
#' @importFrom shinyWidgets switchInput pickerInput
#' @importFrom rlang !! sym
#' @importFrom reshape2 melt
#' @noRd
app_server <- function(input, output, session) {
  # # Defining the options for the dataTableOutput:
  DToptions <<- list(autoWidth = FALSE, scrollX = TRUE,
                    columnDefs = list(list(width = "125px", targets = "_all")),dom = 'tpB',
                    lengthMenu = list(c(5, 10,-1), c('5', '10', 'All')), pageLength = 15)
  DToptions_short <<- list(autoWidth = FALSE, scrollX = TRUE,
                     columnDefs = list(list(width = "125px", targets = "_all")),dom = 'tpB',
                     lengthMenu = list(c(5, 10,-1), c('5', '10', 'All')), pageLength = 10)


  # Defining the components that can be added here:
  ## Reason: Some of them should be textInputs and some have already predefined choices
  process_period <<- shiny::selectInput(inputId = "MU_wizard_process_period", label = "Process Period:",
                                       choices = c("P03 2022", "P06 2022", "P09 2022", "P12 2022",
                                                   "P03 2023", "P06 2023", "P09 2023", "P12 2023",
                                                   "P03 2024", "P06 2024", "P09 2024", "P12 2024",
                                                   "P03 2025", "P06 2025", "P09 2025", "P12 2025"),
                                       selected = "P03 2022")
  type_of_business <<- shiny::textInput(inputId = "MU_wizard_type_of_business",
                                       label = "Type of Business:", value = "All types")
  line_of_business <<- shiny::textInput(inputId = "MU_wizard_line_of_business",
                                       label = "Line of Business:", value = "All types")
  period_type <<- shiny::selectInput(inputId = "MU_wizard_period_type", label = "Period Type:",
                                    choices = c("Underwriting"), selected = "Underwriting")
  process_type <<- shiny::selectInput(inputId = "MU_wizard_process_type", label = "Process Type:",
                                     choices = c("Group Reserving L&H", "Local Stat", "Local Reserving",
                                                 "Group Reserving P&C", "Local Reserving P&C"), selected = "Local Reserving P&C")
  description <<- shiny::textInput(inputId = "MU_wizard_description", label = "Descriptions:", value = "Anything")
  origin_frequency <<- shiny::selectInput(inputId = "MU_wizard_origin_frquency", label = "Origin Frequency:",
                                         choices = c("Monthly", "Quarterly", "Half-yearly", "Annual"),
                                         selected = "Annual")
  legal_entity <<- shiny::selectInput(inputId = "MU_wizard_legal_entity", label = "Legal Entity:",
                                     choices = c("Swiss Re Zurich", "Swiss Re Institute"),
                                     selected = "Swiss Re Zurich")
  currency <<- shiny::selectInput(inputId = "MU_wizard_currency", label = "Currency:",
                                 choices = c("CHF", "EUR", "USD", "JPY"),
                                 selected = "USD")
  development_period_frequency <<- shiny::selectInput(inputId = "MU_wizard_development_period_frequency", label = "Development Period Frequency:",
                                                     choices = c("Monthly", "Quarterly", "Half-yearly", "Annual"),
                                                     selected = "Annual")
  portfolio_name <<- shiny::textInput(inputId = "MU_wizard_portfolio_name", label = "Portfolio Name:",
                                     placeholder = "Please provide portfolio name")


  # Reactive value for data wizard:
  upload_wizard <- reactiveValues()
  upload_wizard_triangle <- reactiveValues()

  #----------------------------------------------------------------------------#
  #----------------------------------------------------------------------------#
  #################################
  ###   Upload Manual Data      ###
  #################################

  # Generate the file upload information:
  observeEvent(input$MU_fileupload, {
    # Create list with all the data:
    Fileinp <- input$MU_fileupload
    upload_wizard$Fileinp <- Fileinp

    upload_wizard$first_raw_data <- fileinp.filereadin(fileinp = Fileinp, shtnms = NULL,
                                     range.selection = NULL, mltple = T)
    upload_wizard$sheetnames <- upload_wizard$first_raw_data[[1]][[5]]

    # Generate a selectInput for the right sheet input:
    output$MU_sheetname_selectInput <- renderUI({
      outputlist <- list()

      # Tab-Selectinput
      outputlist[[1]] <- shiny::selectInput(inputId = "MU_fileupload_sheetnm", label = "In which tab is the data located?",
                                     choices = upload_wizard$sheetnames, selected = upload_wizard$sheetnames[1], multiple = F)
      # Range selection
      outputlist[[2]] <- fluidRow(
        col_8(
          helpText("Do you want to select a specific range?")
        ),
        col_4(
          switchInput(inputId = "MU_fileupload_rangeselection",
                      label = " ", value = F,
                      onLabel = "Yes", offLabel = "No")
        )
      )

      # uiOutput for a potential range selection:
      outputlist[[3]] <- uiOutput("MU_rangeseleciton_ui")

      # File Upload Button
      outputlist[[4]] <- actionButton(inputId = "MU_fileupload_button",
                                      label = "Load File", width = "100%",
                                      style = "color: #FFFFFF; background-color:  #24a0ed; border-color:  #24a0ed")

      return(outputlist)
    })

    # Generate the range selection if the checkbox is selected:
    output$MU_rangeseleciton_ui <- renderUI({
      outputlist <- list()
      outputlist <- NULL

      # If the checkBox is selected, we add the following range elements:
      if (input$MU_fileupload_rangeselection == T) {
        outputlist[[1]] <- helpText("Please make sure to include in the
                                    first row the column names of the data.")
        outputlist[[2]] <-
          fluidRow(
            col_6(
              textInput(inputId = "MU_upload_startingrange", label = "Starting Cell:",
                        placeholder = "Starting cell in excel")
            ),
            col_6(
              textInput(inputId = "MU_upload_endingrange", label = "Ending Cell:",
                        placeholder = "Ending cell in excel")
            )
          )
      }

      return(outputlist)
    })

  })


  # As soon as the "Load" - button is clicked we generate the following:
  observeEvent(input$MU_fileupload_button, {
    # Upload the data of interest:
    ## If there are multiple ranges selected, then we do the following:
    if (input$MU_fileupload_rangeselection) {
      upload_wizard$raw_data <-  fileinp.filereadin(fileinp = upload_wizard$Fileinp, shtnms = input$MU_fileupload_sheetnm,
                                                    range.selection = paste0(input$MU_upload_startingrange, "-", input$MU_upload_endingrange),
                                                    mltple = F)
    }else{
      upload_wizard$raw_data <-  fileinp.filereadin(fileinp = upload_wizard$Fileinp, shtnms = input$MU_fileupload_sheetnm,
                                                    range.selection = NULL, mltple = F)
    }
    # Read out the data frame:
    upload_wizard$data <- as.data.frame(upload_wizard$raw_data[[1]])

    #Generate the column names:
    upload_wizard$colnames <- as.character(upload_wizard$data[1,])
    upload_wizard$spire_columns <- c("Process Period", "Process Type", "Portfolio Name", "Legal Entity",
                                     "Line of Business", "Type of Business", "Description", "Currency",
                                     "Period Type", "Origin frequency", "Development Period frequency",
                                     "Origin Period", "Development Period", "Type of Amount", "Amount")

    #--------------------------------------------------------------------------#
    # COLUMN SELECTION:
    #--------------------------------------------------------------------------#

    output$MU_column_selection <- renderUI({
      outputlist <- list()

      outputlist[[1]] <- box(title = "Select given data columns:", solidHeader = TRUE, status = "info", collapsible = T, width = "100%",
                             helpText("Please select all SPIRE column-names that are represented in your data table.
                                  If you see some columns selected, that are not given in your data table
                                  then you have to deselect them by clicking on them."),
                             shinyWidgets::pickerInput(
                              inputId = "MU_columns_selected", label = "Select represented columns:", choices =  upload_wizard$spire_columns,
                              options = list(`actions-box` = TRUE), multiple = T,
                              selected = c("Line of Business",  "Origin Period", "Development Period", "Type of Amount", "Amount")
                             ),
                             actionButton(inputId = "MU_update_UpWiz",label = "Update Upload Wizard", width = "100%",
                                  style = "color: #FFFFFF; background-color:  #24a0ed; border-color:  #24a0ed")
                          )

      return(outputlist)
    })


    #--------------------------------------------------------------------------#
    # RAW DATA TABLE:
    #--------------------------------------------------------------------------#
    # Show the raw uploaded data:
    output$MU_upload_rawtable_view <- shiny::renderUI({
      outputlist <- list()
      outputlist[[1]] <- box(title = "Raw - Data table:", solidHeader = TRUE,
                             status = "info", collapsible = T, width = "100%",
                             DT::dataTableOutput("MU_upload_rawtable")
      )
      return(outputlist)
    })

    # Show / Load the data table:
    output$MU_upload_rawtable <- DT::renderDataTable({
      return(datatable(upload_wizard$data, options = DToptions, class = 'cell-border stripe',
                       editable = T, rownames = F, filter = "none"))
    })

  })


  shiny::observeEvent(input$MU_update_UpWiz, {
    upload_wizard$given_SPIRE_columns <- input$MU_columns_selected
    upload_wizard$remaining_columns <- upload_wizard$spire_columns[!upload_wizard$spire_columns %in% upload_wizard$given_SPIRE_columns]

    #--------------------------------------------------------------------------#
    # UPLOAD WIZARD:
    #--------------------------------------------------------------------------#
    # Create the upload wizard depending on the selection of column from above
    output$MU_upload_wizard <- shiny::renderUI({
      outputlist <- list()

      # Output-Wizard
      outputlist[[1]] <- box(title = "Upload Wizard - Columnselection:", solidHeader = TRUE, status = "info", collapsible = T, width = "100%",
                             shiny::helpText("Please select the the columns to define the following data types:"),
                             shiny::uiOutput("MU_UpWiz_colsel"),
                             htmltools::hr(),
                             shiny::helpText("Please select for the following "),
                             shiny::uiOutput("MU_UpWiz_allocatedmissingval"),
                             htmltools::hr(),
                             shiny::helpText('As soon as you are sure with the selection above, you can press
                                      the "Generate SPIRE template"-Button to generate the template.'),
                             shiny::actionButton(inputId = "MU_wizard_generate_template",
                                          label = "Generate SPIRE template", width = "100%",
                                          style = "color: #FFFFFF; background-color:  #24a0ed; border-color:  #24a0ed")
      )

      return(outputlist)
    })


    # Generate the selectInputs for the part of the Upload Wizard that should have a column selection:
    output$MU_UpWiz_colsel <- shiny::renderUI({
      outputlist <- list()
      # If we have a even number of elements to show then we do the following:
      if (length(upload_wizard$given_SPIRE_columns) %% 2 == 0) {
        # For each element that was selected in the SPIRE column names we generate an selectInput
        for (i in 1:(length(upload_wizard$given_SPIRE_columns)/2)) {
          outputlist[[i]] <- shiny::fluidRow(
            col_6(shiny::selectInput(inputId = paste0("MU_wizard_",gsub(" ", "_", tolower(upload_wizard$given_SPIRE_columns[2*(i - 1) + 1]))),
                              label = upload_wizard$given_SPIRE_columns[2*(i - 1) + 1],
                              choices = upload_wizard$colnames[grepl(substr(tolower(upload_wizard$given_SPIRE_columns[2*(i - 1) + 1]), 1,3),
                                                                     tolower(upload_wizard$colnames))])
            ),
            col_6(shiny::selectInput(inputId = paste0("MU_wizard_",gsub(" ", "_", tolower(upload_wizard$given_SPIRE_columns[2*(i - 1) + 2]))),
                              label = upload_wizard$given_SPIRE_columns[2*(i - 1) + 2],
                              choices = upload_wizard$colnames[grepl(substr(tolower(upload_wizard$given_SPIRE_columns[2*(i - 1) + 2]), 1,3),
                                                                     tolower(upload_wizard$colnames))])
            )
          )
        }
      }else{
        # Otherwise we do the the same calculation for all the elements except of the last one and add the last one:
        for (i in 1:floor(length(upload_wizard$given_SPIRE_columns)/2)) {
          outputlist[[i]] <- shiny::fluidRow(
            col_6(shiny::selectInput(inputId = paste0("MU_wizard_",gsub(" ", "_", tolower(upload_wizard$given_SPIRE_columns[2*(i - 1) + 1]))),
                              label = upload_wizard$given_SPIRE_columns[2*(i - 1) + 1],
                              choices = upload_wizard$colnames[grepl(substr(tolower(upload_wizard$given_SPIRE_columns[2*(i - 1) + 1]), 1,3),
                                                                     tolower(upload_wizard$colnames))])
            ),
            col_6(shiny::selectInput(inputId = paste0("MU_wizard_",gsub(" ", "_", tolower(upload_wizard$given_SPIRE_columns[2*(i - 1) + 2]))),
                              label = upload_wizard$given_SPIRE_columns[2*(i - 1) + 2],
                              choices = upload_wizard$colnames[grepl(substr(tolower(upload_wizard$given_SPIRE_columns[2*(i - 1) + 2]), 1,3),
                                                                     tolower(upload_wizard$colnames))])
            )
          )
        }
        outputlist[[length(upload_wizard$given_SPIRE_columns)]] <-
          shiny::fluidRow(
            col_6(shiny::selectInput(inputId = paste0("MU_wizard_",gsub(" ", "_", tolower(upload_wizard$given_SPIRE_columns[length(upload_wizard$given_SPIRE_columns)]))),
                              label = upload_wizard$given_SPIRE_columns[length(upload_wizard$given_SPIRE_columns)],
                              choices = upload_wizard$colnames[grepl(substr(tolower(upload_wizard$given_SPIRE_columns[length(upload_wizard$given_SPIRE_columns)]), 1,3),
                                                                     tolower(upload_wizard$colnames))])
            ),
            col_6()
          )

      }


      return(outputlist)
    })

    # Display the remaing places
    output$MU_UpWiz_allocatedmissingval <- shiny::renderUI({

      # Creating the output variable:
      outputlist <- list()

      # Take the case into account, that everything is given in the given case:
      if (length(upload_wizard$remaining_columns) == 0) {
        return(outputlist)
      }

      # The following information need to be part of the given data talbe
      if (any(c("Line of Business",  "Origin Period", "Development Period",
                "Type of Amount", "Amount") %in% upload_wizard$remaining_columns)) {
        outputlist[[1]] <- shiny::helpText('Please make sure that the following column types provided in your data table:',
                                    "\n", "- Amount", "\n", "- Development Period",
                                    "\n", "- Origin Period", "\n", "- Type of Amount",
                                    "\n", "- Portfolio Name", "\n", "- Line of Business")
        return(outputlist)
      }

      # Depending on the given columns in the data table, we need to load the remaining ones:
      if (length(upload_wizard$remaining_columns) %% 2 == 0) {
        for (i in 1:(length(upload_wizard$remaining_columns)/2)) {

          temp.str <- paste0(
            paste0("outputlist[[", i + 1, "]] <- fluidRow(
              col_6(", gsub(" ", "_", tolower(upload_wizard$remaining_columns[2*(i - 1) + 1])) ,"),
              col_6(", gsub(" ", "_", tolower(upload_wizard$remaining_columns[2*(i - 1) + 2])) ,"),
            )")
          )
          # Run the prepared string
          eval(parse(text = temp.str))
        }
      }else{
        for (i in 1:floor(length(upload_wizard$remaining_columns)/2)) {
          temp.str <- paste0(
            paste0("outputlist[[", i + 1, "]] <- fluidRow(
              col_6(", gsub(" ", "_", tolower(upload_wizard$remaining_columns[2*(i - 1) + 1])) ,"),
              col_6(", gsub(" ", "_", tolower(upload_wizard$remaining_columns[2*(i - 1) + 2])) ,"),
            )")
          )
          # Run the prepared string
          eval(parse(text = temp.str))
        }

        temp.str <- paste0(
          paste0("outputlist[[", length(upload_wizard$remaining_columns) + 1, "]] <- fluidRow(
              col_6(", gsub(" ", "_", tolower(upload_wizard$remaining_columns[length(upload_wizard$remaining_columns)])) ,"),
              col_6(),
            )")
        )
        eval(parse(text = temp.str))
      }

      return(outputlist)

    })

  })





  #################################
  ### Display Data & Download   ###
  #################################
  shiny::observeEvent(input$MU_wizard_generate_template, {
    ### Prepare the data table to the desired format:
    # Load data table:
    dattab <- upload_wizard$data
    colnames(dattab) <- dattab[1,]
    dattab <- dattab[-1,]

    # Dummy result matrix:
    dummy_res <- as.data.frame(matrix(rep(NA, 15*nrow(dattab)), ncol = 15, nrow = nrow(dattab)))
    colnames(dummy_res) <- c("Process Period", "Process Type", "Portfolio Name", "Legal Entity", "Line of Business",
                                  "Type of Business", "Description", "Currency", "Period Type",
                                  "Origin frequency", "Development Period frequency",
                                  "Origin Period", "Development Period", "Type of Amount", "Amount")

    # For each given column name, we change
    for (i in upload_wizard$given_SPIRE_columns) {
      dummy_res[,which(colnames(dummy_res) %in% i)] <- dattab[,which(colnames(dattab) %in% input[[paste0("MU_wizard_",gsub(" ", "_", tolower(i)))]])]
    }

    # Adding to the result dataframe also the not in the data table given columns:
    for (j in upload_wizard$remaining_columns) {
      # Only in the case of the Portfolio Name we need to do a concatenation
      # as there could be multiple LoBs in the datatable and therefore we can't name them
      # all the same
      if (j != "Portfolio Name") {
        dummy_res[, j] <- input[[paste0("MU_wizard_",gsub(" ", "_", tolower(j)))]]
      }else{
        if (all(is.na(dummy_res[,"Line of Business"]))) {
          dummy_res[, j] <- paste0(input[[paste0("MU_wizard_",gsub(" ", "_", tolower(j)))]],"_",
                                   input[[paste0("MU_wizard_",gsub(" ", "_", tolower("Line of Business")))]])
        }else{
          dummy_res[, j] <- paste0(input[[paste0("MU_wizard_",gsub(" ", "_", tolower(j)))]],"_",dummy_res[,"Line of Business"])
        }

      }

    }

    # Save the template to the reactive list
    upload_wizard$final_df <- dummy_res

    #Generate the box element, that allows us to download the data table
    output$MU_data_display_exportbox <- shiny::renderUI({
      outputlist <- list()
      outputlist[[1]] <-
        box(title = "Export Template", solidHeader = TRUE, status = "info", collapsible = T, width = "100%",
            shiny::helpText("Please check the data table to the right. As soon as you are happy
                     with the content of the data table, you can export the
                     table to a Excel-File using the button below.",
                     "In a next step you can upload the exported template to SPIRE"),
            shiny::textInput("MU_data_exportname", label = "How should the template be named?",
                             value = "SPIRE_template"),
            shiny::downloadButton('MU_data_display_download', 'Create SPIRE Template', width = "100%",
                           style = "color: #FFFFFF; background-color:  #24a0ed;
                               border-color:  #24a0ed", icon = shiny::icon("plus"))
        )

      return(outputlist)
    })

    # Generate the UI element for the table output:
    output$MU_data_display_tablecheck <- shiny::renderUI({
      outputlist <- list()
      outputlist[[1]] <-
        box(title = "Prepared Template Table", solidHeader = TRUE, status = "info", collapsible = T, width = "100%",
            DT::dataTableOutput("MU_data_dt_show")
        )

      return(outputlist)
    })

    # Forward the user to the next view:
    shinydashboard::updateTabItems(session, "overall_sidebar_view", selected = "MU_data_display")

  })

  output$MU_data_dt_show <- DT::renderDataTable({
    return(datatable(upload_wizard$final_df, options = DToptions_short, class = 'cell-border stripe',
                            editable = T, rownames = F, filter = "none"))

  })

  # Download the template:
  output$MU_data_display_download <- shiny::downloadHandler(
    filename = function(){
      filename <- input$MU_data_exportname
      paste(Sys.Date(),"_",filename , ".xlsx", sep = "")
    },
    content = function(file){
      # Create the working directory:
      wb <- createWorkbook()

      # Create a new Tab in the workbook, with the predefined name:
      addWorksheet(wb,"data")

      # Reads out the triangle data and saves it in a temp. variabel
      writeData(wb = wb, sheet = "data", x = upload_wizard$final_df,
                rowNames = F ,colNames = TRUE)

      # Adding the standard Excel Style to the workbook:
      addingstyletowb(wb = wb, sheetnm = "data", data = upload_wizard$final_df)

      # Save the file
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  #----------------------------------------------------------------------------#
  #----------------------------------------------------------------------------#
  #################################
  ###   Upload Manual Data      ###
  #################################
  # As soon as the data is uploaded, we look for the number of sheets and respective sheetnms:
  shiny::observeEvent(input$MU_triangle_fileupload, {
    # Create list with all the data:
    Fileinp <- input$MU_triangle_fileupload
    upload_wizard_triangle$Fileinp <- Fileinp
    uploadedfiles <<- Fileinp

    upload_wizard_triangle$first_raw_data <- fileinp.filereadin(fileinp = Fileinp, shtnms = NULL,
                                                       range.selection = NULL, mltple = T)
    uploaded <<- upload_wizard_triangle$first_raw_data
    # Extracting the filenames and the respective sheetnames:
    upload_wizard_triangle$filenames <- unlist(lapply(upload_wizard_triangle$first_raw_data, '[[', 4))
    upload_wizard_triangle$sheetnames <- unique(unlist(lapply(upload_wizard_triangle$first_raw_data, '[[', 5)))
  })

  # As soon as the load data button is clicked --> Generate the two boxes:
  shiny::observeEvent(input$MU_triangle_load_button , {
    # Ask all the input variables here:
    output$MU_triangle_informationbox <- renderUI({
      outputlist <- list()
      outputlist[[1]] <-
        box(title = "Triangle Data Information", solidHeader = TRUE, status = "info", collapsible = T, width = "100%",
            helpText("Please provide additional Information about the data:"),
            portfolio_name, process_period, legal_entity, type_of_business,
            currency, origin_frequency, process_type, line_of_business,
            description,period_type, development_period_frequency
        )
      return(outputlist)
    })

    # Create the views for the triangles:
    output$MU_triangle_trianglebox <- renderUI({
      outputlist <- list()
      outputlist[[1]] <-
        box(title = "Triangle Data Information", solidHeader = TRUE, status = "info", collapsible = T, width = "100%",
            helpText("Below you will find the indicated number of triangles of interest."),
            helpText("For each selection, please indicate the type of triangle,
                     in which file and in which tab the triangle is located
                     and in which range."),
            helpText("Please ", strong("make sure"), "that the uploaded data contains the origin period in the first column!"),
            uiOutput("MU_triangle_detailbox")
        )
      return(outputlist)
    })

    # Detail information for the triangle box:
    output$MU_triangle_detailbox <- renderUI({
      # Define possbile choices:
      pos_choices <- c("ACR", "Case Reserves", "Claims Paid", "Claims Reported incl. ACR",
                       "Claims Reported excl. ACR", "Costs Written Accounted",
                       "Costs Written Pipeline", "Cedent IBNR", "IBNR", "IBNR USGAAP",
                       "Premium Written Accounted", "Premium Written Pipeline",
                       "RIOS (Reinstatement Premiums)", "Premium Underwriter",
                       "Premium Written US GAAP", "Technical Result")
      # Generate UI:
      outputlist <- list()
      for (i in 1:input$MU_triangle_numtri) {
        outputlist[[2*(i - 1) + 1]] <- fluidRow(
          col_6(
            htmltools::h5(paste0(i,". Triangle information"),
                          style = "text-decoration: underline; font-weight: bold"),
            selectInput(inputId = paste0("MU_triangle_selecttritype_", i),
                        label = paste0("Please select 'Type of Amount' of the triangle ", i, ":"),
                        choices = pos_choices, selected = pos_choices[i]),
            selectInput(inputId = paste0("MU_triangle_uploaded_tab_",i),
                        label = paste0("Please select the source tab:"),
                        choices = upload_wizard_triangle$sheetnames,
                        selected = upload_wizard_triangle$sheetnames[[1]])
          ),
          col_6(
            checkboxInput(inputId = paste0("MU_triangle_form_",i),
                          label = "Data is given in triangular form?", value = T),
            selectInput(inputId = paste0("MU_triangle_uploaded_file_",i),
                        label = paste0("Please select the source excel file:"),
                        choices = upload_wizard_triangle$filenames,
                        selected = upload_wizard_triangle$filenames[i]),
            fluidRow(
              col_6(
                textInput(inputId = paste0("MU_triangle_startingcell_",i),
                          label = paste0("Starting cell of range ", i), placeholder = "E.g. A1")
              ),
              col_6(
                textInput(inputId = paste0("MU_triangle_endingcell_",i),
                          label = paste0("Ending cell of range ", i), placeholder = "E.g. F10")
              )
            )
          )
        )
        outputlist[[2*(i - 1) + 2]] <- hr()
      }

      outputlist[[2*input$MU_triangle_numtri]] <-
        actionButton(inputId = "MU_triangle_loadtri_toshow",
                     label = "Load Triangles", width = "100%",
                     style = "color: #FFFFFF; background-color:  #24a0ed; border-color:  #24a0ed")
      return(outputlist)
    })


  })

  # As soon as the data is loaded and the needed information is provided we start with the calculation:
  shiny::observeEvent(input$MU_triangle_loadtri_toshow, {

    for (i in 1:input$MU_triangle_numtri) {
      # In case multiple files were uploaded, deduce from which file we want to download the data:
      if (dim(upload_wizard_triangle$Fileinp)[1] > 1) {
        #File input row of interest:
        filupload_number <- which( input[[paste0("MU_triangle_uploaded_file_",i)]] ==
                                     upload_wizard_triangle$Fileinp[,1])
      }

      # Read in the triangles:
      upload_wizard_triangle[[paste0("Triangle_data_",i)]] <- fileinp.filereadin(
                      fileinp = upload_wizard_triangle$Fileinp[filupload_number,],
                      shtnms = input[[paste0("MU_triangle_uploaded_tab_",i)]],
                      range.selection = paste0(input[[paste0("MU_triangle_startingcell_",i)]],":",
                                               input[[paste0("MU_triangle_endingcell_",i)]]),
                      mltple = F)[[1]]

      print(upload_wizard_triangle[[paste0("Triangle_data_",i)]])
      print(input$MU_triangle_cumorinc)

      test_tri_data <<- reshape2::melt(upload_wizard_triangle[[paste0("Triangle_data_",i)]])

      if ((input$MU_triangle_cumorinc == F) && input[[paste0("MU_triangle_form_",i)]]) {
        # Transforming data frame into a long format DF:
        long_tri <- as.data.frame(reshape2::melt(upload_wizard_triangle[[paste0("Triangle_data_",i)]]))
        colnames(long_tri) <- c("origin", "development", "value")
        # Delete all NA columns:
        long_tri <- long_tri[!is.na(long_tri$value),]


        print(long_tri)

        print(as.data.frame(upload_wizard_triangle[[paste0("Triangle_data_",i)]]))
      }

    }
  })


  #----------------------------------------------------------------------------#
  #----------------------------------------------------------------------------#
  # Define reactiveValues for loaded data and manipulated data:
  data_upload <- reactiveValues()

  # Reactive value for the numbers of LoBs:
  numberofLoBs <- reactiveVal("3")

  # In order to create the right numbers of renderDataTable elements:
  renderTable_rv <- reactiveVal(NULL)
  #################################
  ###   Upload Schedule Data    ###
  #################################

  # Read-in uploaded data:
  observeEvent(input$SP_data_upload_fileupload, {
    # Create list with all the data:
    Fileinp <- input$SP_data_upload_fileupload
    data_upload$Fileinp <- Fileinp

    data_list <<- fileinp.filereadin(fileinp = Fileinp, shtnms = NULL,
                                    range.selection = NULL, mltple = T)
    data_upload$raw <- data_list

    output$SP_IBNR_data <- reactive({
      data <- lapply(data_upload$raw, function(x) grepl("ibnr", tolower(x[[4]])))
      return(data)
    })

    # Create the Deep Dive UI:

    output$SP_data_upload_deepdive_tabbox <- renderUI({
      outputlist <- list()
      outputlist[[1]] <-
        box(title = "Schedule P - Data Deep Dive", solidHeader = TRUE, status = "info", collapsible = T, width = "100%",
            tabBox(title = " ", width = "100%", height = "auto",
                   tabPanel(title = "Reported",
                            fluidRow(
                              col_6(
                                uiOutput("SP_data_upload_reported_tab_ui")
                              ),
                              col_6(
                                shiny::selectInput(inputId = "SP_dataupload_reported_numboftri", label = "How many LoBs do you want to consider?",
                                            choices = c("All", 1:20), selected = numberofLoBs()),
                              )
                            ),
                            hr(),
                            h4("Range Selection:", style = "text-decoration: underline"),
                            conditionalPanel(
                              condition = "input.SP_dataupload_reported_numboftri == 'All'",
                              helpText("Please include in case you are selecting 'All' the portfolio name in the first column."),
                              fluidRow(
                                col_6(
                                  textInput(inputId = "SP_dataupload_reported_startingcell",
                                            label = "Please provide the first data cell:",
                                            value = "", placeholder = "E.g. A1"),
                                ),
                                col_6(
                                  textInput(inputId = "SP_dataupload_reported_endingcell",
                                            label = "Please provide the ending data cell:",
                                            value = "", placeholder = "E.g. F75"),
                                )
                              )
                            ),
                            conditionalPanel(
                              condition = "input.SP_dataupload_reported_numboftri != 'All'",
                              uiOutput(outputId = "SP_dataupload_reported_selection")
                            ),




                   ),
                   tabPanel(title =  "Paid",
                            fluidRow(
                              col_6(
                                uiOutput("SP_data_upload_paid_tab_ui")
                              ),
                              col_6(
                                shiny::selectInput(inputId = "SP_dataupload_paid_numboftri", label = "How many LoBs do you want to consider?",
                                            choices = c("All", 1:20),
                                            selected = numberofLoBs()),
                              )
                            ),
                            hr(),
                            h4("Range Selection:", style = "text-decoration: underline"),
                            conditionalPanel(
                              condition = "input.SP_dataupload_paid_numboftri == 'All'",
                              helpText("Please include in case you are selecting 'All' the portfolio name in the first column."),
                              fluidRow(
                                col_6(
                                  textInput(inputId = "SP_dataupload_paid_startingcell",
                                            label = "Please provide the first data cell:",
                                            value = "", placeholder = "E.g. A1"),
                                ),
                                col_6(
                                  textInput(inputId = "SP_dataupload_paid_endingcell",
                                            label = "Please provide the ending data cell:",
                                            value = "", placeholder = "E.g. F75"),
                                )
                              )
                            ),
                            conditionalPanel(
                              condition = "input.SP_dataupload_paid_numboftri != 'All'",
                              uiOutput(outputId = "SP_dataupload_paid_selection")
                            ),



                   ),
                   tabPanel(title =  "IBNR",
                            fluidRow(
                              col_6(
                                uiOutput("SP_data_upload_IBNR_tab_ui")
                              ),
                              col_6(
                                shiny::selectInput(inputId = "SP_dataupload_IBNR_numboftri", label = "How many LoBs do you want to consider?",
                                            choices = c("All", 1:20),
                                            selected = numberofLoBs()),
                              )
                            ),
                            hr(),
                            h4("Range Selection:", style = "text-decoration: underline"),
                            conditionalPanel(
                              condition = "input.SP_dataupload_IBNR_numboftri == 'All'",
                              helpText("Please include in case you are selecting 'All' the portfolio name in the first column."),
                              fluidRow(
                                col_6(
                                  textInput(inputId = "SP_dataupload_IBNR_startingcell",
                                            label = "Please provide the first data cell:",
                                            value = "", placeholder = "E.g. A1"),
                                ),
                                col_6(
                                  textInput(inputId = "SP_dataupload_IBNR_endingcell",
                                            label = "Please provide the ending data cell:",
                                            value = "", placeholder = "E.g. F75"),
                                )
                              )
                            ),
                            conditionalPanel(
                              condition = "input.SP_dataupload_IBNR_numboftri != 'All'",
                              uiOutput(outputId = "SP_dataupload_IBNR_selection")
                            ),

                   ),
                   tabPanel(title = "Premium",
                            fluidRow(
                              col_6(
                                uiOutput("SP_data_upload_prem_tab_ui")
                              ),
                              col_6(
                                shiny::selectInput(inputId = "SP_dataupload_prem_numboftri", label = "How many LoBs do you want to consider?",
                                            choices = c("All", 1:20),
                                            selected = numberofLoBs()),
                              )
                            ),
                            hr(),
                            h4("Range Selection:", style = "text-decoration: underline"),
                            conditionalPanel(
                              condition = "input.SP_dataupload_prem_numboftri == 'All'",
                              helpText("Please include in case you are selecting 'All' the portfolio name in the first column."),
                              fluidRow(
                                col_6(
                                  textInput(inputId = "SP_dataupload_prem_startingcell",
                                            label = "Please provide the first data cell:",
                                            value = "", placeholder = "E.g. A1"),
                                ),
                                col_6(
                                  textInput(inputId = "SP_dataupload_prem_endingcell",
                                            label = "Please provide the ending data cell:",
                                            value = "", placeholder = "E.g. F75"),
                                )
                              )
                            ),
                            conditionalPanel(
                              condition = "input.SP_dataupload_prem_numboftri != 'All'",
                              uiOutput(outputId = "SP_dataupload_prem_selection")
                            ),


                   ),
            ),
            helpText("Please click on 'Load data'", strong(" only after "), "filling out all the information above!"),
            fluidRow(
              col_6(
                shiny::selectInput(inputId = "SP_dataupload_processpreiod", label = "Please provide the process period:",
                            choices = c("P03 2022", "P06 2022", "P09 2022", "P12 2022", "P03 2023", "P06 2023", "P09 2023", "P12 2023",
                                        "P03 2024", "P06 2024", "P09 2024", "P12 2024"), selected = "P03 2022")
              ),
              col_6(
                actionButton(inputId = "SP_dataupload_loaddata",
                             label = "Load data", width = "100%",
                             style = "color: #FFFFFF; background-color:  #24a0ed; border-color:  #24a0ed"),
              ),
            )


        )

      return(outputlist)

    })

    # If the first selectInput (the number of LoBs) is changed, then the others should have the same value:
    shiny::observeEvent(input$SP_dataupload_reported_numboftri, {
      # Running the reactive value in order to not only toggle the value:
      numberofLoBs(input$SP_dataupload_reported_numboftri)
      # Updating all the other values:
      shinydashboard::updateSelectInput(session, "SP_dataupload_paid_numboftri", selected = input$SP_dataupload_reported_numboftri)
      shinydashboard::updateSelectInput(session, "SP_dataupload_IBNR_numboftri", selected = input$SP_dataupload_reported_numboftri)
      shinydashboard::updateSelectInput(session, "input.SP_dataupload_prem_numboftri", selected = input$SP_dataupload_reported_numboftri)
    })

    # If the Portfolio name is changed, then we change automatically the name of the others:
    output$SP_dataupload_IBNR_selection <- renderUI({
      # Create the outputlist
      outputlist <- list()

      # Extract the number of selection we want to do:
      nmbr.extraction <- as.numeric(input$SP_dataupload_IBNR_numboftri)

      # Generate the ranges:
      for (i in 1:nmbr.extraction) {
        outputlist[[(i - 1)*2 + 1]] <- textInput(inputId = paste0("SP_dataupload_IBNR_portfolioname_",i),
                                               label = paste0("Portfolio name of ", i, ". range selection:"),
                                               placeholder = "Provide the portfolio name:")
        outputlist[[(i - 1)*2 + 2]] <-
          fluidRow(
            col_6(
              textInput(inputId = paste0("SP_dataupload_IBNR_startingcell_",i),
                        label = paste0("Please provide the first data cell of the ", i, ". range:"),
                        value = "", placeholder = "E.g. A1"),
            ),
            col_6(
              textInput(inputId = paste0("SP_dataupload_IBNR_endingcell_",i),
                        label = paste0("Please provide the ending data cell of the ",i,". range:"),
                        value = "", placeholder = "E.g. F75"),
            )
          )
      }

      return(outputlist)

    })


    output$SP_dataupload_reported_selection <- renderUI({
      # Create the outputlist
      outputlist <- list()

      # Extract the number of selection we want to do:
      nmbr.extraction <- as.numeric(input$SP_dataupload_reported_numboftri)

      # Generate the ranges:
      for (i in 1:nmbr.extraction) {
        outputlist[[(i - 1)*2 + 1]] <- textInput(inputId = paste0("SP_dataupload_reported_portfolioname_",i),
                                                 label = paste0("Portfolio name of ", i, ". range selection:"),
                                                 placeholder = "Provide the portfolio name:")
        outputlist[[(i - 1)*2 + 2]] <-
          fluidRow(
            col_6(
              textInput(inputId = paste0("SP_dataupload_reported_startingcell_",i),
                        label = paste0("Please provide the first data cell of the ", i, ". range:"),
                        value = "", placeholder = "E.g. A1"),
            ),
            col_6(
              textInput(inputId = paste0("SP_dataupload_reported_endingcell_",i),
                        label = paste0("Please provide the ending data cell of the ",i,". range:"),
                        value = "", placeholder = "E.g. F75"),
            )
          )
      }

      return(outputlist)

    })

    output$SP_dataupload_paid_selection <- renderUI({
      # Create the outputlist
      outputlist <- list()

      # Extract the number of selection we want to do:
      nmbr.extraction <- as.numeric(input$SP_dataupload_paid_numboftri)

      # Generate the ranges:
      for (i in 1:nmbr.extraction) {
        outputlist[[(i - 1)*2 + 1]] <- textInput(inputId = paste0("SP_dataupload_paid_portfolioname_",i),
                                                 label = paste0("Portfolio name of ", i, ". range selection:"),
                                                 placeholder = "Provide the portfolio name:")
        outputlist[[(i - 1)*2 + 2]] <-
          fluidRow(
            col_6(
              textInput(inputId = paste0("SP_dataupload_paid_startingcell_",i),
                        label = paste0("Please provide the first data cell of the ", i, ". range:"),
                        value = "", placeholder = "E.g. A1"),
            ),
            col_6(
              textInput(inputId = paste0("SP_dataupload_paid_endingcell_",i),
                        label = paste0("Please provide the ending data cell of the ",i,". range:"),
                        value = "", placeholder = "E.g. F75"),
            )
          )
      }

      return(outputlist)

    })

    output$SP_dataupload_prem_selection <- renderUI({
      # Create the outputlist
      outputlist <- list()

      # Extract the number of selection we want to do:
      nmbr.extraction <- as.numeric(input$SP_dataupload_prem_numboftri)

      # Generate the ranges:
      for (i in 1:nmbr.extraction) {
        #ADD HERE ALSO THE FACT THAT THE NAME HAS TO BE GIVEN!
        outputlist[[(i - 1)*2 + 1]] <- textInput(inputId = paste0("SP_dataupload_prem_portfolioname_",i),
                                                 label = paste0("Portfolio name of ", i, ". range selection:"),
                                                 placeholder = "Provide the portfolio name:")
        outputlist[[(i - 1)*2 + 2]] <-
          fluidRow(
            col_6(
              textInput(inputId = paste0("SP_dataupload_prem_startingcell_",i),
                        label = paste0("Please provide the first data cell of the ", i, ". range:"),
                        value = "", placeholder = "E.g. A1"),
            ),
            col_6(
              textInput(inputId = paste0("SP_dataupload_prem_endingcell_",i),
                        label = paste0("Please provide the ending data cell of the ",i,". range:"),
                        value = "", placeholder = "E.g. F75"),
            )
          )
      }

      return(outputlist)

    })


  })

  # Tab selection for all Deep Dive views:
  output$SP_data_upload_IBNR_tab_ui <- renderUI({
    # Extract the data from the IBNR data file:
    data <- Filter(function(x) grepl("ibnr", tolower(x[[4]])), data_upload$raw)

    # Generate the selectionInput for the tabs
    outputlist <- list()
    outputlist[[1]] <- shiny::selectInput(inputId = "SP_dataupload_tab_ibnr", label = "Please select the tab that contains the data:",
                                   choices = data[[1]][[5]], selected = data[[1]][[5]][1])
    return(outputlist)
  })

  # Tab selection for all Deep Dive views:
  output$SP_data_upload_reported_tab_ui <- renderUI({
    # Extract the data from the IBNR data file:
    data <- Filter(function(x) grepl("ult", tolower(x[[4]])), data_upload$raw)

    # Generate the selectionInput for the tabs
    outputlist <- list()
    outputlist[[1]] <- shiny::selectInput(inputId = "SP_dataupload_tab_reported", label = "Please select the tab that contains the data:",
                                   choices = data[[1]][[5]], selected = data[[1]][[5]][1])
    return(outputlist)
  })

  # Tab selection for all Deep Dive views:
  output$SP_data_upload_paid_tab_ui <- renderUI({
    # Extract the data from the IBNR data file:
    data <- Filter(function(x) grepl("paid", tolower(x[[4]])), data_upload$raw)

    # Generate the selectionInput for the tabs
    outputlist <- list()
    outputlist[[1]] <- shiny::selectInput(inputId = "SP_dataupload_tab_paid", label = "Please select the tab that contains the data:",
                                   choices = data[[1]][[5]], selected = data[[1]][[5]][1])
    return(outputlist)
  })

  # Tab selection for all Deep Dive views:
  output$SP_data_upload_prem_tab_ui <- renderUI({
    # Extract the data from the IBNR data file:
    data <- Filter(function(x) grepl("prem", tolower(x[[4]])), data_upload$raw)

    # Generate the selectionInput for the tabs
    outputlist <- list()
    outputlist[[1]] <- shiny::selectInput(inputId = "SP_dataupload_tab_prem", label = "Please select the tab that contains the data:",
                                   choices = data[[1]][[5]], selected = data[[1]][[5]][1])
    return(outputlist)
  })

################################################################################
  # Change the Tab-view as soon as the button is clicked:
  observeEvent(input$SP_dataupload_loaddata, {
    ### Read-in the file-information
    ######  Reported:   ######
    # Loading the raw data:
    data_upload$raw_reported <-
      fileinp.filereadin(fileinp = input$SP_data_upload_fileupload[which(grepl("ult los", tolower(input$SP_data_upload_fileupload[,1]))),],
                         shtnms = input$SP_dataupload_tab_reported,
                         range.selection = NULL, mltple = F)[[1]]

    # Loading the ranges:
    if (input$SP_dataupload_reported_numboftri == "All") {
      reported_number_of_ranges <- 1
    }else{
      reported_number_of_ranges <- as.numeric(input$SP_dataupload_reported_numboftri)
    }

    data_upload$reported <- list()
    # Number of ranges:
    data_upload$reported$number_of_ranges <- reported_number_of_ranges

    # List to save the result:
    for (i in 1:reported_number_of_ranges) {
      # Read in the triangle:
      data_upload$reported[[input[[paste0("SP_dataupload_reported_portfolioname_",i)]]]] <-
        fileinp.filereadin(fileinp = input$SP_data_upload_fileupload[which(grepl("ult los", tolower(input$SP_data_upload_fileupload[,1]))),],
                           shtnms = input$SP_dataupload_tab_reported,
                           range.selection = paste0(input[[paste0("SP_dataupload_reported_startingcell_",i)]],":",
                                                    input[[paste0("SP_dataupload_reported_endingcell_",i)]]), mltple = F)[[1]]
    }

    ######  Paid:   ######
    # Loading the raw data:
    data_upload$raw_paid <-
      fileinp.filereadin(fileinp = input$SP_data_upload_fileupload[which(grepl("paid", tolower(input$SP_data_upload_fileupload[,1]))),],
                         shtnms = input$SP_dataupload_tab_paid,
                         range.selection = NULL, mltple = F)[[1]]

    # Loading the ranges:
    if (input$SP_dataupload_paid_numboftri == "All") {
      paid_number_of_ranges <- 1
    }else{
      paid_number_of_ranges <- as.numeric(input$SP_dataupload_paid_numboftri)
    }

    data_upload$paid <- list()
    # Number of ranges:
    data_upload$paid$number_of_ranges <- paid_number_of_ranges

    # List to save the result:
    for (i in 1:paid_number_of_ranges) {
      # Read in the triangle:
      data_upload$paid[[input[[paste0("SP_dataupload_paid_portfolioname_",i)]]]] <-
        fileinp.filereadin(fileinp = input$SP_data_upload_fileupload[which(grepl("paid", tolower(input$SP_data_upload_fileupload[,1]))),],
                           shtnms = input$SP_dataupload_tab_paid,
                           range.selection = paste0(input[[paste0("SP_dataupload_paid_startingcell_",i)]],":",
                                                    input[[paste0("SP_dataupload_paid_endingcell_",i)]]), mltple = F)[[1]]
    }

    ###### IBNR:  ######
    # Loading the raw data:
    data_upload$raw_ibnr <-
      fileinp.filereadin(fileinp = input$SP_data_upload_fileupload[which(grepl("ibnr", tolower(input$SP_data_upload_fileupload[,1]))),],
                         shtnms = input$SP_dataupload_tab_ibnr,
                         range.selection = NULL, mltple = F)[[1]]

    # Loading the ranges:
    if (input$SP_dataupload_IBNR_numboftri == "All") {
      ibnr_number_of_ranges <- 1
    }else{
      ibnr_number_of_ranges <- as.numeric(input$SP_dataupload_IBNR_numboftri)
    }

    data_upload$ibnr <- list()
    # Number of ranges:
    data_upload$ibnr$number_of_ranges <- ibnr_number_of_ranges

    # List to save the result:
    for (i in 1:ibnr_number_of_ranges) {
      # Read in the triangle:
      data_upload$ibnr[[input[[paste0("SP_dataupload_IBNR_portfolioname_",i)]]]] <-
        fileinp.filereadin(fileinp = input$SP_data_upload_fileupload[which(grepl("ibnr", tolower(input$SP_data_upload_fileupload[,1]))),],
                           shtnms = input$SP_dataupload_tab_ibnr,
                           range.selection = paste0(input[[paste0("SP_dataupload_IBNR_startingcell_",i)]],":",
                                                    input[[paste0("SP_dataupload_IBNR_endingcell_",i)]]), mltple = F)[[1]]
    }

    ###### Premium: ######
    # Loading the raw data:
    data_upload$raw_prem <-
      fileinp.filereadin(fileinp = input$SP_data_upload_fileupload[which(grepl("prem", tolower(input$SP_data_upload_fileupload[,1]))),],
                         shtnms = input$SP_dataupload_tab_prem,
                         range.selection = NULL, mltple = F)[[1]]

    # Load the respective ranges:
    if (input$SP_dataupload_prem_numboftri == "All") {
      prem_number_of_ranges <- 1
    }else{
      prem_number_of_ranges <- as.numeric(input$SP_dataupload_prem_numboftri)
    }

    data_upload$prem <- list()
    # Number of ranges:
    data_upload$prem$number_of_ranges <- ibnr_number_of_ranges

    # List to save the result:
    for (i in 1:prem_number_of_ranges) {
      # Read in the triangle:
      data_upload$prem[[input[[paste0("SP_dataupload_prem_portfolioname_",i)]]]] <-
        fileinp.filereadin(fileinp = input$SP_data_upload_fileupload[which(grepl("prem", tolower(input$SP_data_upload_fileupload[,1]))),],
                           shtnms = input$SP_dataupload_tab_prem,
                           range.selection = paste0(input[[paste0("SP_dataupload_prem_startingcell_",i)]],":",
                                                    input[[paste0("SP_dataupload_prem_endingcell_",i)]]), mltple = F)[[1]]
    }

    # Forward the user to the next view:
    updateTabItems(session, "overall_sidebar_view", selected = "SP_data_display")
    # Update the reactiveValue in order to have trigger the generator of renderDataTables:
    renderTable_rv("SP_data_display")
  })

  observeEvent(renderTable_rv(), {

    ####      REPORTED:   ####
    ## Raw
    output$SP_data_display_rawreported <- DT::renderDataTable({
      #Output the datatable
      return(datatable(data_upload$raw_reported, options = DToptions,
                       class = 'cell-border stripe', editable = T, rownames = F, filter = "none"))

    })

    ## Triangles:
    reported_number_of_ranges <- data_upload$reported$number_of_ranges

    # Creating the data triangle output for the triangles uploaded:
    output$SP_data_display_reported_triangle <- renderUI({
      outputlist <- list()

      # Create elements to show the triangle data:
      for (i in 1:reported_number_of_ranges) {
        outputlist[[i]] <-
          box(title = input[[paste0("SP_dataupload_reported_portfolioname_",i)]], solidHeader = TRUE, status = "info", collapsible = T, width = "100%",
              DT::dataTableOutput(paste0("SP_dataupload_datatable_reported_",input[[paste0("SP_dataupload_reported_portfolioname_",i)]]))
          )
      }
      return(outputlist)
    })

    for (i in 1:reported_number_of_ranges) {
      output[[paste0("SP_dataupload_datatable_reported_",input[[paste0("SP_dataupload_reported_portfolioname_",i)]])]] <-
        DT::renderDataTable({
          return(datatable(data = as.data.frame(data_upload$reported[[input[[paste0("SP_dataupload_reported_portfolioname_",i)]]]]),
                                         options = DToptions, class = 'cell-border stripe',
                                         editable = T, rownames = F, filter = "none"))
        })

    }

    ####      PAID:   ####
    ## Raw
    output$SP_data_display_rawpaid <- DT::renderDataTable({
      #Output the datatable
      return(datatable(data_upload$raw_paid, options = DToptions,
                       class = 'cell-border stripe', editable = T, rownames = F, filter = "none"))

    })

    ## Triangles:
    paid_number_of_ranges <- data_upload$paid$number_of_ranges

    # Creating the data triangle output for the triangles uploaded:
    output$SP_data_display_paid_triangle <- renderUI({
      outputlist <- list()

      # Create elements to show the triangle data:
      for (i in 1:paid_number_of_ranges) {
        outputlist[[i]] <-
          box(title = input[[paste0("SP_dataupload_paid_portfolioname_",i)]], solidHeader = TRUE, status = "info", collapsible = T, width = "100%",
              DT::dataTableOutput(paste0("SP_dataupload_datatable_paid_",input[[paste0("SP_dataupload_paid_portfolioname_",i)]]))
          )
      }
      return(outputlist)
    })

    for (i in 1:paid_number_of_ranges) {
      output[[paste0("SP_dataupload_datatable_paid_",input[[paste0("SP_dataupload_paid_portfolioname_",i)]])]] <-
        DT::renderDataTable({
          return(datatable(data = as.data.frame(data_upload$paid[[input[[paste0("SP_dataupload_paid_portfolioname_",i)]]]]),
                           options = DToptions, class = 'cell-border stripe',
                           editable = T, rownames = F, filter = "none"))
        })

    }

    ####      IBNR:   ####
    ## Raw
    output$SP_data_display_rawibnr <- DT::renderDataTable({
      #Output the datatable
      return(datatable(data_upload$raw_ibnr, options = DToptions,
                       class = 'cell-border stripe', editable = T, rownames = F, filter = "none"))

    })

    ## Triangles:
    ibnr_number_of_ranges <- data_upload$ibnr$number_of_ranges

    # Creating the data triangle output for the triangles uploaded:
    output$SP_data_display_ibnr_triangle <- renderUI({
      outputlist <- list()

      # Create elements to show the triangle data:
      for (i in 1:ibnr_number_of_ranges) {
        outputlist[[i]] <-
          box(title = input[[paste0("SP_dataupload_IBNR_portfolioname_",i)]], solidHeader = TRUE, status = "info", collapsible = T, width = "100%",
              DT::dataTableOutput(paste0("SP_dataupload_datatable_ibnr_",input[[paste0("SP_dataupload_IBNR_portfolioname_",i)]]))
          )
      }
      return(outputlist)
    })

    for (i in 1:ibnr_number_of_ranges) {
      output[[paste0("SP_dataupload_datatable_ibnr_",input[[paste0("SP_dataupload_IBNR_portfolioname_",i)]])]] <-
        DT::renderDataTable({
          return(datatable(data = as.data.frame(data_upload$ibnr[[input[[paste0("SP_dataupload_IBNR_portfolioname_",i)]]]]),
                           options = DToptions, class = 'cell-border stripe',
                           editable = T, rownames = F, filter = "none"))
        })

    }

    ####      PREMIUM:   ####
    ## Raw
    output$SP_data_display_rawprem <- DT::renderDataTable({
      #Output the datatable
      return(datatable(data_upload$raw_prem, options = DToptions,
                       class = 'cell-border stripe', editable = T, rownames = F, filter = "none"))

    })

    ## Triangles:
    prem_number_of_ranges <- data_upload$prem$number_of_ranges

    # Creating the data triangle output for the triangles uploaded:
    output$SP_data_display_prem_triangle <- renderUI({
      outputlist <- list()

      # Create elements to show the triangle data:
      for (i in 1:prem_number_of_ranges) {
        outputlist[[i]] <-
          box(title = input[[paste0("SP_dataupload_prem_portfolioname_",i)]], solidHeader = TRUE, status = "info", collapsible = T, width = "100%",
              DT::dataTableOutput(paste0("SP_dataupload_datatable_prem_",input[[paste0("SP_dataupload_prem_portfolioname_",i)]]))
          )
      }
      return(outputlist)
    })

    for (i in 1:prem_number_of_ranges) {
      output[[paste0("SP_dataupload_datatable_prem_",input[[paste0("SP_dataupload_prem_portfolioname_",i)]])]] <-
        DT::renderDataTable({
          return(datatable(data = as.data.frame(data_upload$prem[[input[[paste0("SP_dataupload_prem_portfolioname_",i)]]]]),
                           options = DToptions, class = 'cell-border stripe',
                           editable = T, rownames = F, filter = "none"))
        })

    }


  }, ignoreNULL = T)


  ###################################
  ###   Schedule Data Display     ###
  ###################################

}
