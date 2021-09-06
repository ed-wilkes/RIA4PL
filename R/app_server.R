#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import ggplot2
#' @importFrom magrittr "%>%"
#' @import dr4pl
#' @noRd
app_server <- function( input, output, session ) {
  
  # General functions ----
  window_height <- reactive({ifelse(is.null(input$height), 0, as.numeric(input$height))})
  window_width <- reactive({ifelse(is.null(input$width), 0, as.numeric(input$width))})
  
  # Options ----
  options(
    shiny.maxRequestSize = 100 * 1024^2
    ,shiny.trace = TRUE
    ,spinner.color = "#03A7CA"
  )
  
  # File upload memory ----
  output$file_upload <- reactive({
    return(!is.null(input$input_file))
  })
  outputOptions(output, "file_upload", suspendWhenHidden = FALSE)
  
  # Information page ----
  output$welcome_info <- renderUI({
    HTML("<br><br><b>Welcome to the RAI4PL app for North West London Pathology</b><br>
             Instructions for use are defined in the subsections below.<br>")
  })
  
  output$data_upload_info <- renderUI({
    HTML(
      "<br><b>Info and data upload tab:</b><br><br>
    
      <p style='margin-left: 20px'>
      <b>Data upload:</b>
      <ul>
        <li> Using the <b>[Browse]</b> button, find and select the .csv file containing your data>
      </ul>
      <p style='margin-left: 20px'>
      <b>Data viewer:</b>
      <ul>
        <li>Once uploaded, your data are available to view under the <b>Data viewer</b> tab<br>
        <li>Check the data have uploaded correctly and, when ready, press the <b>[Fit model]</b> button
      </ul>
      "
    )
  })
  
  output$modelling_info <- renderUI({
    HTML(
      "<br><b>Model fitting tab:</b><br><br>
    
      <p style='margin-left: 20px'>
      <b>Model fit:</b>
      <ul>
        <li>Once <b>[Fit model]</b> is pressed, a robust 4PL regression model will be fitted to the data and displayed here
        <li>Blue points represent the individual counts for each standard
        <li>Red triangles represent identified outlying observations
      </ul>
      <p style='margin-left: 20px'>
      <b>Parameters:</b>
      <ul>
        <li>Once fitted, the robust 4PL model's parameters will be shown here
      </ul>
      <p style='margin-left: 20px'>
      <b>Export parameters:</b>
      <ul>
        <li>Click <b>[Export]</b> to export the model's parameters as a .csv file
      </ul>
      "
    )
  })
  
  output$contact_info <- renderUI({
    HTML("<br>Please contact edmund.wilkes@nhs.net for support or more information regarding this app.")
  })
  
  ## Data upload and viewer ----
  # Data upload with a fixed layout
  df_all <- reactive({
    readFile(input$input_file, headings = input$header_option, sheet = NULL) %>%
      dplyr::mutate(
        Conc = rep(c(NA, 0, 200, 100, 50, 25, 12.5, 6.25, 3.125, 1.5625, 0.7813, 0, 1.5625, 12.5, 100, 0, 0, 3.125, 12.5, 100, 1.5625, 12.5, 100, 0, 1.5625, 12.5, 100, 0, 3.125, 12.5, 100, 1.5625, 12.5, 100, 0), each = 2)
      )
  })
  
  # Data viewer
  output$dt_data <- DT::renderDataTable({
    
    req(input$input_file)
    dt <- DT::datatable(
      df_all()
      ,class = "cell-border stripe"
      ,rownames = FALSE
      ,extension = c("Buttons", "Scroller")
      ,options = list(
        paging = TRUE
        ,scrollX = TRUE
        ,scroller = TRUE
        ,scrollY = 700
        ,headerCallback = DT::JS(
          "function(thead) {",
          "  $(thead).css('font-size', '14px');",
          "}"
        )
      )
    ) %>%
      DT::formatStyle(columns = colnames(df_all()), fontSize = "90%")
    
  })
  
  ## Model fitting ----
  wrangle_data <- reactive({
    
    # This reactive function wrangles the input data for use in later functions.
    req(input$input_file)
    df <- df_all() %>%
      # Calculate means, SDs, and CVs for display
      dplyr::group_by(Name) %>%
      dplyr::summarise(
        Conc = mean(Conc)
        ,Count_mean = round(mean(Counts, na.rm = TRUE), 0)
        ,Count_SD = round(sd(Counts, na.rm = TRUE), 0)
        ,Count_CV = round(Count_SD / Count_mean * 100, 1)
        ,n = n()
      ) # %>%
      # dplyr::mutate(
      #   Subtract = (Count_mean - Count_mean[Name == "NSB"])
      #   ,Response =  Count_mean / Count_mean[Name == "ZA"]
      #   ,Logit = qlogis(Response)
      #   ,Log_Conc = log10(Conc)
      # ) %>%
      # dplyr::filter(Name != "NSB")
    return(df)
    
  })
  
  # Reactive values
  model_output <- reactiveValues(fit = NULL)
  notification <- reactiveValues(value = NULL)
  
  # "Fit model" button observations
  observeEvent(input$run_model, {
    # This observes the "Fit model" button being pressed and then fits a 4PL
    # regression model to the data and updates the reactiveValue "model_output"
    req(input$input_file)
    model_output$fit <- dr4pl::dr4pl(Counts ~ Conc, data = df_all(), method.robust = "absolute", method.init = "Mead")
    notification$value <- "<br>Check the <b>[Model fitting]</b> tab for the results" 
  })
  
  output$run_button_text <- renderUI({
    HTML(notification$value)
  })
  
  ## Deprecated ##########################################################
  # fit_model <- reactive({
  #   
  #   # This reactive function fits a 4PL regression model to the data and
  #   # updates the reactiveValue "model_output"
  #   req(input$input_file)
  #   model <- dr4pl::dr4pl(Subtract ~ Conc, data = wrangle_data())
  #   return(model)
  #   
  # })
  ########################################################################
  
  # Model fitting
  output$model_fit <- renderPlot({
    
    req(input$input_file)
    
    if (is.null(model_output$fit)) {
      return()
    } else {
      plot(model_output$fit$robust.plot)+
        # geom_point(data = df_all(), aes(x = Conc, y = Count), alpha = 0.5, colour = "red2")+
        # geom_errorbar(
        #   data = wrangle_data()
        #   ,aes(ymin = Subtract - Count_SD, ymax = Subtract + Count_SD)
        #   ,colour = "blue"
        #   ,width = 0
        #   ,size = 1
        # )+
        xlab("hCG concentration (U/L)")+
        ylab("Mean counts (cpm)")+
        labs(title = "")+
        plotTheme(12)
    }
    
  })
  
  ## Deprecated ############################################
  # output$model_fit_logit <- renderPlot({
  #   p <- ggplot(wrangle_data(), aes(x = Conc, y = Logit))+
  #     geom_point(colour = "blue2", size = 3)+
  #     scale_x_log10()+
  #     xlab("hCG concentration (U/L)")+
  #     ylab("Logit(mean counts)")+
  #     plotTheme(14)
  #   return(p)
  # })
  ##########################################################
  
  # Summarised data display
  output$dt_summary <- DT::renderDataTable({
    
    req(input$input_file)
    dt <- DT::datatable(
      wrangle_data()
      ,class = "cell-border stripe"
      ,rownames = FALSE
      ,extension = c("Buttons", "Scroller")
      ,options = list(
        paging = FALSE
        ,scrollX = TRUE
        # ,scroller = TRUE
        # ,scrollY = 600
        ,headerCallback = DT::JS(
          "function(thead) {",
          "  $(thead).css('font-size', '14px');",
          "}"
        )
      )
    ) %>%
      DT::formatStyle(columns = colnames(wrangle_data()), fontSize = "90%")
    
  })
  
  # Parameter display
  output$parameters <- renderUI({
    
    req(input$input_file)
    
    if (is.null(model_output$fit)) {
      return()
    } else {
     
      boxes <- list(
        fluidRow(
          valueBox(
            subtitle = "A (estimated reference, ZA)"
            ,color = "blue"
            ,width = 3
            ,value = round(model_output$fit$parameters[1], 1)
          )
          ,valueBox(
            subtitle = "B (slope)"
            ,color = "blue"
            ,width = 3
            ,value = round(model_output$fit$parameters[3], 2)
          )
          ,valueBox(
            subtitle = "C (ED50)"
            ,color = "blue"
            ,width = 3
            ,value = round(model_output$fit$parameters[2], 2)
          )
          ,valueBox(
            subtitle = "D (estimated blank, NSB)"
            ,color = "blue"
            ,width = 3
            ,value = round(model_output$fit$parameters[4], 1)
          )
        )
      )
      return(boxes)
    }
    
  })
  
  # Export button
  output$export_parameters <- downloadHandler(
    filename = function() {
      paste(input$input_file, "_parameters.csv", sep = "")
    }
    ,content = function(file) {
      write.csv(
        data.frame(
          Params = c("A", "B", "C", "D")
          ,Values = model_output$fit$parameters
        )
        ,file
        ,row.names = FALSE)
    }
  )
  
}
