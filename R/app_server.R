#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import ggplot2
#' @importFrom magrittr "%>%"
#' @import dr4pl
#' @import drc
#' @import plotly
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
    HTML("<br><br><b>Welcome to the RAICAL app for North West London Pathology</b><br>
             Instructions for use are defined in the subsections below.<br>")
  })
  
  output$data_upload_info <- renderUI({
    HTML(
      "<br><b>Info and data upload tab:</b><br>
    
      <p style='margin-left: 20px'>
      <b>Data upload:</b>
      <ul>
        <li> Using the <b>[Browse]</b> button, find and select the .csv file containing your data
      </ul>
      <p style='margin-left: 20px'>
      <b>Data viewer:</b>
      <ul>
        <li>Once uploaded, your data are available to view under the <b>Data viewer</b> tab
        <li>Check the data you have uploaded carefully
        <li>Choose the model type you wish to fit (defaults to 'Robust' - this is optimal in most instances)
        <li>When ready, press the <b>[Fit model]</b> button
        <li>If the model type is changed retrospectively, the analysis will automatically update
      </ul>
      "
    )
  })
  
  output$modelling_info <- renderUI({
    HTML(
      "<br><b>Model fitting tab:</b><br>
    
      <p style='margin-left: 20px'>
      <b>Model fit:</b>
      <ul>
        <li>Once <b>[Fit model]</b> is pressed, the chosen 4PL regression model will be fitted to the data and displayed here
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
  df_all <- reactive({
    readFile(input$input_file, headings = input$header_option, sheet = NULL) %>%
      dplyr::mutate(
        Conc = dplyr::case_when(
          Name == "STD 1" ~ 200
          ,Name == "STD 2" ~ 100
          ,Name == "STD 3" ~ 50
          ,Name == "STD 4" ~ 25
          ,Name == "STD 5" ~ 12.5
          ,Name == "STD 6" ~ 6.25
          ,Name == "STD 7" ~ 3.125
          ,Name == "STD 8" ~ 1.5625
          ,Name == "STD 9" ~ 0.78125
          ,Name == "ZA" ~ 0
          ,TRUE ~ NA_real_ # i.e. NSB and and other additions
        )
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
        `[hCG] (U/L)` = mean(Conc)
        ,`Count mean (cpm)` = round(mean(Counts, na.rm = TRUE), 0)
        ,`Count SD (cpm)` = round(sd(Counts, na.rm = TRUE), 0)
        ,`Count CV (%)` = round(`Count SD (cpm)` / `Count mean (cpm)` * 100, 1)
        ,n = dplyr::n()
      ) # %>%
    
      ##################################################################
      # Deprecated code that subtracts NSB and calculates response vs ZA
      ##################################################################
      # dplyr::mutate(
      #   Subtract = (Count_mean - Count_mean[Name == "NSB"])
      #   ,Response =  Count_mean / Count_mean[Name == "ZA"]
      #   ,Logit = qlogis(Response)
      #   ,Log_Conc = log10(Conc)
      # ) %>%
      # dplyr::filter(Name != "NSB")
    
    return(df)
    
  })
  
  # Reactive values definition
  model_output <- reactiveValues(model = NULL)
  notification <- reactiveValues(value = NULL)
  plot_output <- reactiveValues(plot = NULL, boxes = NULL)
  
  observeEvent(input$run_model, {
    notification$value <- "<br>Check the <b>[Model fitting]</b> tab for the results" 
  })
  
  # Listen to both "run_model", "model_choice", and "input_file" inputs
  toListen <- reactive({
    list(input$run_model, input$model_choice, input$input_file)
  })
  
  # Observe toListen() and execute code if values change
  observeEvent(toListen(), {
    
    # This observes the "Fit model" button being pressed and then fits a 4PL
    # regression model to the data and updates the reactiveValue "model_output",
    # "notification", and "plot_output".
    # The model fitted to the data depends on "model_choice" and uses either
    # dr4pl (robust) or drc (regular). 
    req(input$input_file)
    
    if (input$model_choice == "dr4pl") {
      model_output$model <- dr4pl::dr4pl(Counts ~ Conc, data = df_all(), method.robust = "absolute", method.init = "Mead")
    } else if (input$model_choice == "drc") {
      model_output$model <- drc::drm(Counts ~ Conc, data = df_all(), robust = "median", fct = drc::LL.4(names = c("Slope", "Lower", "Upper", "IC50")))
    }
    
    # Make plots ----
    df_pred <- data.frame(Conc = exp(seq(log(0.78125), log(200), length.out = 2000)))
    
    if (is.null(model_output$model)) {
      return()
    } else {
      if (input$model_choice == "dr4pl") {
        
        df_points <- df_all() %>%
          dplyr::filter(!is.na(Conc)) %>%
          dplyr::mutate(outlier = "No")
        df_points$outlier[model_output$model$idx.outlier] <- "Yes"
        
        params <- c(
          model_output$model$parameters[1]
          ,model_output$model$parameters[3]
          ,model_output$model$parameters[2]
          ,model_output$model$parameters[4]
        )
        df_pred$Counts_pred <- predictCurve(params = params, data = df_pred)
        
        plot_output$plot <- plotly::ggplotly(
          ggplot2::ggplot(df_points, aes(x = Conc, y = Counts))+
            ggplot2::geom_point(
              alpha = 0.75
              ,size = 4
              ,ggplot2::aes(
                colour = outlier
                ,text = paste0("Standard: ", Name, "\nConcentration: ", Conc, "\nTube: ", ReactionTube, "\nCount: ", Counts)
                ,shape = outlier
              )
            )+
            ggplot2::geom_line(
              data = df_pred
              ,size = 1
              ,ggplot2::aes(x = Conc, y = Counts_pred)
            )+
            plotTheme(14)+
            ggplot2::scale_x_log10()+
            ggplot2::annotation_logticks(side = "b")+
            ggplot2::xlab("hCG concentration (U/L)")+
            ggplot2::ylab("Gamma counts (cpm)")+
            ggplot2::expand_limits(y = 0)+
            ggplot2::scale_colour_manual(values = c("blue2", "red2"))+
            ggplot2::theme(legend.position = "none")+
            ggplot2::scale_shape_manual(values = c(16,17))
          ,tooltip = "text"
        )
        
      } else if (input$model_choice == "drc") {
        
        df_pred$Counts_pred <- predict(object = model_output$model, newdata = df_pred)
        
        plot_output$plot <- plotly::ggplotly(
          ggplot2::ggplot(df_all(), aes(x = Conc, y = Counts))+
            ggplot2::geom_point(
              alpha = 0.75
              ,size = 4
              ,colour = "blue2"
              ,shape = 16
              ,ggplot2::aes(text = paste0("Standard: ", Name, "\nConcentration: ", Conc, "\nTube: ", ReactionTube, "\nCount: ", Counts))
            )+
            ggplot2::geom_line(
              data = df_pred
              ,size = 1
              ,ggplot2::aes(x = Conc, y = Counts_pred)
            )+
            plotTheme(14)+
            ggplot2::scale_x_log10()+
            ggplot2::annotation_logticks(side = "b")+
            ggplot2::xlab("hCG concentration (U/L)")+
            ggplot2::ylab("Gamma counts (cpm)")+
            ggplot2::expand_limits(y = 0)
          ,tooltip = "text"
        )
        
      }
    }
    
    # Make valueBoxes ----
    if (is.null(model_output$model)) {
      return()
    } else {
      
      if (input$model_choice == "dr4pl") {
        params <- c(
          model_output$model$parameters[1]
          ,model_output$model$parameters[3]
          ,model_output$model$parameters[2]
          ,model_output$model$parameters[4]
        )
      } else if (input$model_choice == "drc") {
        params <- c(
          model_output$model$fit$par[3]
          ,-model_output$model$fit$par[1]
          ,model_output$model$fit$par[4]
          ,model_output$model$fit$par[2]
        )
      }
      
      plot_output$boxes <- list(
        fluidRow(
          valueBox(
            subtitle = "A (estimated ZA)"
            ,color = "blue"
            ,width = 3
            ,value = round(params[1], 1)
          )
          ,valueBox(
            subtitle = "B (slope)"
            ,color = "blue"
            ,width = 3
            ,value = round(params[2], 2)
          )
          ,valueBox(
            subtitle = "C (ED50)"
            ,color = "blue"
            ,width = 3
            ,value = round(params[3], 2)
          )
          ,valueBox(
            subtitle = "D (estimated NSB)"
            ,color = "blue"
            ,width = 3
            ,value = round(params[4], 1)
          )
        )
      )
    }
    
  })
  
  # Update text
  output$run_button_text <- renderUI({
    HTML(notification$value)
  })
  
  # Model fit plot
  output$model_fit <- plotly::renderPlotly({
    plot_output$plot
  })
  
  # Dynamic UI to display plot in box with dynamic title
  output$plot_box <- renderUI({
    
    req(input$model_choice)
    if (input$model_choice == "dr4pl") {
      fit_type = "robust"
    } else {
      fit_type = "regular"
    }
    
    box(
      title = paste0("Model fit (", fit_type, ")")
      ,solidHeader = TRUE
      ,status = "primary"
      ,width = 12
      ,plotly::plotlyOutput("model_fit", height = 575)
    )
    
  })
  
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
  
  # Parameter display in valueBoxes
  output$parameters <- renderUI({
    plot_output$boxes
  })
  
  # Export button (exports as a .csv file for PDM pickup)
  output$export_parameters <- downloadHandler(
    filename = function() {
      paste("Parameters_HCXXXXXX", ".csv", sep = "")
    }
    ,content = function(file) {
      
      if (input$model_choice == "dr4pl") {
        params <- c(
          round(model_output$model$parameters[1], 2)
          ,round(model_output$model$parameters[3], 3)
          ,round(model_output$model$parameters[2], 3)
          ,round(model_output$model$parameters[4], 2)
        )
      } else if (input$model_choice == "drc") {
        params <- c(
          round(model_output$model$fit$par[3], 2)
          ,round(-model_output$model$fit$par[1], 3)
          ,round(model_output$model$fit$par[4], 3)
          ,round(model_output$model$fit$par[2], 2)
        )
      }
      
      write.csv(
        x = data.frame(
          Parameter = c("A", "B", "C", "D")
          ,Value = params
        )
        ,file = file
        ,row.names = FALSE
      )
      
    }
  )
  
}
