#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  
  tagList(
    
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    ## Start dashboardPage() ----
    dashboardPage(
      
      ## Title ----
      title = "RIA4PL"
      
      ## Header ----
      ,dashboardHeader(
        # title = tags$a(
        #   href='http://www.nwlpathology.nhs.uk'
        #   ,tags$img(src = 'www/logo-nhs.png', width = '50%')
        # )
        title = "RIA4PL"
      )
      
      ## Sidebar ----
      ,dashboardSidebar(
        sidebarMenu(
          # menuItem("Information", tabName = "information", icon = icon("info-circle"))
          menuItem("Info and data upload", tabName = "data_upload", icon = icon("file-upload"))
          ,menuItem("Model fitting", tabName = "modelling", icon = icon("chart-line"))
        )
      )
      
      ## Body ----
      ,dashboardBody(
        
        # Tags ----
        tags$style(
          HTML(
            ".skin-blue .main-header .logo {
              color:#FFFFFF;
              background-color:#1F335C;
            }
            .skin-blue .main-header .logo:hover {
              color:#FFFFFF;
              background-color:#1F335C;
            }
            .skin-blue .main-header .navbar {
              color:#FFFFFF;
              background-color:#1F335C;
            }
            .small-box.bg-navy {
              background-color:#1F335C !important;
            }
            .small-box.bg-blue {
              background-color:#03A7CA !important;
            }
            .small-box.bg-green {
              background-color:#56AF31 !important;
            }
            .box.box-solid.box-primary>.box-header {
              color:#FFFFFF;
              background:#1F335C;
            }
            .box.box-solid.box-primary {
              border-bottom-color:#1F335C;
              border-left-color:#1F335C;
              border-right-color:#1F335C;
              border-top-color:#1F335C;
            }
            .box.box-solid.box-info>.box-header {
              color:#fff;
              background:#03A7CA;
            }
            .box.box-solid.box-info {
              border-bottom-color:#03A7CA;
              border-left-color:#03A7CA;
              border-right-color:#03A7CA;
              border-top-color:#03A7CA;
            }
            .box.box-solid.box-success>.box-header {
              color:#fff;
              background:#56AF31;
            }
            .box.box-solid.box-success {
              border-bottom-color:#56AF31;
              border-left-color:#56AF31;
              border-right-color:#56AF31;
              border-top-color:#56AF31;
            }"
          )
        )
        
        # Tabs ----
        ,tabItems(
          
          # "Information" ----
          # tabItem(
          #   tabName = "information"
          #   ,p()
          #   ,column(
          #     width = 12
          #     ,fluidRow(
          #       box(
          #         title = "General information"
          #         ,solidHeader = TRUE
          #         ,collapsible = TRUE
          #         ,status = "primary"
          #         ,tags$img(src = "www/logo-nwlp.png", width = "50%")
          #         ,htmlOutput("welcome_info", inline = TRUE)
          #         ,htmlOutput("data_upload_info", inline = TRUE)
          #         ,htmlOutput("modelling_info", inline = TRUE)
          #         ,htmlOutput("contact_info", inline = TRUE)
          #       )
          #     )
          #   )
          # )
          
          # "Data upload" ----
          tabItem(
            tabName = "data_upload"
            ,p()
            ,column(
              width = 4
              ,fluidRow(
                box(
                  title = "General information"
                  ,solidHeader = TRUE
                  ,status = "primary"
                  ,width = 12
                  ,tags$img(src = "www/logo-nwlp.png", width = "50%")
                  ,htmlOutput("welcome_info", inline = TRUE)
                  ,htmlOutput("data_upload_info", inline = TRUE)
                  ,htmlOutput("modelling_info", inline = TRUE)
                  ,htmlOutput("contact_info", inline = TRUE)
                )
              )
            )
            ,column(
              width = 3
              ,fluidRow(
                box(
                  title = "Data upload"
                  ,width = 12
                  ,solidHeader = TRUE
                  ,status = "primary"
                  ,fileInput(
                    inputId = "input_file"
                    ,label = "Select the input file (.csv/.xlsx) containing your data:"
                    ,accept = c(".csv", ".xls", ".xlsx")
                    ,multiple = FALSE
                  )
                  ,conditionalPanel(
                    condition = "output.file_upload == true"
                    ,actionButton(
                      inputId = "run_model"
                      ,label = "Fit model"
                      ,icon = icon("play")
                      ,width = "100%"
                    )
                    ,span(htmlOutput("run_button_text"), style="color:red")
                  )
                )
              )
            )
            ,column(
              width = 5
              ,fluidRow(
                conditionalPanel(
                  condition = "output.file_upload == true"
                  ,box(
                    title = "Data viewer"
                    ,width = 12
                    ,solidHeader = TRUE
                    ,status = "primary"
                    ,DT::dataTableOutput("dt_data")
                  )
                )
              )
            )
          )
          
          # "Results" ----
          ,tabItem(
            tabName = "modelling"
            ,p()
            ,fluidRow(
              conditionalPanel(
                condition = "output.file_upload == true && input.run_model != 0"
                ,box(
                  title = "Model fit"
                  ,solidHeader = TRUE
                  ,status = "primary"
                  ,width = 6
                  ,shinycssloaders::withSpinner(
                    plotOutput("model_fit", height = 625)
                    ,type = 6
                  )
                )
              )
              ,conditionalPanel(
                condition = "output.file_upload == true && input.run_model != 0"
                ,box(
                  title = "Logit-log transformation"
                  ,solidHeader = TRUE
                  ,status = "primary"
                  ,width = 6
                  ,shinycssloaders::withSpinner(
                    plotOutput("model_fit_logit", height = 625)
                    ,type = 6
                  )
                )
              )
            )
            ,fluidRow(
              p()
              ,conditionalPanel(
                condition = "output.file_upload == true && input.run_model != 0"
                ,box(
                  title = "Model parameters"
                  ,solidHeader = TRUE
                  ,status = "primary"
                  ,width = 8
                  ,shinycssloaders::withSpinner(uiOutput("parameters"), type = 6)
                )
              )
              ,conditionalPanel(
                condition = "output.file_upload == true && input.run_model != 0"
                ,box(
                  title = "Data export"
                  ,solidHeader = TRUE
                  ,status = "primary"
                  ,width = 2
                  ,downloadButton("export_parameters", "Export parameters", width = "100%")
                )
              )
            )
          )
          
        ) # closes tabItems
        
        # Window sizing code ----
        ,tags$head(
          tags$script('
                var height = 0;
                var width = 0;
                $(document).on("shiny:connected", function(e) {
                  height = window.innerHeight;
                  width = window.innerWidth;
                  Shiny.onInputChange("height", height);
                  Shiny.onInputChange("width", width);
                });
                $(window).resize(function(e) {
                  height = window.innerHeight;
                  width = window.innerWidth;
                  Shiny.onInputChange("height", height);
                  Shiny.onInputChange("width", width);
                });
                '
          )
        )
        
        ## Skin colour ----
        ,skin = "blue"
        
      ) # closes dashboardBody
    ) # closes dashboardPage
  ) # closes tagList
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'RIA4PL'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

