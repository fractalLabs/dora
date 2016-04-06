####################################################
## This dashboard provides some tools for data   ###
## standarization.                               ###
## Utilities:                                    ###
## 1) Identification and anonymization           ###
##    of entities.                               ###
## 2) Identification and correction of states,   ###
##    municipalities and localities.             ###
## 3) Identification and correction of dates     ###
####################################################

##------------------------------
## Read in functions
##------------------------------
source("./functions.R")


################################
## UI
################################
ui <- dashboardPage(
    ##skin = "purple",
    dashboardHeader(title = "Validadora"),
    ## Dashboard Side Bar
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard",
                     tabName = "dashboard",
                     icon = icon("dashboard"))
        ),
        column(11, offset = 1,
               helpText(h3("Nota: "),
                        p(
                            paste0("Ejemplificación de la corrección automática de errores comúnes",
                                   "en bases de datos públicas"),
                            style="text-align:justify"
                        )
                        )
               )
    ),
    ## Dashboard Main Body
    dashboardBody(
        tags$head(
                tags$style(
                        HTML('
        .skin-blue .main-header .logo {
                              background-color: #4d4d4d;
                              }
        .skin-blue .main-header .navbar {
                              background-color: #4d4d4d;
                              }
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #00cc99;
                              }
        .skin-blue .main-sidebar {
                              background-color: #4d4d4d;
                              }
        .content-wrapper,
        .right-side {
                             background-color: #757575;
                     }
        .nav-tabs {
                              background-color: #bdbdbd;
                     }

        .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
                              background-color: #bdbdbd;
                              border-color: #bdbdbd;
                      }

        .nav-tabs-custom .nav-tabs li.active {
                              border-top-color: #bdbdbd;
                      }'
                      )               
               )
        ),
        tabItems(
            tabItem(
                tabName = "dashboard",
                fluidRow(

                    ##---------------------------
                    ## Controles de base de datos
                    ##---------------------------
                    box(
                        title       = "Base a validar",
                        width       = 3,                       
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE,
                        textInput("url",label = h3("URL del archivo"), value = "url"),
                        radioButtons("class", label = h3("Tipo de archivo"),
                                     choices = list("csv" = 1, "xlsx" = 2,
                                                    "dbf" = 3),
                                     selected = 1),
                        hr(),
                        checkboxInput("s_report", label = "Generar reporte"),
                        checkboxInput("s_correct", label = "Corregir base")
                    ),                    

                    ##---------------------------
                    ## Descarga de resultados
                    ##---------------------------
                    box(
                        title       = "Descarga de base",
                        width       = 3,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE,
                        textInput("filename", label  = h3("Nombre archivo"),
                                  value = "archivo"),
                        radioButtons("dclass", label = h3("Formato de descarga"),
                                     choices = list("csv" = 1, "xlsx" = 2,
                                                    "dbf" = 3),
                                     selected = 1),
                        downloadButton("downloadData","Descarga")
                    ),
                    
                    ##---------------------------
                    ## Reporte
                    ##---------------------------
                    box(
                        title       = "Reporte",
                        width       = 6,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE,
                        htmlOutput("report")
                    ),

                    ##---------------------------
                    ## Base de datos inicial
                    ##---------------------------
                    box(                        
                        title       = "Base de datos inicial",
                        width       = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE,
                        DT::dataTableOutput("in_table")
                    ),

                    ##---------------------------
                    ## Base de datos final
                    ##---------------------------
                    box(
                        ## status      = "info",
                        title       = "Base de datos final",
                        width       = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE,
                        DT::dataTableOutput("out_table")
                    )
                )
            )
        )
    )
)

################################
## SERVER
################################
server <- function(input, output){


    ## ------------------------------
    ## Obtiene base de datos.
    ## ------------------------------
    datasetInput <- reactive({
        ## En esta seccin se carga la base de datos desde la liga que se proporciona.
        ## https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data
        ## https://raw.githubusercontent.com/lromang/IDMX/master/data/coords.csv
        if(input$class == 2){
            full_ent <- data.frame(read.xls(input$url, sheetIndex = 1))
        }else if(input$class == 1){
            full_ent <- read.csv(
                input$url,
                stringsAsFactors = FALSE,
                header = TRUE,
                colClasses = rep("character", 6),
                encoding = "UTF-8"
            )
        }else{
            full_ent <- read.dbf(input$url)
        }
        full_ent
    })

    ## --------------------------------
    ## Despliega base de datos original
    ## --------------------------------
     output$in_table <- DT::renderDataTable({
         ## Despliegue de resultados.
         if(str_length(input$url) > 3){
             table <- datasetInput()
             if(input$s_report != TRUE){
             DT::datatable(table,
                           options = list(scrollX = TRUE))
             }else{
                 cols <- run.a.test(table)[[2]]
                 DT::datatable(table,
                               options = list(scrollX = TRUE))%>%
                     formatStyle(cols,
                                 color = '#c62828',                        
                                 fontWeight = 'bold')
             }
         }
     })

    ## ----------------------------------
    ## Despliega base de datos corregida
    ## ----------------------------------
    output$out_table <- DT::renderDataTable({
        if(str_length(input$url) > 3){
            data <- datasetInput()        
            if(input$s_correct == TRUE){
                ## Get analysis
                res       <- run.a.test(data)
                ent_cols  <- res[[2]][1]

                ## Entities            
                transform_ent  <- transform.all.col(data[,ent_cols], "ent")[,1:2]
                data_final     <- cbind(transform_ent, data)
                DT::datatable(data_final,
                              options = list(scrollX = TRUE)) %>%
                    formatStyle(1:2,
                                color = '#00cc99',
                                fontWeight = 'bold')
            }
        }
    })

    ## --------------------------------
    ## Despliega reporte
    ## --------------------------------
    output$report <- renderUI({
        if(input$s_report == TRUE){
            HTML(run.a.test(datasetInput())[[1]])
        }
    })
    
    ## ------------------------------
    ## Descarga base de datos
    ## ------------------------------
    output$downloadData <- downloadHandler(
        filename = function(){
            if(input$dclass == 1){
                paste0(input$filename, ".csv")
            }else if(input$dclass == 2){
                paste0(input$filename, ".xlsx")
            }else{
                paste0(input$filename, ".dbf")
            }
        },

            content = function(file){
                if(input$dclass == 1){
                    write.table(datasetInput(),
                                file,
                                sep = ",",
                                row.names = FALSE
                                )
                }else if(input$dclass == 2){
                    write.xlsx(datasetInput(),
                               file,
                               sheetName = "Sheet1")
                }else{
                    write.dbf(datasetInput(),
                              file)
                }
            }
    )   
}

shinyApp(ui, server)

