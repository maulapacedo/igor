#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#Carregar Pacotes 

library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(shinydashboard)
library(readr)
library(lubridate)
library(DT)
library(gt)
library(htmlTable)
library(kableExtra)
library(magrittr)
library(skimr)
library(gtsummary)
library(readxl)

#Evitar o cinza 
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })

#Abrir Dados  
afastamentos_2021 <- read_excel("base_afastamentos_2021.xlsx")

#User Interface 
ui <- fluidPage(theme = paste0("sdcwww/", getShinyOption(".guitheme")),
  tabsetPanel(
    # 1 tab panel 
    tabPanel(title = "Visualização",

             #tab box
             tabBox(id = "t2", width = 12,
                    #I - tab panel 
                    tabPanel(title = "Valor da Hora Técnica",
                             icon = icon("dollar"),
                             value = "trends",
                             fluidPage(
                               fluidRow(
                                 column(width = 12,
                                        box(title = "Filtros", width = "100%",
                                            column(width = 5,
                                                  
                                                   box(width = "100%",
                                                       selectizeInput(inputId = "select_UF",
                                                                      label =  "Estados:",
                                                                      choices = c("TODOS", unique(afastamentos_2021$UF_da_UPAG_de_vinculacao)),
                                                                      multiple = T, 
                                                                      selected = "TODOS"))
                                            ),
                                            
                                            column(width = 5,
                                                   box(width = "100%",
                                                       selectizeInput(inputId = "descricao_2",
                                                                      label = "Descrição do Afastamento:",
                                                                      choices = c("TODOS", unique(afastamentos_2021$Descricao_do_afastamento)),
                                                                      multiple = T, options = list(maxItems = 5),
                                                                      selected = "TODOS")))
                                   )
                                 )     
                               ),
                               br(),
                               br(),
                               
                               fluidRow(  
                                 column(width = 12,
                                            gt_output(outputId = "tabela")))
                             )
                    ),
                    
                    #II - tab panel
                    tabPanel(title = "Histograma",
                             icon = icon("chart-bar"),
                             value = "trends",
                             #fluid row 1
                             fluidRow(
                               column(width = 12,
                                      box(title = "Filtros", width = "100%",
                                          column(width = 5,
                                                 box(width = "100%",
                                                     selectizeInput(inputId = "uf_2",
                                                                    label =  "Estados:",
                                                                    choices = c("TODOS", unique(afastamentos_2021$UF_da_UPAG_de_vinculacao)),
                                                                    multiple = T, 
                                                                    selected = "TODOS"))
                                          ),
                                          
                                          column(width = 5,
                                                 box(width = "100%",
                                                     selectizeInput(inputId = "descricao_4",
                                                                    label = "Descrição do Afastamento:",
                                                                    choices = c("TODOS", unique(afastamentos_2021$Descricao_do_afastamento)),
                                                                    multiple = T, options = list(maxItems = 5),
                                                                    selected = "TODOS")))
                                      )
                               )     
                             ),
                             
                             #fluidrow 2  
                             fluidRow(
                               column(width = 12,
                                      box(title = "Histograma - Valor da Hora Técnica (2021)",
                                          status = "primary", 
                                          solidHeader = TRUE, 
                                          collapsible = TRUE,
                                          width = "100%",
                                          plotlyOutput("histplot"))
                               )
                             )
                    ),
                    
                    #III - tab panel
                    tabPanel(title = "BoxPlot",
                             icon = icon("bar-chart-o", lib = "font-awesome"),
                             value = "trends",
                             
                             fluidRow(
                               column(width = 12,
                                      box(title = "Filtros", width = "100%",
                                          column(width = 5,
                                                 box(width = "100%",
                                                     selectizeInput(inputId = "uf_1",
                                                                    label =  "Estados:",
                                                                    choices = c("TODOS", unique(afastamentos_2021$UF_da_UPAG_de_vinculacao)),
                                                                    multiple = T, 
                                                                    selected = "TODOS"))
                                          ),
                                          
                                          column(width = 5,
                                                 box(width = "100%",
                                                     selectizeInput(inputId = "descricao_3",
                                                                    label = "Descrição do Afastamento:",
                                                                    choices = c("TODOS", unique(afastamentos_2021$Descricao_do_afastamento)),
                                                                    multiple = T, options = list(maxItems = 5),
                                                                    selected = "TODOS"))),
                                      )
                               )     
                             ),
                             br(),
                             fluidRow(
                               column(width = 12,
                                      box(width = "100%",
                                          title = "BoxPlot - Valor da Hora Técnica (2021)",
                                          status = "primary", 
                                          solidHeader = TRUE, 
                                          collapsible = TRUE, 
                                          plotlyOutput("boxplot"))
                               ))
                    )
             )
    ),
    
  # 2 tab item
  tabPanel(title = "Base de Dados",
           
           tags$style(HTML("
                           .tabbable > .nav > li > a {background-color: #015227; color: white}")),
           #tab box
           tabBox(id = "t1", width = 12, 
                 tabPanel(title = "Base de Dados",
                          tags$style(HTML("
                           .nav > li > a {color:#015227; font-weight:600}")),
                          icon = icon("table"),
                          fluidPage(
                            titlePanel("Base de Dados Afastamentos (2021)"),
                            
                            # Create a new Row in the UI for selectInputs
                            fluidRow(
                              tags$body(tags$style(HTML(".col-sm-5{
                                            padding: 1.375rem; border-radius: 1rem; 
                                            box-shadow: 0px 1px 3px rgba(0, 0, 0, 0.12);
                                            background-color: #f4f4f4; margin: 0 2rem};"))),
                              column(5, 
                                     selectInput("UF",
                                                 "Estado:",
                                                 c("TODOS",
                                                   unique(afastamentos_2021$UF_da_UPAG_de_vinculacao)))
                              ),
                              column(5,
                                     selectInput("instituicao",
                                                 "Instituição:",
                                                 c("TODOS",
                                                   unique(afastamentos_2021$Nome_orgao_de_origem)))
                              )),
                              br(), 
                            fluidRow(
                              column(12,
                                     br(), 
                                     tags$body(tags$style(HTML(".shiny-download-link{
                                            padding:1.375rem; border-radius:1rem; 
                                            box-shadow: 0px 1px 3px rgba(0, 0, 0, 0.12);
                                            color:white;
                                            background-color: #015227;};"))),
                                     downloadButton("downloadData", "Download"))
                                
                              ,
                              #column(4,
                              #       dateRangeInput(inputId = "data_afastamento",
                              #                      label =  "Data Afastamento:", format = "yyyy",
                              #                      start = min(as.character(afastamentos_2$`Ano Início Afastamento`)),
                              #                      end   = max(as.character(afastamentos_2$`Ano Início Afastamento`))
                              #)),
                             
                              column(
                                br(),
                                br(),
                                     title = "Base Afastamentos",
                                         width = 12,
                                         DT::dataTableOutput("dataT", height = "100px")) 
                              )
                             )
                 ),
                 tabPanel(title = "Estrutura dos Dados",
                          icon = icon("cubes"),
                          fluidPage(
                            titlePanel("Estrutura da Base de Dados de Afastamentos (2021)"),
                            br(),
                            
                            # Create a new Row in the UI for selectInputs
                            fluidRow(
                              column(width = 12,
                                    
                                     gt_output("str"))
                            )
                          )
                 ),
                 
                 tabPanel(title = "Sumário Estatístico",
                          icon = icon("list-alt"),
                          verbatimTextOutput("summary"), 
                          htmlOutput("inc")))
  )),                   
  
tags$body(tags$style(HTML('*{box-sizing: border-box; font-family: "Montserrat", sans-serif};')))

  
), 
textOutput("keep_alive")

  

#-------------------------------------------------------------------------------#

#SERVER
server <- function(input, output, session){
  
  #PRIMEIRO TAB ITEM  
  react_afastamentos <-  reactive({
    ## filtro UF
    print(input)
    b <- afastamentos_2021
    if (! "TODOS" %in% input$UF){
      b <- b |> 
        filter(UF_da_UPAG_de_vinculacao %in% input$UF)
    }
    
    #Filtro Descricao 
    if(! "TODOS" %in% input$instituicao){
      b <- b |>
        filter(Nome_orgao_de_origem %in% input$instituicao)
    }
    return(b)
  }) 
  
  #DATATABLE 
  output$dataT <- DT::renderDataTable({
    DT::datatable(react_afastamentos(),
                  options = list(autoWidth = TRUE,
                                 pagelength = 5,
                                 scrollX = TRUE,
                                 scrollY = "100%",
                                 columnDefs = list(list(
                                   width = "110px", targets = "_all"))
                  ))})
  #download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("react_afastamentos-", Sys.Date(), ".csv", sep = ";")
    },
    content = function(file) {
      write.csv(react_afastamentos(), file)
    }
  )

  #Estrutura dos Dados
  output$str <- render_gt({
    afastamentos_2021 |> 
      skimr::skim() |> 
      gt::gt()}) 
  
  #Sumario Estatistico 
  output$summary <- renderUI({
    req(react_afastamentos())
    
    sumProfile <- summarytools::view(summarytools::dfSummary(react_afastamentos(),
                                                             style = "grid",
                                                             method = "render"),
                                     omit.headings = TRUE, 
                                     bootstrap.css = FALSE, 
                                     escape.pipe = TRUE, 
                                     file = "./tmp.html")
    sumProfile
  })
    
  
  getPage<-function() {
    return(includeHTML("./tmp.html"))
  }
  output$inc <- renderUI({
    req(react_afastamentos())
    getPage()
  })
  
  #------------------------------------------------------------------------------#  
  #SEGUNDO TAB ITEM 
  #filtro tabela   
  meus_dados <-  reactive({
    ## filtro UF
    print(input)
    a <- afastamentos_2021
    if (! "TODOS" %in% input$select_UF){
      a <- a |> 
        filter(UF_da_UPAG_de_vinculacao %in% input$select_UF)
    }
    #Filtro Descricao 
    if(! "TODOS" %in% input$descricao_2){
      a <- a |>
        filter(Descricao_do_afastamento %in% input$descricao_2)
      
    }
    return(a)
  })
  
  #OUTPUT TABELA 
  output$tabela <- render_gt({
    tabela <- meus_dados() |> 
      dplyr::summarize(instituicao = "Afastamentos Realizados por Órgãos Federais",
                       valor_medio_hora = ((sum(Rendimento_Liquido_Hora))/n())
      ) |> 
      gt(rowname_col = "Nome") |> 
      tab_header(
        title = md("**Valor da Hora Técnica para todos os Órgãos**"),
        subtitle = md("**Ano: 2021**")
      ) |> 
      cols_label(
        instituicao = md(""), 
        valor_medio_hora = md("**Valor Médio da Hora Técnica (R$)**")
      ) |> 
      opt_align_table_header(align = "left") |> 
      fmt_number(columns = 2) |> 
      cols_width(
        instituicao~px(200),
        valor_medio_hora~px(200)
      ) |> 
      tab_source_note(source_note = md("*Fonte: Elaboração Própria.*")
      ) |>
      data_color(
        columns = valor_medio_hora,
        colors = "#015227"
      ) |> 
      tab_stubhead(
        label = md ("**Nome da Instituição**")
      ) |> 
      opt_table_font(
        font = google_font("Times New Roman"), 
        weight = 600 
      ) |> 
      cols_align(
        align = "center",
        columns = everything()
      )
  })
  
  #filtro boxplot 
  meus_dados_box <-  reactive({
    ## filtro UF
    print(input)
    a <- afastamentos_2021
    if (! "TODOS" %in% input$uf_1){
      a <- a |> 
        filter(UF_da_UPAG_de_vinculacao %in% input$uf_1)
    }
    #Filtro Descricao 
    if(! "TODOS" %in% input$descricao_3){
      a <- a |>
        filter(Descricao_do_afastamento %in% input$descricao_3)
      
    }
    return(a)
  })
  
  #OUTPUTBOXPLOT
  
  output$boxplot <- renderPlotly({
    boxplot <- meus_dados_box()|>
      plot_ly(color = "#015227") |>
      add_boxplot(~Rendimento_Liquido_Hora) |> 
      layout(xaxis = list(title = "BoxPlot - Valor da Hora Técnica em 2021"))
  })
  
  #filtro histograma 
  meus_dados_hist <-  reactive({
    ## filtro UF
    print(input)
    a <- afastamentos_2021
    if (! "TODOS" %in% input$uf_2){
      a <- a |> 
        filter(UF_da_UPAG_de_vinculacao %in% input$uf_2)
    }
    #Filtro Descricao 
    if(! "TODOS" %in% input$descricao_4){
      a <- a |>
        filter(Descricao_do_afastamento %in% input$descricao_4)
      
    }
    return(a)
  })
  
  #OUTPUT HISTOGRAMA 
  output$histplot <- renderPlotly({
    hist <- meus_dados_hist() |> 
      plot_ly(marker = list(color = "#015227")) |> 
      add_histogram(~Rendimento_Liquido_Hora) |> 
      layout(xaxis = list(title = "Histograma - Valor da Hora Técnica em 2021")) 
    })
  
  output$keep_alive <- renderText({
  req(input$alive_count)
  input$alive_count
})
  
}


# Run the application 
shinyApp(ui = ui, server = server)
