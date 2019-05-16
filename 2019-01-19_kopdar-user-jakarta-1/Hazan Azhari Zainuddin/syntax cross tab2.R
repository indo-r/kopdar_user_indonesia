library(shiny)
library(shinydashboard)
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")





ui <- dashboardPage(
  dashboardHeader(title = "Simpletab"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Import Data", tabName = "import", icon = icon("folder-o")),
      menuItem("Tabulation", tabName = "Cross_tab_metode", icon = icon("list-alt"))
      
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # First tab content
      tabItem(tabName = "import", 
              fluidRow(
                box(
                  h5(helpText("Select the read.table parameters below")),
                  checkboxInput(inputId="header",label = "Header", value=TRUE),
                  checkboxInput(inputId="stringAsFactor",label = "stringAsFactor", value=FALSE),
                  radioButtons(inputId= "sep", label = "Separator", choices = c(Comma=",", Semicolon=";"),selected = ";")
                ),
                box(
                  h4("Import Data"),
                  fileInput("file", label = "Upload data csv"),
                  h5(helpText("Silakan Upload data anda. Aplikasi akan menampilkan secara default contoh data untuk latihan analisis."))
                )
              ),
              br(),
              dataTableOutput("data_awal")
      ),
      
      
      tabItem(tabName = "Cross_tab_metode",

                
                
        
                                fluidRow(
                                  box(solidHeader = TRUE, width=6,status="warning",
                                      selectInput("pilih.variabel.crosstab1","Row Variable","",multiple=TRUE,selectize=TRUE),
                                      selectInput("pilih.variabel.crosstab2","Column Variable","",multiple=TRUE,selectize=TRUE)
                                  ),
                                  box(solidHeader = TRUE, width=6,status="warning",
                                      radioButtons("metodenya.crosstab","type",
                                                   choices=c("Frequency count"="f","Row percentages"="r","Column percentages"="c","Joint percentages"="j","Total percentages"="t"),
                                                   selected="f"),
                                      checkboxInput(inputId="pasangmargin",label = "Add Sum", value=T)
                                  ),
                                  box(solidHeader = TRUE, width=12,status="warning",
                                      actionButton("goButton.crosstab", "RUN")
                                      ),
                                  box(solidHeader = TRUE, width=12,status="warning",
                                      verbatimTextOutput("hasil.mesin.crosstab")
                                  )
                                 
                                 
                                )
                                
                                

              
      )
      
      
      
    )
  )
  
  
)







###Bagian Server
server <- function(input, output, session) { 
  
  ##bagian input data
  origin<-reactive({
    if(is.null(input$file)) {
      df<-read.table("data_awal.csv", sep=input$sep, header=input$header, stringsAsFactors = input$stringAsFactor)
    }            
    else{
      file2<-input$file
      df<-read.table(file=file2$datapath, sep=input$sep, header=input$header, stringsAsFactors = input$stringAsFactor)
    } 
    df
  })
  
  df=reactive({
    origin()
  })
  
  
  
  ##bagian munculkan data import
  output$data_awal<-renderDataTable({
    df()
  },
  options = list(lengthMenu = c(5, 30, 50, 100), pageLength = 10, scrollX=T)
  )
  
  
  df.crosstab.data=reactive({
    origin()
  })
  
  observe({
    
    df.crosstab.data=df.crosstab.data()
    
    vars.crosstab.data=c("Select..."="",colnames(df.crosstab.data))
    
    
    updateSelectInput(session,"pilih.variabel.crosstab1",choices= vars.crosstab.data)
    updateSelectInput(session,"pilih.variabel.crosstab2",choices= vars.crosstab.data)
    
  })
  
  
  
  
  mesin.crosstab=eventReactive(input$goButton.crosstab, { 
    crosstab(df.crosstab.data(), row.vars = paste(input$pilih.variabel.crosstab1), col.vars = paste(input$pilih.variabel.crosstab2), type = paste(input$metodenya.crosstab),addmargins = input$pasangmargin)
    
  })
  
  
  output$hasil.mesin.crosstab<-renderPrint({
    print(mesin.crosstab())
  }) 
  
  
  
  
  
} 





shinyApp(ui, server) 


