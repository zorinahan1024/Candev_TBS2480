#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)

TBS <- read.csv("TPP.csv")
programs <- read.csv("programs.csv")

choice <- unique(factor(TBS$Department))

info <- tibble(programs$Department,programs$DRF.Program, programs$Description.EN)
info <- info%>% filter(programs$Department == "Agriculture and Agri-Food Canada")

header <- dashboardHeader(
  
  titleWidth = 700,
  
  title = "Exploring Government Funding to Businesses"
  
)



sidebar <- dashboardSidebar(
  
  width = 300,
  
  sidebarMenu(
    
    menuItem("English",icon=icon("far fa-credit-card")),
    menuItem("French",icon=icon("far fa-credit-card"))
    
  )
  
)



body <- dashboardBody(
  
  plotlyOutput("NTrans"),
  fluidRow(
    
    selectInput(
      
      "select_department",
      
      "Breakdown by Departments",
      
      choices = choice,
      
      selected= "Agriculture and Agri-Food Canada"
      
    ),
    
    tableOutput("test")
    
  )
)





ui<-dashboardPage(skin ="blue",
                  
                  header, sidebar, body
                  
)


server <- (function(input,output,session){
  sum_dep <- TBS %>% 
    group_by(Department) %>%
    summarise(count=n())
  
  output$NTrans <- renderPlotly({
    p <- plot_ly(sum_dep, labels= ~Department, values = ~count) %>% add_pie(hole = 0.6) %>%
      layout(title = "Number of Transfer Payment Program",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  
  output$test <- renderTable({
    info
  })
  
})

shinyApp(ui = ui, server = server)