#install.packages('rsconnect')
#library(rsconnect)
#rsconnect::setAccountInfo(name='wehouse',
#                          token='44CF7E15C51B46C6199937D081330738',
#                          secret='jBqPDwybIH3N001TSB5UEyoyd82ALl22Rnv/2GXe')

packages = c('treemap', 'tidyverse', 'shiny', 'shinydashboard', 'dplyr', 'ggplot2', 'ggExtra')

for(p in packages){library
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p, character.only = T)
}

realis <- read_csv("data/TreeMap.csv")

realis_grouped <- group_by(realis,
                           `Year`,
                           `Planning Region`, `HDB Town`,
                           Storey_Level)

realis_summarised <- summarise(realis_grouped, 
                               `Total Unit Sold` = sum(Sales, na.rm = TRUE),
                               `Total Area` = sum(`Area (SQM)`, na.rm = TRUE),
                               `Average Resale Price` =  mean(`Average Resale Price`,na.rm=TRUE),
                               `Unit Price (PSF)` =  mean(`Unit Area (PSF)`,na.rm=TRUE))


Overview <- read_csv("data/Overview1.csv")



#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Navigation Bar")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
    menuItem("Dashboard 1", tabName = "dashboard1", icon = icon("dashboard")),
    menuItem("Dashboard 2", tabName = "dashboard2", icon = icon("dashboard")),
    menuItem("Dashboard 3", tabName = "dashboard3", icon = icon("dashboard")),
    menuItem("Dataset", tabName = "Datasets", icon = icon("fas fa-database"),
             menuSubItem("Tree Map Dataset", tabName = "sub_1"), 
             menuSubItem("Map Dataset", tabName = "sub_2"),
             menuSubItem("Overview Dataset", tabName = "sub_3")
    ))
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Overview",
            h1("Overview Page inclusive of Background and other info", align = "center")
    ),
    tabItem(tabName = "dashboard1",
            h1("Overview Dashboard", align = "center")
    ),
    tabItem(tabName = "dashboard2",
            h1("Dashboard 2 content", align = "center"),
            selectInput("Year", "Select Year:", unique(realis_summarised$`Year`), selected = 2020, multiple = FALSE
                       ),
            radioButtons("Plot", "Choose the visualisation to see:",
                         c("Resale Price" = "Average Resale Price",
                           "Unit Price" = "Unit Price (PSF)"), selected = "Average Resale Price"),
            plotOutput("Treemap",height="700px", width="1000px")
    ),
    tabItem(tabName = "dashboard3",
            h1("Dashboard 3 content", align = "center"),
            plotOutput("ScatterHist", height="600px", width="1000px")
            
    ),
    tabItem(tabName = "sub_1",
            h1("Treemap Dataset", align = "center"),
            DT::dataTableOutput(outputId = "TreemapTable")
    ),
    tabItem(tabName = "sub_2",
            h1("Map Dataset", align = "center")
    ),
    tabItem(tabName = "sub_3",
            h1("Overview Dataset", align = "center")
    )
))

ui <- dashboardPage(title = 'Resale Prices in Singapore from 2012 to 2020', header, sidebar, body, skin='red')

server <- function(input, output) {
  output$Treemap <- renderPlot({
  treemapdata <- filter(realis_summarised, `Year` == input$Year)
  .tm <<- 
    treemap(treemapdata,
            index=c("Planning Region", "HDB Town", "Storey_Level"),
            vSize="Total Unit Sold",
            vColor=input$Plot,
            type="manual",
            palette="Blues",
            title="Resale HDB Prices by Planning Region and Town",
            title.legend = "Average Resale Price(S$ Per Sq. ft)"
    )
  })
  output$TreemapTable <- DT::renderDataTable({
    DT::datatable(data = realis_summarised %>% select(1:10),
                  options = list(pageLength = 25),
                  rownames = FALSE)
  })
  output$ScatterHist <- renderPlot({
    p <- ggplot(Overview, aes(x=`Year-Month`, y=`Unit Area (PSF)`, color=`Unit Area (PSF)`, size=`Unit Area (PSF)`)) +
      geom_point() +
      theme(legend.position="none")
    p3 <- ggMarginal(p, type="boxplot")
    p3
  })
}

shinyApp(ui, server)




