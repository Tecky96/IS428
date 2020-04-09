#install.packages('rsconnect')
#library(rsconnect)
#rsconnect::setAccountInfo(name='wehouse',
#                          token='44CF7E15C51B46C6199937D081330738',
#                          secret='jBqPDwybIH3N001TSB5UEyoyd82ALl22Rnv/2GXe')

packages = c('treemap', 'tidyverse', 'shiny', 'shinydashboard', 'dplyr', 'ggplot2', 'ggExtra', 'lattice')

for(p in packages){library
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p, character.only = T)
}

#Datasets
realis <- read_csv("data/TreeMap.csv")
Overview <- read_csv("data/Overview1.csv")
Overview_scatter <- read_csv("data/Overview2.csv")


realis_grouped <- group_by(realis,
                           `Year`,
                           `Planning Region`, `HDB Town`,
                           Storey_Level)

realis_summarised <- summarise(realis_grouped, 
                               `Total Unit Sold` = sum(Sales, na.rm = TRUE),
                               `Total Area` = sum(`Area (SQM)`, na.rm = TRUE),
                               `Average Resale Price` =  mean(`Average Resale Price`,na.rm=TRUE),
                               `Unit Price (PSF)` =  mean(`Unit Area (PSF)`,na.rm=TRUE))

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
            h1("Overview Dashboard", align = "center"),
            plotOutput("Overview1", height="600px", width="1000px"),
            plotOutput("Overview2", height="600px", width="1000px")
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
    DT::datatable(data = realis_summarised %>% select(1:8),
                  options = list(pageLength = 25),
                  rownames = FALSE)
  })
  
  output$ScatterHist <- renderPlot({

    Scatter <- aggregate(Overview_scatter[,c(11,13)], list(Overview_scatter$resale_price), mean)
    names(Scatter)[1] <- "resale_price"
    `Unit Price (PSF)` <- Scatter$resale_price/(Scatter$floor_area_sqm*10.7639)
    `Resale Price` <- Scatter$resale_price
    `Remaining Lease Years` <- Scatter $remaining_lease
    p1 <- ggplot(Scatter,
                 aes(y = `Resale Price`, x = `Remaining Lease Years`)) +
                 geom_point(aes(color = `Unit Price (PSF)`))
    p2 <- ggMarginal(p1, type="boxplot")
    p2
  })
  
  output$Overview1 <- renderPlot({
    LineBar <- aggregate(Overview[,c(3,4,5,6,8,9)], list(Overview$`Year`), mean)
    LineBar1 <- aggregate(Overview[,c(3,4,5,6,8,9)], list(Overview$`Year`), mean)
    names(LineBar)[1] <- "Year"
    ggplot(LineBar)  + 
      geom_bar(aes(x=Year, y=`Unit Area (PSF)`),stat="identity", fill="tan1", colour="sienna3")+
      geom_line(aes(x=Year, y=Sales),stat="identity")+
      geom_text(aes(label=Sales, x=Year, y=Sales), colour="black")
  })
  
  output$Overview2 <- renderPlot({
    xplot <- aggregate(Overview[,c(3,4,5,6,8,9)], by = list(Overview$Flat_Type, Overview$`Year`), mean)
    names(xplot)[1] <- "Flat_Type"
    names(xplot)[2] <- "Year"
    xyplot(`Unit Area (PSF)` ~ Year |Flat_Type, data = xplot, type = "l", pch=19, layout=c(4,2),
           main = "Room Type Resale Trends", ylab = "Average Resale Price", xlab = "Year")
  })
  
}

shinyApp(ui, server)




