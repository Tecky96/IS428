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

#-------------------------------Datasets------------------------------#
overview <- read_csv('data/Overview.csv')
realis <- read_csv("data/TreeMap.csv")
Overview <- read_csv("data/Overview1.csv")
Overview_scatter <- read_csv("data/Overview2.csv")
logo <- img(src="Logo
            .png", width=220, height=80, align = "centre")

realis_grouped <- group_by(realis,
                           `Year`,
                           `Planning Region`, `HDB Town`,
                           Storey_Level)

realis_summarised <- summarise(realis_grouped, 
                               `Total Unit Sold` = sum(Sales, na.rm = TRUE),
                               `Total Area` = sum(`Area (SQM)`, na.rm = TRUE),
                               `Average Resale Price` =  mean(`Average Resale Price`,na.rm=TRUE),
                               `Unit Price (PSF)` =  mean(`Unit Area (PSF)`,na.rm=TRUE))

#-------------------------------HEADER DASHBOARD TITLE------------------------------#
Nav_Title <- "Navigation Bar"
header <- dashboardHeader(
  title = logo
)
  

#-------------------------------SIDEBAR OF CONTENTS------------------------------#
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
    menuItem("Dashboard 1", tabName = "dashboard1", icon = icon("dashboard")),
    menuItem("Dashboard 2", tabName = "dashboard2", icon = icon("dashboard")),
    menuItem("Dashboard 3", tabName = "dashboard3", icon = icon("dashboard")),
    menuItem("Dataset", tabName = "Datasets", icon = icon("fas fa-database"),
             menuSubItem("Tree Map Dataset", tabName = "sub_1"), 
             menuSubItem("Map Dataset", tabName = "sub_2"),
             menuSubItem("Overview Dataset", tabName = "sub_3"))
))
#-------------------------------OVERVIEW DASHBOARD------------------------------#
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Overview",
            HTML('<center><img src="cheatsheet.jpg", height=400, width=600></center>'),
            h1("Problem"),
            span(uiOutput("problem"),style="font-family: Tahoma; font-size: 18px;
                 color:grey;"),
            h1("Motivation"),
            span(uiOutput("motivation"),style="font-family: Tahoma; font-size: 18px;
                 color:grey;"),
            h1("Our objectives"),
            span(uiOutput("objective"),style="font-family: Tahoma; font-size: 18px;
                 color:grey;") 
    ),
#-------------------------------DASHBOARD 1: OVERVIEW------------------------------#
    tabItem(tabName = "dashboard1",
            h1("Overview Dashboard", align = "center", style="font-family: Tahoma; font-size: 24px;"),
            plotOutput("Overview1", height="600px", width="1000px"),
            plotOutput("Overview2", height="600px", width="1000px")
    ),
#-------------------------------DASHBOARD 2: TREEMAP------------------------------#
    tabItem(tabName = "dashboard2",
            h1("Dashboard 2 content", align = "center", style="font-family: Tahoma; font-size: 24px;"),
            selectInput("Year", "Select Year:", unique(realis_summarised$`Year`), selected = 2020, multiple = FALSE
                       ),
            radioButtons("Plot", "Choose the visualisation to see:",
                         c("Resale Price" = "Average Resale Price",
                           "Unit Price" = "Unit Price (PSF)"), selected = "Average Resale Price"),
            plotOutput("Treemap",height="700px", width="1000px")
    ),
#-------------------------------DASHBOARD 3: ASPATIAL------------------------------#
    tabItem(tabName = "dashboard3",
            h1("Dashboard 3 content", align = "center", style="font-family: Tahoma; font-size: 24px;"),
            selectInput(inputId = "variable", "Please Select a Year",
                        unique(overview$Year),
                        selected = 2012),
            plotOutput("distPlot"),
            plotOutput("ScatterHist", height="600px", width="1000px")
            
    ),
#-------------------------------DATASET TAB------------------------------#
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

#-------------------------------HTML CONTENT------------------------------#
ui <- dashboardPage(title = 'Resale Prices in Singapore from 2012 to 2020', header, sidebar, body, skin='yellow') #change the look of the dashboard

server <- function(input, output) {
  output$problem <- renderText({
    HTML("There is many online property information that claims to be 'cheat sheets' that could help Singaporeans decide on their desired choice of homes.</br></br>
    In the context of this project, Resale HDBs is the focal point of our project. Choosing a resale HDB has never been easy as there are many factors to consider such as location, HDB type, number of remaining lease years, resale value, etc.</br></br> 
    On top of that, thousands of Resale HDBs transactions are happening each month, making it almost impossible for an owner to get a view of every transaction. 
         Therefore, the majority of buyers and sellers have to consult property agents for their services.")
  })
  
  output$motivation <- renderText({
    HTML("Our team would like to minimise the number of visualisations a prospective buyer would have to see.
    The data visualisations provided were generally overloaded with information, hence, we aim to create concise visualisations surrounding resale HDB trends. 
    It is vital to relay critical information pertaining to yearly sales trends, average resale prices and volume based on floor level.")
  })
  
  output$objective <- renderText({
    HTML("In this project, we aim to deliver a focused and compact visualisation to allow Singaporeans to be well-informed of the average HDBs resale prices around their desired location.
         <ul>
         <li>Overall change in HDB price trends over time by each planning region and by HDB Town</li>
         <li>Comparing price differences for each HDB Town area and planning region given the remaining lease of the HDB flat</li>
         <li>Determine which month had the highest or lowest resale price sold and number of transactions</li>
         <li>Identify the most expensive streets within each Town area given the floor </li></ul>")
  })
  
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
  
  
  output$distPlot <- renderPlot({ 
    map <- filter(overview, Year == input$variable)
    
    ggplot(map, aes(x=`Year-Month`, y=Median, colour = Storey_Level,
                  group = Storey_Level)) +
    geom_line() +
    facet_wrap(~`HDB Town`, scales="free_y") +
    labs(x="\nMonth", y = "Median Resale Price\n", title = "Resale HDB Market Trend by HDB Town 2012 - 2020\n")
  })
}

shinyApp(ui, server)




