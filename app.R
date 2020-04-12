#install.packages('rsconnect')
#library(rsconnect)
#rsconnect::setAccountInfo(name='wehouse',
#                          token='44CF7E15C51B46C6199937D081330738',
#                          secret='jBqPDwybIH3N001TSB5UEyoyd82ALl22Rnv/2GXe')


packages = c('treemap', 'tidyverse', 'shiny', 'shinydashboard', 'dplyr', 'ggplot2', 'ggExtra', 'lattice', 'geofacet', 'plotly')

library(treemap)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(lattice)
library(geofacet)
library(plotly)

# for(p in packages){library
#   if(!require(p, character.only = T)){
#     install.packages(p)
#   }
#   library(p, character.only = T)
# }

#-------------------------------Datasets------------------------------#
realis <- read_csv("data/TreeMap.csv")
Overview <- read_csv("data/Overview1.csv")
Overview_scatter <- read_csv("data/Overview2.csv")
select_data <- read_csv('data/Map.csv')
select_data1 <- read_csv('data/sg_planning_area_grid1.csv')

logo <- img(src="weHouse_logo.png", width=220, height=75, align = "centre")


#-------------------------------HEADER DASHBOARD TITLE------------------------------#
Nav_Title <- "Navigation Bar"
header <- dashboardHeader(
  title = logo
)

#-------------------------------SIDEBAR OF CONTENTS------------------------------#
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "Overview", icon = icon("dashboard")),
    menuItem("Overview", tabName = "dashboard1", icon = icon("dashboard")),
    menuItem("Segregated Pricings", tabName = "dashboard2", icon = icon("dashboard")),
    menuItem("HDB Town", tabName = "dashboard3", icon = icon("dashboard"),
             menuSubItem("GeoFacet", tabName = "D3_1"), 
             menuSubItem("Scatterplot", tabName="D3_2")
             ),
    menuItem("Dataset", tabName = "Datasets", icon = icon("fas fa-database"),
             menuSubItem("Tree Map Dataset", tabName = "sub_1"), 
             menuSubItem("Map Dataset", tabName = "sub_2"),
             menuSubItem("Overview Dataset", tabName = "sub_3"))
    )
)

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
            fluidRow(column(2, radioButtons("OverviewPlot", "Choose your plot",
                                            c("Resale Price" = "Resale",
                                              "Unit Price" = "Unit"),
                                            selected="Resale"))), 
            conditionalPanel('input.OverviewPlot=="Resale"', plotlyOutput("LB")),
            conditionalPanel('input.OverviewPlot=="Unit"', plotlyOutput("LB1")),
            conditionalPanel('input.OverviewPlot=="Resale"', plotlyOutput("Trellis")),
            conditionalPanel('input.OverviewPlot=="Unit"', plotlyOutput("Trellis1"))
    ),

#-------------------------------DASHBOARD 2: TREEMAP------------------------------#
    tabItem(tabName = "dashboard2",
            h1("HDB Floor Categories vs. Pricing", align = "center", style="font-family: Tahoma; font-size: 24px;"),
            fluidRow(column(2, selectInput("Year", "Select Year:", 
                                           unique(realis$`Year`), 
                                           selected = 2020, 
                                           multiple = FALSE)),
                     column(2, radioButtons("Plot", "Choose the visualisation to see:",
                             c("Resale Price" = "Average Resale Price",
                               "Unit Price" = "Unit Price (PSF)"), 
                             selected = "Average Resale Price"))),
            conditionalPanel('input.Plot=="Average Resale Price"', plotOutput("Treemap")),
            conditionalPanel('input.Plot=="Unit Price (PSF)"', plotOutput("Treemap1"))
    ),

#-------------------------------DASHBOARD 3: ASPATIAL------------------------------#
    tabItem(tabName = "D3_1",
            h1("GeoFacet of HDB AREA vs Price", align = "center", style="font-family: Tahoma; font-size: 24px;"),
            box(selectInput(inputId = "variable", "Please select a year",
                        unique(select_data$Year),
                        selected = 2012, multiple = FALSE)),
            box(selectInput(inputId = "variable1","Please select a floor level type",
                        unique(select_data$Storey_Level),
                        selected = NULL, multiple = FALSE)),
            plotOutput("distPlot", hover="info", height="700px", width="100%")
    ),

    tabItem(tabName = "D3_2",
        h1("Scatter Plot of Average price vs Area", align = "center", style="font-family: Tahoma; font-size: 24px;"),
        fluidRow(column(2, selectizeInput("scatteryear", "Select your year", 
                       unique(Overview_scatter$Year),
                       multiple = FALSE
                       )),
                 column(2, selectizeInput("HDB", "Select your HDB Town", 
                       unique(Overview_scatter$`HDB Town`), 
                       multiple = FALSE))),
        plotlyOutput("ScatterHist", height="700px")
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
  
#---testing----#
  

  
#---------------------------------------------Dashboard 1---------------------------------------------------#
  output$LB <- renderPlotly({
    Overview %>%
      group_by(Year) %>%
      summarize(Price = median(`Average Resale Price`), Sale = sum(Sales)) %>%
      plot_ly(x = ~Year, y = ~Sale, type = "bar", color = I('darkolivegreen1'), name = "Unit Area (PSF)") %>%
      add_trace(x = ~Year, y = ~Price, type = "scatter", mode="lines", color = I('dark green'), name = "Sales", yaxis='y2') %>%
      layout(title = "Overview of Resale",
             xaxis = list(title = "Year"),
             yaxis = list(side = 'left', title = "Sales Volume", tickformat=',d'),
             yaxis2 = list(side = 'right', overlaying ="y", title = 'Median Resale Price'))
  })
  
  output$LB1 <- renderPlotly({
    Overview %>%
      group_by(Year) %>%
      summarize(Price = median(`Median Resale Price`)/(median(`Area (SQM)`)*10.7639), Sale = sum(Sales)) %>%
      plot_ly(x = ~Year, y = ~Sale, type = "bar", color = I('darkolivegreen1'), name = "Unit Area (PSF)") %>%
      add_trace(x = ~Year, y = ~Price, type = "scatter", mode="lines", color = I('dark green'), name = "Sales", yaxis='y2') %>%
      layout(title = "Overview of Resale",
             xaxis = list(title = "Year"),
             yaxis = list(side = 'left', title = "Sales Volume", tickformat=',d'),
             yaxis2 = list(side = 'right', overlaying ="y", title = 'Unit Price (PSF)'))
  })
  
  output$Trellis <- renderPlotly({
    xplot_data <- Overview %>%
      group_by(Year, Flat_Type) %>%
      summarize(Avg_Resale_Price = median(`Average Resale Price`), Sale = sum(Sales), Unit_Area=mean(`Unit Area (PSF)`))
    p <- ggplot(xplot_data,
                aes(x=Year, y=Avg_Resale_Price, colour=Flat_Type)) +
                geom_line(stat="identity", show.legend=TRUE) +
      theme(legend.position="right")+
      facet_wrap(~Flat_Type)
      labs(y = "Resale Price",
           x = "Year",
           title = "Year Vs Median Resale Price")
    ggplotly(p)
  })
  
  output$Trellis1 <- renderPlotly({
    xplot_data <- Overview %>%
      group_by(Year, Flat_Type) %>%
      summarize(Avg_Resale_Price = median(`Average Resale Price`), Sale = sum(Sales), Unit_Area=mean(`Unit Area (PSF)`))
    p <- ggplot(xplot_data,
                aes(x=Year, y=Unit_Area, colour=Flat_Type)) +
      geom_line(stat="identity", show.legend=TRUE) +
      theme(legend.position="right")+
      facet_wrap(~Flat_Type)
    labs(y = "Resale Price",
         x = "Year",
         title = "Year Vs Unit Area (PSF)")
    ggplotly(p)
  })
  
  #---------------------------------------------Dashboard 2---------------------------------------------------#
  output$Treemap <- renderPlot({
    realis_grouped <- group_by(realis,
                               `Year`,
                               `Planning Region`, `HDB Town`,
                               Storey_Level)
    
    realis_summarised <- summarise(realis_grouped, 
                                   `Total Unit Sold` = sum(Sales, na.rm = TRUE),
                                   `Total Area` = sum(`Area (SQM)`, na.rm = TRUE),
                                   `Average Resale Price` =  mean(`Average Resale Price`,na.rm=TRUE),
                                   `Unit Price (PSF)` =  mean(`Unit Area (PSF)`,na.rm=TRUE))
    treemapdata <- filter(realis_summarised, `Year` == input$Year)
    .tm <<- 
      treemap(treemapdata,
              index=c("Planning Region", "HDB Town", "Storey_Level"),
              vSize="Total Unit Sold",
              vColor=input$Plot,
              type="manual",
              palette="Blues",
              title="Average Resale HDB Prices by Planning Region and Town",
              title.legend = "Average Resale Price"
      )
  })
  
  output$Treemap1 <- renderPlot({
    realis_grouped <- group_by(realis,
                               `Year`,
                               `Planning Region`, `HDB Town`,
                               Storey_Level)
    
    realis_summarised <- summarise(realis_grouped, 
                                   `Total Unit Sold` = sum(Sales, na.rm = TRUE),
                                   `Total Area` = sum(`Area (SQM)`, na.rm = TRUE),
                                   `Average Resale Price` =  mean(`Average Resale Price`,na.rm=TRUE),
                                   `Unit Price (PSF)` =  mean(`Unit Area (PSF)`,na.rm=TRUE))
    treemapdata <- filter(realis_summarised, `Year` == input$Year)
    .tm <<- 
      treemap(treemapdata,
              index=c("Planning Region", "HDB Town", "Storey_Level"),
              vSize="Total Unit Sold",
              vColor=input$Plot,
              type="manual",
              palette="Blues",
              title="Unit Prices by Planning Region and Town",
              title.legend = "Unit Prices(S$ Per Sq. ft)"
      )
  })
  
  output$TreemapTable <- DT::renderDataTable({
    DT::datatable(data = realis_summarised %>% select(1:8),
                  options = list(pageLength = 25),
                  rownames = FALSE)
  })
  
  #---------------------------------------------Dashboard 3---------------------------------------------------#
  
  
  output$ScatterHist <- renderPlotly({
    
    Scatter_data <- aggregate(Overview_scatter[,c(11,13)], list(Overview_scatter$resale_price, Overview_scatter$Year), mean)
    names(Scatter_data)[1] <- "resale_price"
    names(Scatter_data)[2] <- "Year"
    
    #`Unit Price (PSF)` <- Scatter$resale_price/(Scatter$floor_area_sqm*10.7639)
    `Resale Price` <- Scatter_data$resale_price
    `Remaining Lease Years` <- Scatter_data$remaining_lease
    
    `Year` <- Scatter_data$Year
    Scatter <- Scatter_data
    
    
    # if (!input$scatteryear){
    test <- filter(Scatter_data, Scatter_data$Year == input$scatteryear)
    p1 <- subplot(plot_ly(type='box', color=I("indianred2")) %>%
                    add_boxplot(test, x=~`Remaining Lease Years`),
                  plotly_empty(), 
                  plot_ly(test, x=~`Remaining Lease Years`, y=~`Resale Price`, color=I("deepskyblue3")),
                  plot_ly(type='box', color=I("lightseagreen")) %>%
                    add_boxplot(test, y=~`Resale Price`),
                  nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), margin = 0,
                  shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE)
    
    p1
    # }else{
    #   test <- filter(Scatter_data$Year == input$scatteryear)
    # p1 <- subplot(plot_ly(type='box', color=I("indianred2")) %>%
    #                 add_boxplot(test, x=~`Remaining Lease Years`),
    #               plotly_empty(), 
    #               plot_ly(test, x=~`Remaining Lease Years`, y=~`Resale Price`, color=I("deepskyblue3")),
    #               plot_ly(type='box', color=I("lightseagreen")) %>%
    #                 add_boxplot(test, y=~`Resale Price`),
    #               nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), margin = 0,
    #               shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE)
    # 
    # p1
    # }
    # 
                        
  })
  

  output$distPlot <- renderPlot({ 
    map <- filter(select_data, Year == input$variable, Storey_Level == input$variable1)
    
    ggplot(map, aes(x=`Year-Month`, y=`Median Unit Price`)) +
      geom_line() +
      facet_geo(~ Code, grid= select_data1, label = "name") +
      scale_x_discrete(guide = guide_axis(n.dodge =2))+
      scale_x_continuous(breaks = c(1,3,5,7,9,11))+
      labs(title = "Resale HDB Market Trend by HDB Town\n\n",
           x = "Month\n\n\n",
           y ="Median Unit Price\n\n\n") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                     scientific = FALSE)) +
      theme(plot.title = element_text(color = "black",size = 30, face = "bold.italic"),
            axis.title.x = element_text(color = "black", size = 15, face ="bold"),
            axis.title.y = element_text(color = "black", size = 15, face = "bold"),
            strip.background = element_rect((fill = "#2E8B57")),
            strip.text = element_text(color = 'white', size= 7, face = "bold"),
            panel.border = element_rect(colour = "black", fill = NA, size = 0.2))
  })
}

shinyApp(ui, server)




