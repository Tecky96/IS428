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

#-------------------Commented out for Deployment Purpose-----------------------------------#
for(p in packages){library
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

#-------------------------------Datasets------------------------------#

Overview <- read_csv("data/Overview1.csv")
realis <- read_csv("data/TreeMap.csv")
select_data <- read_csv('data/Map.csv')
select_data1 <- read_csv('data/sg_planning_area_grid1.csv')
Overview_scatter <- read_csv("data/Scatter.csv")

logo <- img(src="WeHouse_Logo.png", width=150, height=75, align = "centre")


#-------------------------------HEADER DASHBOARD TITLE------------------------------#
Nav_Title <- "Navigation Bar"
header <- dashboardHeader(
  title = logo
)

#-------------------------------SIDEBAR OF CONTENTS------------------------------#
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "Overview", icon = icon("fas fa-caret-square-right")),
    menuItem("Overview", tabName = "dashboard1", icon = icon("fas fa-eye")),
    menuItem("Segregated Pricings", tabName = "dashboard2", icon = icon("fas fa-eye")),
    menuItem("HDB Town", tabName = "dashboard3", icon = icon("fas fa-eye"),
             menuSubItem("Geofacet", tabName = "D3_1"), 
             menuSubItem("Scatterplot", tabName="D3_2")
             ),
    menuItem("Dataset", tabName = "Datasets", icon = icon("fas fa-database"),
             menuSubItem("Overview Dataset", tabName = "sub_1"), 
             menuSubItem("Treemap Dataset", tabName = "sub_2"),
             menuSubItem("Geofacet Dataset", tabName = "sub_3"),
             menuSubItem("Scatter Dataset", tabName = "sub_4"))
    )
)

#-------------------------------OVERVIEW DASHBOARD------------------------------#

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Overview",
            div(tags$img(src='Cheatsheet.jpg', height=300, width=400, align="center"), style="text-align: center;"),
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
            fluidRow(column(12,h1("Overview", align = "center", style="font-family: Tahoma; font-size: 24px;</br>")),
                     column(1, radioButtons("OverviewPlot", "Choose Your Plot",
                                               c("Resale Price" = "Resale",
                                                 "Unit Price" = "Unit"),
                                               selected="Resale")),
                     column(11,conditionalPanel('input.OverviewPlot=="Resale"', plotlyOutput("LB"))), 
                     column(11,conditionalPanel('input.OverviewPlot=="Unit"', plotlyOutput("LB1"))),
                     column(11,conditionalPanel('input.OverviewPlot=="Resale"', plotlyOutput("Trellis")), offset = 1),
                     column(11,conditionalPanel('input.OverviewPlot=="Unit"', plotlyOutput("Trellis1")), offset = 1))
                     
    ),

#-------------------------------DASHBOARD 2: TREEMAP------------------------------#
    tabItem(tabName = "dashboard2",
            fluidRow(column(12,h1("HDB Floor Categories vs Pricing", align = "center", style="font-family: Tahoma; font-size: 24px;")),
                     sidebarPanel(sliderInput(
                                               inputId = "Year", 
                                               label = "Select Year:", 
                                               min = min(unique(realis$`Year`)),
                                               max = max(unique(realis$`Year`)), 
                                               value = 2020, 
                                               sep = "",
                                               animate = animationOptions(loop = TRUE)),
                                  
                                  radioButtons("Plot", "Choose Your Plot:",
                                               c("Resale Price" = "Average Resale Price",
                                                 "Unit Price" = "Unit Price (PSF)"), 
                                               selected = "Average Resale Price"),width=2),
                     
                     column(10,conditionalPanel('input.Plot=="Average Resale Price"', plotOutput("Treemap", height="700px"))),
                     column(10,conditionalPanel('input.Plot=="Unit Price (PSF)"', plotOutput("Treemap1", height="700px"))))
    ),

#-------------------------------DASHBOARD 3: ASPATIAL------------------------------#
    tabItem(tabName = "D3_1",
            
            fluidRow(
              column(12, h1("Geofacet of HDB AREA vs Median Price", align = "center", style="font-family: Tahoma; font-size: 24px;")),
              sidebarPanel(sliderInput(inputId = "variable", 
                                     label = "Select a Year",
                                     min = min(unique(select_data$Year)),
                                     max = max(unique(select_data$Year)), 
                                     value = 2012, 
                                     sep = "",
                                     animate = animationOptions(loop = TRUE)),
                         selectInput(inputId = "variable1","Select a Floor Level Category",
                                     unique(select_data$Storey_Level),
                                     selected = NULL, multiple = FALSE),
                         width=2),
              column(10, plotOutput("distPlot", hover="info", height="700px", width="100%"))
            
    )),

    tabItem(tabName = "D3_2",
            
        fluidRow(column(12,h1("Resale Price Vs Remaining Lease Years", align = "center", style="font-family: Tahoma; font-size: 24px;")),
                 
                 sidebarPanel(selectizeInput(inputId = "scatteryear", 
                                            label = "Select a Year", 
                                            choices = c("Select All", unique(Overview_scatter$Year)),
                                            multiple = FALSE,
                                            selected = "Select All"),
                              
                              selectizeInput(inputId = "HDB", 
                                          label = "Select a HDB Town", 
                                          choices = c("Select All",unique(Overview_scatter$`HDB Town`)), 
                                          selected = "Select All",
                                          multiple = FALSE),
                              
                              selectizeInput(inputId = "FLAT", 
                                             label = "Select a Flat Type", 
                                             choices = c("Select All",unique(Overview_scatter$Flat_Type)), 
                                             selected = "Select All",
                                             multiple = FALSE), width=2),
                 
                 column(10, plotlyOutput("ScatterHist", height="700px"))
                 
)),

#-------------------------------DATASET TAB------------------------------#
    tabItem(tabName = "sub_1",
            h1("Overview Dataset", align = "center"),
            DT::dataTableOutput(outputId = "Overview")
    ),
    tabItem(tabName = "sub_2",
            h1("Treemap Dataset", align = "center"),
            DT::dataTableOutput(outputId = "TreemapTable")
    ),
    tabItem(tabName = "sub_3",
            h1("Geofacet Dataset", align = "center"),
            DT::dataTableOutput(outputId = "Geofacet")
    ),
    tabItem(tabName = "sub_4",
        h1("Scatter Dataset", align = "center"),
        DT::dataTableOutput(outputId = "Scatter")
    )

))

#-------------------------------HTML CONTENT------------------------------#
ui <- dashboardPage(title = 'Resale Prices in Singapore from 2012 to 2020', header, sidebar, body, skin='yellow') #change the look of the dashboard

server <- function(input, output) {
  
  output$problem <- renderText({
    
    HTML("There are many online property information that claims to be 'cheat sheets' that could help Singaporeans decide on their desired choice of homes.
    In the context of this project, Resale HDBs is the focal point of our project. Choosing a resale HDB has never been easy as there are many factors to consider such as location, HDB type, number of remaining lease years, resale value, etc.</br></br> 
    On top of that, thousands of Resale HDBs transactions are happening each month, making it almost impossible for an owner to get a view of every transaction. 
         Therefore, the majority of buyers and sellers have to consult property agents for their services.</br>")
  
    })
  
  output$motivation <- renderText({
    
    HTML("Our team would like to minimise the number of visualisations a prospective buyer would have to see.
    The data visualisations provided were generally overloaded with information, hence, we aim to create concise visualisations surrounding resale HDB trends. 
    It is vital to relay critical information pertaining to yearly sales trends, average resale prices and volume based on floor level.</br>")
  
    })
  
  output$objective <- renderText({
    
    HTML("In this project, we aim to deliver a focused and compact visualisation to allow Singaporeans to be well-informed of the average HDBs resale prices around their desired location.
         <ul>
         <li>Comparing and identifying the change in median per unit price trends of resale HDB Town across different months within the selected year and floor level</li>
         <li>Comparing the prices of the resale flats within individual HDB Town area based on their remaining lease year of the HDB flat</li>
         <li>Determine which year had the highest or lowest resale price sold and number of transactions</li>
         <li>Identify the most expensive town given the flat type category</li></ul>")
  
    })
#--------------------image-----------------------#
  output$image <- renderUI({
    tags$img(src = "WeHouse_Logo.jpg", width=400, height=300)
  })
  
#---------------------------------------------Dashboard 1---------------------------------------------------#
  output$LB <- renderPlotly({
    
    Overview %>%
      group_by(Year) %>%
      summarize(Price = median(`Median Resale Price`), Sale = sum(Sales)) %>%
      plot_ly(x = ~Year, y = ~Sale, type = "bar", color = I('indianred3'), name = "Sales", hovertemplate = '<b>Year</b>: %{x}<br><b>Sales</b>: %{y}<extra></extra>') %>%
      add_trace(x = ~Year, y = ~Price, type = "scatter", mode="lines", color = I('lightgreen'), name = "Median Resale Price", yaxis='y2', 
                hovertemplate = '<b>Year</b>: %{x}<br><b>Median Resale Price</b>: %{y:$.0f}<extra></extra>') %>%
      layout(title = "Overview of HDB Resale Median Price and Volume",
             xaxis = list(title = "Year"),
             yaxis = list(side = 'left', title = "Sales Volume", tickformat=',d'),
             yaxis2 = list(side = 'right', overlaying ="y", title = 'Median Resale Price'))%>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
  })
  
  output$LB1 <- renderPlotly({
    
    Overview %>%
      group_by(Year) %>%
      summarize(Price = median(`Median Resale Price`)/(median(`Area (SQM)`)*10.7639), Sale = sum(Sales)) %>%
      plot_ly(x = ~Year, y = ~Sale, type = "bar", color = I('indianred3'), name = "Sales", hovertemplate = '<b>Year</b>: %{x}<br><b>Sales</b>: %{y}<extra></extra>') %>%     
      add_trace(x = ~Year, y = ~Price, type = "scatter", mode="lines", color = I('lightgreen'), name = "Unit Price (PSF)", yaxis='y2',
                hovertemplate = '<b>Year</b>: %{x}<br><b>Unit Price (PSF)</b>: %{y:$.0f}<extra></extra>') %>%
      layout(title = "Overview of Resale HDB Unit Price (PSF) and volume",
             xaxis = list(title = "Year"),
             yaxis = list(side = 'left', title = "Sales Volume ", tickformat=',d'),
             yaxis2 = list(side = 'right', overlaying ="y", title = 'Unit Price (PSF)'))%>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
  })
  
  output$Trellis <- renderPlotly({
    
    xplot_data <- Overview %>%
      group_by(Year, Flat_Type) %>%
      summarize(`Median Resale Price` = median(`Average Resale Price`), Sale = sum(Sales), `Unit Price (PSF)`=mean(`Unit Area (PSF)`))
    
    p <- ggplot(xplot_data,
                aes(x=Year, y=`Median Resale Price`, colour=Flat_Type),
                text = paste("Year:", Year,
                             "<br>Median Resale Price: $", `Median Resale Price`,
                             "<br>Housing Type:", Flat_Type)) +
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
      summarize(`Median Resale Price` = median(`Average Resale Price`), Sale = sum(Sales), `Unit Price (PSF)`=mean(`Unit Area (PSF)`))
    
    p <- ggplot(xplot_data,
                aes(x=Year, y=`Unit Price (PSF)`, colour=Flat_Type),
                text = paste("Year:", Year,
                             "<br>Unit Price (PSF): $", `Unit Price (PSF)`,
                             "<br>Housing Type:", Flat_Type)) +
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
    
    tm <- 
      treemap(treemapdata,
              index=c("Planning Region", "HDB Town", "Storey_Level"),
              vSize="Total Unit Sold",
              vColor=input$Plot,
              type="manual",
              palette="Blues",
              title="Average Resale HDB Prices by Planning Region and Town",
              title.legend = "Average Resale Price ($)"
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
  Overview <- read_csv("data/Overview1.csv")
  realis <- read_csv("data/TreeMap.csv")
  select_data <- read_csv('data/Map.csv')
  select_data1 <- read_csv('data/sg_planning_area_grid1.csv')
  Overview_scatter <- read_csv("data/Scatter.csv")
#--------------------------Datasets------------------------------------------#  
  output$Overview <- DT::renderDataTable({
    DT::datatable(data = Overview %>% select(1:8),
                  options = list(pageLength = 25),
                  rownames = FALSE)
  })
  output$TreemapTable <- DT::renderDataTable({
    DT::datatable(data = realis %>% select(1:9),
                  options = list(pageLength = 25),
                  rownames = FALSE)
  })
  output$Geofacet <- DT::renderDataTable({
    DT::datatable(data = select_data %>% select(1:18),
                  options = list(pageLength = 25),
                  rownames = FALSE)
  })
  output$Scatter <- DT::renderDataTable({
    DT::datatable(data = Overview_scatter %>% select(1:14),
                  options = list(pageLength = 25),
                  rownames = FALSE)
  })
  
  #---------------------------------------------Dashboard 3---------------------------------------------------#

  output$ScatterHist <- renderPlotly({
    if ("Select All" %in% input$scatteryear){
      Year_filter <- Overview_scatter
    } else{
      Year_filter <- filter(Overview_scatter, Year == input$scatteryear)
    }
    if ("Select All" %in% input$HDB){
      HDB_Town <- Year_filter
    } else {
      HDB_Town <- filter(Year_filter, `HDB Town` == input$HDB)
    }
    if ("Select All" %in% input$FLAT){
      Flat <- HDB_Town
    } else {
      Flat <- filter(HDB_Town
                     , Flat_Type == input$FLAT)
    }

    Scatter_data <- aggregate(Flat[,c(11,13)], list(Flat$resale_price), mean)
    names(Scatter_data)[1] <- "resale_price"

    `Resale Price` <- Scatter_data$resale_price
    `Remaining Lease Years` <- Scatter_data$remaining_lease
  
    p1 <- subplot(plot_ly(type='box',
                          color=I("indianred2"),
                          name="Remaining Lease Years") %>%
                    add_boxplot(Scatter_data, type='box',
                                x=~`Remaining Lease Years`),
                  plotly_empty(type = "scatter"),
                  plot_ly(Scatter_data,
                          type="scatter",
                          x=~`Remaining Lease Years`,
                          y=~`Resale Price`,
                          mode   = 'markers',
                          hovertemplate = '<b>Remaining Lease Years</b>:%{x}<br><b>Resale Price</b>: $%{y}',
                          name = "Resale Price vs Remaining Lease Years",
                          color=I("deepskyblue3")) %>%
                    layout(
                           xaxis = list(title = "Remaining Lease Years",
                                      zeroline = TRUE),
                           yaxis = list(title = "Resale Price",
                                        zeroline = TRUE)),
                  plot_ly(type='box',
                          color=I("lightseagreen"),
                          name="Resale Price") %>%
                    add_boxplot(Scatter_data, type='box',
                                y=~`Resale Price`),
                  nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), margin = 0,
                  shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE)
    p1

  })
  

  output$distPlot <- renderPlot({ 
    map <- filter(select_data, Year == input$variable, Storey_Level == input$variable1)
    ggplot(map, aes(x=`Year-Month`, y=`Median Unit Price`)) +
      geom_line() +
      facet_geo(~ Code, grid= select_data1, label = "name") +
      scale_x_discrete(guide = guide_axis(n.dodge =2))+
      scale_x_continuous(breaks = c(1,3,5,7,9,11))+
      labs(x = "January to December\n\n\n",
           y ="Median Unit Price\n\n\n") +
      ggtitle("Resale HDB Market Trend by HDB Town")+
      theme(plot.title = element_text(hjust = 0.5))+
      scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                     scientific = FALSE)) +
      theme(plot.title = element_text(color = "black",size = 20, face = "bold.italic"),
            axis.title.x = element_text(color = "black", size = 15, face ="bold"),
            axis.title.y = element_text(color = "black", size = 15, face = "bold"),
            strip.background = element_rect((fill = "#2E8B57")),
            strip.text = element_text(color = 'white', size= 7, face = "bold"),
            panel.border = element_rect(colour = "black", fill = NA, size = 0.2))
  })
}

shinyApp(ui, server)




