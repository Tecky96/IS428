
packages = c('treemap', 'tidyverse', 'shiny', 'shinydashboard', 'dplyr', 'ggplot2')

for(p in packages){library
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

Overview <- read_csv("data/Overview1.csv")

realis <- read_csv("data/TreeMap.csv")

realis_grouped <- group_by(realis,
                           `Year`,
                           `Planning Region`, `HDB Town`,
                           Storey_Level)

realis_summarised <- summarise(realis_grouped, 
                               `Total Unit Sold` = sum(Sales, na.rm = TRUE),
                               `Total Area` = sum(`Area (SQM)`, na.rm = TRUE),
                               `Average Resale Price` =  median(`Resale Price`,na.rm=TRUE),
                               `Unit Price (PSF)` =  mean(`Area Unit (PSF)`,na.rm=TRUE))


treemap(realis_summarised,
        index=c("Planning Region", "town", "street_name"),
        vSize="Total Unit Sold",
        vColor="Average Resale Price",
        type="manual",
        palette="Blues",
        title="Resale HDB Prices by Planning Region and Town",
        title.legend = "Average Resale Price(S$ per sq. m)"
)
