
packages = c('treemap','tidyverse', 'shiny', 'shinydashboard', 'dplyr', 'ggplot2', 'devtools', 'ggplotly')

for(p in packages){library
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}
install_github("hafen/geofacet")
install_github("shiny", "rstudio")
install_github('hadley/ggplot2')

Overview2 <- read_csv("data/Overview2.csv")
View(Overview2)

Overview <- read_csv("data/Overview1.csv")
LineBar <- aggregate(Overview[,c(3,4,5,6,8,9)], list(Overview$`Year`), mean)
names(LineBar)[1] <- "Year"

xplot <- aggregate(Overview[,c(3,4,5,6,8,9)], by = list(Overview$Flat_Type, Overview$`Year`), mean)
names(xplot)[1] <- "Year"
names(xplot)[2] <- "Flat_Type"
head(xplot)
view(xplot)

xplot <- aggregate(Overview, by = list(Overview$`Year`, Overview$Flat_Type), FUN = mean)


#Supposed 
p <- ggplot(Overview, aes(x=`Year-Month`, y=`Unit Area (PSF)`, color=`Unit Area (PSF)`, size=`Unit Area (PSF)`)) +
  geom_point() +
  theme(legend.position="none")
p3 <- ggMarginal(p, type="boxplot")
p3


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
