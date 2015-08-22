


library(dplyr)
library(ggplot2)


## read NEI
NEI <- readRDS("data/summarySCC_PM25.rds")
## subset to Baltimore City
NEI <- filter(NEI, fips == "24510")

## convert year and type to factor
NEI$type<- as.factor(NEI$type)
NEI$year<- as.factor(as.character(NEI$year))

## aggregate emissions by year and type
aggData <- aggregate(NEI$Emissions,by=list(NEI$year,NEI$type), FUN=sum, na.rm=TRUE)

## assign names and convert to numeric for x axis
colnames(aggData) <- c("Year","type","Emissions") 
aggData$Year <- as.numeric(as.character(aggData$Year))


## plot
png("plot3.png",width=640,height=480)
g<-ggplot(aggData, aes(Year,Emissions)) + 
        geom_point() + geom_smooth(method="lm") + 
        ylab("Emissions in Tons") +
        facet_grid(. ~ type) +
        ggtitle("Emissions by Type for Baltimore City")

print(g)
dev.off()