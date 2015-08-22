


library(dplyr)
library(ggplot2)



## read NEI
NEI <- readRDS("data/summarySCC_PM25.rds")

## read SCC
SCC <- readRDS("data/Source_Classification_Code.rds")
## subset to those with coal
SCC<-SCC[grep("[Cc]oal",SCC$Short.Name),]

## join to NEI
MERGED <- merge(NEI,SCC,by="SCC")

## set the year as a factor
MERGED$year<- as.factor(as.character(MERGED$year))

## aggregate emissions by sum of year
aggData <- aggregate(MERGED$Emissions,by=list(MERGED$year), FUN=sum, na.rm=TRUE)

## name them and convert year to a numeric for x axis
colnames(aggData) <- c("Year","Emissions") 
aggData$Year <- as.numeric(as.character(aggData$Year)) 
 
png("plot4.png",width=640,height=480)
g<-ggplot(aggData, aes(Year,Emissions)) + 
        geom_point() + geom_smooth(method="lm") +
        ylab("Emissions in Tons") +
        ggtitle("Emissions from Coal-related Sources Across United States")


print(g)
dev.off()