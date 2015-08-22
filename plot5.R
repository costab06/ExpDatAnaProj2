


library(dplyr)
library(ggplot2)

## read NEI
NEI <- readRDS("data/summarySCC_PM25.rds")
## subset to rows from Balitmore City
NEI<-NEI[NEI$fips=="24510",]

## read SCC
SCC <- readRDS("data/Source_Classification_Code.rds")
## subset to those with vehicle exhaust
SCC<-SCC[grep("[Vv]ehicle.*[Ee]xhaust",SCC$Short.Name),]

## join to NEI
MERGED <- merge(NEI,SCC,by="SCC")

## set the year as a factor
MERGED$year<- as.factor(as.character(MERGED$year))

## aggregate emissions by sum of year
aggData <- aggregate(MERGED$Emissions,by=list(MERGED$year), FUN=sum, na.rm=TRUE)

## name them and convert year to a numeric for x axis
colnames(aggData) <- c("Year","Emissions") 
aggData$Year <- as.numeric(as.character(aggData$Year))

## plot
png("plot5.png",width=640,height=480)
g<-ggplot(aggData, aes(Year,Emissions)) + 
        geom_point() + geom_smooth(method="lm") +
        ylab("Emissions in Tons") +
        ggtitle("Emissisons from Motor Vehicle Sources in Baltimore City")


print(g)
dev.off()