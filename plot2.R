

library(dplyr)

## read NEI
NEI <- readRDS("data/summarySCC_PM25.rds")

## subset the NEI
NEI <- filter(NEI, fips == "24510")

## aggregate emissions by year
aggData <- aggregate(NEI$Emissions, by=list(NEI$year),FUN=sum, na.rm=TRUE)

## name them and convert year to numeric for x axis
colnames(aggData) <- c("Year","Emissions")
aggData$Year <- as.numeric(as.character(aggData$Year))


## create a lin reg model
model <- lm(Emissions ~ Year, aggData)

## plot
png("plot2.png",width=640,height=480)
with(aggData, plot(Year,Emissions, 
                   main = "Total PM2.5 Emissions by Year, Baltimore City", 
                   ylab="Total Emissions in Tons"))

abline(model, lwd = 2)
dev.off()
