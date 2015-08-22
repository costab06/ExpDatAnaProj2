

## read NEI
NEI <- readRDS("data/summarySCC_PM25.rds")

## aggregate emissions by year
aggData <- aggregate(NEI$Emissions, by=list(NEI$year),FUN=sum, na.rm=TRUE)

## name them and convert year to numeric for x axis
colnames(aggData) <- c("Year","Emissions")
aggData$Year <- as.numeric(as.character(aggData$Year))

## scale the Emissions
aggData$Emissions<-aggData$Emissions/1000000

## create a lin reg model
model <- lm(Emissions ~ Year, aggData)

## plot
png("plot1.png",width=640,height=480)
with(aggData, plot(Year,Emissions, 
                   main = "Total PM2.5 Emissions Across United States by Year", 
                   ylab="Total Emissions (Millions of Tons)"))
abline(model, lwd = 2)
dev.off()
