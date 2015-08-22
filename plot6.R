


library(dplyr)
library(ggplot2)
library(gridExtra)

## read NEI
NEI <- readRDS("data/summarySCC_PM25.rds")

## subset to rows from Balitmore City and Los Angeles
NEI_24510<-NEI[NEI$fips=="24510",]
NEI_06037 <- NEI[NEI$fips=="06037",]


## read SCC
SCC <- readRDS("data/Source_Classification_Code.rds")
## subset to those with vehicle exhaust
SCC<-SCC[grep("[Vv]ehicle.*[Ee]xhaust",SCC$Short.Name),]

## join to NEI
MERGED_24510 <- merge(NEI_24510,SCC,by="SCC")
MERGED_06037 <- merge(NEI_06037,SCC,by="SCC")



## set the year as a factor
MERGED_24510$year<- as.factor(as.character(MERGED_24510$year))
MERGED_24510$year<- as.factor(as.character(MERGED_24510$year))

## aggregate emissions by sum of year
aggData_24510 <- aggregate(MERGED_24510$Emissions,by=list(MERGED_24510$year), FUN=sum, na.rm=TRUE)
aggData_06037 <- aggregate(MERGED_06037$Emissions,by=list(MERGED_06037$year), FUN=sum, na.rm=TRUE)



## name them and convert year to a numeric for x axis
colnames(aggData_24510) <- c("Year","Emissions") 
aggData_24510$Year <- as.numeric(as.character(aggData_24510$Year))

colnames(aggData_06037) <- c("Year","Emissions") 
aggData_06037$Year <- as.numeric(as.character(aggData_06037$Year))


## reduce to the common years
aggData_24510<-aggData_24510[aggData_24510$Year>=2002,]


## show the change in nominal terms and also in percentage terms
n<-aggData_24510$Emissions[-1]-aggData_24510$Emissions[1]
n<-append(0,n)
aggData_24510$nominalChange<-n


n<-aggData_06037$Emissions[-1]-aggData_06037$Emissions[1]
n<-append(0,n)
aggData_06037$nominalChange<-n


p<-((aggData_24510$Emissions[-1]-aggData_24510$Emissions[1])/
            aggData_24510$Emissions[1])
p<-append(0,p)
aggData_24510$percentChange<-p


p<-((aggData_06037$Emissions[-1]-aggData_06037$Emissions[1])/
            aggData_06037$Emissions[1])
p<-append(0,p)
aggData_06037$percentChange<-p



## plot
png("plot6.png",width=640,height=480)

plot1<-ggplot(aggData_24510, aes(Year,percentChange)) +
        geom_point() + geom_smooth(method="lm") +
        ylab("Percent Change") +
        ggtitle("24510 percent change 2002 - 2005") +
        ylim(-0.2,0)

plot2<-ggplot(aggData_06037, aes(Year,percentChange)) +
        geom_point() + geom_smooth(method="lm") +
        ylab("Percent Change") +
        ggtitle("06037 percent change 2002 - 2005") +
        ylim(-0.2,0)


plot3<-ggplot(aggData_24510, aes(Year,nominalChange)) +
        geom_point() + geom_smooth(method="lm") +
        ylab("Nominal Change in Tons") +
        ggtitle("24510 nominal change 2002 - 2005") +
        ylim(-10,0)

plot4<-ggplot(aggData_06037, aes(Year,nominalChange)) +
        geom_point() + geom_smooth(method="lm") +
        ylab("Nominal Change in Tons") +
        ggtitle("06037 nominal change 2002 - 2005") +
        ylim(-10,0)


grid.arrange(plot1,plot2,plot3,plot4,ncol=2,nrow=2, top="Percentage and Nominal Change in Vehicle Emissions\nBaltimore City and Los Angeles")


dev.off()