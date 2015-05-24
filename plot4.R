setwd("~/A Workspace/exdata-data-NEI_data")
NEI <- readRDS("summarySCC_PM25.rds")
NEI$fipsfact <- factor(as.numeric(NEI$fips))
NEI$sccfact <- factor(NEI$SCC)
NEI$typefact <- factor(NEI$type)
NEI$yearfact <- factor(NEI$year)
balt <- NEI[NEI$fips=="24510",]
SCC <- readRDS("Source_Classification_Code.rds")

SCC[grep("Coal", SCC[,4], ignore.case = TRUE),4]
coalname <- levels(factor(SCC[grep("Coal", SCC[,4], ignore.case = TRUE),4]))
coal <- coalname[grep("coal",coalname,ignore.case = TRUE)]
q <- as.numeric(levels(factor(SCC$SCC[SCC$EI.Sector==coal[1]])))
q <- append(q,as.numeric(levels(factor(SCC$SCC[SCC$EI.Sector==coal[2]]))))
q <- append(q,as.numeric(levels(factor(SCC$SCC[SCC$EI.Sector==coal[3]]))))

func <- function(que, data, year){
  ans <- 0.0
  years <- data[data$yearfact==year,]
  for(i in seq_along(q)){ 
    ##print(sum(years$Emissions[years$SCC==q[i]], na.rm=TRUE))
    ans <- ans + sum(years$Emissions[years$SCC==q[i]], na.rm=TRUE)
  }
  ans
}
cc <- c(func(q, NEI, 1999), func(q, NEI, 2002), func(q, NEI, 2005), func(q, NEI, 2008))
yy <- c(1999, 2002, 2005, 2008)
cf <- data.frame(year = yy, coal = cc)
cf$year <- factor(cf$year)
w <- ggplot(data=cf, aes(x = year, y = coal, color, fill=year))
w + geom_bar(stat="identity")
ggsave("plot4.png", width=6, height=4, dpi=100)
