setwd("~/A Workspace/exdata-data-NEI_data")
NEI <- readRDS("summarySCC_PM25.rds")
NEI$fipsfact <- factor(as.numeric(NEI$fips))
NEI$sccfact <- factor(NEI$SCC)
NEI$typefact <- factor(NEI$type)
NEI$yearfact <- factor(NEI$year)
SCC <- readRDS("Source_Classification_Code.rds")
SCC[grep("mobile", SCC[,4], ignore.case = TRUE),4]
balt <- NEI[NEI$fips=="24510",]
motorname <- levels(factor(SCC[grep("mobile", SCC[,4], ignore.case = TRUE),4]))
motor <- motorname[grep("mobile",motorname,ignore.case = TRUE)]

q <- levels(factor(SCC$SCC[SCC$EI.Sector==motor[1]]))
q <- append(q,(levels(factor(SCC$SCC[SCC$EI.Sector==motor[2]]))))
q <- append(q,(levels(factor(SCC$SCC[SCC$EI.Sector==motor[3]]))))
q <- append(q,(levels(factor(SCC$SCC[SCC$EI.Sector==motor[4]]))))
q <- append(q,(levels(factor(SCC$SCC[SCC$EI.Sector==motor[5]]))))
q <- append(q,(levels(factor(SCC$SCC[SCC$EI.Sector==motor[6]]))))
q <- append(q,(levels(factor(SCC$SCC[SCC$EI.Sector==motor[7]]))))
q <- append(q,(levels(factor(SCC$SCC[SCC$EI.Sector==motor[8]]))))
q <- append(q,(levels(factor(SCC$SCC[SCC$EI.Sector==motor[9]]))))
q <- append(q,(levels(factor(SCC$SCC[SCC$EI.Sector==motor[10]]))))


func <- function(que, data, year){
  ans <- 0.0
  years <- data[data$yearfact==year,]
  for(i in seq_along(q)){ 
    #print(sum(years$Emissions[years$SCC==q[i]], na.rm=TRUE))
    ans <- ans + sum(years$Emissions[years$SCC==q[i]], na.rm=TRUE)
  }
  ans
}

cc <- c(func(q, balt, 1999), func(q, balt, 2002), func(q, balt, 2005), func(q, balt, 2008))
yy <- c(1999, 2002, 2005, 2008)
cf <- data.frame(year = yy, motor = cc)
cf$year <- factor(cf$year)
w <- ggplot(data=cf, aes(x = year, y = motor, color, fill=year))
w + geom_bar(stat="identity")
ggsave("plot5.png", width=6, height=4, dpi=100)

