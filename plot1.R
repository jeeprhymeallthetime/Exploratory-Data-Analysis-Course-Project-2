setwd("~/A Workspace/exdata-data-NEI_data")
NEI <- readRDS("summarySCC_PM25.rds")
NEI$fipsfact <- factor(as.numeric(NEI$fips))
NEI$sccfact <- factor(NEI$SCC)
NEI$typefact <- factor(NEI$type)
NEI$yearfact <- factor(NEI$year)

plot(aggregate(NEI$Emissions, by=list(NEI$year), sum), pch=19, xlab="Year", ylab="Total Emissions") #Q2
dev.off()
png("plot1.png")
plot(aggregate(NEI$Emissions, by=list(NEI$year), sum), pch=19, xlab="Year", ylab="Total Emissions") #Q2
dev.off()









plot(aggregate(NEI$Emissions[NEI$fips=="24510"], by=list(NEI$year[NEI$fips=="24510"]), sum), pch=19, xlab="Year", 
     ylab="24510 Emissions") #Q2, with the below being my justification for my answer for Q2
balt <- NEI[NEI$fips=="24510",]
un <- sd(balt[balt$year=="1999",4],na.rm=TRUE)
deux <- sd(balt[balt$year=="2002",4],na.rm=TRUE)
trois <- sd(balt[balt$year=="2005",4],na.rm=TRUE)
quatre <- sd(balt[balt$year=="2008",4],na.rm=TRUE)

library(ggplot2)

count = 1
for(i in levels(balt$typefact)){
  print(i)
  print(aggregate(balt$Emissions[balt$typefact==i], by=list(balt$year[balt$typefact==i]), sum))
  x <- aggregate(balt$Emissions[balt$typefact==i], by=list(balt$year[balt$typefact==i]), sum)
  x$third <-  c(i,i,i,i)
  if(count == 1){
    tf <- x
  }
  else {tf <- rbind(tf, x)}
  count = count + 1
}
colnames(tf) <- c("year","Emissions","type")
tf$year <- factor(tf$year)
tf$type <- factor(tf$type)

d <- ggplot(data=na.omit(tf), aes(x=year, y=Emissions, color, fill=year), rm.na=TRUE)
d + facet_wrap( ~ type, scales="free") + geom_bar(stat="identity")



SCC[grep("Coal", SCC[,3], ignore.case = TRUE),3]
coalname <- levels(factor(SCC[grep("Coal", SCC[,3], ignore.case = TRUE),4]))
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
w <- ggplot(data=cf, aes(x = year, y = coal))
w + geom_bar(stat="identity")

for(i in seq_along(q)){ 
  print(q[i])
}

##print(aggregate(balt$Emissions[balt$typefact==i], by=list(balt$year[balt$typefact==i]), sum))




