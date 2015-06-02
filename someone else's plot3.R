## Set your working directory to the folder where the data file is to be located
## using the "setwd" function 

#Setting the URL and download the file
url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(url,"data.zip")

#uncompress the downloaded file
unzip("data.zip")

library(ggplot2)
# Reading the data
NEI <- readRDS("summarySCC_PM25.rds")

#Filter out Baltimore City,Maryland records
dtBAL <- NEI[NEI$fips=="24510", ]

##Plotting the chart and export to PNG
g <- ggplot(dtBAL, aes(year, Emissions, color = type))
g <- g + geom_line(stat = "summary", fun.y = "sum") +
  ylab(expression("Emissions (Tons)")) +
  ggtitle("Total Emissions of PM2.5 in Baltimore City\n from 1999 to 2008")
ggsave("plot3.png",width=4.8, height=4.8, dpi=100)