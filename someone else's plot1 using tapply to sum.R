## ----------1. Read of the data files
data <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

data$year <- as.factor(data$year)

## -----------2. Calculation of the total emissions in Baltimore City

png(filename = "plot2.png", width = 480, height = 480, units = "px")

balt <- subset.data.frame(data, data$fips=="24510")
result2 <- tapply(balt$Emissions, balt$year, sum)
barplot(result2, col="yellow", ylab="Emissions of PM2.5 (tons)", main="Evolution of emissions of PM2.5 in Baltimore")

dev.off()