setwd("/home/tsa/Rcoursera")

# We are creating the main data.frames
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# --------------------------------------------------------------------------------------
# QUESTION 1: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources
# for each of the years 1999, 2002, 2005, and 2008.

# We are loading the "dplyr" (piping will be used)
library(dplyr)

# We are grouping the data per year and summarizing the Emissions per year
sum.total <- NEI %>% 
        group_by(year) %>%
        summarize(sum.total = sum(Emissions))

# Creating the plot and printing the png
png(filename = "plot1.png")
plot(sum.total, type= "o", col="red", pch=19, lty=2, ylim = c(0, max(sum.total[,2])), xaxt="n", xlab="Year", ylab="Total Emissions (tons)")
axis(1, at=c(1999, 2002, 2005, 2008), labels=c(1999, 2002, 2005, 2008))
title(main = "Total Annual Emissions in the USA by Year")
dev.off()

# ATTENTION: The number of observations we are interested in varies considerably from year to year.
# So, it would be wiser to use their mean values than their sums.



# --------------------------------------------------------------------------------------
# QUESTION 2: Have total emissions from PM2.5 decreased in the Baltimore City, baltimore (Fips == "24510")
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.

# We are selecting only the data for Baltimore City, grouping the data and summarizing the Emissions by year
sum.baltimore <- NEI %>%
        filter(fips == "24510") %>%
        group_by(year) %>%
        summarize(sum.total = sum(Emissions))

# Creating the plot and printing the png
png(filename = "plot2.png")
plot(sum.baltimore, type= "o", col="red", pch=19, lty=2, ylim = c(0, max(sum.baltimore[,2])), xaxt="n", xlab="Year", ylab="Total Emissions (tons)")
axis(1, at=c(1999, 2002, 2005, 2008), labels=c(1999, 2002, 2005, 2008))
title(main = "Total Annual Emissions in Baltimore City by Year")
dev.off()

# ATTENTION: The number of observations we are interested in varies considerably from year to year.
# So, it would be wiser to use their mean values than their sums.



# --------------------------------------------------------------------------------------
# QUESTION 3: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008?
# Use the ggplot2 plotting system to make a plot answer this question.


# We are selecting only the data for Baltimore City, grouping the data and summarizing the Emissions by type and year
sum.types.baltimore <- NEI %>%
        filter(fips == "24510") %>%
        group_by(type, year) %>%
        summarize(sum.total = sum(Emissions))

# We are loading the ggplot2 library
library(ggplot2)

# We are calculating the plot
plot_sum.types.baltimore <- ggplot(data = sum.types.baltimore, aes(year, sum.total)) +
        geom_point(color = "red",
                   size = 3,
                   alpha = .6) +
        facet_grid(. ~ type) +
        xlab("Year") +
        ylab("Total Emissions [Tons]") +
        ggtitle("Total Annual Emissions in Baltimore City by Type and Year")


# Creating the png
png(filename = "plot3.png")
plot_sum.types.baltimore
dev.off()

# ATTENTION: The number of observations we are interested in varies considerably from year to year.
# So, it would be wiser to use their mean values than their sums.



# --------------------------------------------------------------------------------------
# QUESTION 4: Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?


# From the SCC data.frame, we are saving the SCC-codes corrresponding to coal combustion-related sources
coal.scc <- SCC[grep("coal", SCC$EI.Sector, ignore.case = TRUE),1]
# From the NEI data.frame, we are selecting the rows with these SCC-codes
NEI.coal <- NEI[which(NEI[,2] %in% coal.scc),]
# We are grouping the data for the USA and summarizing the coal combustion-related Emissions by year
sum.coal.total <- NEI.coal %>%
        group_by(year) %>%
        summarize(sum.total = sum(Emissions))

# Creating the plot and printing the png (enlarging width)
png(filename = "plot4.png", width = 700)
plot(sum.coal.total, type= "o", col="red", pch=19, lty=2, xaxt="n", xlab="Year", ylab="Total Coal combustion Emissions (tons)")
axis(1, at=c(1999, 2002, 2005, 2008), labels=c(1999, 2002, 2005, 2008))
title(main = "Total Annual Emissions in the USA from coal combustion-related sources")
dev.off()

# ATTENTION: The number of observations we are interested in varies considerably from year to year.
# So, it would be wiser to use their mean values than their sums.



# --------------------------------------------------------------------------------------
# QUESTION 5: How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

# From the SCC data.frame, we are saving the SCC-codes corrresponding to motor vehicle sources
vehicles.scc <- SCC[grep("veh", SCC$Short.Name, ignore.case = TRUE),1]
# From the NEI data.frame, we are selecting the rows with these SCC-codes
NEI.vehicles <- NEI[which(NEI[,2] %in% vehicles.scc),]
# We are selecting only the data for Baltimore City, grouping the data and summarizing the Emissions by year
sum.vehicles.baltimore <- NEI.vehicles %>%
        filter(fips == "24510") %>%
        group_by(year) %>%
        summarize(sum.vehicle.baltimore = sum(Emissions))

# Creating the plot and printing the png (enlarging width)
png(filename = "plot5.png", width = 700)
plot(sum.vehicles.baltimore, type= "o", col="red", pch=19, lty=2, xaxt="n", xlab="Year", ylab="Total Vehicle Emissions (tons)")
axis(1, at=c(1999, 2002, 2005, 2008), labels=c(1999, 2002, 2005, 2008))
title(main = "Total Annual Emissions in Baltimore City from motor vehicle sources ")
dev.off()

# ATTENTION: The number of observations we are interested in varies considerably from year to year.
# So, it would be wiser to use their mean values than their sums.



# --------------------------------------------------------------------------------------
# QUESTION 6: Compare emissions from motor vehicle sources in Baltimore City with emissions
# from motor vehicle sources in Los Angeles County, California (\color{red}{\verb|fips == "06037"|}fips=="06037").
# Which city has seen greater changes over time in motor vehicle emissions?

# From the SCC data.frame, we are saving the SCC-codes corrresponding to motor vehicle sources
vehicles.scc <- SCC[grep("veh", SCC$Short.Name, ignore.case = TRUE),1]
# From the NEI data.frame, we are selecting the rows with these SCC-codes
NEI.vehicles <- NEI[which(NEI[,2] %in% vehicles.scc),]
# We are selecting only the data for Baltimore City, grouping the data and summarizing the Emissions by year
sum.vehicles.baltimore <- NEI.vehicles %>%
        filter(fips == "24510") %>%
        group_by(year) %>%
        summarize(sum = sum(Emissions))
# We are selecting only the data for Los Angeles, grouping the data and summarizing the Emissions by year
sum.vehicles.losangeles <- NEI.vehicles %>%
        filter(fips == "06037") %>%
        group_by(year) %>%
        summarize(sum = sum(Emissions))

# Creating the plot and printing the png (enlarging width) 
png(filename = "plot6.png", width = 700)
plot(sum.vehicles.baltimore, type= "o", col="red", pch=19, lty=2, xaxt="n", xlab="Year", ylab="Total Vehicle Emissions (tons)", ylim=c(0, max(max(sum.vehicles.baltimore$sum),max(sum.vehicles.losangeles$sum))))
points(sum.vehicles.losangeles$year, sum.vehicles.losangeles$sum, type= "o", col="green", pch=19, lty=2)
axis(1, at=c(1999, 2002, 2005, 2008), labels=c(1999, 2002, 2005, 2008))
legend("right", legend=c("Los Angeles", "Baltimore City"),
       col=c("green", "red"), lty=2)
title(main = "Total Annual Emissions from motor vehicle sources in Baltimore City and Los Angeles")
dev.off()

# ATTENTION: The number of observations we are interested in varies considerably from year to year.
# So, it would be wiser to use their mean values than their sums.