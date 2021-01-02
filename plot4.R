##Code for the second Peer Review assigments of course 4 Exploratory data analysis

#Loaging the packages:
library(zip); library(ggplot2)

#Download, unzip the file, openning the data:
if (!file.exists("NEI_data.zip")){
  download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                destfile = "NEI_data.zip", method = "curl")
}else{
  print("File already exists")
}
unzip("NEI_data.zip", exdir = "./NEI_Data")

#Since both archives are in RDS format, to open them:
summary_data <- readRDS("./NEI_Data/summarySCC_PM25.rds")
source_code <- readRDS("./NEI_Data/Source_Classification_Code.rds")

#Transform the year in a factor:
summary_data <- transform(summary_data, year = as.factor(year))
summary_data <- transform(summary_data, SCC = as.factor(SCC))
#Plot4:
#Getting all the data involved in coal:
coal <- source_code$SCC[grep("[Cc]oal", source_code$EI.Sector)]
sub_coal <- summary_data[summary_data$SCC %in% coal, ]

sum_coal <- aggregate(sub_coal$Emissions, by = list(Year = sub_coal$year), FUN = sum)

#Plotting
png("plot4.png", width = 480, height = 480)
barplot(sum_coal$x, names.arg = sum_coal$Year, ylab = "PM25 Emission by Coal", xlab = "Year")
  
dev.off()
