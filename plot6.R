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

#Plot6:
#Getting all the data involved in with motor vehicles:
vehicles <- source_code$SCC[grep("Vehicles", source_code$EI.Sector)]
sub_vehicles <- summary_data[summary_data$SCC %in% vehicles, ]
baltimore_sub <- subset(sub_vehicles, fips == "24510")
losangeles_sub <- subset(sub_vehicles, fips == "06037")

#Creating the sum of Baltimore and Los Angeles by Vehicles:
sum_balti_vehicle <- aggregate(baltimore_sub$Emissions, by = list(Year = baltimore_sub$year), FUN = sum)
sum_losangeles_vehicle <- aggregate(losangeles_sub$Emissions, by = list(Year = losangeles_sub$year), FUN = sum)

#Putting the data in a Ggplot format
graph_data <- rbind(sum_balti_vehicle, sum_losageles_vehicle)
graph_data <- cbind(graph_data, City = c(rep("Baltimore", 4), rep("LA", 4)))
#Plotting
png("plot6.png", width = 480, height = 480)

ggplot(data = graph_data, aes(x = Year, y = x, group = City, col = City))+
  geom_line() + 
  geom_point() + 
  ylab("Emissions of PM25") + 
  theme_classic()
dev.off()
