#Analyze baby names files from SSA
#https://www.ssa.gov/oact/babynames/limits.html
require(dplyr);
require (ggplot2);
require(reshape2); #Used to combine vectors into a dataframe

#Change to your data directory
setwd("c:/Users/pviechnicki/Desktop/pviechnicki_home/Projects/complexity/data/baby_names");

#Create vector of dataset names
years <-(1880:2014);
datasetNames <- 0;

for (i in 1:length(years))
{
	datasetNames[i] <- sprintf("yob%4d.txt", years[i]);
}

#initialize vector of birth totals
births <- 0;
#Read in each year's names file into a table object, save the total births
for (i in 1:length(datasetNames))
{
	temp <- read.table(datasetNames[i], header=FALSE, sep=",",
	col.names=c("Name","Sex", "Count"));

	#Analyze total number of births
	births[i] <- summarize(temp, total_births = sum(Count));
	rm(temp);
}

#Combine the birth totals and years vectors into a single data frame
#Trick: need to convert births from list to vector first
birth_stats <- data.frame(years, unlist(births));

colnames(birth_stats) <- c("Year", "Births");

#Make line graph of results
myplot <- ggplot(data=birth_stats, aes(x=Year, y=Births)) + geom_line();

