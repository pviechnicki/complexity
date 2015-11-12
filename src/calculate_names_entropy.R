#Calculate entropy of baby names files from SSA
#https://www.ssa.gov/oact/babynames/limits.html
# P. Viechnicki 11/11/15
require(dplyr);
require (ggplot2);
require(reshape2); #Used to combine vectors into a dataframe
library(grid);
library(gridExtra);
library(cowplot);


#Entropy function 
#Need to find a different name
myEntropy <- function(dataset, totalBirths){
	total <- as.numeric(totalBirths); #Need to convert from list to numeric
	runningTotal <- 0;
	increment <- 0;
	for (i in 1:nrow(dataset))
	{	
		myCount <- dataset[i, "Count"];
		
		increment <- ((myCount/total) * log2(myCount / total));
		#Debug statements
		#print(dataset[i, c("Name", "Count")]);
		#print(increment);
		runningTotal <- runningTotal + increment;
	}
	return (-1 * runningTotal);
}

#Change to your data directory
setwd("c:/Users/pviechnicki/Desktop/pviechnicki_home/Projects/complexity/data/baby_names");
#setwd("c:/Users/pviechnicki/Desktop/pviechnicki_home/Projects/complexity/data/baby_names/test");

#Create vector of dataset names
years <-(1880:2014);
#years <-(1880);
datasetNames <- 0;

for (i in 1:length(years))
{
	datasetNames[i] <- sprintf("yob%4d.txt", years[i]);
}

#initialize vector of birth totals, and similar one for entropies
births <- 0;
entropies <- 0;
#Read in each year's names file into a table object, save the total births
for (i in 1:length(datasetNames))
{
	temp <- read.table(datasetNames[i], header=FALSE, sep=",",
	col.names=c("Name","Sex", "Count"));

	#Analyze total number of births
	births[i] <- summarize(temp, total_births = sum(Count));
	entropies[i] <- myEntropy(temp, births[i]);
	rm(temp);
}

#Combine the birth totals and years vectors into a single data frame
#Trick: need to convert births from list to vector first
birth_stats <- data.frame(years, unlist(births), entropies);

colnames(birth_stats) <- c("Year", "Births");

#Make line graph of results
myBirthsPlot <- ggplot(data=birth_stats, aes(x=Year, y=Births)) + 
geom_line(color="blue") +
ggtitle("Total Births");

myEntropiesPlot <- ggplot(data=birth_stats, aes(x=Year, y=entropies)) + 
geom_line(color="red") + 
ggtitle("Yearly entropy Baby Names");

myBirthsPlot;
myEntropiesPlot;


pdf("entropies.pdf");
plot_grid(myBirthsPlot, myEntropiesPlot, ncol=1, align='v');
dev.off();

