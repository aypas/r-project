library("RColorBrewer")
library("GISTools")
library("Cairo")

colors <- brewer.pal(8, "Set1")
colorsT <- add.alpha(colors, .4)


data <- read.csv("data.csv")
fields <- names(data)[4:length(names(data))]

fields <- c("Life Expectancy", "Adult Mortality", "Infant Death", "Alcohol", "% Healthcare Expenditure",
			"Hepatitis B", "Measles", "BMI", "<5 Death", "Polio", "Total Healthcare Expenditure", "Diphtheria",
			"HIV", "GDP", "Population", "Thinness <20", "Thiness 5-9", "Income", "Schooling")
#log transform all data points from life expextancy on
data <- data[,4:dim(data)[2]]


#I spent a solid 3 hours trying to figure
#out how to manually remove outliers. 
#Little did I know the argument outline=False would do it on its own
boxplot(log10(data), ylim=c(0,10), outline=FALSE, ann=FALSE, axes=FALSE, col=colorsT)


axis(2, at=seq(0,10,2), labels=seq(0,10,2), las=2, cex.axis=.85, mgp=c(3,.5,0), tck=.01)
mtext("Values on Logorithmic Scale", side=2, line=1.5)


mtext(fields, side=1, at=seq(1,length(fields), 1), line=c(0,.4), cex=.4)


mtext("Distribution of Mortality Factors", side=3, cex=1.5)