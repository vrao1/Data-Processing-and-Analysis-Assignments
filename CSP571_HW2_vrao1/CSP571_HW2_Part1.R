# Please note that Homework Two consists of two parts, an R file and a
# Text document. Ensure that you submit both parts.
# Load in the Boston Housing data set using the code below.
install.packages('mlbench')
library('mlbench')
data(BostonHousing)


# 1. Create a scatterplot matrix of all variables in the data set. Save your output
pdf('BostonHousing.pdf')
pairs(BostonHousing, main = "Boston Housing", pch=21 ,bg = "light blue", panel=panel.smooth, cex=1.5, cex.labels = 2, font.labels =2)
dev.off()

# 2. For each numeric variable in BostonHousing, create a separate boxplot using
# "Method 2" listed in the class notes. Do this programmatically; meaning do
# not simply hardcode the creation of every boxplot. Instead, loop over the
# approriate columns and create the boxplots. Save your output. Ensure your boxplots
# all have proper titles

par(las=1)

for (i in 1:length(BostonHousing)){
	filename = paste(names(BostonHousing[i]) , "pdf" , sep ="."	)
	pdf(filename)
	boxplot(as.numeric(BostonHousing[,i]),main = names(BostonHousing[i]),horizontal=TRUE)
	dev.off()
}



# 3. Create a correlation matrix and correlation plot
# for the BostonHousing data set. Save your output.

install.packages('corrplot')
 


pdf("correlation_plot_3.pdf")
library(corrplot)
cols <- c(1:ncol(BostonHousing))
BostonHousing[cols] <- lapply(BostonHousing[cols], as.numeric)
M <- cor(temp)
col3 <- colorRampPalette(c("red", "purple", "maroon", "blue"))
corrplot(M, method="number", col=col3(100))
dev.off()


# 4. Identify the top 3 strongest absolute correlations in the data set. Save your output.

Tax ~ rad → 0.91 correlation coefficient
nox~ dis → - 0.77
indus ~ nox → 0.76



# 5. Create a new variable call ageGroup quartiles. Divide the age variable
# into four even sections and assign it to one quartile.





# 6. Go to the website listed below. Convert the html table into a
# dataframe with columns NO, Player, Highlights
install.packages("rvest")
install.packages("tidyr")

url = 'http://www.espn.com/nfl/superbowl/history/mvps'


library(rvest)
library(tidyr)
library(dplyr)
# Store web url
superBowl <- read_html("http://www.espn.com/nfl/superbowl/history/mvps")


mvp_df <- data.frame(NO = superBowl %>% html_nodes("td:nth-child(1)") %>% html_text() %>% .[3:52], 
           Player = superBowl %>% html_nodes("td:nth-child(2)") %>% html_text() %>% .[2:51],
           Highlights = superBowl %>% html_nodes("td:nth-child(3)") %>% html_text() %>% .[2:51] , stringsAsFactors=FALSE)

print(mvp_df)




# 7.Extract the names of the MVPs, Position and Team into columns
# MVP1, MVP2, Position, Team


mvps <- separate(mvp_df[2],Player,c("mvp","position","team"), sep = ",")


# 8. Determine the 90th%, 92.5th%, 95th%, 97.5th% and 99th% confidence intervals
# for the mean of passing yards
# (as listed in "Highlights" column) for quarterbacks.
# Note that there are a few intermediate steps you'll probably want to do
# in order to accomplish this. I am purposelly leaving that up to you, so that
# you are starting to develop the types of problem solving skills you'll need
# to tackle these problems in the wild.



# 9. The following contains data on the calorie counts of four types
# of foods. Perform an ANOVA and determine the Pr(>F)
food1 <- c(164,   172,   168,   177, 	156, 	195)
food2 <- c(178,   191, 	197, 	182, 	185, 	177)
food3 <- c(175, 	193, 	178, 	171, 	163, 	176)
food4 <- c(155, 	166, 	149, 	164, 	170, 	168)

alpha = .05

# Compute the group means
meanfood1 <- mean(food1)
meanfood2 <- mean(food2)
meanfood3 <- mean(food3)
meanfood4 <- mean(food4)

allObs <- c(food1, food2, food3, food4)
grandMean <- mean(allObs)


nfood1 <- length(food1)
nfood2 <- length(food2)
nfood3 <- length(food3)
nfood4 <- length(food4)
N = length(allObs)


# Compute the Sum of Squares Between Groups (SSB)

SSB <-
  (nfood1 * (meanfood1 - grandMean)^2
  + nfood2 * (meanfood2 - grandMean)^2
  + nfood3 * (meanfood3 - grandMean)^2
  + nfood4 * (meanfood4 - grandMean)^2)

# Compute the Sum of Square Errors
food1SqDiff <- sum((food1 - meanfood1)^2)
food2SqDiff <- sum((food2 - meanfood2)^2)
food3SqDiff <- sum((food3 - meanfood3)^2)
food4SqDiff <- sum((food4 - meanfood4)^2)
SSE = food1SqDiff + food2SqDiff + food3SqDiff + food4SqDiff

# Compute the degrees of freedom
df1 = 4 - 1
df2 = N - 4

# Compute the Means Squares
MSB = SSB / df1
MSE = SSE / df2

# Compute the F-statistic
F = MSB / MSE

# Find the critical value of F
# Reject H0 if F >= criticalValue
criticalValue = qf(p = 1 - alpha
   , df1 = df1
   , df2 = df2
   )
   
if(F >= criticalValue){ 
	print("Reject H0")
	}else{
	print("Fail to reject H0")
	}


# 10. Install the lubridate package and familarize yourseslf with it.
# This is the preferred package for handling
# dates in R, which is an important skill.
# Practing handling dates by using the package to determine how many
# Tuesdays fell on the first of the month
# during the 19th century (1 Jan 1801 to 31 Dec 1901).
install.packages('lubridate')
library('lubridate')

startDate <- dmy("01-Jan-1801")
endDate <- dmy("31-Dec-1901")
myDates <- seq(from = startDate, to = endDate, by = "days")
cat("Number of Mondays between 01-Jan-1801 and 31-Dec-1901 = " , length(which(wday(myDates) == 2)))
