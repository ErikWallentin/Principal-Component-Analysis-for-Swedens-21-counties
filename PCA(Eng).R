#####################################################################################################
# In this project, we are looking to find out which of Sweden's 21 counties are the same and which 
# differ in terms of four variables. The variables used are the number of reported cases of 
# assault, vandalism and drug possession a given county had per 50,000 inhabitants in the year 2014. 
# Why the number is presented per 50,000 inhabitants is because then it is possible to compare 
# counties with many residents, where there is greater likelihood that these figures are higher, 
# with counties with lower population numbers. The fourth variable is post-secondary education of at 
# least three years, which is simply the proportion of residents in each county in 2014 who met this 
# criterion. 
# The result over how the counties differ is later presented in a graph after a Principal Component 
# Analysis (PCA) has been implemented.

# In short, PCA is about creating a low-dimensional representation of high-dimensional data that 
# includes as much of the total variation in the dataset as possible.
# To perform PCA, we first need to import and cleans the data containing information about the four 
# variables mentioned above. Data on assault, vandalism and drug possession are obtained from the 
# database of the Swedish authority "Swedish National Council for Crime Prevention" 
# (http://statistik.bra.se/solwebb/action/index).
# In order to obtain the total number of people in each county with a post-secondary education of at 
# least three years, the database of the Swedish authority "Statistics Sweden" is used 
# (http://www.statistikdatabasen.scb.se/pxweb/sv/ssd/?rxid=fcf5115f-47a0-4f7f-8538-70be022ae865).
# Information about the number of inhabitants in each county in the year of 2014 is required to 
# calculate the total number of reported offenses per 50,000 inhabitants and the proportion with 
# a post-secondary education of at least three years in each county. This information can also be 
# obtained from the database of "Statistics Sweden".

# This project is divided into two parts, where the first part is dedicated to importing and cleansing 
# the data needed to perform the PCA. The second part is about the implementation of our 
# principal component analysis and at the bottom you'll find a brief summary of the result.
#####################################################################################################


#####################################################
##################Prepare the data###################
#####################################################

# Import the xls-file with the name "Brott 2014" which contains information about crime in Sweden for 
# the year 2014.
install.packages("XLConnect")
library(XLConnect)

Crime <- loadWorkbook(file.choose(), create = T)
Crime <- readWorksheet(Crime,1, header = F)

# Remove the first five rows and the entire column three. Column two is then converted to the
# numeric type
Crime <- Crime[-c(1:5),]
Crime$Col3 <- NULL
Crime[,2] <- as.numeric(Crime[,2])

# Using a loop, a vector is created containing the names of all the 21 counties we have access to.
i <-1
County <- rep("",21)

for(d in 1:21){
  name <- Crime[i,1]
  
  County[d] <- name
  i = i + 8
}

# Repeat each name in the "County" vector 3 times. This is done so that we later, in the "Crime" 
# and "Pop" vectors, easily can connect the right rows to the right county.
Countyx3 <- rep(County, each = 3)


# Before we can complete the vector "Crime", we need to create a vector containing information about 
# the Swedish population of year 2014.

# Import the csv-file with the name "Befolkning Sverige 2014" which contains information about the 
# Swedish population of year 2014.
Pop <- read.csv(file.choose(), header=TRUE, sep=";")
Pop <- as.data.frame(tapply(Pop$X2014, Pop$region, sum)) 

# Remove the two digits and the space that's before the name of each county so that we can sort each 
# country's population by county name in alphabetical order.
# "Pop" becomes a data frame containing the population of each county, where each number of inhabitants
# per county is repeated three times to facilitate when this dataframe is later multiplied by "Crime".
row.names(Pop) <- substring(rownames(Pop),4)
Pop <- Pop[order(row.names(Pop)),]
Popx1 <- as.vector(Pop)
Pop <- as.data.frame(rep(Pop, each = 3))

# Divide the population in each county by 50 000
PopDiv50K <- Pop[,1] / 50000
Pop <- cbind(Pop, PopDiv50K, Countyx3)
colnames(Pop) <- c("Population 2014", "Population 2014 divided by 50K", "County")


# We now return to the "Crime" dataframe to complete it.

# Keep only those rows in the dataframe "Crime" that contains values in both columns.
# With the help of the "Pop" dataframe, the proportion of crime per 50,000 inhabitants in the specific 
# county is then calculated.
Crime <- Crime[complete.cases(Crime),]
CrimePer50KInv <- round(Crime[,2] / Pop[,2], 0)
Crime <- cbind(Crime, CrimePer50KInv, Countyx3)
colnames(Crime) <- c("Crime", "Amount","Crime per 50K inhabitants", "County")


# Import the csv-file with the name "Eftergym 2014" which contains information about the number of 
# people per county in Sweden who have a post-secondary education of at least three years.
Education <- read.csv(file.choose(), header=TRUE, sep=";")
Education <- as.data.frame(tapply(Education$X2014, Education$region, sum)) 

# Remove the two digits and the space that's before the name of each county so that we can, after 
# county name, sort the number of people in each county with at least three years of post-secondary 
# education. The proportion of people in each county with post-secondary education of at least three 
# years is then calculated.
row.names(Education) <- substring(rownames(Education),4)
Education <- as.vector(Education[order(row.names(Education)),])
Education <- round(Education / Popx1 * 100, 1)


# We now have everything we need to create the dataframe that are needed for our 
# principal component analysis.
Assault <- subset(Crime$`Crime per 50K inhabitants`, Crime$Crime == "Misshandel inkl. grov (5, 6 §)")
Vandalism <- subset(Crime$`Crime per 50K inhabitants`, Crime$Crime == "12 kap. Skadegörelsebrott")
Drug_Possession <- subset(Crime$`Crime per 50K inhabitants`, Crime$Crime == "Innehav (1-3 a §)")

FullData <- as.data.frame(cbind(Assault, Vandalism, Drug_Possession, Education))
rownames(FullData) <- County


#####################################################
############Principal Component Analysis#############
#####################################################

# In principal component analysis, it's important to scale the variables to be used in the analysis.
# Below we see that the variable "vandalism" has by far the highest variance. Had we not scaled our 
# variables, this variable would by far have the biggest influence to where our counties were placed
# in the final plot that illustrates our principal component analysis.
apply(FullData, 2, var)

# Perform principal component analysis
pca <- prcomp(FullData, scale = TRUE)

# Each column of pca$rotation contains the corresponding principal component loading vector.
# We see below that the first "principal components" depend mostly on the variables "Assault", 
# "Vandalism" and "Drug_Possession".
# The variable that by far has the greatest impact on the second "principal component" is "Education".
pca$rotation

# Create a plot that shows the cumulative proportion of variance that each principal component explains.
pca.var <- pca$sdev^2
pca.varexplained <- pca.var / sum(pca.var)

plot(cumsum(pca.varexplained), xlab = "Principal Component", ylab = "Proportion of variance explained", 
     ylim = c(0,1), type = "b")

# About 85% of all variance is explained by the first two principal components. Below they are plotted .
biplot(pca, scale = 0)


# Below is a plot that better visualizes the counties with regard to crime and education. 
# However, the four arrows that appeared in the previous plot cannot be produced here.

install.packages('ggplot2')
library(ggplot2)
pca.data <- data.frame(Sample = rownames(pca$x), X = pca$x[,1], Y = pca$x[,2])

ggplot(data = pca.data, aes(x=X, y=Y, label = Sample)) + geom_text() +
  xlab(paste("PC1 - ", round(pca.varexplained[1]*100,2), "% of the total variance explained", sep = "")) +
  ylab(paste("PC2 - ", round(pca.varexplained[2]*100,2), "% of the total variance explained", sep = "")) +
  theme_bw() + ggtitle("Principal Component Analysis with two principal components")


#####################################################################################################
# The resulting plot we get after our PCA can be interpreted that counties that sit far to the left on
# the x-axis are counties with low number of reported cases of assault, vandalism and drug possession 
# per 50,000 inhabitants, because these three variables contribute most to principal component number 1.
# Thus, we can summarize it as that these counties have low crime rates in the categories of assault, 
# vandalism and drug possession. In contrast, counties that places themselves further to the right on 
# the x-axis have higher crime rates in these three areas.

# Regarding where a county is located on the y-axis in the plot, the variable education, which is the 
# proportion of people in the county who have a post-secondary education of at least three years, has 
# the greatest influence and thus contributes most to the principal component number 2. The higher the
# proportion highly educated residents a county have, the higher up on the y-axis the county places 
# itself. We see, however, that in particularly the variable assault also affects where on the y-axis 
# a county is located and that the effect now is the opposite, the higher number of reported cases of 
# assault per 50,000 inhabitants, the further down the plot the county places itself.

# One of two counties that really stands out in the result is Södermanlands county which places itself
# very far down and relatively far to the right in the plot. We can thus interpret the location of this
# country as that this is a county that in the year 2014 had a high crime rate, especially regarding
# number of reported cases of assault and was a county that had a low proportion of highly educated 
# residents compared with the other counties in the study.
# The other county that stands out is the county of Stockholm, which places itself extremely far to 
# the right in the plot, which means that in the year 2014, they were at the top of all counties in 
# terms of the number of cases of assault, vandalism and drug possession reported. Since they are 
# placed so far to the right on the x-axis but still above more than half of the counties on the 
# y-axis, Stockholm County must therefore have one of the highest proportions of highly educated 
# residents in the country.

# Other interesting conclusions we can draw from our PCA are that the counties of Halland, Kronoberg,
# Norbotten, Blekinge, Värmland, Jönköping and Jämtland are very similar to each other regarding 
# crime within the three previously mentioned categoris and educational level. Since these counties 
# lie far to the left on the x-axis, it's the level of education that mainly distinguishes eg. 
# Halland County from Jönköping County, where Halland County has a larger proportion of residents 
# with post-secondary education of at least three years.
# If we continue to study Halland County and this time compare them with the county of Östergötland, 
# we see that Halland places itself higher up on the y-axis. Does this mean that they have a higher 
# proportion of residents with a post-secondary education of at least three years? The answer is no. 
# We can interpret it as that Halland has lower crime rates, but since the three variables, 
# especially assault, contributes to a county placing itself further down the y-axis means that 
# Östergötland County has a higher proportion of well-educated residents. 
# The same applies when comparing e.g. Uppsala County and Stockholm County.
#####################################################################################################