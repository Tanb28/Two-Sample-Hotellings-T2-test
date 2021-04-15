#Two sample HotellingsT2 Test

#Two assumptions
#1)Populations Of the two samples follow multivariate normal distribution
#2)Both the Populations have same covariance matrix (Homoscedasticity)

#Hypothesis
#H0 - mu1 = mu2
#H1 - mu1 != mu2

#Installing and loading required packages
install.packages("mvoutlier")
install.packages("rlang")
install.packages("pcaPP")
iinstall.packages("ICSNP")
library(mvoutlier)
library(pcaPP) 
library(ICSNP)

####Example 1

#Info About the first Data file

#Original Source - Everitt 2005
#Following Dataset contains the data on 2 group
#Group 1- Skulls Found in the graves in Sikkim and neighbouring areas of Tibet
#Group 2-  Skulls picked up on the battlefield in Lhasa District and believed to be those of native soldiers from the eastern province of Khans
#Particular Interest - Tibetans from Khans might be survivors of a particular fundamental human type, unrelated to the Mongolian and Indian types that surrounded them
#Response Variables - 5 Measurements (Units - millimeters)
#Length, Breadth, Height, FHeight, FBreadth

#Loading the data
source("E:/tibetskull.txt")$value #This attached file in repository is text file of Tibetskull
head(Tibet)
str(Tibet)

skull_dat <- Tibet[, 1:5]
skull_dat

#Plot For checking Multivariate normality of data
chisq.plot(skull_dat[Tibet$Type == 1, ], quan = 1, ask = FALSE)
chisq.plot(skull_dat[Tibet$Type == 2, ], quan = 1, ask = FALSE)

#Ideally the points will look like a straight line, but considering the 
#low sample size, we assume multivariate normality

#Plot for checking homoscedasticity
par(cex=0.2)
plotcov(cov(skull_dat[Tibet$Type == 1, ]), cov(skull_dat[Tibet$Type == 2, ]), method1 = "Group 1", method2 = "Group 2")

#From the plot we can see that most of the rings coincide with each other, so we can assume homoscedasticity

#Results of the test
HotellingsT2(as.matrix(skull_dat) ~ Tibet$Type)

#As our p-value(0.0029) < 0.05, We Rej H0  at 5% level of significance
#There is strong evidence that the multivariate means of both the groups are different.
#So, the skulls found in the graves in sikkim and tibet and the Skulls picked up on the battlefield 
#in Lhasa District which was believed to be those of native soldiers from the eastern province of Khans are different.




####Example 2

#Info about the data

#A certain type of tropical disease is characterized by fever, low blood pressure and body aches.
#A pharmaceutical company is working on a new drug to treat this type of disease and wanted to determine 
#whether the drug is effective. They took a random sample of 20 people with this type of disease and 
#18 with a placebo. Based on the data they wanted to determine whether the drug is effective at reducing 
#these three symptoms.


data <- read.csv("E:/eg2.csv")  #Read the second Data file of Drug and Placebo 
head(data)
data$Group <- factor(data$Group)
dat<- data[,1:3]

#Plot for checking multivariate normality
chisq.plot(dat[data$Group == 'Drug', ], quan = 1, ask = FALSE)
chisq.plot(dat[data$Group == 'Placebo', ], quan = 1, ask = FALSE)

#Ideally the points will look like a straight line, but considering the 
#low sample size, we assume multivariate normality


#Plot for checking homoscedasticity
plotcov(cov(dat[data$Group == "Drug", ]), cov(dat[data$Group == "Placebo", ]), method1 = "Drug", method2 = "Placebo")

#From the plot we can see that most of the rings coincide with each other, so we can assume homoscedasticity

#Results
HotellingsT2(as.matrix(dat) ~ data$Group)

#As our p-value(0.2917) > 0.05, We do not Rej H0  at 5% level of significance
#There is no significant difference between the mean vectors for the drug and placebo, 
#providing evidence that the drug is not effective in reducing symptoms.

