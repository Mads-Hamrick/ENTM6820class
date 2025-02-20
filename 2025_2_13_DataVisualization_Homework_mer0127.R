##################################################################################################################
#Author: Madeline Redd-Hamrick (Maddie) (mer0127)

#git config --global user.name 'Mads-Hamrick'
#git config --global user.email 'madeline.e.redd@gmail.com'

#2025 February 11

rm(list = ls(all=TRUE)) #Clears Environment to see if code can stand alone

#Class:ENTM 6820
#Assignment Name: Data Visualization 

###################################################################################################################
#Import data from project

data("mtcars")

str(mtcars) #view data structure
###################################################################################################################

plot(x = mtcars$wt, y = mtcars$mpg) #basic graph

plot(x = mtcars$wt, y = mtcars$mpg,
     xlab = "Car Weight", 
     ylab = "Miles per gallon", 
     font.lab = 6, 
     pch = 20)                      #Adding Axis labels, font size, and scatter point type


###################################################################################################################
##Packages needed for code below:

#install.packages("ggplot2")
library(ggplot2)

###################################################################################################################

ggplot(mtcars, aes(x = wt, y = mpg))


ggplot(mtcars, aes(x = wt, y = mpg)) +
      geom_point()

ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + geom_smooth()  #geom_smooth adds a trend line and line can change

ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + geom_smooth(method = lm, se = FALSE)           
                                                              #geom_smooth() using method = 'loess' and formula = 'y ~ x'


ggplot(mtcars, aes(x = wt, y = mpg)) + geom_smooth(method = lm, se = FALSE) + geom_point() #Order matters for the layers


ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_smooth(color= "forestgreen", method = lm, se = FALSE) +
  geom_point() +
  xlab("Weight") + 
  ylab("Miles per Gallon")    #Adding Axis labels, line type



ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_smooth(color= "forestgreen",method = lm, se = FALSE) +
  geom_point(aes(size = wt)) +
  xlab("Weight") + 
  ylab("Miles per Gallon")      #Change by adding point size



ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_smooth(color= "forestgreen", method = lm, se = FALSE) +
  geom_point(aes(color = wt)) +
  xlab("Weight") + 
  ylab("Miles per Gallon") +
  scale_colour_gradient(low = "orange", high = "black")


###################################################################################################################


bull.richness <- read.csv("Bull_richness.csv")    #data with R project folder

##Subset data for the Soy data
bull.richness.soy.no.till <- bull.richness[bull.richness$Crop == "Soy" & bull.richness$Treatment == "No-till",]


#Creating a basic box plot from the data
ggplot(bull.richness.soy.no.till, aes(x = GrowthStage, y = richness, color = Fungicide)) + geom_boxplot() 


ggplot(bull.richness.soy.no.till, aes(x = GrowthStage, y = richness, color = Fungicide)) + 
  geom_boxplot() + 
  xlab("") +  ylab("Bulleribasidiaceae Richness")     #Adding Custom Labels to the Axis



####Adding "jitter points" aka all the data points over the boxplot
ggplot(bull.richness.soy.no.till, aes(x = GrowthStage, y = richness, color = Fungicide)) + 
  geom_boxplot() + 
  xlab("") + ylab("Bulleribasidiaceae Richness") +
  geom_point(position=position_jitterdodge(dodge.width = 0.9))    



##Stat feature of ggplot allows for common summary statistics to be calculated from raw data 
ggplot(bull.richness.soy.no.till, aes(x = GrowthStage, y = richness, color = Fungicide)) + 
  stat_summary(fun=mean,geom="bar") +
  stat_summary(fun.data = mean_se, geom = "errorbar") + 
  xlab("") + ylab("Bulleribasidiaceae Richness") +
  geom_point(position=position_jitterdodge(dodge.width = 0.9)) 



#To the Stat function, add position = "dodge"
  ##Manually telling ggplot to dodge the bars
ggplot(bull.richness.soy.no.till, aes(x = GrowthStage, y = richness, color = Fungicide)) + 
  stat_summary(fun=mean,geom="bar", position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge") + 
  xlab("") + ylab("Bulleribasidiaceae Richness")

#Fixing the colors of the bars
ggplot(bull.richness.soy.no.till, aes(x = GrowthStage, y = richness, color = Fungicide, fill = Fungicide)) + 
  stat_summary(fun=mean,geom="bar", position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge") + 
  xlab("") + ylab("Bulleribasidiaceae Richness")





###ADDING A TIME SERIES 
    #Stat_summary() #adding a group option in the aes function
ggplot(bull.richness.soy.no.till, aes(x = GrowthStage, y = richness, group = Fungicide, color = Fungicide)) + 
  stat_summary(fun=mean,geom="line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +
  ylab("Bulleribasidiaceae \n Richness") + xlab ("")


###Faceting allows you to make separate plots for each subset of your data. We wouldn’t necessarily want to make 
#separate plots for all of them.

#To accomplish this use the function "facet_wrap()"
#facet_wrap(~ Variable * Variable2)  

##Example Below:

ggplot(bull.richness, aes(x = GrowthStage, y = richness, group = Fungicide, color = Fungicide)) + 
  stat_summary(fun=mean,geom="line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +
  ylab("Bulleribasidiaceae \n Richness") + xlab("") +
  facet_wrap(~ Treatment)


#Adding a second title to the plots
ggplot(bull.richness, aes(x = GrowthStage, y = richness, group = Fungicide, color = Fungicide)) + 
  stat_summary(fun=mean,geom="line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +
  ylab("Bulleribasidiaceae \n richness") + xlab("") +
  facet_wrap(~Treatment*Crop)   #Caused a zoomed out plot



#Scale Change
  #Adding scales = "free" allows for the crop problem to be avoided

ggplot(bull.richness, aes(x = GrowthStage, y = richness, group = Fungicide, color = Fungicide)) + 
  stat_summary(fun=mean,geom="line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +
  ylab("Bulleribasidiaceae \n Richness") +  xlab("") +
  facet_wrap(~ Treatment * Crop, scales = "free")



#Order Matters
  ##Facet_wrap(~Treatment_Wanted_Front  * Other Treatment)

ggplot(bull.richness, aes(x = GrowthStage, y = richness, group = Fungicide, color = Fungicide)) + 
  stat_summary(fun=mean,geom="line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +
  ylab("Bulleribasidiaceae \n Richness") +  xlab("") +
  facet_wrap(~ Crop * Treatment, scales = "free")

