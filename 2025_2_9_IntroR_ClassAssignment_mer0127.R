##################################################################################################################
#Author: Madeline Redd-Hamrick (Maddie) (mer0127)

#git config --global user.name 'Mads-Hamrick'
#git config --global user.email "'madeline.e.redd@gmail.com'


#2025 February 9

rm(list = ls(all=TRUE)) #Clears Environment to see if code can stand alone

#Class:ENTM 6820
#Assignment Name: Intro R Class Assignment 

#Late due to missing class on Thursday 2025 February 6 for a out-state presentation

###################################################################################################################
#Defined my Working Directory (WD) to the Lab folder on Laptop   

#PERSONAL LAPTOP
setwd("C:/Users/madel/OneDrive - Auburn University/Auburn Graduate Semseters/Spring 2025/ENTM 6820/2025_datafiles")

###################################################################################################################
#No data to import or packages to install
###################################################################################################################

#Create a vector named 'z' with the values 1 to 200

z<-c(1:200) #Also can type of numbers, creating a for loop if you need only even or odd numbers is quickest
print(z)

#Print the mean and standard deviation of z on the console

zmean<-mean(z)  #Default send solution to the Global Environment when in vector form
zsd<-sd(z)      #if not in vector form, output would directly go to Console

print(zmean)    #print vector outputs data into console
print(zsd)

#Create a logical vector named zlog that is 'TRUE' for z values greater than 30 and 'FALSE' otherwise.

zlog<- z > 30

print(zlog)

#Make a dataframe with z and zlog as columns. Name the dataframe zdf
#Change the column names in your new dataframe to equal “zvec” and “zlogic”
#Make a new column in your dataframe equal to zvec squared (i.e., z2). Call the new column zsquared. 

zdf <- data.frame(z,zlog)

names(zdf)<- c("zvec","zlogic")   #Creating headers for data.frame

zdf$zsquared <- zdf$zvec^2        #data.frame$new column name <- data.frame$selected column+operation 

print(zdf)

#Subset the dataframe with and without the subset() function to only include values of zsquared greater than 10 and less than 100 

zdf_1 <- zdf[zdf$zsquared > 10 & zdf$zsquared < 100, ]    #without subset Fuction
print(zdf_1)

zdf_2 <- subset(zdf, zsquared >10 & zsquared < 100 )      #Subset(data, requirements need to be met)
print(zdf_2)
                                                          #Categorical is subset (data, Column Name == "Variable")

#Subset the zdf dataframe to only include the values on row 26

zdf_row26 <- zdf[26,]
print(zdf_row26)

#Subset the zdf dataframe to only include the values in the column zsquared in the 180th row.


zdf_zsquared_row180 <- zdf[180,"zsquared"]
print(zdf_zsquared_row180)

#Annotate your code, commit the changes and push it to your GitHub