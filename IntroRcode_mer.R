###################################################################################################################
#Author: Madeline Redd-Hamrick (Maddie) (mer0127)

#2025 February 2

rm(list = ls(all=TRUE)) #Clears Environment to see if code can stand alone

#Class:ENTM 6820
#Assignment Name: Intro to R Language

###################################################################################################################
#Data and Packages needed for exercise - Insert Below
packages_needed<- c("ggplot2","plotrix", "dplyr") #edit vector to change or add packages

install.packages(packages_needed)

library(plotrix)
library(ggplot2)



example_code <- read.csv("file_name.csv")    

###################################################################################################################


#Objectives of Assignment 
  #1. Use basic mathematical operators in R
  #2. Know some data objects and how to index them
  #3. Install packages
  #4. Load your data into R from a .csv or .txt file

                                            ###Notes to the Professor###
#I have skipped over objectives 1, 2, and 3, since I have had previous undergraduate and Graduate classes teaching basic 
  #and advance R coding skills. I feel like this assignment was too easy, because I was lucky to start with this kind of 
  #assignment in my undergrad classes.
  

###Learned something new###
#Never loaded in a .txt file


table <- read.table( "filename.txt", header = TRUE, sep = ",")  #code to load in .txt file

comparison_function <- all.equal(csv, table)  #all.equal(target, current)
                                              #Compares R objects and report differences if doesn't return TRUE
print(comparison_function)
                        






