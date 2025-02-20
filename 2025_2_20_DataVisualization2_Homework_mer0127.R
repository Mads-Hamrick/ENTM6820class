##########################################################################################################################

#Author: Madeline Redd-Hamrick (Maddie) (mer0127)

#git config --global user.name 'Mads-Hamrick'
#git config --global user.email 'madeline.e.redd@gmail.com'

#2025 February 17

rm(list = ls(all=TRUE)) #Clears Environment to see if code can stand alone

#Class:ENTM 6820
#Assignment Name: Data Visualization 2 Homework

##########################################################################################################################
#Import data from project
#Packages Needed for Code

#install.packages("tidyverse")
#install.packages("ggrepel")
#install.packages("ggpubr")
#install.packages("lme4")
#install.packages("emmeans")
#install.packages("multcomp")


library(tidyverse) #version 2.0.0
library(ggpubr) #version 0.6.0
library(ggrepel)  #version 0.9.6
library(lme4) #version 1.1-35.5
library(emmeans)  #version 1.10.4
library(multcomp) #version 1.4-28

#New Color Palette 

cbbPalette <- c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#000000")

##########################################################################################################################

BacterialAlpha <- read.csv("BacterialAlpha.csv", na.strings = "na")
head(BacterialAlpha)

BacterialAlpha$Time_Point <- as.factor(BacterialAlpha$Time_Point)
BacterialAlpha$Crop <- as.factor(BacterialAlpha$Crop)
BacterialAlpha$Crop <- factor(BacterialAlpha$Crop, levels = c("Soil", "Cotton", "Soybean"))

str(BacterialAlpha)


##Figure 2a: Bacterial Evenness

Figure2a <- ggplot(BacterialAlpha, aes(x = Time_Point, y = even, color = Crop)) +  
  geom_boxplot(position = position_dodge(0.85)) +   #Add dodge positions to avoid overlap
  geom_point(position = position_jitterdodge(0.05)) +  
                                                    #Add jittered points to show individual data points, avoiding overlap
  ylab("Pielou's Evenness") +  
  xlab("Hours Post Sowing") +  
  scale_color_manual(values = cbbPalette, name = "", labels = c("Soil no seeds", "Cotton spermosphere", 
                                                                "Soybean spermosphere")) +  
                                                        #Manually adding color; choice goes in order of ccbPalette vector
  theme_classic()  

                      Figure2a #Calling plot to pane



##Figure 2b: Water Imbibition Correlate with Bacterial Evenness

BacterialAlpha.NoSoil <- subset(BacterialAlpha, Crop != "Soil")

Figure2b <- ggplot(BacterialAlpha.NoSoil, aes(Time_Point, 1000 * Water_Imbibed, color = Crop)) +  
  geom_jitter(width = 0.5, alpha = 0.5) +                  #Add jittered points to show individual data, 50% transparency
  stat_summary(fun = mean, geom = "line", aes(group = Crop)) +           #Adding Mean Line for each Crop group
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +     #Error Bars each Crop group
  
  #Plot Labels
  xlab("Hours post sowing") +  
  ylab("Water Imbibed (mg)") + 
  
  #Plot Color Selection and Theme
  scale_color_manual(values = c(cbbPalette[[2]], cbbPalette[[3]]), name = "", labels = c("", "")) + 
  theme_classic() +  
  
  #Altering the legend's postion and background
  theme(strip.background = element_blank(), legend.position = "none") +  
  
  facet_wrap(~Crop, scales = "free")  


                                Figure2b #Calling plot to Plot pane



##Figure 2c 
Figure2c <- ggplot(BacterialAlpha.NoSoil, aes(y = even, x = 1000 * Water_Imbibed, color = Crop)) +  
  geom_point(aes(shape = Time_Point)) +  #Changing points to different shapes based on Time.Point
  geom_smooth(se = FALSE, method = lm) +  #Smooth line from lm without confidence interval shading
  
  xlab("Water Imbibed (mg)") +  
  ylab("Pielou's evenness") + 
  
  scale_color_manual(values = c(cbbPalette[[2]], cbbPalette[[3]]), name = "", labels = c("Cotton", "Soybean")) +  
  scale_shape_manual(values = c(15, 16, 17, 18), name = "", labels = c("0 hrs", "6 hrs", "12 hrs", "18 hrs")) +  
  theme_classic() +  
  
  theme(strip.background = element_blank(), legend.position = "none") +
  facet_wrap(~Crop, scales = "free")  

                                Figure2c     #Calling plot to Plot pane
  
##########################################################################################################################

#Figure 2; Significance Levels Added 


Figure2 <- ggarrange(
  
  Figure2a,                      #Arrange multiple ggplot objects into a single figure
  Figure2b,  
  Figure2c,  
  
  labels = "auto",               #Automatically adding labels to plots 
  nrow = 3, ncol = 1,            #Arrange the plots in 1 column, 3 rows
     
  legend = FALSE  )

                Figure2    #Calling plot to Plot pane

                
                  
  Figure2a + stat_compare_means(method = "anova") #Apply an ANOVA to the groups

                
  Figure2a + geom_pwc(aes(group = Crop), method = "t_test", label = "p.adj.format")
                                                      #Example with p-values as significance levels

  Figure2a + geom_pwc(aes(group = Crop), method = "t_test", label = "p.adj.signif")
                                                      #Example with * as significance levels

  Figure2a + geom_pwc(aes(group = Crop), method = "t_test", label = "{p.adj.format}{p.adj.signif}")
                                                      #Example with combined p-value and * to indicate significance

  Figure2c + stat_cor()

  Figure2c + stat_cor(label.y = 0.7) + stat_regline_equation()


##########################################################################################################################

diff_abund <- read.csv("diff_abund.csv")
head(diff_abund)
str(diff_abund)


diff_abund$log10_pvalue <- -log10(diff_abund$p_CropSoybean)
diff_abund_label <- diff_abund[diff_abund$log10_pvalue > 30,]



  ggplot() + 
  geom_point(data = diff_abund, aes(x = lfc_CropSoybean, y = log10_pvalue, color = diff_CropSoybean)) + 
  theme_classic() + 
  geom_text_repel(data = diff_abund_label, aes(x = lfc_CropSoybean, 
                                               y = log10_pvalue, color = diff_CropSoybean, label = Label))



  ggplot() + 
  geom_point(data = diff_abund, aes(x = lfc_CropSoybean, y = log10_pvalue, color = diff_CropSoybean)) + 
  geom_text_repel(data = diff_abund_label, aes(x = lfc_CropSoybean, y = log10_pvalue, 
                                               color = diff_CropSoybean, label = Label)) + 
  
  scale_color_manual(values = cbbPalette, name = "Significant") +
  theme_classic() + 
  
  xlab("Log fold change Soil vs. Soybean") +
  ylab("-log10 p-value")
  


  ggplot() + 
  geom_point(data = diff_abund, aes(x = lfc_CropSoybean, y = log10_pvalue)) + 
  geom_point(data = diff_abund_label, aes(x = lfc_CropSoybean, y = log10_pvalue), color = "#0072B2", 
                  shape = 17, size = 4) +
  geom_text_repel(data = diff_abund_label, aes(x = lfc_CropSoybean, y = log10_pvalue, label = Label), 
                  color = "#0072B2") + 
  
    theme_classic() + 
    
  xlab("Log fold change Soil vs. Soybean") +
  ylab("-log10 p-value")


######################################################################################################################

##Incomplete code due to no access of data, but good example from Canvas  
Stand <- read.csv("raw_data_valent2023_pythium_seedtreatment.csv", na.strings = "na")

head(Stand)
str(Stand)

    ave_stand <- Stand %>%
    filter(days_post_planting != "173 days after planting") %>%
    group_by(Plot, Treatment_name, Rep, days_post_planting) %>%
    dplyr::summarize(
    ave.stand = mean(stand, na.rm=TRUE)) 



#Linear Model
lm <- lmer(ave.stand ~ Treatment_name*days_post_planting + (1|Rep), data = ave_stand)
car::Anova(lm)


lsmeans <- emmeans(lm, ~Treatment_name|days_post_planting) 
                                                                       #Estimate lsmeans of variety within siteXyear
Results_lsmeansEC <- multcomp::cld(lsmeans, alpha = 0.05, reversed = TRUE, details = TRUE,  Letters = letters) 
Results_lsmeansEC                                                      #Contrast with Tukey ajustment



#Extracting the letters for the bars
sig.diff.letters <- data.frame(Results_lsmeansEC$emmeans$Treatment_name, 
                               Results_lsmeansEC$emmeans$days_post_planting,
                               str_trim(Results_lsmeansEC$emmeans$.group))
colnames(sig.diff.letters) <- c("Treatment_name", 
                                "days_post_planting",
                                "Letters")

#Plotting Letters from Significance Test 
    ave_stand2 <- ave_stand %>%
      group_by(Treatment_name, days_post_planting) %>%
        dplyr::summarize(
        ave.stand2 = mean(ave.stand, na.rm=TRUE),
        se = sd(ave.stand)/sqrt(4)) %>%
        left_join(sig.diff.letters) 



ave_stand$Treatment_name <- factor(ave_stand$Treatment_name, levels = c("Neg. control",
                                                                        "Pos. control",
                                                                        "Intego Suite",
                                                                        "V-10525",
                                                                        "Cruiser Maxx",
                                                                        "Cruiser Maxx + Vayantis",
                                                                        "V-10522",
                                                                        "V-10522 + Lumisena"))

ave_stand$days_post_planting <- factor(ave_stand$days_post_planting, levels = c("8 days after planting",
                                                                                "15 days after planting", 
                                                                                "21 days after planting",
                                                                                "29 days after planting"))


#Stand Bar Graph
ggplot(ave_stand, aes(x = Treatment_name, y = ave.stand)) + 
  stat_summary(fun=mean,geom="bar") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +
  
  ylab("Number of emerged plants") + 
  
  geom_jitter(width = 0.02, alpha = 0.5) +
  geom_text(data = ave_stand2, aes(label = Letters, y = ave.stand2+(3*se)), vjust = -0.5) +
  
  xlab("")+
  
  theme_classic() +
  theme(
    strip.background = element_rect(color="white", fill="white", size=1.5, linetype="solid"),
    strip.text.x = element_text(size = 12, color = "black"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  
  facet_wrap(~days_post_planting)

  #Figure Example saved in Project folder


#########################################################################################################################