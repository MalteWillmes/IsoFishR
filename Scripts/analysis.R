# IsofishR script for further data analysis

library(tidyverse)
library(plyr)
library(stringr)
library(zoo)

#Enter project name, test case here is SKT_Denney
Project <- "SKT_Denney"
setwd(paste0("Projects/",Project))



###Import Data files
#Import the summary, comments, and settings csv file from the project
data.summary <- read.csv(paste0(Project,"_summary.csv"),header=TRUE, sep=",", dec=".",stringsAsFactors=FALSE)
data.settings <- read.csv(paste0(Project,"_settings.csv"),header=TRUE, sep=",", dec=".",stringsAsFactors=FALSE)
data.comments <- read.csv(paste0(Project,"_settings.csv"),header=TRUE, sep=",", dec=".",stringsAsFactors=FALSE)
#Import all individual csv files from a project
setwd("Data/")
paths <- dir(pattern = "\\.csv$")
names(paths) <- basename(paths)
data.files <- ldply(paths, read.csv)
setwd("../")


#Import your trim data file if you have one
data.trim <- read.csv(paste0(Project,"_trims.csv"),header=TRUE, sep=",", dec=".",stringsAsFactors=FALSE)


### Trim data
## Trim the data.files based on the values in data.trim

# Add a new field to data.files which is the Slide_ID and remove the right 5 chars which are the .csv
data.joined <- dplyr::mutate(data.files, Slide_ID = str_sub(.id,1,-5))

#Join the data.trim to the data.files based on the Slide_ID
data.joined <-dplyr::left_join(data.joined, data.trim, by = "Slide_ID")

##Trim the left side of the data
data.trimmed <- data.joined [data.joined$distance >=data.joined$Left.Trim,]






### End script file
setwd("../../")



# DS SKT Script to estimate phenotype
# Run this after the analysis script file 

## quick plot of all trimmed data grouped by Slide_ID and colored by region
p <- ggplot (data.trimmed)
p <- p+ geom_line(aes(x=distance, y=MA, group=Slide_ID, color=region))
p <- p+ theme_bw()
p


#3 Manual classification
data.classified <- data.trimmed
# Create a new column called LAME and populate with either FW or SAL 
data.classified <- dplyr::mutate(data.classified, LAME =ifelse(MA<= 0.7075, "FW", "SAL"))

# Determine the phenotype (When more than 10 consequtive data points are one LAME class)
## NEED TO ADD THE CORRECT FUNTION TO FUN=, right now it just does min which does not make sense right?
data.classified.test <- data.classified %>% 
                        group_by(Slide_ID)%>%
                        mutate(LAME_phenotype=rollapply(LAME, width=10, FUN=min, align="left", fill=NA, na.rm=TRUE))%>%
                        mutate(LAME_phenotype_allFW=all(LAME_phenotype=="FW"))%>%
                        mutate(LAME_phenotype_allSAL=all(LAME_phenotype=="SAL"))%>%
                        mutate(LAME_phenotype_sum=ifelse(LAME_phenotype_allFW=="TRUE", "FWR",ifelse(LAME_phenotype_allSAL=="TRUE","BWR","MIG")))
  