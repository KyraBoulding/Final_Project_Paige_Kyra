################################################################################

#                                ~ MAIN SCRIPT ~                               #

################################################################################

#*******************************************************************************
# TABLE OF CONTENTS
#
#   - Overview
#   - Packages
#   - Libraires
#   - Data Download
#   - Varibales
#   - Workflow
#       - Pathways
#
#*******************************************************************************
################################################################################
#
#   This Script contains the work-flow set up, Libraries, Packages and the 
#   data sets required to run this analysis.
#   The scripts in this repo should be run in the following order:
#
#           - 1.MainScript
#                 (Housekeeping script, with overveiw of workflow and layout
#                   of project)
#
#           - 2.Data.Cleaning.1
#                 (Primary cleaning.filtering of data and creation of data
#                   frames for the analysis)
#
#           - 3.Analysis.Network
#                 (Visualization of weighted connections between primary and
#                   metastasis site, Binomial test on probability of 
#                   metastasis in primary site, visualization of network)
#
#           - 4.Analysis.MCA
#                 (MCA on currated test data set, MCA on subsets of our data
#                   as well as on our full data set.)
#
#   All Scripts are located outside of the folders in the main working 
#   directory of the repo. The folders contained in this work-flow are as 
#   follows:
#
#           - 1.Raw.Data  -> the path to this folder is: path.rd
#                 (This folder contains a copy of the original downloaded
#                   data set without alterations.)
#
#           - 2.Clean.Data   -> the path to this folder is: path.cd
#                 (This folder contains all subsets of data as well as the
#                   primary cleaned data set.)
#
#           - 3.Analysis   -> the path to this folder is: path.a
#                 (This folder contains saved out puts of our anaylsis.)
#
#           - 4.Graphs   -> the path to this folder is: path.g
#                 (This folder contains all visualizeations of data.)
#
#               - Table   -> the path to this folder is: path.t
#                   ( * THIS FOLDER IS LOCATED WITHIN THE 4.Graph FOLDER *
#                     It contains all data out put as tables.)
#
################################################################################


#=============================== PACKAGES ======================================

# These are all packages used that are not in base R.

install.packages("tidyverse")
install.packages("igraph")
install.packages("networkD3")
install.packages("ggplot2")
install.packages("readxl")
install.packages("dplyr")
install.packages("reshape2")
install.packages("FactoMineR")
install.packages ("factoextra")


#=============================== LIBRARIES =====================================

library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(igraph)
library(networkD3)
library(reshape2)
library(FactoMineR)
library(factoextra)

#=============================== DATA DOWNLOAD =================================

# The file of the raw data can be accessed with the following code.
# The raw data file was originally taken from:
#   http://hcmdb.i-sanger.com/download    (1.Datasets included in the database)
# It was downloaded as an excel sheet 

work.d <- getwd()#should be set to the main "Final_Project_Paige_Kyra" folder by default

data <- read_xlsx("1.Raw.Data/dataset_information.xlsx")


# *** NOTE ***
#       This randomly does not retreive the entire raw data (all 14 varaibles) 
#       if this occurs manually import the "data_information.xlsx" file, it is 
#       located within the 1.Raw.Data folder. 

#============================== VARIABLES ======================================

# "Alpha.val" is the p-value threshold of significance 
Alpha.val <- 0.05




#============================ WORK FLOW SET UP =================================


out.put.folders <- c("1.Raw.Data","2.Clean.Data", "3.Analysis", "4.Graphs", 
                     "4.Graphs/Table")

# check if folders exist and make them if they don't.
# This loop checks goes through the given out.put.folders list and checks to see 
# if they exisit in the working directory. If they don't they print "does not 
# exisit" and creates them, if it does exist it prints "does exist"

for(i in 1:length(out.put.folders)){
  if(!file.exists(out.put.folders[i])){
    print(paste(i, "does not exist"))
    dir.create(out.put.folders[i])
  }else{
    print(paste(i,"does exist")) 
  }
}
#---- Pathways---------

#path to 1.RawData folder
path.rd <- paste(work.d,"/",out.put.folders[1], "/", sep="")
#test the pathway...
# x.t <- c(5)
# write.csv(x.t,paste(path.rd, "test.x.csv"), row.names=FALSE)
# to remove row names from saved files       ^^^

#Path to 2.Clean.Data
path.cd <- paste(work.d,"/",out.put.folders[2], "/", sep="")

# Path to 3.Analysis
path.a <- paste(work.d,"/",out.put.folders[3], "/", sep="")

# Path to 4.Graphs
path.g <- paste(work.d,"/",out.put.folders[4], "/", sep="")

# Path to Table folder in 4.Graphs folder
path.t <- paste(work.d,"/",out.put.folders[5], "/", sep="")

# write.csv(x.t,paste(path.t, "test.x.csv"), row.names=FALSE)








