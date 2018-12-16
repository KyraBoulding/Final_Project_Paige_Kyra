################################################################################

#                        Data Cleaning 1                                       #

################################################################################
install.packages("tidyverse")

library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)

getwd()
setwd("1.Raw.Data")
data <- read_xlsx("dataset_information.xlsx")
setwd("~/GitHub/Final_Project_Paige_Kyra")

#===================== Visualize Data ==========================================

#---- Metastic Sites -----------------------------------------------------------


# look frequency of metastic sites in data set

metastasis.site.freq <- data.frame(table(data$Metastasis_site))
# 66 different metastic sites identified. 

# visualize frequency of metastic sites
ggplot(metastasis.site.freq, x=metastasis.site.freq$Var1, 
       y= metastasis.site.freq$Freq)+ 
  geom_col(aes(x=metastasis.site.freq$Var1,y= metastasis.site.freq$Freq))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(size =4, angle=45)) 

# lymph node is the highest frequency (2741), then liver, lung, unknown and brain
# many that are put down for multiple metastic sites... 

#---- Primary Sites ------------------------------------------------------------

# identifying primary sites
primary.site.freq <- data.frame(table(data$Primary_site))
# we have 41 different primary sites 

# plot
ggplot(primary.site.freq, x=primary.site.freq$Var1, 
       y= primary.site.freq$Freq)+ 
  geom_col(aes(x=primary.site.freq$Var1,y=primary.site.freq$Freq))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(size =4, angle=45)) 
# highest frequency primary site is breast, then colorectum, prostate and thyroid






#===================== Cleaning Data set =======================================

#===== Initial Cleaning ========================================================

# first, we only want to look at cancer that have metastisized
data<- data.frame(data)
# removing everything that hasnt metasized
prime.data.a <- data %>%
  filter(Metastasis_status == "YES")

# remove columns with variables we aren't using
obs.delete <- c( "Cancer_subtype", "Experiment_id", "Pubmed_id", "Dataset_id", "Platform_id", "Standard", "Class_name", "Sample_id","Sample_label")
prime.data.b <- prime.data.a[, ! names(prime.data.a) %in% obs.delete, drop = F]

# some of the primary sites are listed with mulitple sites....
# get rid of rows with multiple metastisis sites for one sample
prime.data.c <- prime.data.b[!grepl(",", prime.data.b$Metastasis_site),]
# remove the unknown catagory
prime.data.d <- prime.data.c[!grepl("unknown", prime.data.c$Metastasis_site),]
# remove nay rows with NA values
prime.data.e <- na.omit(prime.data.d)

# remove spaces from names in columns
prime.data.f <- as.data.frame(apply(prime.data.e,2,function(x)gsub('\\s+', '_',x)))
#kidney is spelt wrong... replace it with correct spelling
prime.data.g <- as.data.frame(apply(prime.data.f,2,function(x)gsub('kindey', 'kidney',x)))


# check if remomved all missing values
sum(is.na(prime.data.g))
head(prime.data.g)
# rename the final data frame
prime.data <- prime.data.g

#make a frequency table and save as csv
prime.data.h <- data.frame(table(prime.data$Metastasis_site))
# write this file as clean data and save
write.csv(prime.data.h, paste(path.cd, "Freq_of_Metas_Site_All_Primary.csv"),
          row.names = FALSE)

#==== Subset by Primary Site ===================================================
# We want to look at each Primary tumor site indavidually and the metastatic
# sites that are associated with each. We also want to add a column with the
# weight of each metastatic site. We determined the weight of sites by deviding
# the number of occurances at that metastatic site by the total number of 
# samples with that primary site. 
unique(prime.data$Metastasis_site)

#create object with every unique primary site
primary.site.b<- (unique(prime.data$Primary_site))


# create for loop to do this for every primary site
for(i in 1:length(primary.site.b)){
  #i=1
  n<- prime.data %>%
    filter(Primary_site == primary.site.b[i])  #filter by desired metastatic
  # site, assign to temporary object
 
  m <- data.frame(table(n$Metastasis_site))  # create frequency table of filtered
  # subset of data, assign to a temporary object 
  m$Weight <- (m$Freq)/(sum(m$Freq)) #create weight column
  
  colnames(m)[1] <- "Matastatic_Site"  # rename weight column
 
  #create column with the primary site (needed for later merging) 
  m$Primary_site <- rep(primary.site.b[i], length(m$Weight))
  
  #remove rows with zero frequencies
  m<- m[m$Freq!=0,]
  # write this as a clean data file 
  v <- paste (primary.site.b[i] ,"Freq_Weight_Mas_Site.csv", sep="_")
  write.csv(m, paste(path.cd,v[1]))
   
# rename temporary objects so the data frames can be called on later   
  assign(paste(primary.site.b[i], ".freq", sep=""), m) 
 assign(paste(primary.site.b[i], ".data", sep = ""), n)
}



#===== Merge all subsets into one data frame ===================================


# merge all the frequency/weight data sets to get one master set
all.prime.sites.weight <- rbind(breast.freq, colorectum.freq, skin.freq,
                                lung.freq, pancreas.freq, kidney.freq, 
                                oral_cavity.freq, tongue.freq, mandible.freq,
                                FOM.freq, prostate.freq, midgut.freq, liver.freq,
                                ovary.freq, nasopharynx.freq, femur.freq, 
                                humerus.freq, fibula.freq, tibia.freq, costa.freq,
                                pelvis.freq, stomach.freq, testis.freq, thyroid.freq,
                                brain.freq, small_intestine.freq, cervix.freq,
                                penis.freq, adrenal_gland.freq, bladder.freq,
                                esophagus.freq, thymus.freq, uterus.freq, eye.freq)

# write as csv
write.csv(all.prime.sites.weight, paste(path.cd, "All_Freq_weight_data.csv"))


