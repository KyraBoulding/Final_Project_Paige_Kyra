################################################################################
#                                                                              #
#                               Data Cleaning                                  #
#                                                                              #
################################################################################

#*******************************************************************************
# TABLE OF CONTENTS
#
#   - Overview
#   - Visulaize Data
#       - Structure
#       - Metastic Sites
#       - Primary Sites
#   - Cleaning Data Set
#       - Subset by Primary Site
#       - Data Frame for Metastasis_site = Primary_site Analysis
#       - Repeat Above for Each Primary Site
#
#*******************************************************************************
################################################################################
#
#     This script contains all of the code used to sort and clean the raw
#     data set. It also contains all of the coding for the development of
#     the different subsets of cleaned data required for the Analysis 
#     script.
#
################################################################################



#===================== Visualize Data ==========================================

#     This Section contains code for the primary visualization of the raw data.

#--------------------- Structure -----------------------------------------------

# Look at the over all structure and components of the data set.
str(data)

# look at a summary of the data
summary(data)

#*******************************************************************************
# Main Trends/Takeaways:
#
#     - There are 22023 rows (or samples) recorded
#     - There are 14 columns (or variables) recorded for each sample
#     - Variables of Interest: Cancer_type, Metastasis_site, Primary_site,
#       Metastasis_status, Class_id
#     - Variables to be removed: Cancer_subtype, Experiment_id, Pubmed_id,
#       Dataset_id, Platform_id, Standard, Class_name, Sample_id,Sample_label
#
#*******************************************************************************

#--------------------- Metastic Sites ------------------------------------------

# Look at the frequency of each of the metastic sites in data set, and how many
# unique metastastic sites are recorded within the data.

metastasis.site.freq <- data.frame(table(data$Metastasis_site))

# visualize frequency of metastic sites
ggplot(metastasis.site.freq, x=metastasis.site.freq$Var1, 
       y= metastasis.site.freq$Freq)+ 
  geom_col(aes(x=metastasis.site.freq$Var1,y= metastasis.site.freq$Freq))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(size =4, angle=45)) 

#*******************************************************************************
# Main Trends/Takeaways:
#
#         - 66 different metastic sites identified. 
#         - Lymph node is the highest frequency (2741), then liver, lung, 
#           unknown and brain.
#         - Many samples are recorded with multiple metastasis site, these
#           are present as a string of characters within one data cell....
#           This will likely pose an issue/challenge within data cleaning.
#
#*******************************************************************************

#--------------------- Primary Sites -------------------------------------------

# identifying number and frequency of primary sites
primary.site.freq <- data.frame(table(data$Primary_site))

# visualize the frequency of primary sites.
ggplot(primary.site.freq, x=primary.site.freq$Var1, 
       y= primary.site.freq$Freq)+ 
  geom_col(aes(x=primary.site.freq$Var1,y=primary.site.freq$Freq))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(size =4, angle=45)) 

#*******************************************************************************
# Main Trends/Takeaways:
#
#       - Highest frequency primary site is breast, then colorectum, 
#         prostate and thyroid.
#       - # we have 41 different primary sites 
#
#*******************************************************************************


#===================== Cleaning Data set =======================================

#     This section contains the code used to initially clean the raw data

################################################################################

# FOR this study we are only interested in cancer that has metastasized, so we
# must remove everything that hasn't metasized.

prime.data.a <- data %>%     # we can filter out only row in which the 
  filter(Metastasis_status == "YES")   # Metastasis_status = YES

# REMOVE columns with variables we aren't using.

#Create an object containing varibale names we want to remove.
obs.delete <- c( "Cancer_subtype", "Experiment_id", "Pubmed_id", 
                 "Dataset_id","Platform_id", "Standard", "Class_name", 
                 "Sample_id","Sample_label")

# We can then assign a temporary name to the data set, then remove all varibels
# listed in the object above.
prime.data.b <- prime.data.a[, ! names(prime.data.a) %in% obs.delete, drop = F]

#*******************************************************************************
# Here we deal with the samples for which multiple metastasis sites are listed.

# some of the primary sites are listed with mulitple metastasis sites....
#we have chosen to get rid of rows with multiple metastisis sites for one sample.

prime.data.c <- prime.data.b[!grepl(",", prime.data.b$Metastasis_site),]
# we do this by removing all rows in which the Metastasis_site variable contains
# a "," 
#*******************************************************************************


# REMOVE the unknown catagory, as it does not provide useful insight.
prime.data.d <- prime.data.c[!grepl("unknown", prime.data.c$Metastasis_site),]

# REMOVE any rows with NA values.
prime.data.e <- na.omit(prime.data.d)

# REMOVE spaces from names in columns, this will ensure not issues when using 
# column names in future code.
prime.data.f <- as.data.frame(apply(prime.data.e,2,
                                    function(x)gsub('\\s+', '_',x)))

# CORRECT: kidney is spelt wrong... replace it with correct spelling
prime.data.g <- as.data.frame(apply(prime.data.f,2,function(x)gsub('kindey',
                                                                   'kidney',x)))


# CHECK if remomved all missing values
sum(is.na(prime.data.g))
head(prime.data.g)

# RENAME the final data frame
prime.data <- prime.data.g

# MAKE a frequency table of "Metastasis_site" 
prime.data.h <- data.frame(table(prime.data$Metastasis_site))

# WRITE this file as clean data and save to 2.Clean.Data
write.csv(prime.data.h, paste(path.cd, "Freq_of_Metas_Site_All_Primary.csv"),
          row.names = FALSE)

#====================== Subset by Primary Site =================================

#     We want to look at each Primary tumor site indavidually and the
#     metastatic sites that are associated with each. We also want to add 
#     a column with the weight of each metastatic site. We determined the 
#     weight of sites by deviding the number of occurances at that metastatic 
#     site by the total number of samples with that primary site.

# IDENTIFY all the unique Primary sites
unique(prime.data$Metastasis_site)

# CREATE an object with every unique primary site
primary.site.b<- (unique(prime.data$Primary_site))

# CREATE a for loop that will turn out the frequency and weight for each unique
# primary site
for(i in 1:length(primary.site.b)){
  #i=1
  n<- prime.data %>%
    filter(Primary_site == primary.site.b[i])  # FILTER by desired metastatic
  # site, assign to temporary object
 
  m <- data.frame(table(n$Metastasis_site))  # CREATE frequency table of filtered
  # subset of data, assign to a temporary object 
  m$Weight <- (m$Freq)/(sum(m$Freq)) # CREATE weight column
  
  colnames(m)[1] <- "Metastatic_Site"  # RENAME column
 
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
#*******************************************************************************
# All datasets that are subset by primary site can now be called on by typing:
# "name_of_the_site.data"
# And all frequency tables containing weight values can be accessed by typing:
# "name_of_the_site.freq"
#*******************************************************************************

# MERGE all the frequency/weight data sets to get one master data frame for 
# larger analysis later on.

all.prime.sites.weight <- rbind(breast.freq, colorectum.freq, skin.freq,
                                lung.freq, pancreas.freq, kidney.freq, 
                                oral_cavity.freq, tongue.freq, mandible.freq,
                                FOM.freq, prostate.freq, midgut.freq, 
                                liver.freq,ovary.freq, nasopharynx.freq, 
                                femur.freq, humerus.freq, fibula.freq, 
                                tibia.freq, costa.freq, pelvis.freq, 
                                stomach.freq, testis.freq, thyroid.freq,
                                brain.freq, small_intestine.freq, cervix.freq,
                                penis.freq, adrenal_gland.freq, bladder.freq,
                                esophagus.freq, thymus.freq, uterus.freq, 
                                eye.freq)

# WRITE the file as a CSV and ** SAVE INTO 3.Analaysis FOLDER***
write.csv(all.prime.sites.weight, paste(path.a, "All_Freq_weight_data.csv"))


#====== Data Frame for Metastasis_site = Primary_site Analysis =================

################################################################################
#
#     This section is the code to create the data frame that will be used 
#     in the Analysis.Network script to determine the liklihood of metastasis
#     occuring in the same site as the primary tumor. For this data set we 
#     will first determine the overall frequency of metastasis within the 
#     primary site. We will do this by taking the total number of cases within
#     the data set for which Metastasis_site = Primary_site and devide this
#     by the total number of samples within the data set. This will give us
#     the average prabability of metastasis occuring in the primary site,
#     regardless of what the primary site is. We will then use the same 
#     technique to determine the probability of metastasis_site = Primary_site
#     for each unique Primary site. The probability along with the frequency
#     of metastasis_site = Primary_site, the Primary_site and the total number
#     of incidences at that primary sitewill be recorded for each primary site
#     in the data frame.
#
################################################################################

summary(prime.data)


# SET the two columns in question to characters instead of factors, as the 
# following code requires them to be characters.
prime.data$Primary_site <- as.character(prime.data$Primary_site)
prime.data$Metastasis_site <- as.character(prime.data$Metastasis_site)

# ENUSRE they changed to characters
str(prime.data)

# FILTER for all samples for which the primary site = the metastasis site.
paired.data <- filter(prime.data,Primary_site == Metastasis_site)

# COUNT number of instances for which this is true.
number.paired <- count(filter(prime.data,Primary_site == Metastasis_site))

# DETERMINE the percentage of tumors that metastasize in the primary tumor 
# location by dividing the number paired by the total number of samples.
n <- (number.paired)/ (nrow(prime.data))

#*******************************************************************************
# ASSIGN this to a varible that will be used in the Analysis.Network script
overall.prob <- n[1,1]
#*******************************************************************************

#----------------- REPEAT ABOVE FOR EACH PRIMARY SITE --------------------------

# MAKE an empty dataframe to put the probability values of primary site = 
# metastasis site for each subset of primary site.
paired.prob.subset <- data.frame(matrix(ncol = 7, nrow = length(primary.site.b)))

# CREATE an object with the names of the columns.
k <- c("primary_site", "total_samples", "probability", "ob_same", "P_value",
      "CI_lower", "CI_upper")

# ASSIGN the above names to the 7 columns in your empty data frame.
colnames(paired.prob.subset) <- k


# CREATE a for loop that looks at the likelihood of metastasis in the same site 
# for each primary site. The goal will be to compare this to the overall 
# liklihood to determine if some types of cancer have a higher cancer of 
# metastis in the same location than others.

for(i in 1:(length(primary.site.b))) {

  y<- filter(prime.data, Primary_site == primary.site.b[i]) #filter by primary 
  w <-  count(filter(y, Metastasis_site== primary.site.b[i])) #count paired
  z <- (w)/ (nrow(y)) #devided paired by the total samples with specified primary
  # site
#put number of samples per primary site and probability into dataframe
  paired.prob.subset[i,2] <- nrow(y) 
  paired.prob.subset[i,3] <- z
  paired.prob.subset[i,4] <- w
  paired.prob.subset[i,1] <- paste(primary.site.b[i])


}

# WRITE as a csv, save to the 3.Analysis folder as it will be used specifically 
# for a test.
write.csv( paired.prob.subset, paste(path.a,
                                     "Probability_Same_Sites_subset.csv"))


