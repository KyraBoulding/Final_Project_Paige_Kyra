################################################################################
#                                                                              #
#                              Analysis MCA                                    #
#                                                                              #
################################################################################

#*******************************************************************************
# TABLE OF CONTENTS
#
#   - Overview
#   - MCA Analysis
#       - Test Set MCA
#       - Applying to Our Data
#           - Breast MCA
#           - MCA Subset of Thyroid and Bladder Primary Cancers
#           - Prime Data MCA
#
#*******************************************************************************
################################################################################
#
#     This script contians our attempt at MCA Analysis which is the
#     equivalent of a PCA Analysis for catagorical varibles. The main
#     purpose of this analysis was to identify which or our variables
#     best explained the metastasis site of cancers. However this test
#     proved difficult to run and even more difficult to analyize and 
#     understand the out put. As a result of the time restraints of this
#     project we were undable to reach a point with this analysis where we
#     could confidently analyize or draw conclusions from the results.
#     However we did do the following work with test data sets to further
#     our understanding of the method and made considerable progess towards
#     utilizing it with our own data set.
#
################################################################################

#============================ ~ MCA ANALYSIS ~ =================================

#     To better understand the MCA analysis we will create a test data set
#     so that we know the relationships that exit between varibles, and can
#     see how those relationships manifest.


#----------------- Test Set MCA ------------------------------------------------

# CREATE a sample data frame. 
primary <- c((rep("kidney", 4)), (rep("breast", 5)), rep("colon", 2))
secondary <- c(rep("liver", 3), (rep("lung", 3)), (rep("rectum", 5)))
class <- c((rep(4, 5)), (rep(2, 4)), (rep(1, 2)))

#*******************************************************************************
# above i made a pattern between the class of the tumour and its primary 
# location while the metastis 
# locations were kept mostly random. 
# i want to see if the graph will show the class value near the primary location
# which i made it 'linked' to.
# So kidney should be located near class 4 and colon near class 1.
#*******************************************************************************


test.data <- data.frame(primary, secondary, class)
# CREATE a data frame with each of my test variables. 
test.data

# in order to use MCA analysis, all variables must be factors rather than 
# characters or numerical data. 
test.data.factor <- test.data %>% mutate_if(is.character,as.factor)
test.data.factor$Freq <- NULL

# now i can view the structure to make sure the mutation was successful
str(test.data.factor)

# for some reason class did not mutate so i will try another method to turn it 
# into a factor
test.data.factor$class <- factor(test.data.factor$class)
str(test.data.factor)
# sucesss


# now i can run the MCA analysis, I would like it to produce a graph so I list 
#it as graph = TRUE
# in order to simplify the analysis i have specified ncp = 2 which will then only
# look at my data in two dimensions
test.mca <- MCA(test.data.factor, graph = TRUE, ncp = 2)

# i can plot the percent of variance explained by each dimention
fviz_screeplot(test.mca, addlabels = TRUE, ylim = c(0, 45))

# the first dimension explaines 45.6% of the variance

#I can now plot the analysis by it cos2 which will show by colour the qulaity of
# the representaion.
fviz_mca_var(test.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE,
             ggtheme = theme_minimal())

#*******************************************************************************
# the results of the graph show that the association between kidney and class 4
# as i had hoped by being close in location on the left side of the graph. 
# colon and class 1 are very close in the upper right corner
# breast and class 2 are also close on the graph show the pattern as I had hoped
# i had made the metastasis sights random and not in a pattern with the other 
# variables and as a result the rectum and lung are not near anything else which 
# makes sense. 
#*******************************************************************************

#========================= Applying to our Data ================================

#     Now we will apply smaller subsets of our data to the MCA.

#----- BREAST MCA --------------------------------------------------------------

# in order to perform MCA analysis all charcter variables need to be converted 
#into factors.
breast.factor <- breast.data %>% mutate_if(is.character,as.factor)
head(breast.data)
str(breast.data)

# for some reason all of the columns were chnaged to factors except Class id, so
#I will run another function on it to change it to a factor
breast.factor$Class_id <- factor(breast.factor$Class_id)

# now i can perform the mca analysis on the transformed breast data. 
breast.mca <- MCA(breast.factor, graph = TRUE, ncp = 2)
                                          # *side note, this runs for Paige but
                                          # Kyra's computer doesn't like it...*
# now I can view each eigen values
eig.val <- get_eigenvalue(breast.mca)

# now i can plot the eigen values, this will show the percentage of variance 
#explained by each dimension
fviz_screeplot(breast.mca, addlabels = TRUE, ylim = c(0, 45))


head(breast.mca$var$cos2)

# i can also plot the cos2 which is the quality of representation of the 
#categories on the factor map is called the squared cosine (cos2).

fviz_mca_var(breast.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE,
             ggtheme = theme_minimal())

#*******************************************************************************
# looking at the graph the only association that is evident with 
# high representation is the metastasis site'other' and the class of 4. 
# i have not yet determined if low repreesntation discredits the proximatey 
# of two variables. 
#*******************************************************************************

#-------- MCA SUBSET OF THYROID AND BLADDER PRIMARY CANCERS --------------------

# now that i have looked at the breast cancer data set, I wanted to expand
# my analysis to two primary sites. 
# I decided to look at thyroid and bladder because they are mid range in the
# amount of tumours

sub.data <- prime.data

#again i want to remove unnesescary columns
sub.data$Cancer_type <- NULL
sub.data$Metastasis_status <- NULL

# now i need to subset the data to only include thyroid and bladder primary 
# site data
sub.data <- sub.data %>%
  filter(Primary_site %in% c("bladder", "thyroid"))
head(sub.data)

# in order to perform MCA analysis all charcter variables need to be converted 
#into factors.
sub.factor <- sub.data %>% mutate_if(is.character,as.factor)
str(sub.factor)
sub.factor$Class_id <- factor(sub.factor$Class_id)

# now i can perfrom the MCA anaylsis
sub.mca <- MCA(sub.factor, graph = TRUE, ncp = 2)
# the graph show none of the colums being related
#now i can loook at the percent of variance explained by each dimenion

fviz_screeplot(sub.mca, addlabels = TRUE, ylim = c(0, 45))
# the first dimension shows that it explains 32.9% of variance

# i can also plot the cos2 which is the quality of representation of the 
#categories on the factor map is called the squared cosine (cos2).
fviz_mca_var(sub.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE,
             ggtheme = theme_minimal())

#*******************************************************************************
# in the mid left there appear to be an association between the thyroid primary 
# site and metastasis in the lymph node.
# there is also a possible association between the bladder primary site and the
# liver metastasis howver the liver is in blue indicating it is a weak 
# representation.
#*******************************************************************************

#------------------- PRIME DATA MCA --------------------------------------------

# in order to perform MCA analysis all charcter variables need to be converted 
# into factors.
prime.data.factor <- prime.data %>% mutate_if(is.character,as.factor)
head(prime.data.factor)

# in order to (attempt) to reduce the clutter i am going to get rid of some
# unnecessary columns such as cancer type and metastasis status
prime.data.factor$Cancer_type <- NULL
prime.data.factor$Metastasis_status <- NULL
str(prime.data.factor)

# for some reason all of the columns were chnaged to factors except Class id, so
# I will run another function on it to change it to a factor
prime.data.factor$Class_id <- factor(prime.data.factor$Class_id)
str(prime.data.factor)

# now i can perform the mca analysis on the transformed breast data. 
prime.mca <- MCA(prime.data.factor, graph = TRUE)

# now I can view each eigen value 
eig.val <- get_eigenvalue(prime.mca)

# now i can plot the eigen values, this will show the percentage of variance 
# explained by each dimension
fviz_screeplot(prime.mca, addlabels = TRUE, ylim = c(0, 45))

# i can also plot the cos2 which is the quality of representation of the 
# categories on the factor map is called the squared cosine (cos2).

fviz_mca_var(prime.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE,
             ggtheme = theme_minimal())

#*******************************************************************************
# in the top right corner you can see some association between the primary site
# thymus and the metastasis site pleura
# in the bottom right we can see association between the primary site ovary and 
# the metastasis site omentum
# all of the blue in the middle appears to be showing that is little association
# between any of the other variables. 
# looking at this plot we cannot really see all of the variables so overall this
# technique would be best used on smaller data sets with fewer variables.
#*******************************************************************************







