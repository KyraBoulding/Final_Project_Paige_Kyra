################################################################################
#                                                                              #
#                              Analysis Network                                #
#                                                                              #
################################################################################

#*******************************************************************************
# TABLE OF CONTENTS
#
#   - Overview
#   - Visualizing Weights with Heat Map
#   - Binomial Test
#   - Visualization Network
#
#*******************************************************************************

################################################################################
#
#     This script contians the code for a portion of the analysis of the 
#     cleaned data. The code visualizes the weighted relationship between
#     primary sites and metastastic sites to look for over-arching trends
#     in metastis. The script also deals with the analysis of the 
#     probability of metastasis occuring in the primary tumor site.
#
################################################################################


#=================== Visulaizing Weights with Heat Map ========================

#   This section contains code that creates a heat map of primary sites 
#   plotted against their metastasis sites. The colour gradient represents the
#   weight given to each metastasis site for its corrisonding primary site.
#   This heat map will allow detection of any over arching patterns in 
#   metastasis site. For example this heat map would show if the liver happend
#   to be the most common metastasis site regardless of primary site.
#   A reminder that the weight was determined in the Data.Cleaning.1 Script 
#   and reffers to the porportion of samples from the associated primary site
#   that metastasized in that given metastasis site.


# VISUALIZE the weight of each metastic site corrisonding with each primary site
heat.map.weight <- ggplot(all.prime.sites.weight, 
                          aes(Primary_site, Metastatic_Site))+
  geom_raster(aes(fill = Weight))+
  labs(title ="Heat Map", x = "Primary Site", y = "Metastatic Site")+
  scale_fill_continuous(name = "Weight",  breaks = c(1.0, 0.75,0.5, 0.25, 0),
                        labels = c("1.0", "0.75", "0.5", "0.25", "0"),
                        low = "light blue", high = "navy")+  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(size =8, angle=45, vjust = 0.5))
heat.map.weight
# REMINDER that the higher the weight value the more offten that primary site 
# resulted in a metastasis in the corrisonding metastasis site.

# WRITE as a pdf
pdf(paste(path.g, "HeatMap_AllPrimary.pdf"))
plot(heat.map.weight)
dev.off()

#============================= Binomial Test ===================================

#   The goal of this section is to examine whether certain primary sites have
#   higher probability than average of the metastasis occuring in the same site
#   as the primary tumor. We determined the probability of the primary and 
#   metastasis site being the same for all primary sites in the 
#   Data.Cleaning.1 Script (overall.prob). To compare this probability to the
#   probability within each primary site we will use a two-tailed binomal test.
#   This test will tell us whether there is significant difference between the 
#   probability we found for all primary sites and the ones found for each 
#   indavidual primary site. 
#   This section of code used the "paired.prob.subset" data frame that was 
#   produced in the Data.Cleaning.1 script specifically for this test. 


# CREATE a four loop that runs the binom.test for each row in the 
# paired.prob.subset data frame. This for loop will also apply the Bonferroni
# correction to the p-vaules produce in order to correct for multiple 
# association tests being exicuted. Finally the loop will place the corrected
# p-value, the lower end of the CI and the upper end of the CI into the empty
# columns of the paired.prob.subset data frame.
for(i in 1:nrow(paired.prob.subset)){

  l<- binom.test(paired.prob.subset[i,4], paired.prob.subset[i,2],
                 overall.prob, alternative = "two.sided" )
  
  p.corrected<- (l$p.value)*(nrow(paired.prob.subset))  #making the correction
  # to the p-value for multiple comparisons. We are using Bonferroni which 
  # multiplies the p-value by the number of comparisons.
  
  paired.prob.subset[i,5] <- p.corrected #fill column in the table established in 
  # Data.Cleaning.1 Script
  paired.prob.subset[i,6] <- l$conf.int[1]
  paired.prob.subset[i,7] <- l$conf.int[2]
  
}

# WRITE the completed data frame as a csv
write.csv(paired.prob.subset, paste(path.a, "Same_Site_Prob.csv"))

# SUBSET the primary sites with significant p-values.
subset.pval.prob <- paired.prob.subset%>%
  filter(P_value < Alpha.val)%>%
 select(primary_site, probability, P_value, CI_lower,CI_upper,total_samples)
#select only mentioned columns to not have a cluttered table



# CREATE a table with all the primary sites with significantly different 
# probabilities of haveing metastasis occur in the same site as the primary 
# tumor.
#*** ENSURE YOU CLEAR PLOTS FIRST OTHERWISE IT WILL PLOT THE TABLE OVER TOP ****
table.c<- grid.table(subset.pval.prob[1:9, 1:6], theme = ttheme_default())

# SAVE as a pdf
#***** NOTE ******
# this code does not work to save this table. It either saves a table with cut
# off sides or an empty pdf. Despite many different appraoches we could not find
# a way to save the table. As a result a screen shot version of the table has 
# been included in the Table folder of this repo.
pdf(paste(path.t, "Significant_Probs.pdf"))
plot(table.c)
dev.off()


#================== Visualization Network ======================================

#   The following code creates a bipartite network to display the connections
#   between primary and metastasis sites. The idea behind it is that the 
#   primary sites (red squares) would be linked to the their corrisponding 
#   metastasis sites (blue circles) by lines whose widths were representative 
#   of their weight value. It would essentially be an alternative way to
#   display the relationships in the heat map.
#   We were unable to manipulate the graph to the point at which it could be 
#   usable for the paper, due to time constraints.
#   However even in the messy state it conveys the high frequency of certian
#   metastasis site almost better than the heat map. It also shows that certain
#   primary sites were only recorded metastasizing to one or two locations.


# CREATE bipartite weighted network

data.for.graph <- all.prime.sites.weight%>%
  select(Primary_site, Metastatic_Site, Weight) # set up the desired data in the
# correct format for the visual

names(data.for.graph)[3] <- "weight"  # idneitfy the weight variable
g <- graph.data.frame(data.for.graph, directed = F)
V(g)$type <- V(g)$name %in% data.for.graph[,2] 
#the second column of edges is TRUE type

coords <- layout_as_bipartite(g)  # set the layout coordinates

V(g)$color <- V(g)$type
V(g)$color=gsub("FALSE","salmon",V(g)$color) # set colour/shape of primary sites
V(g)$color=gsub("TRUE","light blue",V(g)$color) # metastasis sites
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
plot(g, edge.color="gray30",edge.width=E(g)$weight, layout=coords,
     vertex.label.cex = 0.8, vertex.label.color = "black", 
     vertext.lable.dis = c(rep(4, length(coords))),
     vertex.size = 7, asp=0.3) 









