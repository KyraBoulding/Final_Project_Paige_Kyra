

# Visulaize the weight of each metastic site corrisonding with each primary site
heat.map.weight <- ggplot(all.prime.sites.weight, aes(Primary_site, Matastatic_Site))+
  geom_raster(aes(fill = Weight))+
  labs(title ="Heat Map", x = "Primary Site", y = "Metastatic Site")+
  scale_fill_continuous(name = "Weight",  breaks = c(1.0, 0.75,0.5, 0.25, 0),
                        labels = c("1.0", "0.75", "0.5", "0.25", "0"),
                        low = "light blue", high = "navy")+  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(size =8, angle=45))



install.packages("igraph")
install.packages("networkD3")
library(igraph)
library(networkD3)

#

forceNetwork(Links = breast.freq, 
             Nodes = breast.freq$Primary_site,
             Source = "Primary_site", Target= "Matastatic_Site", Value = "value", NodeID = "name",
             Group = "Primary_site", opacity = 0.8)





