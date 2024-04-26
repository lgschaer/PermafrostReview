# Upset Plot

#install.packages("tidyverse", dependencies = TRUE)
#install.packages("knitr", dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)
library(knitr)
library(tidyverse)
library(ggplot2)
#install.packages("ComplexUpset", dependencies = TRUE)
library(ComplexUpset)




#the data
map2 <- read.csv("/Users/lgschaer/Desktop/MURI/PermafrostReview/ord_vars_for_R_V4.csv", row.names = 1) %>%
  filter(Experiment_Lab == 1 & Reactors == 1) %>%
  mutate(across(everything(), ~ifelse(.==0, FALSE,TRUE))) %>%
  rownames_to_column(var = "Article") %>%
  filter(Article != "Nadelhoffer 1991" & Article != "Shaver 2006" & Article != "Kashi 2023") %>%
  #select(-c("Location_Alaska", #"Location_EasternEurope", "Location_Scandinavia", 
  #         "Location_Canada", "Location_Greenland")) 
  select(c("Article", "Site_PeatMoss","Site_Shrubs","Site_BlackSpruce", "Sequenced_MetaT", "Other_ITS", "Other_MicrobialBiomass", "Other_Microbial", "Other_SIP", 
           "Sequenced_16S", "Other_qPCR", "Other_Viral", "Sequenced_MetaG", "Sequenced_Eukaryotes", "GHG_LessThan30Days", "GHG_30.100Days", "GHG_MoreThan100Days",
           "Emissions_NitrousOxide", "Emissions_VOCs", "Emissions_Methane","Emissions_CarbonDioxide", 
           "Temperature_Below3C", "Temperature_3to5C", "Temperature_Above5to10C", "Temperature_Above10C",
           "Incubation_RampUp", "Incubation_SingleTemp", "Incubation_Comparison",
           "Headspace_Helium",  "Headspace_CarbonDioxide", "Headspace_Oxygen", "Headspace_Nitrogen",
           "Sampled_Permafrost", "Sampled_TransitionZone", "Sampled_ActiveLayer"))
head(map2)
colnames(map2)

rownames(map2)

newColNames <- c(
  "Article", "Peat","Shrubs","Black Spruce", "Metatranscriptomic Sequencing", "ITS qPCR", "Microbial Biomass", "Microbial Other", "Stable Isotope Probing", "16S rRNA Sequencing", "Bacterial qPCR",  "Viral", "Metagenomic Sequencing", "Fungal Sequencing",  
  "< 30 Days", "30 - 100 Days", "> 100 Days",
  "Nitrous Oxide", "Volatile Organic Carbon", "Methane","Carbon Dioxide", 
  "< 3 C", "3 - 5 C", "5 - 10 C", "> 10 C",
  "Ramp Up", "Single Temperature", "Compare 2+ Temperatures",
  "Helium Headspace",  "Carbon Dioxide Headspace", "Oxygen Headspace", "Nitrogen Headspace",
  "Permafrost Sampled", "Transition Zone Sampled", "Active Layer Sampled")

colnames(map2) <- newColNames
head(map2)

dim(map2)

#make new df
names_locations <- read.csv("/Users/lgschaer/Desktop/MURI/PermafrostReview/ord_vars_for_R_V4.csv", row.names = 1) %>%
  filter(Experiment_Lab == 1 & Reactors == 1) %>%
  rownames_to_column(var = "Article") %>%
  filter(Article != "Nadelhoffer 1991" & Article != "Shaver 2006" & Article != "Kashi 2023") %>%
  mutate(Location = case_when(
    Location_Alaska == 1 ~ "Alaska",
    Location_Canada == 1 ~ "Canada", 
    Location_EasternEurope == 1 ~ "Eastern Europe", 
    Location_Scandinavia == 1 ~ "Scandinavia",
    Location_Greenland == 1 ~ "Greenland"),
    Row = "One") %>%
  select(c("Article", "Location", "Row"))
head(names_locations)
colnames(names_locations)

dim(names_locations)

map3 <- map2 %>%
  left_join(names_locations, by = "Article") 
head(map3)
dim(map3)
sum(is.na(map3))

colnames(map3)

order <- c(
  "Article", 
   "Viral", "Fungal Sequencing", "Metatranscriptomic Sequencing",  "ITS qPCR",  "Stable Isotope Probing", "Microbial Other", "Metagenomic Sequencing", "Microbial Biomass", "Bacterial qPCR", "16S rRNA Sequencing",   
  "< 30 Days", "30 - 100 Days", "> 100 Days",
  "Volatile Organic Carbon", "Nitrous Oxide", "Methane","Carbon Dioxide", 
  "< 3 C", "3 - 5 C", "5 - 10 C", "> 10 C",
  "Ramp Up", "Single Temperature", "Compare 2+ Temperatures",
  "Helium Headspace",  "Carbon Dioxide Headspace", "Oxygen Headspace", "Nitrogen Headspace",
  "Black Spruce","Shrubs","Peat",
  "Permafrost Sampled", "Transition Zone Sampled", "Active Layer Sampled")

colors6 <- c("darkcyan", "hotpink4", "orange3", "lightskyblue4", "darkblue")

#movies = as.data.frame(ggplot2movies::movies)
#head(movies)
#genres = colnames(movies)[18:24]
#genres
#movies[genres] = movies[genres] == 1
#t(head(movies[genres], 3))
#movies[movies$mpaa == '', 'mpaa'] = NA
#movies = na.omit(movies)

#set_size = function(w, h, factor=1.5) {
 # s = 1 * factor
  #options(
   # repr.plot.width=w * s,
    #repr.plot.height=h * s,
#    repr.plot.res=100 / factor,
 #   jupyter.plot_mimetypes='image/png',
  #  jupyter.plot_scale=1
#  )
#}

#set_size(8, 3)
#upset(movies, genres, name='genre', width_ratio=0.1, themes=list(default=theme()))

#ComplexUpset::upset(map3, order, themes=list(default=theme()))

#set_size(8, 3)
p <- ComplexUpset::upset(
  map3,
  order,
  sort_intersections = "ascending",
  sort_intersections_by = "cardinality",
  matrix=intersection_matrix(geom=geom_point(shape = 22, size = 6)),
  themes=list(default=theme()),
  base_annotations=list(),
  annotations = list(
    "Names"=(
      ggplot2::ggplot(mapping = aes(y = Row))
      +geom_point(aes(fill = Location), shape = 21, size = 6, color = "black")
      +geom_text(label = map3$Article, angle = 90, size = 4)
      +scale_fill_manual(values = colors6)
      +theme_classic()
      +theme(   
        legend.title = element_blank(),  
        legend.position = "top",
        legend.text = element_text(size = 15, hjust = 0.5),
        axis.text.y.left = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank())
    )),
  width_ratio=0.1,
  height_ratio= 5,
  encode_sets = TRUE,
  sort_sets = FALSE,
  queries=list(
    upset_query(set="Active Layer Sampled", color="black", fill ="tan4"),
    upset_query(set="Transition Zone Sampled", color="black", fill ="tan4"),
    upset_query(set="Permafrost Sampled", color="black", fill ="tan4"),
    upset_query(set="16S rRNA Sequencing", color="black", fill ="darkseagreen4"),
    upset_query(set="Metagenomic Sequencing", color="black", fill ="darkseagreen4"),
    upset_query(set="Metatranscriptomic Sequencing", color="black", fill ="darkseagreen4"),
    upset_query(set="Bacterial qPCR", color="black", fill ="darkseagreen4"),
    upset_query(set="ITS qPCR", color="black", fill ="darkseagreen4"),
    upset_query(set="Viral", color="black", fill ="darkseagreen4"),
    upset_query(set="Fungal Sequencing", color="black", fill ="darkseagreen4"),
    upset_query(set="Stable Isotope Probing", color="black", fill ="darkseagreen4"),
    upset_query(set="Microbial Other", color="black", fill ="darkseagreen4"),
    upset_query(set="Microbial Biomass", color="black", fill ="darkseagreen4"),
    upset_query(set="Methane", color="black", fill ="cornsilk2"),
    upset_query(set="Carbon Dioxide", color="black", fill ="cornsilk2"),
    upset_query(set="Volatile Organic Carbon", color="black", fill ="cornsilk2"),
    upset_query(set="Nitrous Oxide", color="black", fill ="cornsilk2"),
    upset_query(set="Single Temperature", color="black", fill ="gold2"),
    upset_query(set="Ramp Up", color="black", fill ="gold2"),
    upset_query(set="Compare 2+ Temperatures", color="black", fill ="gold2"),
    upset_query(set="< 3 C", color="black", fill ="tomato3"),
    upset_query(set="3 - 5 C", color="black", fill ="tomato3"),
    upset_query(set="5 - 10 C", color="black", fill ="tomato3"),
    upset_query(set="> 10 C", color="black", fill ="tomato3"),
    upset_query(set="Helium Headspace", color="black", fill ="lightskyblue2"),
    upset_query(set="Nitrogen Headspace", color="black", fill ="lightskyblue2"),
    upset_query(set="Oxygen Headspace", color="black", fill ="lightskyblue2"),
    upset_query(set="Carbon Dioxide Headspace", color="black", fill ="lightskyblue2"),
    upset_query(set="< 30 Days", color="black", fill ="pink3"),
    upset_query(set="30 - 100 Days", color="black", fill ="pink3"),
    upset_query(set="> 100 Days", color="black", fill ="pink3"),
    upset_query(set="Black Spruce", color="black", fill ="darkgreen"),
    upset_query(set="Shrubs", color="black", fill ="darkgreen"),
    upset_query(set="Peat", color="black", fill ="darkgreen")
  )
)

p



#summary(upset)


#http://127.0.0.1:8081/graphics/plot_zoom_png?width=1352&height=1200


### Summary of fluxes

#load packages
library(tidyverse)
library(ggh4x)

#load data
flux <- read.csv("/Users/lgschaer/Desktop/MURI/PermafrostReview/PF_Review_03132024/flux_data_compilation.csv", row.names = NULL) %>%
  mutate(Temperature_C = factor(Temperature_C, levels = c("-20", "-10", "-3", "-2", "-1", "0", "2", "3", "4", "5", "6", "8", "10", "11", "12", "15", "16", "20"))#,
         #Temperature_binned = case_when(Temperature_C == "-20" | Temperature_C == "-2" | Temperature_C == "-1" ~ "< 3 C",
         #                              Temperature_C == "4" | Temperature_C == "5" ~ "3 - 5 C",
         #                             Temperature_C == "8" |  Temperature_C == "11" | Temperature_C == "15" ~ "> 10 C")
  ) %>%
  unite(Paper, Author, Year, sep = "_", remove = FALSE)
head(flux)
unique(flux$Temperature_C)

CO2_all <- filter(flux, Compound == "CO2")

colors6 <- c("darkcyan", "firebrick", "darkgoldenrod", "darkgreen", "darkblue")

ggplot(CO2_all, aes(x = Sample_Num, y = Rate_mgC_per_gSoil_day))+
  facet_nested(rows = vars(Paper), cols = vars(Depth), scales = "free_y")+
  geom_col(aes(fill = Depth), color = "black", position = "dodge", show.legend = FALSE)+
  scale_fill_manual(values = colors6)+
  xlab("Sample")+
  ylab("mg CO2 / g soil * day")+
  theme_linedraw()+
  theme(   
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.title = element_blank(),                                      #removes legend title
    strip.background = element_rect(fill = "white", color = "black"),  #adds black boarder around legend
    legend.position = "top",
    legend.text = element_text(size = 15),                                 
    axis.text.y = element_text(size = 15, angle = 0, color = "black"),
    axis.title.y = element_text(size = 20, color = "black"),
    axis.title.x = element_text(size = 20, color = "black"),
    axis.text.x = element_blank(), #element_text(size = 15, angle = 0, color = "black"),
    strip.text.x = element_text(face = "bold", size = 20, angle = 0, hjust = 0.5, color = "black"),
    strip.text.y = element_text(face = "bold", size = 20, angle = 0, hjust = 0.5, color = "black"))+
  guides(fill=guide_legend(nrow = 1,byrow=TRUE))


ggplot(CO2_all, aes(x = Sample_Num, y = Rate_mgC_per_gSoil_day))+
  facet_nested(cols = vars(Temperature_C), scales = "free", shrink = TRUE)+
  geom_point(aes(fill = Depth), color = "black", shape = 21, size = 4)+
  #geom_col(fill = "black", color = "black", position = "dodge", show.legend = FALSE)+
  scale_fill_manual(values = colors6)+
  xlab("Sample")+
  ylab("mg CO2 / g soil * day")+
  theme_linedraw()+
  theme(   
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.title = element_blank(),                                      #removes legend title
    strip.background = element_rect(fill = "white", color = "black"),  #adds black boarder around legend
    legend.position = "top",
    legend.text = element_text(size = 15),                                 
    axis.text.y = element_text(size = 15, angle = 0, color = "black"),
    axis.title.y = element_text(size = 20, color = "black"),
    axis.title.x = element_text(size = 20, color = "black"),
    axis.text.x = element_blank(),#element_text(size = 15, angle = 0, color = "black"),
    strip.text.x = element_text(face = "bold", size = 20, angle = 0, hjust = 0.5, color = "black"),
    strip.text.y = element_text(face = "bold", size = 20, angle = 0, hjust = 0.5, color = "black"))+
  guides(fill=guide_legend(nrow = 1,byrow=TRUE))



colors <- c("firebrick", "orange2", "goldenrod1", "lightgoldenrod", "lightblue1", "lightblue3", "dodgerblue3", "blue1")
#colors <- c("blue4", "blue1", "dodgerblue3", "lightblue3", "lightblue1", "aliceblue")



#### fluxes by depth and compound

flux2 <- flux %>%
  filter(Depth != "TransitionZone") %>%
  filter(Rate_mgC_per_gSoil_day > 0) %>% #remove negative production rates
  group_by(Compound, Depth) %>%
  summarise(
    MeanRate = mean(Rate_mgC_per_gSoil_day),
    MaxRate = max(Rate_mgC_per_gSoil_day),
    MinRate = min(Rate_mgC_per_gSoil_day)
  )

flux2_CH4 <- filter(flux2, Compound == "CH4")
flux2_CO2 <- filter(flux2, Compound == "CO2")

colors6 <- c("darkcyan", "firebrick", "darkgoldenrod", "darkgreen", "darkblue")

ggplot(flux2_CO2, aes(x = Depth, y = MeanRate))+
  geom_col(aes(fill = Depth), color = "black", position = "dodge", show.legend = FALSE)+
  scale_fill_manual(values = colors6)+
  xlab("Depth")+
  ylab("mg CO2 / g soil * day")+
  theme_linedraw()+
  theme(   
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.title = element_blank(),                                      #removes legend title
    strip.background = element_rect(fill = "white", color = "black"),  #adds black boarder around legend
    legend.position = "top",
    legend.text = element_text(size = 15),                                 
    axis.text.y = element_text(size = 15, angle = 0, color = "black"),
    axis.title.y = element_text(size = 20, color = "black"),
    axis.title.x = element_text(size = 20, color = "black"),
    axis.text.x = element_text(size = 15, angle = 0, color = "black"),
    strip.text.x = element_text(face = "bold", size = 20, angle = 0, hjust = 0.5, color = "black"),
    strip.text.y = element_text(face = "bold", size = 20, angle = 270, hjust = 0.5, color = "black"))+
  guides(fill=guide_legend(nrow = 1,byrow=TRUE))

ggplot(flux2_CH4, aes(x = Depth, y = MeanRate))+
  geom_col(aes(fill = Depth), color = "black", position = "dodge", show.legend = FALSE)+
  scale_fill_manual(values = colors6)+
  xlab("Depth")+
  ylab("mg CH4 / g soil * day")+
  theme_linedraw()+
  theme(   
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.title = element_blank(),                                      #removes legend title
    strip.background = element_rect(fill = "white", color = "black"),  #adds black boarder around legend
    legend.position = "top",
    legend.text = element_text(size = 15),                                 
    axis.text.y = element_text(size = 15, angle = 0, color = "black"),
    axis.title.y = element_text(size = 20, color = "black"),
    axis.title.x = element_text(size = 20, color = "black"),
    axis.text.x = element_text(size = 15, angle = 0, color = "black"),
    strip.text.x = element_text(face = "bold", size = 20, angle = 0, hjust = 0.5, color = "black"),
    strip.text.y = element_text(face = "bold", size = 20, angle = 270, hjust = 0.5, color = "black"))+
  guides(fill=guide_legend(nrow = 1,byrow=TRUE))


### CO2 by temperature

range(flux3$Cumulative_mgC_per_gSoil)
unique(flux3$Temperature_C)
#  -2  -3  0   2   3   4   5   6   8   10  11  12  15  16  20 

flux3 <- flux %>%
  filter(Compound != "CH4") %>%
  filter(Cumulative_mgC_per_gSoil > 0) %>%
  mutate(
    Temperature_Groups = case_when(
      Temperature_C == "-10" | Temperature_C == "-3" | Temperature_C == "-2" | Temperature_C == "0" |
        Temperature_C == "2" ~ "<3 C",
      Temperature_C == "3" | Temperature_C == "4" | Temperature_C == "5" ~ "3 - 5 C",
      Temperature_C == "6" | Temperature_C == "8" | Temperature_C == "10" ~ "6 - 10 C",
      Temperature_C == "11" | Temperature_C == "12" | Temperature_C == "15" | 
        Temperature_C == "16" | Temperature_C == "20" ~ ">10 C"),
    Temperature_Groups = factor(Temperature_Groups, levels = c("<3 C", "3 - 5 C", "6 - 10 C", ">10 C"))) %>%
  group_by(Temperature_Groups) %>%
  summarise(
    MeanCumulative = mean(Cumulative_mgC_per_gSoil),
    MaxCumulative = max(Cumulative_mgC_per_gSoil),
    MinCumulative = min(Cumulative_mgC_per_gSoil),
    MeanRate = mean(Rate_mgC_per_gSoil_day),
    MaxRate = max(Rate_mgC_per_gSoil_day),
    MinRate = min(Rate_mgC_per_gSoil_day)
  )
head(flux3)

colors6 <- c("lightblue", "lightgoldenrod", "orange", "firebrick")

ggplot(flux3, aes(x = Temperature_Groups, y = MeanRate))+
  geom_point(aes(fill = Temperature_Groups), size = 6, color = "black", shape = 21, show.legend = FALSE)+
  scale_fill_manual(values = colors6)+
  xlab("Temperature")+
  ylab("mg CO2 / g soil * day")+
  theme_linedraw()+
  theme(   
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.title = element_blank(),                                      #removes legend title
    strip.background = element_rect(fill = "white", color = "black"),  #adds black boarder around legend
    legend.position = "top",
    legend.text = element_text(size = 15),                                 
    axis.text.y = element_text(size = 15, angle = 0, color = "black"),
    axis.title.y = element_text(size = 20, color = "black"),
    axis.title.x = element_text(size = 20, color = "black"),
    axis.text.x = element_text(size = 15, angle = 0, color = "black"),
    strip.text.x = element_text(face = "bold", size = 20, angle = 0, hjust = 0.5, color = "black"),
    strip.text.y = element_text(face = "bold", size = 20, angle = 270, hjust = 0.5, color = "black"))+
  guides(fill=guide_legend(nrow = 1,byrow=TRUE))



### Soil Type


#range(flux3$Cumulative_mgC_per_gSoil)
unique(flux3$Soil_Description)

flux4 <- flux %>%
  filter(Compound != "CH4") %>%
  filter(Soil_Description == "Mineral" | Soil_Description == "Organic") %>%
  filter(Cumulative_mgC_per_gSoil > 0) %>%
  group_by(Soil_Description) %>%
  summarise(
    MeanCumulative = mean(Cumulative_mgC_per_gSoil),
    MaxCumulative = max(Cumulative_mgC_per_gSoil),
    MinCumulative = min(Cumulative_mgC_per_gSoil),
    MeanRate = mean(Rate_mgC_per_gSoil_day),
    MaxRate = max(Rate_mgC_per_gSoil_day),
    MinRate = min(Rate_mgC_per_gSoil_day)
  )
head(flux4)

colors6 <- c("darkgreen", "maroon4", "grey56")

ggplot(flux4, aes(x = Soil_Description, y = MeanRate))+
  geom_col(aes(fill = Soil_Description), color = "black", show.legend = FALSE)+
  scale_fill_manual(values = colors6)+
  xlab("Soil Description")+
  ylab("mg CO2 / g soil * day")+
  theme_linedraw()+
  theme(   
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.title = element_blank(),                                      #removes legend title
    strip.background = element_rect(fill = "white", color = "black"),  #adds black boarder around legend
    legend.position = "top",
    legend.text = element_text(size = 15),                                 
    axis.text.y = element_text(size = 15, angle = 0, color = "black"),
    axis.title.y = element_text(size = 20, color = "black"),
    axis.title.x = element_text(size = 20, color = "black"),
    axis.text.x = element_text(size = 15, angle = 0, color = "black"),
    strip.text.x = element_text(face = "bold", size = 20, angle = 0, hjust = 0.5, color = "black"),
    strip.text.y = element_text(face = "bold", size = 20, angle = 270, hjust = 0.5, color = "black"))+
  guides(fill=guide_legend(nrow = 1,byrow=TRUE))



### oxygen


#range(flux3$Cumulative_mgC_per_gSoil)
unique(flux3$Soil_Description)

flux4 <- flux %>%
  filter(Compound != "CH4") %>%
  filter(Cumulative_mgC_per_gSoil > 0) %>%
  group_by(Oxygen, Compound) %>%
  summarise(
    MeanCumulative = mean(Cumulative_mgC_per_gSoil),
    MaxCumulative = max(Cumulative_mgC_per_gSoil),
    MinCumulative = min(Cumulative_mgC_per_gSoil),
    MeanRate = mean(Rate_mgC_per_gSoil_day),
    MaxRate = max(Rate_mgC_per_gSoil_day),
    MinRate = min(Rate_mgC_per_gSoil_day)
  )
head(flux4)

colors6 <- c("aliceblue", "darkseagreen3")

ggplot(flux4, aes(x = Oxygen, y = MeanRate))+
  #facet_nested(cols = vars(Compound), scales = "free", space = "free")+
  geom_col(aes(fill = Oxygen), color = "black", show.legend = FALSE)+
  scale_fill_manual(values = colors6)+
  xlab("Oxygen")+
  ylab("mg CO2 / g soil * day")+
  theme_linedraw()+
  theme(   
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.title = element_blank(),                                      #removes legend title
    strip.background = element_rect(fill = "white", color = "black"),  #adds black boarder around legend
    legend.position = "top",
    legend.text = element_text(size = 15),                                 
    axis.text.y = element_text(size = 15, angle = 0, color = "black"),
    axis.title.y = element_text(size = 20, color = "black"),
    axis.title.x = element_text(size = 20, color = "black"),
    axis.text.x = element_text(size = 15, angle = 0, color = "black"),
    strip.text.x = element_text(face = "bold", size = 20, angle = 0, hjust = 0.5, color = "black"),
    strip.text.y = element_text(face = "bold", size = 20, angle = 270, hjust = 0.5, color = "black"))+
  guides(fill=guide_legend(nrow = 1,byrow=TRUE))


flux_CO2only <- flux %>%
  filter(Compound != "CH4")

flux_CH4only <- flux %>%
  filter(Compound == "CH4")

dim(flux_CO2only)
sum(flux_CO2only$Oxygen=="Anaerobic")
sum(flux_CO2only$Oxygen=="Aerobic")
136+107 #should be 243

sum(flux_CO2only$Depth=="ActiveLayer")
sum(flux_CO2only$Depth=="Permafrost")
sum(flux_CO2only$Depth=="TransitionZone")
122+119+2 #should be 243

dim(flux_CH4only)
sum(flux_CH4only$Depth=="ActiveLayer")
sum(flux_CH4only$Depth=="Permafrost")
sum(flux_CH4only$Depth=="TransitionZone")
91+20+0 #should be 111

sum(flux$Compound=="CO2")
sum(flux$Compound=="CH4")
243+111 


flux3 <- flux %>%
  filter(Compound != "CH4") %>%
  filter(Cumulative_mgC_per_gSoil > 0) %>%
  mutate(
    Temperature_Groups = case_when(
      Temperature_C == "-10" | Temperature_C == "-3" | Temperature_C == "-2" | Temperature_C == "0" |
        Temperature_C == "2" ~ "<3 C",
      Temperature_C == "3" | Temperature_C == "4" | Temperature_C == "5" ~ "3 - 5 C",
      Temperature_C == "6" | Temperature_C == "8" | Temperature_C == "10" ~ "6 - 10 C",
      Temperature_C == "11" | Temperature_C == "12" | Temperature_C == "15" | 
        Temperature_C == "16" | Temperature_C == "20" ~ ">10 C"),
    Temperature_Groups = factor(Temperature_Groups, levels = c("<3 C", "3 - 5 C", "6 - 10 C", ">10 C")))

dim(flux3)
sum(flux3$Temperature_Groups=="<3 C")
sum(flux3$Temperature_Groups=="3 - 5 C")
sum(flux3$Temperature_Groups=="6 - 10 C")
sum(flux3$Temperature_Groups==">10 C")
22+66+31+63 #should be 182

flux_filt <- flux %>%
  filter(Soil_Description=="Organic"|Soil_Description=="Mineral")
head(flux_filt)
dim(flux_filt)
sum(flux_filt$Soil_Description=="Organic")
sum(flux_filt$Soil_Description=="Mineral")
27+47 #should be 74
unique(flux_filt$Paper)
