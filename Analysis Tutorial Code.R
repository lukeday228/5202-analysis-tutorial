

# NLA Analysis Tutorial ---------------------------------------------------

#load packages
library(tidyverse)


# Organizing NLA 2017 Data ---------------------------------------------------

#### Water chemistry data 

#read in water chemistry file
waterchem_NLA17 <- 
  read_csv("https://raw.githubusercontent.com/lukeday228/5202-analysis-tutorial/main/nla_2017_water_chemistry_chla-data.csv")

#select relevant columns to make it more readable
long_waterchem_NLA17 <- 
  waterchem_NLA17 |> 
  select(UID, SITE_ID, VISIT_NO, ANALYTE, RESULT)

#filter to only show desired ANALYTES
shorter_waterchem_NLA17 <- 
  long_waterchem_NLA17 |> 
  filter(ANALYTE == "CHLA" | ANALYTE == "TURB" | ANALYTE == "NTL" | ANALYTE == "PTL")

#pivot wider to make ANALYTES their own columns
wide_waterchem_NLA17 <- 
  shorter_waterchem_NLA17 |> 
  pivot_wider(names_from = ANALYTE, values_from = RESULT)



#### Land Metrics (ag)

#read in land metrics file
landmets_NLA17 <- 
  read_csv("https://raw.githubusercontent.com/lukeday228/5202-analysis-tutorial/main/nla2017_landMets.csv")

#select relevant columns to make it more readable
ag_landmets_NLA17 <- 
  landmets_NLA17 |> 
  select(UID, 
         SITE_ID, 
         VISIT_NO, 
         MANURE, 
         PCTAG2006SLP10, 
         PCTAG2006SLP20, 
         PCTCROP2006, 
         PCTCROP2011)



#### Secchi data

#read in secchi depth file
secchi_NLA17 <- 
  read_csv("https://raw.githubusercontent.com/lukeday228/5202-analysis-tutorial/main/nla_2017_secchi-data.csv")

#select relevant columns
mysecchi_NLA17 <- 
  secchi_NLA17 |> 
  select(UID, 
         SITE_ID, 
         VISIT_NO, 
         LAT_DD83, 
         LON_DD83, 
         DISAPPEARS, 
         INDEX_SITE_DEPTH)



#### Combine all my trimmed data ####

# Merge waterchem and mysecchi
merged_df <- 
  merge(wide_waterchem_NLA17, mysecchi_NLA17, by = c("SITE_ID", "VISIT_NO"))

# Merge with ag_landmets
mydata_NLA17 <- 
  merge(merged_df, ag_landmets_NLA17, by = c("SITE_ID", "VISIT_NO"))

#display final combined data frame
view(mydata_NLA17)





# Time to make some plots! ------------------------------------------------

#usa map plot data
us <- map_data("usa")


#CHLA map
ggplot() +
  geom_polygon(data = us, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  coord_quickmap() +
  geom_point(data = mydata_NLA17, shape = 19, aes(x = LON_DD83, y = LAT_DD83, color = CHLA)) +
  scale_colour_viridis_c() +
  theme_void() +
  ggtitle("Chlorophyll A") +
  theme(plot.title = element_text(hjust = 0.25))

#CHLA < 400 UG/L map
ggplot() +
  geom_polygon(data = us, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  coord_quickmap() +
  geom_point(data = filter(mydata_NLA17, CHLA < 400), shape = 19, aes(x = LON_DD83, y = LAT_DD83, color = CHLA)) +
  scale_colour_viridis_c() +
  theme_void() +
  ggtitle("Chlorophyll A < 400 UG/L") +
  theme(plot.title = element_text(hjust = 0.25))

#CHLA < 50 UG/L map
ggplot() +
  geom_polygon(data = us, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  coord_quickmap() +
  geom_point(data = filter(mydata_NLA17, CHLA < 50), shape = 19, aes(x = LON_DD83, y = LAT_DD83, color = CHLA)) +
  scale_colour_viridis_c() +
  theme_void() +
  ggtitle("Chlorophyll A < 50 UG/L") +
  theme(plot.title = element_text(hjust = 0.25))



# TURB < 100 NTU map
ggplot() +
  geom_polygon(data = us, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  coord_quickmap() +
  geom_point(data = filter(mydata_NLA17, TURB < 100), shape = 19, aes(x = LON_DD83, y = LAT_DD83, color = TURB)) +
  scale_colour_viridis_c() +
  theme_void() +
  ggtitle("TURB < 100 NTU") +
  theme(plot.title = element_text(hjust = 0.25))



# NTL map < 15 mg/L
ggplot() +
  geom_polygon(data = us, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  coord_quickmap() +
  geom_point(data = filter(mydata_NLA17, NTL < 15), shape = 19, aes(x = LON_DD83, y = LAT_DD83, color = NTL)) +
  scale_colour_viridis_c() +
  theme_void()  +
  ggtitle("Total Nitrogen < 15 mg/L") +
  theme(plot.title = element_text(hjust = 0.25))



# PTL < 3000 gu/L map
ggplot() +
  geom_polygon(data = us, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  coord_quickmap() +
  geom_point(data = filter(mydata_NLA17, PTL <3000), shape = 19, aes(x = LON_DD83, y = LAT_DD83, color = PTL)) +
  scale_colour_viridis_c() +
  theme_void() +
  ggtitle("Total Phosphorus < 3000 ug/L") +
  theme(plot.title = element_text(hjust = 0.25))



# PCT CROP 2011 map
ggplot() +
  geom_polygon(data = us, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  coord_quickmap() +
  geom_point(data = mydata_NLA17, shape = 19, aes(x = LON_DD83, y = LAT_DD83, color = PCTCROP2011)) +
  scale_colour_viridis_c() +
  theme_void() +
  ggtitle("Percent Cropland in Watershed") +
  theme(plot.title = element_text(hjust = 0.25))



# plot MANURE vs PTL (clean outliers)
ggplot(data = mydata_NLA17, aes(x = MANURE, y = PTL)) +
  geom_point() +
  theme_light()  +
  ggtitle("MANURE vs PTL") +
  theme(plot.title = element_text(hjust = 0.25))



# plot CHLA vs PCTCROP2011 
ggplot(data = mydata_NLA17, aes(x = NTL, y = CHLA)) +
  geom_point() +
  theme_light() +
  ggtitle("CHLA vs PCTCROP2011") +
  theme(plot.title = element_text(hjust = 0.25))

# plot CHLA vs PCTCROP2011 (clean outliers)
ggplot(data = filter(mydata_NLA17, CHLA < 400 & NTL < 15), aes(x = NTL, y = CHLA)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_light() +
  ggtitle("CHLA vs PCTCROP2011") +
  theme(plot.title = element_text(hjust = 0.25))








