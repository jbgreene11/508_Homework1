# 508_Homework1
Homework 1 for Public Policy Analytics

# MUSA 508 Lab code - 
# Why Start With Indicators
# 9/7/2020

# Note that 2000 decennial census API endpoints are down at the moment,
# This code replaces 2000 API data with 2009 so that the demo runs smoothly
# In the event the API is not up by class time

# Please consult the original Bookdown for the relevant content to contextualize
# This code - https://urbanspatial.github.io/PublicPolicyAnalytics/

#---- Set Up ----

# Load Libraries

library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)

options(scipen=999)
options(tigris_class = "sf")

# ---- Load Styling options -----

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.text.x = element_text(size = 14))
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

# Load Quantile break functions

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}

# Load hexadecimal color palette

palette5 <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")

# Load census API key

census_api_key("c0e7f2bf1ed21adb8eca6c9652036dfd5c6e1040", overwrite = TRUE)

# ---- Year 2009 tracts -----

# We run our year 2000 code using 2009 ACS (and ACS variables from our 2017 list)
# Notice this returns "long" data - let's examine it

tracts09 <-  
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B15001_050E",
                                             "B15001_009E","B19013_001E","B25058_001E",
                                             "B06012_002E"), 
                year=2009, state=25, county=025, geometry=T) %>% 
  st_transform('ESRI:102728')


# Wide data vs long data (and spread vs gather)

# https://www.garrickadenbuie.com/project/tidyexplain/images/tidyr-spread-gather.gif

# Referencing the data by matrix notation, and learning about the data...
# Let's examine each variable and the elements of an sf object

tracts09[1:3,]

# Look at a table of variables

table(tracts09$variable)

# We create a new data frame consisting only of population

totalPop09 <-
  tracts09 %>%
  filter(variable == "B25026_001")

# Let's examine it

nrow(totalPop09)

names(totalPop09)

head(totalPop09)

glimpse(totalPop09)

# Use the base R plotting function to examine it visually

plot(totalPop09)

plot(totalPop09[,5])

# ---- Using ggplot to visualize census data with sf -----

# Each plot adds more and more nuance and information

# Consult the text to understand the symbology schemes

A <- 
  ggplot() +
  geom_sf(data = totalPop09, aes(fill = estimate)) +
  theme(plot.title = element_text(size=22))

B <- 
  ggplot() +
  geom_sf(data = totalPop09, aes(fill = q5(estimate))) +
  theme(plot.title = element_text(size=22))

C <-
  ggplot() +
  geom_sf(data = totalPop09, aes(fill = q5(estimate))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(totalPop09, "estimate"),
                    name = "Total\nPopluation\n(Quintile Breaks)") +
  theme(plot.title = element_text(size=22))

D <- 
  ggplot() +
  geom_sf(data = totalPop09, aes(fill = q5(estimate))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(totalPop09, "estimate"),
                    name = "Popluation\n(Quintile Breaks)") +
  labs(title = "Total Population", subtitle = "Philadelphia; 2009") +
  mapTheme() + theme(plot.title = element_text(size=22))

# Let's "spread" the data into wide form

tracts09 <- 
  tracts09 %>%
  dplyr::select( -NAME, -moe) %>%
  spread(variable, estimate) %>%
  dplyr::select(-geometry) %>%
  rename(TotalPop = B25026_001, 
         Whites = B02001_002,
         FemaleBachelors = B15001_050, 
         MaleBachelors = B15001_009,
         MedHHInc = B19013_001, 
         MedRent = B25058_001,
         TotalPoverty = B06012_002)

st_drop_geometry(tracts09)[1:3,]

# Let's create new rate variables using mutate

tracts09 <- 
  tracts09 %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop, 0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop), 0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2009") %>%
  dplyr::select(-Whites,-FemaleBachelors,-MaleBachelors,-TotalPoverty)

# Tracts 2009 is now complete. Let's grab 2017 tracts and do a congruent
# set of operations

# ---- 2017 Census Data -----

# Notice that we are getting "wide" data here in the first place
# This saves us the trouble of using "spread"

tracts09 <- 
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B15001_050E",
                                             "B15001_009E","B19013_001E","B25058_001E",
                                             "B06012_002E"), 
          year=2009, state=25, county=025, geometry=T, output="wide") %>%
  st_transform('ESRI:102728') %>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         FemaleBachelors = B15001_050E, 
         MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E) %>%
  dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2009") %>%
  dplyr::select(-Whites, -FemaleBachelors, -MaleBachelors, -TotalPoverty) 
  
  
  tracts12 <- 
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B15001_050E",
                                             "B15001_009E","B19013_001E","B25058_001E",
                                             "B06012_002E"), 
          year=2012, state=25, county=025, geometry=T, output="wide") %>%
  st_transform('ESRI:102728') %>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         FemaleBachelors = B15001_050E, 
         MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E) %>%
  dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2012") %>%
  dplyr::select(-Whites, -FemaleBachelors, -MaleBachelors, -TotalPoverty) 
  
  
  tracts12B = tracts12 [-c(79),]

 
  tracts17 <- 
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B15001_050E",
                                             "B15001_009E","B19013_001E","B25058_001E",
                                             "B06012_002E"), 
          year=2017, state=25, county=025, geometry=T, output="wide") %>%
  st_transform('ESRI:102728') %>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         FemaleBachelors = B15001_050E, 
         MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E) %>%
  dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2017") %>%
  dplyr::select(-Whites, -FemaleBachelors, -MaleBachelors, -TotalPoverty) 
  
  

# --- Combining 12 and 17 data ----

allTracts <- rbind(tracts12B,tracts17)


# ---- Wrangling Transit Open Data -----

MBTAStops <- 
    st_read("https://maps-massgis.opendata.arcgis.com/datasets/a9e4d01cbfae407fbf5afe67c5382fde_0.geojson") %>% 
    mutate(Line = "MBTAStops") %>%
    select(STATION, LINE) %>%  
#    st_read("https://opendata.arcgis.com/datasets/2e9037fd5bef406488ffe5bb67d21312_0.geojson") %>%
#      mutate(Line ="Broad_St") %>%
#      select(Station, Line)) %>%
  st_transform(st_crs(tracts12B))  

# Let's visualize it

ggplot() + 
  geom_sf(data=st_union(tracts12B)) +
  geom_sf(data=MBTAStops, 
          aes(colour = LINE), 
          show.legend = "point", size=2) +
  scale_colour_manual(values = c("blue", "blue", "blue", "green", "green", "green", "orange", "orange", "red", "grey")) +
  labs(title="MBTA Stops", 
       subtitle="Boston, MA", 
       caption="Figure 2.5") +
  mapTheme()

# --- Relating SEPTA Stops and Tracts ----

# Create buffers (in feet - note the CRS) around Septa stops -
# Both a buffer for each stop, and a union of the buffers...
# and bind these objects together

MBTABuffers <- 
  rbind(
    st_buffer(MBTAStops, 2640) %>%
      mutate(Legend = "Buffer") %>%
      dplyr::select(Legend),
    st_union(st_buffer(MBTAStops, 2640)) %>%
      st_sf() %>%
      mutate(Legend = "Unioned Buffer"))

# Let's examine both buffers by making a small multiple
# "facet_wrap" plot showing each

ggplot() +
  geom_sf(data=MBTABuffers) +
  geom_sf(data=MBTAStops, show.legend = "point") +
  facet_wrap(~Legend) + 
  labs(caption = "Figure 2.6") +
  mapTheme()

# ---- Spatial operations ----

# Consult the text to understand the difference between these three types of joins
# and discuss which is likely appropriate for this analysis

# Create an sf object with ONLY the unioned buffer
buffer <- filter(MBTABuffers, Legend=="Unioned Buffer")

# Clip the 2009 tracts ... by seeing which tracts intersect (st_intersection)
# with the buffer and clipping out only those areas
clip <- 
  st_intersection(buffer, tracts12B) %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Clip")

# Do a spatial selection to see which tracts touch the buffer
selection <- 
  tracts12B[buffer,] %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Spatial Selection")

# Do a centroid-in-polygon join to see which tracts have their centroid in the buffer
# Note the st_centroid call creating centroids for each feature
selectCentroids <-
  st_centroid(tracts12B)[buffer,] %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(tracts12B, GEOID)) %>%
  st_sf() %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Select by Centroids")

# Exercise - Can you create a small multiple map of the three types of operations?
# Consult the text for some operations you can try
# This is to be done in breakout groups

myData  <- rbind(selectCentroids, clip) %>%
  rbind(., selection)

  ggplot(myData)+
  geom_sf(data = st_union(tracts12B))+
  geom_sf(aes(fill = q5(TotalPop))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(myData, "TotalPop"),
                    name = "Popluation\n(Quintile Breaks)") +
  labs(title = "Total Population", subtitle = "Boston; 2012") +
  facet_wrap(~Selection_Type)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))
  
  
  # 1.3.1 Maps
  
  ggplot(allTracts.group)+
    geom_sf(data = st_union(tracts12B))+
    geom_sf(aes(fill = TOD)) +
    labs(title = "Time/Space Groups") +
    facet_wrap(~year)+
    mapTheme() + 
    theme(plot.title = element_text(size=22))
  
# Map for median rent
  ggplot(allTracts.group)+
    geom_sf(data = st_union(tracts12B))+
    geom_sf(aes(fill = q5(MedRent.inf))) +
    geom_sf(data = buffer, fill = "transparent", color = "red")+
    scale_fill_manual(values = palette5,
                      labels = qBr(allTracts.group, "MedRent.inf"),
                      name = "Rent\n(Quintile Breaks)") +
    labs(title = "Median Rent 2012-2017", subtitle = "Real Dollars") +
    facet_wrap(~year)+
    mapTheme() + 
    theme(plot.title = element_text(size=22))
 
# Map for total population
  ggplot(allTracts.group)+
    geom_sf(data = st_union(tracts12B))+
    geom_sf(aes(fill = q5(TotalPop))) +
    geom_sf(data = buffer, fill = "transparent", color = "red")+
    scale_fill_manual(values = palette5,
                      labels = qBr(allTracts.group, "TotalPop"),
                     name = "Median HH Income\n(Quintile Breaks)") +
    labs(title = "Total Population 2012-2017", subtitle = "Real Dollars") +
    facet_wrap(~year)+
    mapTheme() + 
    theme(plot.title = element_text(size=22))

# Map for median income  
  ggplot(allTracts.group)+
    geom_sf(data = st_union(tracts12B))+
    geom_sf(aes(fill = q5(MedHHInc))) +
    geom_sf(data = buffer, fill = "transparent", color = "red")+
    scale_fill_manual(values = palette5,
                      labels = qBr(allTracts.group, "MedHHInc"),
                     name = "Median HH Income\n(Quintile Breaks)") +
    labs(title = "Median HH Income 2012-2017", subtitle = "Real Dollars") +
    facet_wrap(~year)+
    mapTheme() + 
    theme(plot.title = element_text(size=22))
  
# Map for percent white  
   ggplot(allTracts.group)+
    geom_sf(data = st_union(tracts12B))+
    geom_sf(aes(fill = q5(pctWhite))) +
    geom_sf(data = buffer, fill = "transparent", color = "red")+
    scale_fill_manual(values = palette5,
                      labels = qBr(allTracts.group, "pctWhite"),
                     name = "Percent White\n(Quintile Breaks)") +
    labs(title = "Percent White 2012-2017", subtitle = "%") +
    facet_wrap(~year)+
    mapTheme() + 
    theme(plot.title = element_text(size=22))
  
  
  #1.4 Submarkets
  
# As a map
    
ggplot(allTracts.threeMarkets)+
  geom_sf(data = st_union(tracts12B))+
  geom_sf(aes(fill = Submarket))+
  scale_fill_manual(values = c("orange", "green", "blue", "black"))+
  labs(title = "Three Submarkets As Tracts") +
  mapTheme()

# As a facetted plot

st_drop_geometry(allTracts.threeMarkets) %>%
  group_by(year, Submarket) %>%
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            Percent_White = mean(pctWhite, na.rm = T),
            Percent_Bach = mean(pctBachelors, na.rm = T),
            Percent_Poverty = mean(pctPoverty, na.rm = T)) %>%
  gather(Variable, Value, -year, -Submarket) %>%
  ggplot(aes(year, Value, fill = Submarket)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free", ncol=5) +
  scale_fill_manual(values = c("orange", "green", "blue", "black"))+
  labs(title = "Indicator differences across submarkets") +
  plotTheme() + theme(legend.position="bottom")



# We do our centroid joins as above, and then do a "disjoin" to get the ones that *don't*
# join, and add them all together.
# Do this operation and then examine it.
# What represents the joins/doesn't join dichotomy?
# Note that this contains a correct 2009-2017 inflation calculation

allTracts.group <- 
  rbind(
    st_centroid(allTracts)[buffer,] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "TOD"),
    st_centroid(allTracts)[buffer, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "Non-TOD")) %>%
  mutate(MedRent.inf = ifelse(year == "2009", MedRent * 1.14, MedRent)) 

# Can you try to create the maps seen in the text?
# The solutions are contained in "map_exercise.R"

# --- TOD Indicator Tables ----

allTracts.Summary <- 
  st_drop_geometry(allTracts.group) %>%
  group_by(year, TOD) %>%
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            Income = mean(MedHHInc, na.rm = T),
            Percent_White = mean(pctWhite, na.rm = T))
         

kable(allTracts.Summary) %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2.2")

# Let's make some comparisons and speculate about the willingness to pay
# and demographics in these areas 2009-2017 (see the 2000 data in the text too)

allTracts.Summary %>%
  unite(year.TOD, year, TOD, sep = ": ", remove = T) %>%
  gather(Variable, Value, -year.TOD) %>%
  mutate(Value = round(Value, 2)) %>%
  spread(year.TOD, Value) %>%
  kable() %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2.3")

# --- TOD Indicator Plots ------

# Let's create small multiple plots
# We use the "gather" command (look this one up please)
# To go from wide to long
# Why do we do this??
# Notice we can "pipe" a ggplot call right into this operation!

allTracts.Summary %>%
  gather(Variable, Value, -year, -TOD) %>%
  ggplot(aes(year, Value, fill = TOD)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free", ncol=5) +
  scale_fill_manual(values = c("#bae4bc", "#0868ac")) +
  labs(title = "Indicator differences across time and space") +
  plotTheme() + theme(legend.position="bottom")

# Examining three submarkets

centerCity <-
  st_intersection(
    st_buffer(filter(septaStops, Line == "El"), 2640) %>% st_union(),
    st_buffer(filter(septaStops, Line == "Broad_St"), 2640) %>% st_union()) %>%
  st_sf() %>%
  mutate(Submarket = "Center City")

el <-
  st_buffer(filter(septaStops, Line == "El"), 2640) %>% st_union() %>%
  st_sf() %>%
  st_difference(centerCity) %>%
  mutate(Submarket = "El")

broad.st <-
  st_buffer(filter(septaStops, Line == "Broad_St"), 2640) %>% st_union() %>%
  st_sf() %>%
  st_difference(centerCity) %>%
  mutate(Submarket = "Broad Street")

threeMarkets <- rbind(el, broad.st, centerCity)

# You can then bind these buffers to tracts and map them or make small multiple plots

allTracts.threeMarkets <-
  st_join(st_centroid(allTracts), threeMarkets) %>%
  st_drop_geometry() %>%
  left_join(allTracts) %>%
  mutate(Submarket = replace_na(Submarket, "Non-TOD")) %>%
  st_sf() 

# Graduated symbol map
# Multiple ring buffer
square <-  
    st_sfc(st_polygon(list(cbind(c(0,3,3,0,0),c(0,0,3,3,0))))) %>%
    st_sf()
    
buffers <- multipleRingBuffer(buffer, 52800, 2640)

ggplot() + geom_sf(data = buffers, aes(fill = distance))

# Create ring data frame
allTracts.ring <- 
  rbind(
    st_centroid(allTracts)[buffers(3),] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(Ring = "1"),
    st_centroid(allTracts)[buffers, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(Ring = "Non-Rings")) %>%
  mutate(MedRent.inf = ifelse(year == "2009", MedRent * 1.14, MedRent)) 

# Can you try to create the maps seen in the text?
# The solutions are contained in "map_exercise.R"

# --- TOD Indicator Tables ----

allTracts.Summary <- 
  st_drop_geometry(allTracts.group) %>%
  group_by(year, Ring) %>%
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            Income = mean(MedHHInc, na.rm = T),
            Percent_White = mean(pctWhite, na.rm = T))
         

kable(allTracts.Summary) %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2.2")


#plot geom line
geom_line(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE,


#Create multiple ring buffer function
multipleRingBuffer <- function(inputPolygon, maxDistance, interval) 
{
#create a list of distances that we'll iterate through to create each ring
  distances <- seq(0, maxDistance, interval)
#we'll start with the second value in that list - the first is '0'
  distancesCounter <- 2
#total number of rings we're going to create
  numberOfRings <- floor(maxDistance / interval)
#a counter of number of rings
  numberOfRingsCounter <- 1
#initialize an otuput data frame (that is not an sf)
  allRings <- data.frame()
  
#while number of rings  counteris less than the specified nubmer of rings
  while (numberOfRingsCounter <= numberOfRings) 
  {
#if we're interested in a negative buffer and this is the first buffer
#(ie. not distance = '0' in the distances list)
    if(distances[distancesCounter] < 0 & distancesCounter == 2)
    {
      #buffer the input by the first distance
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #different that buffer from the input polygon to get the first ring
      buffer1_ <- st_difference(inputPolygon, buffer1)
      #cast this sf as a polygon geometry type
      thisRing <- st_cast(buffer1_, "POLYGON")
      #take the last column which is 'geometry'
      thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
      #add a new field, 'distance' so we know how far the distance is for a give ring
      thisRing$distance <- distances[distancesCounter]
    }
    
    
    #otherwise, if this is the second or more ring (and a negative buffer)
    else if(distances[distancesCounter] < 0 & distancesCounter > 2) 
    {
      #buffer by a specific distance
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #create the next smallest buffer
      buffer2 <- st_buffer(inputPolygon, distances[distancesCounter-1])
      #This can then be used to difference out a buffer running from 660 to 1320
      #This works because differencing 1320ft by 660ft = a buffer between 660 & 1320.
      #bc the area after 660ft in buffer2 = NA.
      thisRing <- st_difference(buffer2,buffer1)
      #cast as apolygon
      thisRing <- st_cast(thisRing, "POLYGON")
      #get the last field
      thisRing <- as.data.frame(thisRing$geometry)
      #create the distance field
      thisRing$distance <- distances[distancesCounter]
    }
    
    #Otherwise, if its a positive buffer
    else 
    {
      #Create a positive buffer
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #create a positive buffer that is one distance smaller. So if its the first buffer
      #distance, buffer1_ will = 0. 
      buffer1_ <- st_buffer(inputPolygon, distances[distancesCounter-1])
      #difference the two buffers
      thisRing <- st_difference(buffer1,buffer1_)
      #cast as a polygon
      thisRing <- st_cast(thisRing, "POLYGON")
      #geometry column as a data frame
      thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
      #add teh distance
      thisRing$distance <- distances[distancesCounter]
    }  
    
    #rbind this ring to the rest of the rings
    allRings <- rbind(allRings, thisRing)
    #iterate the distance counter
    distancesCounter <- distancesCounter + 1
    #iterate the number of rings counter
    numberOfRingsCounter <- numberOfRingsCounter + 1
  }
  
  #convert the allRings data frame to an sf data frame
  allRings <- st_as_sf(allRings)
}
