library(tidyverse)
library(tidycensus)
library(terra)
library(tigris)
library(sf)

## First, we read the two raster files given to us.

sacramento_landsat_near_infrared = rast("sacramento_landsat_near_infrared.tif")
sacramento_landsat_red = rast("sacramento_landsat_red.tif")

## Then, we'll create a map of the NDVI for the Sacramento region
## by using the formula (R - N)/(R + N)

sacramento_ndvi = (sacramento_landsat_near_infrared - sacramento_landsat_red) /
  (sacramento_landsat_red + sacramento_landsat_near_infrared)

## Note: When I was looking for examples of how this type of analysis had
## been done before in R, I found that NASA says that the NDVI is calculated 
## through the formula (N - R)/(N + R) instead of (R - N)/(R + N).  

## We'll now plot this new raster.

plot(sacramento_ndvi)

## Now, we'll use tidycensus to grab tract-level income
## data for Sacramento County, CA.

## This object will bring together the various statistics
## from the census that add up to create a total number of
## residents in each tract that earn each of the following
## incomes.

my_vars = c(
  income20k_35k = "B25121_032",
  income10k_20k = "B25121_017",
  incomeless10k = "B25121_002"
)

## Then we use the 'get_acs' function to get this information
## from the Census Bureau. By adding the 'gemoetry = TRUE' part,
## we can also get the geometry of each individual census
## tract.

sacramento_income =
  get_acs(geography = "tract", variables = my_vars, state = "CA", county = "Sacramento",
          geometry = TRUE, year = 2019)

## Then we combine the variables discussed above.

sacramento_income_less_than_35k = 
  group_by(sacramento_income, NAME) %>%
  summarize(total_less_than_35k = sum(estimate))

## We then grab the total population numbers for each census
## tract.

sacramento_total_income = 
  get_acs(geography = "tract", variables = "B25121_001", state = "CA", county = "Sacramento",
          year = 2019)

## And we can then join the two together to get a dataset
## that contains the number of households earning less than
## $35,000 per year and the total number of households in
## each tract.

sacramento = left_join(sacramento_income_less_than_35k, sacramento_total_income, ON="NAME")

## Finally, we divide the number of households earning less
## than $35,000 by the total number of households in each
## tract.

sacramento$pct_lessthan35k = (sacramento$total_less_than_35k / sacramento$estimate) * 100

## Next, we create a map showing each census tract
## with different shading depending on the proportion
## of households earning less than $35,000.

ggplot() +
  geom_sf(data=sacramento, aes(fill=pct_lessthan35k, geometry = geometry), lwd=0) +
  scale_fill_fermenter(palette="Blues", n.breaks=6)

## Now, we'll move on to the comparison between NDVI
## the proprtion of low income residents.

## First, we'll turn our previous data into a shapefile
## so we can use it for this section and easily use the
## income data from before.

st_write(sacramento, "sacramento.shp", drive = "ESRI Shapefile")

## Next, we read our new shapefile.

tracts = vect("sacramento.shp")

## Note: I tried initially using a shapefile straight from the
## US Census Bureau website but that ended up not working for
## some reason. When I tried using the data that I had combined
## earlier in this assignment, it worked. No clue why.

## Then, we project our new shapefile to fit the NDVI
## raster.

tracts = project(tracts, sacramento_ndvi)

## Next, we'll create an object that will contain the
## average NDVI for each individual census tract.

bytract = terra::extract(sacramento_ndvi, tracts, mean, na.rm=T)

## Now, we will rename our new NDVI average variable to
## "ndvi".

tracts[bytract$ID, "ndvi"] = bytract$sacramento_landsat_near_infrared

## And finally we can a create a plot to see if there is any 
## correlation between the percentage of low-income households
## and NDVI.

plot(tracts$pct_l35, tracts$ndvi)

## The plot shows that there is perhaps a *slight* negative 
## correlation between NDVI and percentage of the census tracts'
## households earning less than $35k per year.

### Extra Credit ###

## Given California's large immigrant population, I decided to
## compare the Sacramento area's mean NDVI index per tract
## with each tract's foreign born population.

## First, we get the ACS 2019's data on immigrant populations.

sacramento_immigrant = 
  get_acs(geography = "tract", variables = "B05002_013", state = "CA", county = "Sacramento",
        geometry = TRUE, year = 2019)
names(sacramento_immigrant)[4] = "immipop"

## Then, we grab the total population data.

sacramento_total =
  get_acs(geography = "tract", variables = "B01003_001", state = "CA", county = "Sacramento",
          geometry = TRUE, year = 2019)
names(sacramento_total)[4] = "totalpop" 

## Now, we create a new variable in 'sacramento' describing
## the proportion of immigrants in a census tract.

sacramento$pct_immigrant = (sacramento_immigrant$immipop / sacramento_total$totalpop) * 100

## Then, we repeat the steps from earlier to build a plot.

st_write(sacramento, "sacramento2.shp", drive = "ESRI Shapefile")
tracts2 = vect("sacramento2.shp")
tracts2 = project(tracts2, sacramento_ndvi)
bytract2 = terra::extract(sacramento_ndvi, tracts2, mean, na.rm=T)
tracts2[bytract2$ID, "ndvi"] = bytract2$sacramento_landsat_near_infrared
plot(tracts2$pct_mmg, tracts$ndvi)

## The correlation here is much less clear than before.
## There appears to be no/very little correlation between
## a census tract's proportion of immigrant residents
## and the amount of vegetation in each tract.
