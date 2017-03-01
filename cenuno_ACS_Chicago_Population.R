#
# Author: Cristian E. Nuno
# Purpose: Using ACS data to generate maps
# Date: February 28, 2017
#

# install necessary packages
library( plyr )
library( dplyr )
library( tidyr )
library( acs )
library( sp )
library( rgdal )
library( maptools )

#2010 Census Tracts - Source: https://www.cityofchicago.org/city/en/depts/doit/dataset/boundaries_-_censustracts.html
chicago.census.tracts <- readOGR( "https://data.cityofchicago.org/api/geospatial/5jrd-6zik?method=export&format=GeoJSON"
                                  , "OGRGeoJSON"
                                  , stringsAsFactors = FALSE
)
# multiple ways to identify a particular tract
# "tractce10" = 6 digit identifier; "name10" = census tract with decimals
# "geoid10" = 11 digit FIPS code; "namelsad10" = "Census" + "Track" + name10
# 801 census tracts located within the City of Chicago

# Cristian E. Nuno's Census Data API
# To acquire your own Census Data API in 5 minutes, please click here: 
# http://api.census.gov/data/key_signup.html
# I know it look sketchy, but the .gov put me at ease. Plus the federal government only asks for an email.
my.key <- "6b2a3bf0f9ec6f097062213125bc40cad0351578" # Census Data API

# Identify census tracts within Cook County
cook.county.census.tracts <- geo.make(state = "IL"
                                      , county = "Cook"
                                      , tract = "*"
)

# Download ACS 2011-2015 5-Year Estimate Data
# Table number B01003 represents the Population table
# A simple description of different tables does not currently exist
# For more information on use of the 'api' package, please read:
# https://cran.r-project.org/web/packages/acs/acs.pdf
cook.county.acs.data <- acs.fetch( endyear = 2015
                                   , span = 5
                                   , geography = cook.county.census.tracts
                                   , table.number = "B01003"
                                   , key = my.key 
)

# 1319 census tracts located within Cook County

# Limit census tracts to those in Chicago
# Merge these two datasets based on chicago.census.tract's "tractce10" and practice's "tract
chi.cook.merge <- merge( chicago.census.tracts@data
                       , cook.county.acs.data@geography
                       , by.x = "tractce10"
                       , by.y = "tract"
)

# assign new dataframe to newly created dataframe to ACS geography dataframe
cook.county.acs.data@geography <- chi.cook.merge

# create new dataframe from ACS estimate matrix
cook.county.acs.estimates <- as.data.frame( as.table( cook.county.acs.data@estimate ))

# Finally, combine your cleaned census tracts to your estimate dataframe 
chi.cook.cleaned <- merge( cook.county.acs.data@geography
                           , cook.county.acs.estimates
                           , by.x = "NAME"
                           , by.y = "Var1"
)
# The results are glorious:
# str( chi.cook.cleaned )
# 'data.frame':	801 obs. of  14 variables:
# $ NAME      : chr  "Census Tract 1001, Cook County, Illinois" "Census Tract 1002, Cook County, Illinois" "Census Tract 1003, Cook County, Illinois" "Census Tract 1004, Cook County, Illinois" ...
# $ tractce10 : chr  "100100" "100200" "100300" "100400" ...
# $ statefp10 : chr  "17" "17" "17" "17" ...
# $ name10    : chr  "1001" "1002" "1003" "1004" ...
# $ commarea_n: chr  "10" "10" "10" "10" ...
# $ namelsad10: chr  "Census Tract 1001" "Census Tract 1002" "Census Tract 1003" "Census Tract 1004" ...
# $ commarea  : chr  "10" "10" "10" "10" ...
# $ geoid10   : chr  "17031100100" "17031100200" "17031100300" "17031100400" ...
# $ notes     : chr  "" "" "" "" ...
# $ countyfp10: chr  "031" "031" "031" "031" ...
# $ state     : int  17 17 17 17 17 17 17 17 17 17 ...
# $ county    : int  31 31 31 31 31 31 31 31 31 31 ...
# $ Var2      : Factor w/ 1 level "B01003_001": 1 1 1 1 1 1 1 1 1 1 ...
# $ Freq      : num  5285 6424 5783 3061 5617 ...

# To make a map of population data by community area, we need to use a new spatial data frame

# City of Chicago Community Area Boundaries in GeoJSON format
chicago.com.area <- readOGR("https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=GeoJSON"
                            , "OGRGeoJSON"
                            , stringsAsFactors = FALSE
)
# Create new variable representing community area side
# Manually matched side to community area in order as they appear
# in chicago.com.areas variable
# for more on chicago community area sides, read here:
# https://www.lib.uchicago.edu/e/collections/maps/censusinfo.html
community.area.side <- factor( c("SOUTH SIDE", "SOUTH SIDE", "SOUTH SIDE"
                                 , "SOUTH SIDE", "SOUTH SIDE", "FAR NORTH SIDE"
                                 , "SOUTH SIDE", "SOUTH SIDE", "SOUTH SIDE"
                                 , "FAR NORTH SIDE", "FAR NORTH SIDE", "FAR NORTH SIDE"
                                 , "FAR NORTH SIDE", "FAR NORTH SIDE", "NORTHWEST SIDE"
                                 , "NORTHWEST SIDE", "NORTHWEST SIDE", "NORTHWEST SIDE"
                                 , "NORTHWEST SIDE", "FAR NORTH SIDE", "NORTHWEST SIDE"
                                 , "NORTH SIDE", "NORTH SIDE", "WEST SIDE"
                                 , "WEST SIDE", "WEST SIDE", "WEST SIDE"
                                 , "WEST SIDE", "WEST SIDE", "WEST SIDE"
                                 , "FAR NORTH SIDE", "WEST SIDE", "WEST SIDE"
                                 , "CENTRAL", "SOUTH SIDE", "FAR NORTH SIDE"
                                 , "CENTRAL", "CENTRAL", "SOUTH SIDE"
                                 , "FAR SOUTHEAST SIDE", "FAR SOUTHEAST SIDE", "FAR SOUTHEAST SIDE"
                                 , "FAR SOUTHEAST SIDE", "FAR SOUTHEAST SIDE", "FAR SOUTHEAST SIDE"
                                 , "NORTH SIDE", "FAR SOUTHEAST SIDE", "FAR SOUTHEAST SIDE"
                                 , "FAR SOUTHEAST SIDE", "FAR SOUTHEAST SIDE", "FAR SOUTHEAST SIDE"
                                 , "FAR SOUTHEAST SIDE", "SOUTHWEST SIDE", "SOUTHWEST SIDE"
                                 , "SOUTHWEST SIDE", "SOUTHWEST SIDE", "NORTH SIDE"
                                 , "SOUTH SIDE", "SOUTHWEST SIDE", "SOUTHWEST SIDE"
                                 , "SOUTHWEST SIDE", "SOUTHWEST SIDE", "SOUTHWEST SIDE"
                                 , "SOUTHWEST SIDE", "SOUTHWEST SIDE", "SOUTHWEST SIDE"
                                 , "SOUTH SIDE", "NORTH SIDE"
                                 , "FAR SOUTHWEST SIDE", "FAR SOUTHWEST SIDE", "FAR SOUTHWEST SIDE"
                                 , "FAR SOUTHWEST SIDE", "FAR SOUTHWEST SIDE", "FAR SOUTHWEST SIDE"
                                 , "FAR NORTH SIDE", "FAR NORTH SIDE", "FAR NORTH SIDE"
) )
# add community area side variable to geospatial dataset
chicago.com.area@data$community.area.side <- community.area.side

# Now add this cleaned ACS 2015 5-Year Population data about the City of Chicago
# into our City of Chicago community area spatial polygon data frame
head( chicago.com.area@data$area_numbe ) # community area numbers in char format
head( chi.cook.cleaned$commarea_n ) # community area numbers in char format

# merge ACS data and Chicago Community area spatial polygon data
# onto the chicago community area variable
chicago.com.area@data <- merge( chicago.com.area@data
                                , chi.cook.cleaned
                                , by.x = "area_numbe"
                                , by.y = "commarea_n"
)
# check structure to ensure a smooth merge
str( chicago.com.area@data )

# total number of people living in each community area
chi.tapply <- tapply( chicago.com.area@data$Freq
                      , list( community_area = chicago.com.area@data$community)
                      , FUN = sum
)
# convert to data frame
chi.tapply <- as.data.frame( as.table( chi.tapply ))
# assign new column names to avoid confusion with the unaggregated population totals variable "Freq"
colnames( chi.tapply ) <- c( "community_area", "Total_Population")

# merge total population into spatial dataset
chicago.com.area@data <- merge( chicago.com.area@data
                                , chi.tapply
                                , by.x = "community"
                                , by.y = "community_area"
)

# Now all this work was to make an awesome map! 

# Assign colors: light blue is least populated community areas
#            , dark blue is most populated community areas
# color.function.pop <- colorRampPalette( c( "#B3DDF2"
#                                            , "cadetblue4" )
#                                         )
# col.ramp.pop <- color.function.pop( 5 ) # Number of groups is 5

col.schema <- c( "#f1eef6"
                 , "#bdc9e1"
                 , "#74a9cf"
                 , "#2b8cbe"
                 , "#045a8d"
                 )

# Break up the values of the vectors into 5 levels according to their ranks and apply to each level one of the 5 colors.
color.pop <- cut( chicago.com.area@data$Total_Population
                  , breaks = 5
                  , labels = col.schema
                  ) 
color.pop <- as.character( color.pop ) # Convert the assigned colors to each value into character

# Let's finally plot!!
plot( chicago.com.area, col = color.pop )
title( main = "Chicago's Population by Community Area" )
legend.text.pop = c( "2,360 - 21,600"
                     , "21,600 - 40,800"
                     , "40,800 - 59,900"
                     , "59,900 - 79,100"
                     , "79,100 - 98,300"
)
legend( "bottomleft"
        , pch = 20
        , pt.cex = 4
        , cex = 1.3
        , bg = "lightgray"
        , legend = legend.text.pop
        , col = col.schema
        , box.col = "lightgray"
        , title = "Total Population"
)
mtext( side = 1
       , line = 2
       , cex = 0.8
       , adj = 1
       , text = "Source: Cristian E. Nuno | ACS 2011-2015 5-Year Estimates"
)

