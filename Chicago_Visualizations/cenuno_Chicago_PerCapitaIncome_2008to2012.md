Chicago's Per Capita Income by Community Area, 2008-2012
================
Cristian E. Nuno
March 16, 2017

Visualizing Spatial Data
------------------------

Maps often translate unfamiliar information into a familiar setting. Using the City of Chicago, I hope that others can use this tutorial to produce other maps with other unfamiliar information.

### Step 1: Load Necessary R Packages

``` r
# Necessary R packages
# Added "message = FALSE" to chunk options to hide the messages that result from calling certain packages.

library( rgdal )
library( sp )
library( rgeos )
library( TeachingDemos )
library( maptools )
library( dplyr )
library( pander )
```

### Step 2: Load Necessary Datasets

In this case, we will be loading two datasets:

1.  2008-2012 ACS 5-Year Estimate Data for Selected Socioeconomic Indicators in Chicago. Here is a [description of the data](https://catalog.data.gov/dataset/census-data-selected-socioeconomic-indicators-in-chicago-2008-2012-36e55).

2.  City of Chicago Community Area Boundaries. Here is a [description of the data](https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6).

For more information on reading and analyzing spatial dataframes take a look at these two introductory documents: [Introduction to Visualizing Spatial Data in R](https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf) and [Applied Spatial Data with R](http://gis.humboldt.edu/OLM/r/Spatial%20Analysis%20With%20R.pdf).

``` r
# Census Data - Selected socioeconomic indicators in Chicago, 2008 – 2012
dat <- read.csv( "https://data.cityofchicago.org/api/views/kn9c-c2s2/rows.csv?accessType=DOWNLOAD"
                 , header = TRUE
                 , stringsAsFactors = FALSE)
# The last row in this dataset is a row representing the entire city
city.wide.info <- tail( dat, n = 1)

# Remove the last row from the dataset
dat <- dat[ -nrow( dat ), ]

# Examine the structure of "dat"
str( dat )
```

    ## 'data.frame':    77 obs. of  9 variables:
    ##  $ Community.Area.Number                       : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ COMMUNITY.AREA.NAME                         : chr  "Rogers Park" "West Ridge" "Uptown" "Lincoln Square" ...
    ##  $ PERCENT.OF.HOUSING.CROWDED                  : num  7.7 7.8 3.8 3.4 0.3 1.1 0.8 1.9 1.1 2 ...
    ##  $ PERCENT.HOUSEHOLDS.BELOW.POVERTY            : num  23.6 17.2 24 10.9 7.5 11.4 12.3 12.9 3.3 5.4 ...
    ##  $ PERCENT.AGED.16..UNEMPLOYED                 : num  8.7 8.8 8.9 8.2 5.2 4.7 5.1 7 6.5 9 ...
    ##  $ PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA: num  18.2 20.8 11.8 13.4 4.5 2.6 3.6 2.5 7.4 11.5 ...
    ##  $ PERCENT.AGED.UNDER.18.OR.OVER.64            : num  27.5 38.5 22.2 25.5 26.2 17 21.5 22.6 35.3 39.5 ...
    ##  $ PER.CAPITA.INCOME                           : int  23939 23040 35787 37524 57123 60058 71551 88669 40959 32875 ...
    ##  $ HARDSHIP.INDEX                              : int  39 46 20 17 6 5 2 1 8 21 ...

``` r
# City of Chicago Community Area Boundaries in GeoJSON format
chicago.com.area <- readOGR("https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=GeoJSON"
                            , "OGRGeoJSON"
                            , stringsAsFactors = FALSE
)
```

    ## OGR data source with driver: GeoJSON 
    ## Source: "https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=GeoJSON", layer: "OGRGeoJSON"
    ## with 77 features
    ## It has 9 fields

``` r
# Transform spatial polygon data frame's "community area number" variable from character to integer
chicago.com.area@data$area_numbe <- as.integer( chicago.com.area@data$area_numbe )
chicago.com.area@data$area_num_1 <- as.integer( chicago.com.area@data$area_num_1 )

# Examine the structure of "chicago.com.area@data"
str( chicago.com.area@data ) #%>% pander
```

    ## 'data.frame':    77 obs. of  9 variables:
    ##  $ community : chr  "DOUGLAS" "OAKLAND" "FULLER PARK" "GRAND BOULEVARD" ...
    ##  $ area      : chr  "0" "0" "0" "0" ...
    ##  $ shape_area: chr  "46004621.1581" "16913961.0408" "19916704.8692" "48492503.1554" ...
    ##  $ perimeter : chr  "0" "0" "0" "0" ...
    ##  $ area_num_1: int  35 36 37 38 39 4 40 41 42 1 ...
    ##  $ area_numbe: int  35 36 37 38 39 4 40 41 42 1 ...
    ##  $ comarea_id: chr  "0" "0" "0" "0" ...
    ##  $ comarea   : chr  "0" "0" "0" "0" ...
    ##  $ shape_len : chr  "31027.0545098" "19565.5061533" "25339.0897503" "28196.8371573" ...

### Step 3: Identify common variable to merge these two datasets

Now that our two datasets are loaded, it is time to identifying a common variable the two share in order to properly merge the datasets. From above, the use of the function str() reveals that the two datasets share a variable identifying community area numbers.

The variable "Community.Area.Number" in the 2008-2012 ACS dataset and the variable "area\_numbe" in the City of Chicago community area boundaries dataset are the two variables to be used when merging these datasets.

#### Note: It is Critical to Merge the Non-Spatial dataset onto the Spatial dataset.

This matters later when we map information. The plot order will not be based on the non-spatial dataset; it will be based on the spatial dataset. Failure to account for this will result in values being plotted in the wrong polygon, ensuring hours of headaches when the colors of the map do not match with the values for each polygon.

*For more information on proper merging, please [read this Stack Overflow thread.](http://stackoverflow.com/questions/3650636/how-to-attach-a-simple-data-frame-to-a-spatialpolygondataframe-in-r)*

``` r
# Refer to the Stack Overflow thread for a better explanation on why I am merging
# the data this way.
chicago.com.area@data <- data.frame( chicago.com.area@data
                                     , dat[
                                       match( chicago.com.area@data[ , "area_numbe" ]
                                              , dat[ , "Community.Area.Number"  ]
                                       )
                                       , ]
)
# take a look at newly merged dataframe to ensure a successful merge
str( chicago.com.area@data ) # 77 obs. of 18 variables
```

    ## 'data.frame':    77 obs. of  18 variables:
    ##  $ community                                   : chr  "DOUGLAS" "OAKLAND" "FULLER PARK" "GRAND BOULEVARD" ...
    ##  $ area                                        : chr  "0" "0" "0" "0" ...
    ##  $ shape_area                                  : chr  "46004621.1581" "16913961.0408" "19916704.8692" "48492503.1554" ...
    ##  $ perimeter                                   : chr  "0" "0" "0" "0" ...
    ##  $ area_num_1                                  : int  35 36 37 38 39 4 40 41 42 1 ...
    ##  $ area_numbe                                  : int  35 36 37 38 39 4 40 41 42 1 ...
    ##  $ comarea_id                                  : chr  "0" "0" "0" "0" ...
    ##  $ comarea                                     : chr  "0" "0" "0" "0" ...
    ##  $ shape_len                                   : chr  "31027.0545098" "19565.5061533" "25339.0897503" "28196.8371573" ...
    ##  $ Community.Area.Number                       : int  35 36 37 38 39 4 40 41 42 1 ...
    ##  $ COMMUNITY.AREA.NAME                         : chr  "Douglas" "Oakland" "Fuller Park" "Grand Boulevard" ...
    ##  $ PERCENT.OF.HOUSING.CROWDED                  : num  1.8 1.3 3.2 3.3 2.4 3.4 5.6 1.5 2.9 7.7 ...
    ##  $ PERCENT.HOUSEHOLDS.BELOW.POVERTY            : num  29.6 39.7 51.2 29.3 21.7 10.9 42.1 18.4 30.7 23.6 ...
    ##  $ PERCENT.AGED.16..UNEMPLOYED                 : num  18.2 28.7 33.9 24.3 15.7 8.2 28.6 8.4 23.4 8.7 ...
    ##  $ PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA: num  14.3 18.4 26.6 15.9 11.3 13.4 25.4 4.3 16.5 18.2 ...
    ##  $ PERCENT.AGED.UNDER.18.OR.OVER.64            : num  30.7 40.4 44.9 39.5 35.4 25.5 42.8 26.2 36.1 27.5 ...
    ##  $ PER.CAPITA.INCOME                           : int  23791 19252 10432 23472 35911 37524 13785 39056 18672 23939 ...
    ##  $ HARDSHIP.INDEX                              : int  47 78 97 57 26 17 88 14 58 39 ...

### Step 4: Assign Colors to Specific Values

At this point, the variable of interest is per capita income throughout the community areas in Chicago. Light colors will represent where per capita income is low compared to other community areas in the city. On the other hand, dark colors will represent where per capita income is higher relevant to other community areas in the city.

*For inspiration, see the [website, COLOURlovers.](http://www.colourlovers.com/palettes/most-loved/all-time/meta)*

``` r
# Decide what colors I would like to use
# First hex value contains the group with the least value; the last hex value 
# contains the group with the highest value.

col.schema <- c( "#CFF09E" # sunlit sea
                 , "#A8DBA8" #seafoam green
                 , "#79BD9A" # sea showing green
                 , "#3B8686" # there we could sail
                 , "#0B486B" # adrift in dreams
)

# Break up the values of the vectors into 5 levels.
income.break <- cut( chicago.com.area@data$PER.CAPITA.INCOME
                     , breaks = 5
                     )
# Use the levels() function to see where the income breaks occur.
# Use this knowledge to label the legend.
levels( income.break )
```

    ## [1] "(8.12e+03,2.43e+04]" "(2.43e+04,4.04e+04]" "(4.04e+04,5.65e+04]"
    ## [4] "(5.65e+04,7.26e+04]" "(7.26e+04,8.87e+04]"

``` r
# Apply the color schema to each level.
# select( chicago.com.area@data, 5, 11, 17)
color.income <- cut( chicago.com.area@data$PER.CAPITA.INCOME
                  , breaks = 5
                  , labels = col.schema
                 )

# Convert the assigned colors to each value into character
color.income <- as.character( color.income )
```

### Step 5: Visualize

Congratulations: by reading this far you get to finally map information, thus making the unfamiliar familiar!

This section of the tutorial uses a lot of plot customization. Click [here for more information on plot parameters](https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/par.html).

*Caution: When replicating this code in RStudio, please delete the option "eval = FALSE". That option is found at the top of the r chunk, located inside the {} brackets. This option was put in place for GitHub viewers to view the output without seeing a tiny picture icon.*

``` r
# Set plot parameters
# Stretch your RStudio plot space to give yourself as much room as possible,
# otherwise your map will be squished.
par( bg = "antiquewhite"
     , oma = c(0, 0, 0, 0)
)

# Plot
plot( chicago.com.area
      , col = color.income
)

# Add title text
title( main = "Chicago's Per Capita Income by Community Area, 2008-2012" )

# Add subtitle text
mtext( line = 0.5
       , adj = 0.5
       , cex = 0.8
       , text = "South of the Stevenson Expressway, per capita income never exceeded $40,400 between 2008 and 2012."
)

# Create legend text using the income breaks from Step 4
legend.text.income = c(   "$  8,120 - $24,300"
                       , "$24,300 - $40,400"
                       , "$40,400 - $56,500"
                       , "$56,500 - $72,600"
                       , "$72,600 - $88,700"
)
# Plot the legend
legend( "topright"
        , pch = 20
        , pt.cex = 3
        , cex = 1
        , bg = "antiquewhite"
        , legend = legend.text.income
        , col = col.schema
        , box.col = "antiquewhite"
        , title = "(In 2012 Dollars)"
)

# Now we need to create community area number and name labels
# Idea: Community Area Number + Name
com.area.num.name <- select( chicago.com.area@data, 10, 11)

# order by community area
com.area.num.name <- com.area.num.name[ order( com.area.num.name$Community.Area.Number), ]

# convert area number to character
com.area.num.name$Community.Area.Number <- as.character( com.area.num.name$Community.Area.Number )

# Plot community area number and name
# Use the paste() function to combine the two variables, separated by user defined space.
legend("bottomleft"
       , legend = paste( com.area.num.name$Community.Area.Number
                         , com.area.num.name$COMMUNITY.AREA.NAME
                         , sep = "   "
       )
       , bty = "n"
       , title = "Community Area Number and Name"
       , cex = 0.30
)

# Plot data source
mtext( side = 1
       , line = 2
       , cex = 0.5
       , adj = 1
       , text = "Source: Cristian E. Nuno | 2008-2012 American Community Survey 5-year Estimates"
)

# add community area numbers
# NOTE:  gCentroid is from the 'rgeos' package
# NOTE: This knowledge was gained using Steven Brey's
#       work in: #http://mazamascience.com/WorkingWithData/?p=1277


# Find the center of each region and label lat and lon of centers
centroids <- gCentroid( chicago.com.area, byid = TRUE )
centroidLons <- coordinates(centroids)[,1] # obtain longitudinal coords
centroidLats <- coordinates(centroids)[,2] # obtain latitutde coords

# Label by area number using centroids
# Create white background for text
shadowtext( centroidLons, centroidLats
            , labels = chicago.com.area@data$area_numbe
            , col = "white"
            , bg = "black"
)
```

![](https://github.com/cenuno/MPA_Portfolio/raw/lab_01/Chicago_Visualizations/cenuno_PCI_2008to2012.png)
