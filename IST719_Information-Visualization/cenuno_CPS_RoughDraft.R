# • ggmap: extends the plotting package ggplot2 for maps
# • rgdal: R’s interface to the popular C/C++ spatial data processing library gdal • rgeos: R’s interface to the powerful vector processing library geos
# • maptools: provides various mapping functions
# • dplyr and tidyr: fast and concise data manipulation packages
# • tmap: a new packages for rapidly creating beautiful map
library( ggmap )
library( rgdal )
library( maptools )
library( dplyr )
library( tidyr )
library( tmap )
library( sp )
library( ggplot2 )
library( spatialEco ) # spatial econometrics package

# City of Chicago Community Area Boundaries in GeoJSON format
chicago.com.area <- readOGR("https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=GeoJSON"
                            , "OGRGeoJSON"
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

# Source: https://data.cityofchicago.org/Education/Chicago-Public-Schools-School-Progress-Reports-SY1/fvrx-esxp
cps.1516 <- read.csv( "https://data.cityofchicago.org/api/views/fvrx-esxp/rows.csv?accessType=DOWNLOAD"
                      , stringsAsFactors = FALSE
                      , header = TRUE
)

# Create descriptive statistics per community area side
# Variables of interest are: chicago.com.area@data$NWEA_Reading_Attainment_Grade_3_Pct
# "Student_Growth_Rating", "Student_Attainment_Rating"
# "Chronic_Truancy_Pct", "Freshman_On_Track_School(Y2 - Y1)", "Attainment_ACT_Grade_11_Pct"
# "Suspspensions_Per_100_Students_Year_(Y2-Y1)_Pct"

# Go through these variables and ensure that NAs are put in their own data frame

# identify data that has "" values
which( cps.1516$Student_Growth_Rating == "")
# assign "" values as "NO DATA AVAILABLE"
cps.1516$Student_Growth_Rating[c(135:136, 669:670)] <- "NO DATA AVAILABLE"
# clean up variable so that "NO DATA AVAILABLE" values are NA
cps.1516$Student_Growth_Rating <- factor( x = cps.1516$Student_Growth_Rating
                                          , levels = c(  NA
                                                         , "FAR BELOW AVERAGE"
                                                         , "BELOW AVERAGE"
                                                         , "AVERAGE"
                                                         , "ABOVE AVERAGE"
                                                         , "FAR ABOVE AVERAGE"
                                          )
)
# identify data that has "" values
which( cps.1516$Student_Attainment_Rating == "")
# assign "" values as "NO DATA AVAILABLE"
cps.1516$Student_Attainment_Rating[c(135:136, 669:670)] <- "NO DATA AVAILABLE"
# clean up variable so that "NO DATA AVAILABLE" values are NA
cps.1516$Student_Attainment_Rating <- factor( x = cps.1516$Student_Attainment_Rating
                                          , levels = c(  NA
                                                         , "FAR BELOW AVERAGE"
                                                         , "BELOW AVERAGE"
                                                         , "AVERAGE"
                                                         , "ABOVE AVERAGE"
                                                         , "FAR ABOVE AVERAGE"
                                          )
)

# which variables am I interested in
# SGR, SAR, Culture Climate Rating, 

sgr.dist <- table( cps.1516$Student_Growth_Rating )
# NO DATA AVAILABLE FAR BELOW AVERAGE     BELOW AVERAGE 
# 78                18               151 
# AVERAGE     ABOVE AVERAGE FAR ABOVE AVERAGE 
# 184               154                85 
# School Types
stype.dist <- table( cps.1516$School_Type )

# Career academy                Charter        Citywide-Option 
# 4                    127                     25 
# Classical               Contract                 Magnet 
# 5                      5                     43 
# Military academy           Neighborhood Regional gifted center 
# 6                    400                     10 
# Selective enrollment                  Small      Special Education 
# 10                     26                      9 

# School category
cps.1516$Primary_Category <- factor( x = cps.1516$Primary_Category
                                     , levels = c( "ES"
                                                   , "MS"
                                                   , "HS"
                                     )
)
scat.dist <- table( cps.1516$Primary_Category )
# ES  HS  MS 
# 472 188  10 

# create CPS lat lon dataframe
xy <- cps.1516[ , c( 152, 151 ) ]
#use xy dataframe to create a spatial points data frame using cps data
cps.spdf <- SpatialPointsDataFrame( coords = xy
                                    , data = cps.1516
                                    , proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
)

# Overlay points and extract just the code column: 
poly.data.matched.to.points <- over( cps.spdf
                                     , chicago.com.area 
)

# Add that data back to points data:
cps.spdf@data <- cbind( cps.spdf@data
                        , poly.data.matched.to.points
)

# Now merge spatial polgyons data frame with spatial points data frame
chicago.com.area@data <- data.frame( chicago.com.area@data
                                     , cps.spdf@data[ match( chicago.com.area@data[, "community"], cps.spdf@data[, "community"] 
                                                             )
                                                      , ]
)  

# erase duplicated columns
chicago.com.area@data[ , c( 164:173 ) ] <- NULL

# practice with mapping data 
plot( chicago.com.area, col = "whitesmoke") # plot the chicago.com.area object
sel <- chicago.com.area@data$community.area.side == "CENTRAL" # add selected zones to map
plot( chicago.com.area[ sel, ], col = "#B3DCF2", add = TRUE)

# Visualize descriptive statistics: 
#### Are students in this district making sufficient academic progress each year? ###
# table( cps.1516$Student_Growth_Rating )
# FAR BELOW AVERAGE     BELOW AVERAGE           AVERAGE     ABOVE AVERAGE FAR ABOVE AVERAGE 
# 18                              151               184               154                85                             23                23                13        9
par( oma = c( 0, 1, 3, 1 ))
barplot( table( cps.1516$Student_Growth_Rating )
         , beside = TRUE
         , las = 1
         , col = c(   "#801113" # far below avg
                    , "#E03526" # below avg
                    , "#FEC325" # avg
                    , "#A0CE66" # above avg
                    , "#709D4D" # far above avg
         )
         , ylim = c(0, 200)
         , ylab = "Number Of CPS Schools"
         , xaxt = "n"
)
# Add legend
legend( "topright", 
        legend = rownames( table( cps.1516$Student_Growth_Rating ) )
        , lwd = 4
        , cex = 0.8
        , bty = "n"
        , lty = 1
        , col = c(       "#801113" # far below avg
                       , "#E03526" # below avg
                       , "#FEC325" # avg
                       , "#A0CE66" # above avg
                       , "#709D4D" # far above avg
        )
)

# Add X label
mtext( side = 1
       , adj = 0.5
       , line = 1
       , cex = 1
       , text = "All CPS Schools"
       )

# title text
mtext( side = 3
       , adj = 0
       , line = 5
       , cex = 1.2
       , text = "Most Chicago Public School (CPS) Students Made Sufficient Academic Progress Between Spring 2014 and Spring 2015"
       )
# subtitle text
mtext( side = 3
       , adj = 0
       , line = 3
       , cex = 0.8
       , text = "Student growth rating measures how much students learn in a year. The rating is the school’s rank in CPS compared to schools with similar pre-test scores.\nAn AVERAGE score means the school is at the CPS average in terms of growth."
       )
# Source text
mtext( side = 1
       , adj = 1
       , line = 3
       , cex = 0.5
       , text = "Source: Cristian E. Nuno | https://data.cityofchicago.org/Education/Chicago-Public-Schools-School-Progress-Reports-SY1/fvrx-esxp"
       )                 
# More information text
mtext( side = 1
       , adj = 0
       , line = 3
       , cex = 0.4
       , text = "For more information on the exam used for elementary and middle schools, see https://www.nwea.org/assessments/map/.\nFor more information on the exam used for high schools, see https://forms.act.org/epas/."
)

par( oma = c( 0, 1, 3, 1 ))
barplot( table( cps.1516$Student_Growth_Rating, cps.1516$Primary_Category )
         , beside = TRUE
         , las = 1
         , col = c(   "#801113" # far below avg
                    , "#E03526" # below avg
                    , "#FEC325" # avg
                    , "#A0CE66" # above avg
                    , "#709D4D" # far above avg
                    )
         , ylim = c(0, 200)
         , ylab = "Number Of CPS Schools"
         , xaxt = "n"
)
# Add legend
legend( "topright", 
        legend = rownames( table( cps.1516$Student_Growth_Rating, cps.1516$Primary_Category ) )
        , lwd = 4
        , cex = 0.8
        , bty = "n"
        , lty = 1
        , col = c(       "#801113" # far below avg
                       , "#E03526" # below avg
                       , "#FEC325" # avg
                       , "#A0CE66" # above avg
                       , "#709D4D" # far above avg
        )
)
# Add X-axis
mtext( side = 1
       , line = 1
       , adj = 0.15
       , cex = 1
       , text = "Elementary Schools"
       )
mtext( side = 1
       , line = 1
       , adj = 0.50
       , cex = 1
       , text = "Middle Schools"
)
mtext( side = 1
       , line = 1
       , adj = 0.85
       , cex = 1
       , text = "High Schools"
)
# title text
mtext( side = 3
       , adj = 0
       , line = 5
       , cex = 1.5
       , text = "CPS Elementary Schools (Appear) To Be Leading the Way"
)
# subtitle text
mtext( side = 3
       , adj = 0
       , line = 3
       , cex = 0.8
       , text = "Of the 592 schools that recorded student growth rating during the 2015 school year, 456 were elementary schools, 10 were middle schools, and 126 were high schools.\nSuch a discrepancy requires that we examine student growth as a percentage of total number of schools in each category."
)
# Source text
mtext( side = 1
       , adj = 1
       , line = 3
       , cex = 0.5
       , text = "Source: Cristian E. Nuno | https://data.cityofchicago.org/Education/Chicago-Public-Schools-School-Progress-Reports-SY1/fvrx-esxp"
)
# turn matrix into a variable
matrix.SGR     <- table( cps.1516$Student_Growth_Rating, cps.1516$Primary_Category )
# turn columns in matrix into vectors based on school category.
# Transform vector into percentages
es.SGR <- round( matrix.SGR[1:5] / sum( matrix.SGR[1:5] ), digits = 2 )
ms.SGR <- round( matrix.SGR[6:10] / sum( matrix.SGR[6:10] ), digits = 2 )
hs.SGR <- round( matrix.SGR[11:15] / sum( matrix.SGR[11:15] ), digits = 2 )
# Combine vectors back into matrix
matrix.SGR.PER <- matrix( data = c(es.SGR, ms.SGR, hs.SGR)
                          , nrow = 5
                          , ncol = 3
                          )
# Rename column and row names
colnames( matrix.SGR.PER ) <- c( "Elementary Schools", "Middle Schools", "High Schools")
rownames( matrix.SGR.PER ) <- c( "FAR BELOW AVERAGE", "BELOW AVERAGE", "AVERAGE", "ABOVE AVERAGE", "FAR ABOVE AVERAGE" )

# Time to plot!
par( oma = c( 0, 1, 3, 1 ))
barplot( matrix.SGR.PER
         , beside = TRUE
         , las = 1
         , col = c(     "#801113" # far below avg
                      , "#E03526" # below avg
                      , "#FEC325" # avg
                      , "#A0CE66" # above avg
                      , "#709D4D" # far above avg
         )
         , ylim = c(0, 1)
         , ylab = "Percentage Of Total Number of CPS Schools"
         , xaxt = "n"
)
# Add legend
legend( "topright", 
        legend = rownames( matrix.SGR.PER )
        , lwd = 4
        , cex = 0.8
        , bty = "n"
        , lty = 1
        , col = c(       "#801113" # far below avg
                       , "#E03526" # below avg
                       , "#FEC325" # avg
                       , "#A0CE66" # above avg
                       , "#709D4D" # far above avg
        )
)
# Add X-axis
mtext( side = 1
       , line = 1
       , adj = 0.15
       , cex = 1
       , text = "Elementary Schools"
)
mtext( side = 1
       , line = 1
       , adj = 0.50
       , cex = 1
       , text = "Middle Schools"
)
mtext( side = 1
       , line = 1
       , adj = 0.85
       , cex = 1
       , text = "High Schools"
)
# title text
mtext( side = 3
       , adj = 0
       , line = 5
       , cex = 1.5
       , text = "Accounting for Number of Schools Reveals Low Student Growth in CPS High Schools"
)
# subtitle text
mtext( side = 3
       , adj = 0
       , line = 3
       , cex = 0.8
       , text = "36% of CPS high schools observed at least average student growth; yet, the remaining 64% observed below and far below average student growth.\nThis situation is not occuring elsewhere as a majority of CPS elementary and middle schools observed average student growth."
)
# Source text
mtext( side = 1
       , adj = 1
       , line = 3
       , cex = 0.5
       , text = "Source: Cristian E. Nuno | https://data.cityofchicago.org/Education/Chicago-Public-Schools-School-Progress-Reports-SY1/fvrx-esxp"
)
# 
# table( cps.1516$Student_Attainment_Rating )
# FAR BELOW AVERAGE     BELOW AVERAGE           AVERAGE     ABOVE AVERAGE FAR ABOVE AVERAGE 
#               73               183               157               110                89 
# Examine student attainment rating
matrix.SAR <- table( cps.1516$Student_Attainment_Rating, cps.1516$Primary_Category )
# turn columns in matrix into vectors based on school category.
# Transform vector into percentages
es.SAR <- round( matrix.SAR[1:5] / sum( matrix.SAR[1:5] ), digits = 2 )
ms.SAR <- round( matrix.SAR[6:10] / sum( matrix.SAR[6:10] ), digits = 2 )
hs.SAR <- round( matrix.SAR[11:15] / sum( matrix.SAR[11:15] ), digits = 2 )
# Combine vectors back into matrix
matrix.SAR.PER <- matrix( data = c(es.SAR, ms.SAR, hs.SAR)
                          , nrow = 5
                          , ncol = 3
)
# Rename column and row names
colnames( matrix.SAR.PER ) <- c( "Elementary Schools", "Middle Schools", "High Schools")
rownames( matrix.SAR.PER ) <- c( "FAR BELOW AVERAGE", "BELOW AVERAGE", "AVERAGE", "ABOVE AVERAGE", "FAR ABOVE AVERAGE" )

# Time to plot!
par( oma = c( 0, 1, 3, 1 ))
barplot( matrix.SAR.PER
         , beside = TRUE
         , las = 1
         , col = c(     "#801113" # far below avg
                        , "#E03526" # below avg
                        , "#FEC325" # avg
                        , "#A0CE66" # above avg
                        , "#709D4D" # far above avg
         )
         , ylim = c(0, 1)
         , ylab = "Percentage Of Total Number of CPS Schools"
         , xaxt = "n"
)
# Add legend
legend( "topright", 
        legend = rownames( matrix.SAR.PER )
        , lwd = 4
        , cex = 0.8
        , bty = "n"
        , lty = 1
        , col = c(       "#801113" # far below avg
                         , "#E03526" # below avg
                         , "#FEC325" # avg
                         , "#A0CE66" # above avg
                         , "#709D4D" # far above avg
        )
)
# Add X-axis
mtext( side = 1
       , line = 1
       , adj = 0.15
       , cex = 1
       , text = "Elementary Schools"
)
mtext( side = 1
       , line = 1
       , adj = 0.50
       , cex = 1
       , text = "Middle Schools"
)
mtext( side = 1
       , line = 1
       , adj = 0.85
       , cex = 1
       , text = "High Schools"
)
# title text
mtext( side = 3
       , adj = 0
       , line = 4
       , cex = 1.5
       , text = "Different Metric, Same Outlook:\n72% of CPS High Schools Are Not Meeting Average Grade Level Standards"
)
# subtitle text
mtext( side = 3
       , adj = 0
       , line = 2
       , cex = 0.8
       , text = "Student attainment measures how well students score on ACT EPAS compared to the national EPAS average score.\nAn AVERAGE rating means the school is performing at the national average."
)
# Source text
mtext( side = 1
       , adj = 1
       , line = 3
       , cex = 0.5
       , text = "Source: Cristian E. Nuno | https://data.cityofchicago.org/Education/Chicago-Public-Schools-School-Progress-Reports-SY1/fvrx-esxp"
)

# There is a story to be told with low performing HS
# Go investigate!!!
# I only want high schools
high.school <- cps.1516[ cps.1516$Primary_Category == "HS", ]

high.school.type <- table( high.school$Student_Growth_Rating, high.school$School_Type )
# geospatial component
plot( chicago.com.area, col = "white" )
legend( "bottomleft", 
        legend = c("FAR BELOW or BELOW AVERAGE", "AVERAGE", "FAR ABOVE or ABOVE AVERAGE")
        , pch = 20
        , cex = 1
        , bty = "n"
        , col = c(       "#801113" # below avg and far below avg
                         , "#FEC325" # avg
                         , "#709D4D" # far above or above avg
        )
)
mtext( side = 3
       , adj = 0
       , cex = 1.3
       , line = 4
       , text = "South of the Stevenson Expressway:\nAbundance of 2015 CPS High Schools With Student Growth Ratings Below Average"
)
mtext( side = 3
       , adj = 0
       , cex = 0.8
       , line = 3
       , text = "Communities living south of the Stevenson Expressway (Interstate 55) will only find 1 high school who's student growth rating was above average."
)
points( high.school$School_Longitude[ high.school$Student_Growth_Rating == "AVERAGE" ]
        , high.school$School_Latitude[ high.school$Student_Growth_Rating == "AVERAGE" ]
        , pch = 20
        , col = "#FEC325" # avg
)
points( high.school$School_Longitude[ high.school$Student_Growth_Rating == "FAR BELOW AVERAGE" | high.school$Student_Growth_Rating == "BELOW AVERAGE" ]
        , high.school$School_Latitude[ high.school$Student_Growth_Rating == "FAR BELOW AVERAGE" | high.school$Student_Growth_Rating == "BELOW AVERAGE" ]
        , pch = 20
        , col = "#801113" # below avg and far below avg
)

points( high.school$School_Longitude[ high.school$Student_Growth_Rating == "ABOVE AVERAGE" | high.school$Student_Growth_Rating == "FAR ABOVE AVERAGE" ]
        , high.school$School_Latitude[ high.school$Student_Growth_Rating == "ABOVE AVERAGE" | high.school$Student_Growth_Rating == "FAR ABOVE AVERAGE" ]
        , pch = 20
        , col = "#709D4D" # above avg and far above avg
)
mtext( side = 1
       , adj = 1
       , line = 2
       , cex = 0.5
       , text = "Source: Cristian E. Nuno | https://data.cityofchicago.org/Education/Chicago-Public-Schools-School-Progress-Reports-SY1/fvrx-esxp."
)
# Community Area Sides
plot( chicago.com.area[ chicago.com.area@data$community.area.side == "FAR NORTH SIDE" |
                          chicago.com.area@data$community.area.side == "NORTHWEST SIDE" |
                          chicago.com.area@data$community.area.side == "NORTH SIDE" |
                          chicago.com.area@data$community.area.side == "WEST SIDE" |
                          chicago.com.area@data$community.area.side == "CENTRAL" |
                          chicago.com.area@data$community.area.side == "SOUTH SIDE" |
                          chicago.com.area@data$community.area.side == "SOUTHWEST SIDE" |
                          chicago.com.area@data$community.area.side == "FAR SOUTHWEST SIDE" |
                          chicago.com.area@data$community.area.side == "FAR SOUTHEAST SIDE", ]
      , col = c(   "#EFD363" # FAR NORTH SIDE
                 , "#EFC7C6" # NORTHWEST SIDE
                 , "#F7E3AD" # NORTH SIDE
                 , "#D7CBD2" # WEST SIDE
                 , "#6BC3E7" # CENTRAL
                 , "#CFF4B2" # SOUTH SIDE
                 , "#F5C08A" # SOUTHWEST SIDE
                 , "#BDD794" # FAR SOUTHWEST SIDE
                 , "#F7BE8C" # FAR SOUTHEAST SIDE
      )
)
plot( chicago.com.area[ chicago.com.area@data$community.area.side == "FAR NORTH SIDE",]
      , col = "red" )