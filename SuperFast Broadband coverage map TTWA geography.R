###################################Mapping superfast broadband coverage in the UK ####################################
######################################################################################################################
#This programme calculates and maps average broadband penetration levels by Travel to Work Area (TTWA)
#It uses postcode level data on broadband coverage from Ofcom, a lookup table from postcodes to coordinates
# and 2001 TTWA shapefile data
#####################################################################################################################

#imports the RColorBrewer, the class intervals package and the mapping packages rgdal, maptools, sp
library("rgdal")
library("RColorBrewer")
library("classInt")
library("maptools")
library("sp")
####################################################################################################################

##Step 1 Reading in the data########################################################################################
#Reads in the 2001 TTWA shape files ################################################################################
ttwa<-readOGR("filepath1", "TTWA_2001_UK_BFE")


#Reads in the 2015 Ofcom broadband data by postcode
broadband<-read.csv("filepath2", header=TRUE, stringsAsFactors=FALSE) 

#Data is from
#http://stakeholders.ofcom.org.uk/market-data-research/market-data/infrastructure/connected-nations-2015/downloads/

#Reads in the 2015 poscode lookup data
postcodes<-read.csv("filepath3", header=TRUE,stringsAsFactors=FALSE)

###################################################################################################################
##Step 2 Cleans up the data########################################################################################

#removes the spaces in the postcode data file 
postcodes$pcd<-gsub(" ", "", postcodes$pcd)  #This is the transformation that's needed to match to the postcode lookup file
##################################################################################################################

###################################################################################################################
##Step 3 merge the broadband data with the postcode file
#Merges the postcode file on the broadband data on postcode, keeping all the broadband data even where there is no match

new<-merge(broadband, postcodes, by.x="postcode"  , by.y="pcd" , all.x=TRUE)

#Calculates how many postcodes we match
f<-broadband$postcode %in% postcodes$pcd
table(f) # table(f) FALSE=106 i.e. 106 of the postcodes don't match. We deal with them separately in step 4
##################################################################################################################

###################################################################################################################
##Step 4 merge in the postcodes that were missing from the file and import their longitudes and lattitudes
######################Matches the missing codes###################################################################
#Reads in the missing codes which have been geocoded separately using
# the website  http://www.doogal.co.uk/BatchGeocoding.php

misscode<-read.csv("filepath4", header=T)#########Pulls in the longitude and latitude variables into the new file
new$long<-ifelse(new$postcode %in% misscode$ï..Address , misscode$Long[match(new$postcode, misscode$ï..Address)], new$long)
new$lat<-ifelse(new$postcode %in% misscode$ï..Address , misscode$Lat[match(new$postcode,misscode$ï..Address )], new$lat)


#Does a check to see if that has worked

new[new$postcode=="CT33FA",c("long")] # should be 1.201226
new[new$postcode=="CT33FA",c("lat")] #should be 51.27798


###################################################################################################################
##Step 5 Creates a spatial points data frame of the long and lat data. We use coordinates to get the TTWAs
# As our postcode lookup file has 2011 TTWAs, but not 2001 TTWAs like our data
####converts the points into spatial points

points = SpatialPoints(cbind(as.numeric(new[,c("long")]), as.numeric(new[,c("lat")])))

##sets up the projection of the spatial points
proj4string(points) <- CRS("+proj=longlat +datum=WGS84")

#sets the categories as a data frame

catsdat<-as.data.frame(new[,c("SFBB.availability.by.PC....premises.","postcode")])

#Creates a spatial points data frame
spdf = SpatialPointsDataFrame(points, catsdat)
######################################################################################################################################################################################################
##Step 6 Sets up the TTWA data so that the points can be overlaid
#sets the projection of the ttwaa so that it's the same as the spatial points 

ttwa<-spTransform(ttwa, CRS("+proj=longlat +datum=WGS84"))


#sorts out the missing Northen Irish TTWA coding
#The Northern Ireland TTWA codes
NI<-c("007","017","055","057","064","075","081","149", "159","168","209")

#Recodes the Northern Ireland codes
ttwa@data$TTWA01CD<-ifelse(ttwa@data$TTWA01CDO  %in% NI,as.character(ttwa@data$TTWA01CDO), as.character(ttwa@data$TTWA01CD))

###################################################################################################################
##Step 7 for each point see which TTWA they fall within and bind the results to the dataframe 

f<-over(points,ttwa, returnlist=TRUE)  #Finds which TTWA region the points fall within

hd<-cbind(spdf, f) 

##Step 8 For each TTWA average the super fast broadband coverage over all the postcodes

#################We  now have a 2001 ttwa variable for each data point and we average over all the postcodes in a ttwa with it 
aggdata1 <-aggregate(hd[,1],list(hd$TTWA01CD),FUN=mean, na.rm=TRUE) 
# na.rm=TRUE Excludes the NAs from the calculation

##Step 9 Merges the aggregated data with the travel to work area shape file data frame 

ttwa@data=data.frame(ttwa@data, aggdata1[match(ttwa@data[,"TTWA01CD"], aggdata1[, "Group.1"]),])
###################################################################################################################

##Step 10 plots the  Super Fast Broadband Map availability#########################################################
breaks2<-classIntervals(as.numeric(ttwa@data$x),n=6,style="fixed", fixedBreaks=c(0, 20,40,60,80,100))$brks
my_colours<-brewer.pal(5, "PuBu") 
plot(ttwa, col=my_colours[findInterval(ttwa@data$x, breaks2, all.inside=FALSE)],axes=FALSE, border=FALSE)
#title(main="Super fast broadband Access (TTWA average by postcode) ", cex.lab=0.07)
plot<-legend("bottomleft", legend=leglabs(round(breaks2, digits=2), between=" to <"), fill=my_colours, bty="n", cex=0.7)
###################################################################################################################


