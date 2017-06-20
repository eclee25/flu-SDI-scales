
## Name: Elizabeth Lee
## Date: 6/20/16
## Function: Edit Census 2010 county shapefile to remove 3 incorrectly disconnected components
## Filenames: reference_data/UScounty_shapefiles/
## Data Source: Census 2010 county shapefiles
## Notes: code adapted from snippet_check_censusCtyShapefile.R
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(readr)
require(ggplot2)
require(dplyr)
require(tidyr)
require(RColorBrewer)
require(maptools)
require(scales)
require(broom)
require(rgeos)
require(spdep)
require(INLA)

#### import data ################################
setwd(dirname(sys.frame(1)$ofile))
setwd("../reference_data/UScounty_shapefiles")
cty.shp <- readShapeSpatial("gz_2010_us_050_00_500k", verbose = TRUE, repair = TRUE) 

#### examine disconnected subgraphs ################################
# See "Notes and Code for Small Area NYC Pedestrian Injury Spatiotemporal Analyses With INLA" in Mendeley
# Section 1.1 Create the Adjacency Matrix Graph

adjMx <- poly2nb(cty.shp) # 3221 regions, 9 regions had no links: 68 546 547 549 1226 1876 2976 3148 3202
# these region numbers refer to the map and not the nb list object
nolinks <- which(card(adjMx) == 0) # IDs for areas with no links, indexed to nb object: 69  547  548  550 1227 1877 2977 3149 3203
attr(adjMx, "region.id")[which(card(adjMx)==0)]
# e.g. adjMx[[1]] returns a list of neighbors to region 1

# include only continental states in new shapefile
statesOnly <- read_csv("/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/reference_data/state_abbreviations_FIPS.csv", col_types = "__c", col_names = c("stateID"), skip = 1) 
continental <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
cty.statesonly <- cty.shp[cty.shp@data$STATE %in% continental,]
adjMx_cont <- poly2nb(cty.statesonly) # 3 regions with no links: 1226, 1876, 2976
plot(cty.statesonly)
# 6/8/16: remove territories from neighbors list in INLA because they have no neighbors and data is limited: source_prepare_inlaData_cty.R/read_shapefile_cty function

# examine the 3 regions with no links
nolinks_ix <- which(card(adjMx_cont)==0) # indexes 1193 1843 2943; rownames 1226 1876 2976
ctyshp_nolinks <- cty.statesonly[nolinks_ix,]
ctyshp_nolinks@data # Nantucket County MA (25019), Richmond County NY (36085), San Juan County (53055)
plot(ctyshp_nolinks)

# Nantucket Cty and San Juan County are islands and shouldn't have neighbors. These counties can be removed. 
# GEO_ID: 0500000US25019 0500000US53055 
rmIDs <- c("0500000US25019", "0500000US53055")
cty.statesonly2 <- cty.statesonly[!(cty.statesonly@data$GEO_ID %in% rmIDs),]

# Richmond County is Staten Island NY, so the shapefile needs to be edited. 
# Richmond county borders: Kings county NY, Hudson county NJ, Union county NJ, Middlesex county NJ
# fips: 36047 34017 34039 34023
# indexes: 1837 1748 1759 1751
# rownames: 1871 1782 1793 1785
NYNJ <- which(cty.statesonly2@data$STATE == '34' | cty.statesonly2@data$STATE == '36')
cty.statesonly2[NYNJ,]@data
ix <- which(rownames(cty.statesonly2@data) == 1785) # to get indexes
cty.statesonly2[ix,]@data # check that index returns the correct county
print(ix)

# # visualize the network
# nx_orig <- edit.nb(adjMx_cont, polys=cty.statesonly2)

# edit the adjacency matrix manually with cty.statesonly2 indexes (Richmond county index = 1842)
adjMx_cont2 <- poly2nb(cty.statesonly2)
adjMx_cont2 # 1 region with no links (richmond county)
# add neighbors to richmond
adjMx_cont2[[1842]] <- as.integer(sort(c(1837, 1748, 1759, 1751)))
# add richmond to each of its neighbors' adjacency lists
adjMx_cont2[[1837]] <- as.integer(sort(c(adjMx_cont2[[1837]], 1842)))
adjMx_cont2[[1748]] <- as.integer(sort(c(adjMx_cont2[[1748]], 1842)))
adjMx_cont2[[1759]] <- as.integer(sort(c(adjMx_cont2[[1759]], 1842)))
adjMx_cont2[[1751]] <- as.integer(sort(c(adjMx_cont2[[1751]], 1842)))

adjMx_cont2 # now all regions have links

#### write edgelist from nb ################################
edgels <- data.frame()
for (i in 1:length(adjMx_cont2)){
  dummycol1 <- rep(i, length(adjMx_cont2[[i]]))
  dummycol2 <- unlist(adjMx_cont2[[i]])
  edgels <- bind_rows(edgels, tbl_df(cbind(dummycol1, dummycol2)))
}

#### exports county adjacency matrix to file ################################
setwd(dirname(sys.frame(1)$ofile))
setwd('../reference_data/UScounty_shapefiles')
path_adjMxExport_cty <- paste0(getwd(), "/US_county_adjacency.graph")
nb2INLA(path_adjMxExport_cty, adjMx_cont2)
# exported 6/20/16

#### exports graph index to file ################################
ID_crosswalk <- cty.statesonly2@data
ID_crosswalk["fips"] <- paste0(ID_crosswalk$STATE, ID_crosswalk$COUNTY)
ID_crosswalk["graphIdx"] <- seq_along(ID_crosswalk$fips)
write_csv(ID_crosswalk, 'US_county_graph_index.csv')
# exported 10/30/16

#### exports county edgelist to file ################################
path_edgelsExport_cty <- paste0(getwd(), "/US_county_edgelist.csv")
write_csv(edgels, path_edgelsExport_cty, col_names = FALSE)
# exported 5/21/17
