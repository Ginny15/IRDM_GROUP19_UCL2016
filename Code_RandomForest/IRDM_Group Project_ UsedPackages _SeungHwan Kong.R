memory.limit(50000)

# install.packages("ggplot2")
require(ggplot2)

# install.packages("plyr")
require(plyr)

# install.packages("grid")
require(grid)

# install.packages("devtools")
require(devtools)

if (!require(mclust)) {
  install.packages("mclust")
  require(mclust)
}

#install.packages("latticeExtra")
library(latticeExtra)
library(lattice)


#install.packages("fpc")
library(fpc)

###
if (!require(sampling)) {
  install.packages("sampling")
  require(sampling)
}

#install.packages("bigmemory")
#install.packages("biganalytics")
 library(bigmemory)
 library(biganalytics)


#install.packages("beepr")
library(beepr)


#install.packages("moments")
library(moments)

#install.packages("outliers")
require(outliers)


###################################### Packages for Social Network Analysis ######################################
#install.packages("igraph")
library(igraph)
#install.packages("network") 
library(network)
#install.packages("sna")
 library(sna)
#install.packages("ndtv")
 library(ndtv)

#install.packages("xts")
require(xts)

#install.packages("quantmod")
require(quantmod)
require(igraph)

library(igraph)

#install.packages("GeneNet")
require(GeneNet)


##################################################################################################################

##### Rank-Frequency plot ##################################################
# install.packages("qdap")
require(qdap)
#install.packages("zipfR")
require(zipfR)

############################################################################
library(moments)
#install.packages("Hmisc")
library(Hmisc)
library(sna)
library(ndtv)
library(network)
library(igraph)
#install.packages("PCIT")
library(PCIT)
library(ggplot2)
library(reshape2)
#install.packages("infotheo")
library(infotheo)
library(tm)
#install.packages("poweRlaw")
library(poweRlaw)

## Kullback-Leibler Divergence
#install.packages("FNN")
require(FNN)


##### add fonts #####
nanumgothic <- windowsFont("³ª´®°íµñ")
windowsFonts(nanumgothic=nanumgothic)
par(family="nanumgothic")


##### Random Forest Package #####
# install.packages("randomForest")
library("randomForest")


##### xts package for Time Series analysis #####
#install.packages("xts")
require("xts")


#install.packages("zoo")
require("zoo")


##### RMSE ####
#install.packages("Metrics")
require("Metrics")
