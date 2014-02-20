##################################################################
# netAddHealth.R
# Michael Metcalf Bishop, bishop@uchicago.edu

# Data Source: The Longitudinal Study of Adolescent Health
# Purpose: Prepare data and create functions for social network analysis with R and igraph
#          It is often convenient to apply functions to add health network data by school or by community.
#          See the functions at the very end: MakeIgraphList and MakeVertexAttribList
#          which can be easily and fruitfully modified 
# Output: igraphList which is a list of igraph objects, one for each community
#         net4 which is the the data imported into net4, plus new measures of centrality 
# Note: Change the location of the data indicated in the first few lines of code.
#       Then, if you already have the three listed packages installed, the remainder of the code may
#       be executed at once.  

# Acknowledgements: I owe thanks to Joyce Tabor, James Moody, & Peter McMahan for assistance but any errors are my own. 
#   Please submit/push suggestions for improvements and I'll add your name here.
##################################################################

# file paths
kRootPath         <- file.path("~","addhealth")
kDataPath         <- file.path("~","addhealth","data")

load(file.path(kDataPath, "net4share2.RData"))
#load(file.path(kDataPath, "net4simple.RData"))
#save.image(file.path(kDataPath, "net4share2.RData"))
library(Hmisc)
library(igraph)
library(reshape)

# Import SAS datasets 
sfri <- sasxport.get(file.path(kDataPath, "sfriend.xpt")) # raw nomination data provided by Add Health
sfri <- sfri[which(sfri$sqid!=999999),] #all sqid=999999 already have all NA for all variables
inschool <- sasxport.get(file.path(kDataPath, "Inschool.xpt")) # In School Survey, Wave I, provided by Add Health
inhome <- sasxport.get(file.path(kDataPath, "allwave1.xpt")) # 500mb file so it takes a while, In Home survey provided by Add Health
net <- sasxport.get(file.path(kDataPath, "network.xpt")) # social network data provided by Add Health / James Moody
# edunet <- sasxport.get(file.path(kDataPath, "edunet.xpt")) # local position data provided by Add Health / Ken Frank 
 

# If you don't have access to all the datasets listed above, you can still get the data into igraph and do some analysis
# e.g., After editing the code below, skipping much of it, sfriend.xpt and Inschool.xpt would be enough to get started
# inhome/allwave1.xpt is helpful because I use it to improve data cleaning and extract community/commid
# net/network.xpt is used in this script only to compare the pre-computed network data with what we compute from raw data

# Creating aid2 with new ids where aid is missing
inschool$aid2 <- ifelse(is.na(inschool$aid), NA, as.numeric(as.character(inschool$aid)))
missingIds <- is.na(inschool$aid2)
replacementIds <- seq(length(missingIds))
describe(missingIds)
describe(replacementIds)
missingIds
replacementIds

inschool$aid2[missingIds] <- replacementIds[missingIds]
rm(missingIds)
rm(replacementIds)
describe(duplicated(inschool$aid2)) #check that there are no duplicate ids
label(inschool$aid2) <- "aid - Respondent Identifier - with missing values replaced"
inschool$aid2.is <- inschool$aid2




# Merge all resulting in net4
net2 <- merge(inschool[c("sqid","aid","aid2","sschlcde","s2")], sfri, by="sqid", all=TRUE)
net3 <- merge(net[c("aid","scid","size","idgx2","odgx2","noutnom","tab113","bcent10x")], net2, by="aid", all=TRUE)
net4 <- merge(inhome[c("aid", "bio.sex", "scid","sscid","commid","sschlcde", "h1gi18", "h1gi19", "h1gi20", "h1gi21")], net3, by="aid", all=TRUE)
net4 <- merge(net4, edunet, by="aid", all=TRUE) 

describe(net$aid)
class(net$aid)
net$aid2 <- as.nc(net$aid)
describe(net$aid2)
class(net$aid2)

describe(edunet$aid)
class(edunet$aid)
edunet$aid2 <- as.nc(edunet$aid)
describe(edunet$aid2)
class(edunet$aid2)
describe(duplicated(edunet$aid)) #check that there are no duplicate ids
duplicated(edunet$aid2)
describe(edunet$aid2[which(duplicated(edunet$aid2)==TRUE)])
describe(edunet$aid2[which(duplicated(edunet$aid2)==FALSE)])
View(edunet)

View(orderedunet)

describe(edunet$aid[which(duplicated(edunet$aid)==TRUE)])
describe(edunet$aid[which(duplicated(edunet$aid)==FALSE)])
View(edunet[which(duplicated(edunet$aid)==TRUE),])
View(edunet[order(edunet$aid),])
ord <- order
?sort

t <- edunet[which(duplicated(edunet$aid)==TRUE),]
u <- edunet[which(duplicated(edunet$aid)==FALSE),]

describe(t$enyrlp)
describe(u$enyrlp)
describe(edunet$enyrlp)

describe(edunet)

t$test <- 1
u$test <- 2
tu <- merge(t, u, by="aid", all=TRUE)
dim(t)
dim(u)
dim(tu)

describe(t$test)
describe(u$test)
describe(tu$test)
describe(tu$aid)

duplicated(t$aid)

duplicated(edunet$aid[which(duplicated(edunet$aid)==TRUE)])


rm("inhome", "inschool", "net2", "net", "net3", "sfri")
ls()



net4 <- rename(net4, c(sschlcde.x="sschlcde.inhome", sschlcde.y="sschlcde.inschool"))
net4 <- rename(net4, c(scid.x="scid.inhome", scid.y="scid.net"))


##### It is often convenient to apply functions to add health network data by community or by school. 
# Create new and "better" school id (sschclde2) and community id (ncommid2) variables 
# Some missing community (commid) and school id (sschlcde and scid) is inferred below.
net4$nsschlcde.inhome <- as.numeric(as.character(net4$sschlcde.inhome))
net4$nsschlcde.inschool <- as.numeric(as.character(net4$sschlcde.inschool))
net4$nscid.inhome <- as.numeric(as.character(net4$scid.inhome))
net4$nscid.net <- as.numeric(as.character(net4$scid.net)) # fyi nscid.inhome and nscid.net have 5 discrepancies
net4$ncommid <- as.numeric(as.character(net4$commid))


net4$sschlcde <- ifelse(is.na(net4$nsschlcde.inschool)==TRUE & is.na(net4$nsschlcde.inhome)==TRUE, NA,
                   ifelse(is.na(net4$nsschlcde.inhome)==TRUE & is.na(net4$nsschlcde.inschool)==FALSE, net4$nsschlcde.inschool, 
                     ifelse(is.na(net4$nsschlcde.inschool)==TRUE & is.na(net4$nsschlcde.inhome)==FALSE, net4$nsschlcde.inhome, 
                       ifelse(net4$nsschlcde.inhome==net4$nsschlcde.inschool, net4$nsschlcde.inschool, 1000))))


net4$scid <- ifelse(is.na(net4$nscid.net)==TRUE & is.na(net4$nscid.inhome)==TRUE, NA,
                   ifelse(is.na(net4$nscid.inhome)==TRUE & is.na(net4$nscid.net)==FALSE, net4$nscid.net, 
                     ifelse(is.na(net4$nscid.net)==TRUE & is.na(net4$nscid.inhome)==FALSE, net4$nscid.inhome, 
                       ifelse(net4$nscid.inhome==net4$nscid.net, net4$nscid.net, 1001))))

# temp<- net4[which(net4$scid==1001),] # View(temp) examine 5 cases where scid.net and scid.inhome conflicts 
# scid=999 means is an inhome sibling, didn't attend target or sister school, 375n

net4$sschlcde2 <- ifelse(is.na(net4$sschlcde) & net4$scid!=999, net4$scid, net4$sschlcde)
label(net4$sschlcde2) <- "sschlcde school code with some missing data imputed from scid" #NA - 375
describe(net4$sschlcde2)

# Problem: Community id variable (commid/ncommid) is only defined for the inhome sample... is missing for most of net4
# Solution:  1) Each school belongs to only one community, find out which community and then use school (sschlcde2)
                    # to assign a community (ncommid2)
# Pair each sschlcde2 with a commid, unfortunately some sschlcde2 are missing commid because no inhome data, 
df.commid <- aggregate(net4[,c("ncommid"), drop=FALSE], by=list(sschlcde2=net4$sschlcde2), FUN=median, na.rm=TRUE)
#            2) I still couldn't find the community for some schools,
#               so Joyce Tabor provided me with these assignments:    


t <- ih[which(ih$commid %in% c(459, 475, 479, 489)),]
table(as.nc(t$scid))
table(as.nc(t$scid), as.nc(t$commid))

# assign correct commid for schools which have sister w/ commid
df.commid$ncommid[which(df.commid$sschlcde2==59)] <- 459
df.commid$ncommid[which(df.commid$sschlcde2==75)] <- 475
df.commid$ncommid[which(df.commid$sschlcde2==175)] <- 475
df.commid$ncommid[which(df.commid$sschlcde2==79)] <- 479
df.commid$ncommid[which(df.commid$sschlcde2==83)] <- 489
df.commid$ncommid[which(df.commid$sschlcde2==183)] <- 489

# There is an absolute difference of 100 between the ID numbers of the schools 
# in the same "community" or the pairs of schools.  That is, if you subtract 
# one from another you will get 100.  Exceptions?


# invent commid for schools w/o commid or sister school w/ commid
df.commid$ncommid[which(df.commid$sschlcde2==4)] <- 504
df.commid$ncommid[which(df.commid$sschlcde2==45)] <- 545
df.commid$ncommid[which(df.commid$sschlcde2==89)] <- 589
df.commid$ncommid[which(df.commid$sschlcde2==105)] <- 605
df.commid$ncommid[which(df.commid$sschlcde2==138)] <- 638
df.commid$ncommid[which(df.commid$sschlcde2==145)] <- 545
df.commid$ncommid[which(df.commid$sschlcde2==267)] <- 767
df.commid$ncommid[which(df.commid$sschlcde2==999)] <- NA # did not attend a target or sister school


net4$ncommid2 <- df.commid$ncommid[match(net4$sschlcde2, df.commid$sschlcde2)]
net4$ncommid2 <- ifelse(is.na(net4$ncommid), net4$ncommid2, net4$ncommid)
label(net4$ncommid2) <- "commid - community ID - with some missing data imputed"
describe(net4$ncommid)
describe(net4$ncommid2)
rm("df.commid")

#table(is.na(as.numeric(net4$aid)),is.na(as.numeric(net4$sschlcde2))) # 375 missing sschlcde2 were added inhome

#missing aid are scattered across schools
#aid in binaid==5 are scattered across schools.

net4$binaid <- cut(as.numeric(net4$aid), c(10000000,20000000,30000000,
40000000,50000000,60000000,70000000,80000000,90000000,123456789))
table(net4$binaid)
table(is.na(as.numeric(net4$aid)),net4$sschlcde2)
net4$binaid <- NULL

# Creating aid2 with new ids where they are missing
net4$aid2 <- ifelse(is.na(net4$aid), NA, as.numeric(net4$aid))
missingIds <- is.na(net4$aid2)
replacementIds <- seq(length(missingIds))
net4$aid2[missingIds] <- replacementIds[missingIds]
   rm(missingIds)
   rm(replacementIds)
   describe(duplicated(net4$aid2)) #check that there are no duplicate ids
label(net4$aid2) <- "aid - Respondent Identifier - with missing values replaced"


# Variables: mf1aid, mf2aid, mf3aid, mf4aid, mf5aid, ff1aid, ff2aid, ff3aid, ff4aid, ff5aid
# represent nominated friends aid variable when a successful nomination is made, but
# some nominations are missing, and some nominations cannot be matched to an aid.
# Nomination variables which cannot be matched have been given a code which tells us 
# something about who was being nominated and/or why their was no matching aid

# The special codes are:
# 77777777 Friend not at the respondent's school or sister school
# 88888888 Friend at sister school but name was not on roster 
# 99999999 Friend at respondent's school but not on roster 
# 99959995 ID for nomination out of range, doesn't match anyone

##### Below I create indicators for whether each of the nomination variables
# is equal to each of the special codes, is missing, or is a completed match to an aid
# The indicator variables are named by starting with the the nomination variable,
# and then appending "c" for completed, "na" for missing, and "7", "8", "9", and"95"
# for the respective special codes.

                    
# Creating indicators of the special codes
net4$mf1aid7 <- ifelse((is.na(net4$mf1aid) | net4$mf1aid!=77777777), 0, 1)
net4$mf2aid7 <- ifelse((is.na(net4$mf2aid) | net4$mf2aid!=77777777), 0, 1)
net4$mf3aid7 <- ifelse((is.na(net4$mf3aid) | net4$mf3aid!=77777777), 0, 1)
net4$mf4aid7 <- ifelse((is.na(net4$mf4aid) | net4$mf4aid!=77777777), 0, 1)
net4$mf5aid7 <- ifelse((is.na(net4$mf5aid) | net4$mf5aid!=77777777), 0, 1)
net4$ff1aid7 <- ifelse((is.na(net4$ff1aid) | net4$ff1aid!=77777777), 0, 1)
net4$ff2aid7 <- ifelse((is.na(net4$ff2aid) | net4$ff2aid!=77777777), 0, 1)
net4$ff3aid7 <- ifelse((is.na(net4$ff3aid) | net4$ff3aid!=77777777), 0, 1)
net4$ff4aid7 <- ifelse((is.na(net4$ff4aid) | net4$ff4aid!=77777777), 0, 1)
net4$ff5aid7 <- ifelse((is.na(net4$ff5aid) | net4$ff5aid!=77777777), 0, 1)

net4$mf1aid8 <- ifelse((is.na(net4$mf1aid) | net4$mf1aid!=88888888), 0, 1)
net4$mf2aid8 <- ifelse((is.na(net4$mf2aid) | net4$mf2aid!=88888888), 0, 1)
net4$mf3aid8 <- ifelse((is.na(net4$mf3aid) | net4$mf3aid!=88888888), 0, 1)
net4$mf4aid8 <- ifelse((is.na(net4$mf4aid) | net4$mf4aid!=88888888), 0, 1)
net4$mf5aid8 <- ifelse((is.na(net4$mf5aid) | net4$mf5aid!=88888888), 0, 1)
net4$ff1aid8 <- ifelse((is.na(net4$ff1aid) | net4$ff1aid!=88888888), 0, 1)
net4$ff2aid8 <- ifelse((is.na(net4$ff2aid) | net4$ff2aid!=88888888), 0, 1)
net4$ff3aid8 <- ifelse((is.na(net4$ff3aid) | net4$ff3aid!=88888888), 0, 1)
net4$ff4aid8 <- ifelse((is.na(net4$ff4aid) | net4$ff4aid!=88888888), 0, 1)
net4$ff5aid8 <- ifelse((is.na(net4$ff5aid) | net4$ff5aid!=88888888), 0, 1)

net4$mf1aid9 <- ifelse((is.na(net4$mf1aid) | net4$mf1aid!=99999999), 0, 1)
net4$mf2aid9 <- ifelse((is.na(net4$mf2aid) | net4$mf2aid!=99999999), 0, 1)
net4$mf3aid9 <- ifelse((is.na(net4$mf3aid) | net4$mf3aid!=99999999), 0, 1)
net4$mf4aid9 <- ifelse((is.na(net4$mf4aid) | net4$mf4aid!=99999999), 0, 1)
net4$mf5aid9 <- ifelse((is.na(net4$mf5aid) | net4$mf5aid!=99999999), 0, 1)
net4$ff1aid9 <- ifelse((is.na(net4$ff1aid) | net4$ff1aid!=99999999), 0, 1)
net4$ff2aid9 <- ifelse((is.na(net4$ff2aid) | net4$ff2aid!=99999999), 0, 1)
net4$ff3aid9 <- ifelse((is.na(net4$ff3aid) | net4$ff3aid!=99999999), 0, 1)
net4$ff4aid9 <- ifelse((is.na(net4$ff4aid) | net4$ff4aid!=99999999), 0, 1)
net4$ff5aid9 <- ifelse((is.na(net4$ff5aid) | net4$ff5aid!=99999999), 0, 1)

net4$mf1aid95 <- ifelse((is.na(net4$mf1aid) | net4$mf1aid!=99959995), 0, 1)
net4$mf2aid95 <- ifelse((is.na(net4$mf2aid) | net4$mf2aid!=99959995), 0, 1)
net4$mf3aid95 <- ifelse((is.na(net4$mf3aid) | net4$mf3aid!=99959995), 0, 1)
net4$mf4aid95 <- ifelse((is.na(net4$mf4aid) | net4$mf4aid!=99959995), 0, 1)
net4$mf5aid95 <- ifelse((is.na(net4$mf5aid) | net4$mf5aid!=99959995), 0, 1)
net4$ff1aid95 <- ifelse((is.na(net4$ff1aid) | net4$ff1aid!=99959995), 0, 1)
net4$ff2aid95 <- ifelse((is.na(net4$ff2aid) | net4$ff2aid!=99959995), 0, 1)
net4$ff3aid95 <- ifelse((is.na(net4$ff3aid) | net4$ff3aid!=99959995), 0, 1)
net4$ff4aid95 <- ifelse((is.na(net4$ff4aid) | net4$ff4aid!=99959995), 0, 1)
net4$ff5aid95 <- ifelse((is.na(net4$ff5aid) | net4$ff5aid!=99959995), 0, 1)

# Creating indicators of missingness 
net4$mf1aidna <- ifelse(is.na(net4$mf1aid), 1, 0)
net4$mf2aidna <- ifelse(is.na(net4$mf2aid), 1, 0)
net4$mf3aidna <- ifelse(is.na(net4$mf3aid), 1, 0)
net4$mf4aidna <- ifelse(is.na(net4$mf4aid), 1, 0)
net4$mf5aidna <- ifelse(is.na(net4$mf5aid), 1, 0)
net4$ff1aidna <- ifelse(is.na(net4$ff1aid), 1, 0)
net4$ff2aidna <- ifelse(is.na(net4$ff2aid), 1, 0)
net4$ff3aidna <- ifelse(is.na(net4$ff3aid), 1, 0)
net4$ff4aidna <- ifelse(is.na(net4$ff4aid), 1, 0)
net4$ff5aidna <- ifelse(is.na(net4$ff5aid), 1, 0)


# Creating indicators of a completed nomination (thus the "c" suffix)
                    # (aka, no special codes, aka friend on roster at same or sister school)
net4$mf1aidc <- ifelse((!is.na(net4$mf1aid) & net4$mf1aid!=77777777 & net4$mf1aid!=88888888 & net4$mf1aid!=99999999 & net4$mf1aid!=99959995), 1, 0)
net4$mf2aidc <- ifelse((!is.na(net4$mf2aid) & net4$mf2aid!=77777777 & net4$mf2aid!=88888888 & net4$mf2aid!=99999999 & net4$mf2aid!=99959995), 1, 0)
net4$mf3aidc <- ifelse((!is.na(net4$mf3aid) & net4$mf3aid!=77777777 & net4$mf3aid!=88888888 & net4$mf3aid!=99999999 & net4$mf3aid!=99959995), 1, 0)
net4$mf4aidc <- ifelse((!is.na(net4$mf4aid) & net4$mf4aid!=77777777 & net4$mf4aid!=88888888 & net4$mf4aid!=99999999 & net4$mf4aid!=99959995), 1, 0)
net4$mf5aidc <- ifelse((!is.na(net4$mf5aid) & net4$mf5aid!=77777777 & net4$mf5aid!=88888888 & net4$mf5aid!=99999999 & net4$mf5aid!=99959995), 1, 0)
net4$ff1aidc <- ifelse((!is.na(net4$ff1aid) & net4$ff1aid!=77777777 & net4$ff1aid!=88888888 & net4$ff1aid!=99999999 & net4$ff1aid!=99959995), 1, 0)
net4$ff2aidc <- ifelse((!is.na(net4$ff2aid) & net4$ff2aid!=77777777 & net4$ff2aid!=88888888 & net4$ff2aid!=99999999 & net4$ff2aid!=99959995), 1, 0)
net4$ff3aidc <- ifelse((!is.na(net4$ff3aid) & net4$ff3aid!=77777777 & net4$ff3aid!=88888888 & net4$ff3aid!=99999999 & net4$ff3aid!=99959995), 1, 0)
net4$ff4aidc <- ifelse((!is.na(net4$ff4aid) & net4$ff4aid!=77777777 & net4$ff4aid!=88888888 & net4$ff4aid!=99999999 & net4$ff4aid!=99959995), 1, 0)
net4$ff5aidc <- ifelse((!is.na(net4$ff5aid) & net4$ff5aid!=77777777 & net4$ff5aid!=88888888 & net4$ff5aid!=99999999 & net4$ff5aid!=99959995), 1, 0)

# Creating indicators for nominations to someone in same school (regardless of roster) or sister school (AND on roster) 
net4$mf1aidc9 <- ifelse((!is.na(net4$mf1aid) & net4$mf1aid!=77777777 & net4$mf1aid!=88888888 & net4$mf1aid!=99959995), 1, 0)
net4$mf2aidc9 <- ifelse((!is.na(net4$mf2aid) & net4$mf2aid!=77777777 & net4$mf2aid!=88888888 & net4$mf2aid!=99959995), 1, 0)
net4$mf3aidc9 <- ifelse((!is.na(net4$mf3aid) & net4$mf3aid!=77777777 & net4$mf3aid!=88888888 & net4$mf3aid!=99959995), 1, 0)
net4$mf4aidc9 <- ifelse((!is.na(net4$mf4aid) & net4$mf4aid!=77777777 & net4$mf4aid!=88888888 & net4$mf4aid!=99959995), 1, 0)
net4$mf5aidc9 <- ifelse((!is.na(net4$mf5aid) & net4$mf5aid!=77777777 & net4$mf5aid!=88888888 & net4$mf5aid!=99959995), 1, 0)
net4$ff1aidc9 <- ifelse((!is.na(net4$ff1aid) & net4$ff1aid!=77777777 & net4$ff1aid!=88888888 & net4$ff1aid!=99959995), 1, 0)
net4$ff2aidc9 <- ifelse((!is.na(net4$ff2aid) & net4$ff2aid!=77777777 & net4$ff2aid!=88888888 & net4$ff2aid!=99959995), 1, 0)
net4$ff3aidc9 <- ifelse((!is.na(net4$ff3aid) & net4$ff3aid!=77777777 & net4$ff3aid!=88888888 & net4$ff3aid!=99959995), 1, 0)
net4$ff4aidc9 <- ifelse((!is.na(net4$ff4aid) & net4$ff4aid!=77777777 & net4$ff4aid!=88888888 & net4$ff4aid!=99959995), 1, 0)
net4$ff5aidc9 <- ifelse((!is.na(net4$ff5aid) & net4$ff5aid!=77777777 & net4$ff5aid!=88888888 & net4$ff5aid!=99959995), 1, 0)


# How many of each type of nomination did each respondent make?  Sum indicator for type of nomination across each of ten possible nominations
net4$faid7 <- (net4$mf1aid7 + net4$mf2aid7 + net4$mf3aid7 + net4$mf4aid7 + net4$mf5aid7 + net4$ff1aid7 + net4$ff2aid7 + net4$ff3aid7 + net4$ff4aid7 + net4$ff5aid7)
net4$faid8 <- (net4$mf1aid8 + net4$mf2aid8 + net4$mf3aid8 + net4$mf4aid8 + net4$mf5aid8 + net4$ff1aid8 + net4$ff2aid8 + net4$ff3aid8 + net4$ff4aid8 + net4$ff5aid8)
net4$faid9 <- (net4$mf1aid9 + net4$mf2aid9 + net4$mf3aid9 + net4$mf4aid9 + net4$mf5aid9 + net4$ff1aid9 + net4$ff2aid9 + net4$ff3aid9 + net4$ff4aid9 + net4$ff5aid9)
net4$faid95 <- (net4$mf1aid95 + net4$mf2aid95 + net4$mf3aid95 + net4$mf4aid95 + net4$mf5aid95 + net4$ff1aid95 + net4$ff2aid95 + net4$ff3aid95 + net4$ff4aid95 + net4$ff5aid95)
net4$faidna <- (net4$mf1aidna + net4$mf2aidna + net4$mf3aidna + net4$mf4aidna + net4$mf5aidna + net4$ff1aidna + net4$ff2aidna + net4$ff3aidna + net4$ff4aidna + net4$ff5aidna)
net4$faidct <- (net4$mf1aidc + net4$mf2aidc + net4$mf3aidc + net4$mf4aidc + net4$mf5aidc + net4$ff1aidc + net4$ff2aidc + net4$ff3aidc + net4$ff4aidc + net4$ff5aidc) # sum on roster at sch or sister sch (all but special codes) across 10 nominations
net4$faidct9 <- (net4$mf1aidc9 + net4$mf2aidc9 + net4$mf3aidc9 + net4$mf4aidc9 + net4$mf5aidc9 + net4$ff1aidc9 + net4$ff2aidc9 + net4$ff3aidc9 + net4$ff4aidc9 + net4$ff5aidc9) # sum everything but special codes 7, 8, 95 across all nominations

label(net4$faid7) <- "# Nominations not at respondent's or sister school"
label(net4$faid8) <- "# Nominations to sister school, but not on roster"
label(net4$faid9) <- "# Nominations to respondent's school, but not on roster"
label(net4$faid95) <- "# Nominations, nomination out of range"
label(net4$faidna) <- "# Nominations is.na == TRUE"
label(net4$faidct) <- "# Nominations, no special codes"
label(net4$faidct9) <- "# Nominations to same school, regardless of roster, and sister school if on roster"


net4$nomtotal <- net4$faid7+net4$faid8+net4$faid9+net4$faid95+net4$faidna+net4$faidct
describe(net4$nomtotal) #yay! it always equals 10
net4$nomtotal <- NULL

dropVars <- names(net4) %in% c("mf1aid7", "mf2aid7", "mf3aid7", "mf4aid7", "mf5aid7", "ff1aid7", "ff2aid7", "ff3aid7", "ff4aid7", "ff5aid7",
                               "mf1aid8", "mf2aid8", "mf3aid8", "mf4aid8", "mf5aid8", "ff1aid8", "ff2aid8", "ff3aid8", "ff4aid8", "ff5aid8",
                               "mf1aid9", "mf2aid9", "mf3aid9", "mf4aid9", "mf5aid9", "ff1aid9", "ff2aid9", "ff3aid9", "ff4aid9", "ff5aid9",
                               "mf1aid95", "mf2aid95", "mf3aid95", "mf4aid95", "mf5aid95", "ff1aid95", "ff2aid95", "ff3aid95", "ff4aid95", "ff5aid95",
                               "mf1aidna", "mf2aidna", "mf3aidna", "mf4aidna", "mf5aidna", "ff1aidna", "ff2aidna", "ff3aidna", "ff4aidna", "ff5aidna",
                               "mf1aidc9", "mf2aidc9", "mf3aidc9", "mf4aidc9", "mf5aidc9", "ff1aidc9", "ff2aidc9", "ff3aidc9", "ff4aidc9", "ff5aidc9") 
net4 <- net4[!dropVars]


dropVars <- names(net4) %in% c("scid.inhome", "scid.net", "nscid.net", "nscid.inhome", "sscid", "sschlcde.inhome","sschlcde.inschool", 
                               "nsschlcde.inhome", "nsschlcde.inschool", "ncommid")
net4 <- net4[!dropVars]
rm("dropVars")


# Note that James Moody created the following variables in network.xpt:                    
# tab113 = completed nominations to sister school on roster
# noutnom is the sum of 7, 8, and tab113 (nominations to sister school on roster)

  
# If the same friend is nominated multiple times, set later nominations to NA
   # Note that the lines below make the nomination type indicators above slightly out of date 

net4$mf2aid <- with(net4, ifelse((mf2aidc==1) & mapply(identical, x=mf2aid, y=mf1aid), NA, mf2aid))
net4$mf3aid <- with(net4, ifelse((mf3aidc==1) & (mapply(identical, x=mf3aid, y=mf2aid) |
                                                  mapply(identical, x=mf3aid, y=mf1aid)), NA, mf3aid))
net4$mf4aid <- with(net4, ifelse((mf4aidc==1) & (mapply(identical, x=mf4aid, y=mf3aid) |
                                                  mapply(identical, x=mf4aid, y=mf2aid) |
                                                  mapply(identical, x=mf4aid, y=mf1aid)), NA, mf4aid))
net4$mf5aid <- with(net4, ifelse((mf5aidc==1) & (mapply(identical, x=mf5aid, y=mf4aid) |
                                                  mapply(identical, x=mf5aid, y=mf3aid) |
                                                  mapply(identical, x=mf5aid, y=mf2aid) |
                                                  mapply(identical, x=mf5aid, y=mf1aid)), NA, mf5aid))

net4$ff2aid <- with(net4, ifelse((ff2aidc==1) & mapply(identical, x=ff2aid, y=ff1aid), NA, ff2aid))
net4$ff3aid <- with(net4, ifelse((ff3aidc==1) & (mapply(identical, x=ff3aid, y=ff2aid) |
                                                  mapply(identical, x=ff3aid, y=ff1aid)), NA, ff3aid))
net4$ff4aid <- with(net4, ifelse((ff4aidc==1) & (mapply(identical, x=ff4aid, y=ff3aid) |
                                                  mapply(identical, x=ff4aid, y=ff2aid) |
                                                  mapply(identical, x=ff4aid, y=ff1aid)), NA, ff4aid))
net4$ff5aid <- with(net4, ifelse((ff5aidc==1) & (mapply(identical, x=ff5aid, y=ff4aid) |
                                                  mapply(identical, x=ff5aid, y=ff3aid) |
                                                  mapply(identical, x=ff5aid, y=ff2aid) |
                                                  mapply(identical, x=ff5aid, y=ff1aid)), NA, ff5aid))

# Below I use three different sources of data on respondents' gender to create net4$male
# with imputed missing values and a best-guess unless the data is highly conflictual. 
#   1. self-described gender Inschool.xpt survey
#   2. self-described gender in-home Allwave1.xpt survey (only 20% subsambple)
#   3. how often were they nominated by others as each gender category (male friends or female friends) 
#       by other students (in-school survey but data is from sfriend.xpt)
                                                  
femaleFriends <- unlist(net4[c("ff1aid", "ff2aid", "ff3aid", "ff4aid", "ff5aid")])
femaleIndegrees <- as.data.frame(table(femaleFriends))
net4$indegreeAsFemale <- femaleIndegrees$Freq[match(net4$aid2, femaleIndegrees$femaleFriends)]
rm("femaleFriends", "femaleIndegrees")
net4$indegreeAsFemale <- ifelse(is.na(net4$indegreeAsFemale), 0, net4$indegreeAsFemale)
describe(net4$indegreeAsFemale)                 


maleFriends <- unlist(net4[c("mf1aid", "mf2aid", "mf3aid", "mf4aid", "mf5aid")])
maleIndegrees <- as.data.frame(table(maleFriends))
net4$indegreeAsMale <- maleIndegrees$Freq[match(net4$aid2, maleIndegrees$maleFriends)]
rm("maleFriends", "maleIndegrees")
net4$indegreeAsMale <- ifelse(is.na(net4$indegreeAsMale), 0, net4$indegreeAsMale)
describe(net4$indegreeAsMale)                 


net4$indegreeAsMF <- net4$indegreeAsMale + net4$indegreeAsFemale
describe(net4$indegreeAsMF)

net4$indegreeAsMaleP <- ifelse(net4$indegreeAsMF>0, 
                              net4$indegreeAsMale/(net4$indegreeAsMale+net4$indegreeAsFemale), .5)
describe(net4$indegreeAsMaleP)

net4$maleLikely <- with(net4, ifelse(indegreeAsMale>0 & net4$indegreeAsMaleP>.51, 1, 
                  ifelse(indegreeAsFemale>0 & net4$indegreeAsMaleP<.49, 0, NA)))

net4$maleVeryLikely <- with(net4, ifelse(indegreeAsMale>1.1 & net4$indegreeAsMaleP>.76, 1, 
                      ifelse(indegreeAsFemale>1.1 & net4$indegreeAsMaleP<.24, 0, NA)))


net4$male.is <- with(net4, ifelse(as.numeric(s2)==9, NA, s2)) #from in-school survey
net4$bio.sexn <- with(net4, ifelse(as.numeric(bio.sex)>=3, NA, bio.sex))

net4$s2male <- (with(net4, ifelse(is.na(male.is) & is.na(bio.sexn), NA,
                        ifelse(is.na(male.is) & !is.na(bio.sexn), bio.sexn,
                        ifelse(!is.na(male.is) & is.na(bio.sexn), male.is,
                        ifelse(male.is==1 & bio.sexn==1, 1,
                        ifelse(male.is==2 & bio.sexn==2, 2, NA))))))-2)*(-1)
label(net4$s2male) <- "Male indicator, imputed values from inhome$bio.sex & s2, conflicts become NA"


net4$male <- with(net4, ifelse(!is.na(maleVeryLikely), maleVeryLikely, ifelse(!is.na(s2male), s2male, maleLikely)))
label(net4$male) <- "Male Indicator, values imputed/corrected using inhome$bio.sex & s2 & nomination data"                 

           
dropVars <- names(net4) %in% c("indegreeAsFemale", "indegreeAsMale", "indegreeAsMaleP",
              "indegreeAsMF", "sexConflict","maleVeryLikely", "maleLikely", "s2male", "male.is", "bio.sexn", "bio.sex")
net4 <- net4[!dropVars]                 
rm("dropVars")

# Some social network algorithms may fail when there are two "identical" ties, 
# i.e., when one friend is nominated twice. I already eliminated duplicates if they were both nominated 
# as the same gender.  If they were nominated once under "Name your five best male friends" and once under, 
# "Name your five best female friends" this raises the question of which one to keep.  
#     Because this happens very rarely, I take the lazy way out and
# simply set nominations to males to NA if they are also nominated, by the same person, as females. 

# First, let's get a sense of how big a problem I'm sweeping under the rug:
mf1aid.conflict <- unlist(with(net4, ifelse((mf1aidc==1) & (mapply(identical, x=mf1aid, y=ff5aid) |
                                                  mapply(identical, x=mf1aid, y=ff4aid) |
                                                  mapply(identical, x=mf1aid, y=ff3aid) |
                                                  mapply(identical, x=mf1aid, y=ff2aid) |
                                                  mapply(identical, x=mf1aid, y=ff1aid)), mf1aid, NA)))
sum(net4$mf1aid.conflict) # 6 cases where the same person is nominated twice, once as male, and once as female, 
                                    # by the same nominator - not a big deal for most analyses
mf1aid.conflict <- data.frame(table(mf1aid.conflict))
mf1aid.conflict$male <- net4$male[match(mf1aid.conflict$mf1aid.conflict, net4$aid2)]
mf1aid.conflict
rm("mf1aid.conflict")

# I set nominations to males to NA if they are also nominated, by the same person, as females.
# A better approach would certainly be possible by carefully considering discrepancies between
# self-indicated gender, and whether/how different people nominate someone as either a male or female friend.
# I don't do anything about the fact that some people are nominated as a "male friend" by one 
# person, and a "female friend" by another.
   # Note that the lines below make the nomination type indicators above slightly out of date                 
                                                 
net4$mf1aid <- with(net4, ifelse((mf1aidc==1) & (mapply(identical, x=mf1aid, y=ff5aid) |
                                                  mapply(identical, x=mf1aid, y=ff4aid) |
                                                  mapply(identical, x=mf1aid, y=ff3aid) |
                                                  mapply(identical, x=mf1aid, y=ff2aid) |
                                                  mapply(identical, x=mf1aid, y=ff1aid)), NA, mf1aid))
                                                  
net4$mf2aid <- with(net4, ifelse((mf2aidc==1) & (mapply(identical, x=mf2aid, y=ff5aid) |
                                                  mapply(identical, x=mf2aid, y=ff4aid) |
                                                  mapply(identical, x=mf2aid, y=ff3aid) |
                                                  mapply(identical, x=mf2aid, y=ff2aid) |
                                                  mapply(identical, x=mf2aid, y=ff1aid)), NA, mf2aid))

net4$mf3aid <- with(net4, ifelse((mf3aidc==1) & (mapply(identical, x=mf3aid, y=ff5aid) |
                                                  mapply(identical, x=mf3aid, y=ff4aid) |
                                                  mapply(identical, x=mf3aid, y=ff3aid) |
                                                  mapply(identical, x=mf3aid, y=ff2aid) |
                                                  mapply(identical, x=mf3aid, y=ff1aid)), NA, mf3aid))

net4$mf4aid <- with(net4, ifelse((mf4aidc==1) & (mapply(identical, x=mf4aid, y=ff5aid) |
                                                  mapply(identical, x=mf4aid, y=ff4aid) |
                                                  mapply(identical, x=mf4aid, y=ff3aid) |
                                                  mapply(identical, x=mf4aid, y=ff2aid) |
                                                  mapply(identical, x=mf4aid, y=ff1aid)), NA, mf4aid))

net4$mf5aid <- with(net4, ifelse((mf5aidc==1) & (mapply(identical, x=mf5aid, y=ff5aid) |
                                                  mapply(identical, x=mf5aid, y=ff4aid) |
                                                  mapply(identical, x=mf5aid, y=ff3aid) |
                                                  mapply(identical, x=mf5aid, y=ff2aid) |
                                                  mapply(identical, x=mf5aid, y=ff1aid)), NA, mf5aid))

                                                  
# Create school means for various variables
netag <- aggregate(net4[,c("faid7", "faid8", "faid9", "faid95", "faidna", "faidct", "odgx2", "tab113"), drop=FALSE], 
                   na.rm=TRUE, by=list(sschlcde2=net4$sschlcde2), FUN=mean)

names(netag) <- c("sschlcde","mfaid7", "mfaid8", "mfaid9", "mfaid95", "mfaidna", "mfaidct", "modgx2", "mtab113")


dropVars <- names(net4) %in% c("mf1aidc", "mf2aidc", "mf3aidc", "mf4aidc", "mf5aidc", 
                               "ff1aidc", "ff2aidc", "ff3aidc", "ff4aidc", "ff5aidc")
net4 <- net4[!dropVars]
rm("dropVars")

load("J:/R/m6dd.RData")
net4 <- merge(net4, m6d[c("aid2", "race4f","race5f")], by="aid2", all=TRUE)
rm("m5d","m6f","m6m","m6s")
names(net4black)
net4black <- net4[which(net4$race5f=="Black"),]

names(edunet)
describe(net4$enclustw)
describe(net4$enpclusw)
describe(net4$encombcw)
table(net4[which(is.na(net4$encombcw)==FALSE)]$ncommid2,net4$encombcw, useNA="ifany")
table(net4$ncommid2[which(is.na(net4$encombcw)==FALSE)],net4$encombcw[which(is.na(net4$encombcw)==FALSE)], useNA="ifany")

describe(net4[which(is.na(net4$encombcw)==FALSE)]$ncommid2)
describe(net4$ncommid2[which(is.na(net4$encombcw)==FALSE)])
describe(net4$ncommid2)
describe(net4$encombcw)
?split
?cbind

#net4$comEncombcw <- outer(net4$ncommid2, net4$encombcw, paste, sep="")
net4$comEncombcw <- cbind(net4$ncommid2, net4$encombcw)
describe(net4$comEncombcw)

essentiallyZeroNetworkDataSchools <- c("46", "61", "70", "79", "94", "179", "203", "222", "265", "322", "327") #79 has 1 nom
netsch <- net4[which(!net4$sschlcde2 %in% essentiallyZeroNetworkDataSchools),] 
netsch <- net4black[which(!net4black$sschlcde2 %in% essentiallyZeroNetworkDataSchools),]

#Break net4 into list of dataframes based on ncommid2
netSplit <- split(netsch,netsch$ncommid2)
rm("essentiallyZeroNetworkDataSchools", "netsch")


#Define a function to apply to each element of netSplit
MakeIgraphList <- function(dataPiece) {
    netEdges <- NULL

    for (idi in c("mf1aid", "mf2aid", "mf3aid", "mf4aid", "mf5aid", "ff1aid", "ff2aid", "ff3aid", "ff4aid", "ff5aid")) {
        netEdge <- dataPiece[c("aid2", idi)]
        names(netEdge) <- c("aid2", "friendID")
        netEdge$weight <- 1
        netEdges <- rbind(netEdges, netEdge)
    }
    
    netEdges <- netEdges[netEdges$friendID %in% netEdges$aid2,]
    #netEdges[netEdges$aid2==netEdges$friendID,]
    netEdges <- netEdges[netEdges$aid2!=netEdges$friendID,]
    g <- graph.data.frame(netEdges, directed=TRUE)
    isolatedVertices <- dataPiece$aid2[which(dataPiece$aid2>90000000 & !dataPiece$aid2 %in% V(g)$name)] #only those who were on the roster
    g2 <- add.vertices(g, length(isolatedVertices), name=isolatedVertices)
    g2
}



rm("inhome", "inschool", "sfri", "net2", "net3")
rm("df.commid", "net", "netag", "vertexAttrib")
gc()
lsos()
memory.profile()
library(igraph)
igraphList <- lapply(netSplit,FUN = makeIgraphList) 

MakeVertexAttribList <- function(netList) {
  vertexAttrib <- data.frame(as.numeric(as.character(V(netList)$name)), 
                             degree(netList, mode="in"), 
                             degree(netList, mode="out") 
                             )
  names(vertexAttrib) <- c("aid2", "indegreeb", "outdegreeb")
  vertexAttrib
}

MakeVertexAttribList <- function(netList) {
  vertexAttrib <- data.frame(as.numeric(as.character(V(netList)$name)), 
                             degree(netList, mode="in"), 
                             degree(netList, mode="out"), 
                             alpha.centrality(netList, alpha=0.09),                       
                             alpha.centrality(netList, alpha=0.1, weights=E(netList)$weight),
                             bonpow(netList, exponent=0.09), 
                             bonpow(netList, exponent=0.1))
  names(vertexAttrib) <- c("aid2", "indegree", "outdegree", "acent09", "acent10", "bonpow09", "bonpow10")
  vertexAttrib
}

                  

vertexAttrib <- do.call("rbind", lapply(igraphList, FUN = MakeVertexAttribList))

net4 <- merge(net4, vertexAttrib, by="aid2", all=TRUE)

names(net4)
ls()
#rm("vertexAttrib")
#save.image(file.path(kDataPath, "J:/R/net4share.RData"))
# save.image(file.path(kDataPath, "J:/R/net4simple.RData")) # removed extra data from net4share

### Note: Like most social science data, this social network data is somewhat messy
# For example, there is data missing, and it is not missing at random.
# all variables created in vertexAttrib will be missing if two conditions hold
# 1) respondent could not be linked to roster, AND 2) respondent did not nominate anyone
# Use care in interpreting, e.g. measures of centrality given that they are calculated
# for some vertices which were not on roster and therefore unable to receive nominations
# There are many other things to consider: 
# e.g. that the response rate and roster accuracty varies considerably across schools