# load the XML library
library(XML)

# function to get contract data from DUTB ####
getCreditData <- function(bank) {

if (bank=="NLB") {url <- "http://www.dutb.eu/si/o-nas/informacije-javnega-znacaja/pogodbe-nlb"}
if (bank=="NKBM") {url <- "http://www.dutb.eu/si/o-nas/informacije-javnega-znacaja/pogodbe-nkbm"}
  
html <- htmlTreeParse(url, useInternalNodes = TRUE)

# save all nodes that are in a table
nodes <- xpathSApply(html, "//tbody/tr/td[@*]",xmlValue)
# save the length (number) of nodes
numEntries <- length(nodes)

# save the data from the nodes to a data frame with named columns
data <- data.frame("ID"    = nodes[seq(from=1, to=numEntries, by=10)],
                   "Naziv" = nodes[seq(from=2, to=numEntries, by=10)],
                   "Sedez" = nodes[seq(from=3, to=numEntries, by=10)],
                   "Kreditodajalec" = nodes[seq(from=4, to=numEntries, by=10)],
                   "St.pogodbe" = nodes[seq(from=5, to=numEntries, by=10)],
                   "ZnesekEUR" = nodes[seq(from=6, to=numEntries, by=10)],
                   "Vrstaposla" = nodes[seq(from=7, to=numEntries, by=10)],
                   "Datumpogodbe" = nodes[seq(from=8, to=numEntries, by=10)],
                   "Vrstazavarovanja" = nodes[seq(from=9, to=numEntries, by=10)],
                   "Predmetzavaroavanja" = nodes[seq(from=10, to=numEntries, by=10)]
)

# transform variables into appropriate format
data[, "ZnesekEUR"] <- gsub(pattern="\\.", replacement = "", x=data[, "ZnesekEUR"])
data[, "ZnesekEUR"] <- gsub(pattern=",", replacement = "\\.", x=data[, "ZnesekEUR"])
data[, "ZnesekEUR"] <- as.numeric(data[, "ZnesekEUR"])
data[, "Datumpogodbe"] <- as.Date(data[, "Datumpogodbe"], format="%d.%m.%Y")

return(data)
}

# get data from website
dataNLB <- getCreditData("NLB")
dataNKBM <- getCreditData("NKBM")

# list info about the data
str(dataNLB)
str(dataNKBM)
head(dataNLB)
head(dataNKBM,100)
summary(dataNLB)
summary(dataNKBM)

# plot the data
library(ggplot2)
library(scales)

# function to get top x contracts by value ####
gettopContracts <- function (data, topN) {
  dataSubset <- head(data[order(data[,c("ZnesekEUR")], decreasing = TRUE),], topN) # top contracts dataset
  dataSubset <- dataSubset[!duplicated(dataSubset[,"St.pogodbe"]),] # remove duplicates of contract numbers
  return(dataSubset)
}

# helper function to format decimal separator ####
dot <- function(x) {gsub(pattern=",", replacement=".", x=as.character(comma(x, decimal.mark=",")))}


# get data for top N contracts
N <- 100
dataSubsetNLB <- gettopContracts(dataNLB, N)
dataSubsetNKBM <- gettopContracts(dataNKBM, N)

# plot NLB top contracts
ggplot(data=dataSubsetNLB, aes(x=Naziv, y=ZnesekEUR)) + geom_bar(stat="identity") + theme_bw() +
  scale_x_discrete(limits=unique(dataSubsetNLB[,"Naziv"])) + scale_y_continuous(labels=dot) + 
  ggtitle(paste("Po zneskih najvišjih", N,"pogodb (NLB)")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plot NKBM top contracts
ggplot(data=dataSubsetNKBM, aes(x=Naziv, y=ZnesekEUR)) + geom_bar(stat="identity") + theme_bw() +
  scale_x_discrete(limits=unique(dataSubsetNKBM[,"Naziv"])) + scale_y_continuous(labels=dot) + 
  ggtitle(paste("Po zneskih najvišjih", N,"pogodb (NKBM)")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
