## 1st way
conNLB <- url("http://www.dutb.eu/si/o-nas/informacije-javnega-znacaja/pogodbe-nlb")
htmlNLB <- readLines(conNLB)
close(conNLB)
htmlNLB


### 2nd way
library(XML)
urlNLB <- "http://www.dutb.eu/si/o-nas/informacije-javnega-znacaja/pogodbe-nlb"
htmlNLB2 <- htmlTreeParse(urlNLB, useInternalNodes = TRUE)

rootNode <- xmlRoot(htmlNLB2)
class(rootNode)
xmlName(rootNode)
names(rootNode)
names(rootNode[["body"]])

bodyNLB <- rootNode[["body"]]
rootNode[["body"]][["text"]]
rootNode[["body"]][[5]]

names(bodyNLB[["div"]][["div"]][["div"]])

str(names(bodyNLB))
xmlName(bodyNLB)

for (name in names(bodyNLB)) {
  print(names(bodyNLB[[name]]))
  
}


tmp <- xpathApply(htmlNLB2, "//tr[@class='tabrow dutb-dt-row']",xmlValue)

class(htmlNLB2)
class(htmlNLBTable)
aaa <- getNodeSet(htmlNLB2, "//tbody/tr/td[@class='tabcol']")
str(lapply(aaa, function(x) {xmlSApply(x, xmlValue)}))
str(aaa)
rootNode[[2]][[7]]

htmlNLBTable <- xpathApply(htmlNLB2, "//tbody")
tmp <- xpathSApply(htmlNLB2, "//tbody/tr/td[@class='tabcol']",xmlValue)
tmp <- xpathSApply(htmlNLB2, "//tbody/tr/td[@*]",xmlValue)
tmp
head(tmp[[1]],10)
tmp[[2]]
length(tmp)/2222
head(tmp)
class(tmp)

numEntries <- length(tmp)

genIndex <- function(start=1, end, by) {
seq(from=start, to=end, by=10)
}
data <- data.frame("ID"    = tmp[seq(from=1, to=numEntries, by=10)],
                   "Naziv" = tmp[seq(from=2, to=numEntries, by=10)],
                   "Sedez" = tmp[seq(from=3, to=numEntries, by=10)],
                   "Kreditodajalec" = tmp[seq(from=4, to=numEntries, by=10)],
                   "St.pogodbe" = tmp[seq(from=5, to=numEntries, by=10)],
                   "ZnesekEUR" = tmp[seq(from=6, to=numEntries, by=10)],
                   "Vrstaposla" = tmp[seq(from=7, to=numEntries, by=10)],
                   "Datumpogodbe" = tmp[seq(from=8, to=numEntries, by=10)],
                   "Vrstazavarovanja" = tmp[seq(from=9, to=numEntries, by=10)],
                   "Predmetzavaroavanja" = tmp[seq(from=10, to=numEntries, by=10)]
  )
head(data)

## 3rd way
library(httr)
htmlNLB3 <- GET(urlNLB)
contentNLB <- content(htmlNLB3, as="text")
parsedNLB <- htmlParse(contentNLB, asText=TRUE)
xpathApply(parsedNLB, "//table",xmlValue)
