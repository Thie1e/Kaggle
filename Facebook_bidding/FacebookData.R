library(data.table)
library(caret)
library(doSNOW)
library(beepr)
library(bit64)
library(plyr)
library(dplyr)
library(reshape2)
library(countrycode)
library(ggplot2)
library(tau)
library(convenience)
library(Rtsne)

# find indices where a vector changes
dup <- function(v) data.table:::uniqlist(list(v))

# Load Workspace?
load("FacebookDataWS.RData")

train <- fread("Data//train.csv")
bids <- fread("Data//bids.csv")
storeThese("bids")
test <- fread("Data//test.csv")

# Datenüberblick -----------------------------------------------------------
# 6614 Bieter in bids, 2013 in train, 4630 in test, public LB also ca. 1400
# 15051 Auktionen insgesamt
# 7351 devices, ist das Telefonmodell des Bieters
# Bids from 200 countries
# 2303991 verschiedene IPs.
# 2013 verschiedene Payment accounts, so viele wie Beobachtungen.
# 2013 verschiedene Adressen, so viele wie Beobachtungen.

# (table(bids$merchandise))
# auto parts  books and music         clothing        computers        furniture       home goods          jewelry
# 9757            51941            16447            81084            99181          1224234          1902058
# mobile office equipment   sporting goods
# 2126587           289838          1855207
# also 10 Kategorien von Produkten

# Anzahl an Beobachtungen pro Auktion
# > summary(as.numeric(table(bids$auction)))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# 1.0      7.0     38.0    508.7    197.0 537300.0

# table(train$outcome)
# 0    1
# 1910  103
# 5,12% sind Bots


# Features erzeugen ----------------------------------------------------------
### Merge with bids
dat <- merge(train,
             bids[, .(bidder_id, auction, merchandise, device, time,
                      country, ip, url)],
             by = "bidder_id", all.x = T)
dat <- na.omit(dat)

test2 <- merge(test,
               bids[, .(bidder_id, auction, merchandise, device, time,
                        country, ip, url)],
               by = "bidder_id", all.x = T)
test2$outcome <- NA

dat <- rbind(dat, test2)
rm(test2); gc()

table(dat$outcome, useNA = "always")
# 0       1    <NA>
# 2658808  412416 4585180

# Merchandise Feld ist messy. Als tatsächliche Kategorie diejenige annehmen, die
# am häufigsten im merchandise Feld vorkommt. Falls zwei verschiedene Kategorien
# gleich häufig vorkommen, den merchandise Wert vorerst auf NA setzen
maxMerch <- dat[, .(merchandise.max = names(tail(sort(table(merchandise)),1))),
                by = auction]
dat <- merge(dat, maxMerch, by = "auction")

load("SOARcache/dat@.RData")
storeThese("dat")

auctions <- unique(dat$auction)

# Gebote pro Bieter und Auktion
nBids <- list()
for (a in seq_along(auctions)){
      tab <- table(dat[auction == auctions[a]]$bidder_id)
      tab <- data.frame(tab, auctions[a])
      nBids[[a]] <- tab
}
nBids <- do.call(rbind, nBids)
colnames(nBids) <- c("bidder_id", "nBids", "auction")
nBids <- data.table(nBids)

nBidsSD <- nBids[, .(nBidsSD = sd(nBids)), by = bidder_id]
# Wenn ein Bieter nur ein Gebot hat, ist die SD = NA

nBidsRange <- nBids[, .(nBidsRange = (max(nBids) - min(nBids))), by = bidder_id]

# Verschiedene IPs pro Bieter und Auktion
nIPs <- list()
for (a in seq_along(auctions)){
      tempAuction <- dat[auction == auctions[a]]
      ips <- tapply(tempAuction$ip, INDEX = tempAuction$bidder_id,
                    function(x) length(unique(x)))
      nIPs[[a]] <- data.frame(rownames(ips), ips, auctions[a])
}
nIPs <- do.call(rbind, nIPs)
colnames(nIPs) <- c("bidder_id", "nIPs", "auction")
nIPs <- data.table(nIPs)

nIPsSD <- nIPs[, .(nIPsSD = sd(nIPs)), by = bidder_id]
# Wenn ein Bieter nur eine IP hat, ist die SD = NA

nIPsRange <- nIPs[, .(nIPsRange = (max(nIPs) - min(nIPs))), by = bidder_id]

# IP n-grams ---------------------------------------------------------------
# Ich muss mit lapply über den Vektor loopen, weil sonst das Ende einer IP
# zusammen mit dem Anfang der nächsten als Bigram gezählt wird
temp2 <- lapply(dat$ip, function(x) textcnt(x, n = 2L, split = "\\.", method = "string"))
temp2 <- unlist(temp2)
# Durch das lapply muss ich noch die n-grams zusammenzählen. Sehr wenige kommen
# jetzt schon 2 mal vor
temp2 <- c(temp2, temp2[which(temp2 == 2)])
# Die n-grams kann ich jetzt einfach anhand der Namen (das sind die bigrams) zählen
temp2 <- table(names(temp2))
commonIPstarts <- head(sort(temp2, decreasing = T), 500)
pat <- gsub(names(commonIPstarts), pattern = " ", replacement = "\\\\.")
pat <- sapply(pat, function(x) paste("^", x, sep = ""))
pat <- paste(pat, collapse = "|")
temp3 <- dat[(grep(pattern = pat, x = dat$ip))]
# Percent of all data that has a "common" IP part
nrow(temp3)/nrow(dat) # 22%, mit Test data 18%
# % Bots mit common IP
table(temp3$outcome)/nrow(temp3) # 5% bots
table(dat$outcome)/nrow(dat) # 13,4% bots
# Bei den Geboten mit common IPs sind ca. 5% von Bots, insgesamt in den Daten 13,4%
# Sind die IPs eines Bieters eher common oder uncommon?
commonIPmean <- dat[, .(commonIPmean = mean(length(grep(pattern = pat, x = ip)) / length(ip))),
                    by = bidder_id]
# temp <- merge(dat4[, .(bidder_id, outcome)], commonIPmean, by = "bidder_id")
# temp <- data.frame(temp)
# temp$outcome <- factor(temp$outcome, labels = c("human", "bot"))
# summary(glm(outcome ~ commonIPmean, data = temp, family = "binomial"))
# Call:
#       glm(formula = outcome ~ commonIPmean, family = "binomial", data = temp)
#
# Deviance Residuals:
#       Min       1Q   Median       3Q      Max
# -0.3520  -0.3520  -0.3344  -0.3072   2.9480
#
# Coefficients:
#       Estimate Std. Error z value Pr(>|z|)
# (Intercept)   -2.7504     0.1187  -23.17   <2e-16 ***
#       commonIPmean  -1.5820     0.7642   -2.07   0.0384 *


# Gibt es irgendein IP Bigram, das besonders vielen Bots zugeordnet ist?
# Daten zusammenfassen: Alle IPs eines Bieters und dazu das outcome
bidderIPs <- dat[, .(ip = unique(ip)), by = bidder_id]
bidderIPs <- merge(bidderIPs, train[, .(outcome, bidder_id)],
                         by = "bidder_id", all.x = T)
ipBigrams <- data.frame(bigram = gsub(pattern = " ", replacement = ".", x = names(temp2)),
                        count = as.numeric(temp2),
                        stringsAsFactors = F, row.names = NULL)


### Wahrscheinlichkeit jeder IP als Mittelwert der Wahrscheinlichkeiten der
# enthaltenen Bigrams errechnen und den Durchschnitt pro Bieter bilden
ipBigrams$prob <- ipBigrams$count / sum(ipBigrams$count)
bidders <- unique(bidderIPs$bidder_id)
bidderIpBigramProbs <- lapply(bidders, function(x){
      nIteration <- which(bidders == x)
      if (nIteration %% 100 == 0) print(nIteration)
      # IPs des Bieters
      IPs <- unique(bidderIPs[x]$ip)
      bigrams <- lapply(IPs, function(ip) textcnt(ip, n = 2L, split = "\\.", method = "string"))
      bigrams <- unlist(bigrams)
      bigrams <- names(bigrams)
      bigrams <- gsub(bigrams, pattern = " ", replacement = ".")
      # Seine Bigrams abgleichen mit den globalen Wahrscheinlichkeiten der Bigrams
      sqrtProbSum <- sum(sqrt(ipBigrams[ipBigrams$bigram %in% bigrams, ]$prob))
      names(sqrtProbSum) <- "sqrtProbSum"
      probSD <- sd(ipBigrams[ipBigrams$bigram %in% bigrams, ]$prob)
      names(probSD) <- "probSD"
      probRange <- max(ipBigrams[ipBigrams$bigram %in% bigrams, ]$prob) -
            min(ipBigrams[ipBigrams$bigram %in% bigrams, ]$prob)
      names(probRange) <- "probRange"
      probs <- summary(ipBigrams[ipBigrams$bigram %in% bigrams, ]$prob)
      return(c(probSD, probRange, sqrtProbSum, probs))
})
bidderIpBigramProbs <- do.call(rbind, bidderIpBigramProbs)
bidderIpBigramProbs <- data.frame(bidder_id = bidders, bidderIpBigramProbs,
                                  stringsAsFactors = F)
bidderIpBigramProbs <- data.table(bidderIpBigramProbs)
save(bidderIpBigramProbs, file = "Mydata/bidderIpBigramProbs.RData")
# temp <- merge(dat4[, .(bidder_id, outcome)], bidderIpBigramProbs, by = "bidder_id")
# temp <- data.frame(temp, stringsAsFactors = F)
# temp$outcome <- factor(temp$outcome, labels = c("human", "bot"))
# summary(glm(outcome ~ sqrtProbSum, data = temp[, -1], family = "binomial"))
# Call:
#       glm(formula = outcome ~ Max., family = "binomial", data = temp[,
#                                                                      -1])
#
# Deviance Residuals:
#       Min       1Q   Median       3Q      Max
# -0.5589  -0.3090  -0.2626  -0.2523   2.6378
#
# Coefficients:
#       Estimate Std. Error z value Pr(>|z|)
# (Intercept)  -3.4511     0.1471 -23.462  < 2e-16 ***
#       Max.        408.5047    58.5144   6.981 2.93e-12 ***

# Habe die Variablen einzeln getestet als Regressoren für outcome.
# Mean, 3rd.Qu. und Max. sind signifikant. Max sogar hochsignifikant.
# Nachträglich habe ich noch sqrtProbSum hinzugefügt, auch hochsignifikant
# IPs hashen war nicht hilfreich


#----------------------------------------------------------------------------

# Anzahl der Geräte pro Bieter und Auktion
nDevices <- list()
for (a in seq_along(auctions)){
      tempAuction <- dat[auction == auctions[a]]
      devices <- tapply(tempAuction$device, INDEX = tempAuction$bidder_id,
                        function(x) length(unique(x)))
      nDevices[[a]] <- data.frame(rownames(devices), devices, auctions[a])
}
nDevices <- do.call(rbind, nDevices)
colnames(nDevices) <- c("bidder_id", "nDevices", "auction")
nDevices <- data.table(nDevices)

nDevicesSD <- nDevices[, .(nDevicesSD = sd(nDevices)), by = bidder_id]
# Wenn ein Bieter nur ein Gebot hat, ist die SD = NA

nDevicesRange <- nDevices[, .(nDevicesRange = (max(nDevices) - min(nDevices))),
                          by = bidder_id]

# Anzahl der insgesamt verwendeten Geräte
nDevicesOverall <- dat[, .(nDevicesOverall = length(unique(device))),
                       by = bidder_id]

# Anzahl der Länder, aus denen der Bieter geboten hat
nCountries <- dat[, .(nCountries = length(unique(country))),
                  by = bidder_id]
nCountriesRange <- dat[, .(nCountries = length(unique(country))),
                       by = .(bidder_id, auction)]
nCountriesRange <- nCountriesRange[, .(nCountriesRange = max(nCountries) - min(nCountries)),
                                   by = bidder_id]


# Aus wie vielen Ländern bietet der Bieter durchschn. pro Auktion?
meanNCountByAuc <- dat[, .(meanNCountByAuc = length(unique(country))),
                       by = .(bidder_id, auction)]
meanNCountByAuc <- meanNCountByAuc[, .(meanNCountByAuc = mean(meanNCountByAuc)),
                                   by = bidder_id]

# Anzahl verschiedener URLs im Verhältnis zur Anzahl der Gebote
nUrl <- bids[, .(nUrl = length(unique(url))), by = bidder_id]
nUrlPerBid <- merge(nUrl,
                    nBids[, .(nBids = sum(nBids)), by = bidder_id],
                    by = "bidder_id")
nUrlPerBid <- nUrlPerBid[, .(nUrlPerBid = nUrl / nBids), by = bidder_id]
nUrlSD <- bids[, .(nUrl = length(unique(url))), by = .(bidder_id, auction)]
nUrlSD <- nUrlSD[, .(nUrlSD = sd(nUrl)), by = bidder_id]
nUrlRange <- bids[, .(nUrl = length(unique(url))), by = .(bidder_id, auction)]
nUrlRange <- nUrlRange[, .(nUrlRange = max(nUrl) - min(nUrl)), by = bidder_id]
rm(bids); gc()
# temp <- merge(nUrlPerBid, dat4, by = "bidder_id")
# ggplot(aes(y = nUrlPerBid, x = factor(outcome)), data = temp) + geom_boxplot()
# summary(glm(temp$outcome ~ temp$nUrlPerBid, family = "binomial"))
# Coefficients:
#     Estimate Std. Error z value Pr(>|z|)
# (Intercept)      -1.9153     0.1536  -12.47  < 2e-16 ***
#     temp$nUrlPerBid  -2.5132     0.3872   -6.49 8.57e-11 ***


# Stelle der Gebote pro Bieter und Auktion
# Letztes Gebot, Mittelwert der Ränge der Gebote
lastBid <- list()
meanRank <- list()
for (a in seq_along(auctions)){
      tempAuction <- dat[auction == auctions[a]]
      tempAuction <- tempAuction[order(time)]
      lastBidTemp <- sapply(unique(tempAuction$bidder_id), function(x){
            tail(which(tempAuction$bidder_id == x), 1) - nrow(tempAuction)
      })
      lastBid[[a]] <- data.frame(names(lastBidTemp), lastBidTemp, auctions[a])
      meanRankTemp <- sapply(unique(tempAuction$bidder_id), function(x){
            mean(which(tempAuction$bidder_id == x) - nrow(tempAuction)) /
                  (-nrow(tempAuction))
      })
      meanRank[[a]] <- data.frame(names(meanRankTemp), meanRankTemp, auctions[a])
}
lastBid <- do.call(rbind, lastBid)
colnames(lastBid) <- c("bidder_id", "lastBid", "auction")
lastBid <- data.table(lastBid)
lastBidSD <- lastBid[, .(lastBidSD = sd(lastBid)), by = bidder_id]
lastBidRange <- lastBid[, .(lastBidRange = (max(lastBid) - min(lastBid))),
                        by = bidder_id]
meanRank <- do.call(rbind, meanRank)
colnames(meanRank) <- c("bidder_id", "meanRank", "auction")
meanRank <- data.table(meanRank)
meanRankSD <- meanRank[, .(meanRankSD = sd(meanRank)), by = bidder_id]
meanRankRange <- meanRank[, .(meanRankRange = (max(meanRank) - min(meanRank))),
                        by = bidder_id]




# Maximale Anzahl aufeinander folgender Gebote eines Bieters pro Bieter und Auktion
# und Anzahl der Sequenzen eines Bieters pro Auktion
bidSequences <- list()
for (a in seq_along(auctions)){
    tempAuction <- dat[auction == auctions[a]]
    tempAuction <- tempAuction[order(time)]
    bidders <- unique(tempAuction$bidder_id)
    auctionSeq <- sapply(bidders, function(x){
        SeqBidder <- ifelse(tempAuction$bidder_id == x, 1, 0)
        # Vektor aus Einsen und Nullen in character umwandeln, bei 0 splitten
        # und mit max(nchar()) die maximale Länge eines der entstehenden
        # Elemente messen
        maxSeqBidder <- max(nchar(unlist(strsplit(paste(as.character(SeqBidder),
                                                        collapse = ""),
                                                  split = "0"))))
        # Den Vektor aus Nullen und Einsen zu z.B. "0011110101110011" machen
        # und bei den Nullen splitten, dann Anzahl der Elemente zählen
        nSeqBidder <- length(unlist(strsplit(paste(as.character(SeqBidder),
                                                   collapse = ""),
                                             split = "0+")))
        result <- c(maxSeqBidder, nSeqBidder)
        names(result) <- c("maxSeqBidder", "nSeqBidder")
        return(result)
    })
    auctionSeq <- t(auctionSeq)
    auctionSeq <- data.frame(bidder_id = rownames(auctionSeq),
               maxSeqBidder = auctionSeq[, "maxSeqBidder"],
               nSeqBidder = auctionSeq[, "nSeqBidder"],
               auction = auctions[a],
               row.names = NULL, stringsAsFactors = F)
    bidSequences[[a]] <- auctionSeq
}
bidSequences <- do.call(rbind, bidSequences)
colnames(bidSequences) <- c("bidder_id", "maxSeq", "nSeq", "auction")
bidSequences <- data.table(bidSequences)
save(bidSequences, file = "Mydata/bidSequences.RData")
maxSeqSD <- bidSequences[, .(maxSeqSD = sd(maxSeq)), by = bidder_id]
maxSeqRange <- maxSeq[, .(maxSeqRange = (max(maxSeq) - min(maxSeq))),
                      by = bidder_id]
nSeqSD <- bidSequences[, .(nSeqSD = sd(nSeq)), by = bidder_id]
nSeqRange <- bidSequences[, .(nSeqRange = (max(nSeq) - min(nSeq))),
                      by = bidder_id]
# Plots, bringt die Differenz zwischen maxSeq und nSeq etwas?
temp <- merge(bidSequences, train, by = "bidder_id", all.x = T)
temp <- na.omit(temp)
ggplot(temp, aes(x=log(maxSeq), y=log(nSeq), shape=factor(outcome), color = factor(outcome))) +
    geom_point() +
    scale_shape_manual(values=c(1,2))
# nichts interessantes
ggplot(temp, aes(factor(outcome), (log(nSeq)))) + geom_boxplot()
# Bots tendenziell mit mehr Sequenzen, aber wohl nicht signifikant
ggplot(temp, aes(factor(outcome), (log(maxSeq)))) + geom_boxplot()
# Bots auch tendenziell mit längeren Sequenzen
ggplot(temp, aes(factor(outcome), log(maxSeq) - log(nSeq))) + geom_boxplot()
t.test(x = log(temp[outcome == 0]$maxSeq) , y = log(temp[outcome == 0]$nSeq))
t.test(x = log(temp[outcome == 1]$maxSeq) - log(temp[outcome == 1]$nSeq),
       y = log(temp[outcome == 0]$maxSeq) - log(temp[outcome == 0]$nSeq))
# t = -6.039, df = 18342, p-value = 1.581e-09
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#     -0.07278451 -0.03711424
# sample estimates:
#     mean of x mean of y
# -1.268402 -1.213453
# Die log-Differenzen von maxSeq und nSeq sind für Bots und Menschen signifikant
# unterschiedlich


### Daten vorläufig zusammenfassen ------------------------------------------
dat2 <- join_all(list(nBids, nIPs, nDevices, lastBid, meanRank, bidSequences),
                 by = c("bidder_id", "auction"))
# Outcome hinzufügen
dat2 <- merge(dat2, dat[, .(outcome = unique(outcome)), by = bidder_id],
              by = "bidder_id", by.x = T)
# merchandise.max hinzufügen
merch <- dat[, .(auction, merchandise.max)]
merch <- merch[!duplicated(merch)]
dat2 <- merge(dat2, merch, by = "auction", by.x = T)
dat2[, auction := factor(auction)]
dat2[, bidder_id := factor(bidder_id)]
dat2[, outcome := factor(outcome, labels = c("human", "bot"))]
dat2[, merchandise.max := factor(merchandise.max)]
save(dat2, file = "Mydata/dat2.RData")

dim(dat2)
# [1] 382341     11
length(unique(dat2$bidder_id))
# [1] 6614
# Es gibt 123170 Beobachtungen ("Auktionsteilnahmen") von 1984 verschiedenen
# Bietern im Trainingsset (labeled),
# es gibt insgesamt (labeled + unlabeled) 382341 Beobachtungen
# ("Auktionsteilnahmen") von 6614 verschiedenen Bietern

# Wie viele Auktionsteilnahmen hatten die jeweiligen Bieter?
bidderParticipations <- table(dat2$bidder_id)
bidderParticipations <- data.table(bidder_id = names(bidderParticipations),
                                   nParticipations = bidderParticipations)

# Auf wie viele Kategorien hat der Bieter geboten?
nMerch <- dat2[, .(nMerch = length(unique(merchandise.max))),
               by = bidder_id]

### Die Daten der Teilnahmen zusammenfassen pro Bieter
# Nur eine Zeile pro Bieter
# Zum Teil auch aus den anderen dat Tabellen
nBidsMean <- dat2[, .(nBidsMean = mean(nBids)), by = bidder_id]
sumBids <- dat2[, .(sumBids = sum(nBids)), by = bidder_id]
nIPsMean <- dat2[, .(nIPsMean = mean(nIPs)), by = bidder_id]
sumIPs <- dat2[, .(sumIPs = sum(nIPs)), by = bidder_id]
nDevicesMean <- dat2[, .(nDevicesMean = mean(nDevices)), by = bidder_id]

lastBidMean <- dat2[, .(lastBidMean = mean(lastBid)), by = bidder_id]
meanMeanRank <- dat2[, .(meanMeanRank = mean(meanRank)), by = bidder_id]
maxSeqMean <- dat2[, .(maxSeqMean = mean(maxSeq)), by = bidder_id]
maxMaxSeq <- dat2[, .(maxSeqMax = max(maxSeq)), by = bidder_id]
nSeqMean <- dat2[, .(nSeqMean = mean(nSeq)), by = bidder_id]
maxNSeq <- dat2[, .(maxNSeq = max(nSeq)), by = bidder_id]
# maxNSeq ist wohl gut geeignet, um die Bieter zu unterscheiden
temp <- join_all(list(maxMaxSeq, maxNSeq, train), by = "bidder_id")
temp <- na.omit(temp)
ggplot(aes(y = log(maxNSeq) + (3/8), x = factor(outcome)), data = temp) + geom_boxplot()
ggplot(aes(log(maxNSeq), log(maxSeqMax), color = factor(outcome)), data = temp) + geom_point()

# Worauf bietet der Bieter normalerweise?
typicalMerch <- dat2[, .(typicalMerch = names(sort(table(merchandise.max),
                                                   decreasing = T)[1])),
                     by = bidder_id]
typicalMerch[, typicalMerch := factor(typicalMerch)]

# Aus welchem Land bietet der Bieter normalerweise?
typicalCountry <- dat[, .(typicalCountry = names(sort(table(country),
                                                      decreasing = T)[1])),
                      by = bidder_id]
typicalCountry[, typicalCountry := factor(typicalCountry)]
typicalCountry[typicalCountry == "EU"]$typicalCountry <- "CH"
typicalCountry[typicalCountry == "UK"]$typicalCountry <- "GB"
# Aus irgendeinem Grund funktioniert das countrycode matching für Taiwan nicht
typicalCountry[typicalCountry == "TW"]$typicalCountry <- "CN" # Taiwan = China
# 13 Bieter ohne Land, häufigsten Wert einsetzen
typicalCountry[typicalCountry == ""]$typicalCountry <- "IN"
typicalContinent <- typicalCountry[, .(countrycode(toupper(typicalCountry),
                                                   warn = T,
                                                   origin = "iso2c",
                                                   destination = "continent")),
                                   by = bidder_id]
setnames(typicalContinent, c("bidder_id", "typicalContinent"))
typicalContinent$typicalContinent <- factor(typicalContinent$typicalContinent)
typicalRegion <- typicalCountry[, .(countrycode(toupper(typicalCountry),
                                                warn = T,
                                                origin = "iso2c",
                                                destination = "region")),
                                by = bidder_id]
setnames(typicalRegion, c("bidder_id", "typicalRegion"))
typicalRegion$typicalRegion <- factor(typicalRegion$typicalRegion)

Convert countries to longitude and latitude
library(geonames)
options(geonamesUsername="Khl4v")
allCountryCodes <- toupper(unique(typicalCountry$typicalCountry))
allCountries <- lapply(allCountryCodes, function(x) GNcountryInfo(country = x))
# An Stelle von Lon und Lat die Variablen north und east verwenden
coord <- lapply(allCountries, function(x) data.frame(north = x$north,
                                                     east = x$east,
                                                     country = x$countryCode))
coord <- do.call(rbind, coord)
# Duplikate dabei. Bilde vorsichtshalber Mittelwerte
coord <- data.table(coord)
coord$north <- as.numeric(as.character(coord$north))
coord$east <- as.numeric(as.character(coord$east))
east <- coord[, .(east = mean(east)), by = country]
north <- coord[, .(north = mean(north)), by = country]
coord <- merge(east, north, by = "country")
# 250 Länder, mehr als in allCountries (weiß nicht warum)
save(coord, file = "coord.RData")
load("Mydata/coord.RData")


# Wie oft wechselt ein Bieter während einer Auktion das Land im Mittelwert?
countryChanges <- dat[, .(countryChanges = length(dup(country)[-1])),
                      by = .(auction, bidder_id)]
countryChangesMean <- countryChanges[, .(countryChangesMean = mean(countryChanges)),
                                     by = bidder_id]
countryChangesMax <- countryChanges[, .(countryChangesMax = max(countryChanges)),
                                    by = bidder_id]
countryChangesSD <- countryChanges[, .(countryChangesSD = sd(countryChanges)),
                                    by = bidder_id]
countryChangesRange <- countryChanges[, .(countryChangesRange =
                                                max(countryChanges) - min(countryChanges)),
                                    by = bidder_id]
rm(countryChanges)

# Wie oft wechselt ein Bieter während einer Auktion die IP im Mittelwert?
ipChanges <- dat[, .(ipChanges = length(dup(ip)[-1])),
                      by = .(auction, bidder_id)]
ipChangesMean <- ipChanges[, .(ipChangesMean = mean(ipChanges)),
                                     by = bidder_id]
ipChangesMax <- ipChanges[, .(ipChangesMax = max(ipChanges)),
                                    by = bidder_id]
ipChangesSD <- ipChanges[, .(ipChangesSD = sd(ipChanges)),
                                     by = bidder_id]
ipChangesRange <- ipChanges[, .(ipChangesRange = max(ipChanges) - min(ipChanges)),
                                    by = bidder_id]
rm(ipChanges)

# Wie oft wechselt ein Bieter während einer Auktion das Gerät (mean, max)?
deviceChanges <- dat[, .(deviceChanges = length(dup(device)[-1])),
                     by = .(auction, bidder_id)]
deviceChangesMean <- deviceChanges[, .(deviceChangesMean = mean(deviceChanges)),
                                   by = bidder_id]
deviceChangesMax <- deviceChanges[, .(deviceChangesMax = max(deviceChanges)),
                                  by = bidder_id]
deviceChangesSD <- deviceChanges[, .(deviceChangesSD = sd(deviceChanges)),
                                  by = bidder_id]
deviceChangesRange <- deviceChanges[, .(deviceChangesRange =
                                              max(deviceChanges) - min(deviceChanges)),
                                  by = bidder_id]
rm(deviceChanges)

# Wie oft pro Gebot kommt der Bieter von einer nicht passenden URL, was die
# Kategorie betrifft?
merchDiscrepancy <- dat[, .(merchDiscrepancy = merchandise != merchandise.max),
     by = .(auction, bidder_id)]
merchDiscrepancyMean <- merchDiscrepancy[, .(merchDiscrepancyMean =
                                                 sum(merchDiscrepancy) /
                                                 length(merchDiscrepancy)),
                                         by = bidder_id]
rm(merchDiscrepancy)
# ggplot(aes(y = merchDiscrepancyMean, x = factor(outcome)), data = dat4) +
#     geom_boxplot()
# Nicht-Bots haben eine höhere Discrepancy
# summary(glm(factor(dat4$outcome) ~ dat4$merchDiscrepancyMean, family = "binomial"))
# Coefficients:
#                                Estimate Std. Error z value Pr(>|z|)
#     (Intercept)                -2.3809     0.1704  -13.98  < 2e-16 ***
#     dat4$merchDiscrepancyMean  -0.9194     0.2673   -3.44 0.000582 ***


# Welche mittlere Distanz legt ein Bieter theoretisch während einer Auktion
# im Mittel zurück? Distanz als Summe der Differenzen von east und north
dat$country <- toupper(dat$country)
# coord muss in der Umgebung sein
### Laufzeit um die 2 Stunden
distance <- list()
for (a in seq_along(auctions)){
      tempAuction <- dat[auction == auctions[a]]
      tempAuction <- tempAuction[order(time)]
      bidders <- unique(tempAuction$bidder_id)
      distanceBidder <- lapply(bidders, function(x){
            tempAuction2 <- tempAuction[bidder_id == x]
            countryChanges <- dup(tempAuction2$country)[-1]
            if (length(countryChanges) == 0){
                  return(data.frame(sumEast = 0, sumNorth = 0))
            } else {
                  route <- tempAuction2$country[countryChanges]
                  coordinates <- data.frame(sapply(route,
                                                   function(c) coord[country == c]))
                  sumEast <- sum(abs(diff(unlist(coordinates[rownames(coordinates) == "east", ]))))
                  sumNorth <- sum(abs(diff(unlist(coordinates[rownames(coordinates) == "north", ]))))
                  return(data.frame(sumEast = sumEast, sumNorth = sumNorth))
            }
      })
      distance[[a]] <- cbind(bidder_id = bidders, do.call(rbind, distanceBidder))
}
distance <- do.call(rbind, distance)
distance <- data.table(distance)
save(distance, file = "Mydata/distance.RData")
sumEastMean <- distance[, .(sumEast = sum(sumEast)), by = bidder_id]
bidders <-  data.frame(table(distance$bidder_id))
colnames(bidders) <- c("bidder_id", "n")
sumEastMean <- merge(sumEastMean, bidders, by = "bidder_id")
sumEastMean <- sumEastMean[, .(sumEast = sumEast / n), by = bidder_id]
sumNorthMean <- distance[, .(sumNorth = sum(sumNorth)), by = bidder_id]
sumNorthMean <- merge(sumNorthMean, bidders, by = "bidder_id")
sumNorthMean <- sumNorthMean[, .(sumNorth = sumNorth / n), by = bidder_id]
sumDistanceMean <- (merge(sumEastMean, sumNorthMean, by = "bidder_id"))
sumDistanceMean <- cbind(as.character(sumDistanceMean$bidder_id),
                     rowSums(sumDistanceMean[, .(sumEast, sumNorth)]))
sumDistanceMean <- data.table(sumDistanceMean)
setnames(sumDistanceMean, c("bidder_id", "sumDistanceMean"))
# Maximum distance that a bidder covered during one auction
distance$distanceSums <- distance[, .(distanceSum = sumEast + sumNorth)]
distanceMax <- distance[, .(distanceMax = max(distanceSums)), by = bidder_id]
distanceSD <- distance[, .(distanceSD = sd(distanceSums)), by = bidder_id]
distanceRange <- distance[, .(distanceRange = max(distanceSums) - min(distanceSums)),
                          by = bidder_id]

dat[, timeNorm := (round((time - 52631578.95) / 1e9 * 19))]
timeNormDiff <- list()
# 15051 Auktionen
for (a in seq_along(auctions)){
      if (a %% 1000 == 0) print(a)
      tempAuction <- dat[auction == auctions[a]]
      tempAuction <- tempAuction[order(timeNorm)]
      timeNormDiffsMeanAuction <- tempAuction[, .(timeNormDiffMean = mean(diff(timeNorm))),
                                          by = bidder_id]
      timeNormDiffsMinAuction <- tempAuction[, .(timeNormDiffMin = min(diff(timeNorm))),
                                         by = bidder_id]
      timeNormDiffsSDAuction <- tempAuction[, .(timeNormDiffSD = sd(diff(timeNorm))),
                                        by = bidder_id]
      timeNormDiffsRangeAuction <- tempAuction[, .(timeNormDiffRange =
                                                     max(diff(timeNorm)) - min(diff(timeNorm))),
                                           by = bidder_id]
      timeNormDiff[[a]] <- join_all(list(timeNormDiffsMeanAuction, timeNormDiffsMinAuction,
                                     timeNormDiffsSDAuction, timeNormDiffsRangeAuction),
                                by = "bidder_id")
}
timeNormDiff <- do.call(rbind, timeNormDiff)
timeNormDiff <- data.table(timeNormDiff)
timeNormDiff$timeNormDiffMin[!is.finite(timeNormDiff$timeNormDiffMin)] <- NA
timeNormDiff$timeNormDiffRange[!is.finite(timeNormDiff$timeNormDiffRange)] <- NA
timeNormDiffMean <- timeNormDiff[, .(timeNormDiffMean = mean(timeNormDiffMean, na.rm = T)),
                         by = bidder_id]
timeNormDiffMin <- timeNormDiff[, .(timeNormDiffMin = min(timeNormDiffMin, na.rm = T)),
                        by = bidder_id]
timeNormDiffMin$timeNormDiffMin[!is.finite(timeNormDiffMin$timeNormDiffMin)] <- NA
timeNormDiffSDMean <- timeNormDiff[, .(timeNormDiffSDMean = mean(timeNormDiffSD, na.rm = T)),
                           by = bidder_id]
timeNormDiffSDSD <- timeNormDiff[, .(timeNormDiffSDSD = sd(timeNormDiffSD, na.rm = T)),
                         by = bidder_id]
timeNormDiffSDRange <- timeNormDiff[, .(timeNormDiffSDRange =
                                      max(timeNormDiffSD, na.rm = T) - min(timeNormDiffSD, na.rm = T)),
                            by = bidder_id]
timeNormDiffSDRange$timeNormDiffSDRange[!is.finite(timeNormDiffSDRange$timeNormDiffSDRange)] <- NA
timeNormDiffRangeMean <- timeNormDiff[, .(timeNormDiffRangeMean = mean(timeNormDiffRange, na.rm = T)),
                              by = bidder_id]
timeNormDiffRangeMean$timeNormDiffRangeMean <- sqrt(timeNormDiffRangeMean$timeNormDiffRangeMean)
timeNormDiffRangeSD <- timeNormDiff[, .(timeNormDiffRangeSD = sd(timeNormDiffRange, na.rm = T)),
                            by = bidder_id]
timeNormDiffRangeRange <- timeNormDiff[, .(timeNormDiffRangeRange =
                                         max(timeNormDiffRange, na.rm = T) - min(timeNormDiffRange, na.rm = T)),
                               by = bidder_id]
timeNormDiffRangeRange$timeNormDiffRangeRange[!is.finite(timeNormDiffRangeRange$timeNormDiffRangeRange)] <- NA
timeNormDiffRangeRange$timeNormDiffRangeRange <- as.numeric(sqrt(timeNormDiffRangeRange$timeNormDiffRangeRange))
timeNormDiff <- merge(timeNormDiffMin, timeNormDiffMean, by = "bidder_id")
# Viele Bieter haben als Minumum 9223372036854775807 (also NA)
# Das sind sehr viele (über 1300). Diese Bieter haben pro Auktion maximal ein
# Gebot abgegeben.
# Diese Zeilen sind nur für time ohne die Normalisierung
# noMin <- which(timeNormDiff$timeNormDiffMin == max(timeNormDiff$timeNormDiffMin))
# timeNormDiff$timeNormDiffMin[noMin] <- NA
# Manche Bieter haben nur ein Gebot abgegeben und fehlen daher, weil sie
# keinen Mean und Min haben
timeNormDiff[, timeNormDiffMin := log(timeNormDiffMin + 1)]
timeNormDiff[, timeNormDiffMean := log(timeNormDiffMean + 1)]


dat3 <- join_all(list(bidderParticipations, nBidsMean, sumBids, nIPsMean,
                      nBidsSD, nBidsRange, nIPsSD, nIPsRange,
                      sumIPs, nDevicesMean, lastBidMean,
                      lastBidSD, lastBidRange, nDevicesSD, nDevicesRange,
                      meanMeanRank, maxSeqMean, maxMaxSeq, typicalMerch, nMerch,
                      meanRankSD, meanRankRange, maxSeqSD, maxSeqRange,
                      nSeqSD, nSeqRange, nSeqMean, maxNSeq,
                      nDevicesOverall, nCountries, meanNCountByAuc, nUrlPerBid,
                      nUrlRange, nUrlSD, ipChangesSD, ipChangesRange,
                      typicalCountry, typicalContinent, typicalRegion,
                      countryChangesMax, countryChangesMean, nCountriesRange,
                      countryChangesSD, countryChangesRange,
                      ipChangesMax, ipChangesMean,
                      bidderIpBigramProbs[, .(bidder_id,
                                              sqrtBigramProbSum = sqrtProbSum,
                                              bidderIpBigramProbsMean = Mean,
                                              bidderIpBigramProbsMax = Max.,
                                              bidderIpProbSD = probSD,
                                              bidderIpProbRange = probRange)],
                      deviceChangesMean, deviceChangesMax,
                      sumDistanceMean, distanceMax, distanceSD, distanceRange,
                      commonIPmean, merchDiscrepancyMean, timeNormDiffSDMean,
                      timeNormDiffSDSD, timeNormDiffSDRange, timeNormDiffRangeMean,
                      timeNormDiffRangeSD, timeNormDiffRangeRange, timeNormDiff),
                 by = "bidder_id")
dat3$country <- toupper(dat3$typicalCountry)
dat3[, typicalCountry := NULL]

# Devices per Bid
dat3[, devicesPerBid := nDevicesOverall / sumBids, by = bidder_id]

# IPs per Bid
dat3[, IPsPerBid := sumIPs / sumBids, by = bidder_id]

# Countries per Bid
dat3[, countriesPerBid := nCountries / sumBids, by = bidder_id]

save(dat3, file = "Mydata/dat3.RData")

# Payment account n-grams
# Die payment accounts sind alle einzigartig, aber viele teilen sich einen
# bestimmten Start (mehr als 20 gleiche Zeichen)
paymentAccounts <- dat[, .(Acc = unique(payment_account)), by = bidder_id]
paymentAccounts <- merge(paymentAccounts, train[, .(outcome, bidder_id)],
                         by = "bidder_id", all.x = T)
payAccPrefix <- paymentAccounts[, .(payAccPrefix = tail(names(textcnt(Acc,
                                                                      n = 15L,
                                                                      split = " ",
                                                                      method = "prefix")), 1)),
                                by = bidder_id]
paymentAccounts <- merge(paymentAccounts, payAccPrefix, by = "bidder_id")
# Ein Acc-Prefix ist extrem häufig
# a3d2de767555655
# 1045
# Welche Bieter mit welchem Outcome haben diesen Prefix
paymentAccounts$commonPaymPrefix <- paymentAccounts$payAccPrefix == "a3d2de767555655"
# confusionMatrix(reference = as.numeric(paymentAccounts$commonPaymPrefix),
#                 data = paymentAccounts$outcome)
# Reference
# Prediction   0   1
# 0 886 995
# 1  53  50
# summary(glm(paymentAccounts$outcome ~ paymentAccounts$commonPaymPrefix,
#             family = "binomial"))
# Nicht signifikant
# Füge die Variable trotzdem vorerst hinzu
paymentAccounts$commonPaymPrefix <- factor(paymentAccounts$commonPaymPrefix,
                                           labels = c("commonPaymPrefix", "uncommonPaymPrefix"))



# Address n-grams
# Verwende Präfixe, häufige Suffixe gibt es nicht
# Die addressen sind alle einzigartig
addresses <- dat[, .(address = unique(address)), by = bidder_id]
addresses <- merge(addresses, train[, .(outcome, bidder_id)],
                   by = "bidder_id", all.x = T)
addressPrefix <- addresses[, .(addressPrefix = tail(names(textcnt(address,
                                                                  n = 10L,
                                                                  split = " ",
                                                                  method = "prefix")), 1)),
                           by = bidder_id]
addresses <- merge(addresses, addressPrefix, by = "bidder_id")
# sort(table(addresses$addressPrefix))
# Ein address-Prefix ist extrem häufig
# a3d2de7675
# 349
# = 17,5%
# Fast alle Adressen, die mit a3 anfangen, werden zu a3d2de7675
# Welche Bieter mit welchem Outcome haben dieses Präfix
addresses$commonAdrPrefix <- addresses$addressPrefix == "a3d2de7675"
# confusionMatrix(reference = as.numeric(addresses$commonAdrPrefix),
#                 data = addresses$outcome)
#               Reference
# Prediction    0    1
#           0 1544  337
#           1   91   12
# summary(glm(factor(addresses$outcome) ~ addresses$commonAdrPrefix,
#             family = "binomial"))
# Nicht signifikant (p = 0.1)
# Füge die Variable trotzdem vorerst hinzu
addresses$commonAdrPrefix <- factor(addresses$commonAdrPrefix,
                                    labels = c("commonAdrPrefix",
                                               "uncommonAdrPrefix"))


# Imputation und letzte Daten zusammenfügen -------------------------------------
# sumDistanceMean ist character
dat3$sumDistanceMean <- as.numeric(dat3$sumDistanceMean)
# Sicherungskopie:
dat4 <- dat3
# Einige Variablen hinzufügen
dat4 <- merge(dat4, paymentAccounts[, .(bidder_id, commonPaymPrefix)],
              by = "bidder_id")
dat4 <- merge(dat4, addresses[, .(bidder_id, commonAdrPrefix)], by = "bidder_id")

# Bag Impute
# Für GBM dat4 mit NAs speichern
dat4na <- dat4
preProc  <- preProcess(dat4[, -which(colnames(dat4) %in% c("bidder_id",
                                                           "typicalMerch",
                                                           "typicalContinent",
                                                           "typicalRegion",
                                                           "country",
                                                           "commonPaymPrefix",
                                                           "commonAdrPrefix")),
                            with = F],
                       method = c("bagImpute"))
dat4 <- predict(preProc, newdata = dat4[, -which(colnames(dat4) %in% c("bidder_id",
                                                                       "typicalMerch",
                                                                       "typicalContinent",
                                                                       "typicalRegion",
                                                                       "country",
                                                                       "commonPaymPrefix",
                                                                       "commonAdrPrefix")),
                                        with = F])
dat4 <- data.table(dat4)
# preProc wird sehr groß: 1400 Mb
rm(preProc); gc()
# Herausgelassene Spalten wieder hinzufügen
dat4 <- cbind(dat4,
              dat3[, .(bidder_id, typicalMerch,
                       typicalContinent, typicalRegion, country,
                       paymentAccounts$commonPaymPrefix,
                       addresses$commonAdrPrefix)])

### Coord hinzufügen
# Dazu EU durch CH und UK durch GB ersetzen, damit das merge funktioniert
dat3[country == "EU"]$country <- "CH"
dat3[country == "UK"]$country <- "GB"
dat3 <- merge(dat3, coord, by = "country", all.x = T, by.x = T)


# Outcome hinzufügen
dat4 <- merge(dat4, train[, .(bidder_id, outcome)], by = "bidder_id", all.x = T)
dat4na <- merge(dat4na, train[, .(bidder_id, outcome)], by = "bidder_id", all.x = T)
# Überflüssige Spalten wieder entfernen. In dat4na behalte ich country
dat4[, country := NULL]
dat4na[, country := NULL]
dim(dat4) # 6614 73, im Trainigsset davon 1984 Zeilen
dim(dat4na) # 6614 73

# Outcome als Faktor
dat4$outcome <- factor(dat4$outcome, levels = c("0","1"), labels = c("human", "bot"))
dat4na$outcome <- factor(dat4na$outcome, levels = c("0","1"), labels = c("human", "bot"))

# Transformationen ----------------------------------------------------------
dat4[, lastBidMean := abs(lastBidMean)]
dat4trans <- dat4
dat4transNa <- dat4na
dat4trans$sumDistanceMean <- log(dat4trans$sumDistanceMean + 1)
dat4trans$distanceMax <- log(dat4trans$distanceMax + 1)
dat4transNa$sumDistanceMean <- log(dat4transNa$sumDistanceMean + 1)
dat4transNa$distanceMax <- log(dat4transNa$distanceMax + 1)

# Für count Daten
countVars <- c("nParticipations", "nBidsMean", "sumBids", "nIPsMean",
               "sumIPs", "nDevicesMean", "maxSeqMean", "nSeqMean", "maxNSeq",
               "maxSeqMax", "nMerch", "nDevicesOverall", "nCountries", "meanNCountByAuc",
               "nUrlPerBid", "countryChangesMax", "countryChangesMean", "ipChangesMax",
               "ipChangesMean", "deviceChangesMean", "deviceChangesMax", "devicesPerBid",
               "IPsPerBid", "countriesPerBid"
               )

momentVars <-  c("nBidsSD", "nBidsRange", "nIPsSD", "nIPsRange", "lastBidSD",
                 "lastBidRange", "nDevicesSD", "nDevicesRange", "meanRankSD",
                 "meanRankRange", "maxSeqSD", "maxSeqRange", "nUrlRange",
                 "nSeqSD", "nSeqRange",
                 "nUrlSD", "ipChangesSD", "ipChangesRange", "nCountriesRange",
                 "countryChangesSD", "countryChangesRange", "bidderIpProbSD",
                 "bidderIpProbRange", "distanceSD", "distanceRange",
                 "timeNormDiffSDSD", "timeNormDiffSDRange", "timeNormDiffRangeSD",
                 "timeNormDiffRangeRange",
                 "lastBidMean" # keine Moment-Var, trotzdem ist die transformation nützlich
                 )

dat4trans[, (countVars) := lapply(.SD, function(x) log(x + (3/8))), .SDcols = countVars]
dat4transNa[, (countVars) := lapply(.SD, function(x) log(x + (3/8))), .SDcols = countVars]
dat4trans[, (momentVars) := lapply(.SD, function(x) log(x + 1)), .SDcols = momentVars]
dat4transNa[, (momentVars) := lapply(.SD, function(x) log(x + 1)), .SDcols = momentVars]

# Evtl. für tree-Modelle NAs als -999
replaceNA = function(DT) {
      for (i in names(DT))
            DT[is.na(get(i)), i := -99999, with = FALSE]
}

dat4_999 <- dat4na
dat4_999[is.na(dat4_999$typicalContinent)]$typicalContinent <- "Asia"
dat4_999[is.na(dat4_999$typicalRegion)]$typicalRegion <- "Southern Asia"
replaceNA(dat4_999)

dat4trans_999 <- dat4transNa
dat4trans_999[is.na(dat4trans_999$typicalContinent)]$typicalContinent <- "Asia"
dat4trans_999[is.na(dat4trans_999$typicalRegion)]$typicalRegion <- "Southern Asia"
replaceNA(dat4trans_999)

# NA factor levels entfernen
# Ein NA factor level gibt es nur in typical region
dat4 <- droplevels(dat4)
dat4na <- droplevels(dat4na)
dat4trans <- droplevels(dat4trans)
dat4transNa <- droplevels(dat4transNa)
dat4_999 <- droplevels(dat4_999)
dat4trans_999 <- droplevels(dat4trans_999)

# Check -----------------------------------------------------------------
dim(dat4) # 6614 73
dim(dat4trans)
dim(dat4_999)
dim(dat4na)
dim(dat4transNa)
dim(dat4trans_999)

sum(is.na(dat4)) - sum(is.na(dat4$outcome))
sum(is.na(dat4trans)) - sum(is.na(dat4$outcome))
sum(is.na(dat4_999)) - sum(is.na(dat4$outcome))
sum(is.na(dat4na)) - sum(is.na(dat4$outcome))
sum(is.na(dat4transNa)) - sum(is.na(dat4$outcome))
sum(is.na(dat4_999)) - sum(is.na(dat4$outcome))

# Speichern ------------------------------------------------------------------
# "Rohe" Daten inkl. NAs und ohne log oder sqrt Transformationen
save(dat4na, file = "Mydata/dat4na.RData")
# NAs per bagImpute aufgefüllt
save(dat4, file = "Mydata/dat4.RData")
# Aufgefüllte Daten da wo es passte per log oder sqrt + 3/8 transformiert
save(dat4trans, file = "Mydata/dat4trans.RData")
# Die Daten ohne Imputation transformiert
save(dat4transNa, file = "Mydata/dat4transNa.RData")
# NAs in den rohen Daten durch -999 ersetzt
save(dat4_999, file = "Mydata/dat4_999.RData")
# NAs in den transformierten Daten durch -999 ersetzt
save(dat4trans_999, file = "Mydata/dat4trans_999.RData")

# Testdata
dat4naTest <- dat4na[is.na(outcome)]
dat4Test <- dat4[is.na(outcome)]
dat4transTest <- dat4trans[is.na(outcome)]
dat4transNaTest <- dat4transNa[is.na(outcome)]
dat4_999Test <- dat4_999[is.na(outcome)]
dat4trans_999Test <- dat4trans_999[is.na(outcome)]
# Save Testdata
save(dat4naTest, file = "Mydata/dat4naTest.RData")
save(dat4Test, file = "Mydata/dat4Test.RData")
save(dat4transNaTest, file = "Mydata/dat4transNaTest.RData")
save(dat4_999Test, file = "Mydata/dat4_999Test.RData")
save(dat4transTest, file = "Mydata/dat4transTest.RData")
save(dat4trans_999Test, file = "Mydata/dat4trans_999Test.RData")

# Traindata
dat4naTrain <- dat4na[!is.na(outcome)]
dat4Train <- dat4[!is.na(outcome)]
dat4transTrain <- dat4trans[!is.na(outcome)]
dat4transNaTrain <- dat4transNa[!is.na(outcome)]
dat4_999Train <- dat4_999[!is.na(outcome)]
dat4trans_999Train <- dat4trans_999[!is.na(outcome)]
# Save Traindata
save(dat4naTrain, file = "Mydata/dat4naTrain.RData")
save(dat4Train, file = "Mydata/dat4Train.RData")
save(dat4transNaTrain, file = "Mydata/dat4transNaTrain.RData")
save(dat4_999Train, file = "Mydata/dat4_999Train.RData")
save(dat4transTrain, file = "Mydata/dat4transTrain.RData")
save(dat4trans_999Train, file = "Mydata/dat4trans_999Train.RData")


# Zeitreihenfeatures der einzelnen Bieter erzeugen ---------------------------
library(anomalousACM)
library(caret)
# tsFeatures <- list()
load("Mydata/tsFeatures.RData")
# ca. 15000 Auktionen
for (a in 8321:length(auctions)){
    if (a %% 10 == 0) print(a)
    if (a %% 20 == 0) {
        save(tsFeatures, file = "Mydata/tsFeatures.RData")
        message("SAVED")
        gc()
    }
    tempAuction <- dat[auction == auctions[a]]
    tempAuction <- tempAuction[order(time)]
    if (nrow(tempAuction) <= 20) {
        message(paste("auction", a, "too short"))
        next}
    # Omit bidders with few bids
    enough <- names(table(tempAuction$bidder_id)[table(tempAuction$bidder_id) > 5])
    tempAuction <- tempAuction[bidder_id %in% enough]
    if (length(unique(tempAuction$bidder_id)) <= 5) {
        message(paste("auction", a, "only 1-5 bidders"))
        next}
    start <- Sys.time()
    # For very long auctions
    if (nrow(tempAuction) > 20000){
        message("long auction, splitting up")
        auctionBidders <- unique(tempAuction$bidder_id)
        auctionBiddersIndices <- createFolds(seq_along(auctionBidders),
                                             k = round(length(auctionBidders)/10))
        for (i in 1:length(auctionBiddersIndices)){
            tempAuction2 <- tempAuction[bidder_id %in% auctionBidders[auctionBiddersIndices[[i]]]]
            aucTs <- sapply(unique(tempAuction2$bidder_id), function(x) num(tempAuction2$bidder_id == x))
            aucTs <- data.table(timeNorm = tempAuction2$timeNorm, aucTs)
            aucTs <- aucTs[, lapply(.SD, sum), by=timeNorm]
            print(dim(aucTs))
            tsFeat <- tsmeasures(aucTs[, 2:ncol(aucTs), with = F]) #, width = mywidth)
            if (i == 1){
                tsFeatures[[a]] <- data.frame(bidder_id = colnames(aucTs)[-1],
                                              auction = auctions[a], tsFeat)
            } else {
                tsFeatures[[a]] <- rbind(tsFeatures[[a]],
                                         data.frame(bidder_id = colnames(aucTs)[-1],
                                                    auction = auctions[a], tsFeat))
            }
            gc()
        }
    } else {
        aucTs <- sapply(unique(tempAuction$bidder_id), function(x) num(tempAuction$bidder_id == x))
        aucTs <- data.table(timeNorm = tempAuction$timeNorm, aucTs)
        aucTs <- aucTs[, lapply(.SD, sum), by=timeNorm]
        print(dim(aucTs))
        tsFeat <- tsmeasures(aucTs[, 2:ncol(aucTs), with = F]) #, width = mywidth)
        print(Sys.time() - start)
        tsFeatures[[a]] <- data.frame(bidder_id = colnames(aucTs)[-1],
                                      auction = auctions[a], tsFeat)
    }
    print(Sys.time() - start)
}
tsFeatures <- do.call(rbind, tsFeatures)
tsFeatures <- data.table(tsFeatures)
save(tsFeatures, file = "Mydata/tsFeatures.RData")

# Zeitreihenplots ------------------------------------------------------------
# Wie sieht die Zeitreihe der Gebote eines Bots aus?
b <- "9655ccc7c0c193f1549475f02c54dce45kjw7"
auctions <- unique(dat[bidder_id %in% b]$auction)
tempdat <- dat[auction %in% auctions]
tempdat$bidderAction <- tempdat[, .(bidderAction = ifelse(bidder_id == b, 1, 0))]
plot(tempdat$bidderAction, type = "l")
# Wie sieht das aus, wenn jemand kein Bot ist, aber viele Gebote abgibt?
h <- train$bidder_id[12] # 888 Gebote
nrow(dat[bidder_id == train$bidder_id[12]])
auctions <- unique(dat[bidder_id %in% h]$auction)
tempdat <- dat[auction %in% auctions]
tempdat$bidderAction <- tempdat[, .(bidderAction = ifelse(bidder_id == h, 1, 0))]
plot(tempdat$bidderAction, type = "l")
# nicht besonders hilfreich

# Aufräumen -------------------------------------------------------------------
rm(bids, dat, preProc, temp3); gc()

# Plots ----------------------------------------------------------------------
ggplot(dat4, aes(x = timeNormDiffMean, y = timeNormDiffMin, color = factor(outcome))) +
      geom_point(size = 4, alpha = 0.4)
ggplot(dat4trans, aes(x = sumDistanceMean, y = distanceMax, color = factor(outcome))) +
      geom_point(size = 4, alpha = 0.4)

# Rtsne Plots -----------------------------------------------------------------
# Dummies für Faktoren erstellen
rtsneDat <- data.frame(dat4trans[, which(!colnames(dat4trans) %in% "outcome"),
                                 with = F])
factors <- which(unlist(lapply(rtsneDat, class)) %in% c("factor"))
dummies <- sapply(factors, function(x){
      model.matrix(~rtsneDat[, x] - 1)
})
dummies <- do.call(cbind, dummies)
colnames(dummies) <- gsub(pattern = "rtsneDat\\[, x\\]", replacement = "",
                          colnames(dummies))
dummies <- dummies[, -(which(colnames(dummies) %in% c("NA", "bot", "human")))]
outcome <- rtsneDat$outcome
rtsneDat <- rtsneDat[, -factors]
rtsneDat <- data.frame(rtsneDat, dummies)
rtsneDat$bidder_id <- NULL

# Es gibt aus irgendeinem Grund jetzt sehr ähnliche Zeilen, die Rtsne als
# Duplikate erkennt. Random noise zu countryChangesSD hinzufügen
duplicates <- which(duplicated(rtsneDat))
rtsneDat$countryChangesSD[duplicates] <- rtsneDat$countryChangesSD[duplicates] +
    rnorm(length(rtsneDat$countryChangesSD[duplicates]), sd = 0.2)

set.seed(1234)
tsne_out_train <- Rtsne(as.matrix(rtsneDat[!is.na(dat4trans$outcome), ]),
                        pca = T, check_duplicates = T,
                        max_iter = 800, perplexity=8, theta=0.5, dims=3,
                        verbose=TRUE)

# Wenn auf die gesamten Daten angepasst:
tsne <- data.frame(bidder_id = dat4trans$bidder_id, tsne = tsne_out_train$Y)
save(tsne, file = "Mydata/tsne.RData")

my_palette = c("red", "blue")
palette(my_palette)
plot(tsne_out_train$Y, col=dat4trans$outcome, #main = i,
     pch=".", cex=4, axes=T)
legend("bottomleft", c("human","bot"),
       lty=c(1,1), lwd=c(5,5), col=my_palette, bty="n", cex = 0.7)
palette("default")

# T-SNE Model --------------------------------------------------------------
modelDat <- tsne_out_train$Y

myTrainControl <- trainControl(method = "repeatedcv", number = 10, repeats = 2,
                               verboseIter = T,
                               savePredictions = T,
                               allowParallel = T,
                               classProbs = TRUE,
                               summaryFunction = twoClassSummary)

set.seed(4321)
start <- Sys.time()
M1 <- train(y = dat4trans$outcome[!is.na(dat4trans$outcome)],
      tuneLength = 1,
      x = modelDat,
      tuneGrid = expand.grid(.mtry = floor(sqrt(ncol(modelDat)))),
      method = "rf",
      metric = "ROC",
      trControl = myTrainControl)
beep(); M1; Sys.time(); Sys.time() - start
# ROC ca. 0.7, natürlich nur mit den 1984 Trainigsbeobachtungen


# Zeitcluster finden ----------------------------------------------------

starttimes <- dat[, .(starttimes = min(time)), by = auction]
endtimes <- dat[, .(endtimes = max(time)), by = auction]
auctionTimes <- merge(starttimes, endtimes, by = "auction")
plot(auctionTimes$starttimes, auctionTimes$endtimes)
# 4 Groups
auctionTimes$group <- NA
for (i in 1:nrow(auctionTimes)){
    temprow <- auctionTimes[i]
    if (temprow$starttimes < 9.66e+15 & temprow$endtimes < 9.66e+15){
        auctionTimes$group[i] <- "A"
    } else {
        if (temprow$starttimes < 9.66e+15 &
            temprow$endtimes > 9.66e+15 &
            temprow$endtimes < 9.72e+15) {
            auctionTimes$group[i] <- "B"
        } else {
            if (temprow$starttimes > 9.66e+15 & temprow$starttimes < 9.72e+15){
                auctionTimes$group[i] <- "C"
            } else {
                if (temprow$starttimes > 9.74e+15) auctionTimes$group[i] <- "D"
            }
        }
    }
}
save(auctionTimes, file = "Mydata/auctionTimes.RData")
# > table(auctionTimes$group, useNA = "always")
# A    B    C    D <NA>
# 2114 3960 2486 6491    0
