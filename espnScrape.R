library(data.table)
library(XML)
library(stringr)
library(XLConnect)


suffix <- "espn"


# Download Projections from ESPN-----------------------------

espn_base_url <- paste0("http://games.espn.com/ffl/tools/projections?")
espn_pos <- list(QB=0, RB=2, WR=4, TE=6, DST=16, K=17)
espn_pages <- c("0", "40", "80")
espn_urls <- paste0(espn_base_url, "&slotCategoryId=", rep(espn_pos, each = length(espn_pages)), "&startIndex=", espn_pages)

# Scrape Data-------------------------------

espn <- lapply(espn_urls, function(x) {data.table(readHTMLTable(x, as.data.frame = TRUE, stringsAsFactors = FALSE)$playertable_0)})
espnList <- espn

# Clean Data--------------------------------

qbNames <- rbNames <- wrNames <- teNames <- dstNames <- kNames <- c("rank", "player","passCompAtt","passYds","passTds","passInt","rushAtt","rushYds","rushTds","rec","recYds","recTds","points")

for (i in 1:length(espnList)) {
  if(nrow(espnList[[i]]) > 0){
    
    # Add position to projection
    
    espnList[[i]][,pos := rep(names(espn_pos), each=length(espn_pages))[i]]
    espnList[[i]][,pos := as.factor(pos)]
    
    # Trim Dimensions
    
    espnList[[i]] <- espnList[[i]][2:nrow(espnList[[i]])]
    
    # Add Variable Names
    
    if(unique(espnList[[i]][,pos]) == "QB"){
      setnames(espnList[[i]], c(qbNames, "pos"))
    } else if (unique(espnList[[i]][,pos]) == "RB"){
      setnames(espnList[[i]], c(rbNames, "pos"))
    } else if (unique(espnList[[i]][,pos]) == "WR"){
      setnames(espnList[[i]], c(wrNames, "pos"))
    } else if (unique(espnList[[i]][,pos]) == "TE"){
      setnames(espnList[[i]], c(teNames, "pos"))
    } else if (unique(espnList[[i]][,pos]) == "DST"){
      setnames(espnList[[i]], c(dstNames, "pos"))
    } else if (unique(espnList[[i]][,pos]) == "K"){
      setnames(espnList[[i]], c(kNames, "pos"))
    }
  }
}

# Merge Lists------------------------------

espn_projections <- rbindlist(espnList, use.names=TRUE, fill=TRUE)

# Replace symbols with value of zero----------------------------

espn_projections[which(passCompAtt == "--/--"), passCompAtt := "0/0"]
espn_projections[which(passYds == "--"), passYds := "0"]
espn_projections[which(passTds == "--"), passTds := "0"]
espn_projections[which(passInt == "--"), passInt := "0"]
espn_projections[which(rushAtt == "--"), rushAtt := "0"]
espn_projections[which(rushYds == "--"), rushYds := "0"]
espn_projections[which(rushTds == "--"), rushTds := "0"]
espn_projections[which(rec == "--"), rec := "0"]
espn_projections[which(recYds == "--"), recYds := "0"]
espn_projections[which(recTds == "--"), recTds := "0"]
espn_projections[which(points == "--"), points := "0"]

# Separate pass completions from attempts--------------------------

espn_projections[, passComp := str_sub(string=passCompAtt, end=str_locate(string=passCompAtt, '/')[,1]-1)]
espn_projections[, passAtt := str_sub(string=passCompAtt, start=str_locate(string=passCompAtt, '/')[,1]+1)]

# Convert variables from character strings to numeric-------------------

espn_projections <- data.frame(espn_projections)
numericVal <- c("rank", "passYds", "passTds", "passInt", "rushAtt", "rushYds", "rushTds", "rec", "recYds", "recTds", "points", "passComp", "passAtt")

for (i in 1:length(numericVal)) {
  espn_projections[, numericVal[i]] <- as.numeric(espn_projections[, numericVal[i]])
}

# Player teams----------------------------------------

espn_projections$team <- str_sub(espn_projections$player, start=str_locate(espn_projections$player, ',')[, 1]+2, end=str_locate(espn_projections$player, ',')[, 1]+4)
espn_projections[which(espn_projections$pos == "DST"), "team"] <- str_sub(espn_projections$player[which(espn_projections$pos == "DST")], end=str_locate(espn_projections$player[which(espn_projections$pos == "DST")], " ")[,1]-1)
espn_projections[which(espn_projections$pos == "DST"), "team"] <- degroot::convertTeam(teamName = espn_projections[which(espn_projections$pos == "DST"), "team"])
espn_projections$team <- str_trim(espn_projections$team, side="right")
espn_projections$team <- toupper(espn_projections$team)

# Player names----------------------------------------

espn_projections$name <- str_sub(espn_projections$player, end=str_locate(espn_projections$player, ",")[, 1]-1)
espn_projections[which(espn_projections$pos == "DST"), "name"] <- str_sub(espn_projections$player[which(espn_projections$pos == "DST")], end=str_locate(espn_projections$player[which(espn_projections$pos == "DST")], " ")[,1]-1)
espn_projections$name <- str_replace_all(espn_projections$name, "[*]", "")
espn_projections$name <- str_trim(espn_projections$name, side="right")

# Calculate Overall Rank

espn_projections <- as.data.table(espn_projections)
espn_projections <- espn_projections[order(-points)][,overallRank := 1:.N]

# Calculate Position Rank-----------------------------

espn_projections <- espn_projections[order(-points)][,positionRank := 1:.N, by=list(pos)]

# cleaning Data Frame----------------------------------

espn_projections$sourceName <- suffix
espn_projections <- espn_projections[order(espn_projections$overallRank), ]
espn_projections <- as.data.frame(espn_projections)
drops <- c("player", "passCompAtt", "rank")
espn_projections <- espn_projections[, !(names(espn_projections) %in% drops)]
espn_projections <- espn_projections[c("overallRank", "name", "team", "pos", "positionRank", "passComp", "passAtt", "passYds", "passTds", "passInt", "rushAtt", "rushYds", "rushTds", "rec", 'recYds', "recTds", "points", "sourceName")]

# Save Data-------------------------------------------

save(espn_projections, file = paste(getwd(), "/Data/ESPN-Projections.RData", sep = ""))
write.csv(espn_projections, file = paste(getwd(), "/Data/ESPN-Projections.csv", sep = ""), row.names = FALSE)

save(espn_projections, file = paste(getwd(), "/Data/Historical Projections/ESPN-Projections-2016.Rdata",  sep = ""))
write.csv(espn_projections, file = paste(getwd(), "/Data/Historical Projections/ESPN-Projections-2016.csv", sep = ""), row.names = FALSE)

