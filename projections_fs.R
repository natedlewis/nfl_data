# load helper functions ----

# cleans names
nameMerge <- function(name){
  newName <- toupper(gsub("Sr|Jr|III|[[:punct:]]| ", "", name))
  return(newName)
}

# converts variable types of multiple columns of a dataframe at once
convert.magic <- function(obj, type){
  FUN1 <- switch(type,
                 character = as.character,
                 numeric = as.numeric,
                 factor = as.factor)
  out <- lapply(obj, function(x) FUN1(as.character(x)))
  as.data.frame(out)
}

# load fantasysharks projections ----
projections_fs <- read_csv("http://www.fantasysharks.com/apps/Projections/SeasonCSVProjections.php?l=21")

# clean fantasysharks projections ----

#Player position
projections_fs$pos <- as.factor(projections_fs$Pos)

#Keep only QB, RB, WR, TE
projections_fs <- projections_fs %>% filter(Pos %in% c("QB","RB","WR","TE"))

#Player names
projections_fs$name_fs <- paste(projections_fs$FirstName, projections_fs$LastName, sep=" ")
projections_fs$name <- nameMerge(projections_fs$name_fs)

#Team
projections_fs$team_fs <- as.character(projections_fs$Team)

#Variables
projections_fs$passAtt_fs <- NA
projections_fs$passComp_fs <- projections_fs$PassCmps
projections_fs$passYds_fs <- projections_fs$PassYards
projections_fs$passTds_fs <- projections_fs$PassTDTotal
projections_fs$passInt_fs <- projections_fs$PassInt
projections_fs$rushAtt_fs <- projections_fs$RushAtt
projections_fs$rushYds_fs <- projections_fs$RushYards
projections_fs$rushTds_fs <- projections_fs$RushTDTotal
projections_fs$rec_fs <- projections_fs$Receptions
projections_fs$recYds_fs <- projections_fs$RecYards
projections_fs$recTds_fs <- projections_fs$RecTDTotal
projections_fs$fumbles_fs <- projections_fs$Fumbles
projections_fs$returnTds_fs <- NA
projections_fs$twoPts_fs <- NA
projections_fs$pts_fs <- projections_fs$FantasyPts

#Convert to numeric
projections_fs[,c("passAtt_fs","passComp_fs","passYds_fs","passTds_fs","passInt_fs","rushAtt_fs","rushYds_fs","rushTds_fs","rec_fs","recYds_fs","recTds_fs","returnTds_fs","fumbles_fs","twoPts_fs","pts_fs")] <-
  convert.magic(projections_fs[,c("passAtt_fs","passComp_fs","passYds_fs","passTds_fs","passInt_fs","rushAtt_fs","rushYds_fs","rushTds_fs","rec_fs","recYds_fs","recTds_fs","returnTds_fs","fumbles_fs","twoPts_fs","pts_fs")], "numeric")

#Remove duplicate cases
projections_fs[projections_fs$name %in% projections_fs[duplicated(projections_fs$name),"name"],]

#Calculate overall rank
projections_fs$overallRank_fs <- rank(-projections_fs$pts_fs, ties.method="min")

#Calculate Position Rank
projections_fs <- projections_fs %>% 
  dplyr::group_by(Pos) %>%
  dplyr::mutate(pos_rk = 1:n())

#Order variables in data set
projections_fs <- projections_fs[,c(prefix, paste(varNames, suffix, sep="_"))]

#Order players by overall rank
projections_fs <- projections_fs[order(projections_fs$overallRank_fs),]
row.names(projections_fs) <- 1:dim(projections_fs)[1]