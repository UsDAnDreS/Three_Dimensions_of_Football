### JUST DOING A BASIC SUM of EPA by EACH UNIT...
##
##  SUM OF EPA.DIFF from OFFENSE, DEFENSE, ST.COVER, ST.RECEIVE


##    Q: Do we COMBINE "ST.COVER" AND "ST.RECEIVE"?

library(tidyverse)


for (year in 2014:2020){
  
#year <- 2014
load(file=paste0(c("pbp_by_drive_w_EPA_Off_Def_ST_with_FG_", year, ".Robj", sep=""), collapse=""))

pbp_by_drive_full[which(pbp_by_drive_full$drive_result_detailed == "End of Half"), ]$ep_after <- 0
pbp_by_drive_full[which(pbp_by_drive_full$drive_result_detailed == "End of Half"), ]$EPA.diff <- -pbp_by_drive_full[which(pbp_by_drive_full$drive_result_detailed == "End of Half"), ]$ep_before
  
  
  
## For the times when possession switched between teams, assign the NEGATIVE of next drive's "ep_before"
our.ind <- which(!(pbp_by_drive_full$ep_after %in% c(-8,-7,-6,-2,0,3,6,7,8)) & 
                     pbp_by_drive_full$pos_team != lead(pbp_by_drive_full$pos_team) )

pbp_by_drive_full$ep_after[our.ind] <- -pbp_by_drive_full$ep_before[our.ind+1]

## For the times when possession switched between teams, assign the NEGATIVE of next drive's "ep_before"
our.ind <- which(!(pbp_by_drive_full$ep_after %in% c(-8,-7,-6,-2,0,3,6,7,8)) & 
                   pbp_by_drive_full$pos_team == lead(pbp_by_drive_full$pos_team) )

pbp_by_drive_full$ep_after[our.ind] <- pbp_by_drive_full$ep_before[our.ind+1]

table(pbp_by_drive_full$drive_result_detailed[our.ind])


pbp_by_drive_full$EPA.diff <- pbp_by_drive_full$ep_after - pbp_by_drive_full$ep_before

View(pbp_by_drive_full[sort(c(our.ind, our.ind+1)), ] %>%
       select(ep_before, ep_after, EPA.diff,
              home,
              first.play.text.no.pen, first.play.text.with.pen,
              last.play.text.no.pen, last.play.text.with.pen))

save(pbp_by_drive_full,
          file=paste0("pbp_by_drive_w_EPA_Off_Def_ST_FG_ep_before_focused_", year, ".Robj"))

# table(pbp_by_drive_full$drive_result_detailed[pbp_by_drive_full$ep_after == 0])





#####
## FOR "OFF+ST"/"DEF+ST"
#####

#year <- 2014
load(file=paste0(c("pbp_by_drive_UPDATED_w_EPA_Off_Def_", year, ".Robj", sep=""), collapse=""))

pbp_by_drive$side <- ifelse(pbp_by_drive$home == pbp_by_drive$pos_team,
                            "Offense",
                            "Defense")

pbp_by_drive[which(pbp_by_drive$drive_result_detailed == "End of Half"), ]$ep_after <- 0
pbp_by_drive[which(pbp_by_drive$drive_result_detailed == "End of Half"), ]$EPA.diff <- -pbp_by_drive[which(pbp_by_drive$drive_result_detailed == "End of Half"), ]$ep_before


## For the times when possession switched between teams, assign the NEGATIVE of next drive's "ep_before"
our.ind <- which(!(pbp_by_drive$ep_after %in% c(-8,-7,-6,-2,0,3,6,7,8)) & 
                   pbp_by_drive$pos_team != lead(pbp_by_drive$pos_team) )

pbp_by_drive$ep_after[our.ind] <- -pbp_by_drive$ep_before[our.ind+1]

## For the times when possession switched between teams, assign the NEGATIVE of next drive's "ep_before"
our.ind <- which(!(pbp_by_drive$ep_after %in% c(-8,-7,-6,-2,0,3,6,7,8)) & 
                   pbp_by_drive$pos_team == lead(pbp_by_drive$pos_team) )

pbp_by_drive$ep_after[our.ind] <- pbp_by_drive$ep_before[our.ind+1]

table(pbp_by_drive$drive_result_detailed[our.ind])


pbp_by_drive$EPA.diff <- pbp_by_drive$ep_after - pbp_by_drive$ep_before

View(pbp_by_drive[sort(c(our.ind, our.ind+1)), ] %>%
       select(ep_before, ep_after, EPA.diff,
              home,
              first.play.text.no.pen, first.play.text.with.pen,
              last.play.text.no.pen, last.play.text.with.pen))

save(pbp_by_drive,
     file=paste0("pbp_by_drive_UPDATED_Off_Def_ep_before_focused_", year, ".Robj"))



}
