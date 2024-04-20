### JUST DOING A BASIC SUM of EPA by EACH UNIT...
###   !!! FEB 16th: I'M NOT ACCOUNTING FOR THE "BASELINE EP" - BEFORE KICKOFFS....
##         NEED TO ADD THEM TO EPA sums to SEE IF THE "SUM(EPA) = ACTUAL POINTS" CHECKS OUT
##
##  SUM OF EPA.DIFF from OFFENSE, DEFENSE, ST.COVER, ST.RECEIVE


##    Q: Do we COMBINE "ST.COVER" AND "ST.RECEIVE"?

library(tidyverse)

# all.unique.teams <- NULL

load("major_team_names.Robj")


# for (year in 2014:2020){
#   
#  # year <- 2014
#   print(year)
#   
#   
#   
#   load(file=paste0(c("pbp_by_drive_w_EPA_Off_Def_ST_with_FG_", year, ".Robj", sep=""), collapse=""))
#   
#   
#   # all.unique.teams <- c(all.unique.teams, 
#   #                       unique(pbp_by_drive_full$pos_team))
#   
#   }

# Abilene Christian
# The Citadel

# major.team.names <- names(which(table(all.unique.teams) == 7))
# major.team.names <- major.team.names[!major.team.names %in% c("Abilene Christian", "The Citadel")]
# save(major.team.names,
#      file="major_team_names.Robj")



#for (year in 2014:2020){
year <- 2014
# load(file=paste0(c("pbp_by_drive_w_EPA_Off_Def_ST_with_FG_", year, ".Robj", sep=""), collapse=""))
# load(file=paste0(c("pbp_by_drive_w_EPA_Off_Def_ST_FG_ep_before_focused_", year, ".Robj", sep=""), collapse=""))
load(file=paste0(c("pbp_by_drive_UPDATED_Off_Def_ep_before_focused_", year, ".Robj", sep=""), collapse=""))


pbp_by_drive$side <- ifelse(pbp_by_drive$home == pbp_by_drive$pos_team,
                            "Offense",
                            "Defense")

pbp_by_drive[pbp_by_drive$drive_result_detailed == "End of Half", ]$ep_after <- 0
pbp_by_drive[pbp_by_drive$drive_result_detailed == "End of Half", ]$EPA.diff <- -pbp_by_drive[pbp_by_drive$drive_result_detailed == "End of Half", ]$ep_before


pbp_by_drive$EPA.diff.home <- ifelse(pbp_by_drive$side %in% c("Offense"),
                                          pbp_by_drive$EPA.diff,
                                          -pbp_by_drive$EPA.diff)

pbp_by_drive$EP.Kickoffs <- ifelse((str_detect(pbp_by_drive$first.play.type.no.pen, "Kickoff") | str_detect(pbp_by_drive$first.play.type.with.pen, "Kickoff")),
                                        ifelse(pbp_by_drive$side %in% c("Offense"), 
                                               pbp_by_drive$ep_before,
                                               -pbp_by_drive$ep_before),
                                        NA)

pbp_by_drive$score_pts_by_text.home <- ifelse(pbp_by_drive$side %in% c("Offense"),
                                                   pbp_by_drive$score_pts_by_text,
                                                   -pbp_by_drive$score_pts_by_text)


View(pbp_by_drive %>% 
       group_by(game_id_half, side, home) %>%
       summarise(away = ifelse(side %in% c("Offense"), def_pos_team, pos_team)[1],
                 EP.Kickoffs = sum(EP.Kickoffs, na.rm=T),
                 #sum(ep_before[first.play.type.no.pen == "Kickoff" | first.play.type.with.pen == "Kickoff"]),
                 EPA.diff = sum(EPA.diff.home, na.rm=T),
                 score_pts_by_text = sum(score_pts_by_text.home, na.rm=T)) %>%
       group_by(game_id_half) %>%
       summarise(home=home[1],
                 away=away[1],
                 EP.Kickoffs = sum(EP.Kickoffs, na.rm=T),
                 EPA.diff = sum(EPA.diff, na.rm=T),
                 EPA.diff.and.EP.Kickoffs = sum(EPA.diff + EP.Kickoffs, na.rm=T),
                 score_pts_by_text = sum(score_pts_by_text, na.rm=T)) 
     #%>% filter(abs(EPA.diff.and.EP.Kickoffs - score_pts_by_text)>10^{-6})
)
#%>%  summarise(n()))




View(pbp_by_drive %>%
       # filter(side == "Off") %>%
       select(side, ep_before, ep_after, EPA.diff,
              home,
              first.play.text.no.pen, first.play.text.with.pen,
              last.play.text.no.pen, last.play.text.with.pen))



View(pbp_by_drive %>% 
       filter(game_id_half == "400548405-2") %>%
       group_by(game_id_half, side) %>%
       summarise(EPA.diff = sum(EPA.diff.home, na.rm=T),
                 score_pts_by_text = sum(score_pts_by_text.home, na.rm=T)))

View(pbp_by_drive %>% 
       filter(game_id_half == "400548405-2") %>%
       # group_by(game_id_half, side) %>%
       select(ep_before, ep_after, EPA.diff, score_pts,
              home,
              stopped_position,
              first.play.text.no.pen, first.play.text.with.pen,
              last.play.text.no.pen, last.play.text.with.pen))



#}