
library(tidyverse)

year <- 2014
# load(file=paste0(c("pbp_by_drive_w_EPA_Off_Def_ST_with_FG_", year, ".Robj", sep=""), collapse=""))
load(file=paste0(c("pbp_by_drive_w_EPA_Off_Def_ST_FG_ep_before_focused_", year, ".Robj", sep=""), collapse=""))



## Make the "game_id" variable (excluding the half)
pbp_by_drive_full$game_id <- str_remove(pbp_by_drive_full$game_id_half, "-1|-2")


# For END-OF-HALF drive results, make "ep_after" NA
pbp_by_drive_full$ep_after[which(str_detect(pbp_by_drive_full$drive_result_detailed, "End of"))] <- NA

# For results RIGHT BEFORE the END OF HALF - also make "ep_after" NA
pbp_by_drive_full$ep_after[which(str_detect(lead(pbp_by_drive_full$drive_result_detailed), "End of"))] <- NA

## REMOVING ALL THE ROWS with EP_AFTER = NA
pbp_by_drive_full <- pbp_by_drive_full %>% filter(!is.na(ep_after))

pbp_by_drive_full$unit <- ifelse(pbp_by_drive_full$side %in% c("Offense", "Defense"),
                                 "Off_Def",
                                 ifelse(# str_detect(pbp_by_drive_full$drive_result_detailed, "Field Goal") & str_detect(pbp_by_drive_full$side, "FG."),
                                   # (str_detect(tolower(pbp_by_drive_full$last.play.text.with.pen), "field goal") | str_detect(pbp_by_drive_full$drive_result_detailed, "Field Goal")) & str_detect(pbp_by_drive_full$side, "FG."),
                                   str_detect(pbp_by_drive_full$side, "FG."),
                                   "FG",
                                   ifelse(str_detect(tolower(pbp_by_drive_full$drive_result_detailed), "kickoff") | str_detect(tolower(pbp_by_drive_full$drive_result_detailed), "on-side kick"),
                                          "KO",
                                          # ifelse(str_detect(pbp_by_drive_full$drive_result_detailed, "Punt") & str_detect(pbp_by_drive_full$side, "ST."),
                                          ifelse(str_detect(pbp_by_drive_full$side, "ST."),
                                                 "Punt",
                                                 "Other"))))

print(table(pbp_by_drive_full$drive_result_detailed[pbp_by_drive_full$unit == "Punt"]))



# View(pbp_by_drive_full[sort(c(our.ind,our.ind-1,our.ind+1)), ] %>% 
#        #filter(unit == "Other") %>%
#        select(ep_before, ep_after, EPA.diff,
#                              first.play.text.no.pen, first.play.text.with.pen,
#                              last.play.text.no.pen, last.play.text.with.pen,
#               first.play.type.no.pen, first.play.type.with.pen,
#               last.play.type.no.pen, last.play.type.with.pen))



# Make adjustments for the "ep_after" depending on whether there was a score
# Initialize it at "ep_after"
pbp_by_drive_full$ep_after_new <- pbp_by_drive_full$ep_after

# Set all the ones followed by NEXT HALF/GAME to "NA"
our.ind <- which(pbp_by_drive_full$game_id_half != lead(pbp_by_drive_full$game_id_half))
pbp_by_drive_full$ep_after_new[our.ind] <- NA



##########
## For each specific "drive type":
##    - Kickoff (ST)
##    - Punt (ST)
##    - Field Goal att (ST)
##    - Offense/Defense
##
## NOTE: GOTTA BE CAREFUL about the SCORING DRIVES/PLAYS...
##      USE THE POSITION FOR THE SUBSEQUENT KICKOFF PLAY for "EP_AFTER" instead of the FULL POINTS
##########



## Kickoff (ST)

# View(pbp_by_drive_full %>% 
#   filter((str_detect(drive_result_detailed, "Kickoff") | str_detect(drive_result_detailed, "On-Side Kick")) & 
#            score_pts_by_text > 0) %>% 
#   select(ep_before, ep_after, EPA.diff,
#          first.play.text.no.pen, first.play.text.with.pen,
#          last.play.text.no.pen, last.play.text.with.pen))

## The scoring plays where possession team DID change (so, the first possessing team was the one that scored)
our.ind.1 <- which((str_detect(pbp_by_drive_full$drive_result_detailed, "Kickoff") | str_detect(pbp_by_drive_full$drive_result_detailed, "On-Side Kick")) & 
                     pbp_by_drive_full$score_pts_by_text != 0 & 
                     pbp_by_drive_full$game_id_half == lead(pbp_by_drive_full$game_id_half) & 
                     pbp_by_drive_full$pos_team != lead(pbp_by_drive_full$pos_team))

pbp_by_drive_full$ep_after_new[our.ind.1] <- -pbp_by_drive_full$ep_before[our.ind.1+1]


## The scoring plays where possession team DID NOT change (so, the first possessing team was the one that ALLOWED a SCORE)
#  AND the NEXT DRIVE WAS NOT "ONE PENALTY" (meaning that it was a REAL DRIVE, rather than STEMMING FROM PREVIOUS UNIT)
our.ind.1 <- which((str_detect(pbp_by_drive_full$drive_result_detailed, "Kickoff") | str_detect(pbp_by_drive_full$drive_result_detailed, "On-Side Kick")) & 
                     pbp_by_drive_full$score_pts_by_text != 0 & 
                     pbp_by_drive_full$game_id_half == lead(pbp_by_drive_full$game_id_half) &
                     pbp_by_drive_full$pos_team == lead(pbp_by_drive_full$pos_team) & 
                     !is.na(lead(pbp_by_drive_full$first.play.text.no.pen)))

print("NUMBER OF POST-ST NON-PENALTIES:")
print(length(our.ind.1))

pbp_by_drive_full$ep_after_new[our.ind.1] <- pbp_by_drive_full$ep_before[our.ind.1+1]



## The scoring plays where possession team DID NOT change (so, the first possessing team was the one that ALLOWED a SCORE)
#  AND the NEXT "DRIVE" WAS !!ONE PENALTY!! (meaning that it was the PREVIOUS POSSESSION when the PENALTY WAS CONDUCTED)
our.ind.1 <- which((str_detect(pbp_by_drive_full$drive_result_detailed, "Kickoff") | str_detect(pbp_by_drive_full$drive_result_detailed, "On-Side Kick")) & 
                     pbp_by_drive_full$score_pts_by_text != 0 & 
                     pbp_by_drive_full$game_id_half == lead(pbp_by_drive_full$game_id_half) &
                     pbp_by_drive_full$pos_team == lead(pbp_by_drive_full$pos_team) 
                   & is.na(lead(pbp_by_drive_full$first.play.text.no.pen))
)

print("NUMBER OF POST-ST PENALTIES:")
print(length(our.ind.1))


## CLEANING UP THE POST-ST PENALTY PLAY...
pbp_by_drive_full$side[our.ind.1+1] <- pbp_by_drive_full$side[our.ind.1]
pbp_by_drive_full$ep_after_new[our.ind.1] <- pbp_by_drive_full$ep_before[our.ind.1+2]
pbp_by_drive_full$ep_after_new[our.ind.1+1] <- NA 




our.ind <- which((str_detect(pbp_by_drive_full$drive_result_detailed, "Kickoff") | str_detect(pbp_by_drive_full$drive_result_detailed, "On-Side Kick")))
our.ind

# our.ind.1 <- which(abs(resid(lm.obj)) >=4)

# View(pbp_by_drive_full[sort(c(our.ind[our.ind.1], our.ind[our.ind.1]+1)) ,  ] %>% 
#        select(ep_before, ep_after_new, EPA.diff,
#               home,
#               first.play.text.no.pen, first.play.text.with.pen,
#               last.play.text.no.pen, last.play.text.with.pen))






##########
## Punt (ST)
##########


## The scoring plays where possession team DID change (so, the first possessing team was the one that scored)
our.ind.1 <- which(str_detect(pbp_by_drive_full$drive_result_detailed, "Punt") & str_detect(pbp_by_drive_full$side, "ST.") & 
                     pbp_by_drive_full$score_pts_by_text != 0 & 
                     pbp_by_drive_full$game_id_half == lead(pbp_by_drive_full$game_id_half) & 
                     pbp_by_drive_full$pos_team != lead(pbp_by_drive_full$pos_team))

pbp_by_drive_full$ep_after_new[our.ind.1] <- -pbp_by_drive_full$ep_before[our.ind.1+1]



## The scoring plays where possession team DID NOT change (so, the first possessing team was the one that ALLOWED a SCORE)
#  AND the NEXT DRIVE WAS NOT "ONE PENALTY" (meaning that it was a REAL DRIVE, rather than STEMMING FROM PREVIOUS UNIT)
our.ind.1 <- which(str_detect(pbp_by_drive_full$drive_result_detailed, "Punt") & str_detect(pbp_by_drive_full$side, "ST.") & 
                     pbp_by_drive_full$score_pts_by_text != 0 & 
                     pbp_by_drive_full$game_id_half == lead(pbp_by_drive_full$game_id_half) &
                     pbp_by_drive_full$pos_team == lead(pbp_by_drive_full$pos_team) & 
                     !is.na(lead(pbp_by_drive_full$first.play.text.no.pen)))

print("NUMBER OF POST-ST NON-PENALTIES:")
print(length(our.ind.1))

pbp_by_drive_full$ep_after_new[our.ind.1] <- -pbp_by_drive_full$ep_before[our.ind.1+1]



## The scoring plays where possession team DID NOT change (so, the first possessing team was the one that ALLOWED a SCORE)
#  AND the NEXT "DRIVE" WAS !!ONE PENALTY!! (meaning that it was the PREVIOUS POSSESSION when the PENALTY WAS CONDUCTED)
our.ind.1 <- which(str_detect(pbp_by_drive_full$drive_result_detailed, "Punt") & str_detect(pbp_by_drive_full$side, "ST.") & 
                     pbp_by_drive_full$score_pts_by_text != 0 & 
                     pbp_by_drive_full$game_id_half == lead(pbp_by_drive_full$game_id_half) &
                     pbp_by_drive_full$pos_team == lead(pbp_by_drive_full$pos_team) 
                   & is.na(lead(pbp_by_drive_full$first.play.text.no.pen))
)

print("NUMBER OF POST-ST PENALTIES:")
print(length(our.ind.1))



#################
## Field Goal (ST)
#################


## The scoring plays where possession team DID change (so, the first possessing team was the one that scored)
our.ind.1 <- which(str_detect(pbp_by_drive_full$drive_result_detailed, "Field Goal") & str_detect(pbp_by_drive_full$side, "FG.") & 
                     pbp_by_drive_full$score_pts_by_text != 0 & 
                     pbp_by_drive_full$game_id_half == lead(pbp_by_drive_full$game_id_half) & 
                     pbp_by_drive_full$pos_team != lead(pbp_by_drive_full$pos_team))

pbp_by_drive_full$ep_after_new[our.ind.1] <- -pbp_by_drive_full$ep_before[our.ind.1+1]


## The scoring plays where possession team DID NOT change (so, the first possessing team was the one that ALLOWED a SCORE)
#  AND the NEXT DRIVE WAS NOT "ONE PENALTY" (meaning that it was a REAL DRIVE, rather than STEMMING FROM PREVIOUS UNIT)
our.ind.1 <- which(str_detect(pbp_by_drive_full$drive_result_detailed, "Field Goal") & str_detect(pbp_by_drive_full$side, "FG.") & 
                     pbp_by_drive_full$score_pts_by_text != 0 & 
                     pbp_by_drive_full$game_id_half == lead(pbp_by_drive_full$game_id_half) &
                     pbp_by_drive_full$pos_team == lead(pbp_by_drive_full$pos_team) & 
                     !is.na(lead(pbp_by_drive_full$first.play.text.no.pen)))

print("NUMBER OF POST-ST NON-PENALTIES:")
print(length(our.ind.1))

pbp_by_drive_full$ep_after_new[our.ind.1] <- -pbp_by_drive_full$ep_before[our.ind.1+1]



## The scoring plays where possession team DID NOT change (so, the first possessing team was the one that ALLOWED a SCORE)
#  AND the NEXT "DRIVE" WAS !!ONE PENALTY!! (meaning that it was the PREVIOUS POSSESSION when the PENALTY WAS CONDUCTED)
our.ind.1 <- which(str_detect(pbp_by_drive_full$drive_result_detailed, "Field Goal") & str_detect(pbp_by_drive_full$side, "FG.") & 
                     pbp_by_drive_full$score_pts_by_text != 0 & 
                     pbp_by_drive_full$game_id_half == lead(pbp_by_drive_full$game_id_half) &
                     pbp_by_drive_full$pos_team == lead(pbp_by_drive_full$pos_team) 
                   & is.na(lead(pbp_by_drive_full$first.play.text.no.pen))
)

print("NUMBER OF POST-ST PENALTIES:")
print(length(our.ind.1))



#############
## Offense/Defense
##   WHAT ABOUT DEFENSIVE TDs??? NO SWITCH IN POS
#############

# View(pbp_by_drive_full %>% 
#        filter(side %in% c("Offense", "Defense")) %>% 
#        select(ep_before, ep_after, EPA.diff,
#               first.play.text.no.pen, first.play.text.with.pen,
#               last.play.text.no.pen, last.play.text.with.pen))


## The scoring plays where possession team DID change (so, the first possessing team was the one that scored)
our.ind.1 <- which(pbp_by_drive_full$side %in% c("Offense", "Defense") & 
                     pbp_by_drive_full$score_pts_by_text != 0 & 
                     pbp_by_drive_full$game_id_half == lead(pbp_by_drive_full$game_id_half) & 
                     pbp_by_drive_full$pos_team != lead(pbp_by_drive_full$pos_team))

pbp_by_drive_full$ep_after_new[our.ind.1] <- -pbp_by_drive_full$ep_before[our.ind.1+1]



## The scoring plays where possession team DID NOT change (so, the first possessing team was the one that ALLOWED a SCORE)
#  AND the NEXT DRIVE WAS NOT "ONE PENALTY" (meaning that it was a REAL DRIVE, rather than STEMMING FROM PREVIOUS UNIT)
our.ind.1 <- which(pbp_by_drive_full$side %in% c("Offense", "Defense") & 
                     pbp_by_drive_full$score_pts_by_text != 0 & 
                     pbp_by_drive_full$game_id_half == lead(pbp_by_drive_full$game_id_half) &
                     pbp_by_drive_full$pos_team == lead(pbp_by_drive_full$pos_team) & 
                     !is.na(lead(pbp_by_drive_full$first.play.text.no.pen)))

print("NUMBER OF POST-ST NON-PENALTIES:")
print(length(our.ind.1))

pbp_by_drive_full$ep_after_new[our.ind.1] <- -pbp_by_drive_full$ep_before[our.ind.1+1]


## The scoring plays where possession team DID NOT change (so, the first possessing team was the one that ALLOWED a SCORE)
#  AND the NEXT "DRIVE" WAS !!ONE PENALTY!! (meaning that it was the PREVIOUS POSSESSION when the PENALTY WAS CONDUCTED)
our.ind.1 <- which(str_detect(pbp_by_drive_full$drive_result_detailed, "Field Goal") & str_detect(pbp_by_drive_full$side, "FG.") & 
                     pbp_by_drive_full$score_pts_by_text != 0 & 
                     pbp_by_drive_full$game_id_half == lead(pbp_by_drive_full$game_id_half) &
                     pbp_by_drive_full$pos_team == lead(pbp_by_drive_full$pos_team) 
                   & is.na(lead(pbp_by_drive_full$first.play.text.no.pen))
)

print("NUMBER OF POST-ST PENALTIES:")
print(length(our.ind.1))



########
###### ADDING LAGS
########




pbp_by_drive_full$ep_before <- ifelse(pbp_by_drive_full$side %in% c("Offense", "ST.Return", "FG.Kick"),
                                      pbp_by_drive_full$ep_before,
                                      -pbp_by_drive_full$ep_before)

pbp_by_drive_full$ep_after <- ifelse(pbp_by_drive_full$side %in% c("Offense", "ST.Return", "FG.Kick"),
                                      pbp_by_drive_full$ep_after,
                                      -pbp_by_drive_full$ep_after)

pbp_by_drive_full$ep_after_new <- ifelse(pbp_by_drive_full$side %in% c("Offense", "ST.Return", "FG.Kick"),
                                         pbp_by_drive_full$ep_after_new,
                                         -pbp_by_drive_full$ep_after_new)


### ADDING THE LAG VARIABLES

pbp_by_drive_full <- pbp_by_drive_full %>%
  group_by(game_id_half) %>%
  mutate(unit=factor(unit),
         lag1_ep = dplyr::lag(ep_before, n=1, default= NA), lag1_unit = dplyr::lag(unit, n=1, default= NA),
         lag2_ep = dplyr::lag(ep_before, n=2, default= NA), lag2_unit = dplyr::lag(unit, n=2, default= NA),
         lag3_ep = dplyr::lag(ep_before, n=3, default= NA), lag3_unit = dplyr::lag(unit, n=3, default= NA),
         lag4_ep = dplyr::lag(ep_before, n=4, default= NA), lag4_unit = dplyr::lag(unit, n=4, default= NA),
         lag5_ep = dplyr::lag(ep_before, n=5, default= NA), lag5_unit = dplyr::lag(unit, n=5, default= NA),) %>%
         select(unit,
                ep_before, 
                lag1_ep, lag2_ep, lag3_ep, lag4_ep, lag5_ep,
                lag1_unit, lag2_unit, lag3_unit, lag4_unit, lag5_unit,
                ep_after_new,  EPA.diff,
                               first.play.text.no.pen, first.play.text.with.pen,
                               last.play.text.no.pen, last.play.text.with.pen)

dim(pbp_by_drive_full)
pbp_by_drive_full

# pbp_by_drive_full$EPA.diff.home <- ifelse(pbp_by_drive_full$side %in% c("Offense", "ST.Return", "FG.Kick"),
#                                           pbp_by_drive_full$EPA.diff,
#                                           -pbp_by_drive_full$EPA.diff)
# 
# pbp_by_drive_full$EP.Kickoffs <- ifelse((str_detect(pbp_by_drive_full$first.play.type.no.pen, "Kickoff") | str_detect(pbp_by_drive_full$first.play.type.with.pen, "Kickoff")),
#                                         ifelse(pbp_by_drive_full$side %in% c("Offense", "ST.Return", "FG.Kick"), 
#                                                pbp_by_drive_full$ep_before,
#                                                -pbp_by_drive_full$ep_before),
#                                         NA)
# 
# 
# pbp_by_drive_full$score_pts_by_text.home <- ifelse(pbp_by_drive_full$side %in% c("Offense", "ST.Return", "FG.Kick"),
#                                                    pbp_by_drive_full$score_pts_by_text,
#                                                    -pbp_by_drive_full$score_pts_by_text)


library(car)

lm.obj <- lm(ep_after_new ~  unit + ep_before + ep_before:unit + 
               lag1_ep + lag2_ep + lag3_ep + lag4_ep + lag5_ep +
               lag1_unit + lag2_unit + lag3_unit + lag4_unit + lag5_unit +
               lag1_ep:lag1_unit + lag2_ep:lag2_unit +lag3_ep:lag3_unit +lag4_ep:lag4_unit +lag5_ep:lag5_unit, #+ s(ep_before, k=4):unit,
               data=pbp_by_drive_full)

vif(lm.obj)

plot(lm.obj, which=1)

summary(lm.obj)
