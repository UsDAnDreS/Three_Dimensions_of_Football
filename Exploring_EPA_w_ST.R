## NOTE: GOTTA BE CAREFUL about the SCORING DRIVES/PLAYS...
##      USE THE POSITION FOR THE SUBSEQUENT KICKOFF PLAY for "EP_AFTER" instead of the FULL POINTS

## MAKING SURE "EP_AFTER" is actually the POSITION AT WHICH THE NEXT UNIT TAKES OVER...
## (Rather than cases when it's =7 after a TD score, or =3..)

## The "ep_before" for 2nd half kickoff drives DEPEND ON THE SCORE DIFFERENCE...
##    - Negative scores associate with negative "ep_before", STRONGLY NEGATIVE leading to STRONGLY NEGATIVE EP_BEFORE's (like -1 or -2)
##    - Positive scores associate with positive "ep_before", STRONGLY POS lead to STRONGLY POS EP_BEFORE'S (like +3)
##
## WHICH I DON'T QUITE UNDERSTAND....
##
##  !!! NFL DATA SET IS FINE IN THAT REGARD !!!!...
# 0.323525853611276 0.814998475712213 
# 1791               256 
# 0.323525853611276 0.522673470857014 0.814998475712213 
# 1795                 2               258 


library(tidyverse)
library(splines)
library(mgcv)
library(fuzzyjoin)


for (year in 2014){


  print(year)
  
  

# load(file=paste0(c("R_Object_Files/pbp_by_drive_w_EPA_Off_Def_ST_with_FG_", year, ".Robj", sep=""), collapse=""))
  load(file=paste0(c("R_Object_Files/pbp_by_drive_w_EPA_Off_Def_ST_FG_ep_before_focused_", year, ".Robj", sep=""), collapse=""))
  
####
####
  
## Make the "game_id" variable (excluding the half)
pbp_by_drive_full$game_id <- str_remove(pbp_by_drive_full$game_id_half, "-1|-2")

## CLEAN UP THE DRIVE #'s
pbp_by_drive_full <- pbp_by_drive_full %>% group_by(game_id) %>% mutate(drive_new_number = row_number())
pbp_by_drive_full$drive_number <- pbp_by_drive_full$drive_new_number

pbp_by_drive_full$game_id_drive <- paste(pbp_by_drive_full$game_id, pbp_by_drive_full$drive_number, sep="-")
pbp_by_drive_full$game_id_drive <- factor(pbp_by_drive_full$game_id_drive, levels = unique(pbp_by_drive_full$game_id_drive))
pbp_by_drive_full$drive_new_number <- NULL

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







our.ind <- which(pbp_by_drive_full$unit == "Punt"  & pbp_by_drive_full$drive_result_detailed == "Safety")
  lag(pbp_by_drive_full$drive_result_detailed) == "Safety"

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


# which((pbp_by_drive_full$side == "Offense" & lead(pbp_by_drive_full$side) == "Offense") |
#         (pbp_by_drive_full$side == "Defense" & lead(pbp_by_drive_full$side) == "Defense")  & 
#         pbp_by_drive_full$game_id_half == lead(pbp_by_drive_full$game_id_half))


View(pbp_by_drive_full %>% select(ep_before, ep_after, ep_after_new, EPA.diff,
                             first.play.text.no.pen, first.play.text.with.pen,
                             last.play.text.no.pen, last.play.text.with.pen))



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




#}




# View(pbp_by_drive_full[c(our.ind.1 + 0:2), ] %>% 
#        #filter((str_detect(drive_result_detailed, "Kickoff") | str_detect(drive_result_detailed, "On-Side Kick")) & 
#       #          score_pts_by_text > 0) %>% 
#        select(ep_before, ep_after, EPA.diff,
#               first.play.text.no.pen, first.play.text.with.pen,
#               last.play.text.no.pen, last.play.text.with.pen))



lm.obj <- lm(ep_after_new ~ ep_before,
             data=pbp_by_drive_full[(str_detect(pbp_by_drive_full$drive_result_detailed, "Kickoff") | str_detect(pbp_by_drive_full$drive_result_detailed, "On-Side Kick")),])
plot(lm.obj, which=1)


## ACCOUNTING FOR WITHIN-HALF DEPENDENCE
library(lme4)
lmer.obj <- lmer(ep_after_new ~ ep_before + (1|game_id),
     data=pbp_by_drive_full[(str_detect(pbp_by_drive_full$drive_result_detailed, "Kickoff") | str_detect(pbp_by_drive_full$drive_result_detailed, "On-Side Kick")),])

print(lm.obj$coefficients)
print(anova(lmer.obj, lm.obj))

# resid(lm.obj)




our.ind <- which((str_detect(pbp_by_drive_full$drive_result_detailed, "Kickoff") | str_detect(pbp_by_drive_full$drive_result_detailed, "On-Side Kick")))
our.ind

our.ind.1 <- which(abs(resid(lm.obj)) >=4)

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


lm.obj <- lm(ep_after_new ~ ep_before,
             data=pbp_by_drive_full[str_detect(pbp_by_drive_full$drive_result_detailed, "Punt") & str_detect(pbp_by_drive_full$side, "ST."), ])
plot(lm.obj, which=1)

lmer.obj <- lmer(ep_after_new ~ ep_before + (1|game_id),
                 data=pbp_by_drive_full[str_detect(pbp_by_drive_full$drive_result_detailed, "Punt") & str_detect(pbp_by_drive_full$side, "ST."), ])

print(lm.obj$coefficients)
print(anova(lmer.obj, lm.obj))

resid(lm.obj)



#################
## Field Goal (ST)
#################

# View(pbp_by_drive_full %>% 
#        filter(str_detect(drive_result_detailed, "Field Goal") & str_detect(side, "FG.")) %>% 
#        select(ep_before, ep_after, EPA.diff,
#               first.play.text.no.pen, first.play.text.with.pen,
#               last.play.text.no.pen, last.play.text.with.pen))


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



lm.obj <- lm(ep_after_new ~ ep_before + I(ep_before^2),
             data=pbp_by_drive_full[str_detect(pbp_by_drive_full$drive_result_detailed, "Field Goal") & str_detect(pbp_by_drive_full$side, "FG."), ])
plot(lm.obj, which=1)

# print(lm.obj$coefficients)

## ACCOUNTING FOR WITHIN-HALF DEPENDENCE
lmer.obj <- lmer(ep_after_new ~ ep_before + I(ep_before^2) + (1|game_id),
                 data=pbp_by_drive_full[str_detect(pbp_by_drive_full$drive_result_detailed, "Field Goal") & str_detect(pbp_by_drive_full$side, "FG."), ])

print(lm.obj$coefficients)
print(anova(lmer.obj, lm.obj))







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




lm.obj <- lm(ep_after_new ~ ep_before, #+ I(ep_before^2),
             data=pbp_by_drive_full[pbp_by_drive_full$side %in% c("Offense", "Defense") , ] 
             # %>% filter(score_pts_by_text == 0)
             )
plot(lm.obj, which=1)
lm.obj

hist(pbp_by_drive_full[pbp_by_drive_full$side %in% c("Offense", "Defense") , ]$ep_after_new)
head(sort(table(pbp_by_drive_full[pbp_by_drive_full$side %in% c("Offense", "Defense") , ]$ep_after_new), decreasing = T))

our.ind <- sample(nrow(pbp_by_drive_full[pbp_by_drive_full$side %in% c("Offense", "Defense") , ]),
                  1000)
plot(resid(lm.obj)[our.ind] ~ predict(lm.obj)[our.ind], ylim=c(1,5))
text(predict(lm.obj)[our.ind], resid(lm.obj)[our.ind], c(1:nrow(pbp_by_drive_full[pbp_by_drive_full$side %in% c("Offense", "Defense") , ]))[our.ind])

pbp_by_drive_full[c(1231,2864, 5817, 12260),] %>% select(ep_after_new, ep_before)

# our.ind <- which(pbp_by_drive_full$side %in% c("Offense", "Defense") 
#                  & pbp_by_drive_full$ep_before == 0 & !is.na(pbp_by_drive_full$ep_after_new))
# sum(pbp_by_drive_full[pbp_by_drive_full$side %in% c("Offense", "Defense"), ]$ep_after_new == 6,
#     na.rm=T)
# head(sort(table(pbp_by_drive_full[pbp_by_drive_full$side %in% c("Offense", "Defense"), ]$ep_before),
#      decreasing = T),10)
# 
# hist(pbp_by_drive_full[pbp_by_drive_full$side %in% c("Offense", "Defense"), ]$ep_after_new)

# View(pbp_by_drive_full[sort(c(our.ind, our.ind+1)), ] %>%
#   #filter(ep_after_new == 0) %>%
#   select(ep_before, ep_after_new, EPA.diff,
#          first.play.text.no.pen, first.play.text.with.pen,
#          last.play.text.no.pen, last.play.text.with.pen))


# ## ACCOUNTING FOR WITHIN-HALF DEPENDENCE
# lmer.obj <- lmer(ep_after_new ~ ep_before + I(ep_before^2) + (1|game_id),
#                  data=pbp_by_drive_full[pbp_by_drive_full$side %in% c("Offense", "Defense") , ])
# 
# print(lm.obj$coefficients)
# print(anova(lmer.obj, lm.obj))




########
## Cleaning up the team names, including the "Non-Major" category for all the non-FBS teams
########

team.names.year <- data.frame(Team=sort(unique(c(pbp_by_drive_full$pos_team, pbp_by_drive_full$def_pos_team))))
CFBSTATS_Team_Names <- data.frame(Team = read.csv("~/Documents/Work/New_College/Research/Play_by_Play_Complementary_Football_Project/CFBSTATS_vs_REFERENCE_Team_Names.csv")$CFBSTATS)
CFBSTATS_Team_Names_Bar_Last <- data.frame(Team = gsub("\\s\\w+$", "", CFBSTATS_Team_Names$Team))



# Matching up with standardized team names from "cfbstats.com" website.
matches.df <- stringdist_join(CFBSTATS_Team_Names_Bar_Last, team.names.year, 
                              by='Team', #match based on team
                              mode='left', #use left join
                              method = "jw", #use jw distance metric
                              max_dist=1, 
                              distance_col='dist') %>%
  group_by(Team.x) %>%
  slice_min(order_by=dist, n=1)


## Fixing up all the identified mismatches, such as:
##       "Ole Miss" for "Mississippi"
##       "NC State" for "North Carolina State" 
##       "North Carolina A&T" matches to "North Carolina" along with "North Carolina Tar"...
##        For 2020, "Connecticut" didn't play, gotta drop its match ("Cincinnati")
##        etc..

FBS.team.names <- matches.df$Team.y
FBS.team.names[matches.df$Team.x == "Mississippi"] <- "Ole Miss"
FBS.team.names[matches.df$Team.x == "North Carolina State"] <- "NC State"
FBS.team.names[matches.df$Team.x == "Miami (Florida)"] <- "Miami"
FBS.team.names <- sort(
  FBS.team.names[!(matches.df$Team.x == "Coastal Carolina" & matches.df$Team.y == "East Carolina") & 
                   FBS.team.names != "Charleston Southern" & 
                   matches.df$Team.x != "North Carolina A&T" & 
                   FBS.team.names != "North Carolina A&T" &
                   !(matches.df$Team.x == "UAB" & matches.df$Team.y == "UT San Antonio") & 
                   !(matches.df$Team.x == "Connecticut" & matches.df$Team.y == "Cincinnati") & 
                   !(matches.df$Team.x == "Idaho" & matches.df$Team.y == "Indiana") & 
                   !(matches.df$Team.x == "New Mexico State" & matches.df$Team.y == "New Mexico") & 
                   !(matches.df$Team.x == "Old Dominion" & matches.df$Team.y == "Wyoming")])

# Liberty only became FBS from 2018 onwards:
if (year <= 2017) FBS.team.names <- FBS.team.names[FBS.team.names != "Liberty"]
# Idaho stopped being FBS from 2018 onwards:
if (year >= 2018) FBS.team.names <- FBS.team.names[FBS.team.names != "Idaho"]


## Creating the non-major category
nonmajor.teams <- team.names.year$Team[!team.names.year$Team %in% FBS.team.names]
pbp_by_drive_full$pos_team <- ifelse(pbp_by_drive_full$pos_team %in% FBS.team.names,
                                pbp_by_drive_full$pos_team,
                                "Non-Major")
pbp_by_drive_full$def_pos_team <- ifelse(pbp_by_drive_full$def_pos_team %in% FBS.team.names,
                                    pbp_by_drive_full$def_pos_team,
                                    "Non-Major")

pbp_by_drive_full$game_id <- as.factor(str_remove(pbp_by_drive_full$game_id_half, "-1|-2"))




######
## CLEANING SOME OF THE VARIABLES
######

pbp_by_drive_full <- pbp_by_drive_full %>% mutate(game_id_half = factor(game_id_half))
pbp_by_drive_full$drive <- as.numeric(str_remove(pbp_by_drive_full$game_id_drive, "^\\d+-"))

pbp_by_drive_full <- pbp_by_drive_full %>% mutate(unit = factor(unit))


#######
### REGULAR MODELS: LM, LMER...
#######

# lm.obj <- lm(ep_after_new ~ ep_before,
#              data=pbp_by_drive_full)
# plot(lm.obj, which=1)
# 
# 
# ## ACCOUNTING FOR WITHIN-HALF DEPENDENCE
# library(lme4)
# lmer.obj <- lmer(ep_after_new ~ ep_before + (1|game_id),
#                  data=pbp_by_drive_full)
# 
# print(lm.obj$coefficients)
# print(anova(lmer.obj, lm.obj))
# 
# # resid(lm.obj)
# 
# 
# 
# predict(lmer.obj)
# plot(lmer.obj, which=1)
# 
# predict(lm.obj)
# plot(lm.obj, which=1)
# 
# 
# gam.obj <- gam(ep_after_new ~  s(ep_before, k=4), #+ s(ep_before, k=4):unit,
#                data=pbp_by_drive_full)
# 
# gam.obj
# summary(gam.obj)
# 
# plot(gam.obj)





####
## GAM with MIXED MODELS
####


#####
## Using RANDOM EFFECTS for BOTH:
##    * the AR1 correlation structure to reflect DEPENDENCE OF CLOSE-TO-EACH-OTHER DRIVES 
##    * random intercepts for game_id_half
##
## gamm performs poorly with binary data, since it uses PQL. It is better to use gam with s(...,bs="re") terms, or gamm4.
#####



gamm.full.obj <- gamm(ep_after_new ~  unit + s(ep_before, k=4, by=unit),
                      random=list(game_id= ~ 1),
                     # random=~ 1+drive | game_id_half,
                      correlation = corAR1(form = ~ drive | game_id_half),
                      data=pbp_by_drive_full %>% 
                        filter(!is.na(ep_after_new) & !is.na(ep_before)))
gamm.full.obj

# View(pbp_by_drive_full[190:200,] %>% 
#   filter(!is.na(ep_after_new) & !is.na(ep_before)) %>%
#     select(ep_before, ep_after_new, score_pts_by_text,
#            drive,
#            drive_result_detailed, 
#            first.play.text.no.pen, first.play.text.with.pen,
#            last.play.text.no.pen, last.play.text.with.pen))


#####
## Using RANDOM EFFECTS ONLY for:
##    * the AR1 correlation structure to reflect DEPENDENCE OF CLOSE-TO-EACH-OTHER DRIVES 
#####

gamm.obj <- gamm(ep_after_new ~  unit + s(ep_before, k=4, by=unit),
                 #random=list(game_id= ~ 1),
                 correlation = corAR1(form = ~ drive | game_id_half),
                 data=pbp_by_drive_full)
gamm.obj
summary(gamm.obj$gam)
summary(gamm.obj$lme)

#corAR1(form = ~ months_since_start_of_timeseries | site)

# plot(gamm.obj$gam,pages=1)
# gam.check(gamm.obj$gam,pch=19,cex=.3)

# vis.gam(gamm.obj$gam)


## The NO RANDOM EFFECTS model (for testing their significance)
gam.obj <- gamm(ep_after_new ~  unit + s(ep_before, k=4, by=unit),
                data=pbp_by_drive_full)


pbp_by_drive_full <- pbp_by_drive_full %>% 
  filter(!is.na(ep_after_new) & !is.na(ep_before) & !is.na(unit))


print(anova(gam.obj$lme, gamm.obj$lme))


}


summary(gam.obj$gam)
resid(gam.obj$gam)

pbp_by_drive_full$posit_margin_gam <- resid(gam.obj$gam)

gam.obj <- gam(ep_after_new ~  unit + s(ep_before, k=4, by=unit), #+ s(ep_before, k=4):unit,
               data=pbp_by_drive_full %>%
                 mutate(unit = factor(unit)))


gam.obj

gam.obj$model
model.matrix(gam.obj)

View(data.frame(model.matrix(gam.obj), gam.obj$model[,3]))

summary(gam.obj)

plot(gam.obj)


## Offensive side of the ball
pbp_by_drive_full %>%
  group_by(pos_team, unit) %>%
  summarise(mean.posit.margin = mean(posit_margin_gam)) %>%
  arrange(desc(mean.posit.margin))

## Defensive side of the ball
pbp_by_drive_full %>%
  group_by(def_pos_team, unit) %>%
  summarise(mean.posit.margin = mean(-posit_margin_gam)) %>%
  arrange(desc(mean.posit.margin))


####
## GETTING RANKINGS by EPA
####

View(pbp_by_drive)

## OFFENSE (per drive)
pbp_by_drive_full %>%
  group_by(pos_team, unit) %>%
  summarise(EPA = mean(EPA.diff),
            n.drives = n(),
            n.halves = length(unique(game_id_half))) %>%
  arrange(desc(EPA))


## DEFENSE (per drive)

pbp_by_drive_full %>%
  group_by(def_pos_team, unit) %>%
  summarise(EPA = mean(-EPA.diff),
            n.drives = n(),
            n.halves = length(unique(game_id_half))) %>%
  arrange(desc(EPA))


####
## GETTING RANKINGS by POINTS SCORED/ALLOWED
####

pbp_by_drive_full %>%
  group_by(pos_team, unit) %>%
  summarise(Pts = mean(score_pts_by_text),
            n.drives = n(),
            n.halves = length(unique(game_id_half))) %>%
  arrange(desc(Pts))


pbp_by_drive_full %>%
  group_by(def_pos_team, unit) %>%
  summarise(Pts = mean(score_pts_by_text),
            n.drives = n(),
            n.halves = length(unique(game_id_half))) %>%
  arrange(Pts)


####
## GETTING RANKINGS by POINTS SCORED/ALLOWED,
##    ADJUSTED FOR THE COMPLEMENTARY EPA... ONLY LOOKING AT THE PRECEDING UNITS
##  
## RESIDUAL = True - Predicted
##  1. If preceding unit is FROM YOUR OWN TEAM => subtract the residual (positive => they overperformed expectation, negative - vice versa)
##  2. If it's from THE OPPONENT => ALSO subtract the residual (positive => your unit allowed LESS than was expected)
####

## Create a "lag" variable
# pbp_by_drive_full$lag_posit_margin <- ifelse(lag(pbp_by_drive_full$game_id_half) == pbp_by_drive_full$game_id_half,
#                                              ifelse(lag(pbp_by_drive_full$pos_team) == pbp_by_drive_full$pos_team,
#                                                     lag(resid(gam.obj)),
#                                                     -lag(resid(gam.obj))),
#                                              NA)

pbp_by_drive_full$lag_posit_margin <- ifelse(lag(pbp_by_drive_full$game_id_half) == pbp_by_drive_full$game_id_half,
                                             lag(resid(gam.obj)),
                                             NA)

## OFFENSE
View(pbp_by_drive_full %>%
  group_by(pos_team, unit) %>%
  summarise(Pts = mean(score_pts_by_text),
            Adj.Pts = mean(score_pts_by_text) - mean(lag_posit_margin),
            Avg.Lag_Posit_Margin = mean(lag_posit_margin),
            n.drives = n(),
            n.halves = length(unique(game_id_half))) %>%
  arrange(desc(Adj.Pts)))

## DEFENSE
View(pbp_by_drive_full %>%
       group_by(def_pos_team, unit) %>%
       summarise(Pts = mean(score_pts_by_text),
                 Adj.Pts = mean(score_pts_by_text)+  mean(lag_posit_margin),
                 Avg.Lag_Posit_Margin = mean(lag_posit_margin),
                 n.drives = n(),
                 n.halves = length(unique(game_id_half))) %>%
       arrange(Adj.Pts))


#######
## Teams playing best TOTAL COMPLEMENTARY FOOTBALL
## (SUM OF "OVER EXPECTED" for BOTH OFFENSE & DEFENSE)
#######


pbp_by_drive_full %>%
  group_by(pos_team) %>%
  summarise(mean.posit.margin.off = mean(posit_margin_gam)) %>% left_join(
    pbp_by_drive_full %>%
      group_by(def_pos_team) %>%
      summarise(mean.posit.margin.def = mean(-posit_margin_gam)),
    by = c("pos_team" = "def_pos_team")) %>%
  mutate(total.mean.posit.margin = mean.posit.margin.off + mean.posit.margin.def) %>%
  arrange(desc(total.mean.posit.margin))


## WORST complementary teams

pbp_by_drive_full %>%
  group_by(pos_team) %>%
  summarise(mean.posit.margin.off = mean(posit_margin_gam)) %>% left_join(
    pbp_by_drive_full %>%
      group_by(def_pos_team) %>%
      summarise(mean.posit.margin.def = mean(-posit_margin_gam)),
    by = c("pos_team" = "def_pos_team")) %>%
  mutate(total.mean.posit.margin = mean.posit.margin.off + mean.posit.margin.def) %>%
  arrange(total.mean.posit.margin)







View(pbp_by_drive_full %>%
       # filter(def_pos_team == "Hawai'i", unit == "FG") %>%
  #pbp_by_drive_full[our.ind.1,] %>%
       # filter(str_detect(drive_result_detailed, "Punt") & str_detect(side, "ST.")) %>%
       select(pos_team, def_pos_team, unit, lag_posit_margin, drive_result_detailed,
              ep_before, ep_after, ep_after_new, EPA.diff, score_pts,
              first.play.text.no.pen, first.play.text.with.pen,
              last.play.text.no.pen, last.play.text.with.pen))


pbp_by_drive_full$score_pts_by_text
