###  OCT 21st, 2023: 
##   !! HUGE PROBLEMS with "END OF" PLAY TEXT... the PTS SCORED gets FLIPPED ON THOSE... !!

# https://stackoverflow.com/questions/36814303/r-markovchain-package-fitting-the-markov-chain-based-on-the-states-sequence-ma

library(tidyverse)
library(markovchain)


for (year in 2014:2020){
  
#year <- 2014

print(year)

# load(paste0("pbp_",year,"_on_campus_data.Robj"))
load(paste0("/home/andrey/Documents/Work/New_College/Research/Play_by_Play_Complementary_Football_Project/pbp_", year, "_on_campus_data.Robj"))
  

dim(pbp)

table(pbp$year)

length(unique(pbp$game_id))



###############################################
###############################################
###  1. Super early generic preprocessing:  ###
###############################################
###############################################


### DROPPING ALL THE OVERTIME STUFF
pbp <- pbp %>% filter(drive_start_period <=4 & period <= 4)
dim(pbp)


#### PENALTIES: 
##      1. Making sure that "no play" penalties are going under "play_type = Penalty", nothing else.
pbp$play_type[which(pbp$penalty_no_play)] <- "Penalty"
##      2. Disposing of crazy typos where one penalty is >=100 in absolute value, just setting them to NA
# print("Number of TYPOS in PENALTY YARDAGES (to be set to NA):")
our.ind <- which(abs(pbp$yds_penalty) >= 100)
#print(length(our.ind))

if (length(our.ind) > 0){
  pbp$yds_penalty[our.ind] <- NA
}


### FAULTY YARDS GAINED:
#print("Number of TYPOS in GAINED YARDAGES (to be set to NA):")
our.ind <- which(abs(pbp$yards_gained) > 100)
#print(length(our.ind))

if (length(our.ind) > 0){
  pbp$yards_gained[our.ind] <- NA
}


## CREATING A "GAME.HALF" ID VARIABLE, to break data down BY HALVES rather than GAMES
pbp$game_id_half <- paste(pbp$game_id, pbp$half, sep="-")
pbp$game_id_half <- factor(pbp$game_id_half, levels = unique(pbp$game_id_half))

## CREATING a "GAME.DRIVE" ID VARIABLE, to break data down by DRIVES for finer analysis
pbp$game_id_drive <- paste(pbp$game_id, pbp$drive_number, sep="-")
pbp$game_id_drive <- factor(pbp$game_id_drive, levels = unique(pbp$game_id_drive))




### DROPPING ALL TIMEOUTS: 
##     Many TIMEOUTS screw with SCORE, POSSESSION, and YARDAGES
##     TIMEOUTS CAN SCREW WITH DRIVES AS WELL:
##            E.g. after a "Punt Return Touchdown" or "Fumble Return Touchdown", if one takes a time out, it counts as a "drive", with SCREWED UP SCORE & YARDAGES....
##         !!! HENCE THE "DOUBLE-SKIPS" IN DRIVE NUMBERS !!!
##
##     !! THE ONLY VALUABLE COMPONENT TO TIMEOUTS IS REALLY THE CHANGES IN POSSESSION !!!
##       (change_of_pos_team)
##      BECAUSE THEY RUIN THOSE FOR OTHER OBSERVATIONS AS WELL, SO WE NEED TO FIX THAT, SO...
##      ...
##      IDEA: when deleting "Timeout", make "change_of_pos_team" for the PRECEDING OBSERVATION =
##          ("change of pos team" for preceding play + "change of pos team" for Timeout) MOD 2
##      So if 
##           * they're both 0 or 1, then it should be 0 (when they're both 1, the possession - DUE TO SCREWERY - switched back-and-forth, arriving back at 0..)
##           * If one of them is 1, then there was 1 switch in possession, so should just remain 1.


# Where does yardage change after a Timeout: 
# relev.ind <- which(pbp$play_type == "Timeout" & pbp$yards_to_goal !=  pbp$yards_to_goal_end)
# relev.ind
#
# bad.ind <- which(pbp$play_type == "Timeout" & pbp$change_of_pos_team == 1)
#
# View(pbp[sort(c(bad.ind-1,bad.ind, bad.ind+1)), ] %>%
#        # pbp %>% filter(game_id_drive %in% c("400548425-17", "400548425-18")) %>%
#        select(game_id_drive, pos_team_score, def_pos_team_score, pos_team, def_pos_team, change_of_pos_team, yards_gained, yards_to_goal, yards_to_goal_end, penalty_flag, 
#               drive_result_detailed, play_type, play_text))

## Doing the CHANGE OF "CHANGE_OF_POS_TEAM" VALUES:
##   1. Identifying plays that preceded the timeouts
##   2. Changing their "change_of_pos_team" to be MOD 2 of its own and timeout's "change_of_pos" values:

to.preceding.plays <- which(lead(pbp$play_type) == "Timeout")
if (length(to.preceding.plays) > 0){
  pbp[to.preceding.plays,]$change_of_pos_team <- (pbp[to.preceding.plays,]$change_of_pos_team + pbp[to.preceding.plays+1,]$change_of_pos_team) %% 2
}



##   NOW THAT WE GOT WHAT WE WANTED FROM TIMEOUTS => DROP THOSE OBSERVATIONS.
dim(pbp)
pbp <- pbp %>% filter(play_type != "Timeout")
dim(pbp)




## Dropping "End of 1st Quarter" and "End of 3rd Quarter" stuff - these are pretty much pointless 
## (unlike end of Half/Game, where there's no continuity into the next play)
## => THESE SEEM FINE TO DROP, as THEY DON'T APPEAR TO AFFECT YARDAGES, actually MOSTLY KILLING THE CONTINUITY
##

dim(pbp)
pbp <- pbp %>% filter(!play_text %in% c("End of 1st Quarter", "End of 3rd Quarter"))
dim(pbp)

## NOTE: In some years, play_type "End Period" is used solely of "End of 1st/3rd Quarters"..
## but in OTHERS, it's also sometimes used to denote "End of 4th Quarter"
# pbp %>% filter(play_type == "End Period") %>% select(play_text)
# pbp %>% filter(play_text == "End of 4th Quarter") %>% select(drive_result_detailed) %>% table()
#
# SO FOR "End of 4th Quarter" play texts, we need to RENAME THE "play_type" for "End Period" to "End of Game"
end.of.4th.end.of.period.ind <- which(pbp$play_type == "End Period" & pbp$play_text == "End of 4th Quarter")
if (length(end.of.4th.end.of.period.ind) > 0) pbp[which(pbp$play_type == "End Period" & pbp$play_text == "End of 4th Quarter"), ]$play_type <- "End of Game"


## NO CASES OF DIFFERING YARDS_TO_GOAL & YARDS_TO_GOAL_END for the "End Period" PLAYS
# relev.ind <- which(pbp$play_text %in% c("End of 1st Quarter", "End of 3rd Quarter") 
#                   # & pbp$yards_to_goal !=  pbp$yards_to_goal_end
#                    )
# relev.ind
# 
# View( pbp[sort(c(relev.ind-1, relev.ind, relev.ind + 1)), ] %>% 
#   #pbp %>%  filter(game_id_drive %in% c("400547646-26", "400547646-27")) %>%
#     select(game_id_drive, pos_team, def_pos_team, drive_number, change_of_pos_team, drive_result_detailed, play_type, play_text,
#            yards_to_goal, yards_to_goal_end,
#            pos_team_score, def_pos_team_score, clock.minutes, clock.seconds))


### MOREOVER, TO AVOID TEMPTATION, LET'S DROP ALL THE "LEAD/LAG" VARIABLE NAMES...
##  Because, once we have started dropping out "in-between" plays, like "Timeouts"/"End of 1st/3rd Quarter"),
##  many of those could be deceiving.
##  INSTEAD WE SHOULD JUST RELY ON R's "lag()" and "lead()" FUNCTIONS

lead.lag.colnames <- colnames(pbp)[str_detect(colnames(pbp), "^lead|^lag")]
dim(pbp)
pbp[, lead.lag.colnames] <- NULL
dim(pbp)



cat("\n")
cat("\n")
print("DATA SET SIZE ORIGINALLY")
print(dim(pbp))
print(length(unique(pbp$game_id_half)))
cat("\n")
cat("\n")




## Defining a "seconds elapsed" variable to contain the seconds elapsed since the start of the game till the end of this play
pbp$SecondsElapsed <- (pbp$period-1)*900 + (14-pbp$clock.minutes)*60 + (60-pbp$clock.seconds)
# summary((14-pbp$clock.minutes)*60 + (60-pbp$clock.seconds))
# summary(pbp$SecondsElapsed)
# View(pbp %>% select(drive_id, play_text, play_type, 
#                     offense_score, defense_score, pos_team, def_pos_team,
#                     period,  TimeSecsRem, TimeSecsRem_end, SecondsElapsed, clock.minutes, clock.seconds, 
#                     drive_result_detailed) %>%
#        head(6))


## Removing the pointless "team_score", which actually messes with some duplicate removal
pbp$team_score <- NULL

## Removing the pointless "drive_result" fields (all but "drive_result_detailed")
pbp$drive_result <- pbp$drive_result2 <- pbp$drive_result_detailed_flag <- NULL


## For the useful "drive_result_detailed"

## "End Game" is about as good as "End Half" in regards to drive result, so make it all consistently "End Half"

if (length(which(pbp$drive_result_detailed == "End Game")) > 0)
  pbp$drive_result_detailed[which(pbp$drive_result_detailed == "End Game")] <- "End of Half"

## Also changing "End Half" into "End of Half", to match up the drive_result_detailed names with the play_type name
if (length(which(pbp$drive_result_detailed == "End Half")) > 0)
  pbp$drive_result_detailed[which(pbp$drive_result_detailed == "End Half")] <- "End of Half"

# table(pbp$play_type)
# pbp %>% filter(play_type == "End of Half") %>% select(play_text) %>% table()
# pbp %>% filter(play_type == "End of Game") %>% select(play_text) %>% table()
#
## !!! KEEP IN MIND - HANDFUL OF THE GAMES END UP NOT HAVING AN "OFFICIAL" 'END OF 2nd quarter', 'End of 4th quarter' PLAY.. !!!


## "Fumble Recovery (Opponent) Touchdown" and "Fumble Return Touchdown" seemingly have no tangible difference..
## some are improperly coded within a drive, but that doesn't change based on those namings.. "Fumble Return Touchdown"
## (both play_type and drive_result_detailed)

if (length(which(pbp$drive_result_detailed == "Fumble Recovery (Opponent) Touchdown")) > 0)
  pbp$drive_result_detailed[which(pbp$drive_result_detailed == "Fumble Recovery (Opponent) Touchdown")] <- "Fumble Return Touchdown"

if (length(which(pbp$play_type == "Fumble Recovery (Opponent) Touchdown")) > 0)
  pbp$play_type[which(pbp$play_type == "Fumble Recovery (Opponent) Touchdown")] <- "Fumble Return Touchdown"

if (length(which(pbp$play_type == "Kickoff Return (Offense)")) > 0)
  pbp$play_type[which(pbp$play_type == "Kickoff Return (Offense)")] <- "Kickoff"


## Matching up the drive_result names with play_type names (for consistency's sake)
pbp$drive_result_detailed[which(pbp$drive_result_detailed == "Fumble")] <- "Fumble Recovery (Opponent)"

## There's a "Pass Completion" and "Pass Reception" play types, with the LATTER being much more prevalent, so replace with it

if (length(which(pbp$play_type == "Pass Completion")) > 0) 
  pbp$play_type[which(pbp$play_type == "Pass Completion")] <- "Pass Reception"


## DISPOSING OF DUPLICATES
dim(pbp)
pbp <- pbp[!duplicated(pbp), ]
dim(pbp)



## Throwing out halves where DRIVE NUMBERS ARE OUT OF ORDER 
## (there's cases where a touchdown from previous drive somehow gets delayed until after the kickoff of consecutive drive)
## 
##  Athough DRIVE_NUM might be smoother than DRIVE_NUMBER/DRIVE_ID
##      (e.g. 1) it has NO NEGATIVE CONSECUTIVE DIFFERENCES, going CONSECUTIVELY; 
##            2) .. plus it accounts for 1-PLAY POSSESSIONS, like a KICKOFF FUMBLE),
##   It has SEVERAL ISSUES such as:
##              0) Not corresponding with distinct "$drive_id" numbers (while "drive_number" does)
##                      length(unique(pbp$drive_id))
##                      sum(!duplicated(pbp[, c("game_id", "drive_num")]))
##                      length(unique(pbp$drive_id))
##                      sum(!duplicated(pbp[, c("game_id", "drive_number")]))
##
##              1) play ordering within the drive 
##                      (e.g. some drives have KICKOFFS put at the end of the drive.. 
##                      and play numbering doesn't help to diagnose this... at all),
##                      leading to drive number changes within the drive (feels like "Kickoff" triggers drive_num change)
##                      (see game_id == 400547651, the drive ~16-17, towards the end of the first half.)
##
##              2) Issues with SKIPPING KICKOFF after the PREVIOUS DRIVE CLEARLY ENDED...
##                   (see game_id == 400547665, drive_num == 1)
##


## If one wants to do "drive_number"
## Checking all the cases where higher drive # occurs before a lower one (2nd - 1st < 0);
## And also if there are "jumps" in drive numbers (e.g. going up 2 spots) - those seem to be OK, but still something to keep an eye on later
switchy.drive.no <- pbp %>% 
  group_by(game_id_half) %>%
  summarize(ord.drives = any(diff(drive_number) < 0)) %>%
  # summarize(ord.drives = any(diff(drive_number) < 0 | diff(drive_number) > 1, na.rm=T)) %>%
  filter(ord.drives) %>%
  select(game_id_half)


switchy.drive.no$game_id_half
#
# View(pbp %>% filter(game_id_half == switchy.drive.no$game_id_half[2]) %>%
#        select(#yds_kickoff, yds_kickoff_return, yards_gained,
#          yards_to_goal, pos_team_score, def_pos_team_score, drive_number, drive_play_number, play_type, play_text))
dim(pbp)
pbp <- pbp %>% filter(!(game_id_half %in% switchy.drive.no$game_id_half))
dim(pbp)

# View(pbp %>% filter(game_id_half == switchy.drive.no$game_id_half[9]) %>%
# View(pbp %>% filter(game_id_half == "400547905-1") %>%
#        select(pos_team, def_pos_team, drive_number, change_of_pos_team, drive_result_detailed, play_type, play_text,
#               yards_to_goal, yards_to_goal_end,
#               pos_team_score, def_pos_team_score, clock.minutes, clock.seconds))


## What about PLAY numbers?
##    None of these are helpful in determining the true order of plays within a game..
##    Just see game_id == 400547651, the drive ~16-17, towards the end of the first half.
##    Kickoff shows up as the last play of the drive, even though there clearly were plays after it 
##    (just check yards_to_goal, yards_to_goal_end)



## Throwing out GAMES where HALVES are out of order
## (there's a game where "End of 2nd quarter" comes deep into the 2nd half...)
##   => throw the ENTIRE GAME OUT.

switchy.halves <- pbp %>% 
  group_by(game_id) %>%
  summarize(ord.halves = any(diff(half) < 0)) %>%
  # summarize(ord.drives = any(diff(drive_number) < 0 | diff(drive_number) > 1, na.rm=T)) %>%
  filter(ord.halves) %>%
  select(game_id) %>% .[[1]]
switchy.halves

#print("# of games with switchy halves, TO BE DROPPED:")
#print(length(switchy.halves))

## Throwing these game out
dim(pbp)
pbp <- pbp %>% filter(!(game_id %in% switchy.halves))
dim(pbp)


## ALSO there are games where: HALF SWITCHED, but NOT THE DRIVE NUMBER
##    Retain only those that are PENALTIES and have "period = 3", changing the drive # to the next one, retaining half as is.
##    DROP OTHER GAMES ("unresolvable") ENTIRELY

post.eof.plays.issues.resolvable <- which((pbp$half != lag(pbp$half)) & (pbp$drive_number == lag(pbp$drive_number) & pbp$game_id == lag(pbp$game_id) & 
                                                           pbp$play_type == "Penalty" & pbp$period == 3))
#print("# games with half switchover issues, resolved:")
#print(length(post.eof.plays.issues.resolvable))

if (length(post.eof.plays.issues.resolvable) > 0){
  pbp$drive_number[post.eof.plays.issues.resolvable] <- pbp$drive_number[post.eof.plays.issues.resolvable+1]
}

pbp$game_id_drive <- paste(pbp$game_id, pbp$drive_number, sep="-")
pbp$game_id_drive <- factor(pbp$game_id_drive, levels = unique(pbp$game_id_drive))


post.eof.plays.issues.unresolvable <- which((pbp$half != lag(pbp$half)) & (pbp$drive_number == lag(pbp$drive_number)) & pbp$game_id == lag(pbp$game_id))
#print("# games with half switchover issues, UNRESOLVABLE (TO BE DROPPED):")
#print(length(post.eof.plays.issues.unresolvable))

## Throwing these game out
if (length(post.eof.plays.issues.unresolvable) > 0){
    bad.half.switch.games <- unique(pbp$game_id[post.eof.plays.issues.unresolvable])
    dim(pbp)
    pbp <- pbp %>% filter(!(game_id %in% bad.half.switch.games))
    dim(pbp)
}





#####################################################
#####################################################
###  2. Dealing with DATA ENTRY INCONSISTENCIES.  ###
#####################################################
#####################################################

## ISSUES:
##    0p1. Some KICKOFF RETURN TDs are MISTAKENLY just called "Kickoff"
##    0p2. Kickoffs are at times NOT the first play of the drive, removing "drive_num".
##    1. Some things (drive_ids, clock,..) are out of order
##    2. Sometimes clock DOESN'T MOVE within a possession.
##    3. Some repeats..

####
##    0p1. Some KICKOFF RETURN TDs are MISTAKENLY just called "Kickoff"
###         Their indicators are words "kickoff return" plus SCORING IS AT "TRUE" & CHANGE_OF_POS is AT 1
##          So just CHANGE THOSE TO "Kickoff"
ko.return.td.mistaken <- which(pbp$play_type == "Kickoff" & pbp$change_of_pos_team == 1 & pbp$scoring &
       str_detect(tolower(pbp$play_text), "kickoff return") & !pbp$penalty_flag)

if (length(ko.return.td.mistaken) > 0)
  pbp[ko.return.td.mistaken, ]$play_type <- "Kickoff Return Touchdown"


########
## 0p2. Checking the games where KICKOFFS are ANYWHERE but at the FIRST PLAY OF THE DRIVE, 
##    using DRIVE_NUMBER as the DRIVE INDICATOR
##    (DRIVE_NUM seems to be AUTOMATICALLY SWITCHING whenever it sees "Kickoff".. even though the "Kickoff" play is at times ERRONEOUSLY MISPLACED within the drive..)
#########


### Obtaining drives with at least 1 kickoff

kickoff.drive.id <- pbp %>% group_by(game_id_drive) %>% summarise(koff.num=sum(str_detect(play_type, "Kickoff")),
                                                                  koff.play.index=ifelse(length(which(str_detect(play_type, "Kickoff"))) == 0, 0, which(str_detect(play_type, "Kickoff"))))
# %>%  select(game_id_drive) %>% .[[1]]
kickoff.drive.id
table(kickoff.drive.id$koff.num)

length(kickoff.drive.id[[1]])
length(unique(pbp$game_id_drive))


### b. For TWO-kickoff (or even more) drives:
##     1. MOST SEEM TO START WITH A KICKOFF TD RETURN, WITHOUT FLIPPING OVER THE DRIVE NUMBER !!!
##        SO, we'll do the following:
##            For all such "first play Kickoff Return TD" (such that its drive = next obs drive, and != previous obs drive, which shows that it's the first play of its own drive)
##            Subtract a -0.5 from its drive number, so that its below the subsequent plays, and still above the previous drive
##     2. Kickoff safeties: same there, subtract -0.5

koff.id.two <- kickoff.drive.id %>% filter(koff.num > 1) %>% select(game_id_drive) %>% .[[1]]

ind.play.type <- which(pbp$game_id_drive %in% koff.id.two & pbp$play_type == "Kickoff Return Touchdown" & (pbp$game_id_drive == lead(pbp$game_id_drive)) & (pbp$game_id_drive != lag(pbp$game_id_drive)))
ind.play.type
if (length(ind.play.type) > 0)
  pbp[ind.play.type, ]$drive_number <- pbp[ind.play.type, ]$drive_number - 0.5

pbp$game_id_drive <- paste(pbp$game_id, pbp$drive_number, sep="-")
pbp$game_id_drive <- factor(pbp$game_id_drive, levels = unique(pbp$game_id_drive))

ind.play.type <- which(pbp$game_id_drive %in% koff.id.two & pbp$play_type == "Kickoff (Safety)" & (pbp$game_id_drive == lead(pbp$game_id_drive)) & (pbp$game_id_drive != lag(pbp$game_id_drive)))
ind.play.type
if (length(ind.play.type) > 0)
  pbp[ind.play.type, ]$drive_number <- pbp[ind.play.type, ]$drive_number - 0.5

pbp$game_id_drive <- paste(pbp$game_id, pbp$drive_number, sep="-")
pbp$game_id_drive <- factor(pbp$game_id_drive, levels = unique(pbp$game_id_drive))


# View(pbp %>% filter(game_id_drive %in% koff.id.two) %>%
#        select(#yds_kickoff, yds_kickoff_return, yards_gained,
#          game_id_drive, pos_team, def_pos_team, change_of_pos_team, play_type, play_text, scoring))

## Recalculate the 2-kickoff drives, and delete the remaining ones. Those are TOUGH TO SPOT.
kickoff.drive.id <- pbp %>% group_by(game_id_drive) %>% summarise(koff.num=sum(str_detect(play_type, "Kickoff")),
                                                                  koff.play.index=ifelse(length(which(str_detect(play_type, "Kickoff"))) == 0, 0, which(str_detect(play_type, "Kickoff"))))

kickoff.drive.id
koff.id.two <- kickoff.drive.id %>% filter(koff.num > 1) %>% select(game_id_drive) %>% .[[1]]
koff.id.two
koff.id.two.halves <- unique(pbp$game_id_half[pbp$game_id_drive %in% koff.id.two])
koff.id.two.halves

dim(pbp)
pbp <- pbp[!(pbp$game_id_half %in% koff.id.two.halves), ]
dim(pbp)


## c. ONE-kickoff drives, where KICKOFF IS NOT THE FIRST PLAY

# summary(kickoff.drive.id$koff.play.index)
kickoff.not.first.play.drive.id <- kickoff.drive.id %>% filter(koff.num == 1 & koff.play.index > 1) %>% select(game_id_drive) %>% .[[1]]
kickoff.not.first.play.drive.id

# None of these are 1-play Kickoffs like a return TD, safety, or fumble recovered by kicking team, as it should be
unique(pbp %>% filter(game_id_drive %in% kickoff.not.first.play.drive.id) %>% select(drive_result_detailed) %>% .[[1]])

# "400547646.1" - seems to be misplaced to the end of the drive, but yards to goal don't really match up..
# 400547646.7 - the final play (rushing TD) got moved to the top of the drive, ahead of kickoff..
# Misplaced to the end, yards match up, BUT.. the drive_result_detailed is nonsensical:
#   400547728.1, 400547728.7, 400547751.1, 400547960.15

## c1. Need to find all the drives where Kickoff's  yards_to_goal_end correspond with yards_to_goal of the FIRST play of the drive..
##

salvageable.koff.id <- pbp[pbp$game_id_drive %in% kickoff.not.first.play.drive.id,] %>% 
  group_by(game_id_drive) %>%
  summarise(kickoff.salvageable = (#str_detect(tail(play_type,1), "Kickoff") & 
    (yards_to_goal_end[which(str_detect(play_type, "Kickoff"))] == head(yards_to_goal, 1)))) %>%
  filter(kickoff.salvageable) %>% .[[1]]

# View(pbp %>% filter(game_id_drive == salvageable.koff.id[3]) %>%
#        select(#yds_kickoff, yds_kickoff_return, yards_gained,
#          yards_to_goal, pos_team_score, def_pos_team_score, drive_number, drive_play_number, play_type, play_text))



## Sometimes "Kickoff" plays get weird drive_result_detailed values, singular stand-out values..
##  We'll just assign "NA" as drive_result for these "misplaced Kickoff plays"
our.ind <- which(pbp$game_id_drive %in% salvageable.koff.id & str_detect(pbp$play_type, "Kickoff"))
if (length(our.ind) > 0)
  pbp[our.ind, "drive_result_detailed"] <- NA

##  Even though it happens more often than not, but not always, and we'll just tackle all these "multi drive results" issues later
# n.diff.drive.res <- pbp[pbp$game_id_drive %in% salvageable.koff.id, ] %>%
#     group_by(game_id_drive) %>%
#     summarize(drive.results = length(unique(drive_result_detailed))) %>%
#     ungroup()
# 
# table(n.diff.drive.res$drive.results)
# 
# ## For drives with 2 different outcomes
# two.diff.drive.res <- n.diff.drive.res %>% filter(drive.results == 2) %>% select(game_id_drive) %>% .[[1]]
# 
# ## For drives that have >2 different outcomes - drop those. These are tough to fix systematically
# three.diff.drive.res <- n.diff.drive.res %>% filter(drive.results > 2) %>% select(game_id_drive) %>% .[[1]]



# dim(pbp[which(pbp$game_id_drive %in% salvageable.koff.id), ])
# dim(pbp[which(pbp$game_id_drive %in% salvageable.koff.id), ] %>%
#       group_by(game_id_drive) %>%
#       dplyr::slice(which(str_detect(play_type, "Kickoff")), c(1:length(play_type))[-which(str_detect(play_type, "Kickoff"))]) %>%
#       ungroup())



## Making "Kickoff" as the first play in these drives

######### !!! ORDER OF GAMES GETS RUINED HERE !!!  SLICE/SUMMARISE GET THE GROUPING VARIABLE OUT OF ORDER !!!!

pbp[which(pbp$game_id_drive %in% salvageable.koff.id), ] <- pbp[which(pbp$game_id_drive %in% salvageable.koff.id), ] %>%
  group_by(game_id_drive) %>%
  dplyr::slice(which(str_detect(play_type, "Kickoff")), c(1:length(play_type))[-which(str_detect(play_type, "Kickoff"))]) %>%
  ungroup()

## Checking if the order matches:
bad.ind <- which(pbp[which(pbp$game_id_drive %in% salvageable.koff.id), ] %>% select(game_id_drive) %>% .[[1]] !=
                   pbp[which(pbp$game_id_drive %in% salvageable.koff.id), ] %>%
                   group_by(game_id_drive) %>%
                   dplyr::slice(which(str_detect(play_type, "Kickoff")), c(1:length(play_type))[-which(str_detect(play_type, "Kickoff"))]) %>%
                   ungroup() %>% select(game_id_drive) %>% .[[1]])

## For those that it doesn't match, there are issues in drive ordering => DISPOSE OF THOSE HALVES (there's just one of them)
if (length(bad.ind) > 0){
  bad.game_half <- pbp[which(pbp$game_id_drive %in% salvageable.koff.id)[bad.ind], ] %>% select(game_id_half) %>% unique() %>% .[[1]]
  bad.game_half
  
  ## Check the resulting arrangements:
  # View(pbp %>% filter(game_id_drive == salvageable.koff.id[1]) %>%
  #        select(#yds_kickoff, yds_kickoff_return, yards_gained,
  #          yards_to_goal, pos_team_score, def_pos_team_score, drive_number, drive_play_number, play_type, play_text))
  
  
  dim(pbp)
  pbp <- pbp[!(pbp$game_id_half %in% bad.game_half), ]
  dim(pbp)
}



## c2. Rest (unsalvageable) - just drop their respective halves.

non.salvageable.koff.id <- kickoff.not.first.play.drive.id[!kickoff.not.first.play.drive.id %in% salvageable.koff.id]
non.salvageable.koff.id

# Collecting all problematic halves, dropping them
non.salvageable.koff.id.halves <- unique(pbp[pbp$game_id_drive %in% non.salvageable.koff.id,]$game_id_half)

dim(pbp)
pbp <- pbp[!(pbp$game_id_half %in% non.salvageable.koff.id.halves), ]
dim(pbp)

# View(pbp %>% filter(game_id_drive %in% non.salvageable.koff.id) %>%
#        select(#yds_kickoff, yds_kickoff_return, yards_gained,
#          game_id_drive, pos_team, def_pos_team, change_of_pos_team, play_type, play_text, scoring, yards_to_goal, yards_to_goal_end))

# table(pbp$yards_to_goal[str_detect(pbp$play_text, "kickoff")])



## d. Some kickoffs have "yards_to_goal" at 35 for some reason, instead of 65 (same for previous play's "yards_to_goal_end")
##    Gotta just flip to 65, for consistency.

ind.play.type <- which(str_detect(pbp$play_text, "kickoff") & pbp$yards_to_goal == 35)
ind.play.type


#print("problematic, not to remove")
#print(length(ind.play.type))

if (length(ind.play.type) > 0){
  pbp[ind.play.type, ]$yards_to_goal <- 65
  pbp[ind.play.type-1, ]$yards_to_goal_end <- 65
}

  
  
# View(#pbp[ind.play.type,] %>%
#   #pbp[sort(c(ind.play.type)),] %>%
#   pbp[sort(c(ind.play.type, ind.play.type-1)),] %>%
#     # pbp[sort(c(ind.play.type, ind.play.type-1, ind.play.type+1)),] %>%
#     # pbp[ind.play.type[which(pbp[ind.play.type, "yards_gained"] != (pbp[ind.play.type, "yards_to_goal"] - pbp[ind.play.type, "yards_to_goal_end"]))], ] %>%
#     #  filter(game_id_half %in% bad.switch.halves.vol.3) %>%
#     #pbp %>% filter(game_id_drive %in% c("400610227-20", "400610227-21")) %>%
#     select(game_id_drive,
#            pos_team, def_pos_team, drive_number, change_of_pos_team, drive_result_detailed, play_type, play_text,
#            yards_to_goal, yards_to_goal_end,
#            yards_gained, 
#            yds_kickoff, yds_kickoff_return,
#            # yds_punted, yds_punt_return, yds_punt_gained, 
#            # yds_rushed, yds_receiving,
#            # rush_yds, reception_yds, 
#            yds_fumble_return,
#            penalty_flag, yds_penalty,
#            pos_team_score, def_pos_team_score))






## "Kickoff (Safety)"
# Flipping the possession, because for Kickoff (Safety) it's not the same as for Kickoff.. but it kinda should be.
ko.safety.ind <- which(pbp$play_type == "Kickoff (Safety)")

if (length(ko.safety.ind) > 0){
  pbp[ko.safety.ind, c("pos_team", "def_pos_team")] <- pbp[ko.safety.ind, c("def_pos_team", "pos_team")]
  pbp[ko.safety.ind, c("pos_team_score", "def_pos_team_score")] <- pbp[ko.safety.ind, c("def_pos_team_score", "pos_team_score")]
  pbp[ko.safety.ind, c("pos_score_diff_start", "pos_score_diff_start_end", "pos_score_diff", "score_pts")] <- - pbp[ko.safety.ind, c("pos_score_diff_start", "pos_score_diff_start_end", "pos_score_diff", "score_pts")]
  pbp[ko.safety.ind, "pos_team_receives_2H_kickoff"] <- 1 - pbp[ko.safety.ind, "pos_team_receives_2H_kickoff"]
}



## "On-Side Kick Lost" (BY THE RECEIVING TEAM)

# Delete the PENALTY ones.. those are SUPER rare.. and penalties are tiny anyway (5 yds)
dim(pbp)
pbp <- pbp[!(str_detect(tolower(pbp$play_text), "on-side kick") & (pbp$change_of_pos_team == 1) & pbp$penalty_no_play), ]
dim(pbp)

## Checking the drive_id with onside kicks
##   Yep, whenever "pbp$change_of_pos_team == 1" (or, equivalently, the pos/def_pos teams don't change after the Kickoff row),
##   that means that the kicking team recovered it, hence the RETURNING TEAM LOST IT

# onside.kick.play.id <- str_detect(tolower(pbp$play_text), "on-side kick") & (pbp$change_of_pos_team == 0)
# onside.kick.drive.id <- unique(pbp$game_id_drive[onside.kick.play.id])
# onside.kick.drive.id
# 
# View(pbp %>%
#       # filter(game_id_drive %in% onside.kick.drive.id) %>%
#        filter(game_id_drive %in% c("400610179-20", "400610179-21")) %>%
#        select(game_id_drive, play_text, play_type, drive_number, change_of_pos_team,
#               yards_to_goal, yards_to_goal_end,
#               offense_score, defense_score, pos_team, def_pos_team,
#               period,  TimeSecsRem, TimeSecsRem_end, SecondsElapsed,
#               clock.minutes, clock.seconds,
#               drive_result_detailed))


## If we have an on-side kick LOST BY THE RECEIVING TEAM (RARE OCCASION)
##    1. We rename the corresponding DRIVE result into "On-Side Kick Lost"

onside.recov.play.id <- which(str_detect(tolower(pbp$play_text), "on-side kick") & (pbp$change_of_pos_team == 1) & pbp$pos_team != lead(pbp$pos_team) & !pbp$penalty_no_play)
if (length(onside.recov.play.id) > 0)
  pbp$drive_result_detailed[onside.recov.play.id] <- "On-Side Kick Lost"

# View(pbp[sort(c(onside.recov.play.id, onside.recov.play.id+1)), 
#          c("game_id_drive", "pos_team", "def_pos_team", "yards_to_goal", "yds_kickoff", "yds_kickoff_return", "yards_gained", "play_text", "penalty_no_play")])

## There are also ISSUES WITH CLOCK for On-Side kick plays.. specifically, that clock at the end of the on-side kick play is EARLIER than clock at the START OF THE DRIVE..
##  (for MOST of them.. there's a handful where it's not an issue, ~5%, but doesn't really matter)
## Instead of screwing with drive starting times, and given that on-side kick plays are typically SHORT, I just changed the clock at the end of the on-side kick play to be = start of the drive

pbp[which(str_detect(tolower(pbp$play_text), "on-side kick")), c("clock.minutes", "clock.seconds")] <- pbp[which(str_detect(tolower(pbp$play_text), "on-side kick")), c("drive_time_minutes_start", "drive_time_seconds_start")]


# View(pbp[str_detect(tolower(pbp$play_text), "on-side kick"), ] %>% 
#        # pbp[pbp$play_type == "Kickoff Return Touchdown", ] %>% 
#        filter(60*clock.minutes + clock.seconds < 60*drive_time_minutes_start + drive_time_seconds_start) %>%
#        #  filter(60*clock.minutes + clock.seconds >= 60*drive_time_minutes_start + drive_time_seconds_start) %>%
#        select(game_id_drive, penalty_no_play, play_type, play_text, clock.minutes, clock.seconds, 
#               drive_time_minutes_start, drive_time_seconds_start,
#               drive_time_minutes_end, drive_time_seconds_end,
#               drive_time_minutes_elapsed, drive_time_seconds_elapsed))


## LASTLY, We need to count "On-Side Kick Lost" as a 1-play possession for the receiving team,
##  Using the "drive_number - 0.5" trick 
##  (Doing "-0.5" instead of "+0.5" makes sure it's not larger than for the plays in the drive to follow)

onside.lost.play.id <- which(str_detect(tolower(pbp$play_text), "on-side kick") & (pbp$change_of_pos_team == 1)  & pbp$pos_team != lead(pbp$pos_team) & !pbp$penalty_no_play)

if (length(onside.lost.play.id) > 0){
  pbp$drive_number[onside.lost.play.id] <- pbp$drive_number[onside.lost.play.id] - 0.5 
  pbp[onside.lost.play.id, c("drive_time_minutes_elapsed", "drive_time_seconds_elapsed")] <- cbind(0, apply(pbp[onside.lost.play.id, c("drive_time_minutes_start","drive_time_seconds_start")] -
                                                                                                            pbp[onside.lost.play.id, c("clock.minutes","clock.seconds")],
                                                                                                          1, function(x) sum(60*x[1] + x[2])))
}

pbp$game_id_drive <- paste(pbp$game_id, pbp$drive_number, sep="-")
pbp$game_id_drive <- factor(pbp$game_id_drive, levels = unique(pbp$game_id_drive))







## DEALING WITH "ONE-PLAY KICKOFF POSSESSION" SCENARIOS: "Kickoff Return Touchdown", "Kickoff (Safety)", "Kickoff Team Fumble Recovery"

## a1. Some are "Kickoff Return Touchdown", or "Kickoff (Safety)", followed by another "Kickoff", with NO CHANGE OF POSSESSION BEING RECORDED...
#       BUT.. some of them actually have a legitimate "2nd play" which is a PENALTY
#       THEN.. some "Kickoff Team Fumble Recovery" gets treated either as a 1-play possession, or actually includes the drive to follow without change in possession..
#
# If it's a one-play drive - no changes needed.
# If it's a two-play drive - check if the second is "Penalty", then treat it as a proper drive, otherwise:
##      - For "Kickoff Team Fumble Recovery", make the first play as its own drive by subtracting -0.5
##      - For "Kickoff Return Touchdown", "Kickoff (Safety)" - remove those halves, as this is an error.
# If it's a three+ play drive:
##     - For "Kickoff Team Fumble Recovery", make the first play as its own drive by subtracting -0.5
##    - For "Kickoff Return Touchdown", "Kickoff (Safety)" - remove those halves, as this is an error.

#       For these, we can do a CHANGE OF DRIVE NUMBER via "-0.5" trick.. Let's do it for ALL "Kickoff Return Touchdown" and "Kickoff (Safety)"

koff.fumb.recov.id <- which(pbp$play_type %in% c("Kickoff Team Fumble Recovery"))
koff.score.drive.id <- unique(pbp[pbp$play_type %in% c("Kickoff Team Fumble Recovery"),]$game_id_drive)

# Finding all the 2-play drives with "Kickoff Team Fumble Recovery" that did NOT have the second play as "Penalty" or "Timeout"
koff.fumb.recov.two.play.drive_ids <- pbp %>% 
  group_by(game_id_drive) %>% 
  summarize(no.plays=n(), relev.play = any(play_type == "Kickoff Team Fumble Recovery"), second.play = play_type[2]) %>% 
  filter(no.plays == 2 & relev.play & !(second.play %in% c("Penalty", "Timeout"))) %>% .[[1]]
koff.fumb.recov.two.play.drive_ids

# Applying the "-0.5" to first plays of the "Kickoff Team Fumble Recovery" 2-play drives that did NOT have the second play as "Penalty"
# (plus adjusting the "drive" time)
# ISSUE: DRIVE TIMES WON'T BE SUPER-CONSISTENT, because I'M NOT SUBTRACTING THE "QUICK SCORE/FUMBLE" TIME from the WHOLE DRIVE...
#        BUT IT WILL BE CLOSE

our.ind <- which((pbp$game_id_drive %in% koff.fumb.recov.two.play.drive_ids) & (pbp$play_type %in% c("Kickoff Team Fumble Recovery")))

if (length(our.ind) > 0){
  pbp[our.ind, ]$drive_number <- pbp[our.ind, ]$drive_number - 0.5 
  pbp[our.ind, c("drive_time_minutes_elapsed", "drive_time_seconds_elapsed")] <- cbind(0, apply(pbp[our.ind, c("drive_time_minutes_start","drive_time_seconds_start")] -
                                                                                                        pbp[our.ind, c("clock.minutes","clock.seconds")],
                                                                                                                   1, function(x) sum(60*x[1] + x[2])))
}

pbp$game_id_drive <- paste(pbp$game_id, pbp$drive_number, sep="-")
pbp$game_id_drive <- factor(pbp$game_id_drive, levels = unique(pbp$game_id_drive))




# Finding all the 3+ play drives with "Kickoff Team Fumble Recovery"
koff.fumb.recov.three.plus.play.drive_ids <- pbp %>% 
  group_by(game_id_drive) %>% 
  summarize(no.plays=n(), relev.play = any(play_type == "Kickoff Team Fumble Recovery")) %>% 
  filter(no.plays > 2 & relev.play) %>% .[[1]]
koff.fumb.recov.three.plus.play.drive_ids 

# Applying the "-0.5" to first plays of the "Kickoff Team Fumble Recovery" 3+ play drives
our.ind <- which((pbp$game_id_drive %in% koff.fumb.recov.three.plus.play.drive_ids) & (pbp$play_type %in% c("Kickoff Team Fumble Recovery")))
if (length(our.ind) > 0){
  pbp[our.ind, ]$drive_number <- pbp[our.ind, ]$drive_number - 0.5 
  pbp[our.ind, c("drive_time_minutes_elapsed", "drive_time_seconds_elapsed")] <- cbind(0, apply(pbp[our.ind, c("drive_time_minutes_start","drive_time_seconds_start")] -
                                                                                                         pbp[our.ind, c("clock.minutes","clock.seconds")],
                                              1, function(x) sum(60*x[1] + x[2])))
}

pbp$game_id_drive <- paste(pbp$game_id, pbp$drive_number, sep="-")
pbp$game_id_drive <- factor(pbp$game_id_drive, levels = unique(pbp$game_id_drive))



# Finding all the 2-play drives with "Kickoff Return Touchdown", "Kickoff (Safety)" that did NOT have the second play as "Penalty" or "Timeout"
koff.TD.or.Safety.two.play.drive_ids <- pbp %>% 
  group_by(game_id_drive) %>% 
  summarize(no.plays=n(), relev.play = any(play_type %in% c("Kickoff Return Touchdown", "Kickoff (Safety)")), second.play = play_type[2]) %>% 
  filter(no.plays == 2 & relev.play & !(second.play %in% c("Penalty", "Timeout"))) %>% .[[1]]
koff.TD.or.Safety.two.play.drive_ids

# Removing those halves, as that's in error.
koff.TD.or.Safety.two.play.half_ids <- unique(pbp[pbp$game_id_drive %in% koff.TD.or.Safety.two.play.drive_ids, ]$game_id_half)
dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% koff.TD.or.Safety.two.play.half_ids, ]
dim(pbp)

# Finding all the 3+ play drives with "Kickoff Return Touchdown", "Kickoff (Safety)"
koff.TD.or.Safety.three.plus.play.drive_ids <- pbp %>% 
  group_by(game_id_drive) %>% 
  summarize(no.plays=n(), relev.play = any(play_type %in% c("Kickoff Return Touchdown", "Kickoff (Safety)"))) %>% 
  filter(no.plays > 2 & relev.play) %>% .[[1]]
koff.TD.or.Safety.three.plus.play.drive_ids 

# Removing those halves, as that's in error.
koff.TD.or.Safety.three.plus.play.half_ids <- unique(pbp[pbp$game_id_drive %in% koff.TD.or.Safety.three.plus.play.drive_ids, ]$game_id_half)
dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% koff.TD.or.Safety.three.plus.play.half_ids, ]
dim(pbp)

# View(pbp %>%
#        #filter(game_id_drive %in% koff.fumb.recov.two.play.drive_ids) %>%
#        # filter(game_id_drive %in% koff.fumb.recov.three.plus.play.drive_ids) %>%
#        # filter(game_id_drive %in% koff.TD.or.Safety.two.play.drive_ids) %>%
#        filter(game_id_drive %in% koff.TD.or.Safety.three.plus.play.drive_ids) %>%
#        select(#yds_kickoff, yds_kickoff_return, yards_gained,
#          game_id_drive, pos_team, def_pos_team, change_of_pos_team, play_type, play_text, scoring))





pbp$game_id_drive <- paste(pbp$game_id, pbp$drive_number, sep="-")
pbp$game_id_drive <- factor(pbp$game_id_drive, levels = unique(pbp$game_id_drive))

# All clean.
# #   Checking the drive results for these 1-play kickoffs "drives": all exactly as need be ("diagonal" confusion matrix)
# # 
# koff.score.play.id <- which(pbp$play_type %in% c("Kickoff Return Touchdown", "Kickoff (Safety)", "Kickoff Team Fumble Recovery"))
# table(pbp[koff.score.play.id, c("play_type", "drive_result_detailed")])
# #   Checking the length of these 1-play kickoff "drives": all 1-play drives, as they should be
# koff.score.drive.id <- unique(pbp$game_id_drive[koff.score.play.id])
# table(pbp %>% filter(game_id_drive %in% koff.score.drive.id) %>% group_by(game_id_drive) %>% summarise(drive.length = length(play_type)) %>% select(drive.length) %>% .[[1]])



# dim(pbp[pbp$game_id_drive == "400547905-12",])












########
## 1. Checking the games where clock gets out-of-order
##  (would drop about 20 halves)
#########


bad.clock.id <- pbp %>% group_by(game_id_half) %>% summarise(flag_clock = sum(diff(SecondsElapsed) < 0)) %>% filter(flag_clock > 0) %>% select(game_id_half) %>% .[[1]]
bad.clock.id

# Issues with out-of-order plays, plus stagnant clock within possessions:
##  400547646, 400547654, 400547726

## One weird off-play (seems FIXABLE by just ARRANGING IT PROPERLY)
# 400547679, 400547706, 40054774522, 400547757

## One weird off-play.. but doesn't look fixable - data entry error for CLOCK..
# 400547710, 400547765

# View(pbp %>% 
#        #filter(game_id_half %in% bad.clock.id[1]) %>%
#        mutate(clock.diff = c(NA,diff(SecondsElapsed))) %>%
#        select(drive_id, play_text, play_type, 
#               offense_score, defense_score, pos_team, def_pos_team,
#               period,  TimeSecsRem, TimeSecsRem_end, SecondsElapsed, clock.diff, 
#               clock.minutes, clock.seconds, 
#               drive_result_detailed))

dim(pbp)
pbp <- pbp %>% filter(!(game_id_half %in% bad.clock.id))
dim(pbp)



#####
## 2. Checking games where the clock gets STAGNANT within possessions.
## ~700 halves have at least some clock stagnation.. pretty pointless to try & chase those down...
####

stagn.clock.id <- pbp %>% group_by(game_id_half, drive_number) %>% 
  summarise(stagn_clock = ifelse(length(SecondsElapsed) > 3,
                                 all(SecondsElapsed[2:(length(SecondsElapsed)-1)] == SecondsElapsed[2]),
                                 NA),
            .groups = "keep") %>%                                 # SIMPLY HERE TO AVOID PRINTOUTS
  filter(stagn_clock) #%>% select(game_id) %>% .[[1]]

## Sanity check for ".groups = "keep", if needed
# all(stagn.clock.id$game_id_half == stagn.clock.id.1$game_id_half)
# all(stagn.clock.id$drive_number == stagn.clock.id.1$drive_number)
# all(stagn.clock.id$stagn_clock == stagn.clock.id.1$stagn_clock)



unique(stagn.clock.id$game_id_half)
# View(stagn.clock.id)
# tail(stagn.clock.id)


# View(pbp %>% filter(game_id == 400610225) %>%
#              mutate(drive.diff = c(NA,diff(drive_num))) %>%
#              select(drive_id, play_text, play_type, 
#                     offense_score, defense_score, pos_team, def_pos_team,
#                     period,  TimeSecsRem, TimeSecsRem_end, SecondsElapsed, 
#                     drive.diff, drive_num, drive_num,
#                     clock.minutes, clock.seconds, 
#                     drive_result_detailed))


#####
## 2p5. YARDS_TO_GOAL/END that are NA to begin with.. and don't have to do with "END OF HALF" stuff
##    NO SUCH CASES...
#####

## Not really an issue for "yards_to_goal"
relev.ind <- which(is.na(pbp$yards_to_goal))
summary(pbp$yards_to_goal)

##
relev.ind <- which(is.na(pbp$yards_to_goal_end) & !str_detect(tolower(pbp$play_type), "end of"))
summary(pbp$yards_to_goal_end)

# View(pbp[relev.ind, ] %>%
#        #pbp %>% filter(game_id_drive %in% "400547641-14") %>%
#        select(game_id_drive, pos_team, def_pos_team, change_of_pos_team, yards_gained, yards_to_goal, yards_to_goal_end, penalty_flag, drive_result_detailed,
#               play_type, play_text))


#####
## 3. YARDS_TO_GOAL, YARDS_TO_GOAL END - making them NA for all the  End of Half/Game plays (they are not reliable)
#####

## MEANWHILE, for the PRE-End of Half/Game plays - the YARDS_TO_GOAL_END is NOT RELIABLE then.. so MAKE IT "NA" for EVERYONE at first..
##  ALSO, there are some HALVES that end ON REGULAR PLAY OBSERVATIONS, NOT OF "END OF HALF" DESIGNATION (via DATA ENTRY ISSUES),
##  FOR THOSE, make sure to get their "yards_to_goal_end" as NA

pbp[which((str_detect(tolower(pbp$play_type), "end of half") | str_detect(tolower(pbp$play_type), "end of game"))), 
    c("yards_to_goal", "yards_to_goal_end")] <- NA

pre.eof.play.ind <- which(str_detect(tolower(lead(pbp$play_type)), "end of half") | str_detect(tolower(lead(pbp$play_type)), "end of game"))
if (length(pre.eof.play.ind) > 0) pbp[pre.eof.play.ind, c("yards_to_goal_end")] <- NA

## Halves that ended with the non-"End of Half" plays due to data entry issues
no.eof.play.ind <- which(!(str_detect(tolower(pbp$play_type), "end of half") | str_detect(tolower(pbp$play_type), "end of game")) &
                           (pbp$game_id_half != lead(pbp$game_id_half)))
if (length(no.eof.play.ind) > 0) pbp[no.eof.play.ind, c("yards_to_goal_end")] <- NA

# View(pbp[sort(c(which(relev.ind), which(relev.ind)+1)), ] %>%
#       # pbp[sort(c(pre.eof.play.ind, pre.eof.play.ind+1)), ] %>% 
#        #pbp %>% filter(game_id_drive %in% "400547641-14") %>%
#        select(game_id_drive, pos_team, def_pos_team, change_of_pos_team, yards_gained, yards_to_goal, yards_to_goal_end, penalty_flag, drive_result_detailed,
#               play_type, play_text))


## SANITY CHECKING:

# ## All the "yards_to_goal" and "yards_to_goal_end" are NA for pure "End of Half" plays
# summary(pbp[which(str_detect(tolower(pbp$play_type), "end of half") | str_detect(tolower(pbp$play_type), "end of game")),
#                   c("yards_to_goal", "yards_to_goal_end")])
# 
# ## All the "yards_to_goal_end" are NA for last plays of half that aren't denoted as "End of Half" due to data entry issues
# summary(pbp[!(str_detect(tolower(pbp$play_type), "end of half") | str_detect(tolower(pbp$play_type), "end of game")) & (pbp$game_id_half != lead(pbp$game_id_half)), 
#               c("yards_to_goal", "yards_to_goal_end")])



##########################################
##### 4. DEALING WITH "100/0" VALUES of YARDS_TO_GOAL/_END...
#####   THOSE VASTLY LEAD TO PROBLEMS IN CONTINUITY OF "current YARDS_TO_GOAL_END == next YARDS_TO_GOAL"
#####
##### DECISION FOR NOW: JUST THROW OUT HALVES that have CONSECUTIVE PLAYS with 0/100 VALUES in "current YARDS_TO_GOAL_END/next YARDS_TO_GOAL"
#### (THERE'S ONLY 15 OF SUCH HALVES in 2014)
##########################################

## All the 100's happen on penalties (penalty_flag == TRUE)
# bad.ind <- which(pbp$yards_to_goal == 100 & !pbp$penalty_flag)
# bad.ind

## Checking consecutive 100s (yards_to_goal == previous yards_to_goal_end)
# bad.ind <- which(pbp$yards_to_goal == 100 & lag(pbp$yards_to_goal_end) == 100)
# bad.ind


####
## LET'S SAY I THROW OUT THESE HALVES (ANYTHING THAT HAS A 100 OR A 0 in YARDS_TO_GOAL STUFF)
## => LEADS TO LOSING EXTRA 80 HALVES... about 8K OBSERVATIONS
## MIGHT NOT WANNA DO THAT.
##
# bad.ind <- which(pbp$yards_to_goal_end == 100 | pbp$yards_to_goal == 100 | pbp$yards_to_goal == 0 | pbp$yards_to_goal_end == 0)
# length(unique(pbp$game_id_half[bad.ind]))

####
## BUT WE CAN THROW OUT HALVES with CONSECUTIVE 0/100 VALUES ("UNSALVAGEABLE CASES", SO-TO-SPEAK)
##    => THAT'S JUST 15 HALVES (AND THEY ARE ALL TRULY BROKEN)
##
bad.ind <- which(pbp$yards_to_goal_end %in% c(0, 100) & lead(pbp$yards_to_goal) %in% c(0,100))
length(unique(pbp$game_id_half[bad.ind]))

# table(pbp[pbp$yards_to_goal_end == 100, ]$play_type)
# table(pbp[pbp$yards_to_goal == 100, ]$play_type)
# pbp[pbp$play_type == "End Period",]$play_text


# View(pbp[sort(c(bad.ind, bad.ind+1)), ] %>%
#   #pbp %>% filter(game_id_drive %in% "400547916-1") %>%
#     select(game_id_drive, pos_team, def_pos_team, change_of_pos_team, yards_gained, yards_to_goal, yards_to_goal_end, penalty_flag, play_type, play_text))


## REMOVING those bad observations

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% unique(pbp$game_id_half[bad.ind]), ]
dim(pbp)




####
## 5p5. TOUCHBACK KICKOFFS ISSUES:
##      - seem to keep showing 25 YARDS for some...
##   Sometimes it just RANDOMLY decides to say that it's "yards_to_goal=25" for the team that starts driving after touchback, which should be 75
##   BUT that's not the worst part - it keeps counting SUBSEQUENT yards_to_goal as if it were indeed 25 yards to goal, 
##   even though play text CLEARLY indicates movement in the other direction
## => GOTTA JUST DROP THOSE HALVES (WHICH I THINK WAS GONNA HAPPEN ANYWAY due to SUBSEQUENT DISCREPANCIES.. BUT BETTER MAKE SURE)
####

bad.ind <- which(ifelse(pbp$play_type == "Kickoff" & str_detect(pbp$play_text, "touchback") & 
                          !pbp$penalty_flag &
                          (pbp$yards_to_goal_end == 25 | lead(pbp$yards_to_goal) == 25), T, F))
bad.ind

# View(pbp[sort(c(bad.ind, bad.ind+1)), ] %>%
#        #pbp %>% filter(game_id_drive %in% "400547916-1") %>%
#        select(game_id_drive, pos_team, def_pos_team, change_of_pos_team, yards_gained, yards_to_goal, yards_to_goal_end, penalty_flag, drive_result_detailed, play_type, play_text))

bad.halves <- unique(pbp$game_id_half[bad.ind])
bad.halves

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% bad.halves, ]
dim(pbp)



#####
## TOUCHBACK KICKOFFS WITH PENALTIES with "yards_to_goal_end" != 75 - need to be LOOKED INTO
##  Evidently none of them here?
#####
bad.ind <- which(ifelse(pbp$play_type == "Kickoff" & str_detect(pbp$play_text, "touchback") & 
                          pbp$yards_to_goal_end != 75 & !pbp$penalty_flag, T, F))
bad.ind

bad.halves <- unique(pbp$game_id_half[bad.ind])
bad.halves

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% bad.halves, ]
dim(pbp)


#######
## 5p6. Kickoffs WITH WEIRD "YARDS_TO_GOAL" AT THE START (and the PREVIOUS PLAY'S "YARDS_TO_GOAL_END" is THE SAME)
##    The 50-yd line kickoffs are actually a thing, but BELOW THAT - NOPE.
##    So we just REMOVE ANYTHING THAT SAYS "YARDS_TO_GOAL < 50" for the KICKOFF plays, and then:
##        * Plug in 65 if the previous play was NOT a safety
##        * Plug in 80 if previous play was a safety
#######

###
# Checking the weird YARDS_TO_GOAL values at Kickoff
###

ind.play.type <- which((tolower(pbp$play_type) %in% c("kickoff")) 
                       & pbp$yards_to_goal < 50)
ind.play.type

#print("Kickoffs with bad YARDS_TO_GOAL values (<50):")
#print(length(ind.play.type))

table(pbp[ind.play.type, ]$yards_to_goal)


## Taking care of NON-SAFETIES
ind.play.type <- which((tolower(pbp$play_type) %in% c("kickoff")) 
                       & pbp$yards_to_goal < 50 
                       & !str_detect(tolower(lag(pbp$play_type)), "safety"))

if (length(ind.play.type) > 0){
## Just plug in 65
pbp[ind.play.type, ]$yards_to_goal <- 65
pbp[ind.play.type-1, ]$yards_to_goal_end <- ifelse(str_detect(tolower(pbp[ind.play.type-1,]$play_type), "end of"),  
                                                   pbp[ind.play.type-1, ]$yards_to_goal_end,
                                                   65)
}


## Taking care of SAFETIES (RARE though)
ind.play.type <- which((tolower(pbp$play_type) %in% c("kickoff")) 
                       & pbp$yards_to_goal < 50 
                       & str_detect(tolower(lag(pbp$play_type)), "safety"))

if (length(ind.play.type) > 0){
  ## Just plug in 65
  pbp[ind.play.type, ]$yards_to_goal <- 80
  pbp[ind.play.type-1, ]$yards_to_goal_end <- ifelse(str_detect(tolower(pbp[ind.play.type-1,]$play_type), "end of"),  
                                                     pbp[ind.play.type-1, ]$yards_to_goal_end,
                                                     80)
}


# View(   # pbp[ind.play.type,] %>%
#   pbp[sort(c(ind.play.type, ind.play.type-1)),] %>%
#     # pbp[sort(c(ind.play.type, ind.play.type-1, ind.play.type+1)),] %>%
#     #pbp %>% filter(game_id_drive %in% c("400933888-22", "400933888-23")) %>%
#     # pbp %>% filter(game_id_drive %in% c("400945258-2")) %>%
#     # pbp %>% filter(play_type == "Kickoff", yards_to_goal < 50, yards_to_goal != 20) %>%
#     select(game_id_drive,
#            pos_team, def_pos_team, drive_number, change_of_pos_team, drive_result_detailed, play_type, play_text,
#            yards_to_goal, yards_to_goal_end,
#            yards_gained,
#            yds_kickoff, yds_kickoff_return,
#            yds_fumble_return,
#            penalty_flag, yds_penalty,
#            pos_team_score, def_pos_team_score))








#######
## 5p7. Check to YARDS_TO_GOAL_END of current observation vs YARDS_TO_GOAL of the NEXT OBSERVATIONS..
##    DO IT WITHIN EACH HALF..
##      - Many of these are some really stupid, albeit tiny, data entry errors, where the difference is 1 yard..
##      - Some are DUPLICATES.. gotta dispose of them at the VERY BEGINNING of the cleaning
##
## !!! That kills a HUGE AMOUNT OF HALVES (650 out of 1450.. 2/5th or so..) !!!
#######

## Seems like there are cases where it reasonably deviates by 2yds within a margin of data entry/on-field measurement error/issues...
yds.margin <- 2




######
## IDEA:
##    1. For those that are  WITHIN DRIVES and have a 100-X relationship between consecutive yards_to_goal:
##          Check the CHANGE_OF_POS_TEAM: If it's 0, then ASSIGN "YARDS_TO_GOAL - YARDS_GAINED" TO BOTH 
##                                        If it's 1, then EITHER DROP.. or ASSIGN THE "YARDS_TO_GOAL" OF THE 2ND OBSEVATION?? ALTHOUGH IT HAS ISSUES...
######


# If we want to drop all that are "within yds.margin" as well
# bad.drive.ids <- pbp %>%
#   group_by(game_id_half) %>%
#   summarize(bad_drives =c(ifelse((head(yards_to_goal_end, length(yards_to_goal_end)-1) == tail(yards_to_goal, length(yards_to_goal)-1)),
#                                  0,
#                                  1), 0))

# If we want to retain stuff that is "within yds.margin"
bad.drive.ids <- pbp %>%
  group_by(game_id_half) %>%
  summarize(bad_drives =c(ifelse((head(yards_to_goal_end, length(yards_to_goal_end)-1) >= tail(yards_to_goal, length(yards_to_goal)-1) -yds.margin) &
                                   (head(yards_to_goal_end, length(yards_to_goal_end)-1) <= tail(yards_to_goal, length(yards_to_goal)-1) +yds.margin),
                                 0,
                                 1), 0),
            .groups = "keep")

## Sanity check of .groups = "keep":
# all.equal(bad.drive.ids.1, bad.drive.ids)
# all(bad.drive.ids.1$game_id_half == bad.drive.ids$game_id_half)
# all(bad.drive.ids.1$bad_drives == bad.drive.ids$bad_drives, na.rm = T)


# data.frame(bad.drive.ids$game_id_half, pbp$game_id_half)[which(bad.drive.ids$game_id_half != pbp$game_id_half),]


## INDICES OF ALL BAD PLAYS
our.ind <- which(ifelse(bad.drive.ids$bad_drives == 1, T,F))
length(our.ind)

## ALL THE (FIRST) PLAY TYPES INVOLVED IN BADLY CONNECTED YARDAGES
table(pbp[our.ind, "play_type"])



## HOW MANY HALVES WOULD BE LOST if we JUST DROP ALL THE INCONSISTENCIES:
length(unique(pbp$game_id_half[bad.drive.ids$bad_drives == 1]))
length(unique(pbp$game_id_half))
## About 50%.... (702/1500)


## If it's the within-drive, 100-X, complements of each other (and NOT EQUAL TO EACH OTHER, e.g. 50=100-50), then:
##    If it's change_of_pos_team == 0, then:
##          Check if "yards_to_goal" - yards_gained of the 1st observation is equal to either "yards_to_goal_end" of it, or "yards_to_goal" of 2nd obs
##          If yes - assign it. Otherwise, keep as is (otherwise we can get UNREASONABLE VALUES, like >100 or <0)
##    => That saves us about 200 halves!
complement.ind <- ifelse((pbp[our.ind, "yards_to_goal_end"] != pbp[our.ind+1, "yards_to_goal"]) & (pbp[our.ind, "yards_to_goal_end"] == 100- pbp[our.ind+1, "yards_to_goal"]) & pbp[our.ind,]$change_of_pos_team == 0 & 
                          (pbp[our.ind, "yards_to_goal"] - pbp[our.ind, "yards_gained"] == pbp[our.ind, "yards_to_goal_end"] | 
                             pbp[our.ind, "yards_to_goal"] - pbp[our.ind, "yards_gained"] == pbp[our.ind+1, "yards_to_goal"]), T, F)
# complement.ind <- ifelse(is.na(complement.ind), FALSE, complement.ind) # Takes care of "NA" cases
if (length(our.ind[complement.ind]) > 0){
  pbp[our.ind[complement.ind], "yards_to_goal_end"] <- pbp[our.ind[complement.ind], "yards_to_goal"] - pbp[our.ind[complement.ind], "yards_gained"]
  pbp[our.ind[complement.ind]+1, "yards_to_goal"] <- pbp[our.ind[complement.ind], "yards_to_goal"] - pbp[our.ind[complement.ind], "yards_gained"]
}

## SANITY-CHECKING:
#
# which(pbp[our.ind[complement.ind], "yards_to_goal"] - pbp[our.ind[complement.ind], "yards_gained"] > 100)
# which(pbp[our.ind[complement.ind], "yards_to_goal"] - pbp[our.ind[complement.ind], "yards_gained"] < 0)


### !! NOT A GOOD IDEA: !!
##    If it's change_of_pos_team == 1, and THERE ARE NON-ZERO YARDS GAINED (which are mostly NEGATIVE):
##        Assign 100-(yards_to_goal - yards_gained) to both
##  RESULTED IN AN ARRAY OF BAD RESULTS..

# complement.ind <- which(ifelse((pbp[our.ind, "yards_to_goal_end"] != pbp[our.ind+1, "yards_to_goal"]) & (pbp[our.ind, "yards_to_goal_end"] == 100- pbp[our.ind+1, "yards_to_goal"]), T, F) & pbp[our.ind,]$change_of_pos_team == 1 & pbp[our.ind,]$yards_gained != 0)
# complement.ind <- ifelse(is.na(complement.ind), FALSE, complement.ind) # Takes care of "NA" cases
# pbp[our.ind[complement.ind], "yards_to_goal_end"] <- 100-(pbp[our.ind[complement.ind], "yards_to_goal"] - pbp[our.ind[complement.ind], "yards_gained"])
# pbp[our.ind[complement.ind]+1, "yards_to_goal"] <- 100-(pbp[our.ind[complement.ind], "yards_to_goal"] - pbp[our.ind[complement.ind], "yards_gained"])

### ??? JUST ASSIGN THE SECOND VALUE, AS IT SHOULD BE MORE RELIABLE DUE TO POSSESSION TURNING OVER TO THE OTHER TEAM???
###  SEEMS TO WORK FOR MOST, BUT ALSO HAS A COUPLE MISSES.. BETTER THAN "YARDS_GAINED" THOUGH
complement.ind <- ifelse((pbp[our.ind, "yards_to_goal_end"] != pbp[our.ind+1, "yards_to_goal"]) & (pbp[our.ind, "yards_to_goal_end"] == 100- pbp[our.ind+1, "yards_to_goal"]) & pbp[our.ind,]$change_of_pos_team == 1
                         , T, F)
# complement.ind <- ifelse(is.na(complement.ind), FALSE, complement.ind) # Takes care of "NA" cases
if (length(our.ind[complement.ind]) > 0)
  pbp[our.ind[complement.ind], "yards_to_goal_end"] <- pbp[our.ind[complement.ind]+1, "yards_to_goal"]





## SANITY-CHECKING:
# complement.ind <- which(ifelse(pbp[our.ind, "yards_to_goal_end"] == 100- pbp[our.ind+1, "yards_to_goal"] & pbp[our.ind, "yards_to_goal_end"] != pbp[our.ind+1, "yards_to_goal"], T, F))
# length(complement.ind)
#
# View(# pbp[sort(c(our.ind[complement.ind], our.ind[complement.ind]-1)), ] %>%
#   pbp[sort(c(our.ind, our.ind-1)), ] %>%
#      #  pbp %>% filter(game_id_drive %in% c("400547802-5", "400547802-6")) %>%
#        select(game_id_drive, pos_team, def_pos_team, change_of_pos_team, yards_gained, yards_to_goal, yards_to_goal_end, penalty_flag, drive_result_detailed, play_type, play_text))


## For those with "yards_to_goal_end = 100" entries for the first play... these are all BS. Gotta flip to next play's "yards_to_goal"
##  UNLESS that next play is "yards_to_goal = 0", which are generally a mistake

##   Yep. All of those just seem RANDOM, and the actual number is conveyed more properly (as can be judged from play text) by the next play's "yards_to_goal"
##  AS LONG AS IT'S NOT 0, which, LIKE 100, IS A POTENTIALLY PROBLEMATIC VALUE
## SO, BAD PLAYS WILL REMAIN with "yards_to_goal_end = 100" HERE, with NON-CORRESPONDING "yards_to_goal" & "yards_to_goal_end"
complement.ind <- ifelse(pbp[our.ind, "yards_to_goal_end"] == 100, T, F)
if (length(our.ind[complement.ind]) > 0)
  pbp[our.ind[complement.ind], "yards_to_goal_end"] <- ifelse(pbp[our.ind[complement.ind]+1, "yards_to_goal"] %in% c(0,100),
                                                              100,
                                                              pbp[our.ind[complement.ind]+1, "yards_to_goal"])



## For those with "yards_to_goal = 100" entries of the second play... these are ALSO all BS. Gotta flip to PREVIOUS play's "yards_to_goal_end"
## UNLESS IT'S A 0 OR A 100 (0/100's are only resonable for "yards_to_goal_end" BEFORE the "End of Half" plays, which is NOT something we look at here)
##   Yep. All of those just seem RANDOM, and the actual number is conveyed more properly (as can be judged from play text) by the next play's "yards_to_goal"
## SO, BAD PLAYS WILL REMAIN with "yards_to_goal_end = 100" HERE, with NON-CORRESPONDING "yards_to_goal" & "yards_to_goal_end"
complement.ind <- ifelse(pbp[our.ind+1, "yards_to_goal"] == 100, T, F)
if (length(our.ind[complement.ind]) > 0)
  pbp[our.ind[complement.ind] + 1, "yards_to_goal"] <- ifelse(pbp[our.ind[complement.ind], "yards_to_goal_end"] %in% c(0,100),
                                                              100,
                                                              pbp[our.ind[complement.ind], "yards_to_goal_end"])



## For "yards_to_goal"=0
##   Those are vastly a result of PENALTIES, and we are to replace those with previous play's "yards_to_goal_end" IN CASE it's NOT 0 OR 100 ITSELF
complement.ind <- ifelse(pbp[our.ind, "yards_to_goal"] == 0 & pbp[our.ind, "penalty_flag"] == TRUE, T, F)
complement.ind
if (length(our.ind[complement.ind]) > 0)
  pbp[our.ind[complement.ind], "yards_to_goal"] <- ifelse(pbp[our.ind[complement.ind]-1, "yards_to_goal_end"] %in% c(0,100),
                                                          0,
                                                          pbp[our.ind[complement.ind]-1, "yards_to_goal_end"])

# View(pbp[sort(c(our.ind[complement.ind], our.ind[complement.ind]-1)), ] %>%
#        #pbp %>% filter(game_id_drive %in% "400547641-14") %>%
#        select(game_id_drive, pos_team, def_pos_team, change_of_pos_team, yards_gained, yards_to_goal, yards_to_goal_end, penalty_flag, drive_result_detailed, play_type, play_text))


## "yards_to_goal_end" == 0 - shouldn't be a big issue, as those are reasonable values for a handful pre-End of Half plays.

complement.ind <- ifelse(pbp[our.ind, "yards_to_goal_end"] == 0, T, F)
complement.ind
if (length(our.ind[complement.ind]) > 0){
  pbp[our.ind[complement.ind], "yards_to_goal_end"] <- ifelse(pbp[our.ind[complement.ind]-1, "yards_to_goal_end"] %in% c(0,100),
                                                              0,
                                                              pbp[our.ind[complement.ind]-1, "yards_to_goal_end"])
}




## For those with "Punt for a touchback" - it automatically assigns "65" to the "yards_to_goal_end", probably confusing with Kickoff touchback
##      => FLIP IT TO NEXT PLAY'S "yards_to_goal" (NOT necessarily to 80, because GOD FORBID there's some PENALTY SNEAKING IN...)
complement.ind <- ifelse(pbp[our.ind, "play_type"] == "Punt" & str_detect(pbp[our.ind, "play_text"], "touchback"), T, F)
complement.ind
if (length(our.ind[complement.ind]) > 0)
  pbp[our.ind[complement.ind], "yards_to_goal_end"] <- pbp[our.ind[complement.ind]+1, "yards_to_goal"]


## For Kickoffs, a lot of penalties creeping in, making it from 75 to 70 (5yd penalty), or sometimes by weirder amounts (e.g. personal fouls.. 13yrds)
## So we check for whether a play was a Kickoff & whether it had a penalty flag! For those, flip to next play's "yards_to_goal"
complement.ind <- ifelse(pbp[our.ind, "play_type"] == "Kickoff" & pbp[our.ind, "penalty_flag"], T, F)
complement.ind
if (length(our.ind[complement.ind]) > 0)
  pbp[our.ind[complement.ind], "yards_to_goal_end"] <- pbp[our.ind[complement.ind]+1, "yards_to_goal"]


## For Kickoffs, what about NO PENALTY though?
##  If it's a TOUCHBACK it'll pop up - these will get taken care of later, so I'm explicitly excluding touchbacks
##  If it's NOT a touchback - ASSIGNING THE SECOND VALUE SHOULD BE MORE RELIABLE
complement.ind <- ifelse(pbp[our.ind, "yards_to_goal_end"] != pbp[our.ind+1, "yards_to_goal"] & 
                                 pbp[our.ind, "play_type"] == "Kickoff" & !pbp[our.ind, "penalty_flag"] & !str_detect(pbp[our.ind, "play_text"], "touchback"), T, F)
complement.ind
if (length(our.ind[complement.ind]) > 0)
  pbp[our.ind[complement.ind], "yards_to_goal_end"] <- pbp[our.ind[complement.ind]+1, "yards_to_goal"]


## "Kickoff Return Touchdown", "Kickoff Team Fumble Recovery" - don't appear to have problems
# complement.ind <- which(ifelse(pbp[our.ind, "yards_to_goal_end"] != pbp[our.ind+1, "yards_to_goal"] & 
#                                  pbp[our.ind, "play_type"] %in% c("Kickoff Return Touchdown", "Kickoff Team Fumble Recovery") 
#                                , T, F))
# complement.ind <- ifelse(is.na(complement.ind), FALSE, complement.ind) # Takes care of "NA" cases
# complement.ind


## For Sacks.. some really weird stuff - the play text would state "for a loss of X yards", the yards_to_goal_end would reflect that, BUT..
##   then play text also says "to ... yard-line", and that yard-line contradicts the "loss of X yards", but is taken as gospel for next observations "yards_to_goal"
## THAT'S A DATA ERROR, and NO IDEA WHICH VALUE IS RIGHT.. SO NEED TO DROP THOSE, SORRY...

# View(pbp[sort(c(which(pbp$play_type == "Sack") -1, which(pbp$play_type == "Sack"), which(pbp$play_type == "Sack")+1)), ] %>% 
#        select(game_id_drive, yards_to_goal, yards_to_goal_end, drive_result_detailed, play_type, play_text))

## For PENALTIES...
##  Mostly some legit errors with data recording (yards_to_goal are misplaced between the 2 teams.. constant misnomers.. observations NEED TO BE DROPPED)
# complement.ind <- which(ifelse(pbp[our.ind, "play_type"] == "Penalty", T, F))
# View(pbp[sort(c(our.ind[complement.ind]-1, our.ind[complement.ind], our.ind[complement.ind]+1)), ] %>% 
#        select(game_id_drive, yards_to_goal, yards_to_goal_end, pos_team, def_pos_team, penalty_flag, drive_result_detailed, play_type, play_text))

#  Only cases that are somewhat distinguishable: PENALTY RIGHT AFTER A TOUCHDOWN SCORE??
## YES, because the EXTRA POINT generally takes place from a 3-YARD LINE.. but IT CAN GET MOVED DUE TO THE PENALTY 
## (either to 1-yd or 13-yd, for example) - and EACH TIME THAT HAPPENS, there's NO CONSISTENCY IN CONSECUTIVE "YARDS_TO_GOAL" !!!
##  INSTEAD, we should make ALL OF IT - 
##    1. Yds_to_goal_end of the touchdown score
##    2. Both yardages of the penalty
##  - EQUAL to "YARDS_TO_GOAL" of  the NEXT PLAY (which is Kickoff, and equals 65 pretty much all the time..)

complement.ind <- ifelse(pbp[our.ind, "play_type"] == "Penalty" & 
                                 str_detect(tolower(pbp[our.ind-1, "play_type"]), "touchdown") &
                                 str_detect(tolower(pbp[our.ind+1, "play_type"]), "kickoff")
                               #& pbp[our.ind, "penalty_flag"]
                               , T, F)
complement.ind

if (length(our.ind[complement.ind]) > 0){
  pbp[our.ind[complement.ind]-1, "yards_to_goal_end"] <- pbp[our.ind[complement.ind]+1, "yards_to_goal"]
  pbp[our.ind[complement.ind], "yards_to_goal"] <- pbp[our.ind[complement.ind]+1, "yards_to_goal"]
  pbp[our.ind[complement.ind], "yards_to_goal_end"] <- pbp[our.ind[complement.ind]+1, "yards_to_goal"]
}




## For "Field Goal Good"s:
##    Some have "yards_to_goal_end" at 75 for some reason, instead of 65.. doesn't seem to be penalty-related either..
##    So for those are to FOLLOWED BY A CLASSIC "65": flipped them them to 65, OTHERS - REMOVE?? 

complement.ind <- ifelse(pbp[our.ind, "play_type"] == "Field Goal Good" & 
                           pbp[our.ind, "yards_to_goal_end"] == 75 & 
                           pbp[our.ind+1, "yards_to_goal"] == 65
                         #& pbp[our.ind, "penalty_flag"]
                         , T, F)

if (length(our.ind[complement.ind]) > 0) pbp[our.ind[complement.ind], ]$yards_to_goal_end <- 65



bad.ind <- ifelse(pbp[our.ind, "play_type"] == "Field Goal Good" & 
                           pbp[our.ind, "yards_to_goal_end"] == 75 & 
                           pbp[our.ind+1, "yards_to_goal"] != 65
                         #& pbp[our.ind, "penalty_flag"]
                         , T, F)

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% unique(pbp$game_id_drive[our.ind[bad.ind]]), ]
dim(pbp)

# View(pbp[sort(c( which(pbp$play_type == "Field Goal Good" & pbp$yards_to_goal_end == 75), which(pbp$play_type == "Field Goal Good" & pbp$yards_to_goal_end == 75)+1)), ] %>%
#               select(game_id_drive, yards_to_goal, yards_to_goal_end, penalty_flag, drive_result_detailed, play_type, play_text))
# table(pbp[which(pbp$play_type == "Field Goal Good")+1, "yards_to_goal"])

# bad.ind <- which(ifelse((pbp[, "yards_to_goal_end"] != lead(pbp[, "yards_to_goal"])) & 
#                                  pbp[, "play_type"] == "Field Goal Good", T, F))
# bad.ind <- ifelse(is.na(bad.ind), FALSE, bad.ind) # Takes care of "NA" cases
# bad.ind
# 
# 
# View(pbp[sort(c(bad.ind, bad.ind+1)), ] %>% 
#        #pbp %>% filter(game_id_drive %in% "400547641-14") %>%
#        select(game_id_drive, pos_team, def_pos_team, change_of_pos_team, yards_gained, yards_to_goal, yards_to_goal_end, penalty_flag, drive_result_detailed, play_type, play_text))




#######
## For "Punt":
##    If it's not equal to the next play's yards_to_goal AND it's equal to 65 => CHANGE IT
##    (Probably confuses it with something)
#######

complement.ind <- ifelse(pbp[our.ind, "play_type"] == "Punt" & pbp[our.ind,"yards_to_goal_end"] == 65,
                               T, F)
complement.ind
if (length(our.ind[complement.ind]) > 0)
  pbp[our.ind[complement.ind], "yards_to_goal_end"] <- pbp[our.ind[complement.ind]+1, "yards_to_goal"]



## For several TOUCHDOWN plays: 
##  Seem like there's a couple "3".. (mb attempted 2pt conversions, but not really.. no penalties either), so need to switch that to next play's yardage.
#  There are also some other weird ones with 75, which are ALWAYS followed by a TOUCHBACK KICKOFF => need to switch that to next play's yardage.
#
complement.ind <- ifelse(str_detect(tolower(pbp[our.ind, "play_type"]), "touchdown")
                               & ((pbp[our.ind, "yards_to_goal_end"] == 3) | (pbp[our.ind, "yards_to_goal_end"] == 75 & str_detect(tolower(pbp[our.ind+1, "play_text"]), "touchback"))),
                               T, F)
complement.ind
if (length(our.ind[complement.ind]) > 0)
  pbp[our.ind[complement.ind], "yards_to_goal_end"] <- pbp[our.ind[complement.ind]+1, "yards_to_goal"]
                                                            


# View(pbp[sort(c(our.ind[complement.ind], our.ind[complement.ind]+1)), ] %>%
#        #pbp %>% filter(game_id_drive %in% "400547764-8") %>%
#        select(game_id_drive, pos_team, def_pos_team, change_of_pos_team, yards_gained, yards_to_goal, yards_to_goal_end, penalty_flag, drive_result_detailed, play_type, play_text))



## For pass plays ("Pass Incompletion", "Pass Reception") & rushes ("Rush") - those are all just messed up.. no tendency noticeable. Gotta drop



######
## "Penalty" 
#######

####
##   On "NO PLAY" ones: there's a clear "100-X" flip happening from "yards_to_goal" to "yards_to_goal_end" of that observation,
##     WHILE FOR NEXT ONE it PROPERLY REVERTS BACK.
##     SO, we could do the check of: 
##           if (yards_to_goal == 100-yards_to_goal_end) & (yards_to_goal[ind+1] == 100-yards_to_goal_end),
##           then just set the "yards_to_goal_end" = yards_to_goal.
complement.ind <- ifelse(pbp[our.ind, "play_type"] == "Penalty" & pbp[our.ind, "penalty_no_play"] & 
                                 (pbp[our.ind, "yards_to_goal"] == 100-pbp[our.ind, "yards_to_goal_end"]) & (pbp[our.ind+1, "yards_to_goal"] == 100-pbp[our.ind, "yards_to_goal_end"])    
                               , T, F)
complement.ind
if (length(our.ind[complement.ind]) > 0)
  pbp[our.ind[complement.ind], "yards_to_goal_end"] <- pbp[our.ind[complement.ind], "yards_to_goal"]



######
## NEXT OBSERVATION'S "play_type" = PENALTY, penalty_no_play = TRUE, yards_to_goal = 0"
##   These all have faulty "yards_to_goal", which should be equated to "yards_to_goal_end" of the CURRENT OBS
######

complement.ind <- ifelse(pbp[our.ind+1, "play_type"] == "Penalty" & pbp[our.ind+1, "penalty_no_play"]  & pbp[our.ind+1, "yards_to_goal"] == 0 
                               , T, F)
complement.ind
if (length(our.ind[complement.ind]) > 0)
  pbp[our.ind[complement.ind]+1, "yards_to_goal"] <- pbp[our.ind[complement.ind], "yards_to_goal_end"]




####
## Checked various combinations:
##    our.ind, our.ind+1
##    0 & 100
####
# complement.ind <- which(ifelse(pbp[our.ind, "play_type"] == "Penalty"  & pbp[our.ind, "yards_to_goal_end"] == 100 
#                                , T, F))
# complement.ind





#######
## !!! "play_type != "Penalty", BUT "penalty_flag = TRUE" PLAYS !!
##  
##   ( Pure "Penalty" plays - (pbp[our.ind, "play_type"] == "Penalty") - actually seem QUITE ALRIGHT as far as 
##       GETTING THE "yards_to_goal" & "yards_to_goal_end" CORRESPONDING TO THE PLAY TEXT...)
## 
##     It's the "play_type != Penalty & penalty_flag == TRUE" that are the issue.. where there's SOMETHING ELSE BESIDES THE PENALTY in the play
##       (which leads to bad reading of the play text)
##
##   FIRST, cleaning out bad observations such that this is the FIRST PLAY in them:
##     WHAT TO DO??? NEED TO CHECK IF:
##           THE PRECEDING PLAY is NOT such an observations (play_type != Pen & penalty_flag = TRUE), 
##                  THEN WE ASSIGN ITS "yards_to_goal_end" to current obs "yards_to_goal"
##           THE FOLLOWING PLAY is NOT such an observations (play_type != Pen & penalty_flag = TRUE), 
##                  THEN WE ASSIGN ITS "yards_to_goal" to current obs "yards_to_goal_end"
##            OTHERWISE, it'll end up being dropped.

complement.ind <- ifelse(# pbp[our.ind, "play_type"] == "Penalty" 
  pbp[our.ind, "play_type"] != "Penalty" & pbp[our.ind, "penalty_flag"]
  # & pbp[our.ind+1, "penalty_flag"]
  , T, F)
complement.ind

if (length(our.ind[complement.ind]) > 0){
pbp[our.ind[complement.ind], "yards_to_goal"] <- ifelse(!(pbp[our.ind[complement.ind]-1, "play_type"] != "Penalty" & pbp[our.ind[complement.ind]-1, "penalty_flag"]),
                                                        pbp[our.ind[complement.ind]-1, "yards_to_goal_end"],
                                                        pbp[our.ind[complement.ind], "yards_to_goal"])

pbp[our.ind[complement.ind], "yards_to_goal_end"] <- ifelse(!(pbp[our.ind[complement.ind]+1, "play_type"] != "Penalty" & pbp[our.ind[complement.ind]+1, "penalty_flag"]),
                                                            pbp[our.ind[complement.ind]+1, "yards_to_goal"],
                                                            pbp[our.ind[complement.ind], "yards_to_goal_end"])
}




##   SECOND, cleaning out bad observations such that this is the SECOND PLAY in them:
complement.ind <- ifelse(# pbp[our.ind, "play_type"] == "Penalty" 
  pbp[our.ind+1, "play_type"] != "Penalty" & pbp[our.ind+1, "penalty_flag"]
  # & pbp[our.ind+1, "penalty_flag"]
  , T, F)
complement.ind

if (length(our.ind[complement.ind]) > 0){

pbp[our.ind[complement.ind]+1, "yards_to_goal"] <- ifelse(!(pbp[our.ind[complement.ind], "play_type"] != "Penalty" & pbp[our.ind[complement.ind], "penalty_flag"]),
                                                          pbp[our.ind[complement.ind], "yards_to_goal_end"],
                                                          pbp[our.ind[complement.ind]+1, "yards_to_goal"])

pbp[our.ind[complement.ind]+1, "yards_to_goal_end"] <- ifelse(!(pbp[our.ind[complement.ind]+2, "play_type"] != "Penalty" & pbp[our.ind[complement.ind]+2, "penalty_flag"]),
                                                              pbp[our.ind[complement.ind]+2, "yards_to_goal"],
                                                              pbp[our.ind[complement.ind]+1, "yards_to_goal_end"])
}
# View(# pbp[sort(c(our.ind[complement.ind])), ] %>%
#   # pbp[sort(c(our.ind[complement.ind], our.ind[complement.ind]-1, our.ind[complement.ind]+1)), ] %>%
#      pbp[sort(c(our.ind[complement.ind], our.ind[complement.ind]+1, our.ind[complement.ind]+2)), ] %>%
#     # pbp %>% filter(game_id_drive %in% c("401012248-18")) %>%
#     select(game_id_drive,  pos_team_score, def_pos_team_score, pos_team, def_pos_team, change_of_pos_team,
#            yards_gained, yds_rushed, yds_receiving, yds_penalty,
#            yards_to_goal, yards_to_goal_end, penalty_flag,  play_type, play_text))







## Kickoff issues are ALL AFTER A TOUCHBACK... although the "yards_to_goal_end" are properly at 75 for the Kickoff play itself..
# complement.ind <- which(ifelse(pbp[our.ind, "play_type"] == "Kickoff" & str_detect(tolower(pbp[our.ind, "play_text"]), "touchback")
#                                , T, F))
# complement.ind

##  TOUCHBACK KICKOFFS with "YARDS_TO_GOAL" of next play being at 25 ARE ALL BROKEN... THE SUBSEQUENT PLAYS GO IN THE WRONG DIRECTION.
##  WILL NEED TO JUST DROP THOSE LATER.
# complement.ind <- which(ifelse(pbp[our.ind, "play_type"] == "Kickoff" & str_detect(tolower(pbp[our.ind, "play_text"]), "touchback") & pbp[our.ind+1, "yards_to_goal"] == 25
#                                , T, F))
# complement.ind


## TOUCHBACK KICKOFFS with PENALTIES ON A SUBSEQUENT PLAY:
##  THOSE PENALTY-SUBSEQUENT-PLAYS seem to be TRULY THE ODD ONE OUT as far as YARDS_TO_GOAL values
##  => THE REST IS FINE.
##  Check "401056699-17", "401056697-25" ...
## 
## SO, we just need to change that SUBSEQUENT PLAY'S:
##   * "yards_to_goal" into the Kickoff's "yards_to_goal_end"
##   * "yards_to_goal_end" into the 3rd play's "yards_to_goal"

# complement.ind <- which(ifelse(pbp[our.ind, "play_type"] == "Kickoff" & str_detect(tolower(pbp[our.ind, "play_text"]), "touchback")
#                                &  pbp[our.ind+1, "penalty_flag"]
#                                , T, F))
# complement.ind
# pbp[our.ind[complement.ind]+1, "yards_to_goal"] <- pbp[our.ind[complement.ind], "yards_to_goal_end"]
# pbp[our.ind[complement.ind]+1, "yards_to_goal_end"] <- pbp[our.ind[complement.ind]+2, "yards_to_goal"]




# View(pbp[sort(c(our.ind[complement.ind], our.ind[complement.ind]+1, our.ind[complement.ind]+2)), ] %>%
#        select(game_id_drive,  pos_team_score, def_pos_team_score, pos_team, def_pos_team, change_of_pos_team, yards_gained, yards_to_goal, yards_to_goal_end, penalty_flag, drive_result_detailed, play_type, play_text))




####
## BETWEEN DRIVES
##  Adding the "(head(drive_number, length(drive_number) -1) != tail(drive_number, length(drive_number) -1))"
####


bad.drive.ids <- pbp %>%
  group_by(game_id_half) %>% 
  summarize(bad_drives = c(ifelse(((head(yards_to_goal_end, length(yards_to_goal_end)-1) < tail(yards_to_goal, length(yards_to_goal)-1) - yds.margin) |
                                     (head(yards_to_goal_end, length(yards_to_goal_end)-1) > tail(yards_to_goal, length(yards_to_goal)-1) + yds.margin)) 
                                  &
                                    (head(drive_number, length(drive_number) -1) != tail(drive_number, length(drive_number) -1)) , # If we want those JUST BETWEEN drives
                                  1,
                                  0), 0),
            .groups = "keep")   # SIMPLY HERE TO AVOID PRINTOUTS

## Sanity check of .groups = "keep":
# all.equal(bad.drive.ids.1, bad.drive.ids)
# all(bad.drive.ids.1$game_id_half == bad.drive.ids$game_id_half)
# all(bad.drive.ids.1$bad_drives == bad.drive.ids$bad_drives, na.rm = T)



## If we want to just focus on ALL THE CONSECUTIVE-YARDAGE ISSUES:
our.ind <- which(ifelse(bad.drive.ids$bad_drives == 1, T,F))
our.ind

## Meh, nothing specific that pertains to "changes between drives" could be picked up, so just leave as is..
complement.ind <- ifelse(pbp[our.ind, "change_of_pos_team"] == 1, T, F)

# View(pbp[sort(c(our.ind[complement.ind], our.ind[complement.ind]+1)), ] %>%
#        select(game_id_drive, pos_team, def_pos_team, change_of_pos_team, yards_gained, yards_to_goal, yards_to_goal_end, penalty_flag, drive_result_detailed, play_type, play_text))




######
## GETTING RID OF EVERYTHING INCONSISTENT UP TO THIS POINT:
######

# If we want to drop all that are "within yds.margin" as well
# bad.drive.ids <- pbp %>%
#   group_by(game_id_half) %>%
#   summarize(bad_drives =c(ifelse((head(yards_to_goal_end, length(yards_to_goal_end)-1) == tail(yards_to_goal, length(yards_to_goal)-1)),
#                                  0,
#                                  1), 0))

# If we are keepin around the stuff that's "within yds.margin"
bad.drive.ids <- pbp %>%
  group_by(game_id_half) %>%
  summarize(bad_drives =c(ifelse((head(yards_to_goal_end, length(yards_to_goal_end)-1) >= tail(yards_to_goal, length(yards_to_goal)-1) -yds.margin) &
                                   (head(yards_to_goal_end, length(yards_to_goal_end)-1) <= tail(yards_to_goal, length(yards_to_goal)-1) +yds.margin),
                                 0,
                                 1), 0),
            .groups = "keep")   # SIMPLY HERE TO AVOID PRINTOUTS

## Sanity check of .groups = "keep":
# all.equal(bad.drive.ids.1, bad.drive.ids)
# all(bad.drive.ids.1$game_id_half == bad.drive.ids$game_id_half)
# all(bad.drive.ids.1$bad_drives == bad.drive.ids$bad_drives, na.rm = T)


our.ind <- which(ifelse(bad.drive.ids$bad_drives == 1, T,F))
length(our.ind)
unique(bad.drive.ids[our.ind, ]$game_id_half)




### A. FIRST, CHECKING VARIOUS PLAY TYPES THAT STILL SHOW UP AMONG INCONSISTENT YARDAGES

#### FIRST PLAYS ("our.ind")

table(pbp[our.ind, "play_type"])


## ALMOST ALL THE PLAYS SEEM TO SUFFER from being followed by MESSILY-RECORDED PENALTIES...
##   AND IT'S TOUGH TO TRACK. 
##
## "Pass Incompletion" - if followed by penalty, that penalty play is messed up,
## "Pass Reception" - same stuff... etc etc
## 
# complement.ind <- which(ifelse(pbp[our.ind, "play_type"] == "Pass Reception" &
#                                  pbp[our.ind+1, "penalty_flag"]
#                                , T, F))
# complement.ind


#### SECOND PLAYS ("our.ind + 1")
##
## no tendency, all hit-n-miss:
##   "Field Goal Good", "Blocked Field Goal", "Field Goal Missed - some weirdness.. tough to pin down, doesn't appear to match with the play text either
##   "Kickoff" - clearly scoring play at the end of the previous drive is missing
##   "Pass Incompletion" - just bad data recording
##   "Penalty" - yards_to_goal values just get messed up, can't do much more than we already did
##   "Punt" - bad data recording.. some distances from which punts happen just make no sense
##   "Rushing Touchdown" - bad data recording.. some plays clearly missing
##   "Sack" - bad recording..

table(pbp[our.ind+1, "play_type"])

complement.ind <- which(ifelse(pbp[our.ind+1, "play_type"] == "Sack"
                               , T, F))

# View(pbp[sort(c(our.ind[complement.ind], our.ind[complement.ind]+1)), ] %>%
#        select(game_id_drive,  pos_team_score, def_pos_team_score, pos_team, def_pos_team, change_of_pos_team, yards_gained,yds_penalty, yards_to_goal, yards_to_goal_end, penalty_flag, drive_result_detailed, play_type, play_text))






### B. NOW, ACTUALLY GETTING RID OF STUFF.

dim(pbp)
length(unique(pbp$game_id_half))

pbp <- pbp[!pbp$game_id_half %in% unique(bad.drive.ids[our.ind, ]$game_id_half),] 

dim(pbp)
length(unique(pbp$game_id_half))








####
## YARDS_TO_GOAL/YARDS_TO_GOAL_END = 100
## YARDS_TO_GOAL/YARDS_TO_GOAL_END = 0
####

## First, double-checking the CONSECUTIVE 0/100 values 
## (which we technically removed prior to the MAIN cleaning in stage 5, 
##  but might've mistakenly got some of those back in)
##
##  NONE REALLY.

bad.ind <- which(pbp$yards_to_goal_end %in% c(0, 100) & lead(pbp$yards_to_goal) %in% c(0,100))

#print("Number of consecutive 0/100 yardages (perfectly should be 0):")
#print(length(unique(pbp$game_id_half[bad.ind])))

# View(pbp[sort(c(bad.ind, bad.ind+1)), ] %>%
#   #pbp %>% filter(game_id_drive %in% "400547916-1") %>%
#     select(game_id_drive, pos_team, def_pos_team, change_of_pos_team, yards_gained, yards_to_goal, yards_to_goal_end, penalty_flag, play_type, play_text))


## REMOVING those bad observations

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% unique(pbp$game_id_half[bad.ind]), ]
dim(pbp)





##   yards_to_goal == 100, NO cases right now, but before we had stuff like that:
##      If change_of_pos_team == 0 ON PREVIOUS PLAY (just one such case), then it's a LEGIT CASE, e.g. kickoff downed at 0
##      If change_of_pos_team == 1 ON PREVIOUS PLAY (several such scenarios), then it's a RECORDING ERROR THAT IS TOUGH TO FIX, SO GOTTA DROP THOSE

# bad.ind <- which(pbp$yards_to_goal == 100 & lag(pbp$change_of_pos_team) == 0)
# bad.ind

bad.ind <- which(pbp$yards_to_goal == 100 & lag(pbp$change_of_pos_team) == 1)
bad.ind

bad.halves <- unique(pbp$game_id_half[bad.ind])
bad.halves

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% bad.halves, ]
dim(pbp)

##   yards_to_goal_end == 100
##     Just a couple values, either:
##         * "within 1yd" of the next one that's more reliable (99)
##
bad.ind <- which(pbp$yards_to_goal_end == 100)
bad.ind

## BAD OBSERVATIONS:
#print("potentially problematic (within 2yds), not removing")
#print(length(bad.ind))


# bad.ind <- which(pbp$yards_to_goal == 0 & pbp$penalty_flag)
bad.ind <- which(pbp$yards_to_goal == 0)
bad.ind

## BAD OBSERVATIONS:
#print("potentially problematic (within 2yds), not removing")
#print(length(bad.ind))


# View(pbp[sort(c(bad.ind, bad.ind+1, bad.ind-1)), ] %>%
#      #  pbp %>% filter(game_id_drive %in% c("400548335-14", "400548335-15")) %>%
#        select(game_id_drive, pos_team, def_pos_team, change_of_pos_team, yards_gained, yards_to_goal, yards_to_goal_end, penalty_flag, drive_result_detailed, play_type, play_text))




##   yards_to_goal_end == 0, NO such cases right now (the "End of Half/Game" stuff will be filled out shortly after this)
##      BUT some of the problematic cases prior were:
##        "400548127-26" - offsetting penalties just create two "0" for yards_to_goal/yards_to_goal_end.. plus next yards_to_goal
##        "400548335-14" - punt out of bounds.. just not possible to catch


# bad.ind <- which(pbp$yards_to_goal_end == 0 & (!lead(pbp$play_type) %in% c("End of Half", "End of Game")))

bad.ind <- which(pbp$yards_to_goal_end == 0)
bad.ind

#print("potentially problematic, not removing")
#print(length(bad.ind))

####
## YARDS_TO_GOAL/YARDS_TO_GOAL_END = 3 
## on POST-TOUCHDOWN PLAYS
##
## These seem to be just mess-ups.. some from failed 2pt/xpt, but others have no reason. JUST FLIP IT + THE NEXT "yards_to_goal" to 65
##
bad.ind <- which(str_detect(tolower(pbp$play_type), "touchdown") & (pbp$yards_to_goal_end == 3))
bad.ind

if (length(bad.ind) > 0){
    pbp[bad.ind, ]$yards_to_goal_end <- 65
    pbp[bad.ind+1, ]$yards_to_goal <- 65
}





####
## YARDS_TO_GOAL_END OF PREV PLAY is WITHIN 2 YDS of NEXT PLAY'S YARDS_TO_GOAL,
##  BUT NOT EQUAL TO IT
####

bad.drive.ids <- pbp %>%
  group_by(game_id_half) %>%
  summarize(bad_drives =c(ifelse((head(yards_to_goal_end, length(yards_to_goal_end)-1) == tail(yards_to_goal, length(yards_to_goal)-1)),
                                 0,
                                 1), 0), .groups = "keep")   # SIMPLY HERE TO AVOID PRINTOUTS

## Sanity check of .groups = "keep":
# all.equal(bad.drive.ids.1, bad.drive.ids)
# all(bad.drive.ids.1$game_id_half == bad.drive.ids$game_id_half)
# all(bad.drive.ids.1$bad_drives == bad.drive.ids$bad_drives, na.rm = T)




our.ind <- which(ifelse(bad.drive.ids$bad_drives == 1, T,F))
length(our.ind)
unique(bad.drive.ids[our.ind, ]$game_id_half)

# Most typical first & second play types
table(pbp[our.ind, "play_type"])
table(pbp[our.ind+1, "play_type"])


## For FIELD GOAL ATTEMPTS: there's constantly that SHIFT in "yards_to_goal" by 1-2 yards from the "yards_to_goal_end" of the previous play..
##  PREVIOUS PLAY SEEMS MORE RELIABLE/CONTINUOUS, SO COULD JUST PLUG THAT IN THERE...
##   !!! AS LONG AS WE DON'T USE "FG MADE" DISTANCE as a "SANITY CHECK" MEASURE (where it's "17 + yards_to_goal") !!!

# Checking cases where the PLAY PRECEDING THE FIELD GOAL ATTEMPT did NOT have PROPER ALIGNMENT OF YARDS_TO_GOAL & YARDS_GAINED VALUES
#  Only 1 value like that, and even then it's a FUMBLE recovery, which is always A BIT IFFY on that CONSISTENCY UNTIL LATER PARTS
#  SO IT'S STILL OK to REPLACE WITH ITS "YARDS_TO_GOAL END" as opposed to "YARDS_TO_GOAL" of a FG ATTEMPT PLAY
complement.ind <- ifelse(str_detect(pbp[our.ind+1, "play_type"], "Field Goal") & (pbp[our.ind, "yards_to_goal"] - pbp[our.ind, "yards_to_goal_end"] != pbp[our.ind, "yards_gained"]), T, F)
complement.ind

complement.ind <- ifelse(str_detect(pbp[our.ind+1, "play_type"], "Field Goal"), T,F)
complement.ind

if (length(our.ind[complement.ind]) > 0)
  pbp[our.ind[complement.ind]+1, "yards_to_goal"] <- pbp[our.ind[complement.ind], "yards_to_goal_end"]


# View(pbp[sort(c(our.ind[complement.ind], our.ind[complement.ind]+1)), ] %>%
#   #pbp %>% filter(game_id_drive %in% c("400548335-14", "400548335-15")) %>%
#     select(game_id_drive, pos_team, def_pos_team, change_of_pos_team, yards_gained, rush_yds, reception_yds, yds_rushed,yds_receiving, yards_to_goal, yards_to_goal_end, penalty_flag, drive_result_detailed, play_type, play_text))


##   FOR PUNTS: 
#     it's CLEARLY A CONSISTENT ERROR of "YARDS_TO_GOAL_END" OF THE PUNT PLAY being BADLY RECORDED compared to "YARDS_TO_GOAL" OF SUBSEQUENT PLAY..
#            (even the PUNT'S PLAY TEXT confirms it)..
#           WITH THAT SAID, I WANT TO FULLY MAKE SURE THAT THIS VALUE IS CORRECT, so I'LL ALSO BE VERIFYING THAT IT IS EQUAL TO:
#                (100 - (yards_to_goal-yds_punted) - yds_punt_return)
#           => and THEN assign THAT "YARDS_TO_GOAL" OF THE SECOND ONE

complement.ind <- ifelse(pbp[our.ind, "play_type"] == "Punt" 
                               & (100-(pbp[our.ind,"yards_to_goal"] - pbp[our.ind,"yds_punted"])-pbp[our.ind,"yds_punt_return"]) == pbp[our.ind+1, "yards_to_goal"]
                               , T, F)
complement.ind
if (length(our.ind[complement.ind]) > 0)
  pbp[our.ind[complement.ind], "yards_to_goal_end"] <- pbp[our.ind[complement.ind]+1, "yards_to_goal"]



### CHECKING OTHER PLAY TYPES for the FIRST PLAY:
#
#   "Penalty" - everyone matches their description (with penalty yards, yards gained, etc..)... can't even pick
#   "Pass Incompletion" - this is tough to pursue: the play BEFORE incompletion appears reliable, and the play AFTER appears reliable.. 
#          both resulting in a CONFLICTING YARDAGE for "yards_to_goal" and "yards_to_goal_end" of the incompletion, WHICH SHOULD BE EQUAL actually..
#          would probably have to leave it AS IS...
#   "Rush" - everyone either seemingly matches their description (with play text, yards gained, etc..), or there are clear-cut errors based on play text,
#         BUT THOSE ARE TOUGH TO CATCH SYSTEMATICALLY...
#   "Pass Reception" - even though the SECOND PLAY seems mostly on point, sometimes BOTH PLAYS HAVE THEIR CONSISTENT DATA (based on play text, yards gained, etc..)
#                 so CAN'T REALLY GO AFTER IT SYSTEMATICALLY

table(pbp[our.ind, "play_type"])

complement.ind <- which(ifelse(pbp[our.ind, "play_type"] == "Pass Reception" 
                               #& pbp[our.ind, "change_of_pos_team"] == 0
                               , T, F))
# pbp[our.ind[complement.ind], "yards_to_goal_end"] <- pbp[our.ind[complement.ind]+1, "yards_to_goal"]

complement.ind

# View(pbp[sort(c(our.ind[complement.ind], our.ind[complement.ind]+1)), ] %>%
#        #pbp %>% filter(game_id_drive %in% c("400548335-14", "400548335-15")) %>%
#        select(game_id_drive, pos_team, def_pos_team, change_of_pos_team, yards_gained, yds_rushed,yds_receiving, yds_penalty, yds_punted, yds_punt_return, yards_to_goal, yards_to_goal_end, penalty_flag, drive_result_detailed, play_type, play_text))


### CHECKING OTHER PLAY TYPES for the SECOND PLAY:
##    "Fumble Recovery (Opponent)" (3 plays) - the first play is more reliable yards_to_goal'wise, but both have their proper "yards_gained"... shouldn't pursue
##    "Interception Return" (5 plays) - meh, tough to gauge a tendency
##    "Pass Incomplettion" (17 plays) - not much of a tendency
##    "Pass Reception" (22) - no tendency
##    "Passing Touchdown" (3) - no tendency
##    "Punt" (5) - the second play is mostly more reliable, but there's also one pass incompletion preceding it that also made sense from play text.. won't pursue

table(pbp[our.ind+1, "play_type"])

complement.ind <- which(ifelse(pbp[our.ind+1, "play_type"] == "Punt" 
                               #& pbp[our.ind, "change_of_pos_team"] == 0
                               , T, F))
# pbp[our.ind[complement.ind], "yards_to_goal_end"] <- pbp[our.ind[complement.ind]+1, "yards_to_goal"]

complement.ind

# View(pbp[sort(c(our.ind[complement.ind], our.ind[complement.ind]+1)), ] %>%
#        #pbp %>% filter(game_id_drive %in% c("400548335-14", "400548335-15")) %>%
#        select(game_id_drive, pos_team, def_pos_team, change_of_pos_team, yards_gained, yds_rushed,yds_receiving, yds_penalty, yds_punted, yds_punt_return, yards_to_goal, yards_to_goal_end, penalty_flag, drive_result_detailed, play_type, play_text))




########
## 6. CORRECTING SOME YARDAGES for PRE-"END OF HALF" PLAYS
##    BUT THEN.. break it up into (KEEPING IN MIND THAT THE POSSESSION DOES NOT TURN OVER TO THE OTHER TEAM.. as IT'S THE END OF THE HALF)
##      1. MOVED TO AFTER THE "YARDS_GAINED" GOT FIXED UP... Pure plays - just rushes/passes/sacks etc, NON-SCORING PLAYS, for which we can still derive the legitimate YARDS TO GOAL END, by taking YARDS_TO_GOAL + YARDS_GAINED
##      2. Missed FG - the yds to goal_END is THE SAME as YARDS_TO_GOAL (the possession)
##      3. Offensive score - 0 YARDS REMAINING till the END ZONE/GOAL...
##      4. Defensive score - 100 YARDS REMAINING till the END ZONE/GOAL...
##   THE REST (PUNTS, FUMBLES, KICKOFF RETURNS, ETC) ARE TOO COMPLICATED TO MESS WITH... STAY WITH "NA"
########



fg.miss.ind <- which((str_detect(tolower(lead(pbp$play_type)), "end of half") | str_detect(tolower(lead(pbp$play_type)), "end of game"))
                     & pbp$play_type %in% c("Field Goal Missed"))
if (length(fg.miss.ind) > 0) pbp[fg.miss.ind, c("yards_to_goal_end")] <- pbp[fg.miss.ind, "yards_to_goal"] 


off.score.ind <- which((str_detect(tolower(lead(pbp$play_type)), "end of half") | str_detect(tolower(lead(pbp$play_type)), "end of game"))
                       & pbp$play_type %in% c("Field Goal Good", "Passing Touchdown", "Rushing Touchdown", "Kickoff Return Touchdown", "Punt Return Touchdown", "Blocked Punt Touchdown", "Blocked Punt (Safety)"))
if (length(off.score.ind) > 0) pbp[off.score.ind, c("yards_to_goal_end")] <- 0

def.score.ind <- which((str_detect(tolower(lead(pbp$play_type)), "end of half") | str_detect(tolower(lead(pbp$play_type)), "end of game"))
                       & pbp$play_type %in% c("Interception Return Touchdown", "Blocked Field Goal Touchdown", "Fumble Return Touchdown", "Fumble Recovery (Opponent) Touchdown", "Safety", "Punt Team Fumble Recovery Touchdown"))
if (length(def.score.ind) > 0) pbp[def.score.ind, c("yards_to_goal_end")] <- 100



########
## CUTOFF for the 2014-2021 CHECK
########


# cat("\n")
# cat("\n")
# print("DATA SET SIZE AFTER EARLY CLEANING")
# print(dim(pbp))
# print(length(unique(pbp$game_id_half)))
# cat("\n")
# cat("\n")

########


# }






############
## 8p5. LAST PLAY OF DRIVE != DRIVE_RESULT_DETAILED
############

bad.play.drive.match <- pbp %>% 
  mutate(play_type = ifelse(play_type == "End of Game", "End of Half", play_type)) %>%
  group_by(game_id_drive) %>%
  summarize(mismatch = (tail(play_type, 1) != tail(drive_result_detailed, 1)),
            good.downs.TO = (tail(drive_result_detailed, 1) == "Downs Turnover") & (tail(down,1) == 4),
            end.of.half.play = (tail(play_type,1) == "End of Half"),
            end.of.half.drive = (tail(drive_result_detailed,1) == "End of Half"),
            punt.fumb.recov = (tail(play_type,1) == "Punt Team Fumble Recovery"),
            onside.kick.recov = (tail(drive_result_detailed, 1) == "On-Side Kick Lost"),
            penalty.play = (tail(play_type,1) == "Penalty"),
            scoring.play = (tail(scoring_play,1) == 1),
            # unsportsmanlike = (str_detect(tolower(tail(play_text,1)), "unsportsmanlike")),
            penalty.no.play = tail(penalty_no_play,1)) %>%
  ## For PENALTY-ending drives:
  #   filter(mismatch & !good.downs.TO & !end.of.half.play & !punt.fumb.recov & !onside.kick.recov & penalty.play & penalty.no.play & !scoring.play) %>%
  ## For NON-PENALTY STUFF:
  ##   END-OF-HALF, with PENALTIES:
  # filter(mismatch & !good.downs.TO & !end.of.half.play & !punt.fumb.recov & !onside.kick.recov & penalty.play & end.of.half.drive) %>%
  ##   END-OF-HALF, withOUT PENALTIES:
  filter(mismatch & !good.downs.TO & !end.of.half.play & !punt.fumb.recov & !onside.kick.recov & !penalty.play & end.of.half.drive) %>%
  select(game_id_drive) %>% .[[1]]

bad.play.drive.match


## 8p5.A PENALTY-ENDING drives:
#
# penalty_no_play = TRUE => typically either:
#        a penalty on a SUBSEQUENT KICKOFF, which still counts as part of previous drive for some reason...
#        or a penalty on a PUNT, screwing with the subsequent punt attempt, and ALSO POTENTIALLY NOT HAVING "PUNT" as its DRIVE RESULT..
#  => I ended up DROPPING the PUNT-problematic halves, while SHIFTING THE KICKOFF PLAY to the NEXT DRIVE.
#    !! IF "scoring.play = 1", then it could be A SAFETY - so KEEP THESE!!
#
#
#  Some specific drives & comments:
#   "400609076-10", "400609076-11" - unclear false start penalty.. can't see its impacts at all
#   "400548454-3", "400547760-23", "400550419-16", "400547949-25" - unsportsmanlike conduct -15yds enforced on kickoff (spot moved back from 35 to 20yd)
#   "400548167-12" - SAFETY via PENALTY
#   "400548163-3" - penalty during punt (the drive result didn't say "Punt" either, instead having "Rushing TD" - which is the result of the subsequent drive),
#                   "illegal kicking", penalty_no_play=TRUE
#   "400547785-6" - unsportsmanlike, but 0yds for some reason..
#   "400548440-24" - penalty on the subsequent kickoff, still treating it as if it's a part of the previous possession.. KIND OF AN ERROR? TRY DIAGNOSE IT?
#   "400548431-13", "400548288-8" - penalty on the punt (illegal formation, holding), makes the subsequent punt attempt into its own "drive"...
##                  ERROR, BUT MAY BE SYSTEMATICALLY FIXABLE? E.g. via "penalty_no_play = TRUE"????
#   "400548360-12", "400547921-3 - PERSONAL FOUL (same result as unsportsmanlike - moving the kickoff point)
#   "400548284-12" - penalty AFTER the fair catch on a punt, drive number doesn't change, but the DRIVE RESULT for some reason flips over to the next drive (e.g. instead of "Punt" it becomes "Rushing TD")
#   "400547921-7", "400548204-23" - "Offensive Offside", 5yds enforced on the kickoff spot
#   "400547823-18" - facemask, enforced on the kickoff spot


####
## penalty_no_play = TRUE case =>
##    8p5.A1 PENALTIES ON PUNTS: 
##        If drive continues (the dataset doesn't change the possession) - we need to make that "PENALTY PUNT" PLAY into a SPECIAL TEAMS POSSESSION.. MOVING THAT TASK DOWN to the "PUNT DRIVE RESHUFFLING" stages
##        SO EXCLUDING THOSE CASES FOR NOW (!"1st down" & !penalty_no_play)
##        
##        For NON-1st DOWN PUNT PENALTIES, hence where the PUNTING TEAM HAD TO RE-DO IT, or where THE PLAY CALL CHANGED (e.g. they decided NOT to punt after getting a penalty)
##          - ALL OBSERVATIONS ARE VASTLY GOOD (keeping the drive # the same for both punt attempts, the drive outcome is "Punt", or "Downs Turnover" on one of them due to changed play call), 
##           but will NEED TO KEEP THAT IN MIND when RE-NUMBERING DRIVES for PUNTS (to also count the PENALTY attempt as part of the next drive, not just the successful punt)
##          - There are a couple MISHAPS with DRIVE RESULT stuff at times... gotta make it as follows:
##              If the LAST PLAY of your drive is a PUNT PENALTY, then.. I'LL JUST DROP THOSE.

punt.penalty.drives <- pbp %>% 
  filter(str_detect(tolower(play_text), "punt") & !(str_detect(tolower(play_text), "1st down") & pbp$penalty_no_play) & play_type == "Penalty") %>%
  select(game_id_drive) %>% .[[1]]

last.play.punt.penalty.drives <- pbp %>% 
  group_by(game_id_drive) %>%
  summarize(last.play.punt.penalty = str_detect(tolower(tail(play_text,1)), "punt") & tail(play_type,1) == "Penalty") %>%
  filter(last.play.punt.penalty) %>%
  select(game_id_drive) %>% .[[1]]

# "400548288-8", "400548163-3"

# "400548288-8" - the case where the 2nd punt attempt becomes its own drive.
# "400548163-3" - the case where the penalty was actually resulting in a LOSS OF DOWN and TURNOVER OF THE POSSESSION... hmm...
#....
## JUST DROP THOSE

last.play.punt.penalty.halves <- unique(pbp$game_id_half[pbp$game_id_drive %in% last.play.punt.penalty.drives])
last.play.punt.penalty.halves

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% last.play.punt.penalty.halves, ]
dim(pbp)


# View(pbp %>%
#       # filter(game_id_drive %in% c("400548163-3", "400548163-4")) %>%
#       # filter(game_id_drive %in% last.play.punt.penalty.drives) %>%
#        filter(game_id_drive %in% punt.penalty.drives) %>%
#        select(game_id_drive,
#               pos_team, def_pos_team, drive_number, change_of_pos_team, drive_result_detailed, play_type, play_text,
#               down,
#               yards_to_goal, yards_to_goal_end,
#               pos_team_score, def_pos_team_score, clock.minutes, clock.seconds,
#               # first_by_penalty, firstD_by_penalty,
#               penalty_declined, penalty_flag, penalty_no_play, yds_penalty, penalty_play_text, penalty_text))



####
## penalty_no_play = TRUE case =>
##    8p5.A2 PENALTIES ON KICKOFFS:
##      For penalties on kickoffs, which for some reason get counted towards PRECEDING DRIVE.... 
##      SHIFT those towards the NEXT DRIVE, making them a "STARTING PLAY", but MAKE SURE TO REMEMBER THAT THIS WOULD START SOME DRIVES with a "PENALTY" play, NOT with a "KICKOFF"

kickoff.penalty.drives <- pbp %>% 
  filter(str_detect(tolower(play_text), "kickoff") & play_type == "Penalty") %>%
  select(game_id_drive) %>% .[[1]]

kickoff.penalty.plays <- which(str_detect(tolower(pbp$play_text), "kickoff") & pbp$play_type == "Penalty")
pos_team.vars <- colnames(pbp)[(str_detect(colnames(pbp), "pos_") | str_detect(colnames(pbp),"_pos")) & !str_detect(colnames(pbp), "change") & !str_detect(colnames(pbp), "by_poss")]
drive.vars <- colnames(pbp)[str_detect(colnames(pbp), "drive")]

if (length(kickoff.penalty.plays) > 0){
  ##    copy from the next row: pos_team, def_pos_team, pos_team_score, def_pos_team_score, DRIVE VARIABLES (bar the drive_play_number) ...
  pbp[kickoff.penalty.plays, c("drive_number",  "drive_result_detailed", pos_team.vars)] <- pbp[kickoff.penalty.plays+1, c("drive_number",  "drive_result_detailed", pos_team.vars)]
  ##    make "drive_play_number" a 0.5
  pbp[kickoff.penalty.plays, "drive_play_number"] <- 1
}


pbp$game_id_drive <- paste(pbp$game_id, pbp$drive_number, sep="-")
pbp$game_id_drive <- factor(pbp$game_id_drive, levels = unique(pbp$game_id_drive))



# View(pbp %>%
#        #filter(game_id_drive %in% c("400547729-1", "400547729-2", "400547729-3", "400547729-4")) %>% 
#        filter(game_id_drive %in% kickoff.penalty.drives) %>% 
#        select(game_id_drive,
#               # drive_play_number, 
#               pos_team, def_pos_team, drive_number, change_of_pos_team, drive_result_detailed, play_type, play_text,
#               down,
#               yards_to_goal, yards_to_goal_end,
#               pos_team_score, def_pos_team_score, clock.minutes, clock.seconds,
#               # first_by_penalty, firstD_by_penalty, 
#               penalty_declined, penalty_flag, penalty_no_play, yds_penalty, penalty_play_text, penalty_text))

####
#  penalty_no_play = FALSE =>
#     8p5.A2 !!! JUST KEEPING THESE. !!!
#   typically PRETTY GOOD on having the legitimate stuff to be enforced on the kickoff (unsportsmanlike, personal foul, offsides, etc etc),
#                            Has some WEIRD ones, where it says "2yd penalty to a 1yd line", but it doesn't show up really anywhere.. COULD LEAVE THOSE IN THOUG
#                            Some stuff probably enforced at the EXTRA POINT KICK, but it BARELY SHOWS UP.. so could LEAVE THAT IN
####


####
# PUNT TEAM FUMBLE RECOVERY:
#  "Punt Team Fumble Recovery" - always leads to CHANGE IN THE DRIVE NUMBER, but NO CHANGE IN THE POSSESSING TEAM..
#                                PLUS the DRIVE RESULT is the SAME FOR THE TWO "DRIVES" THAT ARE GETTING CONNECTED this way..
#                               (we will SPLIT THOSE UP though later, when making "Punt" plays roll over to the next possession, or making them a 1-play drive)

####
# END OF HALF, DOWNS TURNOVER, ON-SIDE KICK
#   "End of Half", "Turnover on Downs", "On-Side Kick Lost" - won't correspond with the last play in many cases



#####
## 8p5.C NON-PENALTY ENDING DRIVES:
##  

## 8p5.C1
##    CHECK FOR CONSECUTIVE DRIVES ENDING IN "END OF HALF" !!!
##    DROP THOSE FOR THE MOST PART (only ones left would be "Punt Team Fumble Recovery", where "End Half" spans two consecutive "drives" by the same team)
#
#  "400547846-2" - just creates an extra 'drive', where possession switches but it's just "End of Half" again...
#  "400548267-1" - One of these is the DRIVE THAT KEPT GOING UNDER "PUNT TEAM FUMBLE RECOVERY"... which we will be CHANGING
#  "400547695-26" - turnover on downs is missed in drive result, ERROR
#  "400548070-1" - just a fully messed up incomplete game? Only has 1 drive pretty much...
#
# Finding all observations such that:
#   They have "End of Half" as the drive result;
#   Their LEAD drive result is also "End of Half",
#   The DRIVE NUMBER changes,
#   It's NOT a "Punt Team Fumble Recovery" - which is something we'll tackle later
# !! DROP THEM !!

end.of.half.bad.halves <- unique(pbp$game_id_half[pbp$drive_result_detailed == "End of Half" & lead(pbp$drive_result_detailed) == "End of Half" 
                                                  & pbp$drive_number != lead(pbp$drive_number)
                                                  & pbp$play_type != "Punt Team Fumble Recovery"])
#& (pbp$pos_team != lead(pbp$pos_team))])
end.of.half.bad.halves <- end.of.half.bad.halves[!is.na(end.of.half.bad.halves)]

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% end.of.half.bad.halves, ]
dim(pbp)


## 8p5.D CHECKING FOR "End of Half" plays that are NOT THE LAST PLAY OF THEIR RESPECTIVE DRIVE:

end.of.half.drives <- unique(pbp$game_id_drive[pbp$play_type == "End of Half"])

## 8p5.D1. DRIVES WITH PENALTY "AFTER THE END OF HALF", TO BE IMPOSED TO THE NEXT HALF'S KICKOFF:
#
# => JUST DROP THOSE PLAYS.. THE "YARDS_TO_GOAL" ON THE KICKOFF REFLECTS THE IMPACT OF THE PENALTY ANYWAY
# More detail:


end.of.half.penalty.play <- pbp %>% filter(game_id_drive %in% end.of.half.drives) %>%
  group_by(game_id_drive) %>%
  summarize(end.of.half.penalty = (tail(play_type,1) == "Penalty")) %>%
  filter(end.of.half.penalty) %>% .[[1]]

bad.ind <- which(pbp$game_id_drive %in% end.of.half.drives & pbp$play_type == "Penalty" & pbp$drive_number != lead(pbp$drive_number))
bad.ind

dim(pbp)
if (length(bad.ind) > 0) pbp <- pbp[-bad.ind, ]
dim(pbp)

## 8p5.D2. Other "End of Half" issues.
#
#  400547995-13 - flipped order of the "End of Half" and the actual final play
#  !! 400547962-12 - flipped order, BUT... A TOUCHDOWN AS THE HALF EXPIRES, where "End of Half" was the drive result for ALL BUT THAT "LAST PLAY"... !!
# 400547698-15 - flipped order, but also INTERCEPTION RETURN as the half expired, where "End of Half" was the drive result for ALL BUT THAT "LAST PLAY"... !!

end.of.half.not.last.play.drives <- pbp %>% filter(game_id_drive %in% end.of.half.drives) %>%
  group_by(game_id_drive) %>%
  summarize(end.of.half.last.play = (tail(play_type,1) == "End of Half")) %>%
  filter(!end.of.half.last.play) %>% .[[1]]


## Need to CHANGE YARDS_TO_GOAL from NA to 65 for the TOUCHBACKs that are AFTER "END OF HALF"

tb.after.eof <- which(is.na(pbp$yards_to_goal) & str_detect(tolower(pbp$play_text), "touchback") &
        str_detect(tolower(lag(tolower(pbp$play_type))), "end of"))  # !str_detect(lead(tolower(pbp$play_type)), "end of") & 

if (length(tb.after.eof) > 0) pbp$yards_to_goal[tb.after.eof] <- 65


# View(pbp %>%
#        #filter(game_id_drive %in% c("400547675-11", "400547675-12", "400547675-13")) %>%
#       # filter(game_id_drive %in% c("400548000-18", "400548000-19")) %>%
#        #filter(game_id_drive %in% end.of.half.not.last.play.drives) %>%
#         filter(game_id_drive %in% end.of.half.penalty.play) %>%
#        select(game_id_drive,
#               pos_team, def_pos_team, drive_number, change_of_pos_team, drive_result_detailed, # lead_drive_result_detailed,
#               play_type, play_text,
#               down,
#               yards_to_goal, yards_to_goal_end,
#               pos_team_score, def_pos_team_score, clock.minutes, clock.seconds,
#               penalty_declined, penalty_flag, penalty_no_play, yds_penalty, penalty_play_text, penalty_text))


#  ALL THESE CAN BE TACKLED VIA LOOKING AT DRIVES WITH >1 "DRIVE_RESULT_DETAILED" & FIXING IT
#  WE'RE DOING THAT BELOW

#######
## 8p5.D2A
## Checking inconsistencies with drive_result_detailed: 
##         POSSESSIONS WITH >1 DRIVE RESULT 
#####

bad.drive.no <- pbp %>% 
  group_by(game_id_drive) %>% 
  summarise(n_drive_res = length(unique(drive_result_detailed))) %>%
  filter(n_drive_res > 1 | n_drive_res == 0)

bad.drive.no

table(bad.drive.no$n_drive_res)


## TWO OUTCOMES:
##      1. If one is NA, pick the other one.
##           Some Kickoffs have "NA" drive result next to it for some reason (while the rest is fine)
##           Some penalties do as well..
##      2. If none are NA, but one is "End of Half", pick the other one.
##           This is MOST OF THE CASES: There's an "End of Half" drive, that actually ends with a SCORING PLAY or a TURNOVER/MISSED FG..
##          That play gets a PROPER "DRIVE RESULT", while the REST OF THE DRIVE it keeps showing "END OF HALF", which ISN'T AS INFORMATIVE...
##      3. If none are NA or "End of Half" - DROP THOSE

two.drive.outcomes.id <- bad.drive.no %>% 
  filter(n_drive_res == 2) %>%
  select(game_id_drive) %>% 
  .[[1]]


## Checking the drives where we got TWO DISTINCT NON-NA/NON-"END OF HALF" DRIVE RESULTS
#    While some issues MIGHT be fixable by just assigning the LATTER drive name.. MOSTLY !! These are JUST BAD ERRORS... GOTTA REMOVE those HALVES !!!
#       "400547787-14" - "Fumble Recovery (Opponent)", "Fumble Return Touchdown", should be the latter, but for some reason it created TWO PLAYS for the fumble and the TD return...
#       "400547671-25", "400548349-4" - "Passing Touchdown", "Rushing Touchdown", should be latter, BUT.. it also calls a basic pass completion a "PASSING TD" PLAY TYPE (not just drive result).. SO BETTER REMOVE THAT CRAP...
#       "400548284-12" - PENALTY after the punt, which for some reason counts into the previous possesion WHILE the DRIVE RESULT SWITCHES OVER TO THE NEXT POSSESSSION
#       "400548237-20", "400548306-18"- says "DOWNS TURNOVER", although there was actually a PENALTY on the unsuccessful 4th down attempt, followed by a PUNT which counted towards this possession..
#       "400548319-13" - has a PUNT OUT-OF-BOUNDS, but then THE SAME TEAM KEEPS DRIVING THE BALL UNTIL A RUSHING TD... MAKES NO SENSE
#    !! These are JUST BAD ERRORS... GOTTA REMOVE those HALVES !!!

two.drive.outcomes.non.na.non.end.half.id <- pbp %>% filter(game_id_drive %in% two.drive.outcomes.id) %>% 
  group_by(game_id_drive) %>%
  summarize(na.drive = any(is.na(drive_result_detailed)),
            end.of.half.drive = any(drive_result_detailed == "End of Half")) %>%
  filter(!na.drive & !end.of.half.drive) %>% .[[1]]

two.drive.outcomes.non.na.non.end.half.HALVES <- unique(pbp$game_id_half[pbp$game_id_drive %in% two.drive.outcomes.non.na.non.end.half.id])
two.drive.outcomes.non.na.non.end.half.HALVES

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% two.drive.outcomes.non.na.non.end.half.HALVES,]
dim(pbp)


# View(pbp %>%
#       # filter(game_id_drive %in% c("400548319-13", "400548319-14")) %>% 
#        # filter(game_id_half %in% c("400548070-1")) %>% 
#        #filter(game_id_drive %in% end.of.half.not.last.play.drives) %>% 
#        filter(game_id_drive %in% two.drive.outcomes.id) %>% 
#        select(game_id_drive,
#               pos_team, def_pos_team, drive_number, change_of_pos_team, drive_result_detailed, # lead_drive_result_detailed,
#               play_type, play_text,
#               down,
#               yards_to_goal, yards_to_goal_end,
#               pos_team_score, def_pos_team_score, clock.minutes, clock.seconds,
#               penalty_declined, penalty_flag, penalty_no_play, yds_penalty, penalty_play_text, penalty_text))



## Making sure the game_id_drive matches (should be TRUE if so)
##   For NA-containing drive outcomes: assign the other one
##   OTHERWISE, for END OF HALF containing ones: assigne the other one

# print("Confirmation of data set order (TRUE if good)")
# print(all(pbp[pbp$game_id_drive %in% two.drive.outcomes.id[!two.drive.outcomes.id %in% two.drive.outcomes.non.na.non.end.half.id], "game_id_drive"] == 
#             pbp %>% filter(game_id_drive %in% two.drive.outcomes.id[!two.drive.outcomes.id %in% two.drive.outcomes.non.na.non.end.half.id]) %>% 
#             group_by(game_id_drive) %>%
#             summarise(drive_result_detailed = rep(ifelse(any(is.na(unique(drive_result_detailed))),
#                                                          unique(drive_result_detailed)[!is.na(unique(drive_result_detailed))],
#                                                          unique(drive_result_detailed)[unique(drive_result_detailed) != "End of Half"]),
#                                                   length(play_type)),
#                       .groups = "keep") %>%           # SIMPLY HERE TO AVOID PRINTOUTS
#             ungroup() %>% select(game_id_drive) %>% .[[1]]))



## Replacement:
pbp[pbp$game_id_drive %in% two.drive.outcomes.id[!two.drive.outcomes.id %in% two.drive.outcomes.non.na.non.end.half.id], "drive_result_detailed"] <- pbp %>% filter(game_id_drive %in% two.drive.outcomes.id[!two.drive.outcomes.id %in% two.drive.outcomes.non.na.non.end.half.id]) %>% 
  group_by(game_id_drive) %>%
  summarise(drive_result_detailed = rep(ifelse(any(is.na(unique(drive_result_detailed))),
                                               unique(drive_result_detailed)[!is.na(unique(drive_result_detailed))],
                                               # ifelse(any(unique(drive_result_detailed) == "End of Half"),
                                               unique(drive_result_detailed)[unique(drive_result_detailed) != "End of Half"]),
                                        #tail(unique(drive_result_detailed), 1))),
                                        length(play_type)),
            .groups = "keep") %>%
  ungroup() %>% select(drive_result_detailed) %>% .[[1]]




## MORE THAN 2 OUTCOMES: DROP THOSE
more.than.two.drive.outcomes.id <- bad.drive.no %>% 
  filter(n_drive_res > 2) %>%
  select(game_id_drive) %>%
  .[[1]]

more.than.two.drive.outcomes.halves <- unique(pbp$game_id_half[pbp$game_id_drive %in% more.than.two.drive.outcomes.id])

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% more.than.two.drive.outcomes.halves,]
dim(pbp)

# View(pbp %>%
#        filter(game_id_drive %in% two.drive.outcomes.id) %>%
#       # filter(game_id_drive %in% more.than.two.drive.outcomes.id) %>%
#        #filter(game_id_drive %in% bad.drive.no$game_id_drive) %>%
#        select(game_id_drive,
#               drive_result_detailed,
#               #drive_num, drive_number,
#               play_text, play_type,
#               # drive_result_detailed_flag,
#               offense_score, defense_score, pos_team, def_pos_team,
#               period,  TimeSecsRem, TimeSecsRem_end, SecondsElapsed,
#               drive_num, drive_num,
#               clock.minutes, clock.seconds))


#######
## 8p5.D2B.
##   POSSESSIONS where there are flipped "End of Half" and actual plays, also affecting the YARDS TO GOAL stuff...
##    So we need to:
##      1. Flip the last two rows
##      2. Assign the 3rd-to-last row's "yards_to_goal_end" to be the same as next row's "yards_to_goal"
## !! ALL THE LEAD/LAG variables will get screwed over though... !!

relev.play.id <- which(pbp$game_id_drive %in% end.of.half.not.last.play.drives & pbp$play_type == "End of Half")

if (length(relev.play.id) > 0){
  last.row <- pbp[relev.play.id, ]
  penultimate.row <- pbp[relev.play.id+1, ]
  penultimate.row$yards_to_goal_end <- NA  # As the row preceding the "End of Half" play, with unreliable yards_to_goal_end

  pbp[c(relev.play.id, relev.play.id + 1), ] <- rbind(penultimate.row, last.row)
  #pbp[which(pbp$game_id_drive %in% end.of.half.not.last.play.drives & pbp$play_type == "End of Half")+1, ] <- last.row
  pbp[relev.play.id-1, "yards_to_goal_end"] <- penultimate.row$yards_to_goal

  end.of.half.not.last.play.drives

# View(pbp %>%
#        # filter(game_id_drive %in% c("400547675-11", "400547675-12", "400547675-13")) %>%
#        # filter(game_id_half %in% c("400548070-1")) %>%
#        filter(game_id_drive %in% end.of.half.not.last.play.drives) %>%
#        select(game_id_drive,
#               pos_team, def_pos_team, drive_number, change_of_pos_team, drive_result_detailed, # lead_drive_result_detailed,
#               play_type, play_text,
#               down,
#               yards_to_goal, yards_to_goal_end,
#               pos_team_score, def_pos_team_score, clock.minutes, clock.seconds,
#               penalty_declined, penalty_flag, penalty_no_play, yds_penalty, penalty_play_text, penalty_text))

}



## Now that should be empty - we disposed of all the issues where "End of Half" drives that don't have "End of Half" as their last play
end.of.half.not.last.play.drives <- pbp %>% filter(game_id_drive %in% end.of.half.drives) %>%
  group_by(game_id_drive) %>%
  summarize(end.of.half.last.play = (tail(play_type,1) == "End of Half")) %>%
  filter(!end.of.half.last.play) %>% .[[1]]

# print(length(unique(end.of.half.not.last.play.drives)))


####
## 8p5.D2C.  Other "End of Half" issues
#
# mismatch & !good.downs.TO & !end.of.half.play & !punt.fumb.recov & !onside.kick.recov & !penalty.play & end.of.half.drive
#     Just the cases where there's no explicit "End of Half" play - which is fine. The drive result still says "End of Half", and it's near the end of clock.
#
#  CLEAN
###

####
## 8p5.E. The NON-PENALTY, NON-END-OF-HALF issues:
#    VIRTUALLY EVERYTHING HERE IS BAD OBSERVATIONS - so THROW OUT, BAR ONE ASPECT:
##    play = "Blocked Punt (Safety)" & drive = "Safety" are legit.
#   - DROP THAT STUFF
##
##  #  Clearly some error (to be deleted):
#     "400609096-26", "400609096-27" - "Rushing Touchdown" although the drive stops on an incomplete pass on a 3rd down... just flips the possession team, and the NEXT drive has "Rushing Touchdown"
#     "400547781-24"  -  drive result = Punt, play = Rush
#     "400547849-27" - Blocked FG drive result, Pass Reception play
#     "400548197-20" - drive = Passing TD, play_type = Rush,
#     "400548386-27", "400548158-15" - drive = Fumble Recovery (Opponent), play = Kickoff
#     "400548282-17" - drive = Punt, play = Rush
#     "400547873-2"  - drive = Punt, play = Pass Incompletion
#     "400547700-2" - drive = Downs Turnover, last play is on 2nd down
#     "400548343-1" - drive = Punt, play = Kickoff, but ACTUALLY RESULTING IN A KICKOFF RETURN TD...
#     "400548158-20" - drive = Rushing TD, play = rush
#     "400609096-26" - drive = Rushing TD, play = pass incompletion
###

last.bad.play.drive.match <- pbp %>% 
  mutate(play_type = ifelse(play_type == "End of Game", "End of Half", play_type)) %>%
  group_by(game_id_drive) %>%
  summarize(mismatch = (tail(play_type, 1) != tail(drive_result_detailed, 1)),
            good.downs.TO = (tail(drive_result_detailed, 1) == "Downs Turnover") & (tail(down,1) == 4),
            end.of.half.play = (tail(play_type,1) == "End of Half"),
            end.of.half.drive = (tail(drive_result_detailed,1) == "End of Half"),
            punt.fumb.recov = (tail(play_type,1) == "Punt Team Fumble Recovery"),
            onside.kick.recov = (tail(drive_result_detailed, 1) == "On-Side Kick Lost"),
            penalty.play = (tail(play_type,1) == "Penalty"),
            scoring.play = (tail(scoring_play,1) == 1),
            penalty.no.play = tail(penalty_no_play,1),
            punt.blk.safety = tail(play_type,1) == "Blocked Punt (Safety)" & tail(drive_result_detailed,1) == "Safety") %>%
  ##   END-OF-HALF, withOUT PENALTIES:
  # filter(mismatch & !good.downs.TO & !end.of.half.play & !punt.fumb.recov & !onside.kick.recov & !penalty.play & end.of.half.drive) %>%
  #    NON-END-OF-HALF, NON-PENALTY (non punt-block-safety)
  filter(mismatch & !good.downs.TO & !end.of.half.play & !punt.fumb.recov & !onside.kick.recov & !penalty.play & !end.of.half.drive & !punt.blk.safety) %>%
  select(game_id_drive) %>% .[[1]]


last.bad.play.drive.half.ids <- unique(pbp$game_id_half[pbp$game_id_drive %in% last.bad.play.drive.match])

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% last.bad.play.drive.half.ids, ]
dim(pbp)

# View(pbp %>%
#        # filter(game_id_drive %in% c("400547667-20", "400547667-21")) %>%
#        # filter(game_id_half %in% c("400548070-1")) %>%
#       filter(game_id_drive %in% last.bad.play.drive.match) %>%
#        select(game_id_drive,
#               pos_team, def_pos_team, drive_number, change_of_pos_team, drive_result_detailed, # lead_drive_result_detailed,
#               play_type, play_text,
#               down,
#               yards_to_goal, yards_to_goal_end,
#               pos_team_score, def_pos_team_score, clock.minutes, clock.seconds,
#               penalty_declined, penalty_flag, penalty_no_play, yds_penalty, penalty_play_text, penalty_text))

# dim(pbp[pbp$game_id_drive == "400547905-12",])

####
## 8p5.SUMMARY:
##

bad.play.drive.match <- pbp %>% 
  mutate(play_type = ifelse(play_type == "End of Game", "End of Half", play_type)) %>%
  group_by(game_id_drive) %>%
  summarize(mismatch = (tail(play_type, 1) != tail(drive_result_detailed, 1)),
            good.downs.TO = (tail(drive_result_detailed, 1) == "Downs Turnover") & (tail(down,1) == 4),
            end.of.half.play = (tail(play_type,1) == "End of Half"),
            end.of.half.drive = (tail(drive_result_detailed,1) == "End of Half"),
            punt.fumb.recov = (tail(play_type,1) == "Punt Team Fumble Recovery"),
            onside.kick.recov = (tail(drive_result_detailed, 1) == "On-Side Kick Lost"),
            penalty.play = (tail(play_type,1) == "Penalty"),
            scoring.play = (tail(scoring_play,1) == 1),
            penalty.no.play = tail(penalty_no_play,1),
            punt.blk.safety = tail(play_type,1) == "Blocked Punt (Safety)" & tail(drive_result_detailed,1) == "Safety") %>%
  ## For PENALTY-ending drives - SHOULD RETURN 0
  # filter(mismatch & !good.downs.TO & !end.of.half.play & !punt.fumb.recov & !onside.kick.recov & penalty.play & penalty.no.play & !scoring.play) %>%
  ## For NON-PENALTY STUFF:
  ##   END-OF-HALF, with PENALTIES - SHOULD RETURN 0
  # filter(mismatch & !good.downs.TO & !end.of.half.play & !punt.fumb.recov & !onside.kick.recov & penalty.play & end.of.half.drive) %>%
  ##   END-OF-HALF, withOUT PENALTIES - SHOULD RETURN NON-ZERO, but THESE ARE MOSTLY FINE, they just DON'T HAVE THE "END OF HALF" PLAY
  # filter(mismatch & !good.downs.TO & !end.of.half.play & !punt.fumb.recov & !onside.kick.recov & !penalty.play & end.of.half.drive) %>%
  #    NON-END-OF-HALF, NON-PENALTY (non punt-block-safety) - SHOULD RETURN 0
  filter(mismatch & !good.downs.TO & !end.of.half.play & !punt.fumb.recov & !onside.kick.recov & !penalty.play & !end.of.half.drive & !punt.blk.safety) %>%
  select(game_id_drive) %>% .[[1]]


# print(bad.play.drive.match)




#####
## 7. NA drive results 
#####


colnames(pbp)[grepl("drive", colnames(pbp))]

unique(pbp$drive_result_detailed)
summary(as.factor(pbp$drive_result_detailed))



drive_ids_na_drdetail <- unique(pbp %>% filter(is.na(drive_result_detailed)) %>% select(game_id_drive))$game_id_drive
drive_ids_na_drdetail

game_ids_na_drive_results <- unique(pbp %>% filter(is.na(drive_result_detailed)) %>% 
                                      select(game_id))$game_id



## How many plays are in NA drives?
##   1. Those that are just 1-play -> mostly some weird penalty or a timeout.. gotta just drop those particular "drives".
##   2. Those with >1 play -> gotta probably drop the entire half..

hey <- pbp %>%
  filter(game_id_drive %in% drive_ids_na_drdetail) %>%
  group_by(game_id_drive) %>%
  summarise(n.plays = n())

na_drive_res_1play <- hey$game_id_drive[hey$n.plays == 1]
na_drive_res_several_plays <- hey$game_id_drive[hey$n.plays > 1]


## The 1-PLAY "DRIVES" (WILL BE REMOVING JUST THE DRIVES, NOT THE ENTIRE HALVES)
na_drive_res_1play

# "400553383-16", "400547796-6", "400548408-10" - just a penalty, seemingly not affecting anything
# "400548017-11" - imposed on the kickoff, but it's clear from the "yards_to_goal"

# Removing those:
dim(pbp)
pbp <- pbp[!pbp$game_id_drive %in% na_drive_res_1play, ]
dim(pbp)

dim(pbp[pbp$game_id_drive == "400547905-12",])

## Removing the multiple-play, but NA, halves
## The MULTI-PLAY NA DRIVES - GONNA DROP THE ENTIRE HALVES
na_drive_res_several_plays

# "400548421-8" - Clearly an error, some plays missing in-betwee
# "400548359-11" - a recording error as well, failed to recognize fumble returned for a TD.
# "400548455-7" - recording error, calls it "Uncategorized" even though it's just a clear rushing TD..
bad.halves <- unique(pbp$game_id_half[pbp$game_id_drive %in% na_drive_res_several_plays])

# Removing the ENTIRE HALVES:
dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% bad.halves, ]
dim(pbp)


# View(pbp %>%
#        filter(game_id_drive %in% c("400548455-6", "400548455-7", "400548455-8")) %>%
#        #filter(game_id_drive %in% na_drive_res_1play) %>%
#       # filter(game_id_drive %in% na_drive_res_several_plays) %>%
#        # filter(game_id %in% game_ids_na_drive_results) %>%
#       # filter(game_id_drive %in% drive_ids_na_drdetail) %>%
#               select(game_id_drive,
#                      pos_team, def_pos_team, drive_number, change_of_pos_team, drive_result_detailed, # lead_drive_result_detailed,
#                      play_type, play_text,
#                      down,
#                      yards_to_goal, yards_to_goal_end,
#                      pos_team_score, def_pos_team_score, clock.minutes, clock.seconds,
#                      penalty_declined, penalty_flag, penalty_no_play, yds_penalty, penalty_play_text, penalty_text))


# dim(pbp[pbp$game_id_drive == "400547905-12",])



#####
##   8. ONE-PLAY drives
#####

# View(pbp %>% filter(drive_id %in% hey$drive_id[hey$d_results == 0]) %>% select(drive_id, play_text, period, clock.minutes, clock.seconds, drive_result_detailed))

## checking the ONE-PLAY POSSESSIONS 
one.play.drive_ids <- pbp %>% group_by(game_id_drive) %>% summarize(no.plays=n()) %>% filter(no.plays == 1) %>% .[[1]]
one.play.drive_ids
halves.with.one.play.drive_ids <- unique(pbp$game_id_half[pbp$game_id_drive %in% one.play.drive_ids])


table(pbp[pbp$game_id_drive %in% one.play.drive_ids, ]$play_type)
table(pbp[pbp$game_id_drive %in% one.play.drive_ids, ]$drive_result_detailed)


pbp[pbp$game_id_drive %in% one.play.drive_ids & pbp$play_type %in% c("Timeout", "Penalty"), ]$scoring_play

# 1. Those that have PLAY_TYPE at PENALTY, and are NOT A SCORING PLAY: DROP THOSE "DRIVES"
#    NOTE: Some drives would have PENALTY as their only "play" resulting in a SAFETY (a scoring play) - those are LEGIT "DRIVES"
pen.nonscor.play.drives <- unique(pbp[pbp$game_id_drive %in% one.play.drive_ids & pbp$play_type %in% c("Penalty") & pbp$scoring_play == 0
                                      , ]$game_id_drive)

dim(pbp)
pbp <- pbp[!pbp$game_id_drive %in% pen.nonscor.play.drives, ]
dim(pbp)

# 2.  Those that have PLAY_TYPE either END OF GAME or END OF HALF - DROP THOSE "DRIVES" (NOT THE ENTIRE HALVES though)
# end.of.half.play.drives <- unique(pbp[pbp$game_id_drive %in% one.play.drive_ids & pbp$play_type %in% c("End of Game", "End of Half"), ]$game_id_drive)
# 
# dim(pbp)
# pbp <- pbp[!pbp$game_id_drive %in% end.of.half.play.drives, ]
# dim(pbp)
# 
# dim(pbp[pbp$game_id_drive == "400547905-12",])


one.play.drive_ids <- pbp %>% group_by(game_id_drive) %>% summarize(no.plays=n()) %>% filter(no.plays == 1) %>% .[[1]]
one.play.drive_ids
table(pbp[pbp$game_id_drive %in% one.play.drive_ids, ]$drive_result_detailed)
table(pbp[pbp$game_id_drive %in% one.play.drive_ids, ]$play_type)

##
# 3. Those that have play_type as one of: "Field Goal Missed", "Field Goal Good", "Blocked Field Goal", 
#           "Missed Field Goal Return", "Blocked Field Goal Touchdown" (?? not sure if it counts for OTHER TEAM'S POSSESSION??)
#           "Punt", "Punt Return Touchdown", "Blocked Punt", "Blocked Punt Touchdown", "Blocked Punt Safety", "Punt Team Fumble Recovery Touchdown" (RECALL: WE ARE YET TO MAKE PUNTS into a PART OF RECEIVING TEAM'S POSSESSION - so they REALLY DON'T MAKE SENSE as a "ONE-PLAY POSSESSION" JUST YET)
#     and drive result as one of: "Punt", "Downs Turnover", "Field Goal Missed", "Field Goal Good", "Blocked Field Goal",  "Blocked Field Goal Touchdown" (?? not sure if it counts for OTHER TEAM'S POSSESSION??)
#           "Missed Field Goal Return", "Blocked Field Goal Touchdown" (?? not sure if it counts for OTHER TEAM'S POSSESSION??)
#           "Punt", "Punt Return Touchdown", "Blocked Punt", "Blocked Punt Touchdown", "Blocked Punt Safety", "Punt Team Fumble Recovery Touchdown" (RECALL: WE ARE YET TO MAKE PUNTS into a PART OF RECEIVING TEAM'S POSSESSION - so they REALLY DON'T MAKE SENSE as a "ONE-PLAY POSSESSION" JUST YET)
#
# DROP THE HALVES FOR THOSE

### Checking the "Missed Field Goal Return", "Blocked FG/TD" - what does the data set do for those
##   "Missed Field Goal Return" - no switch in possession
##   "Blocked Field Goal Touchdown" - no switch in possession
# unique(pbp$game_id_drive[pbp$play_type == "Blocked Field Goal Touchdown"])
# View(pbp %>%
#   filter(game_id_drive %in% c("400547865-1", "400547865-2", "400547865-3")) %>%
#   select(game_id_drive,
#          drive_result_detailed,
#          penalty_declined,
#          #drive_num, drive_number,
#          play_text, play_type,
#          yards_to_goal, yards_to_goal_end,
#          # drive_result_detailed_flag,
#          offense_score, defense_score, pos_team, def_pos_team,
#          period,  TimeSecsRem, TimeSecsRem_end, SecondsElapsed,
#          drive_num, drive_num,
#          clock.minutes, clock.seconds))


### !!! THESE SHOULD BE POTENTIALLY CAUGHT by "DRIVE_RESULT_DETAILED" == "LAST PLAY TYPE" CHECK !!!
###   Because these punt plays turning into "1-play" drives INDEED have no rhyme or reason...
###  And "Punt" is CLEARLY THE ONLY PLAY TYPE here that makes NO SENSE as the "ONE-PLAY POSSESSION" TYPE,
###  so it DESERVES TO BE SINGLED OUT

non.starter.plays <- c("Field Goal Missed", "Field Goal Good", "Blocked Field Goal", 
                       "Missed Field Goal Return", "Blocked Field Goal Touchdown",
                       "Punt", "Punt Return Touchdown", "Blocked Punt", "Blocked Punt Touchdown", "Blocked Punt Safety", "Punt Team Fumble Recovery Touchdown")

non.starter.drive_results <- c("Punt", "Downs Turnover", "Field Goal Missed", "Field Goal Good", "Blocked Field Goal",  "Blocked Field Goal Touchdown",
                               "Missed Field Goal Return", "Blocked Field Goal Touchdown",
                               "Punt", "Punt Return Touchdown", "Blocked Punt", "Blocked Punt Touchdown", "Blocked Punt Safety", "Punt Team Fumble Recovery Touchdown")

non.starter.game_id_drives <- pbp[(pbp$game_id_drive %in% one.play.drive_ids) & (pbp$drive_result_detailed %in% non.starter.drive_results | pbp$play_type %in% non.starter.plays)
                                  #pbp$play_type == "Kickoff"
                                  , ]$game_id_drive
halves.with.one.play.non.starter.drive_ids <- unique(pbp$game_id_half[pbp$game_id_drive %in% non.starter.game_id_drives])

non.starter.game_id_drives

## 400548431-14 - punt PRECEDED BY PENALTY/NO-PLAY PUNT (could be possibly salvageable, but might not be worth it...)

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% halves.with.one.play.non.starter.drive_ids, ]
dim(pbp)

dim(pbp[pbp$game_id_drive == "400547905-12",])


# "Punt" legit for no reason is being treated as its own separate drive, while it's just a single play

# View(pbp %>%
#        # filter(game_id_drive %in% one.play.drive_ids & play_type %in% c("Timeout", "Penalty")) %>%
#        # filter(game_id_drive %in% one.play.drive_ids & drive_result_detailed %in% c("Punt")) %>%
#        # filter(game_id_half %in% halves.with.one.play.punt.drive_ids) %>%
#       # filter(game_id_half %in% halves.with.one.play.to.pen.drive_ids) %>%
#        # filter(game_id_half %in% halves.with.one.play.end.of.half.drive_ids) %>%
#      # filter(game_id_drive %in% one.play.drive_ids, drive_result_detailed == "Punt") %>%
#        filter(game_id_drive %in% c("400548431-13", "400548431-14", "400548431-15")) %>%
#                      select(game_id_drive,
#                             pos_team, def_pos_team, drive_number, change_of_pos_team, drive_result_detailed, # lead_drive_result_detailed,
#                             play_type, play_text,
#                             down,
#                             yards_to_goal, yards_to_goal_end,
#                             pos_team_score, def_pos_team_score, clock.minutes, clock.seconds,
#                             penalty_declined, penalty_flag, penalty_no_play, yds_penalty, penalty_play_text, penalty_text))











#####
##   9. WORKING WITH "CHANGE_OF_POS_TEAM" INDICATOR: 
##        9A. Checking all the play types for "change_of_pos_team = 1", 
##             looking for weird ones (e.g. "Fumble Recovery (Own)"), verifying that those are due to end of half scenarios (whether the opponent gets the ball in the next half).
##        9B. Assigning specific values for different Kickoff outcomes (along with fumbles, TD returns, On-side kicks),
##             because there's NO consistency as to which Kickoff plays have change_of_pos_team = 1 or 0
##        9C. Assigning specific values for "End of Half" drives/plays, as those should not really matter as far as "changing possession" for complementary football effect's sake.
## 
##    Some drives end up in situations where there's a FG/Punt block, or INT/Fumble, that finds its way BACK to the offense during the same play..
##          YET the DRIVE GETS BROKEN APART INTO TWO PIECES... which we probably DON'T WANT, because the COMPLEMENTARY/SPECIAL TEAM UNITS DIDN'T COME ONTO THE FIELD IN-BETWEEN..
##    THAT'S TO BE TAKEN CARE OF LATER
#########





## 9A.  Checking all the play types for "change_of_pos_team = 1", 
#       All types of plays with "change_of_pos_team == 1"

sort(unique(pbp[pbp$change_of_pos_team == 1, ]$play_type))
#
##   - "Fumble Recovery (Own)" are on 4th downs or at the end of halves, where the other team starts the 2nd half.
# View(pbp %>% filter(change_of_pos_team == 1 & play_type == "Fumble Recovery (Own)") %>% select(down,play_text, game_id_drive))
# View(pbp %>% filter(game_id_drive %in% c("400547648-19", "400547648-20")) %>% select( down, play_text, play_type, change_of_pos_team))

##   - "Fumble Return Touchdown"
##  GETS MESSED UP BECAUSE OF THE PENALTY...
# View(pbp %>% filter(change_of_pos_team == 1 & play_type == "Fumble Return Touchdown") %>% select(down, clock.minutes, clock.seconds, play_text, game_id_drive))
# View(pbp %>% filter(game_id_drive %in% c("400547648-19", "400547648-20")) %>% select( down, play_text, play_type, change_of_pos_team))

##   - "Interception Return Touchdown"
##  GETS MESSED UP BECAUSE OF THE PENALTY...
# View(pbp %>% filter(change_of_pos_team == 1 & play_type == "Interception Return Touchdown") %>% select(down, clock.minutes, clock.seconds, play_text, game_id_drive))
# View(pbp %>% filter(game_id_drive %in% c("400548398-18", "400548398-19")) %>% select( pos_team, def_pos_team, down, play_text, play_type, change_of_pos_team, drive_number))


## 9.B CHANGING the "CHANGE_OF_POS_TEAM" VALUES for KICKOFFS
# - "Kickoff" stuff: Making "change_of_poss = 0" for all PURE "Kickoff" PLAYS (not "Kickoff Team Fumble Recovery" etc etc)..
#       those should NOT lead to change of possession.
pbp[which(pbp$play_type == "Kickoff" & pbp$drive_result_detailed != "On-Side Kick Lost"), ]$change_of_pos_team <- 0

##   - "Kickoff Return Touchdown", "Kickoff Team Fumble Recovery", "Kickoff (Safety)" - CHANGE THE POSSESSION
##   (as we treat the RECEIVING TEAM as the "possessor" at first)
pbp[which(pbp$play_type %in% c("Kickoff Return Touchdown", "Kickoff Team Fumble Recovery", "Kickoff (Safety)")), ]$change_of_pos_team <- 1

# View(pbp %>% filter(change_of_pos_team == 1 & play_type == "Kickoff") %>% select(down,play_text, game_id_drive))
# View(pbp %>% filter(game_id_half %in% c("400547728-1")) %>% select( down, play_text, play_type, change_of_pos_team))

## 9.B CHANGING the "CHANGE_OF_POS_TEAM" VALUES for END OF HALVES

##   - "End of Half" - probably due to the other team getting the ball to start the 2nd half.
##       I WOULD CHANGE ALL THE "End of Half" DRIVE RESULTS' "CHANGE_OF_POS_TEAM" INDICATORS to 0
# View(pbp[pbp$drive_result_detailed == "End of Half", c("game_id_drive", "change_of_pos_team", "play_text") ])
#
# Making "change_of_poss = 0" for all plays in the "End of Half" drives
# aaaand also for all "End of Half" plays and their PRECEDING plays..

pbp[which(pbp$drive_result_detailed == "End of Half"), ]$change_of_pos_team <- 0

end.of.half.ind <- which(pbp$play_type == "End of Half")
if (length(end.of.half.ind) >0){
  pbp[end.of.half.ind, ]$change_of_pos_team <- 0
  pbp[end.of.half.ind-1, ]$change_of_pos_team <- 0  # Some plays will be actually possession-changers, but.. shouldn't matter. It's the end of half, not much complementary impact carrying over.
}

# Some "change_of_pos_team = 1" are a result of a weird penalty DURING the defensive score.. counts as a "separate play after the score"..
#   See 400548398-2  400548398-18,  
#       400548101-2  400548101-21,
# pbp[pbp$game_id_drive == "400548398-18", c("change_of_pos_team", "play_text")]

# Others are END OF HALF interceptions, where possession switches for that END OF HALF "play":
#   400547853-2  400547853-31
#
# Others are just a mistake:
#   See 400547796-1   400547796-5,
#       400548017-1  400548017-10,
#       400548367-1  400548367-12

# pbp[pbp$play_type == "Interception Return Touchdown" & pbp$change_of_pos_team == 1,
#     c("game_id_half", "game_id_drive")]




## Probably the only one that's systematically fixable:
##      - change_of_poss = 0 for plays that had a Fumble returned for a TD, which is correct, 
#               but the drive outcome just says "Fumble Recovery (Opponent)", even though the drive # changes (with the possession team staying the same..)
#           400547643-1, "400547741-2", 400547831-1, 400547670-2, 400547687-2, 400548423-1, 400547819-2, 400548215-1, 400548220-2,
#           400547792-1, 400548250-2

# "scoring_play", "def_scoring_play" - all these are off-base, staying at 0 for clearly scoring plays by defense..
# "pos_score_diff_start", "pos_score_diff_start_end" - are the only ones being reflective.. and the difference should be 6 or 7 if a fumble was returned for a TD

our.ind <- which(pbp$play_type == "Fumble Recovery (Opponent)" & 
                   pbp$change_of_pos_team == 0 & 
                   pbp$drive_result_detailed == "Fumble Recovery (Opponent)" &
                   ((pbp$pos_score_diff_start - pbp$pos_score_diff_start_end) %in% c(6:8)))

# pbp[our.ind, "game_id_half"]
# 
# View(pbp[our.ind, c("game_id_half",  "game_id_drive", "play_type", "play_text",
#                     "drive_result_detailed", "pos_team_score", "def_pos_team_score",
#                     "pos_score_diff_start", "pos_score_diff_start_end", "pos_team", "def_pos_team", "change_of_pos_team",
#                     "down")])

## Switching those "Fumble Recovery (Opponent)" drive results into "Fumble Return Touchdown", making them INSTANT DEFENSE SCORES
## Changing the play type and scoring status of the play as well

if (length(our.ind) > 0){
  pbp[which(pbp$game_id_drive %in% pbp[our.ind, "game_id_drive"]), ]$drive_result_detailed <- "Fumble Return Touchdown"
  pbp[which(pbp$game_id_drive %in% pbp[our.ind, "game_id_drive"]), ]$play_type <- "Fumble Return Touchdown"
  pbp[which(pbp$game_id_drive %in% pbp[our.ind, "game_id_drive"]), ]$scoring_play <- 1
}





# That one miiight be?
#       - change_of_poss =0, while drive # changes, for plays that had the ball come back to the offense after an INT, Fumble, Blocked FG/Punt:
#            "400547648-2", 400610177-2", 400548102-1, 400547983-2, 400547768-2, 400547973-1
#
### !!! DON'T WORK !!!
##      WIPES OUT FALSE POSITIVES TOO, and it's TOUGH TO CONTROL...

## REMEMBER: here we are yet to flip punts over to the receiving team as the "possession" team, hence -
#                   "Punt Team Fumble Recovery" actually means NO CHANGE IN POSSESSION (punt was your team's play, and you recovered it)
#                   "Punt Team Fumble Recovery Touchdown" IS a change in possession (punt was your team's play, you scored, and that turns possession over to the opponent)
instant.scores <- c("Blocked Field Goal Touchdown", "Fumble Recovery (Opponent) Touchdown", 
                    "Fumble Return Touchdown", "Interception Return Touchdown",
                    # ,"Punt Team Fumble Lost Touchdown"
                    "Punt Team Fumble Recovery", "Punt Return Touchdown", "Blocked Punt Touchdown"#, "Safety"
                    #,"Punt Team Fumble Recovery Touchdown"
)

## All halves where a drive number switched on a NON-INSTANT SCORE PLAY, despite 
# 1) the "POS_TEAM" name not switching, and 2) change_of_pos_team = 0
## DROP THOSE
bad.switch.halves.vol.3 <- pbp %>% 
  group_by(game_id_half) %>%
  summarise(switch.mismatch = sum((!(play_type[-length(play_type)] %in% c("Punt Team Fumble Recovery"))) & !(play_type[-length(play_type)] %in% instant.scores)
                                  & (diff(as.numeric(factor(pos_team))) == 0) & (diff(drive_number) != 0) & 
                                    (change_of_pos_team[-length(change_of_pos_team)] == 1))) %>%
  filter(switch.mismatch != 0) %>% .[["game_id_half"]]

bad.switch.halves.vol.3

## Dropping those:
dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% bad.switch.halves.vol.3,]
dim(pbp)


# View(pbp %>%
#        #  filter(game_id_half %in% bad.switch.halves.vol.3) %>%
#        filter(game_id_half == "400547905-1") %>% 
#        select(pos_team, def_pos_team, drive_number, change_of_pos_team, drive_result_detailed, play_type, play_text,
#               yards_to_goal, yards_to_goal_end,
#               pos_team_score, def_pos_team_score, clock.minutes, clock.seconds))







########
## 10. After a SCORING DRIVE (including SAFETY??), checking that the NEXT PLAY is always either a:
##    - Kickoff,
##    - End of Game/Half,
##    - Penalty
## The rest?? It's CLEAR THAT THE KICKOFF PLAY HAS NOT BEEN ENTERED... MANY OF THOSE PLAYS START FROM "75 yds to go", hence after a KICKOFF TOUCHBACK..
## BUT ALSO, A HANDFUL OF SUCH "LAZY RECORDING TENDENCIES" SEEM TO COME FROM THE SAME GAMES !!!
#######

# our.ind <- which((pbp$lag_play_type == "Field Goal Good" | 
#                     str_detect(tolower(pbp$lag_play_type), "touchdown") |
#                     str_detect(tolower(pbp$lag_play_type), "safety")) & 
#                    !str_detect(tolower(pbp$play_type), "kickoff") &
#                    !pbp$play_type %in% c("End of Game", "End of Half", "Penalty"))

our.ind <- which(((pbp$play_type[-length(pbp$play_type)] == "Field Goal Good" | 
                     str_detect(tolower(pbp$play_type[-length(pbp$play_type)]), "touchdown") |
                     str_detect(tolower(pbp$play_type[-length(pbp$play_type)]), "safety")) & 
                    !str_detect(tolower(lead(pbp$play_type)[-length(pbp$play_type)]), "kickoff") &
                    !(lead(pbp$play_type)[-length(pbp$play_type)] %in% c("End of Game", "End of Half", "Penalty"))))
our.ind

bad.follow.score.drives <- unique(pbp$game_id_drive[our.ind])
bad.follow.score.drives

## CLEARLY MISSING A KICKOFF PLAY:
##    "400548008-13", "400548008-14" 
##    "400548169-5", "400548169-6" 
##    "400547842-22", "400547842-22"
##    "400547665-1", "400547665-2"
##    "400547665-5", "400547665-6"
##    "400547665-8", "400547665-9"  etc etc
## GOTTA DROP THOSE..

bad.follow.score.halves <- unique(pbp$game_id_half[pbp$game_id_drive %in% bad.follow.score.drives])
bad.follow.score.halves

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% bad.follow.score.halves, ]
dim(pbp)


# View(pbp %>%
#        #  filter(game_id_half %in% bad.switch.halves.vol.3) %>%
#        filter(game_id_drive %in% c("400547665-8", "400547665-9")) %>% 
#        select(pos_team, def_pos_team, drive_number, change_of_pos_team, drive_result_detailed, play_type, play_text,
#               yards_to_goal, yards_to_goal_end,
#               pos_team_score, def_pos_team_score, clock.minutes, clock.seconds))


######
## NOTE FROM MARCH 12th:
##    CHECK THE "yards_to_goal_end = 75" for FIELD GOAL GOOD
########

### THESE ARE GONE as a RESULT OF THE ABOVE DELETION, which is CORRECT
our.ind <- which(pbp$play_type == "Field Goal Good" & pbp$yards_to_goal_end == 75) 
our.ind          

# print("# of bad obs here (not to be removed though):")
# print(length(our.ind))
                   
                   

########
## CUTOFF for the 2014-2021 CHECK
########

# cat("\n")
# cat("\n")
# print("DATA SET SIZE AFTER SECONDARY CLEANING")
# print(dim(pbp))
# print(length(unique(pbp$game_id_half)))
# cat("\n")
# cat("\n")


# }














#################
#################
### FIXING UP certain STATISTICAL ENTRIES
##################
##################



######
## fumble return TDs (where it's unclear of the preceding play was pass/rush...)
######

# View(pbp %>% 
#        filter(#play_type == "Fumble Recovery (Own)", 
#          str_detect(tolower(play_type), fixed("fumble return")) & !is.na(yds_fumble_return),
#        ) %>%
#        .[, c("game_id", "pos_team", "def_pos_team", "pos_team_score", "def_pos_team_score", 
#              "play_type", "play_text", "drive_result_detailed",
#              "yards_to_goal", "yards_to_goal_end", "penalty_flag", "yds_penalty",  "yds_fumble_return",
#              "fumble_vec", "sack_vec", "yds_sacked",
#              "yards_gained", "yds_rushed", "yds_receiving", 
#              "rush_yds", "reception_yds", "yds_fumble_return",
#              "drive_time_minutes_elapsed", "drive_time_seconds_elapsed",
#              "period", "drive_time_minutes_end", "drive_time_seconds_end",
#              "down")])

# pbp$yds_fumble_return[pbp$play_type == "Fumble Recovery (Opponent)"]

## For fumble return TD, just make it yards_gained = "yards_to_goal - 100", same for "yds_fumble_return" ONLY POSITIVE
## As far as INDICATOR of a fumble play during by_drive summary, it's gonna be done via "play_text" field analysis anyway
## 
our.ind <- which(str_detect(tolower(pbp$play_type), fixed("fumble return touchdown")))
if (length(our.ind) > 0){
  pbp$yards_gained[our.ind] <- pbp$yards_to_goal[our.ind] - 100
  pbp$yds_fumble_return[our.ind] <- -pbp$yards_gained[our.ind]
  pbp$fumble_vec[our.ind] <- 1
}

# View(pbp[our.ind, ] %>% 
#   select(game_id_drive, pos_team, def_pos_team, play_text, yards_gained, yds_fumble_return, yards_to_goal, yards_to_goal_end, fumble_vec))



###########
### yards_gained, rush_yds, receiving_yds
###########

##   "yards_gained" vs "yds_rushed" - gotta figure out which one to use for rushing plays, as there's a BUNCH OF DISCONNECTS
##      Seems like, whenever there's NO PENALTY on the play, & 
##          * its your OWN TEAM picking the fumble back up, SUMMING "yards_gained" & "yds_rushed" should do the trick
##          * its the OPPONENT picking it up, gotta FLIP THE SIGN of the "yards_gained" and SUM it with "yds_rushed"
##          * if its "Safety" play type, sum "yards_gained" & "yds_rushed"
##          * if it's "rush" play type, rather than fumble, use "yards_gained" (they probably messed up in typing the full play text..)
#           
##      For PENALTY plays.. keep it AS IS??? Penalty yards are penalty yards, rushing yards (before the penalty) are rushing yards.. So, ONE PLAY gives BIRTH to 2 DISTINCT YARDAGES...
##        BESIDES the cases where it is FUMBLE RECOVERED BY OPPONENT... 
##        because YARDS GAINED need to FLIP SIGN?? - EVIDENTLY NOT!!! It properly accounts for PENALTY yards from the perspective of the team that possessed the ball prior to fumbling, and CORRECTLY CALCULATES YARDS GAINED
##        => NOTHING NEEDED THERE...
##
##      ONLY THING: "yds_fumble_return" is for some reason excluded from calculations whenever there's PENALTY FLAG.
##                  So, whenever it's NON-NA for such PENALTY SITUATIONS, need to SUBTRACT IT from YARDS GAINED??

##   400547928, 24-20
##   400547931, 14-7

# View(pbp %>% 
#        filter(play_type == "Fumble Recovery (Own)" #, 
#               # yards_gained != yds_rushed, !penalty_flag, !penalty_declined
#               # yards_gained != yds_rushed + yds_penalty, penalty_flag, !penalty_declined
#               # str_detect(tolower(play_type), fixed("fumble recovery")), penalty_flag, !penalty_declined,
#               # str_detect(tolower(play_type), fixed("fumble return")),
#        ) %>%
#        .[, c("game_id", "pos_team", "def_pos_team", "pos_team_score", "def_pos_team_score", 
#              "play_type", "play_text", "yards_to_goal", "yards_to_goal_end", "penalty_flag", "yds_penalty",  "yds_fumble_return",
#              "fumble_vec", "sack_vec", "yds_sacked",
#              "yards_gained", "yds_rushed", "yds_receiving", 
#              "rush_yds", "reception_yds", "yds_fumble_return",
#              "drive_time_minutes_elapsed", "drive_time_seconds_elapsed",
#              "period", "drive_time_minutes_end", "drive_time_seconds_end",
#              "down")])


### Trying to do that through YARDS_TO_GOAL


####
##   "fumble recovery (opponent)" (ANALOGOUS FOR "receiving_yds")
#####
##      If change_of_pos_team == 0 and penalty.flag == FALSE, then assign yards_to_goal - yards_to_goal_end to BOTH yards_gained and rush_yards
##        These are VERY FEW & FAR BETWEEN, but still an INTEGRAL part. See below on what was done, but those were REPLACED for "fumble recovery (own)".
##      If change_of_pos_team == 1 and penalty.flag == FALSE, then assign yards_to_goal - (100-yards_to_goal_end) to BOTH yards_gained and rush_yards
##      If change_of_pos_team == 0 and penalty.flag ==TRUE, then assign 
##            * yards_to_goal - yards_to_goal_end to yards gained;
##            * yards_gained - penalty_yds to rush_yds
##      If change_of_pos_team == 1 and penalty.flag ==TRUE, then assign 
##            * yards_to_goal - (100- yards_to_goal_end) to yards gained;
##            * yards_gained - penalty_yds to rush_yds
##      For "penalty_declined=TRUE" that still works - the "yds_penalty" will just be at 0 then
##    (pbp$yds_penalty[pbp$penalty_declined])

#####
## !!!  fumble recovery (opponent), pbp$change_of_pos_team == 0, , penalty_flag == FALSE  !!!
##  JUST DROP THESE - the implications are NOT CONSISTENT ACROSS THE YEARS...
##
##
## These are VERY FEW & FAR BETWEEN, and in most cases have to do with FUMBLE that was INITIALLY recovered by the opponent,
## but THEN recovered BACK by the offense, without defense having to come onto the field.. SO SHOULD STILL BE THE SAME DRIVE
##
##    1. Change the PRE-FUMBLE DRIVE RESULT to the POST-FUMBLE DRIVE RESULT
##    2. CHANGE THE SUBSEQUENT DRIVE'S (IF IT EXISTS) ENUMERATION to THAT FUMBLING ONE
##
##  NOTE: "NA" ARE POSSIBLE for the DRIVE_RESULT_DETAILED whenever there's NO DRIVE AFTER THE "FUMBLE RECOVERY"
##
#######
##    


bad.drive.trans.id <- which(str_detect(tolower(pbp$play_type), fixed("fumble recovery (opponent)")) & pbp$change_of_pos_team == 0 & !is.na(pbp$yards_to_goal_end))

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% unique(pbp$game_id_half[bad.drive.trans.id]),]
dim(pbp)

# if (length(bad.drive.trans.id) > 0){
#   
# bad.drive.transitions <- pbp[bad.drive.trans.id, ] %>% 
#   select(game_id, drive_number, game_id_drive, game_id_half)
# 
# pbp$play_type[bad.drive.trans.id] <- "Fumble Recovery (Own)"
# 
# for (j in 1:nrow(bad.drive.transitions)){
#   
#   next.drive.obs <- which(pbp$game_id == bad.drive.transitions$game_id[j] & pbp$drive_number == (bad.drive.transitions$drive_number[j] + 1))
#   if (length(next.drive.obs) > 0){
#   pbp[which(pbp$game_id == bad.drive.transitions$game_id[j] & pbp$drive_number == (bad.drive.transitions$drive_number[j])),]$drive_result_detailed <- ifelse(is.na(pbp[pbp$game_id == bad.drive.transitions$game_id[j] & pbp$drive_number == (bad.drive.transitions$drive_number[j] + 1),]$drive_result_detailed[1]),
#                                                                                                                                                       NA,
#                                                                                                                                                       pbp[pbp$game_id == bad.drive.transitions$game_id[j] & pbp$drive_number == (bad.drive.transitions$drive_number[j] + 1),]$drive_result_detailed[1])
#   
#   pbp[which(pbp$game_id == bad.drive.transitions$game_id[j] & pbp$drive_number == (bad.drive.transitions$drive_number[j] + 1)), c("drive_number", "game_id_drive")] <- data.frame(bad.drive.transitions$drive_number[j], bad.drive.transitions$game_id_drive[j])
# }
# 
# bad.drive.transitions
# }


# View( #pbp[sort(c(ind.play.type)),] %>%
#         # pbp[ind.play.type[which(pbp[ind.play.type, "yards_gained"] != (pbp[ind.play.type, "yards_to_goal"] - (100-pbp[ind.play.type, "yards_to_goal_end"])))], ] %>%
#        # pbp %>% filter(game_id_drive %in% bad.drive.transitions$game_id_drive) %>%
#         # pbp %>% filter(game_id_drive %in% c("401019499-9", "401019499-10")) %>%
#   pbp %>% filter(game_id_drive %in% c("401019499-10", "401019499-11")) %>%
#   
#   
#         select(game_id_drive,
#                pos_team, def_pos_team, drive_number, change_of_pos_team, drive_result_detailed, play_type, play_text,
#                down,
#                yards_to_goal, yards_to_goal_end,
#                yards_gained, yds_rushed, yds_receiving,
#                rush_yds, reception_yds, yds_fumble_return,
#                penalty_flag, yds_penalty,
#                pos_team_score, def_pos_team_score, clock.minutes, clock.seconds))


#####
## !!!  "fumble recovery (opponent)" , pbp$change_of_pos_team == 1, penalty_flag == FALSE  !!!
##    SAME AS FOR THE ANALOGOUS CASE of ""fumble recovery (own)", PLEASE SEE FURTHER BELOW
####

ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("fumble recovery (opponent)")) & pbp$change_of_pos_team ==1 & pbp$penalty_flag == FALSE)
ind.play.type

if (length(ind.play.type) > 0){

pbp$yards_gained[ind.play.type] <- ifelse(!is.na(pbp$yards_to_goal[ind.play.type]) & !is.na(pbp$yards_to_goal_end[ind.play.type]),
                                          pbp$yards_to_goal[ind.play.type] - (100 - pbp$yards_to_goal_end[ind.play.type]),
                                          pbp$yards_gained[ind.play.type])
pbp$yds_rushed[ind.play.type] <- ifelse(is.na(pbp$yds_rushed[ind.play.type]), NA,
                                        ifelse(!is.na(pbp$yards_to_goal[ind.play.type]) & !is.na(pbp$yards_to_goal_end[ind.play.type]),
                                               pbp$yards_to_goal[ind.play.type] - (100 - pbp$yards_to_goal_end[ind.play.type]),
                                               pbp$yds_rushed[ind.play.type]))
pbp$yds_receiving[ind.play.type] <- ifelse(is.na(pbp$yds_receiving[ind.play.type]), NA,
                                           ifelse(!is.na(pbp$yards_to_goal[ind.play.type]) & !is.na(pbp$yards_to_goal_end[ind.play.type]),
                                                  pbp$yards_to_goal[ind.play.type] - (100 - pbp$yards_to_goal_end[ind.play.type]),
                                                  pbp$yds_receiving[ind.play.type]))

}

#####
## !!!  "fumble recovery (opponent)",  pbp$change_of_pos_team == 0, penalty_flag == TRUE !!!
##    SAME AS FOR THE ANALOGOUS CASE of ""fumble recovery (own)", PLEASE SEE FURTHER BELOW
####

ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("fumble recovery (opponent)")) & pbp$change_of_pos_team ==0 & pbp$penalty_flag == TRUE)
ind.play.type

if (length(ind.play.type) > 0){
pbp$yards_gained[ind.play.type] <- ifelse(!is.na(pbp$yards_to_goal[ind.play.type]) & !is.na(pbp$yards_to_goal_end[ind.play.type]),
                                          pbp$yards_to_goal[ind.play.type] - pbp$yards_to_goal_end[ind.play.type],
                                          pbp$yards_gained[ind.play.type])
}

## TOUGH TO RELY ON "YDS_PENALTY" TO PROPERLY REDISTRIBUTE CREDIT BETWEEN RUSH/PASS & PENALTY, so JUST STICKING WITH ORIGINAL VALUES HERE
# pbp$yds_rushed[ind.play.type] <- ifelse(is.na(pbp$yds_rushed[ind.play.type]), NA,
#                                         ifelse(!is.na(pbp$yards_to_goal[ind.play.type]) & !is.na(pbp$yards_to_goal_end[ind.play.type]),
#                                                pbp$yards_gained[ind.play.type] - pbp$yds_penalty[ind.play.type],
#                                                pbp$yds_rushed[ind.play.type]))
# pbp$yds_receiving[ind.play.type] <- ifelse(is.na(pbp$yds_receiving[ind.play.type]), NA,
#                                            ifelse(!is.na(pbp$yards_to_goal[ind.play.type]) & !is.na(pbp$yards_to_goal_end[ind.play.type]),
#                                                   pbp$yards_gained[ind.play.type] - pbp$yds_penalty[ind.play.type],
#                                                   pbp$yds_receiving[ind.play.type]))


#####
## !!!  "fumble recovery (opponent)",  pbp$change_of_pos_team == 1, penalty_flag == TRUE !!!
##    These are just some nonsensical values all across the board, but especially for PENALTY YARDAGES (45, 38, 26... not correspondent with text or realistic yardage scenarios)
####

ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("fumble recovery (opponent)")) & pbp$change_of_pos_team ==1 & pbp$penalty_flag == TRUE)
ind.play.type

if (length(ind.play.type) > 0){
pbp$yards_gained[ind.play.type] <- ifelse(!is.na(pbp$yards_to_goal[ind.play.type]) & !is.na(pbp$yards_to_goal_end[ind.play.type]),
                                          pbp$yards_to_goal[ind.play.type] - (100 - pbp$yards_to_goal_end[ind.play.type]),
                                          pbp$yards_gained[ind.play.type])
}
## TOUGH TO RELY ON "YDS_PENALTY" TO PROPERLY REDISTRIBUTE CREDIT BETWEEN RUSH/PASS & PENALTY, so JUST STICKING WITH ORIGINAL VALUES HERE
# pbp$yds_rushed[ind.play.type] <- ifelse(is.na(pbp$yds_rushed[ind.play.type]), NA,
#                                         ifelse(!is.na(pbp$yards_to_goal[ind.play.type]) & !is.na(pbp$yards_to_goal_end[ind.play.type]),
#                                                pbp$yards_gained[ind.play.type] - pbp$yds_penalty[ind.play.type],
#                                                pbp$yds_rushed[ind.play.type]))
# pbp$yds_receiving[ind.play.type] <- ifelse(is.na(pbp$yds_receiving[ind.play.type]), NA,
#                                         ifelse(!is.na(pbp$yards_to_goal[ind.play.type]) & !is.na(pbp$yards_to_goal_end[ind.play.type]),
#                                                pbp$yards_gained[ind.play.type] - pbp$yds_penalty[ind.play.type],
#                                                pbp$yds_receiving[ind.play.type]))









####
##   "fumble recovery (own)" (ANALOGOUS FOR "receiving_yds")
####
##      If change_of_pos_team == 0 and penalty.flag == FALSE, then assign yards_to_goal - yards_to_goal_end to BOTH yards_gained and rush_yards
##      If change_of_pos_team == 1 and penalty.flag == FALSE, then assign yards_to_goal - (100-yards_to_goal_end) to BOTH yards_gained and rush_yards
##      If penalty.flag == TRUE, the "yds_penalty" is a BIT SHAKY, so I would just STICK TO THE ORIGINAL VALUES of "yds_rush"/"yds_receiving" for those..
##
##      (( BUT, if we were to try and REDISTRIBUTE CREDIT between RUSH/PASS & PENALTY for the YARDAGE ON THE PLAY:
##          If change_of_pos_team == 0 and penalty.flag ==TRUE, then assign 
##            * yards_to_goal - yards_to_goal_end to yards gained;
##            * yards_gained - penalty_yds to rush_yds
##          If change_of_pos_team == 1 and penalty.flag ==TRUE, then assign 
##            * yards_to_goal - (100- yards_to_goal_end) to yards gained;
##            * yards_gained - penalty_yds to rush_yds
##         For "penalty_declined=TRUE" that still works - the "yds_penalty" will just be at 0 then
##    (pbp$yds_penalty[pbp$penalty_declined]) ))
##
##  NOTE: 
##     * If the "yards_to_goal_end" is NA (which is a RARE CASE of the END OF HALF plays), then we just stick with the original values for yards_gained, yds_rushed, FOR SIMPLICITY
##
##
##  !!! HOW ABOUT BLOCKED FG & PUNTS (AS FAR AS DRIVE ENUMERATION) !!!




# View(pbp[sort(c(ind.play.type, ind.play.type+1)),] %>%
#  # pbp[ind.play.type[which(pbp[ind.play.type, "yards_gained"] != (pbp[ind.play.type, "yards_to_goal"] - pbp[ind.play.type, "yards_to_goal_end"]))], ] %>%
#        #pbp %>% filter(game_id_drive %in% c("400547773-13", "400547773-14")) %>%
#        select(game_id_drive,
#               pos_team, def_pos_team, drive_number, change_of_pos_team, drive_result_detailed, play_type, play_text,
#               yards_to_goal, yards_to_goal_end,
#               yards_gained, yds_rushed, yds_receiving,
#               rush_yds, reception_yds, yds_fumble_return,
#               penalty_flag, yds_penalty,
#               pos_team_score, def_pos_team_score))




ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("fumble recovery (own)")) & pbp$change_of_pos_team == 0 & pbp$penalty_flag == FALSE)
ind.play.type

if (length(ind.play.type) > 0){
pbp$yards_gained[ind.play.type] <- ifelse(!is.na(pbp$yards_to_goal[ind.play.type]) & !is.na(pbp$yards_to_goal_end[ind.play.type]),
                                          pbp$yards_to_goal[ind.play.type] - pbp$yards_to_goal_end[ind.play.type],
                                          pbp$yards_gained[ind.play.type])
pbp$yds_rushed[ind.play.type] <- ifelse(is.na(pbp$yds_rushed[ind.play.type]), NA,
                                        ifelse(!is.na(pbp$yards_to_goal[ind.play.type]) & !is.na(pbp$yards_to_goal_end[ind.play.type]),
                                               pbp$yards_to_goal[ind.play.type] - pbp$yards_to_goal_end[ind.play.type],
                                               pbp$yds_rushed[ind.play.type]))
pbp$yds_receiving[ind.play.type] <- ifelse(is.na(pbp$yds_receiving[ind.play.type]), NA,
                                           ifelse(!is.na(pbp$yards_to_goal[ind.play.type]) & !is.na(pbp$yards_to_goal_end[ind.play.type]),
                                                  pbp$yards_to_goal[ind.play.type] - pbp$yards_to_goal_end[ind.play.type],
                                                  pbp$yds_receiving[ind.play.type]))
}


ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("fumble recovery (own)")) & pbp$change_of_pos_team == 1 & pbp$penalty_flag == FALSE)

if (length(ind.play.type) > 0){

pbp$yards_gained[ind.play.type] <- ifelse(!is.na(pbp$yards_to_goal[ind.play.type]) & !is.na(pbp$yards_to_goal_end[ind.play.type]),
                                          pbp$yards_to_goal[ind.play.type] - (100 - pbp$yards_to_goal_end[ind.play.type]),
                                          pbp$yards_gained[ind.play.type])
pbp$yds_rushed[ind.play.type] <- ifelse(is.na(pbp$yds_rushed[ind.play.type]), NA,
                                        ifelse(!is.na(pbp$yards_to_goal[ind.play.type]) & !is.na(pbp$yards_to_goal_end[ind.play.type]),
                                               pbp$yards_to_goal[ind.play.type] - (100 - pbp$yards_to_goal_end[ind.play.type]),
                                               pbp$yds_rushed[ind.play.type]))
pbp$yds_receiving[ind.play.type] <- ifelse(is.na(pbp$yds_receiving[ind.play.type]), NA,
                                           ifelse(!is.na(pbp$yards_to_goal[ind.play.type]) & !is.na(pbp$yards_to_goal_end[ind.play.type]),
                                                  pbp$yards_to_goal[ind.play.type] - (100 - pbp$yards_to_goal_end[ind.play.type]),
                                                  pbp$yds_receiving[ind.play.type]))
}


ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("fumble recovery (own)")) & pbp$change_of_pos_team == 0 & pbp$penalty_flag == TRUE)

if (length(ind.play.type) > 0){
  
pbp$yards_gained[ind.play.type] <- ifelse(!is.na(pbp$yards_to_goal[ind.play.type]) & !is.na(pbp$yards_to_goal_end[ind.play.type]),
                                          pbp$yards_to_goal[ind.play.type] - pbp$yards_to_goal_end[ind.play.type],
                                          pbp$yards_gained[ind.play.type])

}

## TOUGH TO RELY ON "YDS_PENALTY" TO PROPERLY REDISTRIBUTE CREDIT BETWEEN RUSH/PASS & PENALTY, so JUST STICKING WITH ORIGINAL VALUES HERE
# pbp$yds_rushed[ind.play.type] <- ifelse(is.na(pbp$yds_rushed[ind.play.type]), NA,
#                                         ifelse(!is.na(pbp$yards_to_goal[ind.play.type]) & !is.na(pbp$yards_to_goal_end[ind.play.type]),
#                                                pbp$yards_gained[ind.play.type] - pbp$yds_penalty[ind.play.type],
#                                                pbp$yds_rushed[ind.play.type]))
# pbp$yds_receiving[ind.play.type] <- ifelse(is.na(pbp$yds_receiving[ind.play.type]), NA,
#                                         ifelse(!is.na(pbp$yards_to_goal[ind.play.type]) & !is.na(pbp$yards_to_goal_end[ind.play.type]),
#                                                pbp$yards_gained[ind.play.type] - pbp$yds_penalty[ind.play.type],
#                                                pbp$yds_receiving[ind.play.type]))

ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("fumble recovery (own)")) & pbp$change_of_pos_team == 1 & pbp$penalty_flag == TRUE)

if (length(ind.play.type) > 0){
pbp$yards_gained[ind.play.type] <- ifelse(!is.na(pbp$yards_to_goal[ind.play.type]) & !is.na(pbp$yards_to_goal_end[ind.play.type]),
                                          pbp$yards_to_goal[ind.play.type] - (100 - pbp$yards_to_goal_end[ind.play.type]),
                                          pbp$yards_gained[ind.play.type])
}

## TOUGH TO RELY ON "YDS_PENALTY" TO PROPERLY REDISTRIBUTE CREDIT BETWEEN RUSH/PASS & PENALTY, so JUST STICKING WITH ORIGINAL VALUES HERE
# pbp$yds_rushed[ind.play.type] <- ifelse(is.na(pbp$yds_rushed[ind.play.type]), NA,
#                                         ifelse(!is.na(pbp$yards_to_goal[ind.play.type]) & !is.na(pbp$yards_to_goal_end[ind.play.type]),
#                                                pbp$yards_gained[ind.play.type] - pbp$yds_penalty[ind.play.type],
#                                                pbp$yds_rushed[ind.play.type]))
# pbp$yds_receiving[ind.play.type] <- ifelse(is.na(pbp$yds_receiving[ind.play.type]), NA,
#                                            ifelse(!is.na(pbp$yards_to_goal[ind.play.type]) & !is.na(pbp$yards_to_goal_end[ind.play.type]),
#                                                   pbp$yards_gained[ind.play.type] - pbp$yds_penalty[ind.play.type],
#                                                   pbp$yds_receiving[ind.play.type]))



#####
## CHANGING THE DRIVE NUMBERS, in case THEY SWITCHED:
#####

## CHECKING IF THERE ARE UNWARRANTED DRIVE NUMBER CHANGES (which sometimes happens when the ball changes TEAMS during ONE PLAY):
##   We check if the "change_of_pos = 0", "yards_to_goal_end" is not NA (which would indicate the pre-End of Half play),
##     and the DRIVE NUMBER STILL CHANGES => THAT INDICATES THE DYNAMIC WE WANT TO AVOID.. so we just need to ASSIGN THE 2ND HALF OF THE DRIVE VALUES TO THE ENTIRE POSSESSION


ind.play.type <- which((tolower(pbp$play_type) %in% "fumble recovery (own)") & pbp$change_of_pos_team == 0 &
                         !is.na(pbp$yards_to_goal_end) &
                         c(diff(pbp$drive_number), NA) != 0
)
ind.play.type

##
# print("Unwarranted drive number changes:")
# print(length(unique(pbp$game_id_half[ind.play.type])))

if (length(ind.play.type) > 0){
  drive.number.pre.int <- pbp$game_id_drive[ind.play.type]
  drive.number.post.int <- pbp$game_id_drive[ind.play.type+1]
  drive.number.pre.int
  drive.number.post.int

  for (drive.num.j in 1:length(drive.number.pre.int)){
    pbp[pbp$game_id_drive == drive.number.pre.int[drive.num.j], c("game_id_drive", "drive_number", "drive_result_detailed")] <- head(pbp[pbp$game_id_drive == drive.number.post.int[drive.num.j], c("game_id_drive", "drive_number", "drive_result_detailed")], 1)
  }
}




# SANITY-CHECKING:
ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("fumble recovery (own)")))
ind.play.type






######
## Play_type = "SACK"
######

##   NO PENALTY FLAG => NO PROBLEM, SEEMINGLY (at least for year 2017)
##   (BE IT for CHANGE_OF_POS=1 or CHANGE_OF_POS=0)

# For CHANGE_OF_POS=1
ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("sack")) & !pbp$penalty_flag &
                         pbp$change_of_pos_team == 1 & pbp$pos_team != lead(pbp$pos_team))

# print("problematic, TO BE LOOKED INTO (for SACKS):")
# print(sum(pbp[ind.play.type, "yards_gained"] != (pbp[ind.play.type, "yards_to_goal"] - (100-pbp[ind.play.type, "yards_to_goal_end"])),
#           na.rm=T))

# For CHANGE_OF_POS=0
ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("sack")) & !pbp$penalty_flag &
                         pbp$change_of_pos_team == 0 & (pbp$pos_team == lead(pbp$pos_team) | pbp$game_id_half != lead(pbp$game_id_half)))

# print("problematic, TO BE LOOKED INTO (for SACKS):")
# print(sum(pbp[ind.play.type, "yards_gained"] != (pbp[ind.play.type, "yards_to_goal"] - (pbp[ind.play.type, "yards_to_goal_end"])),
#           na.rm=T))

## Replacing yards_to_goal with "yds_to_goal - (100-yds_to_goal_end)"

ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("sack")) & pbp$penalty_flag &
                         pbp$change_of_pos_team == 1 & pbp$pos_team != lead(pbp$pos_team))

# print("problematic, not to be removed")
# print(sum(pbp[ind.play.type, "yards_gained"] != (pbp[ind.play.type, "yards_to_goal"] - (100-pbp[ind.play.type, "yards_to_goal_end"])),
#           na.rm=T))

pbp[ind.play.type, ]$yards_gained <- ifelse(!is.na(pbp$yards_to_goal[ind.play.type]),
                                            pbp[ind.play.type, ]$yards_to_goal - (100 - pbp[ind.play.type, ]$yards_to_goal_end),
                                            pbp[ind.play.type, ]$yards_gained)




######
## Play_type = "SACK"
## CHANGE_OF_POS_TEAM == 0
##   SOMETIMES there's a MISHAP between STARTING POSITION and the SACK #... and the SACK YDS are MORE ACCURATE.. BUT sometimes THEY ARE NOT,
##   and that is generally MORE EGREGIOUS
##   => hence use YARD_TO_GOAL stuff..
######

## Replacing yards_to_goal with "yds_to_goal - (100-yds_to_goal_end)"
ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("sack")) & pbp$penalty_flag &
                         pbp$change_of_pos_team == 0 & (pbp$pos_team == lead(pbp$pos_team) | pbp$game_id_half != lead(pbp$game_id_half)))

# print("problematic, not to be removed")
# print(sum(pbp[ind.play.type, "yards_gained"] != (pbp[ind.play.type, "yards_to_goal"] - (pbp[ind.play.type, "yards_to_goal_end"])),
#           na.rm=T))

pbp[ind.play.type, ]$yards_gained <- ifelse(!is.na(pbp$yards_to_goal[ind.play.type]),
                                            pbp[ind.play.type, ]$yards_to_goal - (pbp[ind.play.type, ]$yards_to_goal_end),
                                            pbp[ind.play.type, ]$yards_gained)



## POSITIVE YARDS GAINED ON SACKS on plays with NO PENALTY FLAGS, TO BE LOOKED AT:
##    Some are typos, some are plays before the end of half, some have mixed up "change_of_pos" values.. tough to pin down
summary(pbp[ind.play.type, ]$yards_gained)

# print("POTENTIALLY problematic, TO BE LOOKED INTO (for SACKS), TO BE SET TO 'NA':")
ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("sack")) & !pbp$penalty_flag & pbp$yards_gained > 0)
#print(length(ind.play.type))
if (length(ind.play.type) > 0){
  pbp[ind.play.type, ]$yards_gained <- NA
}





#print("UNACCOUNTED for SACK cases (not to be removed):")
## Replacing yards_to_goal with "yds_to_goal - (100-yds_to_goal_end)"
ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("sack")) & 
                         !(pbp$penalty_flag & pbp$change_of_pos_team == 0 & (pbp$pos_team == lead(pbp$pos_team) | pbp$game_id_half != lead(pbp$game_id_half))) & 
                         !(pbp$change_of_pos_team == 1 & pbp$pos_team != lead(pbp$pos_team)) & 
                         !(!pbp$penalty_flag & !pbp$penalty_flag & pbp$change_of_pos_team == 1 & pbp$pos_team != lead(pbp$pos_team)) &
                         !(!pbp$penalty_flag & pbp$change_of_pos_team == 0 & (pbp$pos_team == lead(pbp$pos_team) | pbp$game_id_half != lead(pbp$game_id_half))))
#print(length(ind.play.type))





# View(#pbp[sort(c(ind.play.type)),] %>%
#        pbp[ind.play.type[which(pbp[ind.play.type, "yards_gained"] != (pbp[ind.play.type, "yards_to_goal"] - pbp[ind.play.type, "yards_to_goal_end"]))], ] %>%
#   # pbp[ind.play.type[which(pbp[ind.play.type, "yards_gained"] != (pbp[ind.play.type, "yards_to_goal"] - (100-pbp[ind.play.type, "yards_to_goal_end"])))], ] %>%
#     
#    #  filter(game_id_half %in% bad.switch.halves.vol.3) %>%
#        #pbp %>% filter(game_id_drive %in% c("401117515-24", "401117515-25")) %>%
#        select(game_id_drive,
#               pos_team, def_pos_team, 
#               change_of_pos_team,
#               drive_number, change_of_pos_team, drive_result_detailed, play_type, play_text,
#               yds_sacked,
#               yards_to_goal, yards_to_goal_end,
#               yards_gained, yds_rushed, yds_receiving,
#               rush_yds, reception_yds, yds_fumble_return,
#               penalty_flag, yds_penalty,
#               pos_team_score, def_pos_team_score))








####
##   "safety" (ANALOGOUS FOR "receiving_yds")
####
##  For SAFETY, the penalty shouldn't matter as much probably.. just overall yards gained, being
##     -(100 - yards_to_goal)
##   
##   BUT
##     Could do:
##        If penalty_flag == FALSE: if rush is not NA, make yds_rush = yds_gained,
##        If penalty_flag == TRUE: make yds_penalty = yds_gained, yds_rush = NA
##          OTHERWISE: if rush is not NA, make yds_rush = yds_gained,
##          OTHERWISE: leave everything as is.
##        
##
##  NOTE: 
##     * If the "yards_to_goal_end" is NA (which is a RARE CASE of the END OF HALF plays), then we just stick with the original values for yards_gained, yds_rushed, FOR SIMPLICITY
##


ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("safety")) & pbp$penalty_flag == FALSE)
ind.play.type

if (length(ind.play.type) > 0){
pbp$yards_gained[ind.play.type] <- ifelse(!is.na(pbp$yards_to_goal[ind.play.type]),
                                          -(100-pbp$yards_to_goal[ind.play.type]),
                                          pbp$yards_gained[ind.play.type])
pbp$yds_rushed[ind.play.type] <- ifelse(is.na(pbp$yds_rushed[ind.play.type]), 
                                        NA, 
                                        pbp$yards_gained[ind.play.type])
}


ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("safety")) & pbp$penalty_flag == TRUE)
ind.play.type

if (length(ind.play.type) > 0){
  
pbp$yards_gained[ind.play.type] <- ifelse(!is.na(pbp$yards_to_goal[ind.play.type]),
                                          -(100-pbp$yards_to_goal[ind.play.type]),
                                          pbp$yards_gained[ind.play.type])
pbp$yds_rushed[ind.play.type] <- NA 
pbp$yds_penalty[ind.play.type] <- pbp$yards_gained[ind.play.type]

}








####
##   "rush" plays
##      "rushing touchdown":
##          just make  yds_gained = yds_rush = yds_to_goal, for EVERYBODY
##      "rush" play:
##          For penalty.flag == FALSE, there are a couple sporadic issues, but STICKING TO ORIGINAL YARDS_GAINED/RUSHED VALUES IS MOST RELIABLE HERE (NO CHANGE)
##          For change_of_pos_team == 0 & penalty.flag == FALSE - there are some TRULY BAD RECORDINGS, so GOTTA DROP THOSE ALTOGETHER.
##          For  change_of_pos_team == 1 & penalty.flag == FALSE there were no issues.
####

# There's just 1 recording with mismatch (21 yards gained, while yards_to_goal indicate 31),
#  I still think replacing values is just fine.

ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("rushing touchdown")))
ind.play.type

# Number of problematic observations:
if (length(ind.play.type) > 0){
  
# print("problematic, not for removal")
# print(sum(pbp[ind.play.type, "yards_gained"] != pbp[ind.play.type, "yards_to_goal"],
#           na.rm=T))

pbp$yards_gained[ind.play.type] <- ifelse(!is.na(pbp$yards_to_goal[ind.play.type]),
                                          pbp$yards_to_goal[ind.play.type],
                                          pbp$yards_gained[ind.play.type])
pbp$yds_rushed[ind.play.type] <- ifelse(!is.na(pbp$yards_gained[ind.play.type]),
                                        pbp$yards_gained[ind.play.type],
                                        pbp$yds_rushed[ind.play.type])
}


## (pbp$play_type == "Rush" & pbp$change_of_pos_team == 0 & pbp$penalty_flag == FALSE)
## LEAVE THE "RUSH_YDS/YDS_GAINED" VALUES AS IS => THEY SEEM MORE CONSISTENT
#
# For more detail:
## "400547982-21" -  the "within 2yds" issue. Yards_to_goal is not quite right (should be 49 instead of 51)
## "400548043-15" - flips "100-X" due to wrong "change_of_pos" value.. it's on "downs turnover" and is still at 0
## "400547718-24" - also issue with "within 2yds" stuff..

ind.play.type <- which(pbp$play_type == "Rush" & pbp$change_of_pos_team == 0 & pbp$penalty_flag == FALSE)
ind.play.type

if (length(ind.play.type) > 0){
  
# Number of problematic observations:
# print("problematic, not for removal")
# print(sum(pbp[ind.play.type, "yards_gained"] != (pbp[ind.play.type, "yards_to_goal"] - pbp[ind.play.type, "yards_to_goal_end"]),
#           na.rm=T))

}


### (pbp$play_type == "Rush" & pbp$change_of_pos_team == 1 & pbp$penalty_flag == FALSE)
## LEAVE THE "RUSH_YDS/YDS_GAINED" VALUES AS IS => THEY SEEM MORE CONSISTENT
#
#  JUST ONE mishap, due to bad "change_of_pos" value (=1 on a rush before the FG)

ind.play.type <- which(pbp$play_type == "Rush" & pbp$change_of_pos_team == 1 & pbp$penalty_flag == FALSE)
ind.play.type


if (length(ind.play.type) > 0){
  
# Number of problematic observations:
# print("problematic, not for removal")
# print(sum(pbp[ind.play.type, "yards_gained"] != (pbp[ind.play.type, "yards_to_goal"] - (100-pbp[ind.play.type, "yards_to_goal_end"])),
#           na.rm=T))

}
  




### (pbp$play_type == "Rush" & pbp$change_of_pos_team == 0 & pbp$penalty_flag == TRUE)
## LEAVE THE "RUSH_YDS/YDS_GAINED" VALUES AS IS => THEY SEEM MORE CONSISTENT
#
#   10 BAD HALVES:
#       * "yards_to_goal" values just don't make sense for the text of the play..
#       * "yards_to_goal_end" values are hit-n-miss..
#   WHILE the YARDS_GAINED/RUSHED/PENALTY are GOOD as far as NUMBER MENTIONS IN THE TEXT.. but NOT AS FAR AS the FIELD POSITIONS
#  THE CATCH is that the TEXT OF THE PLAY IS ITSELF INCONSISTENT SOMETIMES:
# "Devin Gardner run for 6 yds to the MICHIGAN 46 for a 1ST down APPALACHIAN ST Penalty, Offensive Holding (15 Yards) to the AppSt 41 for a 1ST down"
#  Michigan driving, from Michigan 40, ending up at AppSt 41, should be 19 yards, but play says 6 rush yds + 15 penalty yds = 21
#
#  The PREVIOUS PLAY seems to MAKE SENSE for the "yards_to_goal_end" standpoint.. corresponds to the text.
#
# It's just BAD DATA RECORDING... PROBABLY NEED TO DROP.
#
#  See 400547943-2, "400547988-20", 


ind.play.type <- which(pbp$play_type == "Rush" & pbp$change_of_pos_team == 0 & pbp$penalty_flag == TRUE)
ind.play.type

if (length(ind.play.type) > 0){
  
# PROBLEMATIC (FOR REMOVAL):
# print(sum(pbp[ind.play.type, "yards_gained"] != (pbp[ind.play.type, "yards_to_goal"] - pbp[ind.play.type, "yards_to_goal_end"]),
#           na.rm=T))
  
}


bad.ind <- which(pbp$play_type == "Rush" & pbp$change_of_pos_team == 0 & pbp$penalty_flag == TRUE & 
                   pbp$yards_gained != (pbp$yards_to_goal - pbp$yards_to_goal_end))
bad.ind

dim(pbp)
pbp <- pbp %>% filter(!game_id_half %in% pbp$game_id_half[bad.ind])
dim(pbp)


### (pbp$play_type == "Rush" & pbp$change_of_pos_team == 1 & pbp$penalty_flag == TRUE)
## NO ISSUES, so LEAVE THE "RUSH_YDS/YDS_GAINED" VALUES AS IS => THEY SEEM MORE CONSISTENT

ind.play.type <- which(pbp$play_type == "Rush" & pbp$change_of_pos_team == 1 & pbp$penalty_flag == TRUE)
ind.play.type

if (length(ind.play.type) > 0){
  
# Number of problematic observations:
# print("problematic, not for removal")
# print(sum(pbp[ind.play.type, "yards_gained"] != (pbp[ind.play.type, "yards_to_goal"] - (100-pbp[ind.play.type, "yards_to_goal_end"])),
#           na.rm=T))

}

####
##   "Pass" plays
##      "Passing touchdown":
##          just make  yds_gained = yds_Pass = yds_to_goal, for EVERYBODY
##      "Pass" play:
##          For penalty.flag == FALSE, there are a couple sporadic issues, but STICKING TO ORIGINAL YARDS_GAINED/PassED VALUES IS MOST RELIABLE HERE (NO CHANGE)
##          For change_of_pos_team == 0 & penalty.flag == FALSE - there are some TRULY BAD RECORDINGS, so GOTTA DROP THOSE ALTOGETHER.
##          For  change_of_pos_team == 1 & penalty.flag == FALSE there were no issues.
####

# There's 3 recordings with mismatches, 2 are negligible difference, and 1 is badly recorded.. I still think replacing values is just fine.
ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("passing touchdown")))
ind.play.type

if (length(ind.play.type) > 0){
  
# Number of problematic observations:
# print("problematic, not for removal")
# print(sum(pbp[ind.play.type, "yards_gained"] != pbp[ind.play.type, "yards_to_goal"],
#           na.rm=T))

pbp$yards_gained[ind.play.type] <- ifelse(!is.na(pbp$yards_to_goal[ind.play.type]),
                                          pbp$yards_to_goal[ind.play.type],
                                          pbp$yards_gained[ind.play.type])
pbp$yds_rushed[ind.play.type] <- ifelse(!is.na(pbp$yards_gained[ind.play.type]),
                                        pbp$yards_gained[ind.play.type],
                                        pbp$yds_rushed[ind.play.type])

}

####
## For PASS INCOMPLETIONS:
##
##  CHANGE their "yds_receiving" from NA to 0, so that it's clear that we're dealing with a passing play
##  (NOTE: "yards_gained" can vary due to PENALTIES.. so they don't always have to be zero)
###

ind.play.type <- which(pbp$play_type == "Pass Incompletion")
#& pbp$change_of_pos_team == 0 & pbp$penalty_flag == FALSE)
ind.play.type

if (length(ind.play.type) > 0){
  
pbp$yds_receiving[ind.play.type] <- 0

}

# table(pbp[ind.play.type, "yards_gained"])
# table(pbp[ind.play.type, "yds_receiving"])
# table(pbp[ind.play.type, "yds_penalty"])



####
## For PASS RECEPTIONS
####

## (pbp$play_type == "Pass" & pbp$change_of_pos_team == 0 & pbp$penalty_flag == FALSE)
## LEAVE THE "Pass_YDS/YDS_GAINED" VALUES AS IS => THEY SEEM MORE CONSISTENT
#

ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("pass ")) & pbp$change_of_pos_team == 0 & pbp$penalty_flag == FALSE)
ind.play.type

# Number of problematic observations:
##  4 problematic ones: 3 incompletions, where yds_gained are properly at 0, and yards_to_goal are within 2yds of each other at least..
##                      1 completion, and the TEXT is CONFLICTING
##                       (Tommy Armstrong Jr. pass complete to Brandon Reilly for 12 yds to the Neb 44 for a 1ST down lat. to Kenny Bell for a loss of 2 yards to the Neb 44 for a 1ST down)
##   LEAVE AS IS..

if (length(ind.play.type) > 0){
  
# print("problematic, not for removal")
# print(sum(pbp[ind.play.type, "yards_gained"] != (pbp[ind.play.type, "yards_to_goal"] - pbp[ind.play.type, "yards_to_goal_end"]),
#           na.rm=T))

}




### (pbp$play_type == "Pass" & pbp$change_of_pos_team == 1 & pbp$penalty_flag == FALSE)
## LEAVE THE "Pass_YDS/YDS_GAINED" VALUES AS IS => THEY SEEM MORE CONSISTENT
#
#   Just 1 issue: bad change_of_pos value for one observation.
#    Leaving yds_gained as is should work though.

ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("pass ")) & pbp$change_of_pos_team == 1 & pbp$penalty_flag == FALSE)
ind.play.type

if (length(ind.play.type) > 0){
  
# Number of problematic observations:
# print("problematic, not for removal")
# print(sum(pbp[ind.play.type, "yards_gained"] != (pbp[ind.play.type, "yards_to_goal"] - (100-pbp[ind.play.type, "yards_to_goal_end"])),
#           na.rm=T))

}


### (pbp$play_type == "Pass" & pbp$change_of_pos_team == 0 & pbp$penalty_flag == TRUE)
## LEAVE THE "Pass_YDS/YDS_GAINED" VALUES AS IS => THEY SEEM MORE CONSISTENT
#
#   Some BAD RECORDING... SHOULD REMOVE THESE.


ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("pass ")) & pbp$change_of_pos_team == 0 & pbp$penalty_flag == TRUE)
ind.play.type

if (length(ind.play.type) > 0){
  
## PROBLEMATIC OBSERVATIONS (FOR REMOVAL):
# print(sum(pbp[ind.play.type, "yards_gained"] != (pbp[ind.play.type, "yards_to_goal"] - pbp[ind.play.type, "yards_to_goal_end"]),
#           na.rm=T))
  
}

bad.ind <- which(str_detect(tolower(pbp$play_type), fixed("pass ")) & pbp$change_of_pos_team == 0 & pbp$penalty_flag == TRUE & 
                   pbp$yards_gained != (pbp$yards_to_goal - pbp$yards_to_goal_end))
bad.ind

dim(pbp)
pbp <- pbp %>% filter(!game_id_half %in% pbp$game_id_half[bad.ind])
dim(pbp)


### (pbp$play_type == "Pass" & pbp$change_of_pos_team == 1 & pbp$penalty_flag == TRUE)
## NO ISSUES, so LEAVE THE "Pass_YDS/YDS_GAINED" VALUES AS IS => THEY SEEM MORE CONSISTENT

ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("pass ")) & pbp$change_of_pos_team == 1 & pbp$penalty_flag == TRUE)
ind.play.type

if (length(ind.play.type) > 0){
  
# Number of problematic observations:
# print("problematic, not for removal")
# print(sum(pbp[ind.play.type, "yards_gained"] != (pbp[ind.play.type, "yards_to_goal"] - (100-pbp[ind.play.type, "yards_to_goal_end"])),
#           na.rm=T))

}


# View(# pbp[ind.play.type[which(pbp[ind.play.type, "yards_gained"] != pbp[ind.play.type, "yards_to_goal"])], ] %>%
#   pbp[ind.play.type[which(pbp[ind.play.type, "yards_gained"] != (pbp[ind.play.type, "yards_to_goal"] - pbp[ind.play.type, "yards_to_goal_end"]))], ] %>%
#      # pbp[ind.play.type[which(pbp[ind.play.type, "yards_gained"] != (pbp[ind.play.type, "yards_to_goal"] - (100-pbp[ind.play.type, "yards_to_goal_end"])))], ] %>%
#     # pbp %>% filter(game_id_drive %in% c("400547673-20")) %>%
#     select(game_id_drive,
#            pos_team, def_pos_team, drive_number, change_of_pos_team, drive_result_detailed, play_type, play_text,
#            yards_to_goal, yards_to_goal_end,
#            yards_gained, yds_rushed, yds_receiving,
#            rush_yds, reception_yds, yds_fumble_return,
#            penalty_flag, yds_penalty,
#            pos_team_score, def_pos_team_score, clock.minutes, clock.seconds))






############
## Checking BLOCKED & MISSED/RETURNED FGs
############

# "blocked field goal", "missed field goal return"
## If one tried to return a missed FG, wherever they get tackled - that's where the ball gets placed. HENCE THE RISK.

## For cases when FG is blocked AND:
##    * the blocking team keeps possession (change_of_pos_team == 1), we do 
##        yards_gained = yards_to_goal - (100 - yards_to_goal_end)
##        yds_block_return = yards_gained
##    * the blocking team picks up the ball, and then FUMBLES IT, to be RECOVERED BACK by the KICKING TEAM (change_of_pos_team == 0)
##        yards_gained = yards_to_goal - yards_to_goal_end
##        yds_block_return = yards_gained
##    * blocking team RETURNS FOR A TOUCHDOWN:
##        yards_gained = yards_to_goal - 100
##        yds_block_return = yards_gained
## ## For cases when FG is missed AND returned:
##    * with returning team keeping the possessions:
##        yards_gained = yards_to_goal - (100 - yards_to_goal_end)
##        yds_fg_return = yards_gained
##    * the returning team FUMBLES IT, to be RECOVERED BACK by the KICKING TEAM (change_of_pos_team == 0)
##        yards_gained = yards_to_goal - yards_to_goal_end
##        yds_block_return = yards_gained

##    * when there's ALSO AN NON-DECLINED PENALTY => the PENALTY YARDAGE ISN'T REPORTED.. HENCE WE WON'T BE "PIECING IT" AWAY from BLOCK RETURN YARDAGE

pbp$yds_fg_return <- NA

ind.play.type <- which((tolower(pbp$play_type) %in% c("blocked field goal", "missed field goal return")) & pbp$change_of_pos_team == 1)

if (length(ind.play.type) > 0){
  pbp$yards_gained[ind.play.type] <- pbp$yards_to_goal[ind.play.type] - (100 -  pbp$yards_to_goal_end[ind.play.type])
  pbp$yds_fg_return[ind.play.type] <- pbp$yards_gained[ind.play.type]
}

ind.play.type <- which((tolower(pbp$play_type) %in% c("blocked field goal", "missed field goal return")) & pbp$change_of_pos_team == 0)
if (length(ind.play.type) > 0){
  pbp$yards_gained[ind.play.type] <- pbp$yards_to_goal[ind.play.type] - pbp$yards_to_goal_end[ind.play.type]
  pbp$yds_fg_return[ind.play.type] <- pbp$yards_gained[ind.play.type]
}

ind.play.type <- which((tolower(pbp$play_type) == "blocked field goal touchdown"))
if (length(ind.play.type) > 0){
  pbp$yards_gained[ind.play.type] <- pbp$yards_to_goal[ind.play.type] - 100
  pbp$yds_fg_return[ind.play.type] <- pbp$yards_gained[ind.play.type]
}


## If it's either of FG made or missed, then put "yards gained" to 0
ind.play.type <- (tolower(pbp$play_type) %in% c("field goal good", "field goal missed"))
if (length(ind.play.type) > 0){
  pbp$yards_gained[ind.play.type] <- 0
}

## Some MISSED FG are constantly followed by "YARDS_TO_GOAL = 80" FOR SOME REASON
##  https://en.wikipedia.org/wiki/Field_goal#:~:text=Missed%20field%20goals%20attempted%20from,the%20spot%20of%20the%20kick.
##  "Missed field goals attempted from the defending team's 20-yard line or closer result in the defense taking possession at their 20-yard line. "

# ind.play.type <- (tolower(pbp$play_type) %in% c("field goal missed"))
#pbp$yards_gained[ind.play.type] <- 0


#####
## CHANGING THE DRIVE NUMBERS, in case THEY SWITCHED:
#####

## CHECKING IF THERE ARE UNWARRANTED DRIVE NUMBER CHANGES (which sometimes happens when the ball changes TEAMS during ONE PLAY):
##   We check if the "change_of_pos = 0", "yards_to_goal_end" is not NA (which would indicate the pre-End of Half play),
##     and the DRIVE NUMBER STILL CHANGES => THAT INDICATES THE DYNAMIC WE WANT TO AVOID.. so we just need to ASSIGN THE 2ND HALF OF THE DRIVE VALUES TO THE ENTIRE POSSESSION


ind.play.type <- which((tolower(pbp$play_type) %in% c("blocked field goal", "missed field goal return")) &  pbp$change_of_pos_team == 0 &
                         !is.na(pbp$yards_to_goal_end) &
                         c(diff(pbp$drive_number), NA) != 0
)
ind.play.type

# print("Unwarranted drive changes (during back-n-forth fumbles)")
# print(length(unique(pbp$game_id_half[ind.play.type])))

if (length(ind.play.type) > 0){
  drive.number.pre.int <- pbp$game_id_drive[ind.play.type]
  drive.number.post.int <- pbp$game_id_drive[ind.play.type+1]
  drive.number.pre.int
  drive.number.post.int
  
  for (drive.num.j in 1:length(drive.number.pre.int)){
    pbp[pbp$game_id_drive == drive.number.pre.int[drive.num.j], c("game_id_drive", "drive_number", "drive_result_detailed")] <- head(pbp[pbp$game_id_drive == drive.number.post.int[drive.num.j], c("game_id_drive", "drive_number", "drive_result_detailed")], 1)
  }
}


# View(pbp[sort(c(ind.play.type, ind.play.type+1)),] %>%
#        # pbp[ind.play.type[which(pbp[ind.play.type, "yards_gained"] != (pbp[ind.play.type, "yards_to_goal"] - pbp[ind.play.type, "yards_to_goal_end"]))], ] %>%
#        #  filter(game_id_half %in% bad.switch.halves.vol.3) %>%
#        #pbp %>% filter(game_id_drive %in% c("400547773-13", "400547773-14")) %>%
#        select(game_id_drive,
#               pos_team, def_pos_team, drive_number, change_of_pos_team, drive_result_detailed, play_type, play_text,
#               yards_to_goal, yards_to_goal_end,
#               yards_gained, yds_rushed, yds_receiving,
#               rush_yds, reception_yds, yds_fumble_return,
#               penalty_flag, yds_penalty,
#               pos_team_score, def_pos_team_score))



############
## Checking PUNTS (Regular, Blocked, Touchdown, Fumble etc etc)
############

## NOTES on YARDAGES DURING PUNT PLAYS:
##    * "yds_punt_gained" seems like GARBAGE...
##    * "yds_punt_return" is always non-negative, even when text clearly says "returned for a LOSS", and "yards_gained" actually shows NEGATIVE value
##       !!! "yds_punted", ON THE OTHER HAND, seems GOOD TO GO. !!!
##    * when there's ALSO AN NON-DECLINED PENALTY => the PENALTY YARDAGE ISN'T REPORTED.. HENCE WE WON'T BE "PIECING IT" AWAY from PUNT RETURN YARDAGE
##          (will probably be MIXING ALL INTO "yds_punt_return")
##
## FOR ILLUSTRATION: check drive "400547772-7"
# ind.play.type <- which(pbp$play_type == "Punt"  & pbp$change_of_pos_team == 1 & 
#                          pbp$penalty_flag & (pbp$yards_to_goal - (100-pbp$yards_to_goal_end) != (pbp$yds_punted - pbp$yds_punt_return)))
# ind.play.type

##
##  SO, MAIN TAKEAWAY:
##    * set "yds_punt_gained" to NA
##    * For pure punt length, we are good using the original "yds_punted" values
##    * For NET yardage, AKA yards_gained, will HAVE to use "yards_to_goal - yards_to_goal_end"-style stuff
##    * To deal with the issues of "yds_punt_return", we'll actually REPLACE those with "yds_punted - yards_gained".


# [1] "Punt"                                "Punt Team Fumble Recovery"           "Punt Return Touchdown"               "Blocked Punt"                       
# [5] "Blocked Punt Touchdown"              "Blocked Punt (Safety)"               "Punt Team Fumble Recovery Touchdown"

unique(pbp$play_type[str_detect(tolower(pbp$play_type), "punt")])


## REGULAR PUNTS w/ CHANGE IN POSSESSION (pbp$change_of_pos_team == 1),
## also checking penalty_flag = TRUE/FALSE, and ACCOUNTING FOR TOUCHBACKS
##  
ind.play.type <- which(pbp$play_type == "Punt"  & pbp$change_of_pos_team == 1 
                     #  & !pbp$penalty_flag 
)
ind.play.type

if (length(ind.play.type) > 0){
  pbp$yds_punt_gained[ind.play.type] <- NA
  pbp$yards_gained[ind.play.type] <- pbp$yards_to_goal[ind.play.type] - (100 - pbp$yards_to_goal_end[ind.play.type])
  # If it's NOT a touchback: assign the punt return yards as follows
  non.touchback.punt.ids <- ind.play.type[which(!str_detect(tolower(pbp$play_text[ind.play.type]), "touchback"))]
  pbp$yds_punt_return[non.touchback.punt.ids] <- pbp$yds_punted[non.touchback.punt.ids] - pbp$yards_gained[non.touchback.punt.ids]
  # If it's a touchback: have it at NA
  touchback.punt.ids <- ind.play.type[which(str_detect(tolower(pbp$play_text[ind.play.type]), "touchback"))]
  pbp$yds_punt_return[touchback.punt.ids] <- NA
}








## REGULAR PUNTS w/ NO CHANGE IN POSSESSION (pbp$change_of_pos_team == 0),
## MAKING SURE THOSE ARE NOT "PRE-END OF HALF" (yards_to_goal_end != NA)
## also checking penalty_flag = TRUE/FALSE, and ACCOUNTING FOR TOUCHBACKS
##  
##  ONLY 15 OF THOSE:
##
##    1. For those where POSSESSION TEAM NAME CLEARLY CHANGED (9): treat them the same as regular punts, JUST ALSO CHANGE THE "change_of_pos_team" to 1
##      (400547728-1, 400547728-7 ...)

ind.play.type <- which(pbp$play_type == "Punt"  & pbp$change_of_pos_team == 0
                       & !is.na(pbp$yards_to_goal_end)
                       & (pbp$pos_team != lead(pbp$pos_team))
                       #  & !pbp$penalty_flag 
)
ind.play.type

if (length(ind.play.type) > 0){
  pbp$yds_punt_gained[ind.play.type] <- NA
  pbp$yards_gained[ind.play.type] <- pbp$yards_to_goal[ind.play.type] - (100 - pbp$yards_to_goal_end[ind.play.type])
  # If it's NOT a touchback: assign the punt return yards as follows
  non.touchback.punt.ids <- ind.play.type[which(!str_detect(tolower(pbp$play_text[ind.play.type]), "touchback"))]
  pbp$yds_punt_return[non.touchback.punt.ids] <- pbp$yds_punted[non.touchback.punt.ids] - pbp$yards_gained[non.touchback.punt.ids]
  # If it's a touchback: have it at NA
  touchback.punt.ids <- ind.play.type[which(str_detect(tolower(pbp$play_text[ind.play.type]), "touchback"))]
  pbp$yds_punt_return[touchback.punt.ids] <- NA

  # And CHANGING ITS "CHANGE_OF_POS" to 1, as the possession clearly changed
  pbp$change_of_pos_team[ind.play.type] <- 1
}

##    2. For those where POSSESSION TEAM NAME DID NOT CHANGE (8, but several are from the same game)
##       THESE ARE JUST TERRIBLY RECORDED... ISSUES ALL OVER THE PLACE: 
##           either with "yards_to_goal_end" veracity,
##           some plays literally have empty text,
##           others are actually punt return touchdown, not just punt...
##    
##   !!! DELETE THESE HALVES !!!
##      




ind.play.type <- which(pbp$play_type == "Punt"  & pbp$change_of_pos_team == 0
                       & !is.na(pbp$yards_to_goal_end)
                       & (pbp$pos_team == lead(pbp$pos_team))
                       #  & !pbp$penalty_flag 
)
ind.play.type


### BAD OBSERVATIONS (FOR REMOVAL)
# print(length(unique(pbp$game_id_half[ind.play.type])))

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% unique(pbp$game_id_half[ind.play.type]), ]
dim(pbp)



######
## 3. NOTE: SEEMS LIKE DRIVES THAT GET EXTENDED BY ROUGHING/RUNNING INTO THE KICKER got turned into PURE PENALTY PLAYS
##    AND THE DRIVE NUMBER DOESN'T CHANGE (although we later need to actually change it)
######

ind.play.type <-  which(str_detect(tolower(pbp$play_text), "punt") & (str_detect(tolower(pbp$play_text), "1st down")) & pbp$penalty_no_play
                      # & pbp$change_of_pos_team == 0
                      # & !is.na(pbp$yards_to_goal_end)
                      # & (pbp$pos_team == lead(pbp$pos_team))
                     #  & (pbp$game_id_drive != lead(pbp$game_id_drive))
)
ind.play.type



###
## For PUNT RETURNS PRIOR TO THE END OF HALVES that HAVE 0 CHANGE OF POSSESSION:
## 
## Can't rely on "yards_to_goal_end" here, but "yds_punted" and "yds_punt_return" appear OK,
## so just calculate "yards_gained" via: yds_punted - yds_punt_return (OR for touchbacks: yards_to_goal - 20)

ind.play.type <- which(pbp$play_type == "Punt"  & pbp$change_of_pos_team == 0
                       & is.na(pbp$yards_to_goal_end))

if (length(ind.play.type) > 0){
  pbp$yds_punt_gained[ind.play.type] <- NA
  # If it's NOT a touchback: assign the punt return yards as follows
  non.touchback.punt.ids <- ind.play.type[which(!str_detect(tolower(pbp$play_text[ind.play.type]), "touchback"))]
  pbp$yards_gained[non.touchback.punt.ids] <- pbp$yds_punted[non.touchback.punt.ids] - pbp$yds_punt_return[non.touchback.punt.ids]
  # If it's a touchback: have it at NA
  touchback.punt.ids <- ind.play.type[which(str_detect(tolower(pbp$play_text[ind.play.type]), "touchback"))]
  pbp$yards_gained[touchback.punt.ids] <- pbp$yards_to_goal[touchback.punt.ids] - 20
  pbp$yds_punt_return[touchback.punt.ids] <- NA
}
                       
                       
                       
## Any other Punt Return plays?
## Seems like not, which is good.

ind.play.type <- which(pbp$play_type == "Punt"  & 
                         !((pbp$change_of_pos_team == 1)  |
                             (pbp$change_of_pos_team == 0 & !is.na(pbp$yards_to_goal_end) & (pbp$pos_team != lead(pbp$pos_team))) |
                             (pbp$change_of_pos_team == 0 & !is.na(pbp$yards_to_goal_end) & (pbp$pos_team == lead(pbp$pos_team))) | 
                             (pbp$change_of_pos_team == 0 & is.na(pbp$yards_to_goal_end)))
                       #  & !pbp$penalty_flag 
)
ind.play.type

## NUMBER OF UNNACOUNTED FOR PUNT RETURN TOUCHDOWN PLAYS:
# print(length(ind.play.type))

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% unique(pbp$game_id_half[ind.play.type]),]
dim(pbp)






## PUNT RETURN TOUCHDOWNS w/ CHANGE IN POSSESSION (pbp$change_of_pos_team == 1)
##   - Which SHOULD NOT BE HAPPENING
## also checking penalty_flag = TRUE/FALSE
##  
##  These seem to be the "Timeout" play issues: a "Timeout drive" is getting skipped, treating it as "change of possession"
##  For these, we set "change_of_pos_team" to 0.
##
##   ISSUE: "yds_punted" is not always properly available (e.g. see drive "400553383-15", where it's NA), BUT it should be calculable from:
##     yards_gained (which shows the length of punt return) - (100 - yards_to_goal), SO..
##     if "yds_punted" is not NA - use it; otherwise, use "yards_gained - (100-yards_to_goal)
##
##   yards_gained = - (100 - yards_to_goal)
##   yds_punt_return = yds_punted - yards_gained

ind.play.type <- which(pbp$play_type == "Punt Return Touchdown"  & pbp$change_of_pos_team == 1 
                       & !is.na(pbp$yards_to_goal_end) & pbp$pos_team == lead(pbp$pos_team)
                       #  & !pbp$penalty_flag 
)
ind.play.type

if (length(ind.play.type) > 0){
  pbp$yds_punt_gained[ind.play.type] <- NA
  pbp$yds_punted[ind.play.type] <- ifelse(is.na(pbp$yds_punted[ind.play.type]), 
                                          pbp$yards_gained[ind.play.type] - (100 - pbp$yards_to_goal[ind.play.type]),
                                          pbp$yds_punted[ind.play.type])
  pbp$yards_gained[ind.play.type] <- - (100 - pbp$yards_to_goal[ind.play.type])
  # If it's NOT a touchback: assign the punt return yards as follows
  pbp$yds_punt_return[ind.play.type] <- pbp$yds_punted[ind.play.type] - pbp$yards_gained[ind.play.type]

  pbp$change_of_pos_team[ind.play.type] <- 0
}



## PUNT RETURN TOUCHDOWNS w/ NO CHANGE IN POSSESSION (pbp$change_of_pos_team == 0), which is the way IT SHOULD BE
## 
## also checking penalty_flag = TRUE/FALSE
##
## All the exact same stuff as with "change_of_pos =1", besides changing the "change_of_pos" itself part.
##
##  NOTE: THERE MIGHT BE NEGATIVE "yds_punted" VALUES - THOSE CORRESPOND TO BAD RECORDINGS...
##       MIGHT CONSIDER DROPPING THOSE LATER

ind.play.type <- which(pbp$play_type == "Punt Return Touchdown"  & pbp$change_of_pos_team == 0
                        & pbp$pos_team == lead(pbp$pos_team)
                       #  & !pbp$penalty_flag 
)
ind.play.type

if (length(ind.play.type) > 0){
  pbp$yds_punt_gained[ind.play.type] <- NA
  pbp$yds_punted[ind.play.type] <- ifelse(is.na(pbp$yds_punted[ind.play.type]), 
                                          pbp$yards_gained[ind.play.type] - (100 - pbp$yards_to_goal[ind.play.type]),
                                          pbp$yds_punted[ind.play.type])
  pbp$yards_gained[ind.play.type] <- - (100 - pbp$yards_to_goal[ind.play.type])
  # If it's NOT a touchback: assign the punt return yards as follows
  pbp$yds_punt_return[ind.play.type] <- pbp$yds_punted[ind.play.type] - pbp$yards_gained[ind.play.type]
}

## Any other "Punt Return Touchdown" plays?
## Seems like not, which is good.

ind.play.type <- which(pbp$play_type == "Punt Return Touchdown"  & 
                         !((pbp$change_of_pos_team == 1 & !is.na(pbp$yards_to_goal_end) & pbp$pos_team == lead(pbp$pos_team)) | 
                             (pbp$change_of_pos_team == 0 & pbp$pos_team == lead(pbp$pos_team)))
                       #  & !pbp$penalty_flag 
)
ind.play.type

## NUMBER OF UNNACOUNTED FOR PUNT RETURN TOUCHDOWN PLAYS:
# print(length(ind.play.type))

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% unique(pbp$game_id_half[ind.play.type]),]
dim(pbp)




####
## "Punt Team Fumble Recovery" w/ CHANGE IN POSSESSION (pbp$change_of_pos_team == 1), which evidently IT SHOULD NOT BE:
##   FOR NOW (before we do the adjustments later) it just counts as RETENTION OF POSSESSION BY THE PUNTING TEAM
## also checking penalty_flag = TRUE/FALSE
##
##  No such cases, which is good.
####

## (I just put it at the end in "All other cases".)


####
## "Punt Team Fumble Recovery" w/ NO CHANGE IN POSSESSION (pbp$change_of_pos_team == 0), 
## which is WHAT SHOULD BE FOR NOW (before we do the adjustments later), as it just counts as RETENTION OF POSSESSION BY THE PUNTING TEAM
##
##  NOTE: While it DOESN'T CHANGE THE POSSESSION, it DOES CHANGE THE DRIVE NUMBER!!! 
##      WHICH IS ACTUALLY GOOD FOR US, as we will BE ADDING A "SINGLE-PLAY POSSESSION" FOR THE RETURNING TEAM LATER ANYWAY (their fumble)
##
##  yds_punted is good here
##
## NEED TO MAKE:
##  yds_gained = yds_to_goal - yds_to_goal_end
##  yds_punt_return = yds_gained - yds_punted (so pretty much what happened since the returned caught it/tried catching it)
##     AND IT SHOULD BE FROM THE PERSPECTIVE OF THE RETURNING TEAM.. so COULD DEFINITELY BE NEGATIVE if FUMBLED GOT PICKED UP AND CARRIED FOR A BIT
####

ind.play.type <- which(pbp$play_type == "Punt Team Fumble Recovery"  & pbp$change_of_pos_team == 0
                        & !is.na(pbp$yards_to_goal_end) & (pbp$pos_team == lead(pbp$pos_team)) & (pbp$drive_number != lead(pbp$drive_number)))
                       #  & !pbp$penalty_flag 

ind.play.type

if (length(ind.play.type) > 0){
  pbp$yds_punt_gained[ind.play.type] <- NA
  pbp$yards_gained[ind.play.type] <- pbp$yards_to_goal[ind.play.type] - pbp$yards_to_goal_end[ind.play.type]
  pbp$yds_punt_return[ind.play.type] <- pbp$yds_punted[ind.play.type] - pbp$yards_gained[ind.play.type] 
}



## Any other "Punt Team Fumble Recovery" plays?
## Seems like not, which is good.

ind.play.type <- which(pbp$play_type == "Punt Team Fumble Recovery"  & 
                         !((pbp$change_of_pos_team == 0 & !is.na(pbp$yards_to_goal_end) & (pbp$pos_team == lead(pbp$pos_team)) & (pbp$drive_number != lead(pbp$drive_number))))
                       #  & !pbp$penalty_flag 
)
ind.play.type

## NUMBER OF UNNACOUNTED FOR PUNT RETURN TOUCHDOWN PLAYS:
# print(length(ind.play.type))

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% unique(pbp$game_id_half[ind.play.type]),]
dim(pbp)





####
## "Punt Team Fumble Recovery Touchdown" w/ CHANGE IN POSSESSION (pbp$change_of_pos_team == 1),
##   which is THE WAY IT SHOULD Be.. AT LEAST FOR NOW (before we do the adjustments later) it just counts as RETENTION OF POSSESSION BY THE PUNTING TEAM
##
##  THESE ARE SUPER RARE
##
##   yds_punted is good here
##     GOTTA MAKE:
##        yards_gained = yards_to_goal
##        yds_punt_return = yds_punted - yards_gained
####

ind.play.type <- which(pbp$play_type == "Punt Team Fumble Recovery Touchdown"  & pbp$change_of_pos_team == 1
                        & !is.na(pbp$yards_to_goal_end) & pbp$pos_team != lead(pbp$pos_team)
                       #  & !pbp$penalty_flag 
)
ind.play.type

if (length(ind.play.type) > 0){
  pbp$yds_punt_gained[ind.play.type] <- NA
  pbp$yards_gained[ind.play.type] <- pbp$yards_to_goal[ind.play.type]
  pbp$yds_punt_return[ind.play.type] <- pbp$yds_punted[ind.play.type] - pbp$yards_gained[ind.play.type] 
}



## Any other "Punt Team Fumble Recovery Touchdown" plays?
## Seems like not, which is good.

ind.play.type <- which(pbp$play_type == "Punt Team Fumble Recovery Touchdown"  & 
                         !(pbp$change_of_pos_team == 1 & !is.na(pbp$yards_to_goal_end) & pbp$pos_team != lead(pbp$pos_team))
                       #  & !pbp$penalty_flag 
)
ind.play.type

## NUMBER OF UNNACOUNTED FOR "Punt Team Fumble Recovery Touchdown" PLAYS:
# print(length(ind.play.type))

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% unique(pbp$game_id_half[ind.play.type]),]
dim(pbp)





## BLOCKED PUNTS w/ CHANGE IN POSSESSION (pbp$change_of_pos_team == 1), which is THE WAY THOSE SHOULD BE
##    MAKING SURE THOSE ARE NOT "PRE-END OF HALF" (yards_to_goal_end != NA)
##    also checking penalty_flag = TRUE/FALSE,
##
##  yds_punted = 0, which it should be
##  yds_gained = ards_to_goal - (100 - yards_to_goal_end)
##  yds_punt_return = yds_gained


ind.play.type <- which(pbp$play_type == "Blocked Punt"  & pbp$change_of_pos_team == 1
                       & !is.na(pbp$yards_to_goal_end)
                       & (pbp$pos_team != lead(pbp$pos_team))
                       #  & !pbp$penalty_flag 
)
ind.play.type

if (length(ind.play.type) > 0){
  pbp$yds_punt_gained[ind.play.type] <- NA
  pbp$yards_gained[ind.play.type] <- pbp$yards_to_goal[ind.play.type] - (100 - pbp$yards_to_goal_end[ind.play.type])
  pbp$yds_punt_return[ind.play.type] <- pbp$yds_punted[ind.play.type] - pbp$yards_gained[ind.play.type]
}


## BLOCKED PUNTS w/ NO CHANGE IN POSSESSION (pbp$change_of_pos_team == 0), which SHOULD NOT BE HAPPENING
## MAKING SURE THOSE ARE NOT "PRE-END OF HALF" (yards_to_goal_end != NA)
## also checking penalty_flag = TRUE/FALSE,
##
##   THESE ARE THE CASES where BLOCKING TEAM FUMBLES THE BALL and the PUNTING TEAM PICKS IT BACK UP.
##   It results in CHANGE IN DRIVE NUMBER, BUT NOT THE POSSESSING TEAM, which WORKS FOR US.

ind.play.type <- which(pbp$play_type == "Blocked Punt"  & pbp$change_of_pos_team == 0
                       & !is.na(pbp$yards_to_goal_end)
                       & (pbp$pos_team == lead(pbp$pos_team))
                       #  & !pbp$penalty_flag 
)
ind.play.type

if (length(ind.play.type) > 0){
  pbp$yds_punt_gained[ind.play.type] <- NA
  pbp$yards_gained[ind.play.type] <- pbp$yards_to_goal[ind.play.type] - pbp$yards_to_goal_end[ind.play.type]
  pbp$yds_punt_return[ind.play.type] <- pbp$yds_punted[ind.play.type] - pbp$yards_gained[ind.play.type]
}






## Any other "Punt Team Fumble Recovery Touchdown" plays?
## Seems like not, which is good.

## BLOCKED PUNTS w/ NO CHANGE IN POSSESSION (pbp$change_of_pos_team == 0), which SHOULD NOT BE HAPPENING
## MAKING SURE THOSE ARE NOT "PRE-END OF HALF" (yards_to_goal_end != NA)
## also checking penalty_flag = TRUE/FALSE,



ind.play.type <- which(pbp$play_type == "Blocked Punt"  & 
                         !(pbp$change_of_pos_team == 1 & !is.na(pbp$yards_to_goal_end) & (pbp$pos_team != lead(pbp$pos_team)) | 
                             pbp$change_of_pos_team == 0 & !is.na(pbp$yards_to_goal_end) & (pbp$pos_team == lead(pbp$pos_team))))
                       #  & !pbp$penalty_flag 

ind.play.type

## NUMBER OF UNNACOUNTED FOR "Punt Team Fumble Recovery Touchdown" PLAYS:
# print(length(ind.play.type))

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% unique(pbp$game_id_half[ind.play.type]),]
dim(pbp)







#####
## "Blocked Punt Touchdown" w/ NO CHANGE IN POSSESSION (pbp$change_of_pos_team == 0), which is the way IT SHOULD BE
#####
## also checking penalty_flag = TRUE/FALSE
##
##  yds_punted are fine at 0, as they should be
##
##  yards_gained should be = -(100-yards_to_goal), as the possession flips, and to score a TD you need to go all the way
##  yds_punt_return is just the positive version of yards gained here, as yds_punted will be 0 for blocked punts


ind.play.type <- which(pbp$play_type == "Blocked Punt Touchdown"  & pbp$change_of_pos_team == 0
                       & pbp$pos_team == lead(pbp$pos_team)
                       #  & !pbp$penalty_flag 
)
ind.play.type

if (length(ind.play.type) > 0){
  pbp$yds_punt_gained[ind.play.type] <- NA
  pbp$yards_gained[ind.play.type] <- - (100 - pbp$yards_to_goal[ind.play.type])
  pbp$yds_punt_return[ind.play.type] <- pbp$yds_punted[ind.play.type] - pbp$yards_gained[ind.play.type]
}

## Any other "Punt Return Touchdown" plays?
## Seems like not, which is good.

ind.play.type <- which(pbp$play_type == "Blocked Punt Touchdown"  & 
                         !(pbp$change_of_pos_team == 0 & pbp$pos_team == lead(pbp$pos_team))
                       #  & !pbp$penalty_flag 
)
ind.play.type

## NUMBER OF UNNACOUNTED FOR PUNT RETURN TOUCHDOWN PLAYS:
# print(length(ind.play.type))

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% unique(pbp$game_id_half[ind.play.type]),]
dim(pbp)





#####
## "Blocked Punt (Safety)" w/ CHANGE IN POSSESSION (pbp$change_of_pos_team == 1), which is the way IT SHOULD BE
##   (Safeties CHANGE THE POSSESSION)
#####
##
##  SUPER RARE
##
##  yds_punted are fine at 0, as they should be
##
##  yards_gained should be = -(100-yards_to_goal), as the possession flips, and to score a TD you need to go all the way
##  yds_punt_return is just the positive version of yards gained here, as yds_punted will be 0 for blocked punts

ind.play.type <- which(pbp$play_type == "Blocked Punt (Safety)"  & pbp$change_of_pos_team == 1
                       & pbp$pos_team != lead(pbp$pos_team)
                       #  & !pbp$penalty_flag 
)
ind.play.type

if (length(ind.play.type) > 0){
  pbp$yds_punt_gained[ind.play.type] <- NA
  pbp$yards_gained[ind.play.type] <- - (100 - pbp$yards_to_goal[ind.play.type])
  pbp$yds_punt_return[ind.play.type] <- pbp$yds_punted[ind.play.type] - pbp$yards_gained[ind.play.type]
}

## Any other "Punt Return Touchdown" plays?
## Seems like not, which is good.

ind.play.type <- which(pbp$play_type == "Blocked Punt (Safety)"  & 
                         !(pbp$change_of_pos_team == 1 & pbp$pos_team != lead(pbp$pos_team))
                       #  & !pbp$penalty_flag 
)
ind.play.type

## NUMBER OF UNNACOUNTED FOR PUNT RETURN TOUCHDOWN PLAYS:
# print(length(ind.play.type))

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% unique(pbp$game_id_half[ind.play.type]),]
dim(pbp)








# View(#pbp[ind.play.type,] %>%
#   # pbp[sort(c(ind.play.type)),] %>%
#   # pbp[sort(c(ind.play.type, ind.play.type+1)),] %>%
#   pbp[sort(c(ind.play.type, ind.play.type-1, ind.play.type+1)),] %>%
#     # pbp[ind.play.type[which(pbp[ind.play.type, "yards_gained"] != (pbp[ind.play.type, "yards_to_goal"] - pbp[ind.play.type, "yards_to_goal_end"]))], ] %>%
#     #  filter(game_id_half %in% bad.switch.halves.vol.3) %>%
#     #pbp %>% filter(game_id_drive %in% c("400547773-13", "400547773-14")) %>%
#     select(game_id_drive,
#            pos_team, def_pos_team, drive_number, change_of_pos_team, drive_result_detailed, play_type, play_text,
#            yards_to_goal, yards_to_goal_end,
#            yards_gained, 
#            yds_punted, yds_punt_return, yds_punt_gained, 
#            # yds_rushed, yds_receiving,
#            # rush_yds, reception_yds, 
#            yds_fumble_return,
#            penalty_flag, yds_penalty,
#            pos_team_score, def_pos_team_score))







############
## Checking for YARDAGE "GAINED" on INTERCEPTION RETURNS
############


## For cases when INT is returned, and
##    * the intercepting team KEEPS THE BALL withOUT SCORING A TD
##        yds_after_int <- yds_gained
##        yds_gained <- yards_to_goal - (100 - yards_to_goal_end)
##    * the intercepting team SCORES, we do 
##        yds_after_int <- yds_gained
##        yds_gained <- yards_to_goal - 100
##    * the intercepting team FUMBLES the BALL BACK TO THE OFFENSE.. NO TOUCHBACK though,
##        yds_after_int <- yds_gained
##        yds_gained <- yards_to_goal - yards_to_goal_end
##    * if it's a TOUCHBACK with NO POSSESSION CHANGE (which is at the ENDS OF HALVES)
##        yds_after_int <- yds_gained
##        yds_gained <- yards_to_goal - 20
##    * ??? PENALTIES ???
##       Not really reflective of what's going on... feels like those might be SPOT penalties for the most part.. so just MERGE THEM into YARDS GAINED..



pbp$yds_after_int <- NA

ind.play.type <- (tolower(pbp$play_type) %in% c("interception return")) & pbp$change_of_pos_team == 1
if (length(ind.play.type) > 0){
  pbp$yds_after_int[ind.play.type] <- pbp$yards_gained[ind.play.type]
  pbp$yards_gained[ind.play.type] <- pbp$yards_to_goal[ind.play.type] - (100 -  pbp$yards_to_goal_end[ind.play.type])
}

ind.play.type <- (tolower(pbp$play_type) %in% c("interception return touchdown"))
if (length(ind.play.type) > 0){
  pbp$yds_after_int[ind.play.type] <- pbp$yards_gained[ind.play.type]
  pbp$yards_gained[ind.play.type] <- pbp$yards_to_goal[ind.play.type] - 100
}

## NOTE: there frequently is "change_of_pos_team = 0" during the interception due to END OF HALF, which would have "yards_to_goal_end = NA" => yards_gained will become NA
##   WE NEED TO DROP HALVES WHERE:
##         There's a INT return with no change of possession, NOT BEFORE END OF HALF (yards_to_goal_end NOT NA) while also NO FUMBLE RETURN DATA 

ind.play.type <- which((tolower(pbp$play_type) %in% c("interception return")) & pbp$change_of_pos_team == 0 & 
                         !is.na(pbp$yards_to_goal_end) &
                         is.na(pbp$yds_fumble_return) &
                         !str_detect(pbp$play_text, "touchback"))


## PROBLEMATIC PLAYS:
# print("problematic, but not up for removal")
# print(length(unique(pbp$game_id_half[ind.play.type])))

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% unique(pbp$game_id_half[ind.play.type]), ]
dim(pbp)


## For the interceptions with a fumble that returned back to the offense:
##   "yards_gained" seems to contain data on interception return length - so assign it to "yds_after_int"
##   while using "yards_to_goal - yards_to_goal_end" as the actual yards gained.
##
## NOTE: the DRIVE NUMBER/RESULT CHANGES HERE - MAKE SURE THAT'S NOT THE CASE. ASSIGN EVERYTHING TO THAT SECOND, POST-"ALMOST TURNOVER" PART.

## NOTE: ??? NEED TO GET BACK TO "FUMBLE RECOVERY" STUFF, CHECKING FOR SIMILAR SCENARIOS... ???

ind.play.type <- (tolower(pbp$play_type) %in% c("interception return")) & pbp$change_of_pos_team == 0 & 
                         !is.na(pbp$yards_to_goal_end) &
                         !is.na(pbp$yds_fumble_return) &
                         !str_detect(pbp$play_text, "touchback")

if (length(ind.play.type) > 0){
  pbp$yds_after_int[ind.play.type] <- pbp$yards_gained[ind.play.type]
  pbp$yards_gained[ind.play.type] <- pbp$yards_to_goal[ind.play.type] - pbp$yards_to_goal_end[ind.play.type]
}

#####
## CHANGING THE DRIVE NUMBERS, in case THEY SWITCHED:
#####

ind.play.type <- which((tolower(pbp$play_type) %in% c("interception return")) & pbp$change_of_pos_team == 0 & 
                         !is.na(pbp$yards_to_goal_end) &
                         !is.na(pbp$yds_fumble_return) &
                         !str_detect(pbp$play_text, "touchback") &
                         c(diff(pbp$drive_number), NA) != 0
                       )

# print("Unwarranted drive number switches")
# print(length(unique(pbp$game_id_half[ind.play.type])))

if (length(ind.play.type) > 0){
  drive.number.pre.int <- pbp$game_id_drive[ind.play.type]
  drive.number.post.int <- pbp$game_id_drive[ind.play.type+1]
  drive.number.pre.int
  drive.number.post.int
  
  for (drive.num.j in 1:length(drive.number.pre.int)){
    pbp[pbp$game_id_drive == drive.number.pre.int[drive.num.j], c("game_id_drive", "drive_number", "drive_result_detailed")] <- head(pbp[pbp$game_id_drive == drive.number.post.int[drive.num.j], c("game_id_drive", "drive_number", "drive_result_detailed")], 1)
  }
}



## In the "NO CHANGE OF POSSESSION" TOUCHBACKS - THOSE ARE ALL NEAR THE END OF HALVES... AND THERE'S ONLY A COUPLE OF THEM ANYWAY
## Just do very basic: yds_after_int = yards_gained (which will be 0 anyway, barring penalties)
##                     yards_gained = yards_to_goal - 20 (which is a starting position after touchback)
ind.play.type <- (tolower(pbp$play_type) %in% c("interception return")) & pbp$change_of_pos_team == 0 & str_detect(pbp$play_text, "touchback")
if (length(ind.play.type) > 0){
  pbp$yds_after_int[ind.play.type] <- pbp$yards_gained[ind.play.type]
  pbp$yards_gained[ind.play.type] <- pbp$yards_to_goal[ind.play.type] - 20
}


####
## For PRE-END OF HALF PLAYS (yds_to_goal_end = NA): just set to NA, although can assign "yds_after_int" the yards_gained value.
##  The yards_gained here will end up reflecting the interception return
####
ind.play.type <- which((tolower(pbp$play_type) %in% c("interception return")) & pbp$change_of_pos_team == 0 & 
  is.na(pbp$yards_to_goal_end))

if (length(ind.play.type) > 0){
  pbp$yds_after_int[ind.play.type] <- pbp$yards_gained[ind.play.type]
  pbp$yards_gained[ind.play.type] <- NA
}

####
## Any other "interception return" plays??
#####

ind.play.type <- which((tolower(pbp$play_type) %in% c("interception return"))  & 
                         !( (pbp$change_of_pos_team == 1) | 
                             (pbp$change_of_pos_team == 0 & !is.na(pbp$yards_to_goal_end) & is.na(pbp$yds_fumble_return) & !str_detect(pbp$play_text, "touchback")) |
                           (pbp$change_of_pos_team == 0 & !is.na(pbp$yards_to_goal_end) & !is.na(pbp$yds_fumble_return) & !str_detect(pbp$play_text, "touchback")) |
                           (pbp$change_of_pos_team == 0 & str_detect(pbp$play_text, "touchback")) | 
                           (pbp$change_of_pos_team == 0 & is.na(pbp$yards_to_goal_end))))


ind.play.type

## NUMBER OF UNNACOUNTED FOR INTERCEPTION RETURN PLAYS:
# print(length(ind.play.type))

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% unique(pbp$game_id_half[ind.play.type]),]
dim(pbp)






######
## !!! Check the NON-KICKOFF/NON-PUNT TOUCHBACKS !!!
## On one (game_id == 400547672), after the interception, it DIDN'T SAY "change_of_pos" for some reason...
######

# [1] "Fumble Recovery (Opponent)" "Interception Return"        "Kickoff"                    "Penalty"                    "Punt"                      
sort(unique(pbp$play_type[str_detect(pbp$play_text, "touchback")]))

## The "penalty" touchbacks are generally on NO PLAYS ("penalty_no_play = TRUE")
## The "fumble recovery (opponent)" touchbacks - there's only 2 of them - are same as interception ones


ind.play.type <- which((tolower(pbp$play_type) %in% c("fumble recovery (opponent)")) & 
  pbp$change_of_pos_team == 1 & str_detect(pbp$play_text, "touchback"))
ind.play.type

if (length(ind.play.type) > 0){
  pbp$yds_after_int[ind.play.type] <- pbp$yards_gained[ind.play.type]
  pbp$yards_gained[ind.play.type] <- pbp$yards_to_goal[ind.play.type] - (100 -  pbp$yards_to_goal_end[ind.play.type])
}


ind.play.type <- which((tolower(pbp$play_type) %in% c("fumble recovery (opponent)")) & pbp$change_of_pos_team == 0 & str_detect(pbp$play_text, "touchback"))
if (length(ind.play.type) > 0){
  pbp$yds_after_int[ind.play.type] <- pbp$yards_gained[ind.play.type]
  pbp$yards_gained[ind.play.type] <- pbp$yards_to_goal[ind.play.type] - 20
}




######
## SAFETIES - CHECK "YARDS GAINED" for these plays
######

## For change_of_pos_team = 1, AS IT SHOULD BE:
##    yards_gained, for some reason, do NOT ALWAYS JUST GO "YARDS_TO_GO - 100".... although in many places they do.
##    MAKE ALL OF THEM "yards_to_go - 100"

ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("safety")) & pbp$change_of_pos_team == 1)
if (length(ind.play.type) > 0){
  pbp$yards_gained[ind.play.type] <- pbp$yards_to_goal[ind.play.type] - 100
}

## for change_of_pos_team = 0 => LEAVE "YARDS_GAINED" AS IS... these are shady observations..
ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("safety")) & pbp$change_of_pos_team == 0)
ind.play.type

## NUMBER OF UNNACOUNTED FOR SAFETY PLAYS:
# print(length(ind.play.type))

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% unique(pbp$game_id_half[ind.play.type]),]
dim(pbp)






#####
## Play_type == "KICKOFF"
#####


####
##    "Kickoff Return Touchdown":
####
# View(pbp[pbp$play_type == "Kickoff Return Touchdown", c("yds_kickoff", "yds_kickoff_return", "yards_gained", "yards_to_goal", "play_text")])


## For change_of_pos_team = 1, which is WHAT IT SHOULD BE
ind.play.type <- which((tolower(pbp$play_type) %in% c("kickoff return touchdown")) & pbp$change_of_pos_team == 1)
ind.play.type

if (length(ind.play.type) > 0){
  
##  SPECIFICALLY for KICKOFF RETURN TDs:
##
##      * Sometimes "yds_kickoff_return" is NA, while play text clearly shows return yardage, which is contained in "yards_gained". So we assign "yards_gained" to the "yds_kickoff_return"
# summary(pbp[pbp$play_type == "Kickoff Return Touchdown", c("yds_kickoff_return")])
# summary(pbp[pbp$play_type == "Kickoff Return Touchdown", c("yards_gained")])
pbp[ind.play.type, c("yds_kickoff_return")] <- pbp[ind.play.type, c("yards_gained")] 
##
##      * Sometimes "yds_kickoff" is NA, while play text clearly shows "kickoff for X yards", and that could be extracted as follows:
##          yards gained by the returner ("yards_gained") minus the distance between the kicking team's goal line and the kickoff mark (100 - yards_to_goal, typically 35)
pbp[ind.play.type, c("yds_kickoff")] <- pbp[ind.play.type, c("yards_gained")] - (100-pbp[ind.play.type, c("yards_to_goal")])
# summary(pbp[pbp$play_type == "Kickoff Return Touchdown", c("yds_kickoff")])
# summary(pbp[pbp$play_type == "Kickoff Return Touchdown", c("yards_gained")] - (100-pbp[pbp$play_type == "Kickoff Return Touchdown", c("yards_to_goal")]))

### CHECKING OBSERVATIONS WHERE:
##     pbp[ind.play.type, c("yds_kickoff_return")] - pbp[ind.play.type, c("yds_kickoff")] 
##                   IS NOT EQUAL TO
##     (100 - pbp[ind.play.type, "yds_to_goal"])
##
#print("problematic, not for removal")
#print(length(which(pbp[ind.play.type, c("yds_kickoff_return")] - pbp[ind.play.type, c("yds_kickoff")] != (100 - pbp[ind.play.type, "yards_to_goal"]))))

## Seems like that part is OK, so no changes needed, so could simply go:
pbp[ind.play.type, c("yards_gained")] <- 100 - pbp[ind.play.type, "yards_to_goal"]
}


####
## Any other "KICKOFF RETURN TD" plays??
#####

ind.play.type <- which((tolower(pbp$play_type) %in% c("kickoff return touchdown")) & pbp$change_of_pos_team == 0)
ind.play.type

## NUMBER OF UNNACOUNTED FOR KICKOFF RETURN TD PLAYS:
# print(length(ind.play.type))

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% unique(pbp$game_id_half[ind.play.type]),]
dim(pbp)




#####
##  "KICKOFF RETURN"
#####

######
##   On TOUCHBACKS:
#####

## FIRST: Are there non-penalty touchback kickoff inconsistencies?
##   As in, any such observations that are NOT with "yds_to_goal_end = 75", yards_gained at 0, and yds kickoff return at 25
ind.play.type <- which((tolower(pbp$play_type) %in% c("kickoff") & pbp$drive_result_detailed != "On-Side Kick Lost") & pbp$change_of_pos_team == 0
                       & str_detect(tolower(pbp$play_text), "touchback")
                       & !pbp$penalty_flag & 
                         !(pbp$yards_to_goal_end == 75 & pbp$yards_gained == 0 & pbp$yds_kickoff_return == 25))

# PROBLEMATIC OBSERVATIONS:
# print("PAY ESPECIALLY CLOSE ATTENTION TO THESES (DIDN'T REMOVE THEM INITIALLY, BUT NOW I AM")
# print(length(ind.play.type))

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% unique(pbp$game_id_half[ind.play.type]),]
dim(pbp)
  



##      Vastly puts "yds_ko_return" at 25, and "yards_gained" at 0.
##      With PENALTY, it does NOT distinguish the yardages (penalty_yds are NA or 0), so might as well just keep it in "yards_gained" with no distinction.
##
ind.play.type <- which((tolower(pbp$play_type) %in% c("kickoff") & pbp$drive_result_detailed != "On-Side Kick Lost" 
                       & str_detect(tolower(pbp$play_text), "touchback")))
                       # & pbp$penalty_flag
                         
ind.play.type



## If not we do the following:
##      put "yds_ko_return" at NA
##      put "yards_gained" at (100 - yards_to_goal) - yards_to_goal_end

if (length(ind.play.type) > 0){
  pbp[ind.play.type, "yds_kickoff_return"] <- NA
  pbp[ind.play.type, "yards_gained"] <- (100 - pbp[ind.play.type, "yards_to_goal"]) - pbp[ind.play.type, "yards_to_goal_end"]
}



#######
## For ON-SIDE KICKS that are NOT LOST BY THE RECEIVING TEAM, it shows:
#######

##     yds_kickoff = NA, yds_kickoff_return = NA (for MOST.. sometimes there's a return though!),
##     yards_gained = proper difference in yards_to_goal, actually!
##   SO,
##     yards_gained = (100 - pbp[ind.play.type, "yards_to_goal"]) - pbp[ind.play.type, "yards_to_goal_end"]
##     if yds_kickoff_return is NA, then 
##         yds_kickoff_return = 0, yds_kickoff = -yards_gained
##     if yds_kickoff_return is not NA, then
##         yds_kickoff_return is as is, yds_kickoff = yds_kickoff_return - yards_gained

ind.play.type <- which((tolower(pbp$play_type) %in% c("kickoff") & pbp$drive_result_detailed != "On-Side Kick Lost"
                       & str_detect(tolower(pbp$play_text), "on-side kick")))
                       # & pbp$penalty_flag

ind.play.type

if (length(ind.play.type) > 0){
  pbp[ind.play.type, "yards_gained"] <- (100 - pbp[ind.play.type, "yards_to_goal"]) - pbp[ind.play.type, "yards_to_goal_end"]
  pbp[ind.play.type, "yds_kickoff"] <- ifelse(is.na(pbp[ind.play.type, "yds_kickoff_return"]),
                                              -pbp[ind.play.type, "yards_gained"],
                                              pbp[ind.play.type, "yds_kickoff_return"] - pbp[ind.play.type, "yards_gained"])
  pbp[ind.play.type, "yds_kickoff_return"] <- ifelse(is.na(pbp[ind.play.type, "yds_kickoff_return"]),
                                                    0,
                                                    pbp[ind.play.type, "yds_kickoff_return"])
}





#######
## For ON-SIDE KICKS that are LOST BY THE RECEIVING TEAM:
##
## If we have an on-side kick LOST BY THE RECEIVING TEAM (RARE OCCASION)
##    1. We assign "yards_gained" values (all 0s) to the "yds_kickoff_return" field (which itself is NA)
##    2. To measure "yds_kickoff" (which is NA), we take the "yards_to_goal" at the kickoff (mostly 65) minus "yards_to_goal_end" (hence AFTER kickoff). It'll still be for the same team, so makes sense.

ind.play.type <- which((tolower(pbp$play_type) %in% c("kickoff") & pbp$drive_result_detailed == "On-Side Kick Lost")
                       # & pbp$penalty_flag
)
ind.play.type


if (length(ind.play.type) > 0){
  pbp[ind.play.type, c("yds_kickoff_return")] <- pbp[ind.play.type, c("yards_gained")]

  pbp[ind.play.type, c("yds_kickoff")] <- ifelse(is.na(pbp[ind.play.type, c("yds_kickoff")]),
                                                pbp[ind.play.type, c("yards_to_goal")] - pbp[ind.play.type, c("yards_to_goal_end")],
                                                pbp[ind.play.type, c("yds_kickoff")])
  pbp[ind.play.type, c("yards_gained")] <- pbp[ind.play.type, c("yds_kickoff")]
}





#########
#### For NON-TOUCHBACK/NON-ONSIDE-KICK RETURNS
###  For change_of_pos_team = 0, which is WHAT IT SHOULD BE
#########

##
##   Without PENALTIES:
##      yds_gained is set equal to yds_kickoff_return, values are consistent.. 

ind.play.type <- which((tolower(pbp$play_type) %in% c("kickoff") & pbp$drive_result_detailed != "On-Side Kick Lost") & pbp$change_of_pos_team == 0
                       & !str_detect(tolower(pbp$play_text), "touchback")
                       & !str_detect(tolower(pbp$play_text), "on-side kick")
                        & !pbp$penalty_flag
)
ind.play.type


##      WE'LL CHECK IF:
##        "(100-yards_to_goal) - yards_to_goal_end" is equal to "yds_kickoff_return - yds_kickoff"
##      ISSUE: "yds_kickoff_return" actually not very reliable, sometimes DON'T REFLECT NEGATIVE PLAYS (still put them as POSITIVE).
##             "yds_kickoff" is, on the other hand, OK. Stick with it.
##        So, WE SHOULD:
##            - For YARDS GAINED,  SHOULD JUST REPLACE WITH THE "YARDS_TO_GOAL" METRICS
##            - For YDS_KICKOFF_RETURN: make it "yards_gained + yds_kickoff"? (yds_gained = yds_return - yds_kickoff => yds_return = yds_gained + yds_kickoff)
# print(length(which(pbp[ind.play.type, c("yds_kickoff_return")] - pbp[ind.play.type, c("yds_kickoff")] != (100 - pbp[ind.play.type, "yards_to_goal"] - pbp[ind.play.type, "yards_to_goal_end"]))))


if (length(ind.play.type) > 0){
  pbp[ind.play.type, "yards_gained"] <- ifelse(is.na(pbp[ind.play.type, "yards_to_goal_end"]),
                                              pbp[ind.play.type, "yards_gained"],
                                              (100 - pbp[ind.play.type, "yards_to_goal"]) - pbp[ind.play.type, "yards_to_goal_end"])
  pbp[ind.play.type, "yds_kickoff_return"] <- pbp[ind.play.type, "yards_gained"] + pbp[ind.play.type, "yds_kickoff"]
}




## WITH PENALTIES:
##
##   Penalty yards are not being distinguished (all are NA), so we should probably just lump stuff into "kickoff return yards"
##  SO MAKE:
##      Yards_gained = (100 - yards_to_goal) - yards_to_goal_end
##      yds_kickoff should be solid as is
##      yds_kickoff_return = yards_gained + yds_kickoff

ind.play.type <- which((tolower(pbp$play_type) %in% c("kickoff") & pbp$drive_result_detailed != "On-Side Kick Lost") & pbp$change_of_pos_team == 0
                       & !str_detect(tolower(pbp$play_text), "touchback")
                       & !str_detect(tolower(pbp$play_text), "on-side kick")
                       & pbp$penalty_flag
)
ind.play.type

if (length(ind.play.type) > 0){
  pbp[ind.play.type, "yards_gained"] <- ifelse(is.na(pbp[ind.play.type, "yards_to_goal_end"]),
                                              pbp[ind.play.type, "yards_gained"],
                                              (100 - pbp[ind.play.type, "yards_to_goal"]) - pbp[ind.play.type, "yards_to_goal_end"])
  pbp[ind.play.type, "yds_kickoff_return"] <- pbp[ind.play.type, "yards_gained"] + pbp[ind.play.type, "yds_kickoff"]
}


#########
#### For NON-TOUCHBACK/NON-ONSIDE-KICK/NON-TD/FUMBLE RETURNS
###  For change_of_pos_team = 1, which SHOULD NOT BE THE CASE
#########

## NEVER THE CASE, SEEMINGLY

ind.play.type <- which((tolower(pbp$play_type) %in% c("kickoff") & pbp$drive_result_detailed != "On-Side Kick Lost") & pbp$change_of_pos_team == 1
                       & !str_detect(tolower(pbp$play_text), "touchback")
                       & !str_detect(tolower(pbp$play_text), "on-side kick")
                      # & !pbp$penalty_flag
)
ind.play.type

## PROBLEMATIC OBS (UP FOR REMOVAL)
# print(length(ind.play.type))

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% unique(pbp$game_id_half[ind.play.type]),]
dim(pbp)




######
##    "Kickoff Team Fumble Recovery"
######

### With CHANGE OF POS = 1, which is WHAT IT SHOULD BE:

ind.play.type <- which(tolower(pbp$play_type) %in% c("kickoff team fumble recovery") & pbp$change_of_pos_team == 1
                       # & !pbp$penalty_flag
)
ind.play.type

## KEEPING IN MIND that we consider the RETURNING TEAM as the "POSSESSOR",
## and that these Kickoff fumble recoveries are 1-PLAY POSSESSIONS FOR THE RETURNING TEAM:
##
##   "yards_gained" should be = "yards_to_goal_end" - "yards_to_goal", which would show how much (typically) negative yardage the kick returning team gave up
##   "yds_kickoff_return" = yards_gained + yds_kickoff
##
## NOTE: RETURN WOULD INCLUDE WHAT HAPPENS AFTER THE FUMBLE.. NOT JUST THE PURE RUN BY THE RETURNER (THERE'S NO DISTINGUISHING)
## (Don't care about how far the returner ran it at first before the fumble)

if (length(ind.play.type) > 0){
  pbp[ind.play.type, "yards_gained"] <- pbp[ind.play.type, "yards_to_goal_end"] - pbp[ind.play.type, "yards_to_goal"]
  pbp[ind.play.type, "yds_kickoff_return"] <- pbp[ind.play.type, "yards_gained"] + pbp[ind.play.type, "yds_kickoff"]
}




### With CHANGE OF POS = 0, which is WHAT IT should NOT be:

ind.play.type <- which(tolower(pbp$play_type) %in% c("kickoff team fumble recovery") & pbp$change_of_pos_team == 0
                       # & !pbp$penalty_flag
)
ind.play.type



####
## Any other "Kickoff Team Fumble Recovery" plays??
#####

ind.play.type <- which(tolower(pbp$play_type) %in% c("kickoff team fumble recovery") & pbp$change_of_pos_team == 0
                       # & !pbp$penalty_flag
)
ind.play.type

## NUMBER OF UNNACOUNTED "Kickoff Team Fumble Recovery" PLAYS:
# print(length(ind.play.type))

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% unique(pbp$game_id_half[ind.play.type]),]
dim(pbp)




#####
## "Kickoff (Safety)"
## 
##  Used to be in 2014, but seems to have been removed? 
######

ind.play.type <- which(pbp$play_type == "Kickoff (Safety)")
ind.play.type

if (length(ind.play.type) > 0){
  pbp[ind.play.type, c("yds_kickoff_return")] <- pbp[ind.play.type, c("yards_gained")]
  pbp[ind.play.type, c("yds_kickoff")] <- (100 + pbp[ind.play.type, c("yards_gained")]) - (100-pbp[ind.play.type, c("yards_to_goal")])
  # pbp[pbp$play_type == "Kickoff (Safety)", c("yds_kickoff", "yds_kickoff_return", "yards_gained")] 
}








#######
## DOUBLE-CHECKING THE KICKOFFS STUFF
#######

## Pretty much all are "change_of_pos_team = 0", and those that are not - the ON-SIDE KICK RECOVERY LOSSES

## For explicit "play_type = KICKOFF"
table(pbp$change_of_pos_team[pbp$play_type == "Kickoff"])

ind.play.type <- which(pbp$play_type == "Kickoff" & pbp$change_of_pos_team == 1 & pbp$drive_result_detailed != "On-Side Kick Lost")
ind.play.type

## For plays that have "kickoff" in the text, but are NOT of explicit "Kickoff" play type
##   Also dispose of "Kickoff Return Touchdown" or "Kickoff Team Fumble Recovery" - those are legitimately "change_of_pos_team" = 1
##   All the remaining ones are: KICKOFF PENALTIES, "NO PLAY" !!
##     For THESE, we probably need to replace "CHANGE_OF_POS" to 0, while MAKING IT 1 FOR THE PREVIOUS OBSERVATION (if the latter was 0)

table(pbp$change_of_pos_team[str_detect(tolower(pbp$play_text),"kickoff") & !pbp$play_type %in% c("Kickoff", "Kickoff Return Touchdown", "Kickoff Team Fumble Recovery")])

ind.play.type <- which(str_detect(tolower(pbp$play_text),"kickoff") & 
                         !pbp$play_type %in% c("Kickoff", "Kickoff Return Touchdown", "Kickoff Team Fumble Recovery") & 
                         pbp$change_of_pos_team == 1 & lag(pbp$change_of_pos_team) == 0 & 
                         pbp$pos_team == lead(pbp$pos_team) & 
                         pbp$penalty_no_play)
ind.play.type

if (length(ind.play.type) > 0){
  pbp[ind.play.type, ]$change_of_pos_team <- 0
  pbp[ind.play.type-1, ]$change_of_pos_team <- 1
}






#######
## Play_type == "PENALTY"
## CHANGE_OF_POS_TEAM == 1
######

# ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("penalty")) & pbp$change_of_pos_team == 1 
#                        & pbp$pos_team != lead(pbp$pos_team))

####
##    Checking if "yards_to_goal - yards_to_goal_end" = yards_gained = yds_penalty
## 
## KEEP IN MIND that, ON KICKOFFS, the YARDS_TO_GOAL DON'T REFLECT THE FACT THAT I WANT TO TREAT THE RETURNING TEAM AS THE "POSSESSOR"...
##       (THEY STILL SHOW "65", FROM THE PERSPECTIVE OF THE KICKING TEAM)
##
##  !!! Especially check the "NO PLAY" PENALTIES, as they MIGHT MESS UP MY "YARDS_GAINED" CALCULATIONS!!!
######

## "offsetting penalties" are fine, they don't really mean much unless they lead to replaying the down, which would go under "NO PLAY" alread

## Checking for how many there's a lack of correspondence between YARDS_TO_GOAL and YARDS_GAINED
##
##  Some TOUCHDOWNS seem to have "yards_to_goal_end" at 50 right away, with NO PENALTY.. BUT.. the TEXT CLEARLY SAYS "KICKOFF FOR 50 YARDS FOR A TOUCHBACK"...
##   I GUESS KICKOFFS COULD INDEED HAPPEN FROM A 50-yd LINE, after a 15-YD PENALTY (35+15) !!!
##
# table(pbp$yards_to_goal_end[str_detect(tolower(pbp[, "play_type"]), "touchdown") & !pbp$penalty_flag])
# ind.play.type <- which(str_detect(tolower(pbp[, "play_type"]), "touchdown") & pbp$yards_to_goal_end == 50)


### GOOD INDICES are the ones where: yards_gained = yards_to_goal - yards_to_goal_end
##
##  For BAD ONES, among them we have:
##    - Tiny penalties on 2pt/extra points resulting in YARDS_GAINED = -2/-5,
##         FOR THOSE, we should:
##              identify them as "yards_to_goal == yards_to_goal_end"
##              SHIFT the "1" on change_of_pos play towards that SCORING PLAY, and set it it 0 for "Penalty
##              DROP THE OBSERVATION?? (NOT THE HALF THOUGH)

ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("penalty")) & pbp$change_of_pos_team == 1 
                       & pbp$pos_team != lead(pbp$pos_team) &
                         pbp$yards_gained != (pbp$yards_to_goal - pbp$yards_to_goal_end) & 
                        pbp$yards_to_goal == 65 & pbp$yards_to_goal_end == 65)
ind.play.type 

if (length(ind.play.type) > 0){
  pbp[ind.play.type, ]$change_of_pos_team <- 0
  pbp[ind.play.type-1, ]$change_of_pos_team <- 1
}

dim(pbp)
pbp <- pbp[-ind.play.type, ]
dim(pbp)


##   - OTHERS: if "penalty_no_play = FALSE" & drive-Result = SAFETY, then:
##      It's a TEAM SAFETY by PENALTY (super rare), then:
##          - CHANGE THE "YARDS_GAINED" and "YDS_PENALTY" to be "yards_to_goal -100"
ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("penalty")) & pbp$change_of_pos_team == 1 
                       & pbp$pos_team != lead(pbp$pos_team) &
                         pbp$yards_gained != (pbp$yards_to_goal - pbp$yards_to_goal_end) & 
                         !(pbp$yards_to_goal == 65 & pbp$yards_to_goal_end == 65) & 
                         pbp$penalty_no_play & pbp$drive_result_detailed == "Safety")
ind.play.type 

if (length(ind.play.type) > 0){
  pbp[ind.play.type, ]$yards_gained <- pbp[ind.play.type, ]$yards_to_goal - 100
  pbp[ind.play.type, ]$yds_penalty <- pbp[ind.play.type, ]$yards_to_goal - 100
}


##   - OTHERS that are NOT "penalty_no_play = TRUE", those are JUST BAD RECORDING... NEED TO REMOVE..
##    Just 1 like that in 2014.
ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("penalty")) & pbp$change_of_pos_team == 1 
                       & pbp$pos_team != lead(pbp$pos_team) &
                         pbp$yards_gained != (pbp$yards_to_goal - pbp$yards_to_goal_end) & 
                         !(pbp$yards_to_goal == 65 & pbp$yards_to_goal_end == 65) & 
                         !(pbp$penalty_no_play & pbp$drive_result_detailed == "Safety"))
ind.play.type 

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% unique(pbp$game_id_half[ind.play.type]),]
dim(pbp)


##  Checking the YDS_PENALTY & YARDS_GAINED for the "change_of_pos_team == 1" cases:
##    BAD SITUATIONS: YDS_PENALTY != YARDS_GAINED
##     DOESN'T APPEAR TO BE THOSE, BUT IF THERE WERE: PROBABLY REPLACE "yds_penalty" with "yards_gained"
ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("penalty")) & pbp$change_of_pos_team == 1 
& pbp$pos_team != lead(pbp$pos_team) &
  pbp$yards_gained != pbp$yds_penalty)

## # of SUCH BAD SITUATIONS:
# print("problematic, but not up for removal")
# print(length(ind.play.type))

if (length(ind.play.type) > 0){
  pbp[ind.play,]$yds_penalty <- pbp[ind.play,]$yards_gained
}





##  Checking if there are cases of "change_of_pos_team == 1" & pbp$pos_team == lead(pbp$pos_team),
##   WHICH SHOULD GENERALLY NOT HAPPEN
ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("penalty")) & pbp$change_of_pos_team == 1 
                       & pbp$pos_team == lead(pbp$pos_team))
ind.play.type

## # of SUCH BAD SITUATIONS (FOR REMOVAL)
# print(length(ind.play.type))

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% unique(pbp$game_id_half[ind.play.type]),]
dim(pbp)



#######
## Play_type == "PENALTY"
## CHANGE_OF_POS_TEAM == 0
######

## CHECKING for BAD CASES where "YARDS_GAINED != YARDS_TO_GOAL - YARDS_TO_GOAL_END":
##  Just 2 of those in 2014, both "penalty_no_play"... but CAN'T PICK UP ON HOW TO DEAL WITH THOSE... BOTH BADLY RECORDED. JUST REMOVE THE HALVES
ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("penalty")) & pbp$change_of_pos_team == 0 & 
                       pbp$pos_team == lead(pbp$pos_team) & 
                         pbp$yards_gained != pbp$yards_to_goal - pbp$yards_to_goal_end 
                      # & pbp$penalty_no_play
                       )
ind.play.type


## # of SUCH BAD SITUATIONS (FOR REMOVAL)
# print(length(ind.play.type))

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% unique(pbp$game_id_half[ind.play.type]),]
dim(pbp)


## CHECKING for BAD CASES where "YARDS_GAINED != YDS_PENALTY":
##   These are all NO PLAYS, and they set "yds_penalty" to 0.. yds_gained is good though, so just gotta plug its value into "yds_penalty"
ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("penalty")) & pbp$change_of_pos_team == 0 & 
                         pbp$pos_team == lead(pbp$pos_team) & 
                         pbp$yards_gained != pbp$yds_penalty
                       # & pbp$penalty_no_play
                       )
ind.play.type

if (length(ind.play.type) > 0){
  pbp[ind.play.type,]$yds_penalty <- pbp[ind.play.type,]$yards_gained
}

## CHECKING for BAD CASES where "change_of_pos_team = 0" and  pbp$pos_team != lead(pbp$pos_team) 
##   WHICH SHOULD GENERALLY NOT HAPPEN
ind.play.type <- which(str_detect(tolower(pbp$play_type), fixed("penalty")) & pbp$change_of_pos_team == 0 & 
                         pbp$pos_team != lead(pbp$pos_team))

ind.play.type

## # of SUCH BAD SITUATIONS (FOR REMOVAL)
# print(length(ind.play.type))

dim(pbp)
pbp <- pbp[!pbp$game_id_half %in% unique(pbp$game_id_half[ind.play.type]),]
dim(pbp)



# View(pbp[ind.play.type,] %>%
#   # pbp[sort(c(ind.play.type, ind.play.type+1)),] %>%
#   # pbp[sort(c(ind.play.type, ind.play.type-1, ind.play.type+1)),] %>%
# 
#  # pbp[ind.play.type[which(pbp[ind.play.type, "yards_gained"] != (pbp[ind.play.type, "yards_to_goal"] - (100-pbp[ind.play.type, "yards_to_goal_end"])))], ] %>%
# 
#     # pbp[ind.play.type[which(pbp[ind.play.type, "yards_gained"] != (pbp[ind.play.type, "yards_to_goal"] - pbp[ind.play.type, "yards_to_goal_end"]))], ] %>%
#    # pbp %>% filter(game_id_drive %in% c("400548029-13", "400548029-14")) %>%
#     select(game_id_drive,
#            pos_team, def_pos_team, drive_number, change_of_pos_team, drive_result_detailed, play_type, play_text,
#            yards_to_goal, yards_to_goal_end,
#            yards_gained,
#            yds_penalty, penalty_no_play,
#            yds_punted, yds_punt_return, yds_punt_gained,
#            # yds_rushed, yds_receiving,
#            # rush_yds, reception_yds,
#            yds_fumble_return,
#            penalty_flag,
#            pos_team_score, def_pos_team_score))







# cat("\n")
# cat("\n")
# print("DATA SET SIZE AFTER TERTIARY (YARDAGE) CLEANING")
# print(dim(pbp))
# print(length(unique(pbp$game_id_half)))
# cat("\n")
# cat("\n")





#############################################################################################
#############################################################################################
####  FLIPPING PUNTS to NEXT POSSESSION, FIXING STUFF UP
#############################################################################################
#############################################################################################



unique(pbp$drive_result_detailed)

## PIVOTAL POINTS:
##    - Play types like "Timeout", "Kickoff", "Penalty" lead to repeats of downs, so need to be carefully adjusted for in calculating the # of 1st, 3rd & 4th downs
##
##    - Penalties gotta be accounted for, can't just dispose of them.. Won't necessarily count them as "plays" per se, but yardage has to be reflected
##        NOTE: A couple weird post-score/pre-Kickoff penalty plays messed with drive_result/observation ordering, so I deleted them,
##             BUT THOSE COULD BE PICKED UP VIA GAUGING THE "YARDS_TO_GOAL" AT THE KICKOFF!
##
##    - Count TOUCHBACKS as OFFENSIVE PLAYS??

## For kickoffs:
##    - Treat it as the team kicking off being the DEFENSE (DON'T TREAT IT AS PART OF THE PREVIOUS OFFENSE'S DRIVE), while team returning is OFFENSE
##    - get the MARGIN between the "default starting position" of 25 (75yds to go), and what it ended up being (x_yds to go)
# View(pbp  %>% select(play_text, play_type, yds_kickoff,  yds_kickoff_return, yards_to_goal))
##
## For punts -  
##    - Treat it as follows: the team PUNTING as the DEFENSE, while team returning as the OFFENSE (don't treat PUNT as PART OF THE PREVIOUS OFFENSIVE DRIVE)
##    - get the MARGIN ("NET") between the sheer punt yardage (how far the punt went before getting touched) and the RETURN (which can be the actual attempted return, or TOUCHBACK)
##    - "PUNT TEAM FUMBLE RECOVERY" - THESE HAVE TO BE TREATED as a 1-PLAY POSSESSION for the RECEIVING TEAM'S OFFENSE...

## !!!! GOTTA SWITCH the PUNT PLAYS over to the NEXT DRIVE !!!!
# View(pbp  %>% select(game_id, drive_id, play_text, play_type, yds_punted, yds_punt_return, yds_punt_gained) %>% filter(yds_punt_return != yds_punt_gained))
# View(pbp  %>% select(game_id, drive_id, pos_team, def_pos_team, play_text, play_type, yds_punted, yds_punt_return, yds_punt_gained) %>% filter(game_id == 400547644))

## ALWAYS POSITIVE.. even when RETURNED FOR A LOSS...
# summary(pbp$yds_punt_return)


## HOW TO ARRANGE "PUNT NUMBERS" FOR THIS???
##    ADD a "+0.5" to the DRIVE # of THESE... so that we DON'T HAVE TO INCREASE THE DRIVE # for EACH SUBSEQUENT DRIVE
## 

## "Punt Return TD", "Blocked Punt TD", "Blocked Punt", "Punt Team Fumble Lost", "Punt Team Fumble Lost Touchdown"

# View(pbp  %>% select(game_id, drive_number, pos_team, def_pos_team, play_text, play_type, yds_punted, yds_punt_return, yds_punt_gained,
#                      yards_to_goal, yards_to_goal_end,
#                      yds_punted, yds_punt_return, yds_punt_gained) %>%
#        # filter(str_detect(tolower(play_text), "touchback") & !str_detect(tolower(play_type),"kickoff") & !str_detect(tolower(play_type),"punt")))
#         filter(play_type == "Blocked Punt"))
#        # filter(play_type == "Blocked Punt TD"))
#        # filter(play_type == "Blocked Punt Touchdown"))
# 
# 
# View(pbp %>% filter(game_id == 400547656) %>% select(game_id, drive_number, pos_team, def_pos_team, down, play_text, play_type,
#                                                      yards_to_goal, yards_to_goal_end,
#                                                      yds_punted, yds_punt_return, yds_punt_gained,
#                                                      drive_result_detailed))



######
## MOVING "Punt", "Blocked Punt" into the FOLLOWING POSSESSION
## MAKING "Punt Team Fumble Recovery" as a 1-PLAY POSSESSION FOR THE RECEIVING TEAM, 
## MAKING "Blocked Punt Touchdown" as a 1
######

## Defining the team-related variables & drive-related variables, so that we could manipulate these
pos_team.vars <- colnames(pbp)[(str_detect(colnames(pbp), "pos_") | str_detect(colnames(pbp),"_pos")) & !str_detect(colnames(pbp), "change") & !str_detect(colnames(pbp), "by_poss")]
drive.vars <- colnames(pbp)[str_detect(colnames(pbp), "drive")]

## NOTE: "drive_start_yards_to_goal" & "drive_start_yards_to_goal_end" AREN'T USEFUL FOR MY APPROACH HERE, but MIGHT BE USEFUL FOR "LAZY DRIVE-BY-DRIVE" ANALYSIS


#######
## For "PUNT"
#######

##   FIRST, all the NON-TRIVIAL PLAYS, where DRIVE NUMBER DIDN'T CHANGE or maybe the HALF CHANGED:
##   (we tackle them first, because otherwise they'd get wiped out after all the drive numbers are manually changed by us)
##   

ind.play.type <- which(pbp$play_type %in% c("Punt") & 
                         !(pbp$drive_number != lead(pbp$drive_number) & pbp$game_id_half == lead(pbp$game_id_half)))
ind.play.type

##   Here it's clear we deal either with:
##        A. An EXPLICIT "End of Half" play after the Punt
##        B. Switching over to a different game/half without that explicit play
##
##    For A. An EXPLICIT "End of Half" play after the Punt (plus NO OTHER PLAYS BEFORE THE HALF IS OVER, via lead(lead(pbp$game_id_half)) != pbp$game_id_half)

ind.play.type <- which(pbp$play_type %in% c("Punt") & 
                         (str_detect(tolower(lead(pbp$play_type)), "end of"))
                        & lead(pbp$drive_number) == pbp$drive_number & lead(lead(pbp$game_id_half)) != pbp$game_id_half
                       # & lead(pbp$game_id_half) != pbp$game_id_half & lead(lead(pbp$game_id_half)) != pbp$game_id_half
                       )
ind.play.type



# For these plays and THE SUBSEQUENT "END OF HALF" plays, we
##          1. Flip the possession values 
##          2. Make "drive_result_detailed" into "End of Half"
##          3. make "drive_play_number" =1
##          4. Increase "drive number" by 0.5 (there could still be drives in the SECOND half, don't forget)

pbp[sort(c(ind.play.type,ind.play.type+1)), "drive_play_number"] <- c(1,2)
pbp[sort(c(ind.play.type,ind.play.type+1)), c("drive_number")] <- pbp[sort(c(ind.play.type,ind.play.type+1)), c("drive_number")] + 0.5
pbp[sort(c(ind.play.type,ind.play.type+1)), c("pos_team", "def_pos_team")] <- pbp[sort(c(ind.play.type,ind.play.type+1)), c("def_pos_team", "pos_team")]
pbp[sort(c(ind.play.type,ind.play.type+1)), c("pos_team_score", "def_pos_team_score")] <- pbp[sort(c(ind.play.type,ind.play.type+1)), c("def_pos_team_score", "pos_team_score")]
pbp[sort(c(ind.play.type,ind.play.type+1)), c("pos_score_diff_start", "pos_score_diff_start_end", "score_pts", "pos_score_diff", "yards_gained")] <- -pbp[sort(c(ind.play.type,ind.play.type+1)), c("pos_score_diff_start", "pos_score_diff_start_end", "score_pts", "pos_score_diff", "yards_gained")]
pbp[sort(c(ind.play.type,ind.play.type+1)), "pos_team_receives_2H_kickoff"] <- 1 - pbp[sort(c(ind.play.type,ind.play.type+1)), "pos_team_receives_2H_kickoff"]

## Making sure the new drive numbers are incorporated into "game_id_drive"
pbp$game_id_drive <- paste(pbp$game_id, pbp$drive_number, sep="-")
pbp$game_id_drive <- factor(pbp$game_id_drive, levels = unique(pbp$game_id_drive))

pbp[sort(c(ind.play.type,ind.play.type+1)), c("drive_result_detailed")] <- "End of Half"


### For B. Switching over to a different game/half without that explicit play

ind.play.type <- which(pbp$play_type %in% c("Punt") & 
                         (pbp$game_id_half != lead(pbp$game_id_half)))
ind.play.type


pbp[ind.play.type, "drive_play_number"] <- 1
pbp[ind.play.type, c("drive_number")] <- pbp[ind.play.type, c("drive_number")] + 0.5
pbp[ind.play.type, c("pos_team", "def_pos_team")] <- pbp[ind.play.type, c("def_pos_team", "pos_team")]
pbp[ind.play.type, c("pos_team_score", "def_pos_team_score")] <- pbp[ind.play.type, c("def_pos_team_score", "pos_team_score")]
pbp[ind.play.type, c("pos_score_diff_start", "pos_score_diff_start_end", "score_pts", "pos_score_diff", "yards_gained")] <- -pbp[ind.play.type, c("pos_score_diff_start", "pos_score_diff_start_end", "score_pts", "pos_score_diff", "yards_gained")]
pbp[ind.play.type, "pos_team_receives_2H_kickoff"] <- 1 - pbp[ind.play.type, "pos_team_receives_2H_kickoff"]

## Making sure the new drive numbers are incorporated into "game_id_drive"
pbp$game_id_drive <- paste(pbp$game_id, pbp$drive_number, sep="-")
pbp$game_id_drive <- factor(pbp$game_id_drive, levels = unique(pbp$game_id_drive))

pbp[ind.play.type, c("drive_result_detailed")] <- "End of Half"


####
## "Punt" plays that were followed by "End of Half" and AT LEAST ONE MORE PLAY before the end of half
####

ind.play.type <- which(pbp$play_type %in% c("Punt") & 
                         (str_detect(tolower(lead(pbp$play_type)), "end of"))
                       & lead(lead(pbp$game_id_half)) == pbp$game_id_half
)

# print("Number of problematic observations, need to be looked at:")
# print(length(ind.play.type))

# View(  #pbp[ind.play.type,] %>%
#   #pbp[sort(c(ind.play.type, ind.play.type+1)),] %>%
#   #pbp[sort(c(ind.play.type, ind.play.type+1, ind.play.type+2)),] %>%
#     # pbp %>% filter(game_id_drive %in% c("400548029-13", "400548029-14")) %>%
#   pbp %>% filter(game_id == "401035252") %>%
#     select(game_id_drive, game_id_half,
#            period, clock.minutes, clock.seconds,
#            pos_team, def_pos_team, drive_number, change_of_pos_team, drive_result_detailed, play_type, play_text,
#            penalty_flag,
#            pos_team_score, def_pos_team_score, pos_score_diff_start, pos_score_diff_start_end, score_pts, pos_score_diff,
#            pos_team_receives_2H_kickoff))




##    Where the drive did change & we're still in the same half, and the POSSESSION TEAM CHANGED, we:
##          1. copy from the next row: pos_team, def_pos_team, pos_team_score, def_pos_team_score, DRIVE VARIABLES (bar the drive_play_number) ...
##          2. make "drive_play_number" a 0.5
##          3. Flip the sign for yards_gained

ind.play.type <- which(pbp$play_type %in% c("Punt") & 
                         pbp$drive_number != lead(pbp$drive_number) & 
                         pbp$game_id_half == lead(pbp$game_id_half) & 
                         pbp$pos_team != lead(pbp$pos_team))


pbp[ind.play.type, c("drive_number",  "drive_result_detailed", pos_team.vars)] <- pbp[ind.play.type+1, c("drive_number",  "drive_result_detailed", pos_team.vars)]
pbp[ind.play.type, "drive_play_number"] <- 0.5
pbp[ind.play.type, "yards_gained"] <- -pbp[ind.play.type, "yards_gained"]


## Making sure the new drive numbers are incorporated into "game_id_drive"
pbp$game_id_drive <- paste(pbp$game_id, pbp$drive_number, sep="-")
pbp$game_id_drive <- factor(pbp$game_id_drive, levels = unique(pbp$game_id_drive))


##    Where the drive did change, we're still in the same half, 
## !!! AND THE POSSESSION TEAM DID NOT CHANGE IN THE NEXT PLAY !!!!
## !!! THOSE CASES HAVE NOT HAPPENED in 2014 !!!!

ind.play.type <- which(pbp$play_type %in% c("Punt") & 
                         pbp$drive_number != lead(pbp$drive_number) & 
                         pbp$game_id_half == lead(pbp$game_id_half) & 
                         pbp$pos_team == lead(pbp$pos_team))
# print("TO BE LOOKED AT, CLOSELY:")
# print(length(ind.play.type))
if (length(ind.play.type) > 0) stop("There are problematic Punt drives")







######
## For "BLOCKED PUNT":
######

##    - VERY SIMILAR to JUST PUNT, besides the fact that we change its drive result to "Punt" (and "Blocked Punt" remains just as PLAY TYPE)

ind.play.type <- which(pbp$play_type %in% c("Blocked Punt") & 
                         !(pbp$drive_number != lead(pbp$drive_number) & pbp$game_id_half == lead(pbp$game_id_half)))
ind.play.type

## NOTE: NONE OF SUCH CASES in 2014

# print("Potentially to be looked at, but not necessarily:")
# print(length(ind.play.type))


if (length(ind.play.type) > 0){
  
ind.play.type <- which(pbp$play_type %in% c("Blocked Punt") & 
                         (str_detect(tolower(lead(pbp$play_type)), "end of"))
                      # & lead(lead(pbp$game_id_half)) != pbp$game_id_half
                       & lead(pbp$drive_number) == pbp$drive_number & lead(lead(pbp$game_id_half)) != pbp$game_id_half
                       
)
ind.play.type


# For these plays and THE SUBSEQUENT "END OF HALF" plays, we
##          1. Flip the possession values 
##          2. Make "drive_result_detailed" into "End of Half"
##          3. make "drive_play_number" =1
##          4. Increase "drive number" by 0.5 (there could still be drives in the SECOND half, don't forget)

pbp[sort(c(ind.play.type,ind.play.type+1)), "drive_play_number"] <- c(1,2)
pbp[sort(c(ind.play.type,ind.play.type+1)), c("drive_number")] <- pbp[sort(c(ind.play.type,ind.play.type+1)), c("drive_number")] + 0.5
pbp[sort(c(ind.play.type,ind.play.type+1)), c("pos_team", "def_pos_team")] <- pbp[sort(c(ind.play.type,ind.play.type+1)), c("def_pos_team", "pos_team")]
pbp[sort(c(ind.play.type,ind.play.type+1)), c("pos_team_score", "def_pos_team_score")] <- pbp[sort(c(ind.play.type,ind.play.type+1)), c("def_pos_team_score", "pos_team_score")]
pbp[sort(c(ind.play.type,ind.play.type+1)), c("pos_score_diff_start", "pos_score_diff_start_end", "score_pts", "pos_score_diff","yards_gained")] <- -pbp[sort(c(ind.play.type,ind.play.type+1)), c("pos_score_diff_start", "pos_score_diff_start_end", "score_pts", "pos_score_diff", "yards_gained")]
pbp[sort(c(ind.play.type,ind.play.type+1)), "pos_team_receives_2H_kickoff"] <- 1 - pbp[sort(c(ind.play.type,ind.play.type+1)), "pos_team_receives_2H_kickoff"]

## Making sure the new drive numbers are incorporated into "game_id_drive"
pbp$game_id_drive <- paste(pbp$game_id, pbp$drive_number, sep="-")
pbp$game_id_drive <- factor(pbp$game_id_drive, levels = unique(pbp$game_id_drive))

pbp[sort(c(ind.play.type,ind.play.type+1)), c("drive_result_detailed")] <- "End of Half"

}


### For B. Switching over to a different game/half without that explicit play


ind.play.type <- which(pbp$play_type %in% c("Blocked Punt") & 
                         (pbp$game_id_half != lead(pbp$game_id_half)))
ind.play.type

if (length(ind.play.type) > 0){

pbp[ind.play.type, "drive_play_number"] <- 1
pbp[ind.play.type, c("drive_number")] <- pbp[ind.play.type, c("drive_number")] + 0.5
pbp[ind.play.type, c("pos_team", "def_pos_team")] <- pbp[ind.play.type, c("def_pos_team", "pos_team")]
pbp[ind.play.type, c("pos_team_score", "def_pos_team_score")] <- pbp[ind.play.type, c("def_pos_team_score", "pos_team_score")]
pbp[ind.play.type, c("pos_score_diff_start", "pos_score_diff_start_end", "score_pts", "pos_score_diff", "yards_gained")] <- -pbp[ind.play.type, c("pos_score_diff_start", "pos_score_diff_start_end", "score_pts", "pos_score_diff", "yards_gained")]
pbp[ind.play.type, "pos_team_receives_2H_kickoff"] <- 1 - pbp[ind.play.type, "pos_team_receives_2H_kickoff"]

## Making sure the new drive numbers are incorporated into "game_id_drive"
pbp$game_id_drive <- paste(pbp$game_id, pbp$drive_number, sep="-")
pbp$game_id_drive <- factor(pbp$game_id_drive, levels = unique(pbp$game_id_drive))

pbp[ind.play.type, c("drive_result_detailed")] <- "End of Half"

}


####
## "Blocked Punt" plays that were followed by "End of Half" and AT LEAST ONE MORE PLAY before the end of half
####

ind.play.type <- which(pbp$play_type %in% c("Blocked Punt") & 
                         (str_detect(tolower(lead(pbp$play_type)), "end of"))
                       & lead(lead(pbp$game_id_half)) == pbp$game_id_half
)

# print("Number of problematic observations, need to be looked at:")
# print(length(ind.play.type))




##    Where the drive did change, we're still in the same half, AND THE POSSESSION TEAM CHANGED IN THE NEXT PLAY, we:
##          1. copy from the next row: pos_team, def_pos_team, pos_team_score, def_pos_team_score, DRIVE VARIABLES (bar the drive_play_number) ...
##          2. make "drive_play_number" a 0.5
##          3. Change the result of previous drive into "Punt" (instead of "Blocked Punt", so that the latter only stays as a PLAY TYPE)

ind.play.type <- which(pbp$play_type %in% c("Blocked Punt") & 
                         pbp$drive_number != lead(pbp$drive_number) & 
                         pbp$game_id_half == lead(pbp$game_id_half) & 
                         pbp$pos_team != lead(pbp$pos_team))

pbp[ind.play.type, c("drive_number",  "drive_result_detailed", pos_team.vars)] <- pbp[ind.play.type+1, c("drive_number",  "drive_result_detailed", pos_team.vars)]
pbp[ind.play.type, "drive_play_number"] <- 0.5
pbp[ind.play.type, "yards_gained"] <- -pbp[ind.play.type, "yards_gained"]

## Making sure the new drive numbers are incorporated into "game_id_drive"
pbp$game_id_drive <- paste(pbp$game_id, pbp$drive_number, sep="-")
pbp$game_id_drive <- factor(pbp$game_id_drive, levels = unique(pbp$game_id_drive))

## Change the drive result for what preceded the Blocked Punt into just "Punt"
pbp[which(pbp$drive_result_detailed == "Blocked Punt"), "drive_result_detailed"] <- "Punt"


##    Where the drive did change, we're still in the same half, AND THE POSSESSION TEAM DID NOT CHANGE IN THE NEXT PLAY, we gotta create a possession by punt receiving team:
##          1. Add "+0.5" to the drive number, making it "sandwiched" between pre- and post-punt drives.
##          2. Flip the possession team information (DON'T COPY "NEXT ROW", THAT WON'T WORK HERE)
##          3. make "drive_play_number" a 1
##          4. Change the result of this "1-play drive" into "Blocked Punt Team Fumble Lost"
##          5. Change the result of previous drive into "Punt" (instead of "Blocked Punt", so that the latter only stays as a PLAY TYPE)

ind.play.type <- which(pbp$play_type %in% c("Blocked Punt") & 
                         pbp$drive_number != lead(pbp$drive_number) & 
                         pbp$game_id_half == lead(pbp$game_id_half) & 
                         pbp$pos_team == lead(pbp$pos_team))

pbp[ind.play.type, "drive_play_number"] <- 1
pbp[ind.play.type, c("drive_number")] <- pbp[ind.play.type, c("drive_number")] + 0.5
pbp[ind.play.type, c("pos_team", "def_pos_team")] <- pbp[ind.play.type, c("def_pos_team", "pos_team")]
pbp[ind.play.type, c("pos_team_score", "def_pos_team_score")] <- pbp[ind.play.type, c("def_pos_team_score", "pos_team_score")]
pbp[ind.play.type, c("pos_score_diff_start", "pos_score_diff_start_end", "score_pts", "pos_score_diff", "yards_gained")] <- -pbp[ind.play.type, c("pos_score_diff_start", "pos_score_diff_start_end", "score_pts", "pos_score_diff", "yards_gained")]
pbp[ind.play.type, "pos_team_receives_2H_kickoff"] <- 1 - pbp[ind.play.type, "pos_team_receives_2H_kickoff"]
pbp[ind.play.type, "drive_result_detailed"] <- "Blocked Punt Team Fumble Lost"

## Making sure the new drive numbers are incorporated into "game_id_drive"
pbp$game_id_drive <- paste(pbp$game_id, pbp$drive_number, sep="-")
pbp$game_id_drive <- factor(pbp$game_id_drive, levels = unique(pbp$game_id_drive))

## Change the drive result for what preceded the Blocked Punt into just "Punt"
pbp[which(pbp$drive_result_detailed == "Blocked Punt"), "drive_result_detailed"] <- "Punt"







#####
## Putting NAs for drive TIME characteristics in case of "Punt" & "Blocked Punt" plays
#####

pbp[which(pbp$play_type %in% c("Punt", "Blocked Punt")), 
    c("drive_time_minutes_elapsed", "drive_time_seconds_elapsed",
      "drive_time_minutes_start", "drive_time_seconds_start",
      "drive_time_minutes_end", "drive_time_seconds_end")] <- NA



#####
##  For "PUNT TEAM FUMBLE RECOVERY"
#####

ind.play.type <- which(pbp$play_type %in% c("Punt Team Fumble Recovery"))
ind.play.type

##   It CHANGES THE DRIVE NUMBER, but keeps the DRIVE_RESULT_DETAILED THE SAME FOR BOTH PRE/POST PUNT TEAM FUMBLE RECOVERY (equal to what happened after the recovery)
##
##   Let's check for cases where EITHER THE DRIVE NUMBER DOESN'T CHANGE, or the DRIVE_RESULT_DETAILED CHANGES
##   FOR 2014 - NONE OF THOSE.
##   For 2015,2016,2018 - there's several of those (7 total):
##      Drive result changes, and it states "END OF HALF" in the PRE-PUNT FUMBLE.. 
##      SOLUTION? NEED TO JUST FIX THOSE THE SAME WAY as WHERE THE DRIVE RESULT DOESN'T CHANGE:
##           PRE-PUNT stuff set to "Punt"
##           Punt fumble a SEPARATE POSSESSION
##           Post-Punt fumble - AS IS.
##    => So I just drop the checking of "change in drive result" value.

ind.play.type <- which(pbp$play_type %in% c("Punt Team Fumble Recovery") & 
                         (pbp$drive_number == lead(pbp$drive_number)) # & (pbp$drive_result_detailed != lead(pbp$drive_result_detailed))
                       )
ind.play.type

# print("Number of problematic observations, need to be looked at:")
# print(length(ind.play.type))


# View(  #pbp[ind.play.type,] %>%
#   #pbp[sort(c(ind.play.type, ind.play.type+1)),] %>%
#   # pbp[sort(c(ind.play.type, ind.play.type-1, ind.play.type+1)),] %>%
#     pbp %>% filter(game_id_drive %in% c("400869188-17", "400869188-17.5","400869188-18")) %>%
#     select(game_id_drive, game_id_half,
#            pos_team, def_pos_team, drive_number, change_of_pos_team, drive_result_detailed, play_type, play_text,
#            penalty_flag,
#            yards_to_goal, yards_to_goal_end,
#            pos_team_score, def_pos_team_score, pos_score_diff_start, pos_score_diff_start_end, score_pts, pos_score_diff,
#            pos_team_receives_2H_kickoff))




## For ALL THE REGULAR ONES, we
##    MAKE IT INTO A ONE-PLAY POSSESSION BY THE RECEIVING TEAM, via:
##       - Adding "0.5" to the drive number, so that it is inbetween the pre- & post- fumble drives
##       - Flipping all the "pos/def pos team" stuff
##       - Setting "change_of_pos_team" to 1?
##       - Setting both the PLAY TYPE & DRIVE RESULT to "Punt Team Fumble Lost"
##       - Setting the DRIVE_RESULT for the PRE-FUMBLE drive to just "Punt"

ind.play.type <- which(pbp$play_type %in% c("Punt Team Fumble Recovery") & 
                         (pbp$drive_number != lead(pbp$drive_number)))
ind.play.type

pbp[ind.play.type, "drive_play_number"] <- 1
pbp[ind.play.type, c("drive_number")] <- pbp[ind.play.type, c("drive_number")] + 0.5
pbp[ind.play.type, c("pos_team", "def_pos_team")] <- pbp[ind.play.type, c("def_pos_team", "pos_team")]
pbp[ind.play.type, c("pos_team_score", "def_pos_team_score")] <- pbp[ind.play.type, c("def_pos_team_score", "pos_team_score")]
pbp[ind.play.type, c("pos_score_diff_start", "pos_score_diff_start_end", "score_pts", "pos_score_diff", "yards_gained")] <- - pbp[ind.play.type, c("pos_score_diff_start", "pos_score_diff_start_end", "score_pts", "pos_score_diff", "yards_gained")]
pbp[ind.play.type, "pos_team_receives_2H_kickoff"] <- 1 - pbp[ind.play.type, "pos_team_receives_2H_kickoff"]
pbp[ind.play.type, "change_of_pos_team"] <- 1

## Making sure the new drive numbers are incorporated into "game_id_drive"
pbp$game_id_drive <- paste(pbp$game_id, pbp$drive_number, sep="-")
pbp$game_id_drive <- factor(pbp$game_id_drive, levels = unique(pbp$game_id_drive))

pbp[ind.play.type, c("play_type", "drive_result_detailed")] <- c("Punt Team Fumble Lost", "Punt Team Fumble Lost")

## To set the PRE-FUMBLE DRIVE RESULTS to "PUNT":
##    1. Find those drive's "game_id_drive", by just doing "ind - 1"
##    2. Replace their "drive_result_detailed" to "Punt"

pbp[which(pbp$game_id_drive %in% unique(pbp$game_id_drive[ind.play.type-1])), "drive_result_detailed"] <- "Punt"



#####
## Putting NAs for drive TIME characteristics in case of "Punt Team Fumble Recovery" plays
##   PLUS making their TIME ("DRIVE DURATION") at 0s, which makes sense for how QUICKLY such plays typically unfold.
#####

pbp[which(pbp$play_type == "Punt Team Fumble Recovery"), 
    c("drive_time_minutes_start", "drive_time_seconds_start",
      "drive_time_minutes_end", "drive_time_seconds_end")] <- NA

pbp[which(pbp$play_type == "Punt Team Fumble Recovery"), 
    c("drive_time_minutes_elapsed", "drive_time_seconds_elapsed")] <- 0

pbp[which(pbp$play_type == "Punt Team Fumble Recovery"), c("drive_time_minutes_start", "drive_time_seconds_start")] <- 
  pbp[which(pbp$play_type == "Punt Team Fumble Recovery"), c("drive_time_minutes_end", "drive_time_seconds_end")]




#####
##  "Punt Team Fumble Recovery Touchdown"
#####

##   It has DRIVE_RESULT_DETAILED at "Punt Team Fumble Recovery Touchdown", and leads to a Kickoff and drive number change, obviously.
##    BUT we STILL WANT TO MAKE SURE that THERE IS NO "SNEAKY PLAY" AFTER IT (e.g. a penalty of some kind) THAT KEEPS THAT SAME DRIVE #..
ind.play.type <- which(pbp$play_type %in% c("Punt Team Fumble Recovery Touchdown") & 
                         pbp$drive_number != lead(pbp$drive_number))
ind.play.type

##
##    WE MAKE IT INTO A ONE-PLAY POSSESSION BY THE RECEIVING TEAM, via:
##       - Adding "0.5" to the drive number, so that it is inbetween the pre- & post- fumble drives
##       - Flipping all the "pos/def pos team" stuff
##       - Setting "change_of_pos_team" to 1?
##       - Setting the DRIVE_RESULT for the PRE-FUMBLE drive to just "Punt"

pbp[ind.play.type, "drive_play_number"] <- 1
pbp[ind.play.type, c("drive_number")] <- pbp[ind.play.type, c("drive_number")] + 0.5
pbp[ind.play.type, c("pos_team", "def_pos_team")] <- pbp[ind.play.type, c("def_pos_team", "pos_team")]
pbp[ind.play.type, c("pos_team_score", "def_pos_team_score")] <- pbp[ind.play.type, c("def_pos_team_score", "pos_team_score")]
pbp[ind.play.type, c("pos_score_diff_start", "score_pts", "pos_score_diff", "yards_gained")] <- - pbp[ind.play.type, c("pos_score_diff_start", "score_pts", "pos_score_diff", "yards_gained")]
pbp[ind.play.type, "pos_team_receives_2H_kickoff"] <- 1 - pbp[ind.play.type, "pos_team_receives_2H_kickoff"]

pbp$game_id_drive <- paste(pbp$game_id, pbp$drive_number, sep="-")
pbp$game_id_drive <- factor(pbp$game_id_drive, levels = unique(pbp$game_id_drive))

## To set the PRE-FUMBLE DRIVE RESULTS to "PUNT":
##    1. Find those drive's "game_id_drive", by just doing "ind - 1"
##    2. Replace their "drive_result_detailed" to "Punt"

ind.drive.pre <- which(pbp$game_id_drive %in% unique(pbp$game_id_drive[ind.play.type-1]))
if (length(ind.drive.pre) > 0) pbp[ind.drive.pre, "drive_result_detailed"] <- "Punt"

## Given that it's a 1-play possession, we: make "time_elapsed" stuff 0, and make drive_start = drive_end
ind.punt.fumb.recov.td <- which(pbp$play_type == "Punt Team Fumble Recovery Touchdown")
if (length(ind.punt.fumb.recov.td) > 0){
  pbp[ind.punt.fumb.recov.td, c("drive_time_minutes_elapsed", "drive_time_seconds_elapsed")] <- 0
  pbp[ind.punt.fumb.recov.td, c("drive_time_minutes_start", "drive_time_seconds_start")] <- 
    pbp[ind.punt.fumb.recov.td, c("drive_time_minutes_end", "drive_time_seconds_end")]
}


### CHECKING JUST IN CASE IF THERE WERE PLAYS WHERE DRIVE # DIDN'T CHANGE AFTER "Punt Fumble Recovery TD"...
##   Maybe due to Penalty or something along those lines

ind.play.type <- which(pbp$play_type %in% c("Punt Team Fumble Recovery Touchdown") & 
                         pbp$drive_number == lead(pbp$drive_number))
ind.play.type

# print("Number of problematic observations, need to be looked at:")
# print(length(ind.play.type))



#########
## "Blocked Punt Touchdown", "Punt Return Touchdown":
##     SIMILAR TO "PUNT FUMBLE TD"
#########


## All show: "change_of_pos = 0", drive number changes afterwards (Kickoff), drive result = Punt Return TD OR Blocked Punt TD
##    BUT we STILL WANT TO MAKE SURE that THERE IS NO "SNEAKY PLAY" AFTER IT (e.g. a penalty of some kind) THAT KEEPS THAT SAME DRIVE #..

ind.play.type <- which(pbp$play_type %in% c("Blocked Punt Touchdown", "Punt Return Touchdown")  & 
                         pbp$drive_number != lead(pbp$drive_number))
ind.play.type

##    WE MAKE IT INTO A ONE-PLAY POSSESSION BY THE RECEIVING TEAM, via:
##       - Adding "0.5" to the drive number, so that it is inbetween the pre- & post- return TD drives
##       - Flipping all the "pos/def pos team" stuff
##       - Setting "change_of_pos_team" to 1?
##       - Setting the DRIVE_RESULT for the PRE-FUMBLE drive to just "Punt"

pbp[ind.play.type, "drive_play_number"] <- 1
pbp[ind.play.type, c("drive_number")] <- pbp[ind.play.type, c("drive_number")] + 0.5
pbp[ind.play.type, c("pos_team", "def_pos_team")] <- pbp[ind.play.type, c("def_pos_team", "pos_team")]
pbp[ind.play.type, c("pos_team_score", "def_pos_team_score")] <- pbp[ind.play.type, c("def_pos_team_score", "pos_team_score")]
pbp[ind.play.type, c("pos_score_diff_start", "score_pts", "pos_score_diff", "yards_gained")] <- - pbp[ind.play.type, c("pos_score_diff_start", "score_pts", "pos_score_diff", "yards_gained")]
pbp[ind.play.type, "pos_team_receives_2H_kickoff"] <- 1 - pbp[ind.play.type, "pos_team_receives_2H_kickoff"]

pbp$game_id_drive <- paste(pbp$game_id, pbp$drive_number, sep="-")
pbp$game_id_drive <- factor(pbp$game_id_drive, levels = unique(pbp$game_id_drive))


## To set the PRE-RETURN TD DRIVE RESULTS to "PUNT":
##    1. Find those drive's "game_id_drive", by just doing "ind - 1"
##    2. Replace their "drive_result_detailed" to "Punt"

ind.drive.pre <- which(pbp$game_id_drive %in% unique(pbp$game_id_drive[ind.play.type-1]))
if (length(ind.drive.pre) > 0) pbp[ind.drive.pre, "drive_result_detailed"] <- "Punt"


### CHECKING JUST IN CASE IF THERE WERE PLAYS WHERE DRIVE # DIDN'T CHANGE AFTER "Punt Fumble Recovery TD"...
##   Maybe due to Penalty or something along those lines

ind.play.type <- which(pbp$play_type %in% c("Blocked Punt Touchdown", "Punt Return Touchdown") & 
                         pbp$drive_number == lead(pbp$drive_number))
ind.play.type

# print("Number of problematic observations, need to be looked at:")
# print(length(ind.play.type))

## Given that it's a 1-play possession, we: make "time_elapsed" stuff 0, and make drive_start = drive_end

ind.punt.return.td <- which(pbp$play_type %in% c("Blocked Punt Touchdown", "Punt Return Touchdown"))

if (length(ind.punt.return.td) >0){
  pbp[ind.punt.return.td, c("drive_time_minutes_elapsed", "drive_time_seconds_elapsed")] <- 0
  pbp[ind.punt.return.td, c("drive_time_minutes_start", "drive_time_seconds_start")] <- 
    pbp[ind.punt.return.td, c("drive_time_minutes_end", "drive_time_seconds_end")]
}



########
## PENALTIES ON PUNTS: 
########
##        If drive continues (the dataset doesn't change the possession) - we need to make that "PENALTY PUNT" PLAY into a SPECIAL TEAMS POSSESSION..
##        More specifically:
##           1. Make the previous drive's "drive_result_detailed" equal "Punt"
##           2. Convert the penalty into 1-play possession (drive_number + 0.5) for the receiving team, "drive_result_detailed" PENALTY TURNOVER
##           3. The subsequent plays: do +0.75, so that it doesn't get above the following drive

ind.play.type <- which(str_detect(tolower(pbp$play_text), "punt") & str_detect(tolower(pbp$play_text), "1st down") & pbp$play_type == "Penalty")
ind.play.type


##    WE MAKE IT INTO A ONE-PLAY POSSESSION BY THE RECEIVING TEAM, via:
##       - Flipping all the "pos/def pos team" stuff
##       - Setting "change_of_pos_team" to 1?

pbp[ind.play.type, "drive_play_number"] <- 1
pbp[ind.play.type, c("pos_team", "def_pos_team")] <- pbp[ind.play.type, c("def_pos_team", "pos_team")]
pbp[ind.play.type, c("pos_team_score", "def_pos_team_score")] <- pbp[ind.play.type, c("def_pos_team_score", "pos_team_score")]
pbp[ind.play.type, c("pos_score_diff_start", "score_pts", "pos_score_diff", "yards_gained")] <- - pbp[ind.play.type, c("pos_score_diff_start", "score_pts", "pos_score_diff", "yards_gained")]
pbp[ind.play.type, "pos_team_receives_2H_kickoff"] <- 1 - pbp[ind.play.type, "pos_team_receives_2H_kickoff"]

pbp[ind.play.type, "drive_result_detailed"] <- "Punt Penalty Turnover"
pbp[ind.play.type, "change_of_pos_team"] <- 1



pbp$game_id_drive <- paste(pbp$game_id, pbp$drive_number, sep="-")
pbp$game_id_drive <- factor(pbp$game_id_drive, levels = unique(pbp$game_id_drive))


######
##       - Adding "0.5" to the drive number, so that it is inbetween the pre- & post-punt penalty
##       - Adding "0.75" to the DRIVE NUMBER for plays that are POST-PUNT PENALTY, to make it HIGHER THAN THE PUNT PENALTY and LOWER that the SUBSEQUENT DRIVE
##       - Setting the DRIVE_RESULT for the PRE-PUNT PENALTY plays to just "Punt", and LEAVING THE POST-PUNT PENALTY "AS IS"
##        (Probably just do it via LOOP)

relev.drives <- pbp$game_id_drive[ind.play.type]
for (j in 1:length(relev.drives)){
  drive.id <- relev.drives[j]
  obs.id.in.drive <- which(pbp$game_id_drive %in% drive.id)
  
  for (i in 1:length(obs.id.in.drive)){
    # If it's the punt penalty play: add 0.5 to drive number
    if (obs.id.in.drive[i] == ind.play.type[j]){
      pbp$drive_number[obs.id.in.drive[i]] <- pbp$drive_number[obs.id.in.drive[i]] + 0.5
    }
    
    # If it's before the pre-punt penalty play: set drive result to "Punt"
    if (obs.id.in.drive[i] < ind.play.type[j]){
      pbp$drive_result_detailed[obs.id.in.drive[i]] <- "Punt"
    }
    # If it's after the post-punt penalty play: add 0.75 to drive number
    if (obs.id.in.drive[i] > ind.play.type[j]){
      pbp$drive_number[obs.id.in.drive[i]] <- pbp$drive_number[obs.id.in.drive[i]] + 0.75
    }
  }
  }


## Given that it's a 1-play possession, we: make "time_elapsed" stuff 0, and make drive_start = drive_end
ind.punt.penalty.drive <- which(pbp$drive_result_detailed == "Punt Penalty Turnover")
if (length(ind.punt.penalty.drive) > 0){
  pbp[ind.punt.penalty.drive, c("drive_time_minutes_elapsed", "drive_time_seconds_elapsed")] <- 0
  pbp[ind.punt.penalty.drive, c("drive_time_minutes_start", "drive_time_seconds_start")] <- 
    pbp[ind.punt.penalty.drive, c("drive_time_minutes_end", "drive_time_seconds_end")]
}


pbp$game_id_drive <- paste(pbp$game_id, pbp$drive_number, sep="-")
pbp$game_id_drive <- factor(pbp$game_id_drive, levels = unique(pbp$game_id_drive))



# View(  #pbp[ind.play.type,] %>%
#   #pbp[sort(c(ind.play.type, ind.play.type+1)),] %>%
#   pbp[sort(c(ind.play.type, ind.play.type-1, ind.play.type+1)),] %>%
#     # pbp %>% filter(game_id_drive %in% c("400547642-8", "400547642-8.5","400547642-8.75")) %>%
#     select(game_id_drive,
#            pos_team, def_pos_team, drive_number, change_of_pos_team, drive_result_detailed, play_type, play_text,
#            penalty_flag,
#            yards_to_goal, yards_to_goal_end,
#            pos_team_score, def_pos_team_score, pos_score_diff_start, pos_score_diff_start_end, score_pts, pos_score_diff,
#            pos_team_receives_2H_kickoff))







######
## BLOCKED PUNT SAFETY
## (VEEERY RARE)
######
##
##  VERY SIMILAR TO BLOCKED PUNT TD

##           3. The subsequent plays: do +0.75, so that it doesn't get above the following drive

ind.play.type <- which(pbp$play_type == "Blocked Punt (Safety)")
ind.play.type

##    WE MAKE IT INTO A ONE-PLAY POSSESSION BY THE RECEIVING TEAM, via:
##       - Adding "0.5" to the drive number, so that it is inbetween the pre- & post- return TD drives
##       - Flipping all the "pos/def pos team" stuff
##       - Setting "change_of_pos_team" to 0?
##       - Setting the DRIVE_RESULT for the PRE-FUMBLE drive to just "Punt"

pbp[ind.play.type, "drive_play_number"] <- 1
pbp[ind.play.type, c("drive_number")] <- pbp[ind.play.type, c("drive_number")] + 0.5
pbp[ind.play.type, c("pos_team", "def_pos_team")] <- pbp[ind.play.type, c("def_pos_team", "pos_team")]
pbp[ind.play.type, c("pos_team_score", "def_pos_team_score")] <- pbp[ind.play.type, c("def_pos_team_score", "pos_team_score")]
pbp[ind.play.type, c("pos_score_diff_start", "score_pts", "pos_score_diff", "yards_gained")] <- - pbp[ind.play.type, c("pos_score_diff_start", "score_pts", "pos_score_diff", "yards_gained")]
pbp[ind.play.type, "pos_team_receives_2H_kickoff"] <- 1 - pbp[ind.play.type, "pos_team_receives_2H_kickoff"]
pbp[ind.play.type, "change_of_pos_team"] <- 0
pbp[ind.play.type, "drive_result_detailed"] <- "Blocked Punt (Safety)"

pbp$game_id_drive <- paste(pbp$game_id, pbp$drive_number, sep="-")
pbp$game_id_drive <- factor(pbp$game_id_drive, levels = unique(pbp$game_id_drive))


## To set the PRE-RETURN TD DRIVE RESULTS to "PUNT":
##    1. Find those drive's "game_id_drive", by just doing "ind - 1"
##    2. Replace their "drive_result_detailed" to "Punt"

ind.drive.pre <- which(pbp$game_id_drive %in% unique(pbp$game_id_drive[ind.play.type-1]))
if (length(ind.drive.pre) > 0) pbp[ind.drive.pre, "drive_result_detailed"] <- "Punt"


### CHECKING JUST IN CASE IF THERE WERE PLAYS WHERE DRIVE # DIDN'T CHANGE AFTER "Punt Fumble Recovery TD"...
##   Maybe due to Penalty or something along those lines

ind.play.type <- which(pbp$play_type %in% c("Blocked Punt (Safety)") & 
                         pbp$drive_number == lead(pbp$drive_number))
ind.play.type

# print("Number of problematic observations, need to be looked at:")
# print(length(ind.play.type))

## Given that it's a 1-play possession, we: make "time_elapsed" stuff 0, and make drive_start = drive_end

ind.block.punt.safety <- which(pbp$play_type %in% c("Blocked Punt (Safety)"))

if (length(ind.punt.return.td) >0){
  pbp[ind.block.punt.safety, c("drive_time_minutes_elapsed", "drive_time_seconds_elapsed")] <- 0
  pbp[ind.block.punt.safety, c("drive_time_minutes_start", "drive_time_seconds_start")] <- 
    pbp[ind.block.punt.safety, c("drive_time_minutes_end", "drive_time_seconds_end")]
}







# View(pbp %>%
#        filter(
#          # str_detect(tolower(play_type), fixed("blocked punt")),
#          str_detect(tolower(play_type), fixed("punt team fumble lost")),
#          #  !is.na(yds_fumble_return),
#          # penalty_flag, !penalty_declined
#          #  game_id == 400548143
#        )
#      
#      %>% .[, c("game_id", "pos_team", "def_pos_team", "pos_team_score", "def_pos_team_score", "change_of_pos_team",
#                "play_type", "play_text",  "yards_to_goal", "yards_to_goal_end",
#                "penalty_flag", "yds_penalty",
#                #"fumble_vec",  "sack_vec",
#                "yards_gained", "yds_rushed", "yds_receiving", "yds_sacked", "yds_fumble_return",
#                "drive_time_minutes_elapsed", "drive_time_seconds_elapsed",
#                "period", "drive_time_minutes_end", "drive_time_seconds_end",
#                "down")])




########
### !!!! need to check all the POS vs DEF NAME SWITCHES within the SAME DRIVE... !!!!!!
########

## THE REST (about 11 drives with issues in 2014):
##      - With Kickoffs, there's "N/A kickoff for .. yards" issues
##      - with Fumble return TDs,
#               There seems to be an issue with "Fumble Return Touchdown" vs "Fumble Recovery (Opponent) Touchdown".. WHAT'S THE DIFFERENCE?
#               There's supposed to be NO DIFFERENCE... it's always "defense score".. BUT..
#               for some drives it FLIPS the offense & defense entries...
#       - Some weird penalties at the end shifting team order...
#
## Screw those, I'll just drop them.


hey <- pbp %>% 
  group_by(game_id_half, game_id_drive) %>%
  summarise(team.switch.count = length(unique(pos_team)),
            drive_res = drive_result_detailed[1])

table(hey$team.switch.count )
switchy.halves <- unique(hey$game_id_half[hey$team.switch.count > 1])
switchy.drives <- unique(hey$game_id_drive[hey$team.switch.count > 1])

dim(pbp)
pbp <- pbp[!(pbp$game_id_half %in% switchy.halves), ]
dim(pbp)



# pbp_by_drive <- pbp_by_drive[!(pbp_by_drive$game_id_half %in% switchy.halves), ]
# dim(pbp_by_drive)


# View(pbp %>%
#        #filter(game_id_drive %in% c( "400548086-21", "400548086-22", "400548086-23")) %>%
#        filter(game_id_drive %in% switchy.drives) %>%
#        # filter(drive_result_detailed == "Fumble Return Touchdown") %>%
#        # filter(drive_result_detailed == "Fumble Recovery (Opponent) Touchdown") %>%
#        select(game_id_drive,
#               drive_result_detailed, 
#               play_type,
#               pos_team, def_pos_team,
#               offense_score_play, defense_score_play,
#               # down,
#               #drive_num, drive_number,
#               play_text, play_type, 
#               yards_gained, yds_rushed, yds_receiving, yds_sacked, yds_fumble_return,
#               # drive_result_detailed_flag,
#               offense_score, defense_score, 
#               period,  TimeSecsRem, TimeSecsRem_end, SecondsElapsed, 
#               drive_num, drive_num,
#               clock.minutes, clock.seconds)) 




#########
## !!! CHECK CONSECUTIVE DRIVES where the POSSESSION TEAM HAS NOT CHANGED !!!
##   (DO IT AFTER ALL THE "0.5" POSSESSION HAVE BEEN ASSIGNED due to PUNTS/SPECIAL TEAM FUMBLE RECOVERIES/ON-SIDE KICKS ETC)
## 
## That should exclude all the INSTANT SCORES by the DEFENSIVE SIDE:
##      
#########

instant.scores <- c("Blocked Field Goal Touchdown", "Fumble Recovery (Opponent) Touchdown", 
                    "Fumble Return Touchdown", "Interception Return Touchdown", 
                    "Punt Team Fumble Recovery Touchdown", "Safety")

### FIXED THOSE:
##    BLOCKED PUNT (SAFETY) PLAY IS OK, SHOULD RETAIN IT !!
##    BLOCKED PUNT that got BACK TO THE PUNTING TEAM after a FUMBLE !! - SEE IF THAT'S FIXABLE into a POSSESSION BY RETURNING TEAM, etc...
##
##  THESE ARE UNFIXABLE:
##    "400547642-1" - RUNNING INTO A KICKER during a FG KICK (not PUNT) is MOSTLY SOLID, but just ONE TIME it messes up with the change in drive # (SHOULDN'T CHANGE IT)
##    "400548259-1" - A TERRIBLY-RECORDED GAME that only has 3 drives in one half.
##

bad.switch.halves <- pbp %>% 
  group_by(game_id_half) %>%
  # summarise(switch.mismatch = sum( (diff(as.numeric(factor(pos_team))) == 0) != (diff(drive_number) == 0))) %>%
  summarise(switch.mismatch = sum(ifelse(diff(as.numeric(factor(pos_team))) == 0,0,1) != ifelse(diff(drive_number) == 0, 0, 1) & !drive_result_detailed[-length(drive_result_detailed)] %in% instant.scores)) %>%
  filter(switch.mismatch != 0) %>% .[["game_id_half"]]

bad.switch.halves
# View(bad.switch.halves)



# View(pbp %>% 
#        filter(
#          #  game_id == 400548282, drive_number %in% c(16:18)
#          # game_id_drive %in% bad.game_id_drive
#          game_id_half == "400547983-2"
#          # game_id_half %in% run.into.kicker.halves
#          
#        )
#      
#      %>% .[, c("pos_team", "def_pos_team", "change_of_pos_team", "game_id_drive", "drive_number",
#                "drive_result_detailed", "pos_team_score", "def_pos_team_score", "change_of_pos_team", # "change_of_poss",
#                "play_type", "play_text",  "yards_to_goal", "yards_to_goal_end", 
#                "clock.minutes", "clock.seconds",
#                # "penalty_flag", "yds_penalty", 
#                #"fumble_vec",  "sack_vec", 
#                "yards_gained", "yds_rushed", "yds_receiving", "yds_sacked", "yds_fumble_return",
#                "drive_time_minutes_elapsed", "drive_time_seconds_elapsed",
#                "period", "drive_time_minutes_end", "drive_time_seconds_end",
#                "down")])



### WIPING OUT all the ones that WERE UNFIXABLE:

dim(pbp)
pbp <- pbp[!(pbp$game_id_half %in% bad.switch.halves), ]
dim(pbp)






######
## CLEANING UP "FIRST DOWNS GAINED"
######


## FIRST DOWN stuff:
##  "firstD_by_yards" seems to be a LAGGED version of "first_by_yards", with the latter being more relevant for DOWNS BEING GAINED
##  SAME FOR "firstD_by_penalty" OR ANY OTHER "firstD_.."
sum(pbp$first_by_yards != pbp$firstD_by_yards, na.rm=T)
sum(pbp$first_by_penalty != pbp$firstD_by_penalty, na.rm=T)

## Checking if "sum(first_by_yards == 1 | first_by_penalty == 1)" is good enough for # of 1st downs gained
##   NOT REALLY: clearly misses on some LEGITIMATE "1st down" mentions in "play_text" (which truly move the sticks)...
##   ALSO: For cases where FIRST_BY_YDS/PENALTY is =1, while there's NO MENTION OF "1st down" IN PLAY TEXT => IT SEEMS TO BE:
##      * EITHER BY ERROR, OR
##      * there are some CASES WHERE IT SEEMS TO BE FOLLOWED BY a "1st DOWN" !!! and a LEGITIMATE ONE !!!

table((pbp$first_by_yards == 1 | pbp$first_by_penalty == 1), str_detect(tolower(pbp$play_text), "1st down"))

pbp$play_text[(pbp$first_by_yards == 1 | pbp$first_by_penalty == 1) & !str_detect(tolower(pbp$play_text), "1st down")]
pbp$down[(pbp$first_by_yards == 1 | pbp$first_by_penalty == 1) & !str_detect(tolower(pbp$play_text), "1st down")]

pbp$play_text[!(pbp$first_by_yards == 1 | pbp$first_by_penalty == 1) & str_detect(tolower(pbp$play_text), "1st down")]
pbp$down[!(pbp$first_by_yards == 1 | pbp$first_by_penalty == 1) & str_detect(tolower(pbp$play_text), "1st down")]

## Checking "1st down" in play text, but "first_by" variables are at 0:
our.ind <- which(!(pbp$first_by_yards == 1 | pbp$first_by_penalty == 1) & str_detect(tolower(pbp$play_text), "1st down"))

# Cases where "first_by_yards/penalty = 1" but "1st down" isn't in play text => VASTLY ERRORS
# BUUUUT... there are some CASES WHERE IT SEEMS TO BE FOLLOWED BY a "1st DOWN" !!! and a LEGITIMATE ONE !!!
##          ALTHOUGH SEEMINGLY AFTER A FUMBLE most times
our.ind <- which((pbp$first_by_yards == 1 | pbp$first_by_penalty == 1) & !str_detect(tolower(pbp$play_text), "1st down"))
our.ind <- which((pbp$first_by_yards == 1 | pbp$first_by_penalty == 1) & !str_detect(tolower(pbp$play_text), "1st down") 
                 & (lead(pbp$down) == 1 & (lead(pbp$game_id_drive) == pbp$game_id_drive) & (lead(pbp$pos_team) == pbp$pos_team))
)

# Checking "1st down" in play text, and NON-1 next play's "Down" value
##  Some are 0's, which is fine
##  But the NON-ZEROS are POTENTIALLY AN ISSUE
our.ind <- which(str_detect(tolower(pbp$play_text), "1st down") & !(lead(pbp$down) %in% c(0,1)))
our.ind

### NEED TO EXPLICITLY CREATE the "first_down_gained" VARIABLE, that DOESN'T CARE if IT WAS BY PENALTY OR YARDS
##  TWO CASES THAT COUNT:
##      1. There's an explicit mention of "1st down" in the play text AND it's:
##          * either FOLLOWED BY "DOWN" VALUE THAT'S EITHER 1 OR 0 !!
##          * HAS NO PENALTY FLAG
##      2. There's a "1" in either first_by_yds or first_by_pen, plus a 1st down on the next play
## AND MAKING SURE THAT THERE'S AN OVERARCHING AGREEMENT ON:
##     THE SUBSEQUENT OBSERVATION being THE SAME DRIVE # & SAME POS_TEAM !!!
pbp$first_down_gained <- ifelse((str_detect(tolower(pbp$play_text), "1st down") & ( (lead(pbp$down) %in% c(0,1)) | !pbp$penalty_flag  ) ) | 
                                  ((pbp$first_by_yards == 1 | pbp$first_by_penalty == 1) & (lead(pbp$down) == 1))
                                & ((lead(pbp$game_id_drive) == pbp$game_id_drive) & (lead(pbp$pos_team) == pbp$pos_team)),
                                1, 0)

## 
# print("First downs ('first_down_gained' variable vs 1st down in text)")
# print(table(pbp$first_down_gained, str_detect(tolower(pbp$play_text), "1st down")))

pbp$first_down_gained

our.ind <- which(!pbp$first_down_gained & str_detect(tolower(pbp$play_text), "1st down") )
our.ind


## THIRD/FOURTH DOWN stuff
##
## CONVERTED: These ways of calculating converted 3rd & 4th downs seem reasonable (as "first_down_gained" ALREADY TAKES CARE OF ALL THE INTRICACIES)
our.ind <- which(pbp$down == 3 & pbp$first_down_gained == 1)
our.ind <- which(pbp$down == 4 & pbp$first_down_gained == 1)
# 

## TOTALS: 
##    For 3rd downs - it should just be "this is the 3rd down, next down is NOT the 3rd down"?
##    Same for 4th downs
our.ind <- which(pbp$down == 3 & (lead(pbp$down) != 3))
our.ind <- which(pbp$down == 3 & !(lead(pbp$down) != 3))  # Checking the "non-counting" cases

our.ind <- which(pbp$down == 4 & (lead(pbp$down) != 4))
our.ind <- which(pbp$down == 4 & !(lead(pbp$down) != 4))  # Checking the "non-counting" cases


# View(#pbp[sort(c(our.ind)), ] %>%
#   pbp[sort(c(our.ind, our.ind+1)), ] %>%
#     #pbp %>%  filter(game_id_drive %in% c("400610209-10")) %>%
#     select(game_id_drive, pos_team, def_pos_team, down, penalty_flag,play_type, play_text, first_by_penalty, first_by_yards)
# )


#####
## For year 2018, there are 2 weird NA values for Kickoff plays:

if (year == 2018){
  our.ind <- which(pbp$play_type == "Kickoff" & is.na(pbp$yards_gained) & is.na(pbp$yards_to_goal))
  if (length(our.ind) > 0){
    pbp$yards_to_goal[our.ind] <- 65
    pbp$yards_gained[our.ind] <- 35 - pbp$yards_to_goal_end[our.ind]
  }
}

cat("\n")
cat("\n")
print("DATA SET SIZE FINAL PRE-BY-DRIVE")
print(dim(pbp))
print(length(unique(pbp$game_id_half)))
cat("\n")
cat("\n")






#############################################################################################
#############################################################################################
#############################################################################################



#############
#############
####  THE "BY-DRIVE" DATA SET
####    (WATCH OUT for REDEFINING THE VARIABLE before YOU USE IT IN DEFINITION OF THE FOLLOWING VARIABLE,
####     e.g. redefining "punt_return" before it's used in calculating "punt_net")
#############
#############

## "YARDS_TO_GO" as a "STOPPED POSITION"?? And then we'll rely on WHETHER PREVIOUS POSSESSION ended in a PUNT, TD/FG, or TURNOVER...

# unique(pbp$drive_result_detailed)
# "Punt" - 100-[yards_to_goal]
# "Fumble, Interception Return, Fumble Recovery, Downs Turnover, Blocked FG, Blocked Punt, FG missed, Missed FG Return" - yards_to_goal_end?
# "Passing/Rushing TD, Punt/Kickoff Return TD, Fumble Recovery/Return TD, Interception Return TD, FG Good" - 35
# "Safety, Kickoff Safety" - 20


## FEB 23rd, 2023:
##    WATCH OUT for NA in "drive_result_detailed" once it gets to SUMMARIZING DRIVES

## All the play types do make sense for respective indicators
table(pbp$play_type[pbp$pass_attempt == 1])
table(pbp$play_type[pbp$completion == 1])
table(pbp$play_type[pbp$rush == 1])


all.drive.results <- sort(unique(pbp$drive_result_detailed))
scoring_drive_results <- c("Field Goal Good", 
                           "Safety", 
                           "Blocked Punt (Safety)",
                           all.drive.results[str_detect(tolower(all.drive.results), "touchdown")])



####
## MAKING DRIVE NUMBERS PROPER CONSECUTIVE INTEGERS
####

## All distinct triples of game_id, drive_no, drive_result
all.distinct.triples <- pbp[!(duplicated(pbp[, c("game_id", "drive_number", "drive_result_detailed")])), 
                            c("game_id", "drive_number", "drive_result_detailed")]
# View(all.distinct.triples)
all.distinct.triples <- all.distinct.triples %>% group_by(game_id) %>% mutate(drive_new_number = row_number())
all.distinct.triples

pbp <- pbp %>% left_join(all.distinct.triples, by=c("game_id", "drive_number", "drive_result_detailed"))
pbp$drive_number <- pbp$drive_new_number

pbp$game_id_drive <- paste(pbp$game_id, pbp$drive_number, sep="-")
pbp$game_id_drive <- factor(pbp$game_id_drive, levels = unique(pbp$game_id_drive))
pbp$drive_new_number <- NULL
# View(pbp %>% select(game_id, game_id_drive, drive_number, drive_result_detailed))






pbp_by_drive <- pbp %>% select(game_id, drive_number, game_id_drive, game_id_half, week, home,  # drive_id,
                               play_type,  play_text, period,
                               
                               pos_team,
                               def_pos_team,
                               
                               change_of_pos_team,
                               
                               first_by_yards, first_by_penalty, first_down_gained,
                               
                               pass_attempt,
                               completion,
                               rush,
                               
                               yds_receiving,
                               yds_rushed,
                               
                               fumble_vec,
                               yds_fumble_return,
                               
                               yds_after_int,
                               
                               drive_time_minutes_elapsed, drive_time_seconds_elapsed,
                               drive_time_minutes_end, drive_time_seconds_end,
                               clock.minutes, clock.seconds,
                               
                               score_pts, pos_score_pts, score_diff_start, score_diff, scoring_play,
                               pos_team_score, def_pos_team_score,
                               
                               yds_penalty,
                               penalty_flag, penalty_declined, penalty_offset, penalty_no_play,
                               
                               yards_gained,
                               
                               yds_fg_return,
                               
                               yds_kickoff_return,
                               yds_kickoff,
                               kickoff_tb,
                               
                               yards_to_goal,
                               yards_to_goal_end,
                               
                               yds_punt_return,
                               yds_punted,
                               yds_punt_gained,
                               
                               yds_sacked,
                               
                               drive_play_number,
                               drive_yards,
                               sack,
                               drive_result_detailed, pos_team, def_pos_team,
                               down,
                               
                               rz_play
                               
                               )




dim(pbp_by_drive)

# View(pbp_by_drive)

ST.plays <- unique(pbp_by_drive$play_type)[str_detect(tolower(unique(pbp_by_drive$play_type)), "punt|kickoff")]
# ST.plays <- c("Punt", "Punt Return Touchdown", "Punt Team Fumble Lost", "Punt Team Fumble Recovery Touchdown", 
#               "Blocked Punt", "Blocked Punt Touchdown", 
#               "Kickoff", "Kickoff Return Touchdown", "Kickoff Team Fumble Recovery", "Kickoff (Safety)")

# PENALTIES during PUNTS & KICKOFFS:
ST.penalty.obs <- which(!(pbp_by_drive$play_type %in% ST.plays) & str_detect(tolower(pbp_by_drive$play_text), "\\bpunt\\b|\\bpunted\\b|\\bkickoff\\b"))
# View(pbp_by_drive[ST.penalty.obs, ])
sort(unique(pbp_by_drive$play_type))

print("# of Non-Penalty ST plays that don't have ST in the name")
print(length(which(pbp_by_drive[ST.penalty.obs,]$play_type != "Penalty")))
# ST.penalty.obs[c(9,25, 46)]

str_detect("punted", "\\bpunt\\b|\\bpunted\\b")


#             stopped_position = ifelse(tolower(tail(drive_result_detailed,1)) == "punt",
#                                       100 - tail(yards_to_goal_end,1),
#                                       ifelse(str_detect(tolower(tail(drive_result_detailed,1)), "safety"),
#                                              20,
#                                              ifelse(str_detect(tolower(tail(drive_result_detailed,1)), "touchdown") | (tolower(tail(drive_result_detailed,1)) == "field goal good"),
#                                                     35,
#                                                     tail(yards_to_goal_end,1)))),
#             


### Create a "side" variable, which takes on:
##    * "Offense" if the home-designated team is on offense
##    * "Defense" if the home-designated team is on defense
##    * "ST.Return" if the home-designated team is returning on special teams
##    * "ST.Cover" if the home-designated team is receiving on special teams

pbp_by_drive$side <- ifelse(pbp_by_drive$play_type %in% ST.plays,
                            ifelse(pbp_by_drive$pos_team == pbp_by_drive$home, "ST.Return", "ST.Cover"),
                            ifelse(pbp_by_drive$pos_team == pbp_by_drive$home, "Offense", "Defense"))

pbp_by_drive$side[ST.penalty.obs] <- ifelse(pbp_by_drive$pos_team[ST.penalty.obs] == pbp_by_drive$home[ST.penalty.obs],
                                            "ST.Return", "ST.Cover")

# View(pbp_by_drive %>% 
#       select(game_id, drive_number, game_id_drive, game_id_half, home, pos_team, def_pos_team, play_type, play_text, side))



## Blocked punts that resulted in a "non-Punt" play type 
## For them, just change the drive # of that play by +0.2, to "sandwich" it between the preceding and subsequent drives
bad.block.punt.id <- which(str_detect(tolower(pbp_by_drive$play_text), "punt blocked") & !str_detect(tolower(pbp_by_drive$play_type), "punt") & pbp_by_drive$play_type != "Penalty")

print("Number of bad blocked punts")
print(length(bad.block.punt.id))

pbp_by_drive$drive_number[bad.block.punt.id] <- pbp_by_drive$drive_number[bad.block.punt.id] + 0.2
pbp_by_drive$game_id_drive <- as.character(pbp_by_drive$game_id_drive)

pbp_by_drive$game_id_drive[bad.block.punt.id] <- as.character(paste0(pbp_by_drive$game_id[bad.block.punt.id],
                                                                     "-", 
                                                                     pbp_by_drive$drive_number[bad.block.punt.id]))


## Find spots where ST play does NOT CHANGE THE DRIVE NUMBER (so it could've been a ROUGHING THE KICKER, OFFENSE BACK ON THE FIELD)
ind.type <- which(str_detect(pbp_by_drive$side, "ST.") & 
                    (lag(pbp_by_drive$drive_number) == pbp_by_drive$drive_number) & (lead(pbp_by_drive$drive_number) == pbp_by_drive$drive_number) & 
                    (lag(pbp_by_drive$game_id_half) == pbp_by_drive$game_id_half) & (lead(pbp_by_drive$game_id_half) == pbp_by_drive$game_id_half))

# View(pbp_by_drive[sort(c(ind.type, ind.type-1, ind.type+1)),] %>%
#   select(game_id, drive_number, game_id_drive, game_id_half, home, pos_team, def_pos_team, play_type, play_text, side))

print("The number of problematic ST transitions INITIALLY (the kickoff penalties & roughing the kicker on punts)")
print(length(ind.type))


sort(unique(pbp_by_drive$game_id_drive))


## Getting all the Kickoffs that had a penalty on the first try
## and changing their & penalty play's drive # to "-0.2" from the next one
ind.subtype <- which(str_detect(tolower(pbp_by_drive[ind.type,]$play_type), "kickoff") & (pbp_by_drive[ind.type-1,]$play_type == "Penalty"))

pbp_by_drive[sort(c(ind.type[ind.subtype], ind.type[ind.subtype]-1)), ]$drive_number <- pbp_by_drive[sort(c(ind.type[ind.subtype], ind.type[ind.subtype]-1)), ]$drive_number-0.2

pbp_by_drive$game_id_drive <- as.character(pbp_by_drive$game_id_drive)

pbp_by_drive$game_id_drive[sort(c(ind.type[ind.subtype], ind.type[ind.subtype]-1))] <- as.character(paste0(pbp_by_drive$game_id[sort(c(ind.type[ind.subtype], ind.type[ind.subtype]-1))],
                                                                                                "-", 
                                                                                                pbp_by_drive$drive_number[sort(c(ind.type[ind.subtype], ind.type[ind.subtype]-1))]))

pbp_by_drive$game_id_drive <- factor(pbp_by_drive$game_id_drive, levels = unique(pbp_by_drive$game_id_drive))
sort(unique(pbp_by_drive$game_id_drive))

print("Amount of kickoff penalties fixed:")
print(length(ind.subtype))


## Getting all the Punts that ended up with roughing the kicker, and offense coming back on the field.
##  Changing the ID of the punt play to be +0.2, and the IDs of all subsequent plays within that same drive as +0.4

ind.subtype <- which(str_detect(tolower(pbp_by_drive[ind.type,]$play_text), "\\bpunt\\b|\\bpunted\\b") & str_detect(tolower(pbp_by_drive[ind.type,]$play_type), "penalty") |
                       str_detect(tolower(pbp_by_drive[ind.type,]$play_text), "fake punt"))
ind.subtype

pbp_by_drive$game_id_drive <- as.character(pbp_by_drive$game_id_drive)


pbp_by_drive[ind.type[ind.subtype], ]$drive_number <- pbp_by_drive[ind.type[ind.subtype], ]$drive_number + 0.2
pbp_by_drive$game_id_drive[ind.type[ind.subtype]] <- paste0(pbp_by_drive$game_id[ind.type[ind.subtype]], "-", pbp_by_drive$drive_number[ind.type[ind.subtype]])


post.punt.roughing.drive.game_id_drive <-  pbp_by_drive[ind.type[ind.subtype]+1,]$game_id_drive

for (j in 1:length(ind.subtype)){
  ind.minitype <- which(pbp_by_drive$game_id_drive == post.punt.roughing.drive.game_id_drive[j])
  ind.minitype <- ind.minitype[ind.minitype > ind.type[ind.subtype[j]]]  # Getting only the plays AFTER the punt with roughing the kicker
  pbp_by_drive$drive_number[ind.minitype] <- pbp_by_drive$drive_number[ind.minitype] + 0.4
  pbp_by_drive$game_id_drive[ind.minitype] <- paste0(pbp_by_drive$game_id[ind.minitype], "-", pbp_by_drive$drive_number[ind.minitype])
}

# Checking the punt/roughing kicker scenarios:
#
# View(pbp_by_drive[sort(c(ind.type[ind.subtype], ind.type[ind.subtype]+1, ind.type[ind.subtype]-1)), ]  %>%
#        select(game_id, drive_number, game_id_drive, game_id_half, home, pos_team, def_pos_team, play_type, play_text, side))

print("Amount of punt roughing kicker scenarios fixed:")
print(length(ind.subtype))

# Checking all replaced scenarios:
# View(pbp_by_drive[sort(c(ind.type, ind.type+1, ind.type-1)), ]  %>%
#        select(game_id, drive_number, game_id_drive, game_id_half, home, pos_team, def_pos_team, play_type, play_text, side))



print("Checking how many bad ST transitions remain:")
ind.type <- which(str_detect(pbp_by_drive$side, "ST.") & 
                    (lag(pbp_by_drive$drive_number) == pbp_by_drive$drive_number) & (lead(pbp_by_drive$drive_number) == pbp_by_drive$drive_number) & 
                    (lag(pbp_by_drive$game_id_half) == pbp_by_drive$game_id_half) & (lead(pbp_by_drive$game_id_half) == pbp_by_drive$game_id_half))

print(length(ind.type))


# View(pbp_by_drive[sort(c(ind.type, ind.type-1, ind.type+1)),] %>%
#  # pbp_by_drive %>%
#     select(game_id, drive_number, 
#            game_id_drive, game_id_half, 
#            home, pos_team, def_pos_team, play_type, play_text, side, 
#            drive_result_detailed,
#            yards_to_goal, yards_to_goal_end))




### !!!! CHECK CASES where SPECIAL TEAMS - FOR THE SAME TEAM - ARE MORE THAN 1 PLAY IN A ROW!!!
### Besides the PENALTY ON KICKOFFS, which are OK
ind.type <- which(str_detect(pbp_by_drive$side, "ST.") & str_detect(lead(pbp_by_drive$side), "ST.") &
                     (lead(pbp_by_drive$drive_number) == pbp_by_drive$drive_number) & 
                    (lead(pbp_by_drive$game_id_half) == pbp_by_drive$game_id_half) &
                    !(pbp_by_drive$play_type == "Penalty" & lead(pbp_by_drive$play_type) == "Kickoff"))
ind.type

print("Other 2-play ST situations:")
print(length(ind.type))


## And FINALLY, check all the REGULAR ST PLAYS, where the THE SUBSEQUENT

ind.type <- which(str_detect(pbp_by_drive$side, "ST.") & !str_detect(lead(pbp_by_drive$side), "ST.") &
                    (lead(pbp_by_drive$drive_number) == pbp_by_drive$drive_number) & (lead(pbp_by_drive$game_id_half) == pbp_by_drive$game_id_half))
ind.type

pbp_by_drive[ind.type, ]$drive_number <- pbp_by_drive[ind.type, ]$drive_number-0.2

pbp_by_drive$game_id_drive <- as.character(pbp_by_drive$game_id_drive)

pbp_by_drive$game_id_drive[ind.type] <- as.character(paste0(pbp_by_drive$game_id[ind.type],
                                                            "-", 
                                                            pbp_by_drive$drive_number[ind.type]))

# mini.ind <- ST.penalty.obs[c(9,25, 46) ]
# View(pbp_by_drive[sort(c(mini.ind, mini.ind-1,mini.ind+1)), ] %>%
#   select(game_id, drive_number, game_id_drive, game_id_half, home, pos_team, def_pos_team, play_type, play_text, side))
#   


## Seems like there's a handful of PASSING/RUSHING TD bad entries
cat("\n")
cat("\n")
print("# of bad passing/rushing TD recordings:")
ind.type <- which(!(pbp_by_drive$play_type %in% c(ST.plays, "Penalty")) &
                    pbp_by_drive$side %in% c("ST.Return", "ST.Cover") & 
                    pbp_by_drive$play_type %in% c("Rushing Touchdown", "Passing Touchdown"))
ind.type
print(length(ind.type))
cat("\n")
cat("\n")

pbp_by_drive[ind.type, ]$side <- ifelse(pbp_by_drive[ind.type, ]$pos_team == pbp_by_drive[ind.type, ]$home,
                                        "Offense",
                                        "Defense")

####
## REMOVING THE "END OF" PLAYS
####

pbp_by_drive <- pbp_by_drive[!str_detect(tolower(pbp_by_drive$play_type), "end of"),]



####
## MAKING DRIVE NUMBERS PROPER CONSECUTIVE INTEGERS
####

## All distinct triples of game_id, drive_no, drive_result
all.distinct.triples <- pbp_by_drive[!(duplicated(pbp_by_drive[, c("game_id", "drive_number", "drive_result_detailed")])), 
                            c("game_id", "drive_number", "drive_result_detailed")]
# View(all.distinct.triples)
all.distinct.triples <- all.distinct.triples %>% group_by(game_id) %>% mutate(drive_new_number = row_number())
all.distinct.triples

pbp_by_drive <- pbp_by_drive %>% left_join(all.distinct.triples, by=c("game_id", "drive_number", "drive_result_detailed"))
pbp_by_drive$drive_number <- pbp_by_drive$drive_new_number

pbp_by_drive$game_id_drive <- paste(pbp_by_drive$game_id, pbp_by_drive$drive_number, sep="-")
pbp_by_drive$game_id_drive <- factor(pbp_by_drive$game_id_drive, levels = unique(pbp_by_drive$game_id_drive))
pbp_by_drive$drive_new_number <- NULL
# View(pbp_by_drive %>% select(game_id, game_id_drive, drive_number, play_type, side, drive_result_detailed))



# 
# View(pbp_by_drive %>%
#        filter(game_id == "400548175") %>%
#        select(game_id, drive_number, game_id_drive, game_id_half, home, pos_team, def_pos_team, play_type, play_text, side))


# print("'End of' play text?? Although I thought I've deleted it all?")
# print(length(which(str_detect(tolower(pbp_by_drive$play_text), "end of"))))

pbp_by_drive_full <- pbp_by_drive %>%
  
 # filter(!str_detect(tolower(play_text), "end of")) %>%
  
  group_by(game_id_half,
           game_id_drive, 
           week,
           side,
           drive_result_detailed, pos_team, def_pos_team, home
  ) %>%
  summarize(n.pass_attempts = sum(pass_attempt, na.rm=T),
            n.completions = sum(completion, na.rm=T),
            n.incompletions = n.pass_attempts - n.completions,

            n.rush = sum(rush, na.rm=T),
            n.stuffed.runs = sum(rush == 1 & (str_detect(tolower(play_text), fixed("run for no gain")) | str_detect(tolower(play_text), fixed("run for a loss")))),

            tot.yds_receiving = sum(yds_receiving, na.rm=T),
            tot.yds_rushed = sum(yds_rushed, na.rm=T),


            ## Time elapsed, Drive start time, period end
            drive_time_period_start = head(period,1),
            drive_time_minutes_start = head(clock.minutes, 1),
            drive_time_seconds_start = head(clock.seconds, 1),

            ## THESE ONES ARE SIMPLY FOR DRIVE-DURATION DIAGNOSTIC PURPOSES,
            ## TO BE DELETED LATER:
            drive_time_minutes_end = tail(drive_time_minutes_end, 1),
            drive_time_seconds_end = tail(drive_time_seconds_end, 1),

            ## Time elapsed,  period end
            drive_duration_seconds = 60*drive_time_minutes_elapsed[!is.na(drive_time_minutes_elapsed) & !is.na(drive_time_seconds_elapsed)][1] +
              drive_time_seconds_elapsed[!is.na(drive_time_minutes_elapsed) & !is.na(drive_time_seconds_elapsed)][1],
            #period_end = tail(period, 1),


            ## STOPPED POSITION: HOW MUCH There's TO GO for your OPPONENT at the START OF THEIR NEXT POSESSION,
            ## (NOTE: CHANGE_OF_POS is OF NO USE HERE - it's MUCH TOUGHER TO LEVERAGE IT than just BREAK IT DOWN BY OBVIOUS CASES)
            ##   be it: the position from which you're punting,
            #           kicking off (35yds to YOUR goal line at the time of YOUR OWN KICKOFF),
            #           where you committed turnover, etc
            # "Punt" - 100-[yards_to_goal]
            # "Fumble, Interception Return, Fumble Recovery, Downs Turnover, Blocked FG, Blocked Punt, FG missed, Missed FG Return" - yards_to_goal_end?
            # "Passing/Rushing TD, Punt/Kickoff Return TD, Fumble Recovery/Return TD, Interception Return TD, FG Good" - 35
            # "Safety, Kickoff Safety" - 20

            stopped_position = ifelse(side[1] %in% c("Offense", "Defense"),
                                      ifelse(tolower(tail(drive_result_detailed,1)) == "punt",
                                             100 - tail(yards_to_goal_end,1),
                                             ifelse(str_detect(tolower(tail(drive_result_detailed,1)), "safety"),
                                                    20,
                                                    ifelse(str_detect(tolower(tail(drive_result_detailed,1)), "touchdown") | (tolower(tail(drive_result_detailed,1)) == "field goal good"),
                                                           35,
                                                           tail(yards_to_goal_end,1)))),
                                      tail(yards_to_goal_end,1)),



            ## Scoring

            ## SCORING
            ## ISSUES: Some scoring drives actually have "scoring play=0" still...
            #    On SCORING PLAYS (scoring_play = 1): score_pts & pos_score_pts are ALWAYS THE SAME
            #    on NON-SCORING PLAYS (scoring_play = 0): when score_pts & pos_score_pts are !=0, then ROLL WITH "pos_score_pts" - in KICKOFFS it shows PROPER SIGN
            #  AFTER THAT: STILL HAS A BUNCH OF ISSUES, so LATER I CREATE A VARIABLE "score_pts_by_text" PURELY BASED ON PLAY/DRIVE RESULT TEXT

            score_pts = ifelse(max(scoring_play) > 0,
                               score_pts[which(scoring_play == 1)],
                               ifelse(drive_result_detailed[1] %in% scoring_drive_results,
                                      pos_score_pts[pos_score_pts != 0],
                                      0)),

            score_diff_start = pos_team_score[1] - def_pos_team_score[1],
            score_diff_stop = tail(pos_team_score,1) - tail(def_pos_team_score,1),

            pos_team_score_end = tail(pos_team_score,1),
            def_pos_team_score_end = tail(def_pos_team_score,1),



            ## KICKOFFS

            yds_kickoff_net = ifelse(any(str_detect(tolower(play_type), "kickoff")),
                                     yards_gained[which(str_detect(tolower(play_type), "kickoff"))],
                                     NA),

            ## Dropped "25 yd improvement" off, because it's too tough to deal with various cases in possession changes for "yards_to_goal_end"..
            ## of TD return, fumble recovery etc etc etc...
            #
            # yds_kickoff_25yd_improvement = ifelse(any(str_detect(tolower(play_type), "kickoff")),
            #                                       75 - ifelse(change_of_pos_team[which(str_detect(tolower(play_type), "kickoff"))] == 0,
            #                                                   yards_to_goal_end[which(str_detect(tolower(play_type), "kickoff"))],
            #                                                   100-yards_to_goal_end[which(str_detect(tolower(play_type), "kickoff"))]),
            #                                       NA),

            kickoff_touchback =  ifelse(any(str_detect(tolower(play_type), "kickoff") & str_detect(tolower(play_text), "touchback")),
                                        1,
                                        0),

            yds_kickoff_return = ifelse(any(str_detect(tolower(play_type), "kickoff")),
                                        yds_kickoff_return[which(str_detect(tolower(play_type), "kickoff"))],
                                        NA),

            yds_kickoff = ifelse(any(str_detect(tolower(play_type), "kickoff")),
                                 yds_kickoff[which(str_detect(tolower(play_type), "kickoff"))],
                                 NA),


            ## PUNTING

            yds_punt_net = ifelse(any(!is.na(yds_punted)),
                                  yards_gained[which(!is.na(yds_punted))],
                                  NA),

            yds_punt_return = ifelse(any(!is.na(yds_punt_return)),
                                     yds_punt_return[!is.na(yds_punt_return)],
                                     NA),

            yds_punted = ifelse(any(!is.na(yds_punted)), yds_punted[which(!is.na(yds_punted))], NA),


            ## FG returns
            yds_fg_return = ifelse(any(!is.na(yds_fg_return)), yds_fg_return[which(!is.na(yds_fg_return))], NA),

            ## Make YARDS GAINED BY OFFENSE ONLY, and then have yards !!! OFF OF PUNTS, KICKOFFS & FG BLOCKS SEPARATELY !!!
            ## There are NAs due to having "End of Half" plays mess it up, will just keep track of them separately via "na.yards_gained"
            ## ALSO we need to exclude POST-TD/FG PENALTY from the PURE OFFENSIVE YARDS

            off.na.yards_gained = sum(is.na(yards_gained[which(!str_detect(tolower(play_type), "punt") & !str_detect(tolower(play_type), "kickoff") &
                                                             !str_detect(tolower(play_type), "field goal") & !str_detect(tolower(play_type), "end of"))])),
            off.yards_gained = sum(yards_gained[which(!str_detect(tolower(play_type), "punt") & !str_detect(tolower(play_type), "kickoff") &
                                                  !str_detect(tolower(play_type), "field goal") & !str_detect(tolower(play_type), "end of") &
                                                    !(play_type == "Penalty" & (str_detect(tolower(lag(play_type)), "touchdown") |  str_detect(tolower(lag(play_type)),"field goal"))))]
                                   , na.rm=T),

            ## There are NAs due to having "End of Half" plays mess it up, will just keep track of them separately via "na.yards_gained"
            tot.na.yards_gained = sum(is.na(yards_gained)),
            tot.yards_gained = sum(yards_gained, na.rm=T),

            # ### OLD STUFF (BELOW)
            # drive_yards = ifelse(str_detect(tolower(play_type[1]), "punt"),
            #                      ifelse(#str_detect(tolower(drive_result_detailed[1]), "touchdown") | str_detect(drive_result_detailed[1], "TD"),
            #                        length(play_type) == 1,
            #                        yds_punt_net,
            #                        min(drive_yards[-1], na.rm=T)),
            #                      min(drive_yards, na.rm=T)),  # WILL HAVE TO CHANGE THAT FOR PUNT-RELATED STATS THOUGH
            # ### OLD STUFF (ABOVE)


            ## SACKS: sack variables seem alright.
            n.sacks = sum(sack, na.rm=T),
            tot.yds_sacked = sum(yds_sacked, na.rm=T),

            ## FUMBLES: fumble variable has issues, missing some returns for TD, but it could be alleviated via using "play_text" PLUS EXCLUDING "PENALTY NO PLAY" ones
              # n.fumbles = sum(fumble_vec | (play_type == "Fumble Return Touchdown"), na.rm=T),
            n.fumbles = sum(str_detect(tolower(play_text), "fumbl") & !penalty_no_play),

            ## PASS BREAKUPS: "pass_breakup_stat" variable has issues, wrongly indicating some plays as broken up passes while they are not,
            ## but it could be alleviated via using "play_text" PLUS EXCLUDING "PENALTY NO PLAY" ones
            n.pass.breakups = sum(str_detect(tolower(play_text), "broken up") & !penalty_no_play),

            # These are yards after the fumble, and are relatively shaky (many are NA..), but probably not that critical anyway.
            tot.yds_fumble_return =  sum(yds_fumble_return, na.rm=T),

            # NEWLY ADDED
            tot.yds_after_int =  sum(yds_after_int, na.rm=T),

            first.downs.gained = sum(first_down_gained == 1),
            third.downs.converted = sum(down == 3 & (first_down_gained == 1)),
            fourth.downs.converted = sum(down == 4 & (first_down_gained == 1)),

            third.downs.total = sum((down == 3 & (lead(down) != 3 | is.na(lead(down)))), na.rm=T),
            fourth.downs.total = sum((down == 4 & (lead(down) != 4 | is.na(lead(down)))), na.rm=T),

            # There's a bunch of penalty plays that didn't record yardages (e.g. "Offside ( Yards)"), or keep saying "0 yards"...
            # Offsetting and declined penalties should not count towards this... UNLESS there was ANOTHER penalty in that same play
            # So just go with "yds_penalty" being < or > 0, and KEEP IN MIND THAT IT COULD BE LESS THAN RELIABLE with some of the recording issues
            penalties.offense = sum(yds_penalty < 0, na.rm=T),
            penalties.defense = sum(yds_penalty > 0, na.rm=T),


            # This will not include
            #     1) penalties committed on kickoff/punt returns, which could NOT be reliably distinguished and already incorporated into net yardages;
            #     2) offsetting penalties;
            #     3) declined penalties.
            tot.yds_penalty = sum(yds_penalty, na.rm=T),


            ## Plays with no penalties = just calculate the # of distinct "drive_play_number" values
            ## Plays with penalties = calculate all non-zero "down" values (zero values correspond to "End of half" stuff, which isn't really a play)
            n.plays.no.penalties = length(unique(drive_play_number[!str_detect(tolower(play_type), "end of")])),
            n.plays.with.penalties = sum(down != 0 & !str_detect(tolower(play_type), "end of")),

            # Negative plays:
            #   Either excluding pure penalty plays, or not.
            #   If it's a play that unfolded PLUS a penalty => counts as a regular, non-penalty, play
            n.negative.plays.no.penalties = sum(yards_gained[str_detect(tolower(play_type),"penalty")] < 0, na.rm=T),
            n.negative.plays.with.penalties = sum(yards_gained < 0, na.rm=T),


            # Red zone visits:
            #   THAT SHOULD WORK, BUT MIGHT NEED SOME CHECKING...
            rz_visit = any(yards_to_goal <= 20),


            ## INDICATOR of >2 PLAY DRIVES with STAGNANT CLOCK
            ## (EXCLUDING "PUNT" PLAYS, which could CARRY OVER FROM PREVIOUS DRIVE'S STAGNANT CLOCK)..
            ##  And DRIVES STARTING FROM 15:00 SEEM TO HAVE ISSUES IN SOME YEARS...
            stagn.clock = ifelse(n.plays.with.penalties > 2,
                                 all(diff(60*clock.minutes[!str_detect(tolower(play_type), "punt")] +
                                            clock.seconds[!str_detect(tolower(play_type), "punt")]) == 0,
                                     na.rm=T),
                                 # all(diff(60*clock.minutes[!str_detect(tolower(play_type), "punt") & clock.minutes != 15] +
                                 #            clock.seconds[!str_detect(tolower(play_type), "punt")] & clock.seconds != 0) == 0,
                                 #     na.rm=T),
                                 FALSE),

            # first.play.type.no.pen = head(play_type[play_type != "Penalty" & !str_detect(tolower(play_type), "end of")], 1),
            # first.play.text.no.pen = head(play_text[play_type != "Penalty" & !str_detect(tolower(play_type), "end of")], 1),
            # last.play.type.no.pen = tail(play_type[play_type != "Penalty" & !str_detect(tolower(play_type), "end of")], 1),
            # last.play.text.no.pen = tail(play_text[play_type != "Penalty" & !str_detect(tolower(play_type), "end of")], 1),
            # 
            # first.play.type.with.pen = head(play_type[!str_detect(tolower(play_type), "end of")], 1),
            # first.play.text.with.pen = head(play_text[!str_detect(tolower(play_type), "end of")], 1),
            # last.play.type.with.pen = tail(play_type[!str_detect(tolower(play_type), "end of")], 1),
            # last.play.text.with.pen = tail(play_text[!str_detect(tolower(play_type), "end of")], 1)
            
            first.play.type.no.pen = ifelse(sum(play_type != "Penalty" & !str_detect(tolower(play_type), "end of"), na.rm=T)>0,
                                            head(play_type[play_type != "Penalty" & !str_detect(tolower(play_type), "end of")], 1),
                                            NA),
            first.play.text.no.pen = ifelse(sum(play_type != "Penalty" & !str_detect(tolower(play_type), "end of"), na.rm=T)>0,
                                            head(play_text[play_type != "Penalty" & !str_detect(tolower(play_type), "end of")], 1),
                                            NA),
            last.play.type.no.pen = ifelse(sum(play_type != "Penalty" & !str_detect(tolower(play_type), "end of"), na.rm=T)>0,
                                           tail(play_type[play_type != "Penalty" & !str_detect(tolower(play_type), "end of")], 1),
                                           NA),
            last.play.text.no.pen = ifelse(sum(play_type != "Penalty" & !str_detect(tolower(play_type), "end of"), na.rm=T)>0,
                                           tail(play_text[play_type != "Penalty" & !str_detect(tolower(play_type), "end of")], 1),
                                           NA),
            
            first.play.type.with.pen = ifelse(sum(!str_detect(tolower(play_type), "end of"), na.rm=T)>0,
                                              head(play_type[!str_detect(tolower(play_type), "end of")], 1),
                                              NA),
            first.play.text.with.pen = ifelse(sum(!str_detect(tolower(play_type), "end of"), na.rm=T)>0,
                                              head(play_text[!str_detect(tolower(play_type), "end of")], 1),
                                              NA),
            last.play.type.with.pen = ifelse(sum(!str_detect(tolower(play_type), "end of"), na.rm=T)>0,
                                             tail(play_type[!str_detect(tolower(play_type), "end of")], 1),
                                             NA),
            last.play.text.with.pen = ifelse(sum(!str_detect(tolower(play_type), "end of"), na.rm=T)>0,
                                             tail(play_text[!str_detect(tolower(play_type), "end of")], 1),
                                             NA)
        )



# View(pbp_by_drive_full %>%
#   select(game_id_drive, pos_team, def_pos_team, side, drive_result_detailed, stopped_position, score_pts,
#          first.play.type.no.pen, first.play.type.with.pen, 
#          last.play.type.no.pen, last.play.type.with.pen))

#####
## Changing the "DRIVE_RESULT" for SPECIAL TEAM DRIVES
##  to the LAST NON-PENALTY PLAY,
##  UNLESS:
##      1. It's the "Punt Penalty Turnover"
##      2. "On-Side Kick Lost"
##  which we'll keep as is
######

our.ind <- which(str_detect(pbp_by_drive_full$side, "ST.") & 
                   !pbp_by_drive_full$drive_result_detailed %in% c("Punt Penalty Turnover", "On-Side Kick Lost"))

table(pbp_by_drive_full$last.play.type.no.pen[our.ind])

sort(unique(pbp_by_drive_full$last.play.type.no.pen))
sort(unique(pbp_by_drive_full$drive_result_detailed))


pbp_by_drive_full$drive_result_detailed[our.ind] <- pbp_by_drive_full$last.play.type.no.pen[our.ind]

pbp_by_drive_full$last.play.type.no.pen[pbp_by_drive_full$drive_result_detailed == "Punt Penalty Turnover"]
pbp_by_drive_full$last.play.type.no.pen[pbp_by_drive_full$drive_result_detailed == "Punt Penalty Turnover"]


###  (APRIL 5th)
### !!! CHECKING POINTS SCORED !!!!

# For 2014: no cases where "score_pts != 0" but drive result is not among "scoring_drive_results"
#   A BUNCH of cases the other way: Kickoff returned for TD - many don't get picked up by "scoring_play" indicator, and don't change score on that play either
#   Fixed those by using POS_SCORE_PTS value of the play_type that fits into SCORING PLAY DESCRIPTION
#
#  There's still 1 misrecording: 0pts for a rushing TD...
# For 2015-2020: there's a couple years with 7, one with 5, a bunch of 2-3. And a couple years with "score_pts != 0" but drive result not among scoring ones.

print("Points scored (PRIOR TO IMPLEMENTING FIXES), PAY CLOSE ATTENTION TO OFF-DIAGONALS")
print(table(score_pts = pbp_by_drive_full$score_pts !=0 , drive_res =pbp_by_drive_full$drive_result_detailed %in% scoring_drive_results))

# Cases where score_pts = 0, but drive result is SCORING PLAY
bad.ind <- which(pbp_by_drive_full$score_pts ==0 & pbp_by_drive_full$drive_result_detailed %in% scoring_drive_results)
pbp_by_drive_full$last.play.text.no.pen[bad.ind]


offensive.TD.score.drive.res <- c("Blocked Punt Touchdown", "Kickoff Return Touchdown", "Passing Touchdown", "Punt Return Touchdown", "Rushing Touchdown")
defensive.TD.score.drive.res <- c("Blocked Field Goal Touchdown", "Fumble Return Touchdown", "Interception Return Touchdown", 
                                  "Kickoff Team Fumble Recovery Touchdown", "Punt Team Fumble Recovery Touchdown", "Safety")
offensive.FG.score.drive.res <- c("Field Goal Good")

## For offensive touchdown scores:
our.ind <- bad.ind[pbp_by_drive_full[bad.ind,]$drive_result_detailed %in% offensive.TD.score.drive.res]
pbp_by_drive_full[our.ind, ]$score_pts <- ifelse(str_detect(tolower(pbp_by_drive_full[our.ind, ]$last.play.text.no.pen), fixed("two")) & !str_detect(tolower(pbp_by_drive_full[our.ind, ]$last.play.text.no.pen), "failed"),
                                                 8,
                                                 ifelse(str_detect(tolower(pbp_by_drive_full[our.ind, ]$last.play.text.no.pen), fixed("kick)")),
                                                        7,
                                                        ifelse(str_detect(tolower(pbp_by_drive_full[our.ind, ]$last.play.text.no.pen), "missed|blocked|failed"),
                                                               6,
                                                               7)))

## For defensive touchdown scores:
our.ind <- bad.ind[pbp_by_drive_full[bad.ind,]$drive_result_detailed %in% defensive.TD.score.drive.res]
pbp_by_drive_full[our.ind, ]$score_pts <- ifelse(str_detect(tolower(pbp_by_drive_full[our.ind, ]$last.play.text.no.pen), fixed("two")) & !str_detect(tolower(pbp_by_drive_full[our.ind, ]$last.play.text.no.pen), "failed"),
                                                 -8,
                                                 ifelse(str_detect(tolower(pbp_by_drive_full[our.ind, ]$last.play.text.no.pen), fixed("kick)")),
                                                        -7,
                                                        ifelse(str_detect(tolower(pbp_by_drive_full[our.ind, ]$last.play.text.no.pen), "missed|blocked|failed"),
                                                               -6,
                                                               -7)))

## For defensive SAFETIES:
our.ind <- bad.ind[pbp_by_drive_full[bad.ind,]$drive_result_detailed == "Safety"]
pbp_by_drive_full[our.ind, ]$score_pts <- -2

# For offensive FG made:
our.ind <- bad.ind[pbp_by_drive_full[bad.ind,]$drive_result_detailed %in% offensive.FG.score.drive.res]
pbp_by_drive_full[our.ind, ]$score_pts <- 3

# For blocked punt SAFETY made:
our.ind <- bad.ind[pbp_by_drive_full[bad.ind,]$drive_result_detailed == "Blocked Punt (Safety)"]
pbp_by_drive_full[our.ind, ]$score_pts <- 2



### Cases where score_pts != 0, but drive result is NOT A SCORING PLAY
## ONLY ONE CASE LIKE THAT, a CLEAR MISRECORDING:
##    Should be a KICKOFF after a SAFETY (from 20yd line, as the "yards_to_goal" show)
##    but shows PUNT

if (year == 2017){
  bad.ind <- which(pbp_by_drive_full$score_pts != 0 & !(pbp_by_drive_full$drive_result_detailed %in% scoring_drive_results))
  pbp_by_drive_full$last.play.text.no.pen[bad.ind]
  pbp_by_drive_full$drive_result_detailed[bad.ind]
  pbp_by_drive_full$game_id_drive[bad.ind]
  
  pbp_by_drive_full[bad.ind, c("drive_result_detailed")] <- c("Safety")
  pbp_by_drive_full[bad.ind, "score_pts"] <- -2
  pbp_by_drive_full[bad.ind+1, "first.play.type.no.pen"] <- "Kickoff"
  pbp_by_drive_full[bad.ind+1,"first.play.type.with.pen"] <- "Kickoff"
  pbp_by_drive_full[bad.ind+1, "first.play.text.no.pen"] <- "Brown Adam kickoff for 61 yds , Kylan Nelson returns for 21 yds to the Ohio 40"
  pbp_by_drive_full[bad.ind+1, "first.play.text.with.pen"] <- "Brown Adam kickoff for 61 yds , Kylan Nelson returns for 21 yds to the Ohio 40"
  
  pbp_by_drive_full[bad.ind+1, ]$yds_kickoff <- pbp_by_drive_full[bad.ind+1, ]$yds_punted
  pbp_by_drive_full[bad.ind+1, ]$yds_kickoff_net <- pbp_by_drive_full[bad.ind+1, ]$yds_punt_net
  pbp_by_drive_full[bad.ind+1, ]$yds_kickoff_return <- pbp_by_drive_full[bad.ind+1, ]$yds_punt_return
  
  pbp_by_drive_full[bad.ind+1, ]$yds_punted <- NA
  pbp_by_drive_full[bad.ind+1, ]$yds_punt_net <- NA
  pbp_by_drive_full[bad.ind+1, ]$yds_punt_return <- NA
  
}


print("Points scored (AFTER IMPLEMENTING FIXES), PAY CLOSE ATTENTION TO OFF-DIAGONALS")
print("STILL ISSUES with NON-SCORING PLAYS getting assigned NON-ZERO POINTS (hence we create 'score_pts_by_text')")
print(table(score_pts = pbp_by_drive_full$score_pts !=0 , drive_res =pbp_by_drive_full$drive_result_detailed %in% scoring_drive_results))


# our.ind <- which((pbp_by_drive_full$score_pts != 0) & !(pbp_by_drive_full$drive_result_detailed %in% scoring_drive_results))
# View(pbp_by_drive_full[our.ind,] %>%
#        select(score_pts, drive_result_detailed, last.play.text.no.pen, last.play.text.with.pen)
#        )



#####
## Creating a "score_pts_by_text"
#####


offensive.TD.score.drive.res <- c("Blocked Punt Touchdown", "Kickoff Return Touchdown", "Passing Touchdown", "Punt Return Touchdown", "Rushing Touchdown")
defensive.TD.score.drive.res <- c("Blocked Field Goal Touchdown", "Fumble Return Touchdown", "Interception Return Touchdown", 
                                  "Kickoff Team Fumble Recovery Touchdown", "Punt Team Fumble Recovery Touchdown", "Safety")
offensive.FG.score.drive.res <- c("Field Goal Good")



# Initializing
pbp_by_drive_full$score_pts_by_text <- 0

## For offensive touchdown scores:
our.ind <- which(pbp_by_drive_full$drive_result_detailed %in% offensive.TD.score.drive.res)

pbp_by_drive_full[our.ind, ]$score_pts_by_text <- ifelse(str_detect(tolower(pbp_by_drive_full[our.ind, ]$last.play.text.no.pen), fixed("two")) & !str_detect(tolower(pbp_by_drive_full[our.ind, ]$last.play.text.no.pen), "failed"),
                                                         8,
                                                         ifelse(str_detect(tolower(pbp_by_drive_full[our.ind, ]$last.play.text.no.pen), fixed("kick)")),
                                                                7,
                                                                ifelse(str_detect(tolower(pbp_by_drive_full[our.ind, ]$last.play.text.no.pen), "missed|blocked|failed"),
                                                                       6,
                                                                       7)))

# pbp_by_drive_full[our.ind, ]$score_pts_by_text <- ifelse(str_detect(tolower(pbp_by_drive_full[our.ind, ]$last.play.text.no.pen),  fixed("kick)")),
#                                                     7,
#                                                     ifelse(str_detect(tolower(pbp_by_drive_full[our.ind, ]$last.play.text.no.pen), fixed("two")) & !str_detect(tolower(pbp_by_drive_full[our.ind, ]$last.play.text.no.pen), "failed"),
#                                                            8,
#                                                            6))

## For defensive touchdown scores:
our.ind <- which(pbp_by_drive_full$drive_result_detailed %in% defensive.TD.score.drive.res)

pbp_by_drive_full[our.ind, ]$score_pts_by_text <- ifelse(str_detect(tolower(pbp_by_drive_full[our.ind, ]$last.play.text.no.pen), fixed("two")) & !str_detect(tolower(pbp_by_drive_full[our.ind, ]$last.play.text.no.pen), "failed"),
                                                         -8,
                                                         ifelse(str_detect(tolower(pbp_by_drive_full[our.ind, ]$last.play.text.no.pen), fixed("kick)")),
                                                                -7,
                                                                ifelse(str_detect(tolower(pbp_by_drive_full[our.ind, ]$last.play.text.no.pen), "missed|blocked|failed"),
                                                                       -6,
                                                                       -7)))

# pbp_by_drive_full[our.ind, ]$score_pts_by_text <- ifelse(str_detect(tolower(pbp_by_drive_full[our.ind, ]$last.play.text.no.pen), fixed("kick)")),
#                                                     -7,
#                                                     ifelse(str_detect(tolower(pbp_by_drive_full[our.ind, ]$last.play.text.no.pen), fixed("two")) & !str_detect(tolower(pbp_by_drive_full[our.ind, ]$last.play.text.no.pen), "failed"),
#                                                            -8,
#                                                            -6))

## For defensive SAFETIES:
our.ind <- which(pbp_by_drive_full$drive_result_detailed == "Safety")
pbp_by_drive_full[our.ind, ]$score_pts_by_text <- -2

# For offensive FG made:
our.ind <- which(pbp_by_drive_full$drive_result_detailed %in% offensive.FG.score.drive.res)
pbp_by_drive_full[our.ind, ]$score_pts_by_text <- 3

# For blocked punt SAFETY made:
our.ind <- which(pbp_by_drive_full$drive_result_detailed == "Blocked Punt (Safety)")
if (length(our.ind) > 0){
  pbp_by_drive_full[our.ind, ]$score_pts_by_text <- 2
}



## NA score_pts for ST plays: just 0
pbp_by_drive_full$score_pts_by_text[str_detect(pbp_by_drive_full$side, "ST.")] <- ifelse(is.na(pbp_by_drive_full$score_pts[str_detect(pbp_by_drive_full$side, "ST.")]),
                                                                                         0,
                                                                                         pbp_by_drive_full$score_pts_by_text[str_detect(pbp_by_drive_full$side, "ST.")])


all.drive.res <- sort(unique(pbp_by_drive_full$drive_result_detailed))


print("The 'score_by_text'=0 drive results:")
print(all.drive.res[!all.drive.res %in% c(offensive.TD.score.drive.res,
                                          defensive.TD.score.drive.res,
                                          offensive.FG.score.drive.res,
                                          "Safety",
                                          "Blocked Punt (Safety)")])


####
## For everything else: 0
####

# our.ind <- which(!pbp_by_drive_full$drive_result_detailed %in% c(offensive.TD.score.drive.res,
#                                                                  defensive.TD.score.drive.res,
#                                                                  offensive.FG.score.drive.res,
#                                                                  "Safety",
#                                                                  "Blocked Punt (Safety)"))
# 
# pbp_by_drive_full[our.ind, ]$score_pts_by_text <- 0

print("Points scored for 'score_pts_by_text', PAY CLOSE ATTENTION TO OFF-DIAGONALS")
print(table(score_pts = pbp_by_drive_full$score_pts_by_text !=0 , drive_res =pbp_by_drive_full$drive_result_detailed %in% scoring_drive_results))



# our.ind <- which((pbp_by_drive_full$score_pts_by_text != 0) & !(pbp_by_drive_full$drive_result_detailed %in% scoring_drive_results))
# 
# pbp_by_drive_full[our.ind,]
# 
# View(pbp_by_drive_full[our.ind,] %>%
#        select(score_pts, score_pts_by_text, drive_result_detailed, last.play.text.no.pen, last.play.text.with.pen)
# )
# 
# View(pbp_by_drive %>%
#        filter(game_id_half == "400547740-2"))


# print("Checking the summary of values for drive scored points:")
# print("Original 'score_pts':")
# print(table(pbp_by_drive_full$score_pts))
# 
# print("My 'score_pts_by_text':")
# print(table(pbp_by_drive_full$score_pts_by_text))


# print("Discrepancies between 'score_pts' and 'score_pts_by_text'")
# print(length(which(pbp_by_drive_full$score_pts != pbp_by_drive_full$score_pts_by_text)))


# Discrepancies betweent "score_pts" and "score_pts_by_text"
# View(pbp_by_drive_full %>%
#        #filter(str_detect(tolower(play_text), "end of")) %>%
#        filter(game_id_drive %in% pbp_by_drive_full$game_id_drive[which(pbp_by_drive_full$score_pts != pbp_by_drive_full$score_pts_by_text)]) %>%
#        select(game_id_drive, pos_team, def_pos_team, side, drive_result_detailed, last.play.text.no.pen,score_pts, score_pts_by_text))
# 
# 
# 
# pbp_by_drive_full %>%
#   filter(game_id_drive == "400547905-12") %>%
#   select(drive_result_detailed, score_pts, score_pts_by_text)


save(pbp_by_drive, file=paste0("pbp_cleaned_", year,".Robj"))
save(pbp_by_drive_full, file=paste0("pbp_by_drive_Off_Def_ST_", year,".Robj"))


}



sort(unique(pbp_by_drive_full$drive_result_detailed))


###
## Checking the SCORE_PTS_BY_TEXT vs DRIVE OUTCOMES
###
i <- 26
sort(unique(pbp_by_drive_full$drive_result_detailed))[i]
table(pbp_by_drive_full$score_pts_by_text[pbp_by_drive_full$drive_result_detailed == sort(unique(pbp_by_drive_full$drive_result_detailed))[i]])


# View(pbp_by_drive_full %>%
#   filter(drive_result_detailed == sort(unique(pbp_by_drive_full$drive_result_detailed))[i]) %>%
#   select(score_pts_by_text, drive_result_detailed, last.play.text.no.pen, last.play.text.with.pen))
#          
#