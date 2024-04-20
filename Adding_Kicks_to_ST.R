library(tidyverse)
library(markovchain)



for (year in 2014:2020){

  print(paste0("Year: ", year))

load(file=paste0("pbp_cleaned_", year,".Robj"))
  
pbp_by_drive
sort(unique(pbp_by_drive$play_type))
sort(unique(pbp_by_drive$drive_result_detailed))



# View(pbp_by_drive %>%
#        filter(str_detect(play_type, "Field Goal")) %>%
#       select(game_id, drive_number, game_id_drive, game_id_half, home, pos_team, def_pos_team, play_type, play_text, side))

FG.plays <- unique(pbp_by_drive$play_type)[str_detect(unique(pbp_by_drive$play_type), "Field Goal")]
FG.plays

## Assigning the SIDE for EXPLICIT FG PLAYS
pbp_by_drive$side <- ifelse(pbp_by_drive$play_type %in% FG.plays,
                            ifelse(pbp_by_drive$pos_team == pbp_by_drive$home, "FG.Kick", "FG.Receive"),
                            pbp_by_drive$side)


###
#### Checking FG plays where THE DRIVE # DIDN'T CHANGE ON THE FOLLOWING PLAY
###

ind.type <- which(pbp_by_drive$play_type %in% FG.plays &
  (lead(pbp_by_drive$drive_number) == pbp_by_drive$drive_number) & 
  (lead(pbp_by_drive$game_id_half) == pbp_by_drive$game_id_half))

## For cases where FG is followed by "End Of", OR a "Penalty" in the direct aftermath of the FG kick (although it's applied to the kickoff)
##    Bump up the id for FG and "End of" plays by 0.1
ind.type <- which(pbp_by_drive$play_type %in% FG.plays &
                    (lead(pbp_by_drive$drive_number) == pbp_by_drive$drive_number) & (lead(pbp_by_drive$game_id_drive) == pbp_by_drive$game_id_drive) &
                    (str_detect(tolower(lead(pbp_by_drive$play_type)), "end of") | (lead(pbp_by_drive$play_type) == "Penalty"))  )
ind.type

pbp_by_drive$side[sort(c(ind.type, ind.type+1))] <- ifelse(pbp_by_drive$pos_team[sort(c(ind.type, ind.type+1))] == pbp_by_drive$home[sort(c(ind.type, ind.type+1))],
                                              "FG.Kick",
                                              "FG.Receive")
pbp_by_drive$drive_number[sort(c(ind.type, ind.type+1))] <- pbp_by_drive$drive_number[sort(c(ind.type, ind.type+1))] + 0.1
pbp_by_drive$game_id_drive <- as.character(pbp_by_drive$game_id_drive)

pbp_by_drive$game_id_drive[sort(c(ind.type, ind.type+1))] <- as.character(paste0(pbp_by_drive$game_id[sort(c(ind.type, ind.type+1))],
                                                                     "-", 
                                                                     pbp_by_drive$drive_number[sort(c(ind.type, ind.type+1))]))





## For cases where FG is NOT followed by "End Of":
##    Mostly PENALTIES in the aftermath for the FG kick, hence should be TAGGED TO FG teams

# 4 of them total: 
#   * 3 where the FG gets blocked BUT LIKELY RECOVERED BY THE KICKING TEAM, so the DRIVE # DOESN'T CHANGE
#   * 1 where it's a DOUBLE-ENTRY: FG play followed by a FG play which is the EXACT SAME

# 1. Removing the double-entry, only retaining the SECOND one
ind.type <- which(pbp_by_drive$play_type %in% FG.plays & lead(pbp_by_drive$play_type) %in% FG.plays)
ind.type

if (length(ind.type)>0) pbp_by_drive <- pbp_by_drive[-ind.type, ]


## 2. For the BLOCKED FG THAT GOT RECOVERED BY THE KICKING TEAM:
##    Bump up the Blocked FG id by 0.1, and bump up the remaining drive id's by 0.2

ind.type <- which(str_detect(pbp_by_drive$play_type, "Blocked Field Goal") &
                    (lead(pbp_by_drive$drive_number) == pbp_by_drive$drive_number) & (lead(pbp_by_drive$game_id_drive) == pbp_by_drive$game_id_drive) &
                    !(str_detect(tolower(lead(pbp_by_drive$play_type)), "end of") | (lead(pbp_by_drive$play_type) == "Penalty")))
ind.type

pbp_by_drive$game_id_drive <- as.character(pbp_by_drive$game_id_drive)


pbp_by_drive$side[ind.type] <- ifelse(pbp_by_drive$pos_team[ind.type] == pbp_by_drive$home[ind.type],
                                      "FG.Kick",
                                      "FG.Receive")
pbp_by_drive[ind.type, ]$drive_number <- pbp_by_drive[ind.type, ]$drive_number + 0.1
pbp_by_drive$game_id_drive[ind.type] <- paste0(pbp_by_drive$game_id[ind.type], 
                                               "-", 
                                               pbp_by_drive$drive_number[ind.type])


post.punt.roughing.drive.game_id_drive <-  pbp_by_drive[ind.type+1,]$game_id_drive

for (j in 1:length(ind.type)){
  ind.minitype <- which(pbp_by_drive$game_id_drive == post.punt.roughing.drive.game_id_drive[j])
  ind.minitype <- ind.minitype[ind.minitype > ind.type[j]]  # Getting only the plays AFTER the FG block that returned to the offense
  pbp_by_drive$drive_number[ind.minitype] <- pbp_by_drive$drive_number[ind.minitype] + 0.2
  pbp_by_drive$game_id_drive[ind.minitype] <- paste0(pbp_by_drive$game_id[ind.minitype], "-", pbp_by_drive$drive_number[ind.minitype])
}


print("# of bad post-FG.plays remaining")
ind.type <- which((pbp_by_drive$play_type %in% FG.plays) &
                    (lead(pbp_by_drive$drive_number) == pbp_by_drive$drive_number) & (lead(pbp_by_drive$game_id_drive) == pbp_by_drive$game_id_drive) &
                    !(str_detect(tolower(lead(pbp_by_drive$play_type)), "end of") | (lead(pbp_by_drive$play_type) == "Penalty")))
print(length(ind.type))






###
#### Checking the non-FG type plays, there were still FGs (judging by the text), where THE DRIVE # DIDN'T CHANGE ON THE FOLLOWING PLAY
###

ind.type <- which(!(pbp_by_drive$play_type %in% FG.plays) & (str_detect(tolower(pbp_by_drive$play_text), "\\bfield goal\\b|\\bfg\\b")) &
                    (lead(pbp_by_drive$drive_number) == pbp_by_drive$drive_number) & (lead(pbp_by_drive$game_id_drive) == pbp_by_drive$game_id_drive))
ind.type


## 1. If it's a penalty, leading to a RE-KICK
##     Bump up by 0.1 for both

ind.type <- which((pbp_by_drive$play_type == "Penalty") & (str_detect(tolower(pbp_by_drive$play_text), "\\bfield goal\\b|\\bfg\\b")) &
                    (lead(pbp_by_drive$play_type) %in% FG.plays) &
                    (lead(pbp_by_drive$drive_number) == pbp_by_drive$drive_number) & (lead(pbp_by_drive$game_id_drive) == pbp_by_drive$game_id_drive))
ind.type

pbp_by_drive$side[sort(c(ind.type, ind.type+1))] <- ifelse(pbp_by_drive$pos_team[sort(c(ind.type, ind.type+1))] == pbp_by_drive$home[sort(c(ind.type, ind.type+1))],
                                                           "FG.Kick",
                                                           "FG.Receive")
pbp_by_drive$drive_number[sort(c(ind.type, ind.type+1))] <- pbp_by_drive$drive_number[sort(c(ind.type, ind.type+1))] + 0.1
pbp_by_drive$game_id_drive <- as.character(pbp_by_drive$game_id_drive)

pbp_by_drive$game_id_drive[sort(c(ind.type, ind.type+1))] <- as.character(paste0(pbp_by_drive$game_id[sort(c(ind.type, ind.type+1))],
                                                                                 "-", 
                                                                                 pbp_by_drive$drive_number[sort(c(ind.type, ind.type+1))]))


## 2. If it's a penalty leading to CONTINUED POSSESSION (not a re-kick)
##     Bump up by 0.1 for the kick, and +0.2 to the remaining plays of the drive

ind.type <- which((pbp_by_drive$play_type == "Penalty") & (str_detect(tolower(pbp_by_drive$play_text), "\\bfield goal\\b|\\bfg\\b")) &
                    !(lead(pbp_by_drive$play_type) %in% FG.plays) &
                    (lead(pbp_by_drive$drive_number) == pbp_by_drive$drive_number) & (lead(pbp_by_drive$game_id_drive) == pbp_by_drive$game_id_drive))
ind.type


pbp_by_drive$game_id_drive <- as.character(pbp_by_drive$game_id_drive)


pbp_by_drive$side[ind.type] <- ifelse(pbp_by_drive$pos_team[ind.type] == pbp_by_drive$home[ind.type],
                                      "FG.Kick",
                                      "FG.Receive")
pbp_by_drive[ind.type, ]$drive_number <- pbp_by_drive[ind.type, ]$drive_number + 0.1
pbp_by_drive$game_id_drive[ind.type] <- paste0(pbp_by_drive$game_id[ind.type], 
                                               "-", 
                                               pbp_by_drive$drive_number[ind.type])


post.punt.roughing.drive.game_id_drive <-  pbp_by_drive[ind.type+1,]$game_id_drive

for (j in 1:length(ind.type)){
  ind.minitype <- which(pbp_by_drive$game_id_drive == post.punt.roughing.drive.game_id_drive[j])
  ind.minitype <- ind.minitype[ind.minitype > ind.type[j]]  # Getting only the plays AFTER the FG block that returned to the offense
  pbp_by_drive$drive_number[ind.minitype] <- pbp_by_drive$drive_number[ind.minitype] + 0.2
  pbp_by_drive$game_id_drive[ind.minitype] <- paste0(pbp_by_drive$game_id[ind.minitype], "-", pbp_by_drive$drive_number[ind.minitype])
}



## 3. If it's NOT a penalty (while also not among FG.plays)
ind.type <- which(!(pbp_by_drive$play_type %in% FG.plays) & (pbp_by_drive$play_type != "Penalty") & (str_detect(tolower(pbp_by_drive$play_text), "\\bfield goal\\b|\\bfg\\b")) &
                    # (lead(pbp_by_drive$play_type) %in% FG.plays) &
                    (lead(pbp_by_drive$drive_number) == pbp_by_drive$drive_number) & (lead(pbp_by_drive$game_id_drive) == pbp_by_drive$game_id_drive))
ind.type

print("# of non-penalty & non-FG.play types that had 'FG' in play text, and didn't change the drive #")
print(length(ind.type))

#####
#####
##  FG plays that HAD THE SAME DRIVE NUMBER on the PREVIOUS PLAY
#####
#####

#### A. Plays that WEREN'T OF EXPLICIT "FG TYPE"

##    1. Fake field goals
##        Bump up by 0.1 (there's none that were converted in our data)

ind.type <- which((str_detect(tolower(pbp_by_drive$play_text), "\\bfake field goal\\b|\\bfake fg\\b")) & 
                    (pbp_by_drive$drive_number == lag(pbp_by_drive$drive_number)) & (pbp_by_drive$game_id_drive == lag(pbp_by_drive$game_id_drive)))

print("Fake field goals:")
print(length(ind.type))

pbp_by_drive$game_id_drive <- as.character(pbp_by_drive$game_id_drive)


pbp_by_drive$side[ind.type] <- ifelse(pbp_by_drive$pos_team[ind.type] == pbp_by_drive$home[ind.type],
                                      "FG.Kick",
                                      "FG.Receive")
pbp_by_drive[ind.type, ]$drive_number <- pbp_by_drive[ind.type, ]$drive_number + 0.1
pbp_by_drive$game_id_drive[ind.type] <- paste0(pbp_by_drive$game_id[ind.type], 
                                               "-", 
                                               pbp_by_drive$drive_number[ind.type])



##    2. Penalties on FG, followed by a re-kick
##         Assign the drive # to the next observation's drive #

ind.type <- which(#(pbp_by_drive$play_type %in% FG.plays) &
  (pbp_by_drive$play_type == "Penalty") & str_detect(tolower(pbp_by_drive$play_text), "\\bfield goal\\b|\\bfg\\b") &
    (lead(pbp_by_drive$play_type) %in% FG.plays) &
    (pbp_by_drive$drive_number == lag(pbp_by_drive$drive_number)) & (pbp_by_drive$game_id_drive == lag(pbp_by_drive$game_id_drive)))
ind.type

print("Penalties on FG, followed by a re-kick:")
print(length(ind.type))

pbp_by_drive$game_id_drive <- as.character(pbp_by_drive$game_id_drive)

pbp_by_drive$side[ind.type] <- ifelse(pbp_by_drive$pos_team[ind.type] == pbp_by_drive$home[ind.type],
                                      "FG.Kick",
                                      "FG.Receive")
pbp_by_drive[ind.type, ]$drive_number <- pbp_by_drive[ind.type+1, ]$drive_number
pbp_by_drive$game_id_drive[ind.type] <- paste0(pbp_by_drive$game_id[ind.type], 
                                               "-", 
                                               pbp_by_drive$drive_number[ind.type])


##    3. Penalties on FG, followed by NOT a re-kick (e.g. a punt)
##         Bump up by 0.1

ind.type <- which(#(pbp_by_drive$play_type %in% FG.plays) &
  (pbp_by_drive$play_type == "Penalty") & str_detect(tolower(pbp_by_drive$play_text), "\\bfield goal\\b|\\bfg\\b") &
    !((str_detect(tolower(pbp_by_drive$play_text), "\\bfake field goal\\b|\\bfake fg\\b"))) &
    !(lead(pbp_by_drive$play_type) %in% FG.plays) &
                    (pbp_by_drive$drive_number == lag(pbp_by_drive$drive_number)) & (pbp_by_drive$game_id_drive == lag(pbp_by_drive$game_id_drive)))
ind.type

print("Penalties on FG, followed by NOT a re-kick (e.g. a punt):")
print(length(ind.type))


pbp_by_drive$game_id_drive <- as.character(pbp_by_drive$game_id_drive)

pbp_by_drive$side[ind.type] <- ifelse(pbp_by_drive$pos_team[ind.type] == pbp_by_drive$home[ind.type],
                                      "FG.Kick",
                                      "FG.Receive")
pbp_by_drive[ind.type, ]$drive_number <- pbp_by_drive[ind.type, ]$drive_number + 0.1
pbp_by_drive$game_id_drive[ind.type] <- paste0(pbp_by_drive$game_id[ind.type], 
                                               "-", 
                                               pbp_by_drive$drive_number[ind.type])



##   4. Non-penalty, non-FG plays

ind.type <- which(!(pbp_by_drive$play_type %in% FG.plays) & (pbp_by_drive$play_type != "Penalty") & str_detect(tolower(pbp_by_drive$play_text), "\\bfield goal\\b|\\bfg\\b") &
   # !((str_detect(tolower(pbp_by_drive$play_text), "\\bfake field goal\\b|\\bfake fg\\b"))) &
  #  !(lead(pbp_by_drive$play_type) %in% FG.plays) &
    (pbp_by_drive$drive_number == lag(pbp_by_drive$drive_number)) & (pbp_by_drive$game_id_drive == lag(pbp_by_drive$game_id_drive)))
ind.type

print("Non-penalty, non-FG plays with 'fg' in play_text and with non-changing drive # remaining:")
print(length(ind.type))


#### B. Plays that WERE ON SIDE "FG.Kick" & "FG.Receive", preceded by NON-FG.Kick/Receive play

## 1. Those of play_type NOT in FG.types
##    (SHOULD BE NONE OF THEM, AFTER ALL THE PREVIOUS CORRECTIONS... but DEFINITELY CHECK IF SOME YEARS STILL HAVE THOSE)

ind.type <- which(pbp_by_drive$side %in% c("FG.Kick", "FG.Receive") & !(lag(pbp_by_drive$side) %in% c("FG.Kick", "FG.Receive")) &
                    !(pbp_by_drive$play_type %in% FG.plays))
ind.type

print("# FG.Kick/Receive plays that are NOT explicit 'FG.type', and have the same drive # as the previous non-FG play:")
ind.type <- which(pbp_by_drive$side %in% c("FG.Kick", "FG.Receive") & !(lag(pbp_by_drive$side) %in% c("FG.Kick", "FG.Receive")) &
                    !(pbp_by_drive$play_type %in% FG.plays) &
                    (pbp_by_drive$drive_number == lag(pbp_by_drive$drive_number)) & (pbp_by_drive$game_id_drive == lag(pbp_by_drive$game_id_drive)))
ind.type
print(length(ind.type))


## 2. Those of play_type IN FG.types
##      Bump up by 0.1

ind.type <- which(pbp_by_drive$side %in% c("FG.Kick", "FG.Receive") & !(lag(pbp_by_drive$side) %in% c("FG.Kick", "FG.Receive")) &
                    (pbp_by_drive$play_type %in% FG.plays) &
                    (pbp_by_drive$drive_number == lag(pbp_by_drive$drive_number)) & (pbp_by_drive$game_id_drive == lag(pbp_by_drive$game_id_drive)))
ind.type


pbp_by_drive$game_id_drive <- as.character(pbp_by_drive$game_id_drive)

pbp_by_drive[ind.type, ]$drive_number <- pbp_by_drive[ind.type, ]$drive_number + 0.1
pbp_by_drive$game_id_drive[ind.type] <- paste0(pbp_by_drive$game_id[ind.type], 
                                               "-", 
                                               pbp_by_drive$drive_number[ind.type])



print("Amount of non-drive-changing FG plays remaining:")
ind.type <- which(pbp_by_drive$side %in% c("FG.Kick", "FG.Receive") & !(lag(pbp_by_drive$side) %in% c("FG.Kick", "FG.Receive")) &
                    (pbp_by_drive$drive_number == lag(pbp_by_drive$drive_number)) & (pbp_by_drive$game_id_drive == lag(pbp_by_drive$game_id_drive)))
ind.type
print(length(ind.type))











#####
## CHECKING BAD "ST" PLAYS (the ST designation doesn't align with play type)
#####

ST.plays <- unique(pbp_by_drive$play_type)[str_detect(tolower(unique(pbp_by_drive$play_type)), "punt|kickoff")]

ind.type <- which(!(pbp_by_drive$play_type %in% c(ST.plays, "Penalty")) &
                    pbp_by_drive$side %in% c("ST.Return", "ST.Cover"))
ind.type

## Seems like there's a handful of PASSING/RUSHING TD bad entries
cat("\n")
cat("\n")
print("# of bad passing/rushing TD recordings, Punt/Kickoff return plays (getting fixed):")
ind.type <- which(!(pbp_by_drive$play_type %in% c(ST.plays, "Penalty")) &
                    pbp_by_drive$side %in% c("ST.Return", "ST.Cover") & 
                    pbp_by_drive$play_type %in% c("Rushing Touchdown", "Passing Touchdown"))
ind.type
print(length(ind.type))
cat("\n")
cat("\n")

if (length(ind.type) > 0){
pbp_by_drive[ind.type, ]$side <- ifelse(pbp_by_drive[ind.type, ]$pos_team == pbp_by_drive[ind.type, ]$home,
                                        "Offense",
                                        "Defense")
}

## How about ST plays FOLLOWED BY "End of" plays?
##    !!! Nah, SHOULD JUST REMOVE ALL THE "END OF" PLAYS, right before the "DRIVE-BY-DRIVE" breakdown !!!



## Seems like there's a handful of PASSING/RUSHING TD bad entries
cat("\n")
cat("\n")
print("# of bad passing/rushing TD recordings, FG plays (won't get fixed):")
ind.type <- which(!(pbp_by_drive$play_type %in% c(FG.plays, "Penalty") | 
                      str_detect(tolower(pbp_by_drive$play_type), "end of") | 
                      str_detect(tolower(pbp_by_drive$play_text), "fake field goal|fake fg")) &
                    pbp_by_drive$side %in% c("FG.Kick", "FG.Receive"))
                    # pbp_by_drive$play_type %in% c("Rushing Touchdown", "Passing Touchdown"))
ind.type
print(length(ind.type))
cat("\n")
cat("\n")










# Checking all replaced scenarios:
View(pbp_by_drive[sort(c(ind.type, ind.type+1, ind.type-1
                         ,ind.type+2
)), ]  %>%
  select(game_id, drive_number, game_id_drive, game_id_half, home, pos_team, def_pos_team, play_type, play_text, side))







#######
#######
## DRIVE-BY-DRIVE SUMMARY
#######
#######


all.drive.results <- sort(unique(pbp_by_drive$drive_result_detailed))
scoring_drive_results <- c("Field Goal Good", 
                           "Safety", 
                           "Blocked Punt (Safety)",
                           all.drive.results[str_detect(tolower(all.drive.results), "touchdown")])



### ON-SIDE KICKS

our.ind <- which(pbp_by_drive$drive_result_detailed == "On-Side Kick Lost")
our.ind 
# View(pbp_by_drive[sort(c(our.ind, our.ind-1, our.ind+1)), ] %>%
#   #pbp_by_drive %>%
#       # filter(game_id == "400548175") %>%
#        select(game_id, drive_number, game_id_drive, game_id_half, home, pos_team, def_pos_team, play_type, play_text, side))


## FACTORIZING the "GAME_ID_DRIVE"
pbp_by_drive$game_id_drive <- factor(pbp_by_drive$game_id_drive, levels = unique(pbp_by_drive$game_id_drive))
  




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






pbp_by_drive_full <- pbp_by_drive %>%
  # filter(!str_detect(tolower(play_text), "end of")) %>%
  group_by(game_id_half,
           drive_number,
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
                                                    ifelse(str_detect(tolower(tail(drive_result_detailed,1)), "touchdown"), # | (tolower(tail(drive_result_detailed,1)) == "field goal good"),
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



our.ind <- which(pbp_by_drive_full$drive_result_detailed == "On-Side Kick Lost")
our.ind 

View(pbp_by_drive_full[sort(c(our.ind, our.ind-1, our.ind+1)), ] %>%
       #pbp_by_drive_full %>%
         select(game_id_drive, pos_team, def_pos_team, side, drive_result_detailed, stopped_position, score_pts,
                first.play.type.no.pen, first.play.type.with.pen,
                last.play.type.no.pen, last.play.type.with.pen))



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
table(pbp_by_drive_full$drive_result_detailed[our.ind])

sort(unique(pbp_by_drive_full$last.play.type.no.pen))
sort(unique(pbp_by_drive_full$drive_result_detailed))


pbp_by_drive_full$drive_result_detailed[our.ind] <- pbp_by_drive_full$last.play.type.no.pen[our.ind]

pbp_by_drive_full$last.play.type.no.pen[pbp_by_drive_full$drive_result_detailed == "Punt Penalty Turnover"]
pbp_by_drive_full$last.play.type.no.pen[pbp_by_drive_full$drive_result_detailed == "Punt Penalty Turnover"]



#####
## Changing the "DRIVE_RESULT" for FIELD GOAL drives to "FG Attempt"
## and their "score_pts" to 0
######

sort(unique(pbp_by_drive_full$drive_result_detailed))

# Any drive preceding a FG attempt
our.ind <- which(!str_detect(pbp_by_drive_full$side, "FG.") & 
                   str_detect(pbp_by_drive_full$drive_result_detailed, "Field Goal")
                  # !str_detect(pbp_by_drive_full$drive_result_detailed, "Touchdown|Safety")
                 )

pbp_by_drive_full$drive_result_detailed[our.ind] <- "Field Goal Attempt"
pbp_by_drive_full$score_pts[our.ind] <- 0

View(pbp_by_drive_full[sort(c(our.ind-1,our.ind, our.ind+1)), ] %>%
       select(game_id_drive, pos_team, def_pos_team, side, drive_result_detailed, stopped_position, score_pts,
              last.play.type.no.pen, last.play.type.with.pen,
              last.play.text.no.pen, last.play.text.with.pen, n.plays.with.penalties))







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



print("Points scored for 'score_pts_by_text', PAY CLOSE ATTENTION TO OFF-DIAGONALS")
print(table(score_pts = pbp_by_drive_full$score_pts_by_text !=0 , drive_res =pbp_by_drive_full$drive_result_detailed %in% scoring_drive_results))




###
## Change the NA values for "score_pts" for 0's (those result from breaking away the ST/Kick plays)
###

# pbp_by_drive_full$score_pts <- ifelse(is.na(pbp_by_drive_full$score_pts),
#                                       0,
#                                       pbp_by_drive_full$score_pts)
# 
# View(pbp_by_drive_full %>%
#        select(game_id_drive, pos_team, def_pos_team, side, drive_result_detailed, stopped_position, score_pts, last.play.text.no.pen, last.play.text.with.pen))

####
## Make "stopped_position" NA for all the plays where HALF SWITCHES OVER
####

pbp_by_drive_full$stopped_position[which(pbp_by_drive_full$game_id_half != lead(pbp_by_drive_full$game_id_half))] <- NA


####
## MADE FG that DIDN'T RESULT IN 65 or 35 stopped pos
##    * Some are from the ROUGHING THE KICKER, hence possession getting back to the kicking team
##    * Sometimes it's just... 70 (2016)? or 50 (2019)?
####
cat("\n")
print("Number of MADE FG that have a bad stopped position afterwards (non-35/65):")
bad.ind <- which((pbp_by_drive_full$side %in% c("FG.Kick", "FG.Receive")) & (pbp_by_drive_full$drive_result_detailed %in% c("Field Goal Good")) &
  !(pbp_by_drive_full$stopped_position %in% c(35,50,65,70)) & 
    (pbp_by_drive_full$game_id_half == lead(pbp_by_drive_full$game_id_half)) & 
    !((pbp_by_drive_full$n.plays.with.penalties == 2) & (pbp_by_drive_full$n.plays.no.penalties == 1)) & 
    (pbp_by_drive_full$pos_team != lead(pbp_by_drive_full$pos_team)))
print(length(bad.ind))
cat("\n")

cat("\n")
print("Number of MISSED FG that have a bad stopped position afterwards (really short fields):")
bad.ind <- which((pbp_by_drive_full$side %in% c("FG.Kick", "FG.Receive")) & (pbp_by_drive_full$drive_result_detailed %in% c("Field Goal Missed"))
                  & (pbp_by_drive_full$stopped_position <50) 
                   # (pbp_by_drive_full$game_id_half == lead(pbp_by_drive_full$game_id_half)) & 
                   # !((pbp_by_drive_full$n.plays.with.penalties == 2) & (pbp_by_drive_full$n.plays.no.penalties == 1)) & 
                   # (pbp_by_drive_full$pos_team != lead(pbp_by_drive_full$pos_team))
                 )
print(length(bad.ind))
cat("\n")




# View(pbp_by_drive_full[sort(c(bad.ind, bad.ind+1)), ] %>%
#        select(game_id_drive, pos_team, def_pos_team, side, drive_result_detailed, stopped_position, score_pts,
#               last.play.type.no.pen, last.play.type.with.pen,
#               last.play.text.no.pen, last.play.text.with.pen, n.plays.with.penalties))



save(pbp_by_drive, file=paste0("pbp_cleaned_with_FG_", year,".Robj"))
save(pbp_by_drive_full, file=paste0("pbp_by_drive_Off_Def_ST_with_FG_", year,".Robj"))

}


sort(unique(pbp_by_drive_full$drive_result_detailed))


###
## Checking the SCORE_PTS_BY_TEXT vs DRIVE OUTCOMES
###
i <- 27
sort(unique(pbp_by_drive_full$drive_result_detailed))[i]
table(pbp_by_drive_full$score_pts_by_text[pbp_by_drive_full$drive_result_detailed == sort(unique(pbp_by_drive_full$drive_result_detailed))[i]])


# View(pbp_by_drive_full %>%
#   filter(drive_result_detailed == sort(unique(pbp_by_drive_full$drive_result_detailed))[i]) %>%
#   select(score_pts_by_text, drive_result_detailed, last.play.text.no.pen, last.play.text.with.pen))
#          
#


