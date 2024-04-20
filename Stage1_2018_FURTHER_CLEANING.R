######
## HAVE TO FIX THE ROUGHING THE KICKER DRIVE CONTINUATIONS!!!
##  drive_result_detailed = "Punt Penalty Turnover"
##  USED TO BE SIMPLY EXCLUDED for the "BY_DRIVE" OBJECT due to having NO NON-PENALTY PLAYS
######

library(tidyverse)
library(markovchain)



for (year in 2014:2020){
  
  print(paste0("Year: ", year))
  
  load(file=paste0("R_Object_Files/pbp_cleaned_", year,".Robj"))
  
  
  pbp <- pbp_by_drive
  sort(unique(pbp$play_type))
  sort(unique(pbp$drive_result_detailed))
  
  
  
  
  all.drive.results <- sort(unique(pbp$drive_result_detailed))
  scoring_drive_results <- c("Field Goal Good", 
                             "Safety", 
                             "Blocked Punt (Safety)",
                             all.drive.results[str_detect(tolower(all.drive.results), "touchdown")])
  
  
  
  pbp_by_drive <- pbp %>% select(game_id, drive_number, game_id_drive, game_id_half, week, home,  # drive_id,
                                 play_type,  play_text, period,
                                 
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
                                 
  ) %>% 
    group_by(game_id_half, 
             game_id_drive, week,
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
              
              stopped_position = ifelse(tolower(tail(drive_result_detailed,1)) == "punt",
                                        100 - tail(yards_to_goal_end,1),
                                        ifelse(str_detect(tolower(tail(drive_result_detailed,1)), "safety"),
                                               20,
                                               ifelse(str_detect(tolower(tail(drive_result_detailed,1)), "touchdown") | (tolower(tail(drive_result_detailed,1)) == "field goal good"),
                                                      35,
                                                      tail(yards_to_goal_end,1)))),
              
              
              
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
              
              ####
              ## OLD STUFF (BELOW)
              ####
              
              # yds_kickoff_net = ifelse(is.na(yds_kickoff_return[1] - yds_kickoff[1]), NA, yds_kickoff_return[1] - yds_kickoff[1]),
              # yds_kickoff_25yd_improvement = ifelse(is.na(yds_kickoff_return[1]), NA, 75 - yards_to_goal_end[1]),
              # kickoff_touchback =  ifelse(is.na(yds_kickoff_return[1]), NA, max(kickoff_tb)),
              # yds_kickoff_return = ifelse(is.na(yds_kickoff_return[1]), NA, yds_kickoff_return[1]),
              # yds_kickoff = yds_kickoff[1],
              
              ####
              ## OLD STUFF (ABOVE)
              ####
              
              
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
  
  
  
  
  cat("\n")
  cat("\n")
  print("BY-DRIVE DATA SET SIZE, INITIAL")
  print(dim(pbp_by_drive))
  cat("\n")
  cat("\n")
  
  
  View(pbp_by_drive %>%
         select(game_id_drive, home, pos_team, def_pos_team, drive_result_detailed, stopped_position))
  
 View( pbp %>%
    select(game_id_drive, home, pos_team, def_pos_team, drive_result_detailed))
  
 
 our.ind <- which(pbp_by_drive$drive_result_detailed == "Punt Penalty Turnover")
 our.ind
 View(pbp_by_drive[sort(c(our.ind-1,our.ind,our.ind+1)),] %>%
        select(game_id_drive, home, pos_team, def_pos_team, drive_result_detailed, stopped_position, 
               first.play.text.with.pen, first.play.text.no.pen,
               last.play.text.with.pen, last.play.text.no.pen))
  
  
  
  
  View(pbp_by_drive)
  
  pbp_by_drive$drive_pos_pts_scored <- 0
  pbp_by_drive$drive_pos_pts_scored
  
  pbp_by_drive$drive_result_detailed
  
  
  ###  (APRIL 5th)
  ### !!! CHECKING POINTS SCORED !!!!
  
  # For 2014: no cases where "score_pts != 0" but drive result is not among "scoring_drive_results"
  #   A BUNCH of cases the other way: Kickoff returned for TD - many don't get picked up by "scoring_play" indicator, and don't change score on that play either
  #   Fixed those by using POS_SCORE_PTS value of the play_type that fits into SCORING PLAY DESCRIPTION
  #
  #  There's still 1 misrecording: 0pts for a rushing TD...
  # For 2015-2020: there's a couple years with 7, one with 5, a bunch of 2-3. And a couple years with "score_pts != 0" but drive result not among scoring ones.
  
  print("Points scored (PRIOR TO IMPLEMENTING FIXES), PAY CLOSE ATTENTION TO OFF-DIAGONALS")
  print(table(score_pts = pbp_by_drive$score_pts !=0 , drive_res =pbp_by_drive$drive_result_detailed %in% scoring_drive_results))
  
  # Cases where score_pts = 0, but drive result is SCORING PLAY
  bad.ind <- which(pbp_by_drive$score_pts ==0 & pbp_by_drive$drive_result_detailed %in% scoring_drive_results)
  pbp_by_drive$last.play.text.no.pen[bad.ind]
  
  
  offensive.TD.score.drive.res <- c("Blocked Punt Touchdown", "Kickoff Return Touchdown", "Passing Touchdown", "Punt Return Touchdown", "Rushing Touchdown")
  defensive.TD.score.drive.res <- c("Blocked Field Goal Touchdown", "Fumble Return Touchdown", "Interception Return Touchdown", 
                                    "Kickoff Team Fumble Recovery Touchdown", "Punt Team Fumble Recovery Touchdown", "Safety")
  offensive.FG.score.drive.res <- c("Field Goal Good")
  
  ## For offensive touchdown scores:
  our.ind <- bad.ind[pbp_by_drive[bad.ind,]$drive_result_detailed %in% offensive.TD.score.drive.res]
  pbp_by_drive[our.ind, ]$score_pts <- ifelse(str_detect(tolower(pbp_by_drive[our.ind, ]$last.play.text.no.pen), fixed("two")) & !str_detect(tolower(pbp_by_drive[our.ind, ]$last.play.text.no.pen), "failed"),
                                              8,
                                              ifelse(str_detect(tolower(pbp_by_drive[our.ind, ]$last.play.text.no.pen), "missed|blocked|failed"),
                                                     6,
                                                     7))
  
  ## For defensive touchdown scores:
  our.ind <- bad.ind[pbp_by_drive[bad.ind,]$drive_result_detailed %in% defensive.TD.score.drive.res]
  pbp_by_drive[our.ind, ]$score_pts <- ifelse(str_detect(tolower(pbp_by_drive[our.ind, ]$last.play.text.no.pen), fixed("two")) & !str_detect(tolower(pbp_by_drive[our.ind, ]$last.play.text.no.pen), "failed"),
                                              -8,
                                              ifelse(str_detect(tolower(pbp_by_drive[our.ind, ]$last.play.text.no.pen), "missed|blocked|failed"),
                                                     -6,
                                                     -7))
  
  ## For defensive SAFETIES:
  our.ind <- bad.ind[pbp_by_drive[bad.ind,]$drive_result_detailed == "Safety"]
  pbp_by_drive[our.ind, ]$score_pts <- -2
  
  # For offensive FG made:
  our.ind <- bad.ind[pbp_by_drive[bad.ind,]$drive_result_detailed %in% offensive.FG.score.drive.res]
  pbp_by_drive[our.ind, ]$score_pts <- 3
  
  # For blocked punt SAFETY made:
  our.ind <- bad.ind[pbp_by_drive[bad.ind,]$drive_result_detailed == "Blocked Punt (Safety)"]
  pbp_by_drive[our.ind, ]$score_pts <- 2
  
  
  
  ### Cases where score_pts != 0, but drive result is NOT A SCORING PLAY
  ## ONLY ONE CASE LIKE THAT, a CLEAR MISRECORDING:
  ##    Should be a KICKOFF after a SAFETY (from 20yd line, as the "yards_to_goal" show)
  ##    but shows PUNT
  
  if (year == 2017){
    bad.ind <- which(pbp_by_drive$score_pts != 0 & !(pbp_by_drive$drive_result_detailed %in% scoring_drive_results))
    pbp_by_drive$last.play.text.no.pen[bad.ind]
    pbp_by_drive$drive_result_detailed[bad.ind]
    pbp_by_drive$game_id_drive[bad.ind]
    
    pbp_by_drive[bad.ind, c("drive_result_detailed")] <- c("Safety")
    pbp_by_drive[bad.ind, "score_pts"] <- -2
    pbp_by_drive[bad.ind+1, "first.play.type.no.pen"] <- "Kickoff"
    pbp_by_drive[bad.ind+1,"first.play.type.with.pen"] <- "Kickoff"
    pbp_by_drive[bad.ind+1, "first.play.text.no.pen"] <- "Brown Adam kickoff for 61 yds , Kylan Nelson returns for 21 yds to the Ohio 40"
    pbp_by_drive[bad.ind+1, "first.play.text.with.pen"] <- "Brown Adam kickoff for 61 yds , Kylan Nelson returns for 21 yds to the Ohio 40"
    
    pbp_by_drive[bad.ind+1, ]$yds_kickoff <- pbp_by_drive[bad.ind+1, ]$yds_punted
    pbp_by_drive[bad.ind+1, ]$yds_kickoff_net <- pbp_by_drive[bad.ind+1, ]$yds_punt_net
    pbp_by_drive[bad.ind+1, ]$yds_kickoff_return <- pbp_by_drive[bad.ind+1, ]$yds_punt_return
    
    pbp_by_drive[bad.ind+1, ]$yds_punted <- NA
    pbp_by_drive[bad.ind+1, ]$yds_punt_net <- NA
    pbp_by_drive[bad.ind+1, ]$yds_punt_return <- NA
    
  }
  
  
  print("Points scored (AFTER IMPLEMENTING FIXES), PAY CLOSE ATTENTION TO OFF-DIAGONALS")
  print(table(score_pts = pbp_by_drive$score_pts !=0 , drive_res =pbp_by_drive$drive_result_detailed %in% scoring_drive_results))
  
  
  
  #####
  ## Creating a "score_pts_by_text"
  #####
  
  # Initializing
  pbp_by_drive$score_pts_by_text <- 0
  
  ## For offensive touchdown scores:
  our.ind <- which(pbp_by_drive$drive_result_detailed %in% offensive.TD.score.drive.res)
  pbp_by_drive[our.ind, ]$score_pts_by_text <- ifelse(str_detect(tolower(pbp_by_drive[our.ind, ]$last.play.text.no.pen), fixed("two")) & !str_detect(tolower(pbp_by_drive[our.ind, ]$last.play.text.no.pen), "failed"),
                                                      8,
                                                      ifelse(str_detect(tolower(pbp_by_drive[our.ind, ]$last.play.text.no.pen), "missed|blocked|failed"),
                                                             6,
                                                             7))
  
  ## For defensive touchdown scores:
  our.ind <- which(pbp_by_drive$drive_result_detailed %in% defensive.TD.score.drive.res)
  pbp_by_drive[our.ind, ]$score_pts_by_text <- ifelse(str_detect(tolower(pbp_by_drive[our.ind, ]$last.play.text.no.pen), fixed("two")) & !str_detect(tolower(pbp_by_drive[our.ind, ]$last.play.text.no.pen), "failed"),
                                                      -8,
                                                      ifelse(str_detect(tolower(pbp_by_drive[our.ind, ]$last.play.text.no.pen), "missed|blocked|failed"),
                                                             -6,
                                                             -7))
  
  ## For defensive SAFETIES:
  our.ind <- which(pbp_by_drive$drive_result_detailed == "Safety")
  pbp_by_drive[our.ind, ]$score_pts_by_text <- -2
  
  # For offensive FG made:
  our.ind <- which(pbp_by_drive$drive_result_detailed %in% offensive.FG.score.drive.res)
  pbp_by_drive[our.ind, ]$score_pts_by_text <- 3
  
  # For blocked punt SAFETY made:
  our.ind <- which(pbp_by_drive$drive_result_detailed == "Blocked Punt (Safety)")
  if (length(our.ind) > 0){
    pbp_by_drive[our.ind, ]$score_pts_by_text <- 2
  }
  
  print("Checking the summary of values for drive scored points:")
  print("Original 'score_pts':")
  print(table(pbp_by_drive$score_pts))
  
  print("My 'score_pts_by_text':")
  print(table(pbp_by_drive$score_pts_by_text))
  
  
  
  
  print("Check if the drive results correspond to SOLELY DEFENSIVE scores (negative)")
  print("Original 'score_pts':")
  bad.ind <- which(!pbp_by_drive$drive_result_detailed[pbp_by_drive$score_pts < 0] %in% c(defensive.TD.score.drive.res, "Safety"))
  print(length(bad.ind))
  if (length(bad.ind) > 0) print(pbp_by_drive$drive_result_detailed[pbp_by_drive$score_pts < 0][bad.ind])
  
  print("My 'score_pts_by_text':")
  bad.ind <- which(!pbp_by_drive$drive_result_detailed[pbp_by_drive$score_pts_by_text < 0] %in% c(defensive.TD.score.drive.res, "Safety"))
  print(length(bad.ind))
  if (length(bad.ind) > 0) print(pbp_by_drive$drive_result_detailed[pbp_by_drive$score_pts_by_text < 0][bad.ind])
  
  
  print("# Check if the drive results correspond to SOLELY OFFENSIVE scores (positive)")
  print("Original 'score_pts':")
  bad.ind <- which(!pbp_by_drive$drive_result_detailed[pbp_by_drive$score_pts > 0] %in% c(offensive.TD.score.drive.res, offensive.FG.score.drive.res, "Blocked Punt (Safety)"))
  print(length(bad.ind))
  if (length(bad.ind) > 0) print(pbp_by_drive$drive_result_detailed[pbp_by_drive$score_pts > 0][bad.ind])
  
  print("My 'score_pts_by_text':")
  bad.ind <- which(!pbp_by_drive$drive_result_detailed[pbp_by_drive$score_pts_by_text > 0] %in% c(offensive.TD.score.drive.res, offensive.FG.score.drive.res, "Blocked Punt (Safety)"))
  print(length(bad.ind))
  if (length(bad.ind) > 0) print(pbp_by_drive$drive_result_detailed[pbp_by_drive$score_pts_by_text > 0][bad.ind])
  
  
  print("# Check if the drive results correspond to NON-SCORING possessions (=0)")
  print("Original 'score_pts':")
  bad.ind <- which(pbp_by_drive$drive_result_detailed[pbp_by_drive$score_pts == 0] %in% c(defensive.TD.score.drive.res, "Safety",
                                                                                          offensive.TD.score.drive.res, offensive.FG.score.drive.res, "Blocked Punt (Safety)"))
  print(length(bad.ind))
  if (length(bad.ind) > 0) print(pbp_by_drive$drive_result_detailed[pbp_by_drive$score_pts == 0][bad.ind])
  
  print("My 'score_pts_by_text':")
  bad.ind <- which(pbp_by_drive$drive_result_detailed[pbp_by_drive$score_pts_by_text == 0] %in% c(defensive.TD.score.drive.res, "Safety",
                                                                                                  offensive.TD.score.drive.res, offensive.FG.score.drive.res, "Blocked Punt (Safety)"))
  print(length(bad.ind))
  if (length(bad.ind) > 0) print(pbp_by_drive$drive_result_detailed[pbp_by_drive$score_pts_by_text == 0][bad.ind])
  
  
  
  ### RED ZONE CONVERSIONS (after having defined a more reliable "score_pts_by_text")
  pbp_by_drive$rz_conversion <- pbp_by_drive$rz_visit & (pbp_by_drive$score_pts_by_text > 0)
  
  
  # View(pbp_by_drive %>%
  #        filter(drive_result_detailed == "Kickoff Return Touchdown") %>%
  #    # filter(score_pts == 0, drive_result_detailed %in% scoring_drive_results) %>%
  #    # filter(score_pts !=0 , !(drive_result_detailed %in% scoring_drive_results)) %>%
  #   select(last.play.text.no.pen, score_pts, score_diff_start, score_diff_stop, drive_result_detailed))
  
  # View(pbp %>% 
  #        filter(pos_score_pts != score_pts, scoring_play == 1) %>% 
  #        select(game_id_drive, pos_team, def_pos_team, play_type, play_text, pos_score_pts, score_pts))
  
  
  # View(pbp %>%
  #       # filter(scoring_play == 0, play_type %in% scoring_drive_results) %>%
  #        filter(game_id_drive %in% c("401261674-19", "401261674-19.5", "401261674-20")) %>%
  #        # filter(game_id_drive %in% c("400547786-10", "400547786-10.5", "400547786-11")) %>%
  #        select(period, clock.minutes, pos_team, def_pos_team, pos_team_score, def_pos_team_score, 
  #               pos_score_pts, score_pts, play_text, play_type, drive_result_detailed,
  #               yards_to_goal, yards_to_goal_end))
  
  
  ## Then could also check:
  
  
  ## NOTE: THE RUSH & PASS YARD GAINS OVER 100 per DRIVE 
  ##  HAPPEN MOSTLY DUE TO PENALTIES !!!
  ##  SO THOSE ARE ACCEPTABLE.
  
  print("Rush yds:")
  print(summary(pbp_by_drive$tot.yds_rushed))
  print("Pass yds:")
  print(summary(pbp_by_drive$tot.yds_receiving))
  
  print("TOTAL drives with >100 rush or pass yds ")
  our.ind <- which(pbp_by_drive$tot.yds_rushed > 100 | pbp_by_drive$tot.yds_receiving > 100)
  print(length(our.ind))
  print("OFFENSIVE PENALTY yardages in drives with >100 rush or pass yds")
  if (length(our.ind) > 0) print(pbp_by_drive[our.ind,]$tot.yds_penalty)
  print("NEGATIVE PLAYS (with penalties) in drives with >100 rush or pass yds")
  if (length(our.ind) > 0) print(pbp_by_drive[our.ind,]$n.negative.plays.with.penalties)
  
  print("# PROBLEMATIC drives with >100 rush or pass yds and 0 OFFENSIVE PENALTIES AND 0 NEGATIVE PLAYS IN GENERAL")
  our.ind <- which((pbp_by_drive$tot.yds_rushed > 100 | pbp_by_drive$tot.yds_receiving > 100) & (pbp_by_drive$penalties.offense == 0 & pbp_by_drive$n.negative.plays.with.penalties == 0 ))
  print(length(our.ind))
  
  
  print("Rushing attempts:")
  print(summary(pbp_by_drive$n.rush))
  print("Stuffed runs:")
  print(table(pbp_by_drive$n.stuffed.runs))
  
  print("Passing attempts:")
  print(summary(pbp_by_drive$n.pass_attempts))
  print("Pass completions:")
  print(summary(pbp_by_drive$n.completions))
  print("Pass incompletions:")
  print(summary(pbp_by_drive$n.incompletions))
  print("Pass breakups")
  print(table(pbp_by_drive$n.pass.breakups))
  
  
  print("Summary of negative plays, NO PENALTIES:")
  print(table(pbp_by_drive$n.negative.plays.no.penalties))
  print("Summary of negative plays, WITH PENALTIES:")
  print(table(pbp_by_drive$n.negative.plays.with.penalties))
  
  print("Penalties count, offense:")
  print(summary(pbp_by_drive$penalties.offense))
  print("Penalties count, defense:")
  print(summary(pbp_by_drive$penalties.defense))
  
  
  
  print("Red zone visits & conversions:")
  print(table(pbp_by_drive$rz_visit))
  print(table(pbp_by_drive$rz_conversion))
  
  print("Fumble totals:")
  print(table(pbp_by_drive$n.fumbles))
  
  print("Sack totals:")
  print(table(pbp_by_drive$n.sacks))
  
  print("Sack yards:")
  print(summary(pbp_by_drive$tot.yds_sacked))
  
  print("Penalty yards:")
  print(summary(pbp_by_drive$tot.yds_penalty))
  
  print("First downs gained:")
  print(summary(pbp_by_drive$first.downs.gained))
  
  print("Third downs attempted:")
  print(summary(pbp_by_drive$third.downs.total))
  
  print("Third downs converted:")
  print(summary(pbp_by_drive$third.downs.converted))
  
  print("Fourth downs attempted:")
  print(summary(pbp_by_drive$fourth.downs.total))
  
  print("Fourth downs converted:")
  print(summary(pbp_by_drive$fourth.downs.converted))
  
  
  
  ### CHECKING CONSISTENCY FOR NUMBER OF PLAYS:
  print("# of PROBLEMATIC drives where n.plays is larger with NO penalties than WITH penalties")
  print(sum(pbp_by_drive$n.plays.no.penalties > pbp_by_drive$n.plays.with.penalties))
  
  
  
  ## KICKOFFS: Checking various kickoff outcomes
  # View(pbp_by_drive %>%
  #        # filter(first.play.type == "Kickoff") %>%
  #        filter(first.play.type.no.pen == "Kickoff", str_detect(tolower(first.play.text.no.pen), "touchback")) %>%
  #        # filter(drive_result_detailed == "Kickoff Return Touchdown") %>%
  #        # filter(drive_result_detailed == "Kickoff Team Fumble Recovery") %>%
  #        # filter(is.na(yds_kickoff_net) & str_detect(tolower(first.play.text), "kickoff")) %>%
  #        select(first.play.text.no.pen, first.play.type.no.pen, yds_kickoff_net, kickoff_touchback, yds_kickoff_return, yds_kickoff))
  
  
  
  ## PUNTS: Checking various punt outcomes
  ##
  ##  Where "yds_punt_net" is NA - that's at the END OF HALVES (which is OK)
  ##  (same for "yds_punt_return" that are not touchbacks)
  ##  
  
  # View(pbp_by_drive %>%
  #       # filter(!is.na(yds_punted), is.na(yds_punt_net)) %>%
  #        filter(!is.na(yds_punted), is.na(yds_punt_return) & !str_detect(tolower(first.play.text.no.pen), "touchback")) %>%
  #        # filter(first.play.type == "Punt", !str_detect(tolower(first.play.text), "touchback")) %>%
  #        # filter(first.play.type == "Punt", str_detect(tolower(first.play.text), "touchback")) %>%
  #        # filter(first.play.type == "Punt Return Touchdown") %>%
  #        # filter(first.play.type == "Punt Team Fumble Lost") %>%
  #        # filter(first.play.type == "Blocked Punt") %>%
  #        # filter(first.play.type == "Punt Team Fumble Recovery Touchdown") %>%
  #        select(first.play.text.with.pen, first.play.type.with.pen, yds_punt_net, yds_punt_return, yds_punted))
  
  pbp_by_drive %>%
    filter(!is.na(yds_punted), is.na(yds_punt_net))
  
  ## Punts: checking drives with >1 punts
  ##       Those are the "NO PLAY" punts
  table(pbp %>% group_by(game_id_drive) %>%
          summarise(n.punts = sum(str_detect(play_text, "punt")),
                    n.yds.punted = sum(!is.na(yds_punted))) %>%
          .[[3]])
  
  pbp %>% group_by(game_id_drive) %>%
    summarise(n.punts = sum(str_detect(play_text, "punt"))) %>%
    filter(n.punts > 1) %>%
    select(game_id_drive)
  
  ## It's clear that "punt" plays & yds_punted not being NA have a PERFECT CORRESPONDENCE 
  ##  BAR.. the "penalty/no play" punts
  ##
  ## To avoid further messing with YARDS_TO_GOAL/YARDS_TO_GOAL_END appropriateness:
  ##    * If it's a RE-PUNT, or a PUNT where the team ended up GOING FOR IT on the RE-TRY:
  ##         JUST KEEP THOSE AS PART OF PREVIOUS POSSESSION.
  ##
  ## !! yds_punted is an indicator OF A PROPER PUNT ANYWAY. !!!
  
  
  ## FG RETURNS:
  # View(pbp_by_drive %>% filter(!is.na(yds_fg_return)) %>%
  #   select(last.play.text.with.pen, last.play.type.with.pen, yds_fg_return))
  
  
  ## STOPPED POSITIONS CHECK:
  ##    Look at the last play, look at "stopped_pos" and "stopped_pos_change_of_pos"
  ##    For VARIOUS DRIVE RESULTS??
  
  sort(unique(pbp_by_drive$drive_result_detailed))
  
  ## "Punt", "Punt Return Touchdown" - ok
  ## "Blocked Field Goal", "Blocked Field Goal Touchdown", "Blocked Punt Team Fumble Lost", "Fumble Return Touchdown", 
  ##    "Kickoff Return Touchdown", "Interception Return Touchdown", "Punt Team Fumble Recovery Touchdown"  - ok
  ## "Downs Turnover" - ok
  ## "End of Half" - all NAs
  ## "Field Goal Good",  "Field Goal Missed",
  ## "Fumble Recovery (Opponent)", "Kickoff Team Fumble Recovery", "On-Side Kick Lost", "Punt Team Fumble Lost" - ok
  ## "Interception Return" - ok
  ## "Missed Field Goal Return" - ok
  ## "Safety" - ok
  
  # View(pbp_by_drive %>%
  #   filter(drive_result_detailed == "Safety") %>%
  #  # filter(stopped_position != stopped_position_change_of_pos) %>%
  #   select(last.play.text.with.pen, last.play.type.with.pen, stopped_position))
  
  ## YARDS GAINED ONLY BY OFFENSE (excluding PUNTS, KICKOFFS, FIELD GOALS, and END OF HALF plays)
  #   Looks quite reasonable, only 1 NA value (without explicitly removing those)
  summary(pbp_by_drive$off.yards_gained)
  
  ## ALL YARDS (including PUNTS, KICKOFFS, FG etc)
  # THAT ONE IS SHAKY...
  summary(pbp_by_drive$tot.yards_gained)
  
  ## SACKS:
  # Some sacks were a part of FUMBLE plays, so they don't show up in PLAY TYPE, but SHOW UP IN PLAY TEXT
  # For those, there's a REALLY GOOD CORRESPONDENCE with SACK VARIABLE, bar PENALTY/NO PLAY cases - which is GOOD, as THOSE SHOULD BE EXCLUDED
  # So KEEP RELYING ON "SACK" VARIABLE
  table(pbp$sack, str_detect(tolower(pbp$play_type), "sack"))
  table(pbp$sack, str_detect(tolower(pbp$play_text), " sack") & !pbp$penalty_no_play)
  pbp$play_text[pbp$sack == 0 & str_detect(tolower(pbp$play_text), " sack")]
  
  ## NUMBER OF FUMBLES:
  #   Those seem to not record on some "fumble returned for TD" plays, so 
  ##  GOTTA GO BY PLAY TEXT and EXCLUDE ALL THE "PENALTY NO PLAY" ONES
  table(pbp$fumble_vec, str_detect(tolower(pbp$play_text), "fumbl"))
  pbp$play_text[pbp$fumble_vec == 0 & str_detect(tolower(pbp$play_text), "fumbl")]
  pbp$penalty_no_play[pbp$fumble_vec == 0 & str_detect(tolower(pbp$play_text), "fumbl")]
  
  ## FUMBLE RETURN YARDS:
  summary(pbp$yds_fumble_return[which(str_detect(tolower(pbp$play_text), "fumbl") & !pbp$penalty_no_play)])
  summary(pbp$yards_gained[which(str_detect(tolower(pbp$play_text), "fumbl") & !pbp$penalty_no_play)])
  
  summary(pbp_by_drive$tot.yds_fumble_return[pbp_by_drive$n.fumbles >= 1])
  summary(pbp_by_drive$tot.yds_fumble_return)
  
  summary(pbp_by_drive$tot.yards_gained[pbp_by_drive$n.fumbles >= 1])
  
  
  
  ## FIRST DOWN stuff
  #     Taken care of before the "by_drive" definition, so see there.
  
  ## THIRD/FOURTH DOWN stuff
  ##    Taken care of, seems to be smooth.
  
  ## PENALTY YARDS:
  ##  Not counted (=NA) during Kickoffs, Punts, Blocked/Missed FG returns
  table(pbp$play_type[pbp$penalty_flag & is.na(pbp$yds_penalty)])
  ## Still has some "FG Missed" (1), Kickoff (5), Punt (13), and "Punt Team Fumble Lost" (1) => THOSE ARE ALL DECLINED
  table(pbp$play_type[pbp$penalty_flag & !is.na(pbp$yds_penalty)])
  table(pbp$play_type[pbp$penalty_flag & !is.na(pbp$yds_penalty) & !pbp$penalty_declined])
  
  pbp$yds_penalty[pbp$penalty_offset]
  pbp$play_text[pbp$penalty_declined & pbp$yds_penalty != 0]
  pbp$yds_penalty[pbp$penalty_flag]
  
  pbp$play_text[which(pbp$yds_penalty == 0 & !pbp$penalty_offset & !pbp$penalty_declined)]
  
  ## Checking the "( Yards)" entries, which are ALL for penalties, but VAST MAJORITY is for "penalty_declined"
  # MEH.. not that important to care too much about.
  #
  # View(pbp[str_detect(pbp$play_text, fixed("( Yards)")) & !pbp$penalty_declined & !pbp$penalty_offset, c("penalty_flag", "yds_penalty", "play_text", "penalty_declined")])
  
  # INTENTIONAL GROUNDING stuff:
  #   Seems like a lot of "intentional grounding" showing up where it was ACTUALLY A SACK (play_type = "Sack"),
  #     HENCE having yds_penalty = 0, and ALL THE LOSS BEING REFLECTED IN "yards_gained", ACTUALLY MAKES SENSE
  #   For OTHERS ("play_type != 'Sack'"), seems OK.
  # View(pbp[str_detect(tolower(pbp$play_text), fixed("intentional grounding")) & pbp$play_type != "Sack", 
  #     c("penalty_flag", "yds_penalty", "play_text", "play_type", "penalty_declined")])
  
  
  
  # View(pbp %>% select(game_id_drive, pos_team, def_pos_team, down, drive_play_number, play_text))
  
  # plays.no.penalties = sum(down != 0 & (!penalty_flag | (penalty_flag & penalty_declined))),
  # plays.with.penalties = sum(down != 0),
  
  
  # View(#pbp[sort(c(our.ind)), ] %>% 
  #  # pbp[sort(c(our.ind, our.ind-1, our.ind+1)), ] %>% 
  #  pbp %>% filter(game_id_drive %in% c("400547640-12")) %>%
  #        # pbp %>% filter(game_id_half == "400547947-2") %>%
  #       .[, c("pos_team", "def_pos_team", "change_of_pos_team", "game_id_drive", "game_id_half", "drive_number",
  #                "drive_result_detailed", "pos_team_score", "def_pos_team_score", "change_of_pos_team", # "change_of_poss",
  #                "play_type", "play_text",  
  #             "first_by_yards", "first_by_penalty", "down",
  #             "yards_to_goal", "yards_to_goal_end", 
  #                "clock.minutes", "clock.seconds",
  #             # "yds_punted",
  #                # "penalty_flag", "yds_penalty", 
  #                #"fumble_vec",  "sack_vec", 
  #                "yards_gained", "yds_rushed", "yds_receiving", "yds_sacked", "yds_fumble_return",
  #                "drive_time_minutes_elapsed", "drive_time_seconds_elapsed",
  #                "period", "drive_time_minutes_end", "drive_time_seconds_end",
  #                "down")])
  
  
  
  #####
  ## 0. Check STOPPED POSITIONS, by DRIVE RESULT potentially..
  ##
  
  
  summary(pbp[str_detect(tolower(pbp$play_type), "half"), "yards_to_goal"])
  mean(is.na(pbp[str_detect(tolower(pbp$play_type), "half"), "yards_to_goal_end"]))
  
  # Checking NA stopped positions:
  ##      ALL OF THEM either:
  ##          * have "lead(game_id_half) != game_id_half", which means IT'S THE LAST DRIVE IN THE HALF, so IT MAKES SENSE FOR IT TO HAVE "NA" value.
  ##          * OR are the drive BEFORE THE "End of Half"-Play-Only "drive", which ALSO MAKES SENSE TO BE NA (0 plays)
  
  ## In case there are NA such that they DON'T PRECEED "END OF HALF"
  our.ind <- which(pbp_by_drive$game_id_half == lead(pbp_by_drive$game_id_half) & is.na(pbp_by_drive$stopped_position) &
                     lead(pbp_by_drive$n.plays.with.penalties) != 0)
  our.ind
  
  print("Number of problematic observations, need to be looked at:")
  print(length(our.ind))
  
  ## Checking halves that DID NOT have "NA" stopped position
  ##  End of Half drives that happen to have determinable "yards_to_goal" at the end, that's fine
  our.ind <- which(pbp_by_drive$game_id_half != lead(pbp_by_drive$game_id_half) & !is.na(pbp_by_drive$stopped_position))
  our.ind
  
  
  
  sum(is.na(pbp_by_drive$stopped_position))
  length(unique(pbp_by_drive$game_id_half))
  
  
  # View(#pbp_by_drive[sort(c(our.ind)), ] %>%
  #   pbp_by_drive[sort(c(our.ind, our.ind+1)), ] %>%
  #     #pbp_by_drive %>% filter(game_id_drive %in% c("400547640-12")) %>%
  #     #pbp %>% filter(game_id_drive %in% c("400560174-15", "400560174-16")) %>%
  #       # filter(drive_result_detailed == "Safety") %>%
  #        # filter(stopped_position != stopped_position_change_of_pos) %>%
  #        select(last.play.text.with.pen, last.play.type.with.pen, stopped_position, n.plays.with.penalties)
  #       #select(game_id_drive, pos_team, def_pos_team, play_type, play_text)
  #     )
  
  
  #####
  ## CHECKING SUMMARIES OF STOPPED POSITIONS FOR VARIOUS DRIVE RESULTS
  ##   ALL LOOK PRETTY DARN GOOD (FG Missed, Punts are worth keeping an eye on though)
  #####
  
  sort(unique(pbp_by_drive$drive_result_detailed))
  j <- 16
  
  stat <- sort(unique(pbp_by_drive$drive_result_detailed))[j]
  sum(pbp_by_drive$drive_result_detailed == stat)
  
  stat
  stop.pos.vec <- pbp_by_drive %>% 
    filter(drive_result_detailed == stat) %>% 
    .[["stopped_position"]]
  
  sort(stop.pos.vec)
  summary(stop.pos.vec)
  
  
  tail(sort(stop.pos.vec), 10)
  
  
  #  PUNTS: when you got < 20yds (stopped position at 80+), it's an issue - GOTTA CHECK THESE OUT
  ##         WHILE 70+ were ACTUALLY LEGITIMATE (sometimes there's no kicker to try it from 30yd line, 45+ yard-FG)
  
  summary(pbp_by_drive %>% filter(drive_result_detailed == "Punt") %>% .[["stopped_position"]]) # %>% summary()
  our.ind <- which(pbp_by_drive$drive_result_detailed == "Punt" & pbp_by_drive$stopped_position >= 80) 
  
  print("Number of problematic PUNT Stopped Positions (>80):")
  print(length(our.ind))
  
  
  
  # FIELD GOALS MISSED: from own 49, stopped position of 51 (fine), 
  ##      but also has one value of stopped position at 84 attempted (although it should put at 80 max, even when missed within 20yd)
  ##      probably just a SLIGHT ERROR... but NEED TO KEEP ON EYE ON THOSE
  
  # boxplot(pbp_by_drive %>% filter(drive_result_detailed == "Field Goal Missed") %>% .[["stopped_position"]]) # %>% summary()
  print("Number of somewhat problematic FG Missed Stopped Positions (>80):")
  print(nrow(pbp_by_drive %>% filter(drive_result_detailed == "Field Goal Missed", stopped_position > 80)))
  
  print("Number of REALLY problematic FG Missed Stopped Positions (<= 30):")
  print(nrow(pbp_by_drive %>% filter(drive_result_detailed == "Field Goal Missed", stopped_position <= 30)))
  
  
  
  
  #######
  ## CHECK TOTAL YARDAGES of A DRIVE:
  ##     1. For abs(yards)>100: NEEDED TO FLIP the SIGN for YARDS GAINED whenever doing all the "TRANSFERRING OF PUNTS OVER TO NEXT POSSESSION" !!!
  ##           (DID THAT EARLIER)
  ##     2. For NA: those are all BEFORE "END OF HALF" stuff, so JUST REMOVE NA's from calculation, 
  ##         but KEEP TRACK OF NAs in a DIFFERENT VARIABLE (JUST IN CASE for FUTURE CHECK-UPS)
  ######
  
  summary(pbp_by_drive$tot.yards_gained)
  summary(pbp_by_drive$off.yards_gained)
  
  ## Some have "yds_offense" > 100 because of a FG that got blocked, yet got back to offense.
  print("# of PROBLEMATIC (over 100) yards gained by TOTAL:")
  print(sum(abs(pbp_by_drive$tot.yards_gained) > 100))
  print("# of PROBLEMATIC (over 100) yards gained by OFFENSE ONLY:")
  print(sum(abs(pbp_by_drive$off.yards_gained) > 100))
  
  print("# of REALLY PROBLEMATIC OFFENSE+SPECIAL TEAMS YARDAGES (> 110):")
  our.ind <- which(pbp_by_drive$off.yards_gained + ifelse(!is.na(pbp_by_drive$yds_punt_return),
                                                          pbp_by_drive$yds_punt_return,
                                                          ifelse(!is.na(pbp_by_drive$yds_kickoff_return),
                                                                 pbp_by_drive$yds_kickoff_return,
                                                                 0)) > 110)
  print(length(our.ind))
  
  
  
  
  
  ## THE RUSH & PASS YARD GAINS OVER 100 per DRIVE 
  ##  HAPPEN MOSTLY DUE TO PENALTIES !!!
  ##  SO THOSE ARE ACCEPTABLE.
  print("# of drives w >100 yards gained by RUSHING, not really problematic (can happen due to penalties):")
  print(sum(abs(pbp_by_drive$tot.yds_rushed) > 100))
  print("# of drives w >100 yards gained by PASSING, not really problematic (can happen due to penalties):")
  print(sum(abs(pbp_by_drive$tot.yds_receiving) > 100))
  
  
  ## THE RUSH & PASS YARD GAINS OVER 100 per DRIVE 
  ##  HAPPEN MOSTLY DUE TO PENALTIES !!!
  ##  SO THOSE ARE ACCEPTABLE.
  print("# of PROBLEMATIC (over 100) yards gained/lost BY PENALTIES:")
  print(sum(abs(pbp_by_drive$tot.yds_penalty) > 100))
  
  
  # View(# pbp_by_drive %>%
  #        # filter(abs(tot.yds_penalty) > 100) %>%
  #      #  filter(abs(tot.yards_gained) > 100) %>%
  #   
  # #pbp_by_drive[sort(c(our.ind)), ] %>%
  # pbp_by_drive[sort(c(our.ind, our.ind-1, our.ind+1)), ] %>%
  # 
  #     select(game_id_drive, # pos_team, def_pos_team, down, play_text,
  #            tot.yards_gained, off.yards_gained, tot.yds_penalty,
  #            yds_punt_return, yds_kickoff_return,
  #            first.play.text.with.pen, first.play.type.with.pen,
  #            last.play.text.with.pen,  last.play.type.with.pen,
  #            # n.plays.with.penalties, penalties.offense,
  #            # drive_duration_seconds,
  #            # drive_time_period_start,
  #            # drive_time_minutes_start, drive_time_seconds_start,
  #            # drive_time_minutes_end, drive_time_seconds_end,
  #            stagn.clock))
  # 
  # 
  # View(pbp %>%
  #         # filter(game_id_drive %in% c("400953393-20", "400953393-21")) %>%
  #        filter(game_id_drive %in% c("400955149-19", "400955149-20")) %>%
  #      
  #       # filter(play_type == "Interception Return") %>%
  #        # filter(drive_time_seconds_elapsed == 0, drive_time_minutes_end == 0, drive_time_seconds_end == 0) %>%
  #        # filter(drive_time_minutes_elapsed == 0 & drive_time_seconds_elapsed == 0) %>%
  #        # %>% filter(drive_time_minutes_elapsed < 0 | drive_time_seconds_elapsed < 0) %>%
  #        # filter(game_id == unique(pbp$game_id[pbp$play_type == "Punt Team Fumble Lost"])[1]) %>%
  #        select(game_id, drive_number, # pos_team, def_pos_team, down, play_text,
  #               pos_team, def_pos_team,
  #               play_type, play_text,
  #               drive_result_detailed,
  #               yds_punt_return, yds_kickoff_return,
  #               # yds_rushed, yds_receiving, yds_penalty, penalty_flag,
  #               yards_gained, yards_to_goal, yards_to_goal_end
  #               # clock.minutes, clock.seconds,
  #               # drive_time_minutes_elapsed, drive_time_seconds_elapsed,
  #               # drive_time_minutes_start, drive_time_seconds_start,
  #               # drive_time_minutes_end, drive_time_seconds_end
  #               ))
  
  
  
  
  # View(pbp_by_drive %>% filter(abs(yards_gained) > 100))
  
  pbp_by_drive %>%
    filter(tot.na.yards_gained > 0)
  
  ## Checking drives with  yards_gained = NA
  ##
  ## Those are VASTLY followed either by:
  ##    1. DIFFERENT HALF, OR
  ##    2. END OF HALF "DRIVE" (0 plays)
  ##  WHICH ARE FINE.
  ##
  ## So we LOOK FOR THOSE that DON'T FIT THAT DESCRIPTION, and CALL THOSE PROBLEMATIC
  our.ind <- which((pbp_by_drive$tot.na.yards_gained > 0 | pbp_by_drive$off.na.yards_gained > 0) & 
                     (pbp_by_drive$game_id_half == lead(pbp_by_drive$game_id_half)) &
                     (lead(pbp_by_drive$n.plays.with.penalties) != 0))
  our.ind
  
  print("# of PROBLEMATIC drives in terms of yards_gained/yards_gained_off ='NA'")
  print(length(our.ind))
  
  # View(pbp_by_drive[sort(c(our.ind, our.ind+1)), ])
  
  
  # pbp %>%
  #  filter(game_id_drive %in% c("400548433-25")) %>%
  #  # filter(game_id_drive %in% c("400548433-2")) %>%
  #  # select(game_id_drive, pos_team, def_pos_team, play_type, yards_gained, yds_punted, yds_punt_return)
  #   select(game_id_drive, pos_team, def_pos_team, play_type, down, drive_play_number)
  #   
  
  
  
  
  
  
  
  
  
  ########################
  ########################
  ## TIME DRIVE LASTED  ##
  ########################
  ########################
  
  summary(pbp_by_drive$drive_duration_seconds)
  summary(pbp_by_drive$drive_time_minutes_start)
  summary(pbp_by_drive$drive_time_seconds_start)
  
  ## 1. NA drive results (for 2014):
  ##    - One due to "End of Half",
  ##    - Another due to it being a PUNT play stuff
  ## For ONE-PLAY "drives", just set it to 0.
  
  our.ind <- which(is.na(pbp_by_drive$drive_duration_seconds) & pbp_by_drive$n.plays.with.penalties %in% c(0,1))
  
  print("# of NA-drive time observations being set to 0:")
  print(length(our.ind))
  if (length(our.ind) > 0){
    pbp_by_drive$drive_duration_seconds[our.ind] <- 0
  }
  
  bad.ind <- which(is.na(pbp_by_drive$drive_duration_seconds) & pbp_by_drive$n.plays.with.penalties > 1)
  print("# of NA-drive time observations THAT ARE BAD ONES, UNACCOUNTED FOR:")
  print(length(bad.ind))
  
  
  #####
  ### Negative drive times
  #####
  
  ## In 2014 there's ONLY 1 NEGATIVE VALUE:
  ##    - due to incorrectly subtracting "15:00" in transitions between games/quarters
  
  print("Overall number of NEGATIVE DURATION DRIVES:")
  print(sum(pbp_by_drive$drive_duration_seconds < 0))
  # boxplot(pbp_by_drive$drive_duration_seconds)
  
  ## "-15:00" types of cases, which could be parsed out and added a 900:
  pbp_by_drive[pbp_by_drive$drive_duration_seconds <0 &
                 pbp_by_drive$drive_time_minutes_end == 15 &
                 pbp_by_drive$drive_time_seconds_end == 00, "drive_duration_seconds"] <- 900 + pbp_by_drive[pbp_by_drive$drive_duration_seconds <0 &
                                                                                                              pbp_by_drive$drive_time_minutes_end == 15 &
                                                                                                              pbp_by_drive$drive_time_seconds_end == 00, "drive_duration_seconds"]
  
  
  print("PROBLEMATIC - Number of negative duration drives STILL REMAINING after +15:00 FIX:")
  print(sum(pbp_by_drive$drive_duration_seconds < 0))
  
  
  ### WAS NOT AN ISSUE in 2014, BUT CHECK LATER YEARS 
  ##
  # ## For others - a single "Kickoff Return Touchdown" observation having similar issue to "On-Side Kicks".. just make it 0?
  # pbp_by_drive[pbp_by_drive$drive_duration_seconds <0, "drive_duration_seconds" ] <- 0
  # # pbp_by_drive[pbp_by_drive$drive_duration_seconds <0, "drive_duration_seconds" ] <- - pbp_by_drive[pbp_by_drive$drive_duration_seconds <0, "drive_duration_seconds"]
  # sort(pbp_by_drive$drive_duration_seconds)[1:10]
  
  
  
  
  #####
  ## NOTE (APRIL 2nd, 2023):
  ##   MAKE SURE TO CHECK 0-DURATIONS for DRIVE RESULTS !!! and PICK UP ON WEIRD ONES
  #####
  
  ## Checking PLAY TYPES:
  ##    Mostly Punt plays, which is good
  ##    Also Kickoff return TD, fumble recovery, on-side kick lost - all good
  table(pbp_by_drive$drive_result_detailed[pbp_by_drive$drive_duration_seconds == 0 & !str_detect(pbp_by_drive$drive_result_detailed, "Punt")])
  
  ## Checking N OF PLAYS:
  table(pbp_by_drive$n.plays.with.penalties[pbp_by_drive$drive_duration_seconds == 0])
  table(pbp_by_drive$n.plays.no.penalties[pbp_by_drive$drive_duration_seconds == 0])
  
  ## Checking the MULTI-PLAY drives with 0 DURATION
  ##   Vastly 1-play, or 2-play ones that are OK for a 0-second duration.
  
  pbp_by_drive[pbp_by_drive$drive_duration_seconds == 0 & pbp_by_drive$n.plays.with.penalties > 1, ]
  
  
  print("Number of PROBLEMATIC 0-DURATION drives with >2 PLAYS (OVERALL):")
  print(sum(pbp_by_drive$n.plays.with.penalties[pbp_by_drive$drive_duration_seconds == 0] > 2))
  
  
  ## Some are just BAD RECORDING: the clock is moving, but "drive_minutes/seconds_start = drive_minutes/seconds_end" nonetheless
  ##   400548259-2, 400548257-20
  ## Others: STAGNANT CLOCK
  ##   400547780-19, 400548038-7
  ##
  
  ## In LATER YEARS (2018+), there are some "SEMI-STAGNANT" clock, where it changes ONCE, halfway through a drive,
  ## but is STILL UNRELIABLE. 
  ## All these have a signature of:
  ##      0s drive duration
  ##      0m0s drive end time (REGARDLESS of WHEN THE DRIVE WAS IN THE GAME)
  ##
  ##  Set DURATIONS for all of these to NA
  
  print("Number of PROBLEMATIC 0-DURATION, 0M:0S END TIME, drives with SNEAKY STAGNANT CLOCK (SET TO NA):")
  our.ind <- which(pbp_by_drive$drive_duration_seconds == 0 & 
                     pbp_by_drive$drive_time_minutes_end == 0 & pbp_by_drive$drive_time_seconds_end == 0)
  print(length(our.ind))
  
  pbp_by_drive[our.ind, "drive_duration_seconds"] <- NA
  
  
  
  ### WITH >2 PLAYS:
  ##
  ## For those with NON-STAGNANT CLOCK & NON 0m:0s END TIME:
  ##    Do the ACTUAL CALCULATION, by simply SUBTRACTING THE STARTING CLOCK of the NEXT DRIVE from THIS ONE
  ##    900*(period_start_next - period_start_current) + 
  ##      60*(minutes_start_current - minutes_start_next) +
  ##        (seconds_start_current - seconds_start_next)
  ##  (period is going up, while minutes & seconds going down, hence makes sense that the difference is flipped)
  
  our.ind <- which(pbp_by_drive$drive_duration_seconds == 0 & 
                     !(pbp_by_drive$drive_time_minutes_end == 0 & pbp_by_drive$drive_time_seconds_end == 0) &
                     pbp_by_drive$n.plays.with.penalties > 2 & 
                     !pbp_by_drive$stagn.clock & 
                     pbp_by_drive$game_id_half == lead(pbp_by_drive$game_id_half))
  
  if (length(our.ind) > 0){
    pbp_by_drive[our.ind, "drive_duration_seconds"] <- 900*(pbp_by_drive[our.ind+1, c("drive_time_period_start")] - pbp_by_drive[our.ind, c("drive_time_period_start")]) +
      60*(pbp_by_drive[our.ind, c("drive_time_minutes_start") ] - pbp_by_drive[our.ind+1, c("drive_time_minutes_start") ]) +
      pbp_by_drive[our.ind, c("drive_time_seconds_start") ] - pbp_by_drive[our.ind+1, c("drive_time_seconds_start")]
  }
  
  ## If next drive is FROM A DIFFERENT HALF:
  ##   Just do the conversion of time left of the current drive into duration
  our.ind.next.end.of.half <- which(pbp_by_drive$drive_duration_seconds == 0 & 
                                      !(pbp_by_drive$drive_time_minutes_end == 0 & pbp_by_drive$drive_time_seconds_end == 0) &
                                      pbp_by_drive$n.plays.with.penalties > 2 & 
                                      !pbp_by_drive$stagn.clock & 
                                      pbp_by_drive$game_id_half != lead(pbp_by_drive$game_id_half))
  
  if (length(our.ind.next.end.of.half) > 0){
    pbp_by_drive[our.ind.next.end.of.half, "drive_duration_seconds"] <- 60*(pbp_by_drive[our.ind.next.end.of.half, c("drive_time_minutes_start")]) +
      pbp_by_drive[our.ind.next.end.of.half, c("drive_time_seconds_start") ] 
  }
  
  ## If the CLOCK IS STAGNANT (& duration is 0), & NON 0m:0s END TIME:
  ##    then SET THE DURATION TO NA:
  
  our.ind.stagn.clock <- which(pbp_by_drive$drive_duration_seconds == 0 & pbp_by_drive$n.plays.with.penalties > 2 & 
                                 !(pbp_by_drive$drive_time_minutes_end == 0 & pbp_by_drive$drive_time_seconds_end == 0) &
                                 pbp_by_drive$stagn.clock)
  
  if (length(our.ind.stagn.clock) > 0){
    pbp_by_drive[our.ind.stagn.clock, "drive_duration_seconds"] <- NA
  }
  
  
  print("Number of 0-DURATION drives with >2 PLAYS (JUST THE STAGNANT CLOCK), TO BE SET TO 'NA':")
  print(length(our.ind.stagn.clock))
  
  
  ######
  ## Overly HIGH drive times
  ######
  
  ## There are some funky issues, especially for latter years, so just do the following:
  ##    Any drive that has: <15 plays & >=900 seconds => SET TO NA
  
  
  print("Top-10 drive durations (in seconds):")
  print(sort(pbp_by_drive$drive_duration_seconds, decreasing = T)[1:10])
  
  print("Number of FULL-QUARTER DRIVES (or higher) with MORE THAN 15 PLAYS:")
  our.ind <- which(pbp_by_drive$drive_duration_seconds >= 900 & pbp_by_drive$n.plays.no.penalties > 15)
  our.ind
  print(length(our.ind))
  
  print("PROBLEMATIC - Number of FULL-QUARTER DRIVES (or higher) with NO MORE THAN 15 PLAYS, TO BE SET TO 'NA':")
  our.ind <- which(pbp_by_drive$drive_duration_seconds >= 900 & pbp_by_drive$n.plays.no.penalties <= 15)
  our.ind
  print(length(our.ind))
  if (length(our.ind) > 0){
    pbp_by_drive[our.ind, ]$drive_duration_seconds <- NA
  }
  
  
  cat("\n")
  cat("\n")
  print("YARDS AFTER INTERCEPTION: ENTIRE SUMMARY")
  print(summary(pbp_by_drive$tot.yds_after_int))
  
  print("YARDS AFTER INTERCEPTION: ONLY DRIVES with INT!!")
  print(summary(pbp_by_drive[pbp_by_drive$drive_result_detailed %in% c("Interception Return", "Interception Return Touchdown"), ]$tot.yds_after_int))
  
  
  
  cat("\n")
  cat("\n")
  print("BY-DRIVE DATA SET SIZE, FINAL")
  print(dim(pbp_by_drive))
  cat("\n")
  cat("\n")
  
  save(pbp_by_drive, file=paste0("R_Object_Files/pbp_by_drive_UPDATED_ROUGHING_KICKER_", year,".Robj"))
  
}