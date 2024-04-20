# https://drmowinckels.io/blog/2018/gamm-random-effects/
# https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/random.effects.html


library(tidyverse)
library(splines)
library(mgcv)
library(fuzzyjoin)


for (year in 2014){
  
  
  print(year)
  
  
  
  # load(file=paste0(c("R_Object_Files/pbp_by_drive_w_EPA_Off_Def_ST_with_FG_", year, ".Robj", sep=""), collapse=""))
  #load(file=paste0(c("R_Object_Files/pbp_by_drive_w_EPA_Off_Def_ST_FG_ep_before_focused_", year, ".Robj", sep=""), collapse=""))
  
  load(file=paste0(c("R_Object_Files/pbp_by_drive_UPDATED_Off_Def_ep_before_focused_", year, ".Robj", sep=""), collapse=""))
  
  
  
  ## Make the "game_id" variable (excluding the half)
  pbp_by_drive$game_id <- str_remove(pbp_by_drive$game_id_half, "-1|-2")
  
  
  # For END-OF-HALF drive results, make "ep_after" NA
  pbp_by_drive$ep_after[which(str_detect(pbp_by_drive$drive_result_detailed, "End of"))] <- NA
  
  # For results RIGHT BEFORE the END OF HALF - also make "ep_after" NA
  pbp_by_drive$ep_after[which(str_detect(lead(pbp_by_drive$drive_result_detailed), "End of"))] <- NA
  
  ## REMOVING ALL THE ROWS with EP_AFTER = NA
  pbp_by_drive <- pbp_by_drive %>% filter(!is.na(ep_after))
  

  
  # Make adjustments for the "ep_after" depending on whether there was a score
  # Initialize it at "ep_after"
  pbp_by_drive$ep_after_new <- pbp_by_drive$ep_after
  
  # Set all the ones followed by NEXT HALF/GAME to "NA"
  our.ind <- which(pbp_by_drive$game_id_half != lead(pbp_by_drive$game_id_half))
  pbp_by_drive$ep_after_new[our.ind] <- NA
  

  
  # View(pbp_by_drive %>% select(ep_before, ep_after, ep_after_new, EPA.diff,
  #                                   first.play.text.no.pen, first.play.text.with.pen,
  #                                   last.play.text.no.pen, last.play.text.with.pen))
  
  
  
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
  

  
  print("PROBLEMATIC (NEXT PLAY AFTER SCORE ISN'T A KICKOFF or a PENALTY)")
  our.ind <- which(pbp_by_drive$score_pts_by_text != 0 & 
                     !(str_detect(lead(pbp_by_drive$first.play.type.no.pen), "Kickoff") | str_detect(lead(pbp_by_drive$first.play.type.with.pen), "Penalty")) &
                     pbp_by_drive$game_id_half == lead(pbp_by_drive$game_id_half) )
                     #pbp_by_drive$pos_team != lead(pbp_by_drive$pos_team))
  
  print(length(our.ind))
  
  
  ######
  ## SCORES where HALF STAYS THE SAME, and POS TEAM CHANGES...
  ######
  
  our.ind <- which(pbp_by_drive$score_pts_by_text != 0 & 
                     # str_detect(lead(pbp_by_drive$first.play.type.no.pen), "Kickoff") & 
                     pbp_by_drive$game_id_half == lead(pbp_by_drive$game_id_half) & 
                     pbp_by_drive$pos_team != lead(pbp_by_drive$pos_team))
  
  pbp_by_drive$ep_after_new[our.ind] <- -pbp_by_drive$ep_before[our.ind+1]
  
  
  ######
  ## !!! DEFENSIVE SCORES !! where HALF STAYS THE SAME afterwards
  ##    but FOR SOME REASON THE TEAM CHANGES
  ##    => seems like FUMBLES RECOVERED BY THE TEAM ITSELF and TAKEN FOR A TD
  ######
  
  our.ind <- which(pbp_by_drive$score_pts_by_text %in% c(-6,-7,-8) & 
                     # str_detect(lead(pbp_by_drive$first.play.type.no.pen), "Kickoff") & 
                     pbp_by_drive$game_id_half == lead(pbp_by_drive$game_id_half) & 
                     pbp_by_drive$pos_team != lead(pbp_by_drive$pos_team))
  our.ind
  
  print("MISRECORDED FUMBLE RETURN TDs (those that got recovered by team itself)")
  print(length(our.ind))
  
  
  if (year == 2018){
    
    our.ind.1 <- our.ind[which(pbp_by_drive$drive_result_detailed[our.ind] != "Kickoff Team Fumble Recovery Touchdown")]
    pbp_by_drive$score_pts_by_text[our.ind.1] <- -pbp_by_drive$score_pts_by_text[our.ind.1]
    pbp_by_drive$ep_after[our.ind.1] <- -pbp_by_drive$ep_after[our.ind.1]
    pbp_by_drive$EPA.diff[our.ind.1] <- pbp_by_drive$ep_after[our.ind.1] - pbp_by_drive$ep_before[our.ind.1]
    pbp_by_drive$drive_result_detailed[our.ind.1] <- "Rushing Touchdown"
    
    our.ind.1 <- our.ind[which(pbp_by_drive$drive_result_detailed[our.ind] == "Kickoff Team Fumble Recovery Touchdown")]
    pbp_by_drive$score_pts_by_text[our.ind.1] <- -pbp_by_drive$score_pts_by_text[our.ind.1]
    pbp_by_drive$ep_after[our.ind.1] <- -pbp_by_drive$ep_after[our.ind.1]
    pbp_by_drive$EPA.diff[our.ind.1] <- pbp_by_drive$ep_after[our.ind.1] - pbp_by_drive$ep_before[our.ind.1]
    pbp_by_drive$drive_result_detailed[our.ind.1] <- "Kickoff Return Touchdown"
    
    
  } else {
    # pbp_by_drive$score_pts[our.ind] <- -pbp_by_drive$score_pts[our.ind]
    pbp_by_drive$score_pts_by_text[our.ind] <- -pbp_by_drive$score_pts_by_text[our.ind]
    pbp_by_drive$ep_after[our.ind] <- -pbp_by_drive$ep_after[our.ind]
    pbp_by_drive$EPA.diff[our.ind] <- pbp_by_drive$ep_after[our.ind] - pbp_by_drive$ep_before[our.ind]
    pbp_by_drive$drive_result_detailed[our.ind] <- "Rushing Touchdown"
  }
  
  print("MISRECORDED FUMBLE RETURN TDs - REMAINING AFTER ADJUSTMENT")
  our.ind <- which(pbp_by_drive$score_pts_by_text %in% c(-6,-7,-8) & 
                     # str_detect(lead(pbp_by_drive$first.play.type.no.pen), "Kickoff") & 
                     pbp_by_drive$game_id_half == lead(pbp_by_drive$game_id_half) & 
                     pbp_by_drive$pos_team != lead(pbp_by_drive$pos_team))
  our.ind
  print(length(our.ind))
  
  
  
  ######
  ## !!! DEFENSIVE SCORES !! where HALF STAYS THE SAME afterwards
  ##    and the POSSESSION TEAM DOESN'T CHANGE (which is the typical behavior)
  ##
  ## For those: just switch "ep_after_new" to be equal to "ep_after" of the subsequent possession,
  ##    and DELETE the subsequent possession...
  ##   BUUUT... gotta make sure we account for cases where there were CONSECUTIVE DEFENSIVE SCORES...
  ##   Doing it VIA WHILE LOOP
  ##
  ######

  while (TRUE){
    
  our.ind <- which(pbp_by_drive$score_pts_by_text %in% c(-6,-7,-8) & 
                     # str_detect(lead(pbp_by_drive$first.play.type.no.pen), "Kickoff") & 
                     pbp_by_drive$game_id_half == lead(pbp_by_drive$game_id_half) & 
                     pbp_by_drive$pos_team == lead(pbp_by_drive$pos_team))
  our.ind
  
  if (length(our.ind) > 0){
    pbp_by_drive$ep_after_new[our.ind] <- pbp_by_drive$ep_after_new[our.ind+1]
    pbp_by_drive <- pbp_by_drive[-c(our.ind[!(c(our.ind-1) %in% our.ind)] + 1), ]
  } else {
    break
  }
  
  }
  
  our.ind <- which(pbp_by_drive$score_pts_by_text %in% c(-6,-7,-8) & 
                     # str_detect(lead(pbp_by_drive$first.play.type.no.pen), "Kickoff") & 
                     pbp_by_drive$game_id_half == lead(pbp_by_drive$game_id_half) )
  our.ind
  
  

  # ### OR, IF WE WANT TO RETAIN THOSE (e.g. for EP.DIFF comparisons etc),
  # ### just do "ep_after_new = next possessions ep_before"
  # 
  # pbp_by_drive$ep_after_new[our.ind] <- pbp_by_drive$ep_before[our.ind+1]
  
  View(pbp_by_drive[sort(c(our.ind, our.ind+1)), ] %>%
   # pbp_by_drive[sort(c(2543:2545)), ] %>%
         #filter((str_detect(drive_result_detailed, "Kickoff") | str_detect(drive_result_detailed, "On-Side Kick")) &
        #          score_pts_by_text > 0) %>%
         select(ep_before, ep_after, ep_after_new, EPA.diff,
                score_pts_by_text,
                n.plays.with.penalties, n.plays.no.penalties,
                first.play.text.no.pen, first.play.text.with.pen,
                last.play.text.no.pen, last.play.text.with.pen))
  
  
  
  
  ### Checking stuff with 1-PLAY PENALTY DRIVE afterwards..
  our.ind <- which(pbp_by_drive$score_pts_by_text != 0 & 
                     # str_detect(lead(pbp_by_drive$first.play.type.no.pen), "Kickoff") & 
                     pbp_by_drive$game_id_half == lead(pbp_by_drive$game_id_half) & 
                     pbp_by_drive$pos_team != lead(pbp_by_drive$pos_team) & 
                     is.na(lead(pbp_by_drive$first.play.text.no.pen)))
  print(length(our.ind))
  
  our.ind <- which(pbp_by_drive$score_pts_by_text != 0 & 
                     # str_detect(lead(pbp_by_drive$first.play.type.no.pen), "Kickoff") & 
                     pbp_by_drive$game_id_half == lead(pbp_by_drive$game_id_half) & 
                     pbp_by_drive$pos_team == lead(pbp_by_drive$pos_team) & 
                     is.na(lead(pbp_by_drive$first.play.text.no.pen)))
  print(length(our.ind))
  
  
}

  

########
## Cleaning up the team names, including the "Non-Major" category for all the non-FBS teams
########

team.names.year <- data.frame(Team=sort(unique(c(pbp_by_drive$pos_team, pbp_by_drive$def_pos_team))))
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
pbp_by_drive$pos_team <- ifelse(pbp_by_drive$pos_team %in% FBS.team.names,
                                pbp_by_drive$pos_team,
                                "Non-Major")
pbp_by_drive$def_pos_team <- ifelse(pbp_by_drive$def_pos_team %in% FBS.team.names,
                                    pbp_by_drive$def_pos_team,
                                    "Non-Major")

pbp_by_drive$game_id <- as.factor(str_remove(pbp_by_drive$game_id_half, "-1|-2"))




######
## CLEANING SOME OF THE VARIABLES
######

pbp_by_drive <- pbp_by_drive %>% mutate(game_id_half = factor(game_id_half))
pbp_by_drive$drive <- as.numeric(str_remove(pbp_by_drive$game_id_drive, "^\\d+-"))
                        
                        
  #######
  ### REGULAR MODELS: LM, LMER...
  #######

  # lm.obj <- lm(ep_after_new ~ ep_before,
  #              data=pbp_by_drive)
  # plot(lm.obj, which=1)
  # 
  # 
  # ## ACCOUNTING FOR WITHIN-HALF DEPENDENCE
  # library(lme4)
  # lmer.obj <- lmer(ep_after_new ~ ep_before + (1|game_id),
  #                  data=pbp_by_drive)
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
  #                data=pbp_by_drive)
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
  
  
  gamm.full.obj <- gamm(ep_after_new ~  s(ep_before, bs="cr"),
                        random=list(game_id= ~ 1),
                        correlation = corAR1(form = ~ drive | game_id_half),
                        data=pbp_by_drive)
  gamm.full.obj
  
  #####
  ## Using RANDOM EFFECTS ONLY for:
  ##    * the AR1 correlation structure to reflect DEPENDENCE OF CLOSE-TO-EACH-OTHER DRIVES 
  #####
  
  gamm.obj <- gamm(ep_after_new ~  s(ep_before, bs="cr"),
                   #random=list(game_id= ~ 1),
                    correlation = corAR1(form = ~ drive | game_id_half),
                   data=pbp_by_drive)
  gamm.obj
  summary(gamm.obj$gam)
  summary(gamm.obj$lme)
  
  #corAR1(form = ~ months_since_start_of_timeseries | site)
  
  # anova(gam.obj, gamm.obj)
  
  plot(gamm.obj$gam,pages=1)
  gam.check(gamm.obj$gam,pch=19,cex=.3)
  
  # vis.gam(gamm.obj$gam)
  
  
  ## The NO RANDOM EFFECTS model (for testing their significance)
  gam.obj <- gamm(ep_after_new ~  s(ep_before, bs="cr"),
                  data=pbp_by_drive)
  
  pbp_by_drive <- pbp_by_drive %>% 
    filter(!is.na(ep_after_new) & !is.na(ep_before))
  
    
  ## HYPOTHESIS TEST
  anova(gam.obj$lme, gamm.obj$lme, gamm.full.obj$lme)
  
  ranef(gamm.obj$lme)
  
  
  resid(gamm.obj$gam)
  gamm.obj$gam$y - predict(gamm.obj$gam)
  
  pbp_by_drive$posit_margin <- resid(gamm.obj$gam)
  pbp_by_drive$posit_margin_gam <- resid(gam.obj$gam)
  
  
  ### OFFENSE ratings: with random effects
  pbp_by_drive %>%
    group_by(pos_team) %>%
    summarise(mean.posit.margin = mean(posit_margin)) %>%
    arrange(desc(mean.posit.margin))
  
  
  # ### OFFENSE ratings: NO random effects
  # pbp_by_drive %>%
  #   group_by(pos_team) %>%
  #   summarise(mean.posit.margin = mean(posit_margin_gam)) %>%
  #   arrange(desc(mean.posit.margin))
  
  
  ### DEFENSE ratings: with random effects
  pbp_by_drive %>%
    group_by(def_pos_team) %>%
    summarise(mean.posit.margin = mean(-posit_margin)) %>%
    arrange(desc(mean.posit.margin))
  
  # ## DEFENSE ratings: NO random effects
  # pbp_by_drive %>%
  #   group_by(def_pos_team) %>%
  #   summarise(mean.posit.margin = mean(-posit_margin_gam)) %>%
  #   arrange(desc(mean.posit.margin))
  
  
####
## GETTING RANKINGS by EPA
####
  
  View(pbp_by_drive)
  
  ## OFFENSE (per drive)
  pbp_by_drive %>%
    group_by(pos_team) %>%
    summarise(EPA = mean(EPA.diff),
              n.drives = n(),
              n.halves = length(unique(game_id_half))) %>%
    arrange(desc(EPA))
  
  
  ## DEFENSE (per drive)
  
  pbp_by_drive %>%
    group_by(def_pos_team) %>%
    summarise(EPA = mean(-EPA.diff),
              n.drives = n(),
              n.halves = length(unique(game_id_half))) %>%
    arrange(desc(EPA))
  
  
  ####
  ## GETTING RANKINGS by POINTS SCORED/ALLOWED
  ####
  
  pbp_by_drive %>%
    group_by(pos_team) %>%
    summarise(Pts = mean(score_pts_by_text),
              n.drives = n(),
              n.halves = length(unique(game_id_half))) %>%
    arrange(desc(Pts))
  
  
  pbp_by_drive %>%
    group_by(def_pos_team) %>%
    summarise(Pts = mean(score_pts_by_text),
              n.drives = n(),
              n.halves = length(unique(game_id_half))) %>%
    arrange(Pts)
  
  
  ####
  ## GETTING RANKINGS by POINTS SCORED/ALLOWED,
  ##    ADJUSTED FOR THE COMPLEMENTARY EPA
  ####
  
  
  ## OFFENSE
  pbp_by_drive %>%
    group_by(pos_team) %>%
    summarise(Pts = mean(score_pts_by_text),
              n.drives = n(),
              n.halves = length(unique(game_id_half))) %>% left_join(
    (pbp_by_drive %>%
      group_by(def_pos_team) %>%
      summarise(mean.posit.margin = mean(-posit_margin))),
    by=c("pos_team" = "def_pos_team")) %>%
    mutate(Adj.Pts = Pts - mean.posit.margin) %>%
    arrange(desc(Adj.Pts))
  
  ## DEFENSE
  pbp_by_drive %>%
    group_by(def_pos_team) %>%
    summarise(Pts = mean(score_pts_by_text),
              n.drives = n(),
              n.halves = length(unique(game_id_half))) %>% left_join(
                (pbp_by_drive %>%
                   group_by(pos_team) %>%
                   summarise(mean.posit.margin = mean(posit_margin))),
                by=c("def_pos_team" = "pos_team")) %>%
    mutate(Adj.Pts = Pts + mean.posit.margin) %>%
    arrange(Adj.Pts)
  
  
  
  
  #######
  ## Teams playing best TOTAL COMPLEMENTARY FOOTBALL
  ## (SUM OF "OVER EXPECTED" for BOTH OFFENSE & DEFENSE)
  #######
  
  
  pbp_by_drive %>%
    group_by(pos_team) %>%
    summarise(mean.posit.margin.off = mean(posit_margin)) %>% left_join(
      pbp_by_drive %>%
        group_by(def_pos_team) %>%
        summarise(mean.posit.margin.def = mean(-posit_margin)),
      by = c("pos_team" = "def_pos_team")) %>%
    mutate(total.mean.posit.margin = mean.posit.margin.off + mean.posit.margin.def) %>%
    arrange(desc(total.mean.posit.margin))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  predict(gamm.obj$lme)
  predict(gamm.obj$gam)
  
  
  # #####
  # ## Getting GAME_ID as a SPLINE RANDOM EFFECT
  # ##
  # ## The first case (where we used the "random=" part) uses a “standard” maximum likelihood function 
  # ## on the estimates, which makes it fast and powerful. 
  # ## The second example introduces a penalized ridge function over the random effects, 
  # ## making it quite a conservative approach. Which you can see through the reduced F-values and 
  # ## increased p-values, and but also the increased correlation!
  # ##
  # #####
  # 
  # gam.obj <- gamm(ep_after_new ~  s(ep_before, k=4) + s(game_id_half, bs="re"),
  #                 data=pbp_by_drive %>% mutate(game_id_half = factor(game_id_half)))
  # 
  # summary(gam.obj$gam)
  # summary(gam.obj$lme)
  # 
  # predict(gam.obj$gam)
  # resid(gam.obj$gam)
  
  
  
  

  


View(pbp_by_drive %>%
       #pbp_by_drive[our.ind.1,] %>%
       # filter(str_detect(drive_result_detailed, "Punt") & str_detect(side, "ST.")) %>%
       select(ep_before, ep_after, ep_after_new, EPA.diff, score_pts,
              first.play.text.no.pen, first.play.text.with.pen,
              last.play.text.no.pen, last.play.text.with.pen))

