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

# load(file="pbp_by_drive_UPDATED_w_EPA_Off_Def_2014.Robj")
# 
# View(pbp_by_drive %>%
#        filter(game_id_half == "400547728-1") %>%
#        select(ep_before, ep_after, 
#               EPA.diff,
#                              first.play.text.no.pen, first.play.text.with.pen,
#                              last.play.text.no.pen, last.play.text.with.pen))

load(file="pbp_cleaned_w_EPA_with_FG_2014.Robj")
# load(paste0("/home/andrey/Documents/Work/New_College/Research/Play_by_Play_Complementary_Football_Project/pbp_2014_on_campus_data.Robj"))

View(pbp_by_drive %>%
    filter(game_id_half == "400547728-1") %>%
# View(pbp %>%
#        filter(game_id == "400547728") %>%
       select(ep_before, ep_after, EPA,
              play_type, play_text))


View(pbp_by_drive %>% 
  filter(EPA.diff > 0, !(str_detect(drive_result_detailed, "Touchdown") | str_detect(drive_result_detailed, "Field Goal")) ) %>%
  select(ep_before, ep_after, EPA.diff,
         first.play.text.no.pen, first.play.text.with.pen,
         last.play.text.no.pen, last.play.text.with.pen))


pbp_by_drive %>%
  filter(str_detect(game_id_drive, "-1$")) %>%
  select(ep_before)

table(pbp_by_drive$ep_before[str_detect(pbp_by_drive$game_id_drive, "-1$")])

View(pbp_by_drive %>%
  filter(str_detect(game_id_drive, "-1$"), str_detect(game_id_half, "-2$"), ep_before >1) %>%
  select(ep_before, score_diff_start, home) %>%
    arrange(score_diff_start))
