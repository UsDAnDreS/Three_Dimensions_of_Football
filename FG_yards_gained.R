library(tidyverse)
load("R_Object_Files/pbp_cleaned_w_EPA_with_FG_2014.Robj")
View(pbp_by_drive %>% filter(play_type == "Punt") %>% select(yards_gained))


load("R_Object_Files/pbp_by_drive_Off_Def_ST_2014.Robj")
pbp_by_drive_full$off.yards_gained
