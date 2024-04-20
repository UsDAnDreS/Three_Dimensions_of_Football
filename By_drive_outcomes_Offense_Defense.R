library(tidyverse)

year <- 2014

for (year in 2014:2020){
  
cat("\n")
cat(year)
cat("\n")
  
load(paste0("/home/andrey/Documents/Work/New_College/Research/Three_Dimensions_Of_Football/pbp_by_drive_UPDATED_Off_Def_", year, ".Robj"))
sort(unique(pbp_by_drive$drive_result_detailed))

# View(pbp_by_drive %>%
#   select(game_id_drive, home, pos_team, def_pos_team, drive_result_detailed, stopped_position))

## Checking all times when the possession team didn't shift from one drive to the next (while being within the same half)
## and that DO NOT have "Touchdown" in them (which are PROBLEMATIC - 
#  because it's only a DEFENSIVE TD type of play where possession doesn't change)

our.ind <- which(pbp_by_drive$game_id_half == lead(pbp_by_drive$game_id_half) &
                   pbp_by_drive$def_pos_team != lead(pbp_by_drive$pos_team) &
                   !str_detect(tolower(pbp_by_drive$drive_result_detailed), "touchdown"))
print("Non-TD drives that don't switch possession (should NOT happen):")
print(length(our.ind))
print(unique(pbp_by_drive$drive_result_detailed[our.ind]))

View(pbp_by_drive[sort(c(our.ind, our.ind+1)),] %>%
       select(game_id_drive, home, pos_team, def_pos_team, drive_result_detailed, stopped_position,
              first.play.text.no.pen, first.play.text.with.pen,
              last.play.text.no.pen, last.play.text.with.pen))


## Checking all times when the possession team didn't shift from one drive to the next (while being within the same half)
## and that have "Touchdown" in them (should mostly be DEFENSIVE TD)

our.ind <- which(pbp_by_drive$game_id_half == lead(pbp_by_drive$game_id_half) &
                   pbp_by_drive$def_pos_team != lead(pbp_by_drive$pos_team) &
                   str_detect(tolower(pbp_by_drive$drive_result_detailed), "touchdown"))
our.ind

print("Touchdown drives that don't switch possession (should only be defensive scores):")
print(unique(pbp_by_drive$drive_result_detailed[our.ind]))


# View(pbp_by_drive[sort(c(our.ind, our.ind+1)),] %>%
#        select(game_id_drive, home, pos_team, def_pos_team, drive_result_detailed, stopped_position))

}



# load(paste0("/home/andrey/Documents/Work/New_College/Research/Three_Dimensions_Of_Football/pbp_cleaned_", year, ".Robj"))
# 
# View(pbp_by_drive %>%
#        filter(game_id_half %in% c("400869090-1")) %>%
#   # filter(game_id_drive %in% c("400547815-2", "400547815-2.75")) %>%
#   select(game_id_drive, home, pos_team, def_pos_team, drive_result_detailed, play_text))
# 
# 
# load(paste0("/home/andrey/Documents/Work/New_College/Research/Play_by_Play_Complementary_Football_Project/pbp_", year, "_on_campus_data.Robj"))
# dim(pbp)
# View(pbp %>%
#        filter(game_id %in% c("400869090")) %>%
#        # filter(game_id_drive %in% c("400547815-2", "400547815-2.75")) %>%
#        select(game_id, drive_number, home, pos_team, def_pos_team, drive_result_detailed, play_text))



View(pbp_by_drive %>%
       select(game_id_half, pos_team, def_pos_team, drive_result_detailed, stopped_position))


sort(unique(pbp_by_drive$drive_result_detailed))

i <- 18
sort(unique(pbp_by_drive$drive_result_detailed))[i]
summary(pbp_by_drive$stopped_position[pbp_by_drive$drive_result_detailed == sort(unique(pbp_by_drive$drive_result_detailed))[i]])

our.ind <- which(pbp_by_drive$drive_result_detailed == "Punt" & pbp_by_drive$stopped_position >= 65)
our.ind

View(pbp_by_drive[sort(c(our.ind, our.ind+1)),] %>%
       #filter(drive_result_detailed == sort(unique(pbp_by_drive$drive_result_detailed))[i]) %>%
       #filter(stopped_position >= 65) %>%
       select(stopped_position, drive_result_detailed,
              first.play.text.no.pen, first.play.text.with.pen,
              last.play.text.no.pen, last.play.text.with.pen))
