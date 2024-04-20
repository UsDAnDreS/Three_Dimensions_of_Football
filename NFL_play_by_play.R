library(tidyverse)

our.data <- read.csv("NFL Play by Play 2009-2016 (v3).csv")
dim(our.data)

hist(our.data$ExpPts[str_detect(our.data$desc, "kicks")])

View(our.data %>%
  filter(str_detect(our.data$desc, "kicks")) %>%
  select(GameID, Drive, TimeSecs, ExpPts, desc))


View(head(our.data,500))

# this.data <- our.data %>% filter(GameID == "2009091000")
this.data %>%
  select(ExpPts)
summary(this.data$ExpPts)


this.data <- our.data %>% filter(TimeSecs == 3600, PlayType == "Kickoff") 
table(this.data$ExpPts)

this.data <- our.data %>% filter(TimeSecs == 1800, PlayType == "Kickoff") 
table(this.data$ExpPts)


View(this.data %>%
  arrange(desc(ExpPts)))


sum(this.data$EPA)
sum(this.data$ExpPts)
View(this.data)

this.data$ScoreDiff  
this.data$PosTeamScore
this.data$DefTeamScore
tail(this.data$desc)

View(this.data %>%
  select(desc, EPA) %>%
  arrange(desc(EPA)))

View(this.data %>%
       select(desc, EPA, ExpPts))


View(this.data %>%
       select(desc, ExpPts) %>%
       arrange(desc(ExpPts)))
