# --- Load data ---

#libraries
library(dplyr)
library(ggplot2)
library(ggthemes)
library(caret)
library(randomForest)

#load data
kobe <- tbl_df(read.csv("/Users/antbae/OneDrive/R/Side_projects/Kobe/data.csv", stringsAsFactors = TRUE))

# --- Preview data ---

#read data
str(kobe)

#change $game_data to a date class
kobe$game_date <- as.Date(kobe$game_date)

#change $season to date and drop the year behind
kobe$season <- as.factor(substr(kobe$season, 1, 4))

# --- Data cleaning ---

#change features to factors
kobe <-
kobe %>%
  mutate_each(funs(as.factor),
              c(period, 
                playoffs, 
                shot_made_flag
                ))

#remove NAs
kobe <-
  kobe %>%
  filter(!is.na(shot_made_flag))

# --- Visualization --- 

#% of missed and made shots
table(kobe$shot_made_flag)

#visualization of shots missed and made
ggplot(kobe, aes(x = lon, y = lat, color = shot_made_flag)) +
  geom_point(alpha = .6, size = .8) +
  ggtitle("Shot Distribution") +
  ylab("") +
  xlab("") +
  theme_few() +
  scale_color_gdocs() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()
        )
  
#shots misssed and made with facets 
labels <- c('0' = "Misses", '1' = "Makes")

ggplot(kobe, aes(x = lon, y = lat, color = shot_made_flag)) +
  geom_point(alpha = .5, size = .5) +
  facet_wrap(~shot_made_flag, labeller=labeller(shot_made_flag = labels)) +
  ggtitle("Misses vs Makes") +
  ylab("") +
  xlab("") +
  theme_few() +
  scale_color_gdocs() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position="none"
  )

#formula for frequency in location of shots 
percn <- function(feat){
  kobe %>%
    group_by_(feat) %>%
    summarise(
      n = n()
    ) %>%
    mutate(
      percent = round(n / (sum(n)), 2)
    ) %>%
    select(-n)
}

#formula for visualizing location of shots
courtp <- function(feat){
  ggplot(kobe, aes(x = lon, y = lat)) +
    geom_point(aes_string(color = feat)) +
    ggtitle(paste(feat)) +
    theme_few() +
    scale_color_few() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank()
    )
}

#shot_zone_range
courtp("shot_zone_range")
percn("shot_zone_range")

#shot_zone_basic
courtp("shot_zone_basic")
percn("shot_zone_basic")

#shot_zone_area
courtp("shot_zone_area")
percn("shot_zone_area")

#show highest % shots, better graph than above
percp <- function(i){
  temp <- prop.table(table(kobe[[i]], kobe$shot_made_flag),1)
  temp <- as.data.frame(temp)
  temp <- temp[temp$Var2 == 1,]
  temp$percent <- round(temp$Freq, 2)
  

  ggplot(temp, aes(x = reorder(Var1, Freq), y = Freq, label = percent)) +
    geom_bar(stat = "identity", fill = "light blue", color = "black") +
    coord_flip() +
    expand_limits(y = c(0, 1)) +
    ggtitle(paste(i)) +
    ylab("") +
    xlab("") +
    theme_few() +
    scale_fill_gdocs() +
    geom_text(size = 3, hjust = - .3)
  
} 

percp("shot_zone_range")
percp("combined_shot_type")
percp("action_type")
percp("opponent")

#how is his shooting if we use the same graph and separate by conference?
western <- c("GSW", "SAS", "OKC", "LAC", "POR", "DAL", "MEM", 
             "HOU", "UTA", "SAC", "DEN", "PHX", "NOP", "MIN")

kobe <- 
  kobe %>%
  mutate(
    conference = as.factor(ifelse(opponent %in% western, "West", "East"))
  )

opponent_conf <- 
  kobe %>%
  group_by(opponent, shot_made_flag) %>%
  mutate(
    shooting_percent_against = n()
  ) %>%
  select(opponent, shot_made_flag, shooting_percent_against, conference) %>%
  distinct(opponent, shot_made_flag) %>%
  arrange(shot_made_flag) %>%
  ungroup %>%
  group_by(opponent) %>%
  mutate(
    shooting_percent_against = round(shooting_percent_against[2] / sum(shooting_percent_against), 2)
  )

ggplot(opponent_conf, aes(x = reorder(opponent, shooting_percent_against), 
                          y = shooting_percent_against, fill = conference)) +
  geom_bar(stat = "identity") +
  theme_few() +
  scale_fill_fivethirtyeight() +
  coord_flip() +
  expand_limits(y = c(0, 1)) +
  ggtitle("opponent by conference") +
  ylab("") +
  xlab("") 

# --- Features ---

#feature creation, month
kobe$month <- as.factor(substr(kobe$game_date, 6,7))
percp("month")

#feature creation, home vs away  
kobe <- 
kobe %>%
  mutate(
    at_home = substr(matchup, 5, 6),
    at_home = as.factor(ifelse(at_home == "vs", 1, 0))
  )
percp("at_home")

#feature creation, shot categories
kobe$shot_category <- as.factor(gsub("([A-Za-z]+).*", "\\1", kobe$action_type))
percp("shot_category")

#feature creation, days since last game
kobe <-
kobe %>% 
  mutate(
    last_game_days = as.numeric((game_date - rep(lag(unique(game_date)), table(game_id)))),
    last_game_days = ifelse(last_game_days > 190, "New Season 1st game", last_game_days)
  )

kobe$last_game_days[kobe$last_game_days < 0] <- 0
kobe$last_game_days <- as.factor(kobe$last_game_days)
  
percp("last_game_days")  
