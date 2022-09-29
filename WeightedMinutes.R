install.packages("worldfootballR")
library(worldfootballR)
library(lubridate)
library(tidyverse)


team_urls <- tm_league_team_urls(country_name = "Poland", start_year = 2022)

ekstraklasa_players <- tm_squad_stats(team_url = team_urls)


ekstraklasa_valuations <- tm_player_market_values(country_name = "Poland",
                                                start_year = 2022)


ekstraklasa_join <- merge(ekstraklasa_players, ekstraklasa_valuations, by = "player_name")

keeps <- c("player_name", "team_name", "player_pos", "minutes_played", "date_joined", "player_dob")

ekstraklasa_join <- ekstraklasa_join[keeps]

ekstraklasa_join <- ekstraklasa_join[!(is.na(ekstraklasa_join$date_joined)),]



days_in_club <- c()

for (i in 1:549){
  a <- length(seq(from=as.Date(ekstraklasa_join$date_joined[i]), to = as.Date("2022-09-23"), by = 'day')) - 1
  days_in_club <- c(days_in_club, a)
}

ekstraklasa <- cbind(ekstraklasa_join, days_in_club)

player_age <- c()


ekstraklasa$player_age = as.numeric(difftime(Sys.Date(),ekstraklasa$player_dob, units = "weeks"))/52.25






ekstraklasa$team_abb <- toupper(substr(ekstraklasa$team_name, 1,3))


ekstraklasa$team_abb[ekstraklasa$team_name=="Górnik Zabrze"] <- "GOR"
ekstraklasa$team_abb[ekstraklasa$team_name=="Lechia Gdansk"] <- "LGD"
ekstraklasa$team_abb[ekstraklasa$team_name=="Lech Poznan"] <- "LPO"
ekstraklasa$team_abb[ekstraklasa$team_name=="Miedz Legnica"] <- "MDZ"
ekstraklasa$team_abb[ekstraklasa$team_name=="Rakow Czestochowa"] <- "RCZ"
ekstraklasa$team_abb[ekstraklasa$team_name=="Wisla Plock"] <- "WPL"




cols <- c("CRA"=rgb(252,0,1,maxColorValue = 255), "GOR"=rgb(254,254,254, maxColorValue = 255),
          "JAG"=rgb(223,210,29, maxColorValue = 255), "KOR"=rgb(254,232,3, maxColorValue = 255),
          "LGD"=rgb(31,112,55, maxColorValue = 255), "LEG"=rgb(247,247,247, maxColorValue = 255),
          "LPO"=rgb(2,64,128, maxColorValue = 255), "MDZ"=rgb(5,163,70, maxColorValue = 255),
          "PIA"=rgb(33,78,164,maxColorValue = 255), "POG"=rgb(129,27,37, maxColorValue = 255),
          "RAD"=rgb(1,113,63, maxColorValue = 255), "RCZ"=rgb(217,14,0, maxColorValue = 255),
          "SLA"=rgb(33,184,129, maxColorValue = 255), "STA"=rgb(0,140,208, maxColorValue = 255),
          "WAR"=rgb(9,118,70, maxColorValue = 255), "WID"=rgb(254,13,11, maxColorValue = 255),
          "WPL"=rgb(20,114,185, maxColorValue = 255), "ZAG"=rgb(241,122,32, maxColorValue = 255))





minutes <- ekstraklasa%>%
  filter(minutes_played>60)
  

  


ekstraklasa$team_name


teams <- ekstraklasa%>%
  group_by(team_name, team_abb)%>%
  summarise(mean(player_age))

weighted_age <- c()
weighted_days <- c()


for (i in 1:18){
  a <- ekstraklasa%>%
    filter(team_name == teams$team_name[i])
  
  weighted_age <- c(weighted_age, weighted.mean(a$player_age, a$minutes_played))
  
}

teams <- cbind(teams, weighted_age)

for (i in 1:18){
  a <- ekstraklasa%>%
    filter(team_name == teams$team_name[i] & days_in_club < 2000)
  
  weighted_days <- c(weighted_days, weighted.mean(a$days_in_club, a$minutes_played))
  
}

teams <- cbind(teams, weighted_days)


colnames(teams) <- c("team_name", "team_abb", "mean_age", "weighted_mean_age", "weighted_mean_days")

teams <- teams[order(teams$weighted_mean_days),]



ggplot(minutes, aes(x=team_abb, y=days_in_club, fill=team_abb))+
  geom_violin()+
  scale_fill_manual(values=cols)+
  scale_x_discrete(limits=teams$team_abb)+
  labs(title="Jak długo zawodnicy Ekstraklasy grają w swoim klubie?",
       subtitle="Źródło: Transfermarkt",
       caption = "Autor: Jacek Staszak")+
  xlab("Klub")+
  ylab("Dni zawodnika w klubie")+
  theme_minimal()+
  theme(legend.position="none")

ggplot(teams, aes(x=team_abb, y=weighted_mean_days, fill=team_abb))+
  geom_bar(stat="identity")+
  scale_x_discrete(limits=teams$team_abb)+
  scale_fill_manual(values=cols)+
  labs(title="Średnia dni piłkarzy w klubie ważona przez minuty rozegrane w tym sezonie",
       subtitle="Źródło: Transfermarkt",
       caption = "Autor: Jacek Staszak")+
  xlab("Klub")+
  ylab("Dni zawodnika w klubie")+
  theme_minimal()+
  theme(legend.position="none")



teams <- teams[order(teams$weighted_mean_age),]

ggplot(minutes, aes(x=team_abb, y=player_age, fill=team_abb))+
  geom_violin()+
  geom_hline(yintercept = 25, linetype = "dashed")+
  geom_hline(yintercept = 30, linetype = "dashed")+
  scale_fill_manual(values=cols)+
  scale_x_discrete(limits=teams$team_abb)+
  labs(title="Struktura wiekowa klubów Ekstraklasy",
       subtitle="Źródło: Transfermarkt",
       caption="Autor: Jacek Staszak")+
  xlab("Klub")+
  ylab("Wiek")+
  theme_minimal()+
  theme(legend.position="none")


ggplot(teams, aes(x=team_abb, y=weighted_mean_age, fill=team_abb))+
  geom_col()+
  coord_cartesian(ylim = c(20, 30))+
  scale_x_discrete(limits=teams$team_abb)+
  scale_fill_manual(values=cols)+
  labs(title="Średni wiek piłkarzy w klubie ważony przez minuty rozegrane w tym sezonie",
       subtitle="Źródło: Transfermarkt",
       caption = "Autor: Jacek Staszak")+
  xlab("Klub")+
  ylab("Wiek")+
  theme_minimal()+
  theme(legend.position="none")



