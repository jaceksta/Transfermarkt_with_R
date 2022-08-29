install.packages("worldfootballR")
install.packages("ggsoccer")
library(worldfootballR)
library(lubridate)
library(tidyverse)
library(ggsoccer)

try({
  library(dplyr)
  library(tidyr)
  ## single match
  players <- fotmob_get_match_players(3610132)
  salah_id <- "292462"
  players %>%
    dplyr::filter(id == salah_id) %>%
    dplyr::select(player_id = id, stats) %>%
    tidyr::unnest(stats)})


try({
  df <- load_understat_league_shots(league="EPL")
})


shots <- df%>%
  group_by(season, player) %>%
  tally()

rename(shots, shots = n)

goals <- goals%>%
  group_by(season, player)%>%
  tally()

rename(goals, goals = n)

new_df <- merge(shots, goals, by.x= c("season", "player"), by.y= c("season", "player"))

new_df <- rename(new_df, shots = n.x)
new_df <- rename(new_df, goals = n.y)

new_df$eff <- new_df$goals / new_df$shots

plus5 <- new_df%>%
  filter(goals > 5)


ggplot(data = new_df, aes(x=shots, y=goals))+
  geom_point()

ggplot(data = new_df, aes(x=eff, y=goals))+
  geom_point()


february <- callum%>%
  filter(date > "2021-11-08")

sum(february$n)/14



ggplot(data = callum, aes(y=n))+
  geom_line()


ggplot(lewa) +
  annotate_pitch(colour = "white",
                 fill   = "springgreen4",
                 limits = FALSE) +
  geom_point(aes(x = (X*100), y = 100 - (Y*100)),
             fill = "yellow", 
             shape = 21,
             size = 4) +
  theme_pitch() +
  theme(panel.background = element_rect(fill = "springgreen4")) +
  coord_flip(xlim = c(49, 101),
             ylim = c(-12, 112)) +
  ggtitle("Zjebane sytuacje przez Lewusa")


ggplot(lewa2) +
  annotate_pitch(colour = "white",
                 fill   = "springgreen4",
                 limits = FALSE) +
  geom_point(aes(x = (X*100), y = 100 - (Y*100)),
             fill = "yellow", 
             shape = 21,
             size = 4) +
  theme_pitch() +
  theme(panel.background = element_rect(fill = "springgreen4")) +
  coord_flip(xlim = c(49, 101),
             ylim = c(-12, 112)) +
  ggtitle("Gole Robercika")

fbref<- data_frame()
shooting <- data_frame()

for (i in 2018:2022){
  fbref <- rbind(fbref, fb_big5_advanced_season_stats(season_end_year=i,stat_type="standard",team_or_player="player"))
}

for (i in 2018:2022){
  shooting <- rbind(shooting, fb_big5_advanced_season_stats(season_end_year=i,stat_type="shooting",team_or_player="player"))
}


fbref <- fbref%>%
  select(Season_End_Year, Squad, Comp, Player, Pos, Min_Playing, Gls, xG_Expected)

shooting <- shooting%>%
  select(Season_End_Year, Squad, Comp, Player, Pos, Sh_Standard, Sh_per_90_Standard, G_per_Sh_Standard)


goals <- merge(fbref, shooting, by=c("Season_End_Year", "Squad", "Comp", "Player", "Pos"))


goals$Gls_per_90_standard <- goals$Gls/(goals$Min_Playing/90)

goals <- goals%>%
  filter(Min_Playing > 600)

goals <- goals%>%
  filter(Sh_Standard > 0)

forwards <- goals%>%
  filter(Pos == "FW" | Pos == "MF,FW" | Pos == "FW,MF")

forwards <- forwards%>%
  filter(Gls > 9)

ggplot(data = forwards, aes(x=Sh_per_90_Standard, y=Gls_per_90_standard))+
  geom_point()+
  ylim(0,1.5)+
  xlim(0, 8)+
  theme_minimal()+
  labs(x="Strzały na 90 minut",
       y="Gole na 90 minut",
       title="Liczba strzałów a liczba bramek",
       subtitle = "Źródło: Fbref.com",
       caption = "Autor: Jacek Staszak")+
  geom_point(aes(x=1.83, y=1.14, color = "red"))+
  geom_text(aes(x=1.83, y=1.14, label = "Jan Kliment 22/23", vjust = -1))+
  theme(legend.position="none")

ggplot(data = forwards, aes(x=Sh_Standard, y=Gls))+
  geom_point()

ggplot(data = forwards, aes(x=G_per_Sh_Standard, y=Gls_per_90_standard))+
  geom_point()+
  ylim(0,1.5)+
  xlim(0, 0.7)+
  theme_minimal()+
  geom_point(aes(x=0.62, y=1.14, color = "red"))+
  geom_text(aes(x=0.6, y=1.14, label = "Jan Kliment 22/23", vjust = -1))+
  theme(legend.position="none")+
  labs(x="Skuteczność strzałów",
       y="Gole na 90 minut",
       title="Skuteczność strzałów a liczba bramek",
       subtitle = "Źródło: Fbref.com",
       caption = "Autor: Jacek Staszak")





  annotate(geom="vline",
           x = 0.6,
           xintercept = 0.6,
           linetype = "dashed")+
  annotate(geom = "text",
           label = "Skuteczność Jana Klimenta na początku sezonu 22/23",
           x = 0.6,
           y = 0.6,
           angle = 90, 
           vjust = -1)
  

install.packages("reshape2")
library(reshape2)
install.packages("ggthemes")
library(ggthemes)
devtools::install_github('bbc/bbplot')

library(bbplot)

Kliment.long <- melt(Kliment)

Kliment.short <- Kliment.long%>%
  filter(variable == "Gole p90" | variable == "xG p90")

Kliment.short <- rename(Kliment.short, "Metryka" = "variable")


ggplot(Kliment.short, aes(Info, value, fill=Metryka))+
  geom_bar(stat="identity",position="dodge")+
  theme_minimal()+
  labs(x="Sezon i klub",
       y="Wynik",
       title="Gole i gole oczekiwane Jana Klimenta",
       subtitle = "Źródło: Wyscout",
       caption = "Autor: Jacek Staszak")
