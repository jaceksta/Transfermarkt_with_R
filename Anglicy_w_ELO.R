library(tidyverse)

elo_TOP25 <- tibble()
which_country <- tibble()

elo_TOP50 <- tibble()
which_country_50 <- tibble()


for (i in 2000:2022){
  url <- paste("http://api.clubelo.com/", toString(i), "-06-30", sep = "")
  a <- read.csv(url)%>%
    filter(Rank < 51)%>%
    group_by(Country)%>%
    summarise(teams = n())

  a$year <- i
  
  elo_TOP50 <- rbind(elo_TOP50, a)
  which_country_50 <- rbind(which_country_50, a[which.max(a$teams),])
  
}

elo_TOP5leagues <- elo_TOP25%>%
  filter(Country == "ENG" | Country == "GER" | Country == "FRA" | Country == "ITA" | Country == "ESP")


ggplot(data = elo_TOP5leagues, aes(x=year, y=teams, group=Country, color=Country))+
  geom_line()

ggplot(data = which_country, aes(x=year, y=teams, color=Country, fill=Country))+
  geom_col()+
  scale_y_continuous(breaks = seq(0,10,1))


ggplot(data = which_country2, aes(x=year, y=teams, fill=Country))+
  geom_col(position = "dodge")+
  scale_y_continuous(breaks = seq(0,10,1))+
  labs(
    title = "Który kraj miał najwięcej klubów w TOP25 rankingu ELO",
    subtitle = "źródło: clubelo.com",
    caption = "Autor: Jacek Staszak"
  )+
  xlab("Rok")+
  ylab("Liczba drużyn")+
  scale_fill_discrete(labels=c('Anglia', 'Hiszpania', 'Niemcy', 'Włochy'))+
  theme_minimal()+
  theme(legend.title= element_blank())



which_country2 <- elo_TOP25%>%
  group_by(year)%>%
  filter(teams == max(teams))

