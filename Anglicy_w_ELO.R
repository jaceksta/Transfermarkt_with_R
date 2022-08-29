library(tidyverse)

elo_TOP25 <- tibble()

for (i in 2000:2022){
  url <- paste("http://api.clubelo.com/", toString(i), "-06-30", sep = "")
  a <- read.csv(url)%>%
    filter(Rank < 26)%>%
    group_by(Country)%>%
    summarise(teams = n())

  a$year <- i
  
  elo_TOP25 <- rbind(elo_TOP25, a)
  
}

elo_TOP5leagues <- elo_TOP25%>%
  filter(Country == "ENG" | Country == "GER" | Country == "FRA" | Country == "ITA" | Country == "ESP")


ggplot(data = elo_TOP5leagues, aes(x=year, y=teams, group=Country, color=Country))+
  geom_line()



