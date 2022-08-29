library(worldfootballR)
library(lubridate)
library(tidyverse)




transfers <- data_frame()
countries <- c("England", "France", "Italy", "Spain", "Germany")

for (i in 2000:2022){
  
  for (j in 1:5){
    a <- tm_team_transfer_balances(countries[j], i)
    a$season <- i
    transfers <- rbind(transfers, a)
  }
  
}


summary <- transfers%>%
  group_by(league, season)%>%
  summarise(fees = sum(expenditure_euros))

summary$fees <- summary$fees / 1000000

ggplot(summary, aes(x=season, y = infl_adj_fees, group=league, color = league))+
  geom_line()+
  theme_minimal()+
  xlab("Sezon")+
  ylab("Wydatki w mln euro")+
  labs(
    title="Wydatki transferowe TOP5 lig Europy",
    subtitle = "Źródło: transfermarkt.com",
    caption = "Autor: Jacek Staszak"
  )


install.packages("priceR") 
library(priceR)

country <- "EU"
inflation_dataframe <- retrieve_inflation_data(country)
countries_dataframe <- show_countries()


transfers$infl_adj_fees <- adjust_for_inflation(transfers$expenditure_euros, transfers$season, country, to_date = 2021,
                     inflation_dataframe = inflation_dataframe,
                     countries_dataframe = countries_dataframe)


this_window <- transfers%>%
  filter(season == 2022)


this_window <- arrange(this_window, expenditure_euros)

ggplot(data = this_window, aes(x=squad, y = expenditure_euros/1000000, fill = league))+
  geom_col()+
  scale_x_discrete(limits=this_window$squad)+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  ylab("Wydatki w mln euro")+
  labs(title="Wydatki transferowe każdego klubu z TOP5 lig Europy w sezonie 22/23",
       subtitle="Źródło: transfermarkt.com")+
  xlab("")
