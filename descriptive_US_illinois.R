library(tidyverse)
library(cowplot)

datD <- read.csv("C:/Users/mrung/gitrepos/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
datC<- read.csv("C:/Users/mrung/gitrepos/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")


editDat <- function(dat){
  colnames(dat ) <- gsub("*X","date_",colnames(dat ))
  
  dat <- dat %>% pivot_longer(cols=-c(Province.State ,Country.Region  ,   Lat  ,   Long), names_to = "Date") %>%
  separate(Date, into=c("del","Date"), sep="_") %>% select(-del) %>%
  mutate(Date = as.Date(Date, format="%m.%d.%y")) %>% 
  group_by(Province.State ,Country.Region) %>% 
  arrange(Province.State ,Country.Region,Date) %>%
  mutate(nday = dplyr::row_number()) %>%
  as.data.frame()

  return(dat)
}

datD <- editDat(datD)
datC <- editDat(datC)

datD$outcome="deaths"
datC$outcome="confirmed cases"

dat <- rbind(datD,datC) %>% as.data.frame

illinois <- as.data.frame(dat[grep("Illinois",dat$Province.State),])

ggplot() + theme_cowplot()+
  geom_line(data=subset(dat, nday>40 & Country.Region=="US"),aes(x=Date, y=value, group=Province.State), size=1, col="grey")+
  geom_line(data=subset(illinois, nday>40),aes(x=Date, y=value),col="red", size=1.3)+
  scale_x_date(date_breaks = "1 day", date_labels = "%d")+
  labs(x="01 - 23 March [days]", y="Total number",
       caption="Red=Illinois\n Data from https://github.com/CSSEGISandData/COVID-19")+
  facet_wrap(~outcome, scales="free")


ggplot() + theme_cowplot()+
  geom_line(data=subset(illinois, nday>40),aes(x=Date, y=value, col=outcome) ,size=2)+
  geom_point(data=subset(illinois, nday>40),aes(x=Date, y=value, col=outcome),shape=21,fill="white", size=4)+
  scale_x_date(date_breaks = "1 day", date_labels = "%d")+
  labs(x="01 - 22 March [days]", y="Cumulative numbers", 
       title="Reported cumulative Covid-19 cases and deaths in US - Illinois\n",
       caption="Data from https://github.com/CSSEGISandData/COVID-19\n",
       col="")+
  geom_label(data=subset(illinois, nday>40 & value>0),aes(x=Date, y=value, label=value), vjust=-0.5)


