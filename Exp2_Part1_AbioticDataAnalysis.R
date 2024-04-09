library(tidyverse)
library(cowplot)
library(readr)
library(ggplot2)
library(lubridate)
library(readxl)
library(dplyr)
library(tibble)
library (ggpubr)

LV <- read.csv("D:/JExpMarBiolEcol_Salas_Cassiopea/Loggers/Exp2/CassioAcclimation.csv")
head(LV)
nrow(LV)
LV <- dplyr::rename(LV, Row = "X")
head(LV)

LV$Condition <- "Variable"
head(LV)

LC <- read.csv("D:/JExpMarBiolEcol_Salas_Cassiopea/Loggers/Exp2/Light_Control_Cassio.csv")
head(LC)
nrow(LC)
LC <- dplyr::rename(LC, Row = "X")
head(LC)
LC$Condition <- "Ambient"

DC <- read.csv("D:/JExpMarBiolEcol_Salas_Cassiopea/Loggers/Exp2/Dark_Control_Cassio.csv")
head(DC)
nrow(DC)
DC <- dplyr::rename(DC, Row = "X")
head(DC)

DC$Condition <- "Shaded"

library(dbplyr)
controls <- rbind(LC, DC)
head(controls)
nrow(controls)
spcontroldf <- rbind(controls, LV)
head(spcontroldf)
nrow(spcontroldf)

spconrtoldf <- drop_na(spcontroldf)
spcontroldf <- tidyr::separate(spcontroldf, 'Date',
                      into = c('longdate', 'Time'),
                      sep= ' ') 
head(spcontroldf)
nrow(spcontroldf)

spcontroldf[['longdate']] <- as.POSIXct(spcontroldf[['longdate']],
                               format = "%m/%d/%Y")
head(spcontroldf)

spcontroldf$longdate <- format(spcontroldf$longdate, "%d/%m/%Y")
spcontroldf <- dplyr::rename(spcontroldf, Date = longdate)
head(spcontroldf)

#Raw Temp
ntempgraph <- ggplot(data=spcontroldf, 
                     aes(x=as.Date(Date, format = "%d / %m / %Y"), 
                         y=Temp, color = Condition)) +
  geom_point(size=1, alpha = 1/10)+ theme_bw()+
  #facet_grid(cols = vars(Plot))+
  theme(axis.text.x = element_text(angle=45, margin = margin(t=20, r=100)))+
  labs(title="Raw temperature data", y="Temperature (?C)", x="Date")
ntempgraph
#Trim Data Set to only Include the Length of the Experiment

spcontroldf$Date <- as.Date(spcontroldf$Date, format = "%d/%m/%Y")
class(spcontroldf$Date)

head(spcontroldf)
view(spcontroldf)
spcontroldftrimmed <- spcontroldf[spcontroldf$Date >= "2022-08-16" & spcontroldf$Date <= "2022-09-11", ]
spcontroldftrimmed <- drop_na(spcontroldftrimmed)
view(spcontroldftrimmed)

ntempgraph <- ggplot(data=spcontroldftrimmed, 
                     aes(x=as.Date(Date, format = "%Y-%m-%d"), 
                         y=Temp, color = Condition)) +
  geom_point(size=1, alpha = 1/10)+ theme_bw()+
  theme(axis.text.x = element_text(angle=45, margin = margin(t=20, r=100)))+
  labs(title="Raw temperature data", y="Temperature (?C)", x="Date")
ntempgraph

#Still need to trim some time off

#View(spcontroldftrimmed)

#The temperature becomes the water temperature at row 2559
#colnames(spcontroldftrimmed)


#dftrimmed <- rename(dftrimmed, Row = "?..Count")
#head(spcontroldftrimmed)
#view(spcontroldftrimmed)
#spcontroldfnew = spcontroldftrimmed[spcontroldftrimmed$Row > 2559, ]

#head(spcontroldfnew)

#tempgraph <- ggplot(data=spconrtoldf, 
                    #aes(x=as.Date(Date, format = "%Y-%m-%d"), 
                        #y=Temp, color = Condition)) +
  #geom_point(size=1, alpha = 1/10)+ theme_bw()+
  #theme(axis.text.x = element_text(angle=45, margin = margin(t=20, r=100)))+
  #labs(title="Raw temperature data", y="Temperature (?C)", x="Date")
#tempgraph

library(ggplot2)
splightgraph <- ggplot(data=spcontroldftrimmed, 
                     aes(x=as.Date(Date, format = "%Y-%m-%d"), 
                         y=Lux, color = Condition)) +
  geom_point(size=1, alpha = 1/10)+ theme_bw()+
  theme(axis.text.x = element_text(angle=45, margin = margin(t=20, r=100)))+
  labs(title="Raw light data", y="Light (Lux)", x="Date")
splightgraph

#Now we have a curated database. But we can do better.
spcontroldf2 <- spcontroldftrimmed %>%
  tidyr::separate('Date',
                  into = c('Year', 'Month', 'Day'),
                  sep= '-',
                  remove = FALSE)
head(spcontroldf2)

#We made new columns for day, month, and year!
#Year and month are not particularly helpful for us,
#But those are the important scales for most ecological studies

#Now make summaries
spcontroldfsummaries <- spcontroldf2 %>%
  dplyr::group_by(Year, Month, Day, Date, Condition)%>%
  dplyr::summarise(meantemp = mean(Temp), meanlux = mean(Lux), mintemp = min(Temp), maxtemp = max(Temp), 
            minlux = min(Lux), maxlux = max(Lux))%>%
  mutate(temprange = maxtemp-mintemp)%>%
  mutate(luxrange = maxlux-minlux)

head(spcontroldfsummaries)
#View(hobofullmean)

#tempplot <- ggplot(spcontroldfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y=meantemp, group= Condition, ))+
  #geom_smooth(aes(), color = "black")+
  #theme_bw()+
  #labs(title= "Daily temperature mean", y="Daily temperature summaries (?C) with 95% CI", x="Date")
#tempplot
head(spcontroldfsummaries)
tempplotmean <- ggplot(spcontroldfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y=meantemp, color=Condition))+
  geom_smooth()+
  theme_bw()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())+
  labs(title= "Daily temperature mean", y="Daily temperature summaries (?C) with 95% CI", x="Date")
tempplotmean

#tempplot <- ggplot(spcontroldfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y=mintemp, group= Condition))+
 # geom_smooth(aes(y=mintemp), color = "blue")+
  #theme_bw()+
  #labs(title= "Daily temperature min", y="Daily temperature summaries (?C) with 95% CI", x="Date")
#tempplot
tempplotmin <- ggplot(spcontroldfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y=mintemp, color= Condition))+
  geom_smooth()+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  theme(legend.position = "none")+
  labs(title= "Daily temperature min", y="Daily temperature summaries (?C) with 95% CI", x="Date")
tempplotmin

#tempplot <- ggplot(spcontroldfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y=maxtemp, group= Condition))+
 # geom_smooth(aes(y = maxtemp), color = "red")+
  #theme_bw()+
  #labs(title= "Daily temperature max", y="Daily temperature summaries (?C) with 95% CI", x="Date")
#tempplot
tempplotmax <- ggplot(spcontroldfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y=maxtemp, color= Condition))+
  geom_smooth(aes(), color = "darkslategray4")+
  geom_smooth(aes(y=mintemp), color = "cadetblue3")+
  geom_smooth(aes(y = meantemp), color = "darkslategray4")+
  theme_bw()+
  theme_classic()+
  theme(legend.position = "none", axis.title.x = element_blank())+
  labs(title= "Daily temperature max", y="Daily temperature summaries (?C) with 95% CI", x="Date")
tempplotmax

library(gridExtra)
grid.arrange(tempplotmax, tempplotmean, tempplotmin, nrow=1)

lightplotmean <- ggplot(spcontroldfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y=meanlux,color= Condition))+
  geom_smooth()+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title= "Daily light mean", y="Daily temperature summaries (Lux) with 95% CI", x="Date")+
  theme(axis.title.x = element_blank())
lightplotmean

lightplotmin <- ggplot(spcontroldfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y=minlux,color= Condition))+
  geom_smooth()+
  theme_bw()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())+
  labs(title= "Daily light min", y="Daily temperature summaries (Lux) with 95% CI", x="Date")
lightplotmin

lightplotmax <- ggplot(spcontroldfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y=maxlux, color=Condition))+
  geom_smooth(aes(), color = "darkslategray4")+
  geom_smooth(aes(y=minlux), color = "cadetblue3")+
  geom_smooth(aes(y = meanlux), color = "darkslategray4")+
  theme_bw()+
  theme_classic()+
  theme(legend.position = "none")+
  labs(title= "Daily light max", y="Daily temperature summaries (Lux) with 95% CI", x="Date")
lightplotmax

library(gridExtra)
grid.arrange(tempplotmax, lightplotmax, ncol=1)

spcontroldf2$DateFactor <- as.factor(spcontroldf2$Date)

#Test for significance between zones, month, LTER sites
hobomeanmonthaov <- aov(Temp~DateFactor, data=spcontroldf2)
summary(hobomeanmonthaov)
#comb through the pairwise comparisons to find when environment changes (If you want)

#Post-hoc tests
HSD <- TukeyHSD(hobomeanmonthaov, ordered=FALSE)
HSD
