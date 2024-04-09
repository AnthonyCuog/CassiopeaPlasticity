library(tidyverse)
library(cowplot)
library(readr)
library(ggplot2)
library(lubridate)
library(readxl)
library(dplyr)
library(tibble)
library (ggpubr)

hobologger <- read.csv("D:/JExpMarBiolEcol_Salas_Cassiopea/Loggers/Exp1/Cassio_sp.csv")
head(hobologger)

df <- tidyr::separate(hobologger, 'Date',
                                    into = c('longdate', 'Time'),
                                    sep= ' ') 
head(df)

df[['longdate']] <- as.POSIXct(df[['longdate']],
                                 format = "%m/%d/%Y")
head(df)

df$longdate <- format(df$longdate, "%d/%m/%Y")
df <- rename(df, Date = longdate)
head(df)

#Raw Temp
ntempgraph <- ggplot(data=df, 
                     aes(x=as.Date(Date, format = "%d / %m / %Y"), 
                         y=Temp)) +
  geom_point(size=1, alpha = 1/10)+ theme_bw()+
  #facet_grid(cols = vars(Plot))+
  theme(axis.text.x = element_text(angle=45, margin = margin(t=20, r=100)))+
  labs(title="Raw temperature data", y="Temperature (?C)", x="Date")
ntempgraph

#Trim Data Set to only Include the Length of the Experiment

df$Date <- as.Date(df$Date, format = "%d/%m/%Y")
class(df$Date)

head(df)

dftrimmed <- df[df$Date >= "2022-06-07" & df$Date <= "2022-08-08", ]
head(dftrimmed)

ntempgraph <- ggplot(data=dftrimmed, 
                     aes(x=as.Date(Date, format = "%Y-%m-%d"), 
                         y=Temp)) +
  geom_point(size=1, alpha = 1/10)+ theme_bw()+
  theme(axis.text.x = element_text(angle=45, margin = margin(t=20, r=100)))+
  labs(title="Raw temperature data", y="Temperature (?C)", x="Date")
ntempgraph

#Still need to trim some time off

head(dftrimmed)

#The temperature becomes the water temperature at row 2559
colnames(dftrimmed)


dftrimmed <- rename(dftrimmed, Row = "X....")
head(dftrimmed)
view(dftrimmed)
dfnew = dftrimmed[dftrimmed$Row > 2559, ]
head(dfnew)

tempgraph <- ggplot(data=dfnew, 
                     aes(x=as.Date(Date, format = "%Y-%m-%d"), 
                         y=Temp)) +
  geom_point(size=1, alpha = 1/10)+ theme_bw()+
  theme(axis.text.x = element_text(angle=45, margin = margin(t=20, r=100)))+
  labs(title="Raw temperature data", y="Temperature (?C)", x="Date")
tempgraph

lightgraph <- ggplot(data=dfnew, 
                     aes(x=as.Date(Date, format = "%Y-%m-%d"), 
                         y=Lux)) +
  geom_point(size=1, alpha = 1/10)+ theme_bw()+
  theme(axis.text.x = element_text(angle=45, margin = margin(t=20, r=100)))+
  labs(title="Raw light data", y="Light (Lux)", x="Date")
lightgraph

#Now we have a curated database. But we can do better.
df2 <- dfnew %>%
  tidyr::separate('Date',
                  into = c('Year', 'Month', 'Day'),
                  sep= '-',
                  remove = FALSE)
head(df2)

#We made new columns for day, month, and year!
#Year and month are not particularly helpful for us,
#But those are the important scales for most ecological studies

#Now make summaries
dfsummaries <- df2 %>%
  group_by(Year, Month, Day, Date)%>%
  summarise(meantemp = mean(Temp), meanlux = mean(Lux), mintemp = min(Temp), maxtemp = max(Temp), 
            minlux = min(Lux), maxlux = max(Lux))%>%
  mutate(temprange = maxtemp-mintemp)%>%
  mutate(luxrange = maxlux-minlux)

head(dfsummaries)
#View(hobofullmean)

tempplot <- ggplot(dfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y=meantemp))+
  geom_smooth(aes(), color = "black")+
  geom_smooth(aes(y=mintemp), color = "blue")+
  geom_smooth(aes(y = maxtemp), color = "red")+
  theme_bw()+
  labs(title= "Daily temperature min, mean, max", y="Daily temperature summaries (?C) with 95% CI", x="Date")
tempplot

lightplot <- ggplot(dfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y=meanlux))+
  geom_smooth(aes(), color = "black")+
  geom_smooth(aes(y=minlux), color = "blue")+
  geom_smooth(aes(y = maxlux), color = "red")+
  theme_bw()+
  labs(title= "Daily light min, mean, max", y="Daily temperature summaries (Lux) with 95% CI", x="Date")
lightplot

library(gridExtra)
grid.arrange(tempplot, lightplot)

df2$DateFactor <- as.factor(df2$Date)

#Test for significance between zones, month, LTER sites
hobomeanmonthaov <- aov(Temp~DateFactor, data=df2)
summary(hobomeanmonthaov)
#comb through the pairwise comparisons to find when environment changes (If you want)

#Post-hoc tests
HSD <- TukeyHSD(hobomeanmonthaov, ordered=FALSE)
HSD

library(googlesheets4)

T3Measurements <- read_sheet("https://docs.google.com/spreadsheets/d/13QEOOUIk2I06zcf94OYqyLR8fs-WWBOs1Edld-Lifao/edit#gid=0")
#rebeccaxsalas@gmail.com
head(T3Measurements)
#Just view the first few lines
head(T3Measurements)

library(naniar)
T3Measurements <- subset(T3Measurements, Diam != c(0))
T3Measurements <- subset(T3Measurements, Pulses != c(0))
head(T3Measurements)
Experiment <- T3Measurements


head(Experiment)

Sp <- subset(Experiment, Species == "C. sp.")
View(Sp)

Sp2 <- Sp[Sp$Date >= "2022-06-07",]
head(Sp2)

spjoined <- left_join(dfsummaries, Sp2, by=c("Date"))
view(spjoined)

#Pearson Correlation Matrix
library(rstatix)

#Temp vs Light
model0 <- lm(maxtemp ~ maxlux, data = spjoined)
summary(model0)

#Pulse vs Env
model1 <- lm(Pulses ~ maxtemp, data = spjoined)
model1
summary(model1)
#Take note of the p value and the Adjusted R-squared value
library(ggplot2)
library(ggpubr)
maxtemppulses <- ggplot(spjoined, aes(maxtemp, Pulses))+
  geom_abline (aes(intercept = -241.861 , slope = 9.687), size = 2.0, alpha = 0.6)+
  geom_point()+
  theme_classic2()

model2 <- lm(Pulses ~ maxlux, data = spjoined)
model2
summary(model2)
#Take note of the p value and the Adjusted R-squared value (You will need to present those to support your claims)

maxluxpulses <- ggplot(spjoined, aes(maxlux, Pulses))+
  geom_abline (aes(intercept = 5.033e+01 , slope = 3.574e-04), size = 2.0, alpha = 0.6)+
  geom_point()+
  theme_classic2()

#Diam vs Env
model3 <- lm(Diam ~ maxlux, data = spjoined)
model3
summary(model3)

#Take note of the p value and the Adjusted R-squared value
maxtempdiam <- ggplot(spjoined, aes(maxtemp, Diam))+
  geom_abline (aes(intercept = 95.327 , slope = -1.291), size = 2.0, alpha = 0.6)+
  geom_point()+
  theme_classic2()

model4 <- lm(Diam ~ maxlux, data = spjoined)
model4
summary(model4)
#Take note of the p value and the Adjusted R-squared value

maxluxdiam <- ggplot(spjoined, aes(maxtemp, Diam))+
  geom_abline (aes(intercept = 5.775e+01 , slope = -7.308e-05), size = 2.0, alpha = 0.6)+
  geom_point()+
  theme_classic2()

grid.arrange(maxluxpulses, maxtemppulses, maxluxdiam, maxtempdiam, nrow=2, top = "Cassiopea Sp. Linear Correlation Analyses")

coeff <- 1000
dfsummaries$Date <- as.POSIXct(dfsummaries$Date)

#Color vs Env
cor.test(spjoined$ColorRate, spjoined$maxlux, method = "spearman")
cor.test(spjoined$ColorRate, spjoined$maxtemp, method = "spearman")

