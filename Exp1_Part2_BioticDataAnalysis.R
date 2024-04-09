#Import your dataset
#Make sure you change your file path to fit the location on your current computer
#library(readxl)
#T3Measurements <- read_excel("https://docs.google.com/spreadsheets/d/13QEOOUIk2I06zcf94OYqyLR8fs-WWBOs1Edld-Lifao/edit#gid=0") 

#install.packages("googlesheets4")
library(googlesheets4)
T3Measurements <- read_sheet("https://docs.google.com/spreadsheets/d/13QEOOUIk2I06zcf94OYqyLR8fs-WWBOs1Edld-Lifao/edit#gid=0")
anthonyc@gsuite.uog.edu
#Just view the first few lines
head(T3Measurements)
#View the whole database
View(T3Measurements)

#Rename to make things easy
cassiodf <- T3Measurements
####Preliminary data cleaning####
library(ggplot2)
ggplot(cassiodf, aes(Date, Diam, group = ID, color = Species))+
  geom_line()

#Looks like there is a typo mistake in your data, let's find that
#install.packages("dplyr")
library(dplyr)
cassiodf[which.max(cassiodf$Diam),]
##July 8 sp. 8 has wrong diameter. Everything else seems fine though

##Let's look at the whole replicate
#cassiodf %>% filter_all(any_vars(ID == "sp.8"))

##I'm curious if you just added an extra number on accident
#For now I am going to replace it with an "N/A", but it should be checked on the original data sheets eventually
#install.packages("naniar")
library(naniar)
#cassiodf <- replace_with_na(cassiodf, replace = list(Diam = c(662)))

#There also seems to be an issue with misidentification in the larger ones, so I think it's okay to treat them as random variation right now
#But if you get the chance, you should go back through and try to fix the IDs based on what you would expect the growth measurement to be

#Now I am going to remove your 0's from the data
cassiodf %>% filter_all(any_vars(Diam == c(0)))

#Actually, I am just going to remove the whole sp. 14 replicate

cassiodf <- subset(cassiodf, ID != "sp.14")

#Let's check to make sure it is gone
cassiodf %>% filter_all(any_vars(Diam == c(0)))
#It is!

#let's check now
#install.packages("ggplot")
library(ggplot2)
ggplot(cassiodf, aes(Date, Diam, group = ID, color = Species))+
  geom_line()

#it looks like one individual may have been left out and one was double counted, thst may skew our mean
#I don't know what to do about it right now, but you may end up having to check images to verify some measurement.
#May take some curation and backtracking, but that's we build in modes of insurance

#Since the repeated measures method is a bit off, we need to use a box plot now. Then we can plot species specific trend lines on top
head(cassiodf)
sp <- subset(cassiodf, Species == "C. sp.")
class(sp$Date)
sp$Date <- as.Date(sp$Date, "%y-%m-%d")

sp <- subset(sp, Date > as.Date("2022-06-06"))
head(sp)

####Test for normality####
#Test for normality
# Diameter
shapiro.test(sp$Diam) # W = 0.9, p-value = 0.01887
ggqqplot(sp$Diam, ylab = "Diameter (mm)")

# Pulses
shapiro.test(sp$Pulses) # W = 0.98222, p-value = 0.0001349
ggqqplot(sp$Pulses, ylab = "Pulsations/min")

# Color
#Semi-Quanitative Data does not need to be tested for normality

####Light and temperature####
library(tidyverse)
library(cowplot)
library(readr)
library(ggplot2)
library(lubridate)
library(readxl)
library(dplyr)
library(tibble)
library (ggpubr)

hobologger <- read.csv("D:/JExpMarBiolEcol_Salas_Cassiopea//Loggers/Exp1/Cassio_sp.csv")
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

dftrimmed <- df[df$Date >= "2022-06-07" & df$Date <= "2022-08-14", ]
head(dftrimmed)

ntempgraph <- ggplot(data=dftrimmed, 
                     aes(x=as.Date(Date, format = "%Y-%m-%d"), 
                         y=Temp)) +
  geom_point(size=1, alpha = 1/10)+ theme_bw()+
  theme(axis.text.x = element_text(angle=45, margin = margin(t=20, r=100)))+
  labs(title="Raw temperature data", y="Temperature (?C)", x="Date")
ntempgraph

#Still need to trim some time off

View(dftrimmed)

#The temperature becomes the water temperature at row 2559
colnames(dftrimmed)


dftrimmed <- rename(dftrimmed, Row = "X.")
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

tempplot <- ggplot(dfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d")))+
  geom_bar(mapping = aes(y = maxlux*11), stat = "identity", color="lightgrey", fill="lightgrey", width = 1) +
  geom_smooth(aes(y=meantemp^4), size = 3, color="#A9DBC7", fill = "#A9DBC7")+
  geom_smooth(aes(y=maxtemp^4), size = 3, color="#89C5D3", fill ="#89C5D3")+
  geom_smooth(aes(y=mintemp^4), size = 3, color="#8C9DCF", fill ="#8C9DCF")+
  theme_classic()+
  scale_y_continuous("Temperature [°C]",
                     sec.axis = sec_axis(~ . *3000, name = "Light (Lux)")
  )+
  labs(title= "Daily temperature min, mean, max", y="Daily temperature summaries (?C) with 95% CI", x="Date")
tempplot
####Visualization and Analysis Exp 1####
Diameter <- ggplot(sp, aes(Date, Diam))+
  geom_bar(data = dfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y = maxlux/3000), stat = "identity", color="lightgrey", fill="lightgrey", width = 1) +
  geom_bar(data = dfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y = meanlux/3000), stat = "identity", color="darkgrey", fill="darkgrey", width = 1) +
  geom_boxplot(aes(fill = Light, group = Date))+
  scale_fill_manual(breaks = c("Light", "Dark"), values=c("coral2", "darkcyan"))+
  scale_y_continuous("Diameter (mm)",
                     sec.axis = sec_axis(~ . *3000, name = "Light (Lux)")
  )+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())
Diameter

#install.packages("FSA")
library(FSA)
#install.packages("rcompanion")
library(rcompanion)
sp <- tidyr::separate(sp, Date, c('year', 'month', 'day'), sep = "-",remove = FALSE)
sp$monthday <- paste(sp$month,sp$day,sep="x")
head(sp)
sp$monthday <- replace(sp$monthday, sp$monthday=="08x01", "08xx1")
unique(sp$monthday)
kruskal.test(Diam ~ monthday, data = sp)
#Kruskal-Wallis chi-squared = 221.88, df = 28, p-value < 2.2e-16

spDiam <- dunnTest(Diam ~ monthday, data = sp, method = "bh")
head(spDiam)
spDiam = spDiam$res
head(spDiam)
cldoutput <- cldList(comparison = spDiam$Comparison, p.value = spDiam$P.adj, threshold = 0.05)
head(cldoutput)
cldDiam <- as.data.frame(cldoutput)
#head(cldDiam)
#class(cldDiam)
sp <- as.data.frame(sp)
head(sp)

sp2 <- sp %>% mutate(monthday = as.character(gsub("0", "", monthday)))
head(sp2)
unique(sp2$monthday)

library(data.table)
head(cldoutput)
wombogrid <- tidyr::separate(cldoutput, MonoLetter, c(' ', 'a', 'b', 'c','d', 'e', 'f','g', 'h', 'i', 'j', 'k'), 
                             sep = "",remove = FALSE)
head(wombogrid)

long <- melt(setDT(wombogrid), id.vars = c("Group","Letter", "MonoLetter"), variable.name = "Pulse.MonoLetter")

names(long)[names(long) == 'Group'] <- 'monthday'
long2<-merge(sp2, long, by=c("monthday"))
head(long2)
long2 <- with(long2, long2[!(value == " " | is.na(value)), ])
long2 <- with(long2, long2[!(value == "" | is.na(value)), ])
head(long2)

Diamstatgroup <- ggplot(long2)+
  geom_tile(aes(Date, value, fill = Light), color = "white")+
  scale_fill_manual(values = c("darkcyan", "coral2"))+
  xlab("")+
  ylab("Statistical Group")+
  theme_classic()+
  theme(legend.position = "none")
Diamstatgroup

D1 <- Diameter+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(), 
        strip.text.x = element_blank(),
        axis.text.y = element_blank())
D1
D2 <- Diamstatgroup+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(), 
        strip.text.x = element_blank(),
        axis.text.y = element_blank())
D2

ArrangeDiam <- grid.arrange(D1, D2, heights = c(4,1))
ArrangeDiam
  
#looks like the small ones are growing and big ones arent, but will need a regression to confirm
  ##Learn how to plot a linear regression to determine their growth rate
#Now let's look at pulsation
Pulses <- ggplot(sp, aes(Date, Pulses))+
  geom_bar(data = dfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y = maxlux/2500), stat = "identity", color="lightgrey", fill="lightgrey", width = 1) +
  geom_bar(data = dfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y = meanlux/2500), stat = "identity", color="darkgrey", fill="darkgrey", width = 1) +
  geom_boxplot(aes(fill = Light, group = Date))+
  scale_fill_manual(breaks = c("Light", "Dark"), values=c("coral2", "darkcyan"))+
  scale_y_continuous("Pulses (#/min)",
                     sec.axis = sec_axis(~ . *2500, name = "Light (Lux)")
  )+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())
Pulses

sp <- tidyr::separate(sp, Date, c('year', 'month', 'day'), sep = "-",remove = FALSE)
sp$monthday <- paste(sp$month,sp$day,sep="x")
head(sp)
sp$monthday <- replace(sp$monthday, sp$monthday=="08x01", "08xx1")
unique(sp$monthday)
kruskal.test(Pulses ~ monthday, data = sp)

spPulse <- dunnTest(Pulses ~ monthday, data = sp, method = "bh")
head(spPulse)
spPulse = spPulse$res
head(spPulse)
cldoutput <- cldList(comparison = spPulse$Comparison, p.value = spPulse$P.adj, threshold = 0.05)
sp <- as.data.frame(sp)
head(sp)

sp2 <- sp %>% mutate(monthday = as.character(gsub("0", "", monthday)))
head(sp2)
unique(sp2$monthday)

head(cldoutput)
wombogrid <- tidyr::separate(cldoutput, MonoLetter, c(' ', 'a', 'b', 'c','d', 'e', 'f','g', 'h', 'i', 'j', 'k', 'l', 'm'), 
                      sep = "",remove = FALSE)
head(wombogrid)

long <- melt(setDT(wombogrid), id.vars = c("Group","Letter", "MonoLetter"), variable.name = "Pulse.MonoLetter")

names(long)[names(long) == 'Group'] <- 'monthday'
long2<-merge(sp2, long, by=c("monthday"))
head(long2)

long2 <- with(long2, long2[!(value == " " | is.na(value)), ])
long2 <- with(long2, long2[!(value == "" | is.na(value)), ])
head(long2)
Pulsestatgroup <- ggplot(long2)+
  geom_tile(aes(Date, value, fill = Light), color = "white")+
  scale_fill_manual(values = c("darkcyan", "coral2"))+
  xlab("")+
  ylab("Statistical Group")+
  theme_classic()+
  theme(legend.position = "none")
Pulsestatgroup

P1 <- Pulses+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(), 
        strip.text.x = element_blank(),
        axis.text.y = element_blank())
P1
P2 <- Pulsestatgroup+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(), 
        strip.text.x = element_blank(),
        axis.text.y = element_blank())
P2

ArrangePulse <- grid.arrange(P1, P2, heights = c(4,1))
grid.arrange(ArrangeDiam, ArrangePulse)

#Paleing
Mode <- function(x) {
  ux <- unique(x)
  if(!anyDuplicated(x)){
    NA_character_ } else { 
      tbl <-   tabulate(match(x, ux))
      toString(ux[tbl==max(tbl)])
    }
}

mode <- sp %>% group_by(Date, Light) %>%
  summarize(Color = Mode(ColorRate))
head(mode)
mode$Color <- as.numeric(mode$Color)
Category <- ggplot()+
  geom_bar(data = dfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y = maxlux/38000), stat = "identity", color="lightgrey", fill="lightgrey", width = 1) +
  geom_bar(data = dfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y = meanlux/38000), stat = "identity", color="darkgrey", fill="darkgrey", width = 1) +
  geom_tile(data = sp, aes(Date, ColorRate,fill = Light, group = Date), alpha=0.2)+
  geom_line(data = mode, aes(Date, Color), color = "gray", size=3)+
  scale_fill_manual(breaks = c("Light", "Dark"), values=c("coral2", "darkcyan"))+
  scale_y_continuous("Color Shade Category",
                     sec.axis = sec_axis(~ . *38000, name = "Light (Lux)")
  )+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())
Category

sp <- tidyr::separate(sp, Date, c('year', 'month', 'day'), sep = "-",remove = FALSE)
sp$monthday <- paste(sp$month,sp$day,sep="x")
head(sp)
sp$monthday <- replace(sp$monthday, sp$monthday=="08x01", "08xx1")
unique(sp$monthday)
kruskal.test(ColorRate ~ monthday, data = sp)
#Kruskal-Wallis chi-squared = 189.48, df = 28, p-value < 2.2e-16

spColor <- dunnTest(ColorRate ~ monthday, data = sp, method = "bh")
head(spColor)
spColor = spColor$res
head(spColor)
cldoutput <- cldList(comparison = spColor$Comparison, p.value = spColor$P.adj, threshold = 0.05)
sp <- as.data.frame(sp)
head(sp)

sp2 <- sp %>% mutate(monthday = as.character(gsub("0", "", monthday)))
head(sp2)
unique(sp2$monthday)

head(cldoutput)
wombogrid <- tidyr::separate(cldoutput, MonoLetter, c(' ', 'a', 'b', 'c','d', 'e', 'f','g', 'h', 'i'), 
                             sep = "",remove = FALSE)
head(wombogrid)

long <- melt(setDT(wombogrid), id.vars = c("Group","Letter", "MonoLetter"), variable.name = "Color.MonoLetter")

names(long)[names(long) == 'Group'] <- 'monthday'
long2<-merge(sp2, long, by=c("monthday"))
head(long2)

long2 <- with(long2, long2[!(value == " " | is.na(value)), ])
long2 <- with(long2, long2[!(value == "" | is.na(value)), ])
head(long2)
Colorstatgroup <- ggplot(long2)+
  geom_tile(aes(Date, value, fill = Light), color = "white")+
  scale_fill_manual(values = c("darkcyan", "coral2"))+
  xlab("")+
  ylab("Statistical Group")+
  theme_classic()+
  theme(legend.position = "none")
Colorstatgroup

C1 <- Category+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(), 
        strip.text.x = element_blank(),
        axis.text.y = element_blank())
C1
C2 <- Colorstatgroup+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(), 
        strip.text.x = element_blank(),
        axis.text.y = element_blank())
C2

ArrangeColor <- grid.arrange(C1, C2, heights = c(4,1))

grid.arrange(ArrangeDiam, ArrangePulse, ArrangeColor)
