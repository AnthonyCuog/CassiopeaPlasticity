####BIOTIC ANALYSIS####
#MUST RUN ABIOTIC ANALYSIS FIRTST
library(googlesheets4)
Sp.Experiment <- read_sheet("https://docs.google.com/spreadsheets/d/1Z6HIrSbd5kzbBBzcNf0VB0IdR5475ahAwQrHYZhS8b4/edit#gid=0")
#rebeccaxsalas@gmail.com
#Just view the first few lines
View(Sp.Experiment)

#Rename to make things easy
cassiosp <- Sp.Experiment

####PLOTS WITH DYNAMICS####
#NEEDS STATISTICAL GROUPS AND ENVIRONMENTAL DATA
library(ggplot2)

cassiosp <- tidyr::separate(cassiosp, Date, c('year', 'month', 'day'), sep = "-",remove = FALSE)
head(cassiosp)

cassiosp <- tidyr::separate(cassiosp, day, c('day', 'time'), sep = " ",remove = FALSE)
head(cassiosp)

cassiosp <- tidyr::separate(cassiosp, time, c('time'), sep = ":",remove = FALSE)
head(cassiosp)

cassiosp$monthdaycon <- paste(cassiosp$month,cassiosp$day,cassiosp$Condition, sep="x")
head(cassiosp)

cassiosp$monthdaytimecon <- paste(cassiosp$month,cassiosp$day,cassiosp$time, cassiosp$Condition, sep="x")
head(cassiosp)

cassiosp$Date <- as.Date(cassiosp$Date)
head(cassiosp)
spcontroldfsummaries <- spcontroldfsummaries [spcontroldfsummaries$Date >= "2022-08-21", ]
head(spcontroldfsummaries)
#Diameter

####Visualization and Analysis Exp 2####
#Three-week acclimation assay
#Diameter
Diameter <- ggplot(cassiosp, aes(Date, Diam))+
  geom_bar(data = spcontroldfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y = maxlux/4000+40), stat = "identity", color="lightgrey", fill="lightgrey", width = 1) +
  geom_bar(data = spcontroldfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y = meanlux/4000+40), stat = "identity", color="darkgrey", fill="darkgrey", width = 1) +
  geom_boxplot(aes(fill = Environment, group = Date))+
  scale_fill_manual(breaks = c("Ambient", "Shaded"), values=c("coral2", "darkcyan"))+
  facet_grid(cols = vars(c(Condition)))+
  scale_y_continuous("Diameter (mm)", 
                     sec.axis = sec_axis(~ . *4000-160000, name = "Light (Lux)")
  )+
  coord_cartesian(ylim = c(40,NA),)+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())
Diameter

#install.packages("FSA")
library(FSA)
#install.packages("rcompanion")
library(rcompanion)

kruskal.test(Diam ~ monthdaycon, data = cassiosp)
#Kruskal-Wallis chi-squared = 97.733, df = 8, p-value = 0.4089

spDiam <- dunnTest(Diam ~ monthdaycon, data = cassiosp, method = "bh")
head(spDiam)
spDiam = spDiam$res
head(spDiam)
cldoutput <- cldList(comparison = spDiam$Comparison, p.value = spDiam$P.adj, threshold = 0.05)
head(cldoutput)
cldDiam <- as.data.frame(cldoutput)

cassiosp <- as.data.frame(cassiosp)

cassiosp2 <- cassiosp %>% mutate(monthdaycon = as.character(gsub("0", "", monthdaycon)))
head(cassiosp2)
unique(cassiosp2$month)

library(data.table)
head(cldoutput)
wombogrid <- tidyr::separate(cldoutput, MonoLetter, c(' ', 'a', 'b', 'c','d', 'e', 'f','g', 'h'), 
                             sep = "",remove = FALSE)
head(wombogrid)

long <- melt(setDT(wombogrid), id.vars = c("Group","Letter", "MonoLetter"), variable.name = "Pulse.MonoLetter")
head(long)
names(long)[names(long) == 'Group'] <- 'monthdaycon'
long2<-merge(cassiosp2, long, by=c("monthdaycon"))
head(cassiosp2)
head(long2)
long2 <- with(long2, long2[!(value == " " | is.na(value)), ])
long2 <- with(long2, long2[!(value == "" | is.na(value)), ])
head(long2)

Diamstatgroup <- ggplot(long2)+
  geom_tile(aes(Date, value, fill = Environment), color = "white")+
  scale_fill_manual(values = c("coral2", "darkcyan"))+
  xlab("")+
  ylab("Statistical Group")+
  facet_grid(cols = vars(c(Condition)))+
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

#Pulses
pulsemean <- cassiosp %>% dplyr::group_by(Date, time, Condition) %>%
  dplyr::summarize(Pulses = mean(Pulses))
head(pulsemean)

Pulses <- ggplot(cassiosp, aes(Date, Pulses))+
  geom_bar(data = spcontroldfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y = maxlux/3000), stat = "identity", color="lightgrey", fill="lightgrey", width = 1) +
  geom_bar(data = spcontroldfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y = meanlux/3000), stat = "identity", color="darkgrey", fill="darkgrey", width = 1) +
  #geom_line(data = pulsemean, aes(Date, Pulses, group = time), linewidth = 1.3)+
  geom_boxplot(aes(fill = interaction(Environment, time), color = interaction(Environment, time), group = interaction(Date, time)))+
  scale_fill_manual(values=c("coral2","darkcyan", "black", "black"))+
  scale_color_manual(values=c("black","black", "coral2", "darkcyan"))+
  facet_grid(cols = vars(c(Condition)))+
  scale_y_continuous("Pulses / min", 
                     sec.axis = sec_axis(~ . *3000, name = "Light (Lux)")
  )+
  coord_cartesian(ylim = c(NA,NA),)+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())
Pulses

kruskal.test(Pulses ~ monthdaytimecon, data = cassiosp)
#Kruskal-Wallis chi-squared = 136.42, df = 53, p-value = 2.76e-09
unique(cassiosp$monthdaytimecon)
spPulse <- dunnTest(Pulses ~ monthdaytimecon, data = cassiosp, method = "bh")
head(spPulse)
spPulse = spPulse$res
head(spPulse)
cldoutput <- cldList(comparison = spPulse$Comparison, p.value = spPulse$P.adj, threshold = 0.05)
head(cldoutput)
cldPulse <- as.data.frame(cldoutput)

cassiosp <- as.data.frame(cassiosp)
head(cassiosp)

cassiosp2 <- cassiosp %>% mutate(monthdaytimecon = as.character(gsub("0", "", monthdaytimecon)))
head(cassiosp2)
unique(cassiosp2$monthdaytimecon)

library(data.table)
head(cldoutput)
wombogrid <- tidyr::separate(cldoutput, MonoLetter, c(' ', 'a', 'b', 'c','d', 'e', 'f','g', 'h', 'i'), 
                             sep = "",remove = FALSE)
head(wombogrid)

long <- melt(setDT(wombogrid), id.vars = c("Group","Letter", "MonoLetter"), variable.name = "Pulse.MonoLetter")
head(long)
names(long)[names(long) == 'Group'] <- 'monthdaytimecon'
long2<-merge(cassiosp2, long, by=c("monthdaytimecon"))
head(cassiosp2)
head(long2)
long2 <- with(long2, long2[!(value == " " | is.na(value)), ])
long2 <- with(long2, long2[!(value == "" | is.na(value)), ])
head(long2)

Pulsestatgroup <- ggplot(long2)+
  geom_tile(aes(interaction(time,Date), value, fill = interaction(Environment, time), color = interaction(Environment, time), group = interaction(Date, time)))+
  scale_fill_manual(values=c("coral2","darkcyan", "black", "black"))+
  scale_color_manual(values=c("black", "black", "coral2", "darkcyan"))+
  xlab("")+
  ylab("Statistical Group")+
  facet_grid(cols = vars(c(Condition)))+
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
ArrangePulse
grid.arrange(ArrangeDiam, ArrangePulse)

#Color
cassiospco <- subset(cassiosp, time == 13)
unique(cassiospco$time)
cassiospco$ColorRating2 <- as.numeric(cassiospco$ColorRating)
head(cassiospco)
#Paleing
Mode <- function(x) {
  ux <- unique(x)
  if(!anyDuplicated(x)){
    NA_character_ } else { 
      tbl <-   tabulate(match(x, ux))
      toString(ux[tbl==max(tbl)])
    }
}
class(cassiospco$ColorRating)
mode <- cassiospco %>% dplyr::group_by(Date, Condition) %>%
  dplyr::summarize(Color = Mode(ColorRating))
head(mode)
mode$Color <- as.numeric(mode$Color)
head(mode)

Category <- ggplot()+
  geom_bar(data = spcontroldfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y = maxlux/38000), stat = "identity", color="lightgrey", fill="lightgrey", width = 1) +
  geom_bar(data = spcontroldfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y = meanlux/38000), stat = "identity", color="darkgrey", fill="darkgrey", width = 1) +
  geom_tile(data = cassiospco, aes(Date, ColorRating,fill = Environment, group = Date), alpha=0.3)+
  geom_line(data = mode, aes(Date, Color), color = "gray", size=3)+
  scale_fill_manual(breaks = c("Ambient", "Shaded"), values=c("coral2", "darkcyan"))+
  scale_y_continuous("Color Shade Category",
                     sec.axis = sec_axis(~ . *38000, name = "Light (Lux)")
  )+
  theme_classic()+
  facet_grid(cols = vars(c(Condition)))+
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())
Category

kruskal.test(ColorRating ~ monthdaycon, data = cassiospco)
#Kruskal-Wallis chi-squared = 102.4, df = 26, p-value = 5.097e-11

spCat <- dunnTest(ColorRating ~ monthdaycon, data = cassiospco, method = "bh")
head(spCat)
spCat = spCat$res
head(spCat)
cldoutput <- cldList(comparison = spCat$Comparison, p.value = spCat$P.adj, threshold = 0.05)
head(cldoutput)
cldCat <- as.data.frame(cldoutput)

cassiospco <- as.data.frame(cassiospco)

cassiosp2 <- cassiosp %>% mutate(monthdaycon = as.character(gsub("0", "", monthdaycon)))
head(cassiosp2)
unique(cassiosp2$monthdaycon)

head(cldoutput)
wombogrid <- tidyr::separate(cldoutput, MonoLetter, c(' ', 'a', 'b', 'c','d'), 
                             sep = "",remove = FALSE)
head(wombogrid)

long <- melt(setDT(wombogrid), id.vars = c("Group","Letter", "MonoLetter"), variable.name = "Cat.MonoLetter")
head(long)
names(long)[names(long) == 'Group'] <- 'monthdaycon'
long2<-merge(cassiosp2, long, by=c("monthdaycon"))
head(cassiosp2)
head(long2)
long2 <- with(long2, long2[!(value == " " | is.na(value)), ])
long2 <- with(long2, long2[!(value == "" | is.na(value)), ])
head(long2)

Catstatgroup <- ggplot(long2)+
  geom_tile(aes(Date, value, fill = Environment), color = "white")+
  scale_fill_manual(values = c("coral2", "darkcyan"))+
  xlab("")+
  ylab("Statistical Group")+
  facet_grid(cols = vars(c(Condition)))+
  theme_classic()+
  theme(legend.position = "none")
Catstatgroup

C1 <- Category+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(), 
        strip.text.x = element_blank(),
        axis.text.y = element_blank())
C1
C2 <- Catstatgroup+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(), 
        strip.text.x = element_blank(),
        axis.text.y = element_blank())
C2

ArrangeCat <- grid.arrange(C1, C2, heights = c(4,1))
ArrangeCat

spcontrolYII <- ggplot(cassiosp, aes(Date, YII))+
  geom_smooth(color = "gray", span=0.7, se=F, size=3)+
  geom_boxplot(aes(fill = Environment, group = Date), alpha=0.7)+
  scale_fill_manual(breaks = c("Light", "Dark"), values=c("coral2", "darkcyan"))+
  facet_grid(cols = vars(c(Condition)))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())
spcontrolYII

#Diameter
YII <- ggplot(cassiosp, aes(Date, YII))+
  geom_bar(data = spcontroldfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y = maxlux/450000+0.4), stat = "identity", color="lightgrey", fill="lightgrey", width = 1) +
  geom_bar(data = spcontroldfsummaries, aes(x=as.Date(Date, format = "%Y-%m-%d"), y = meanlux/450000+0.4), stat = "identity", color="darkgrey", fill="darkgrey", width = 1) +
  geom_boxplot(aes(fill = Environment, group = Date))+
  scale_fill_manual(breaks = c("Ambient", "Shaded"), values=c("coral2", "darkcyan"))+
  facet_grid(cols = vars(c(Condition)))+
  scale_y_continuous("Diameter (mm)", 
                     sec.axis = sec_axis(~ . *450000-180000, name = "Light (Lux)")
  )+
  coord_cartesian(ylim = c(0.4,1.0),)+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())
YII

kruskal.test(YII ~ monthdaycon, data = cassiosp)
#Kruskal-Wallis chi-squared = 46.841, df = 26, p-value = 0.007335

spYII <- dunnTest(YII ~ monthdaycon, data = cassiosp, method = "bh")
head(spYII)
spYII = spYII$res
head(spYII)
cldoutput <- cldList(comparison = spYII$Comparison, p.value = spYII$P.adj, threshold = 0.05)
head(cldoutput)
cldYII <- as.data.frame(cldoutput)

cassiosp <- as.data.frame(cassiosp)

cassiosp2 <- cassiosp %>% mutate(monthdaycon = as.character(gsub("0", "", monthdaycon)))
head(cassiosp2)
unique(cassiosp2$monthdaycon)

library(data.table)
head(cldoutput)
unique(cldoutput$Letter)
wombogrid <- tidyr::separate(cldoutput, MonoLetter, c(' ', 'a'), 
                             sep = "",remove = FALSE)
head(wombogrid)

long <- melt(setDT(wombogrid), id.vars = c("Group","Letter", "MonoLetter"), variable.name = "YII.MonoLetter")
head(long)
names(long)[names(long) == 'Group'] <- 'monthdaycon'
long2<-merge(cassiosp2, long, by=c("monthdaycon"))
head(cassiosp2)
head(long2)
long2 <- with(long2, long2[!(value == " " | is.na(value)), ])
long2 <- with(long2, long2[!(value == "" | is.na(value)), ])
head(long2)

YIIstatgroup <- ggplot(long2)+
  geom_tile(aes(Date, value, fill = Environment), color = "white")+
  scale_fill_manual(values = c("coral2", "darkcyan"))+
  xlab("")+
  ylab("Statistical Group")+
  facet_grid(cols = vars(c(Condition)))+
  theme_classic()+
  theme(legend.position = "none")
YIIstatgroup

Y1 <- YII+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(), 
        strip.text.x = element_blank(),
        axis.text.y = element_blank())
Y1
Y2 <- YIIstatgroup+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(), 
        strip.text.x = element_blank(),
        axis.text.y = element_blank())
Y2

ArrangeYII <- grid.arrange(Y1, Y2, heights = c(4,1))
ArrangeYII

grid.arrange(ArrangeDiam, ArrangePulse, ArrangeCat, ArrangeYII, ncol=2)
grid.arrange(ArrangeDiam, ArrangePulse, ArrangeCat, ArrangeYII, ncol=1)

