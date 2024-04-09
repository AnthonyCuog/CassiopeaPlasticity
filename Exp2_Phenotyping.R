library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggpubr)
library(cowplot)

(files <- fs::dir_ls("D:/JExpMarBiolEcol_Salas_Cassiopea/CassioListMode/", 
                     glob="*.CSV"))
df <- read_csv(files, id="path")
head(df)

names(df) <- gsub("-", ".", names(df), fixed=TRUE)

df$RED.B.HLog <- as.numeric(df$RED.B.HLog)

ggdensity(df, x = "RED.B.HLog", fill = "lightgray", rug = FALSE)+
  scale_x_continuous(limits = c(1.5, 5))
head(df)
DensityY <- density(df$RED.B.HLog)$y
DensityX <- density(df$RED.B.HLog)$x

DensityX < 4 & DensityX > 2
MinYDensity<- min(DensityY[DensityX < 4 & DensityX > 2])
MinYDensity
#0.0033001299
which(DensityY == MinYDensity)
#329
DensityX[329]

#Visualize your threshold here
ggdensity(df, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
  scale_x_continuous(limits = c(1.5, 5))+
  geom_vline(xintercept = density(df$RED.B.HLog)$x[329])

#X Minimum = 3.04001
#3.005898 is your binning threshold. Go back in to InCyte, 
#adjust your "Symbiont" bin on the RED-B-HLog histogram appropriately if

#If desired, subset your symbionts from the noise, and use this data to plot
#fluorescent signatures for each seperately counted cell 

dfsym <- subset(df, RED.B.HLog>=3.04001)
dfsym <- subset(dfsym, SSC.HLog>=1)
head(dfsym)
nrow(dfsym)
ggdensity(dfsym, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
  #scale_y_continuous(limits = c(0,0.3))+
  scale_x_continuous(limits = c(1.5, 5))+
  geom_vline(xintercept = density(df$RED.B.HLog)$x[329])

#you will need to map metadata to this dataset in most cases
#You are not able to get a cells/mL concentration with this dataset
#In most cases, it is easier to reaccess InCyte with your new binning threshold
#And export summarized data from InCyte as a .csv file 
class(dfsym$path)
head(dfsym$path)
df2<-tidyr::separate(dfsym, 'path', into = c(NA, NA,NA,NA,NA,NA,'well',NA))
unique(df2$well)
class(df2$well)
dat4<-df2

dat4$condition<-replace(dat4$well, dat4$well == "A01", "Variable")
dat4$condition<-replace(dat4$condition, dat4$condition == "A02", "Variable")
dat4$condition<-replace(dat4$condition, dat4$condition == "A03", "Variable")
dat4$condition<-replace(dat4$condition, dat4$condition == "A04", "Variable")
dat4$condition<-replace(dat4$condition, dat4$condition == "A05", "Variable")
dat4$condition<-replace(dat4$condition, dat4$condition == "A06", "Variable")
dat4$condition<-replace(dat4$condition, dat4$condition == "A07", "Variable")
dat4$condition<-replace(dat4$condition, dat4$condition == "A08", "Variable")
dat4$condition<-replace(dat4$condition, dat4$condition == "A09", "Variable")
dat4$condition<-replace(dat4$condition, dat4$condition == "A10", "Variable")
dat4$condition<-replace(dat4$condition, dat4$condition == "A11", "Ambient")
dat4$condition<-replace(dat4$condition, dat4$condition == "A12", "Ambient")
dat4$condition<-replace(dat4$condition, dat4$condition == "B01", "Ambient")
dat4$condition<-replace(dat4$condition, dat4$condition == "B02", "Ambient")
dat4$condition<-replace(dat4$condition, dat4$condition == "B03", "Ambient")
dat4$condition<-replace(dat4$condition, dat4$condition == "B04", "Ambient")
dat4$condition<-replace(dat4$condition, dat4$condition == "B05", "Ambient")
dat4$condition<-replace(dat4$condition, dat4$condition == "B06", "Ambient")
dat4$condition<-replace(dat4$condition, dat4$condition == "B07", "Shaded")
dat4$condition<-replace(dat4$condition, dat4$condition == "B08", "Shaded")
dat4$condition<-replace(dat4$condition, dat4$condition == "B09", "Shaded")
dat4$condition<-replace(dat4$condition, dat4$condition == "B10", "Shaded")
dat4$condition<-replace(dat4$condition, dat4$condition == "B11", "Shaded")
dat4$condition<-replace(dat4$condition, dat4$condition == "B12", "Shaded")
dat4$condition<-replace(dat4$condition, dat4$condition == "C01", "Shaded")
dat4$condition<-replace(dat4$condition, dat4$condition == "C02", "Shaded")

dat4$individual<-replace(dat4$well, dat4$well == "A01", "1")
dat4$individual<-replace(dat4$individual, dat4$individual == "A02", "1")
dat4$individual<-replace(dat4$individual, dat4$individual == "A03", "2")
dat4$individual<-replace(dat4$individual, dat4$individual == "A04", "2")
dat4$individual<-replace(dat4$individual, dat4$individual == "A05", "3")
dat4$individual<-replace(dat4$individual, dat4$individual == "A06", "3")
dat4$individual<-replace(dat4$individual, dat4$individual == "A07", "4")
dat4$individual<-replace(dat4$individual, dat4$individual == "A08", "4")
dat4$individual<-replace(dat4$individual, dat4$individual == "A09", "5")
dat4$individual<-replace(dat4$individual, dat4$individual == "A10", "5")
dat4$individual<-replace(dat4$individual, dat4$individual == "A11", "1")
dat4$individual<-replace(dat4$individual, dat4$individual == "A12", "1")
dat4$individual<-replace(dat4$individual, dat4$individual == "B01", "2")
dat4$individual<-replace(dat4$individual, dat4$individual == "B02", "2")
dat4$individual<-replace(dat4$individual, dat4$individual == "B03", "3")
dat4$individual<-replace(dat4$individual, dat4$individual == "B04", "3")
dat4$individual<-replace(dat4$individual, dat4$individual == "B05", "4")
dat4$individual<-replace(dat4$individual, dat4$individual == "B06", "4")
dat4$individual<-replace(dat4$individual, dat4$individual == "B07", "1")
dat4$individual<-replace(dat4$individual, dat4$individual == "B08", "1")
dat4$individual<-replace(dat4$individual, dat4$individual == "B09", "2")
dat4$individual<-replace(dat4$individual, dat4$individual == "B10", "2")
dat4$individual<-replace(dat4$individual, dat4$individual == "B11", "3")
dat4$individual<-replace(dat4$individual, dat4$individual == "B12", "3")
dat4$individual<-replace(dat4$individual, dat4$individual == "C01", "4")
dat4$individual<-replace(dat4$individual, dat4$individual == "C02", "4")
head(dat4)

write.csv(dat4, "CassiopeaExperimentalDataset.csv", row.names=FALSE)

library(ggbiplot)
library(viridis)
dat4<- as.data.frame(dat4)
head(dat4)
LDCass <- dat4
head(LDCass)
CassRED <- ggplot(LDCass, aes(condition, RED.B.HLog))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_fill_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  scale_color_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
CassRED

CassGRN <- ggplot(LDCass, aes(condition, GRN.B.HLog))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_fill_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  scale_color_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
CassGRN

CassFSC <- ggplot(LDCass, aes(condition, FSC.HLog))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_fill_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  scale_color_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
CassFSC

CassSSC <- ggplot(LDCass, aes(condition, SSC.HLog))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_fill_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  scale_color_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
CassSSC

library(gridExtra)
grid.arrange(CassRED, CassGRN, CassFSC, CassSSC, ncol = 4)

library(lattice)
library(FSA)
library(rstatix)
library(car)

head(LDCass)
kruskal.test(RED.B.HLog ~ condition, data = LDCass)
# p < 0.001
kruskal.test(GRN.B.HLog ~ condition, data = LDCass)
# p < 0.001
kruskal.test(FSC.HLog ~ condition, data = LDCass)
# p < 0.001
kruskal.test(SSC.HLog ~ condition, data = LDCass)
# p < 0.001

leveneTest(RED.B.HLog ~ condition, data = LDCass)
# F = 19.782, p = 0.4711
leveneTest(GRN.B.HLog ~ condition, data = LDCass)
# F = 79.675, p < 0.001
leveneTest(FSC.HLog ~ condition, data = LDCass)
# F = 66.731, p < 0.001
leveneTest(SSC.HLog ~ condition, data = LDCass)
# F = 89.699, p < 0.001

#pairwise
dunn_test(RED.B.HLog ~ condition, data = LDCass)
#.y.        group1 group2      n1    n2 statistic        p    p.adj p.adj.signif
#* <chr>      <chr>  <chr>    <int> <int>     <dbl>    <dbl>    <dbl> <chr>       
#  1 RED.B.HLog Shaded   Ambient    16002 18226    -14.2  6.71e-46 1.34e-45 ****        
#  2 RED.B.HLog Shaded   Variable 16002 19755      2.36 1.85e- 2 1.85e- 2 *           
#  3 RED.B.HLog Ambient  Variable 18226 19755     17.4  4.04e-68 1.21e-67 **** 
dunn_test(GRN.B.HLog ~ condition, data = LDCass)
#.y.        group1 group2      n1    n2 statistic         p     p.adj p.adj.signif
#* <chr>      <chr>  <chr>    <int> <int>     <dbl>     <dbl>     <dbl> <chr>       
#  1 GRN.B.HLog Shaded   Ambient    16002 18226     18.8  8.20e- 79 1.64e- 78 ****        
#  2 GRN.B.HLog Shaded   Variable 16002 19755     21.6  3.87e-103 1.16e-102 ****        
#  3 GRN.B.HLog Ambient  Variable 18226 19755      2.51 1.22e-  2 1.22e-  2 *     
dunn_test(FSC.HLog ~ condition, data = LDCass)
#.y.      group1 group2      n1    n2 statistic        p    p.adj p.adj.signif
#* <chr>    <chr>  <chr>    <int> <int>     <dbl>    <dbl>    <dbl> <chr>       
#  1 FSC.HLog Shaded   Ambient    16002 18226    -8.93  4.35e-19 8.73e-19 ****        
#  2 FSC.HLog Shaded   Variable 16002 19755    -0.429 6.68e- 1 6.68e- 1 ns          
#  3 FSC.HLog Ambient  Variable 18226 19755     8.97  2.91e-19 8.73e-19 **** 
dunn_test(SSC.HLog ~ condition, data = LDCass)
#.y.      group1 group2      n1    n2 statistic        p    p.adj p.adj.signif
#* <chr>    <chr>  <chr>    <int> <int>     <dbl>    <dbl>    <dbl> <chr>       
#  1 SSC.HLog Shaded   Ambient    16002 18226     -4.86 1.18e- 6 2.35e- 6 ****        
#  2 SSC.HLog Shaded   Variable 16002 19755      2.28 2.26e- 2 2.26e- 2 *           
#  3 SSC.HLog Ambient  Variable 18226 19755      7.49 7.03e-14 2.11e-13 ****  

LDHost <- LDCass %>% 
  dplyr::group_by(condition, individual) %>%
  dplyr::summarize(RED.B.HLog = mean(RED.B.HLog), 
                   GRN.B.HLog = mean(GRN.B.HLog),
                   FSC.HLog = mean(FSC.HLog),
                   SSC.HLog = mean(SSC.HLog)
  )
View(LDHost)
HostRED <- ggplot(LDHost, aes(condition, RED.B.HLog))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_fill_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  scale_color_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
HostRED

HostGRN <- ggplot(LDHost, aes(condition, GRN.B.HLog))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_fill_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  scale_color_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
HostGRN

HostFSC <- ggplot(LDHost, aes(condition, FSC.HLog))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_fill_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  scale_color_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
HostFSC

HostSSC <- ggplot(LDHost, aes(condition, SSC.HLog))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_fill_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  scale_color_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
HostSSC

CassPheno <- grid.arrange(CassRED, CassGRN, CassFSC, CassSSC, HostRED, HostGRN,HostFSC,HostSSC, ncol = 4)
head(LDHost)
kruskal.test(RED.B.HLog ~ condition, data = LDHost)
#chi-squared = 3.7582, df = 2, p-value = 0.1527
kruskal.test(GRN.B.HLog ~ condition, data = LDHost)
#chi-squared = 8.7824, df = 2, p-value = 0.01239
kruskal.test(FSC.HLog ~ condition, data = LDHost)
#chi-squared = 5.5187, df = 2, p-value = 0.06333
kruskal.test(SSC.HLog ~ condition, data = LDHost)
#chi-squared = 3.5901, df = 2, p-value = 0.1661

leveneTest(RED.B.HLog ~ condition, data = LDHost)
# F = 0.4665, p = 0.6402
leveneTest(GRN.B.HLog ~ condition, data = LDHost)
# F = 0.4568, p = 0.6459
leveneTest(FSC.HLog ~ condition, data = LDHost)
# F = 0.1749, p = 0.8421
leveneTest(SSC.HLog ~ condition, data = LDHost)
# F = 10127, p = 0.3619

#pairwise comparisons
LDHost <- as.data.frame(LDHost)
dunn_test(RED.B.HLog ~ condition, data = LDHost)
# .y.        group1 group2      n1    n2 statistic      p p.adj p.adj.signif
#  * <chr>      <chr>  <chr>    <int> <int>     <dbl>  <dbl> <dbl> <chr>       
#  1 RED.B.HLog Shaded   Ambient        4     4    -1.27  0.204  0.407 ns          
#  2 RED.B.HLog Shaded   Variable     4     5     0.574 0.566  0.566 ns          
#  3 RED.B.HLog Ambient  Variable     4     5     1.91  0.0556 0.167 ns     
dunn_test(GRN.B.HLog ~ condition, data = LDHost)
#  .y.        group1 group2      n1    n2 statistic       p   p.adj p.adj.signif
#  * <chr>      <chr>  <chr>    <int> <int>     <dbl>   <dbl>   <dbl> <chr>       
#  1 GRN.B.HLog Shaded   Ambient        4     4      1.82 0.0694  0.139   ns          
#  2 GRN.B.HLog Shaded   Variable     4     5      2.95 0.00320 0.00961 **          
#  3 GRN.B.HLog Ambient  Variable     4     5      1.03 0.301   0.301   ns     
dunn_test(FSC.HLog ~ condition, data = LDHost)
#  .y.      group1 group2      n1    n2 statistic      p  p.adj p.adj.signif
#  * <chr>    <chr>  <chr>    <int> <int>     <dbl>  <dbl>  <dbl> <chr>       
#  1 FSC.HLog Shaded   Ambient        4     4    -2.18  0.0293 0.0880 ns          
#  2 FSC.HLog Shaded   Variable     4     5    -0.402 0.688  0.688  ns          
#  3 FSC.HLog Ambient  Variable     4     5     1.89  0.0581 0.116  ns 
dunn_test(SSC.HLog ~ condition, data = LDHost)
#  .y.      group1 group2      n1    n2 statistic      p p.adj p.adj.signif
#  * <chr>    <chr>  <chr>    <int> <int>     <dbl>  <dbl> <dbl> <chr>       
#  1 SSC.HLog Shaded   Ambient        4     4    -1.36  0.173  0.347 ns          
#  2 SSC.HLog Shaded   Variable     4     5     0.402 0.688  0.688 ns          
#  3 SSC.HLog Ambient  Variable     4     5     1.84  0.0662 0.198 ns  

head(LDHost)

#Combine with Part2
head(cassiosp)
unique(cassiosp$Day)

LastDay <- subset(cassiosp, Day == 21)
LastDay$individual <-  LastDay$ID
LastDay$condition <- LastDay$Condition

LastDayDay <- LastDay %>% select(condition, individual, Diam, ColorRating, Pulses)
LastDayDay<- LastDayDay %>% drop_na()
head(LastDay)
LastDayNight <- LastDay %>% select(condition, individual, YII, Pulses)
LastDayNight<- LastDayNight %>% drop_na()
LastDayNight$Pulses2 <- LastDayNight$Pulses
LastDayNight <- LastDayNight %>% select(condition, individual, YII, Pulses2)
View(LastDayNight)

HostPheno <- merge(x = LastDayDay, y = LastDayNight, by = c("condition", "individual"))

#cell density\
library(googlesheets4)
CD <- read_sheet("https://docs.google.com/spreadsheets/d/1xl_IyQbRwfXCVgPPQE2GVs42FVBMBXjjj4kEAzHL51w/edit#gid=0")
head(CD)
CD <- CD%>%select(condition, individual, CellDensity)

CD <- CD%>%
  dplyr::group_by(condition, individual) %>%
  dplyr::summarize(CellDensity = mean(CellDensity))
head(CD)  
HostPheno <- merge(x = HostPheno, y = CD, by = c("condition", "individual"))
head(HostPheno)

TotalPheno <- merge(x = HostPheno, y = LDHost, by = c("condition", "individual"))

head(TotalPheno)

theopca1 <- prcomp(TotalPheno[, c(3,4,5,6,7,8,9,10,11,12)], center = TRUE, scale. = TRUE)
summary(theopca1)
casspca <- ggbiplot(theopca1, alpha = 1, size = 2, ellipse = TRUE, groups = TotalPheno$condition)+
  scale_fill_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  scale_color_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  theme_classic2()
casspca

#Spearman correlation analysis
res <- TotalPheno[, c(3,4,5,6,7,8,9,10,11,12)]
res <- cor(res)
round(res,2)

library(Hmisc)
res2 <- rcorr(res, type = c("spearman"))
res2$r
res2$P

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
flattenCorrMatrix(res2$r, res2$P)

library(corrplot)
corrplot(res, method = "square", type = "upper", order = "hclust", 
         tl.col = "black", sig.level = 0.05, insig = "blank", tl.srt = 45)

library("PerformanceAnalytics")
my_data <- mtcars[, c(1,3,4,5,6,7)]
chart.Correlation(res, histogram=TRUE, pch=19, method = "spearman")

#rest of the boxplots
cmpr <- list(c("Ambient", "Shaded"), 
             c("Ambient", "Variable"),
             c("Shaded", "Variable"))
head(TotalPheno)
Diam <- ggplot(TotalPheno, aes(condition, Diam))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_fill_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  scale_color_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  xlab("")+
  #stat_compare_means(comparisons = cmpr, tip.length=0.01,
  #                   label = "p.signif", 
  #                   symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
  #                                      symbols = c("****", "***", "**", "*", "ns")))+
  theme_classic()+
  theme(legend.position = "none")
Diam
ColorRating <- ggplot(TotalPheno, aes(condition, ColorRating))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_fill_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  scale_color_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  xlab("")+
  #stat_compare_means(comparisons = cmpr, tip.length=0.01,
  #                   label = "p.signif", 
  #                   symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
  #                                      symbols = c("****", "***", "**", "*", "ns")))+
  theme_classic()+
  theme(legend.position = "none")
ColorRating

Pulses <- ggplot(TotalPheno, aes(condition, Pulses))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_fill_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  scale_color_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  xlab("")+
  #stat_compare_means(comparisons = cmpr, tip.length=0.01,
  #                   label = "p.signif", 
  #                   symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
  #                                      symbols = c("****", "***", "**", "*", "ns")))+
  theme_classic()+
  theme(legend.position = "none")
Pulses

Pulses2 <- ggplot(TotalPheno, aes(condition, Pulses2))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_fill_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  scale_color_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  xlab("")+
  #stat_compare_means(comparisons = cmpr, tip.length=0.01,
  #                   label = "p.signif", 
  #                   symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
  #                                      symbols = c("****", "***", "**", "*", "ns")))+
  theme_classic()+
  theme(legend.position = "none")
Pulses2

YII <- ggplot(TotalPheno, aes(condition, YII))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_fill_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  scale_color_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  xlab("")+
  #stat_compare_means(comparisons = cmpr, tip.length=0.01,
  #                   label = "p.signif", 
  #                   symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
  #                                      symbols = c("****", "***", "**", "*", "ns")))+
  theme_classic()+
  theme(legend.position = "none")
YII

CellDensity <- ggplot(TotalPheno, aes(condition, CellDensity))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_fill_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  scale_color_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  xlab("")+
  #stat_compare_means(comparisons = cmpr, tip.length=0.01,
  #                   label = "p.signif", 
  #                   symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
  #                                      symbols = c("****", "***", "**", "*", "ns")))+
  theme_classic()+
  theme(legend.position = "none")
CellDensity

HostRED <- ggplot(LDHost, aes(condition, RED.B.HLog))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_fill_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  scale_color_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  xlab("")+
  #stat_compare_means(comparisons = cmpr, tip.length=0.01,
  #                   label = "p.signif", 
  #                   symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
  #                                      symbols = c("****", "***", "**", "*", "ns")))+
  theme_classic()+
  theme(legend.position = "none")
HostRED

HostGRN <- ggplot(LDHost, aes(condition, GRN.B.HLog))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_fill_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  scale_color_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  xlab("")+
  #stat_compare_means(comparisons = cmpr, tip.length=0.01,
  #                   label = "p.signif", 
  #                   symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
  #                                      symbols = c("****", "***", "**", "*", "ns")))+
  theme_classic()+
  theme(legend.position = "none")
HostGRN

HostFSC <- ggplot(LDHost, aes(condition, FSC.HLog))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_fill_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  scale_color_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  xlab("")+
  #stat_compare_means(comparisons = cmpr, tip.length=0.01,
  #                   label = "p.signif", 
  #                   symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
  #                                      symbols = c("****", "***", "**", "*", "ns")))+
  theme_classic()+
  theme(legend.position = "none")
HostFSC

HostSSC <- ggplot(LDHost, aes(condition, SSC.HLog))+
  geom_violin(aes(fill = condition), color = "white", alpha = 0.3)+
  geom_boxplot(aes(color = condition), outlier.alpha = 0.5, size = 1.4, alpha = 0.8)+
  scale_fill_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  scale_color_manual(breaks = c("Ambient", "Shaded", "Variable"), values=c("coral2", "darkcyan", "#ffcdb2"))+
  xlab("")+
  #stat_compare_means(comparisons = cmpr, tip.length=0.01,
  #                   label = "p.signif", 
  #                   symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
  #                                      symbols = c("****", "***", "**", "*", "ns")))+
  theme_classic()+
  theme(legend.position = "none")
HostSSC
head(TotalPheno)


grid.arrange(Diam +
               theme(axis.text.x = element_blank(),
                     axis.title.x = element_blank(), 
                     strip.text.x = element_blank(),
                     axis.text.y = element_blank()), 
             ColorRating+
               theme(axis.text.x = element_blank(),
                     axis.title.x = element_blank(), 
                     strip.text.x = element_blank(),
                     axis.text.y = element_blank()), 
             Pulses+
               theme(axis.text.x = element_blank(),
                     axis.title.x = element_blank(), 
                     strip.text.x = element_blank(),
                     axis.text.y = element_blank()), 
             Pulses2+
               theme(axis.text.x = element_blank(),
                     axis.title.x = element_blank(), 
                     strip.text.x = element_blank(),
                     axis.text.y = element_blank()), 
             YII + 
               theme(axis.text.x = element_blank(),
                     axis.title.x = element_blank(), 
                     strip.text.x = element_blank(),
                     axis.text.y = element_blank()), 
             CellDensity+
               theme(axis.text.x = element_blank(),
                     axis.title.x = element_blank(), 
                     strip.text.x = element_blank(),
                     axis.text.y = element_blank()), 
             HostRED+
               theme(axis.text.x = element_blank(),
                     axis.title.x = element_blank(), 
                     strip.text.x = element_blank(),
                     axis.text.y = element_blank()), 
             HostGRN+
               theme(axis.text.x = element_blank(),
                     axis.title.x = element_blank(), 
                     strip.text.x = element_blank(),
                     axis.text.y = element_blank()), 
             HostFSC+
               theme(axis.text.x = element_blank(),
                     axis.title.x = element_blank(), 
                     strip.text.x = element_blank(),
                     axis.text.y = element_blank()), 
             HostSSC+
               theme(axis.text.x = element_blank(),
                     axis.title.x = element_blank(), 
                     strip.text.x = element_blank(),
                     axis.text.y = element_blank()), nrow = 5
             )
