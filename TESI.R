#Project
rm(list=ls()) 
setwd("C:\\Users\\utente\\Desktop\\tesi e magistrale\\tesi\\analisi")

library(tidyverse)
library(ggplot2)
library(corrplot)
library(janitor)
library(modelsummary)
library(fixest)
library(sandwich)
library(lmtest)
library(readxl)
library(e1071)
PROD_RD<- read_excel("DATABASE.xlsx", sheet = 9)

PROD_RD <- PROD_RD %>%
  group_by(STATE) %>%
  mutate(RDL = dplyr::lag(RD,1)) %>%
  mutate(LABFORCEL = dplyr::lag(LABFORCE,1)) %>%
  mutate(PATENTSL = dplyr::lag(PATENTS,1))

View(PROD_RD)
attach(PROD_RD)

# WORLD MAP
library(ggplot2)
library(maps)
world_map <- map_data("world") 
highlight_states <- c("France", "Germany", "Spain", "Italy", "Netherlands", "Slovakia", "Poland", "Portugal", "Greece", "Romania")
highlight_colors <- c("aquamarine3", "aquamarine3", "aquamarine3", "aquamarine3", "aquamarine3", "blue", "blue", "blue", "blue", "blue")  # Colori per i paesi selezionati
highlight_map <- subset(world_map, region %in% highlight_states)
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_polygon(data = highlight_map, aes(x = long, y = lat, group = group, fill = region), color = "black") +
  scale_fill_manual(values = highlight_colors) +
  guides(fill = FALSE) +  # Rimuove la legenda per il colore
  theme_void() +
  coord_fixed(xlim=c(-20, +40), ylim=c(+35, +60), ratio= 1.5)


world_map <- map_data("world") 
highlight_states <- c("France", "Germany", "Greece",  "Italy", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Spain")
highlight_colors <- c("aquamarine3", "aquamarine3", "blue", "aquamarine3", "aquamarine3", "blue", "blue", "blue", "blue", "aquamarine3")  # Colori per i paesi selezionati
highlight_states <- c("Slovakia", "Poland", "Portugal", "Greece", "Romania")
highlight_colors <- c("blue", "blue", "blue", "blue", "blue")  # Colori per i paesi selezionati
highlight_map <- subset(world_map, region %in% highlight_states)
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_polygon(data = highlight_map, aes(x = long, y = lat, group = group, fill = region), color = "black") +
  scale_fill_manual(values = highlight_colors) +
  guides(fill = FALSE) +  # Rimuove la legenda per il colore
  theme_void() +
  coord_fixed(xlim=c(-20, +40), ylim=c(+35, +60), ratio= 1.5)




#STATISTICAL ANALYSIS of DATA
#summary statistics
PROD_RD %>% summary()

#CORRELATION - direction and intensity of the linear bond
#prod & rd
correlation_PROD_RD= 
  PROD_RD %>%
  select(
    RD,
    PRODUCTIVITY) %>% 
  cor(use = "complete.obs")
correlation_PROD_RD

#prod & labforce
correlation_PROD_LABFORCE= 
  PROD_RD %>%
  select(
    LABFORCE,
    PRODUCTIVITY) %>% 
  cor(use = "complete.obs")
correlation_PROD_LABFORCE

#prod e patents
correlation_PROD_PATENTS= 
  PROD_RD %>%
  select(
    PATENTS,
    PRODUCTIVITY) %>% 
  cor(use = "complete.obs")
correlation_PROD_PATENTS

#CORRELATION MATRIX
data <- data.frame(RD, PRODUCTIVITY, PATENTS, LABFORCE)
correlation_matrix <- cor(data)
print(correlation_matrix)

install.packages("reshape2")
library(reshape2)
library(ggplot2)

cormat <- round(cor(data),2)
head(cormat)
melted_cormat <- melt(cormat)
head(melted_cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
upper_tri

melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "aquamarine3", mid = "white", 
                       midpoint = 0.75, limit = c(0.5,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "aquamarine3", high = "blue", mid = "white", 
                       midpoint = 0.78, limit = c(0.56,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

print(ggheatmap)





#COVARIATION - direction fo linear bond
cov(RD, PRODUCTIVITY)
cov(LABFORCE, PRODUCTIVITY, use = "complete.obs")
cov(PATENTS, PRODUCTIVITY)



#SKEWNESS - symmetric respect to median value
skewness(RD,type=2)
skewness(PRODUCTIVITY,type=2)
skewness(LABFORCE,type=2, na.rm = TRUE)
skewness(PATENTS,type=2)


#KURTOSIS - how distribution moves away from normality
kurtosis(RD, type=2)
kurtosis(PRODUCTIVITY, type=2)
kurtosis(LABFORCE, type=2, na.rm = TRUE)
kurtosis(PATENTS, type=2)





# GRAPHICAL ANALYSIS on main variables 
#RD HISTOGRAM
my_histogram =
  PROD_RD %>%  
  ggplot() +
  aes(x = RD) + 
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.5, color = 'black', fill = 'blue') + 
  theme_bw()   

my_histogram +
  labs(
    x = "RD",
    y = "Density",
    title = "Histogram of RD"
  ) + theme_bw()


#PROD HISTOGRAM
my_histogram =
  PROD_RD %>%  
  ggplot() +
  aes(x = PROD) + 
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.5, color = 'black', fill = 'blue') + 
  theme_bw()   

my_histogram +
  labs(
    x = "PROD",
    y = "Density",
    title = "Histogram of PROD"
  ) + theme_bw()


#TOTLABFORCE HISTOGRAM
my_histogram =
  PROD_RD %>%  
  ggplot() +
  aes(x = TOTLABFORCE) + 
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.5, color = 'black', fill = 'blue') + 
  theme_bw()   

my_histogram +
  labs(
    x = "TOTLABFORCE",
    y = "Density",
    title = "Histogram of LABFORCE"
  ) + theme_bw()


#PATENTS HISTOGRAM
my_histogram =
  PROD_RD %>%  
  ggplot() +
  aes(x = PATENTS) + 
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.5, color = 'black', fill = 'blue') + 
  theme_bw()   

my_histogram +
  labs(
    x = "PATENTS",
    y = "Density",
    title = "Histogram of PATENTS"
  ) + theme_bw()


#BOXPLOT
boxplot(RD, main = "Boxplot of RD", col = "aquamarine3", outlier.colour="darkorange")
boxplot(PRODUCTIVITY, main = "Boxplot of PROD", col = "aquamarine3")
boxplot(LABFORCE, main = "Boxplot of LABFORCE", col = "aquamarine3")
boxplot(PATENTS, main = "Boxplot of PATENTS", col = "aquamarine3")

help(boxplot)

#SCATTERPLOT prod & rd
scatterplot_PROD_RD =
  PROD_RD %>%  ggplot() +
  aes(x = RD, y = PRODUCTIVITY) +
  geom_point() +
  labs(
    x = "R&D INVESTMENT",
    y = "PRODUCTIVITY",
    title = "Relation PROD-RD"
  ) + theme_bw() 
scatterplot_PROD_RD + geom_smooth(method = 'lm')

#SCATTERPLOT prod & rd LOG
scatterplot_PROD_RD_log =
  PROD_RD %>%  ggplot() +
  aes(x = log(RD), y = log(PRODUCTIVITY)) +
  geom_point() +
  labs(
    x = "R&D INVESTMENT",
    y = "PRODUCTIVITY",
    title = "Relation PROD-RD"
  ) + theme_bw() 
scatterplot_PROD_RD_log + geom_smooth(method = 'lm')



#SCATTERPLOT prod & labforce
scatterplot_PROD_LABFORCE =
  PROD_RD %>%  ggplot() +
  aes(x = LABFORCE, y = PRODUCTIVITY) +
  geom_point() +
  labs(
    x = "LABFORCE",
    y = "PRODUCTIVITY",
    title = "Relation PROD-LABFORCE"
  ) + theme_bw() 
scatterplot_PROD_LABFORCE + geom_smooth(method = 'lm')

#SCATTERPLOT prod & labforce LOG
scatterplot_PROD_LABFORCE_log =
  PROD_RD %>%  ggplot() +
  aes(x = log(LABFORCE), y = log(PRODUCTIVITY)) +
  geom_point() +
  labs(
    x = "LABFORCE",
    y = "PRODUCTIVITY",
    title = "Relation PROD-LABFORCE"
  ) + theme_bw() 
scatterplot_PROD_LABFORCE_log + geom_smooth(method = 'lm')

#SCATTERPLOT prod & labforce LOG
scatterplot_PROD_LABFORCE_log =
  PROD_RD %>%  ggplot() +
  aes(x = (LABFORCE)^2, y = PRODUCTIVITY) +
  geom_point() +
  labs(
    x = "TOTLABFORCE",
    y = "PROD",
    title = "Relation PROD-LABFORCE"
  ) + theme_bw() 
scatterplot_PROD_LABFORCE_log + geom_smooth(method = 'lm')


scatterplot_PROD_LABFORCE_log =
  PROD_RD %>%  ggplot() +
  aes(x = (LABFORCE)^2, y = PRODUCTIVITY) +
  geom_point() +
  labs(
    x = "TOTLABFORCE",
    y = "PROD",
    title = "Relation PROD-LABFORCE"
  ) + theme_bw() 
scatterplot_PROD_LABFORCE_log + geom_smooth(method = 'lm')



#SCATTERPLOT prod & patents
scatterplot_PROD_PATENTS =
  PROD_RD %>%  ggplot() +
  aes(x = PATENTS, y = PRODUCTIVITY) +
  geom_point() +
  labs(
    x = "PATENT ACTIVITY",
    y = "PRODUCTIVITY",
    title = "Relation PROD-PATENTS"
  ) + theme_bw() 
scatterplot_PROD_PATENTS + geom_smooth(method = 'lm')

#SCATTERPLOT prod & patents LOG
scatterplot_PROD_PATENTS_log =
  PROD_RD %>%  ggplot() +
  aes(x = log(PATENTS), y = log(PRODUCTIVITY)) +
  geom_point() +
  labs(
    x = "PATENT ACTIVITY",
    y = "PRODUCTIVITY",
    title = "Relation PROD-PATENTS"
  ) + theme_bw() 
scatterplot_PROD_PATENTS_log + geom_smooth(method = 'lm')



#Simultaneity problems
#It may be source of reverse causality: situation in which the causal relation run from the dependent to the independent variable
#and not the other way round




#MODELLO LAD
install.packages("MASS")
library(MASS)
#prod & rd
model <- rlm(log(PRODUCTIVITY) ~ log(RD))
summary(model)
#prod & labforce
model1 <- rlm(log(PRODUCTIVITY) ~ log(LABFORCE))
summary(model1)
#prod & patents
model2 <- rlm(log(PRODUCTIVITY) ~ log(PATENTS))
summary(model2)



#OLS (singolo)
reg_1_ols = 
  feols(data = PROD_RD, 
        fml = PRODUCTIVITY ~ PATENTS, vcov = 'HC1')

modelsummary(list(reg_1_ols), 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)


reg_1A_ols = 
  feols(data = PROD_RD, 
        fml = log(PRODUCTIVITY) ~ log(LABFORCE), vcov = 'HC1')

modelsummary(list(reg_1A_ols), 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)

reg_1B_ols = 
  feols(data = PROD_RD, 
        fml = log(PRODUCTIVITY) ~ log(PATENTS), vcov = 'HC1')

modelsummary(list(reg_1B_ols), 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)

#OLS (multiplo)
reg_2_ols = 
  feols(data = PROD_RD, 
        fml = PRODUCTIVITY ~ RD + LABFORCE + PATENTS, vcov = 'HC1')

modelsummary(list(reg_2_ols), 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)

#OLS (multiplo)
reg_3_ols = 
  feols(data = PROD_RD, 
        fml = log(PRODUCTIVITY) ~ log(RD) + log(LABFORCE) + log(PATENTS), vcov = 'HC1')

modelsummary(list(reg_3_ols), 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)





#FIXED EFFECTS
#Panel - Only LOCATION FE(changes in states does not change in time)
reg_1_iFE = 
  feols(data = PROD_RD, 
        fml = log(PRODUCTIVITY) ~ log(RD) | 
          STATE, vcov = 'HC1')

modelsummary(reg_1_iFE, 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)



reg_3_ols = 
  feols(data = PROD_RD, 
        fml = log(PRODUCTIVITY) ~ log(RD) + log(LABFORCE) + log(PATENTS), vcov = 'HC1')

modelsummary(list(reg_3_ols), 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)


#MULTIPLE
reg_2_iFE = 
  feols(data = PROD_RD, 
        fml = log(PRODUCTIVITY) ~ log(RD) + log(LABFORCE) + log(PATENTS) | 
          STATE, vcov = 'HC1')

modelsummary(reg_2_iFE, 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)

modelsummary(list('4' = reg_3_ols, 
                  '5' = reg_2_iFE), 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)




#Panel - Only TIME FE(changes in time does not change in states)
reg_1_tFE = 
  feols(data = PROD_RD, 
        fml = log(PROD) ~ log(RD) | 
          YEAR, vcov = 'HC1')

modelsummary(reg_1_tFE , 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)



reg_3_ols = 
  feols(data = PROD_RD, 
        fml = log(PRODUCTIVITY) ~ log(RD) + log(LABFORCE) + log(PATENTS), vcov = 'HC1')

modelsummary(list(reg_3_ols), 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)



#MULTIPLE
reg_2_tFE = 
  feols(data = PROD_RD, 
        fml = log(PRODUCTIVITY) ~ log(RD) + log(LABFORCE) + log(PATENTS) | 
          YEAR, vcov = 'HC1')

modelsummary(reg_2_tFE, 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)



modelsummary(list('4' = reg_3_ols,
                  '5' = reg_2_iFE,
                  '6' = reg_2_tFE), 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)



#Panel - Both LOCATION and TIME FE
reg_1_itFE = 
  feols(data = PROD_RD, 
        fml = log(PROD) ~ log(RD) | 
          STATE + YEAR, vcov = 'HC1')

modelsummary(reg_1_itFE, 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)
#MULTIPLE
reg_2_itFE = 
  feols(data = PROD_RD, 
        fml = log(PRODUCTIVITY) ~ log(RD) + log(LABFORCE) + log(PATENTS) | 
          STATE + YEAR, vcov = 'HC1')

modelsummary(reg_2_itFE, 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)


modelsummary(list('4' = reg_3_ols,
                  '5' = reg_2_iFE,
                  '6' = reg_2_tFE,
                  '7' = reg_2_itFE), 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)



#LAG MODEL
library(plm)
reg_DEF1_itFE = 
  feols(data = PROD_RD, 
        fml = log(PRODUCTIVITY) ~ log(RD) + log(LABFORCE) + log(PATENTS)| 
          STATE + YEAR, vcov = 'HC1')

modelsummary(reg_DEF1_itFE, stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3) 

reg_DEF2_itFE = 
  feols(data = PROD_RD, 
        fml = log(PRODUCTIVITY) ~ log(RD) + log(LABFORCE) + log(PATENTS) + 
          log(RDL)  +  log(PATENTSL) + log(LABFORCEL)| 
          STATE + YEAR, vcov = 'HC1')

modelsummary(reg_DEF2_itFE, stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3) 

modelsummary(list("7" = reg_DEF1_itFE,
                  "8" = reg_DEF2_itFE), 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)




#R&D AND PATENTS
reg_a_itFE = 
  feols(data = PROD_RD, 
        fml = log(PATENTS) ~  log(RD)|
          STATE + YEAR, vcov = 'HC1')

modelsummary(list(reg_a_itFE), 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)



reg_a_itFE = 
  feols(data = PROD_RD, 
        fml = log(PATENTS) ~  log(RD))

modelsummary(list(reg_a_itFE), 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)





reg_b_itFE = 
  feols(data = PROD_RD, 
        fml = log(PATENTS) ~  log(RDL)|
          STATE + YEAR, vcov = 'HC1')

modelsummary(list(reg_b_itFE), 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)

reg_c_itFE = 
  feols(data = PROD_RD, 
        fml = log(PATENTS) ~  log(RDL) + log(RD)|
          STATE + YEAR, vcov = 'HC1')

modelsummary(list(reg_c_itFE), 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)


modelsummary(list('1' = reg_a_itFE, 
                  '2' = reg_b_itFE, 
                  '3' = reg_c_itFE), 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)




#Summarising:
modelsummary(list('OLS' = reg_1_ols, 
                  'Location FE' = reg_1_iFE, 
                  'Time FE' = reg_1_tFE,
                  'Location & Time FE'= reg_1_itFE), 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)
#MULTIPLE
modelsummary(list('OLS' = reg_3_ols, 
                  'Location FE' = reg_2_iFE, 
                  'Time FE' = reg_2_tFE,
                  'Location & Time FE'= reg_2_itFE), 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)



#PANEL GRAPHS
library(foreign)
install.packages("gplots")
library(gplots)
#Fixed effects: Heterogeneity across countries (or entities)
plotmeans(RD ~ STATE, main="Heterogeineity across countries", data=PROD_RD)
#Fixed effects: Heterogeneity across years 
plotmeans(RD ~ YEAR, main="Heterogeineity across years", data=PROD_RD)

plotmeans(LABFORCE ~ STATE, main="Heterogeineity across countries", data=PROD_RD)
plotmeans(LABFORCE ~ YEAR, main="Heterogeineity across years", data=PROD_RD)

plotmeans(PATENTS ~ STATE, main="Heterogeineity across countries", data=PROD_RD)
plotmeans(PATENTS ~ YEAR, main="Heterogeineity across years", data=PROD_RD)



#CLUSTERED STANDARD ERRORS
reg_1_itFE_clustered = 
  feols(data = PROD_RD, 
        fml = log(PROD) ~ log(RD) | 
          STATE + YEAR,
        cluster = "STATE")

modelsummary(list('8' = reg_DEF2_itFE,
                  '9' = reg_1_itFE_clustered), 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)

#MULTIPLE
reg_2_itFE_clustered = 
  feols(data = PROD_RD, 
        fml = log(PRODUCTIVITY) ~ log(RD) + log(LABFORCE) + log(PATENTS) + 
          log(RDL) + log(LABFORCEL) +  log(PATENTSL)|  
          STATE + YEAR,
        cluster = "STATE")

modelsummary(list('8' = reg_DEF2_itFE,
                  '9' = reg_2_itFE_clustered), 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)


reg_2_itFE_clustered = 
  feols(data = PROD_RD, 
        fml = log(PRODUCTIVITY) ~ log(RD) + log(LABFORCE) + log(PATENTS) | 
          STATE + YEAR,
        cluster = "STATE")

modelsummary(list('reg_DEF2_itFE' = reg_2_itFE,
                  'reg_2_itFE_clustered' = reg_2_itFE_clustered), 
             stars = c('*' = .1, '**' = .05, '***' = .01), fmt = 3)




# ADF TEST
library(plm)
library(tseries)
adftest=pdata.frame(PROD_RD, index = c("STATE", "YEAR"))
adf.test(adftest$RD,k=1)
adftest=pdata.frame(PROD_RD, index = c("STATE", "YEAR"))
adf.test(adftest$PROD,k=1)

