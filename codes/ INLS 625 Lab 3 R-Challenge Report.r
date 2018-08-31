
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggpubr)

df_stat <- read.csv('dataset/stats.csv')
df_result <- read.csv('dataset/results.csv')

# All the games played during 2006/07 season to 2017/18 season.
head(df_result)

# Detailed statistics for the big 6 during 2006/07 season to 2017/18 season.
head(df_stat)

# First extract the data we are going to use and put it into a data frame.
df_ranking <- df_stat %>% select('team','ranking','season')
# This is how the data frame looks like (order by season).
head(df_ranking)

# Filter the data, put into a data frame and visualize. 
df_champions <- filter(df_stat, ranking == '1') %>% count(team)
pl_champions <- ggplot(df_champions,aes(x=team, y=n))+
    geom_bar(stat='identity', fill='white', color='steelblue', size = 1, width=0.3)+
    labs(title ="Champion Numbers", x = "Teams", y = "Numbers")
print(pl_champions)
# You may find that there are 11 champions they have won in total.
# Yes, the champion for the 15/16 season is Leicester City!

pl_ranking <- ggplot(df_ranking,aes(x=season,y=ranking,group=team))+
    geom_line(aes(color=team))+ geom_point(size = 0.5)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    labs(title ="Rankings of big 6 from 2006 to 2018", x = "Seasons", y = "Ranking") 
print(pl_ranking)

pl_ranking_new <- ggplot(df_ranking,aes(x=season,y=ranking,group=team))+
    geom_line(aes(color=team))+ geom_point(size = 1)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    scale_y_continuous(breaks = seq(1, 14, by = 3))+
    facet_grid(team ~ .)+
    theme(strip.text.y = element_text(size=7),
          strip.background = element_rect(colour="red", fill="#CCCCFF"))+
    labs(title ="Rankings of big 6 from 2006 to 2018", x = "Seasons", y = "Ranking")
print(pl_ranking_new)

df_goals <- df_stat %>% select('team','goals','season')
pl_goals <- ggplot(df_goals,aes(x=season,y=goals,group=team))+
    geom_line(aes(color=team))+ geom_point(size = 1)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    scale_y_continuous(breaks = seq(25, 110, by = 10))+
    facet_grid(team ~ .)+
    theme(strip.text.y = element_text(size=7),
          strip.background = element_rect(colour="red", fill="#CCCCFF"))+
    labs(title ="Goals of big 6 from 2006 to 2018", x = "Seasons", y = "Goals")
print(pl_goals)

# First create a new data frame with 5 columns
df_wins <- data.frame(matrix(ncol = 5, nrow = 0))
x <- c("team","homewins","awaywins","draw","season")
colnames(df_wins) <- x
df_wins

head(df_result)

# create a function to calculate the wins at home/away, and draws
calculate_wins <- function(df, t, s){
  h <- as.numeric(filter(df, home_team == t , result == 'H' , season == s) %>% count())
  a <- as.numeric(filter(df, away_team == t , result == 'A' , season == s) %>% count())
  d <- as.numeric(filter(df, home_team == t , result == 'D' , season == s) %>% count())
  record <- data.frame(team = t, homewins = h, awaywins = a, draw = d, season = s)
  return(record)
}

# use a for loop to calculate for all team and all seasons
teams <- c('Manchester United', 'Manchester City', 'Chelsea', 
           'Arsenal', 'Liverpool', 'Tottenham Hotspur')
seasons <- c('2006-2007','2007-2008','2008-2009','2009-2010','2010-2011',
             '2011-2012','2012-2013','2013-2014','2014-2015','2015-2016',
             '2016-2017','2017-2018')

for (team in teams){
    for (season in seasons){    
    df_wins <- rbind(df_wins,calculate_wins(df_result, team, season))
    }
}    

head(df_wins)

# There should be 72 rows (6*12) and 5 columns.
dim(df_wins)

# Manchester United
df_wins_mu <- filter(df_wins, team == 'Manchester United')
dfm_wins_mu <- melt(df_wins_mu[,c('season','homewins','awaywins','draw')],id.vars = 1)
x <- c("season","type","value")
colnames(dfm_wins_mu) <- x

pl_mu <- ggplot(dfm_wins_mu,aes(x=season,y=value))+
    geom_bar(aes(fill = type),stat = "identity", position = "fill")+
    theme(text = element_text(size=8),
          axis.text.x = element_text(angle = 60, hjust = 1))+
    labs(title ="Manchester United performances", x = "Seasons", y = "Percentage")

# Manchester City
df_wins_mc <- filter(df_wins, team == 'Manchester City')
dfm_wins_mc <- melt(df_wins_mc[,c('season','homewins','awaywins','draw')],id.vars = 1)
x <- c("season","type","value")
colnames(dfm_wins_mc) <- x

pl_mc <- ggplot(dfm_wins_mc,aes(x=season,y=value))+
    geom_bar(aes(fill = type),stat = "identity", position = "fill")+
    theme(text = element_text(size=8),
          axis.text.x = element_text(angle = 60, hjust = 1))+
    labs(title ="Manchester City performances", x = "Seasons", y = "Percentage")

# Liverpool
df_wins_lp <- filter(df_wins, team == 'Liverpool')
dfm_wins_lp <- melt(df_wins_lp[,c('season','homewins','awaywins','draw')],id.vars = 1)
x <- c("season","type","value")
colnames(dfm_wins_lp) <- x

pl_lp <- ggplot(dfm_wins_lp,aes(x=season,y=value))+
    geom_bar(aes(fill = type),stat = "identity", position = "fill")+
    theme(text = element_text(size=8),
          axis.text.x = element_text(angle = 60, hjust = 1))+
    labs(title ="Liverpool performances", x = "Seasons", y = "Percentage")

# Arsenal
df_wins_ar <- filter(df_wins, team == 'Arsenal')
dfm_wins_ar <- melt(df_wins_ar[,c('season','homewins','awaywins','draw')],id.vars = 1)
x <- c("season","type","value")
colnames(dfm_wins_ar) <- x

pl_ar <- ggplot(dfm_wins_ar,aes(x=season,y=value))+
    geom_bar(aes(fill = type),stat = "identity", position = "fill")+
    theme(text = element_text(size=8),
          axis.text.x = element_text(angle = 60, hjust = 1))+
    labs(title ="Arsenal performances", x = "Seasons", y = "Percentage")

# Chelsea
df_wins_ch <- filter(df_wins, team == 'Chelsea')
dfm_wins_ch <- melt(df_wins_ch[,c('season','homewins','awaywins','draw')],id.vars = 1)
x <- c("season","type","value")
colnames(dfm_wins_ch) <- x

pl_ch <- ggplot(dfm_wins_ch,aes(x=season,y=value))+
    geom_bar(aes(fill = type),stat = "identity", position = "fill")+
    theme(text = element_text(size=8),
          axis.text.x = element_text(angle = 60, hjust = 1))+
    labs(title ="Chelsea performances", x = "Seasons", y = "Percentage")

# Hotspur
df_wins_hs <- filter(df_wins, team == 'Tottenham Hotspur')
dfm_wins_hs <- melt(df_wins_hs[,c('season','homewins','awaywins','draw')],id.vars = 1)
x <- c("season","type","value")
colnames(dfm_wins_hs) <- x

pl_hs <- ggplot(dfm_wins_hs,aes(x=season,y=value))+
    geom_bar(aes(fill = type),stat = "identity", position = "fill")+
    theme(text = element_text(size=8),
          axis.text.x = element_text(angle = 60, hjust = 1))+
    labs(title ="Hotspur performances", x = "Seasons", y = "Percentage")

# Mix in to a single graph!
ggarrange(pl_mu, pl_mc, pl_lp, pl_ar, pl_ch, pl_hs, nrow=3, ncol=2)

df_statall <- read.csv('dataset/stats_all.csv')
# make the null value to 0
df_statall[is.na(df_statall)] <- 0
head(df_statall)

set.seed(111)
trainingRowIndex <- sample(1:nrow(df_statall), 0.8*nrow(df_statall))
trainingData <- df_statall[trainingRowIndex, ]
testData <- df_statall[-trainingRowIndex, ]

m1 <- lm(ranking ~. -team-season, data=trainingData)
summary(m1)

m2 <- lm(ranking ~ wins+losses+total_red_card+att_hd_goal+goal_fastbreak+
         total_offside+clean_sheet+goals_conceded+own_goals+
         corner_taken+big_chance_missed, data=trainingData)
summary(m2)

distPred <- predict(m2, testData)

actuals_preds <- data.frame(cbind(actuals=testData$ranking, predicteds=distPred)) 
head(actuals_preds)

# min_max accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape

# Use Q-Q plot to check whether the residuals follow a normal distribution.
qqnorm(residuals(m2),ylab="Residuals",main="")
qqline(residuals(m2))
# Almost normal!
