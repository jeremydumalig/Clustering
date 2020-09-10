library(tidyverse)
library(cluster)
library(factoextra)
library(dendextend)
library(ggplot2)
rm(list=ls())

nba19 <- read_csv(file='https://raw.githubusercontent.com/jeremydumalig/Clustering/master/nba_2019')
nba19 <- 
  nba19 %>%
  na.omit() %>%
  select(Player, `FG%`, FGA, `3P%`, `3PA`, `FT%`, ORB, DRB, AST, STL, BLK, TOV, PTS)

nba19$FGA <- (nba19$FGA - min(nba19$FGA))/(max(nba19$FGA)-min(nba19$FGA))
nba19$`3PA` <- (nba19$`3PA` - min(nba19$`3PA`))/(max(nba19$`3PA`)-min(nba19$`3PA`))
nba19$ORB <- (nba19$ORB - min(nba19$ORB))/(max(nba19$ORB)-min(nba19$ORB))
nba19$DRB <- (nba19$DRB - min(nba19$DRB))/(max(nba19$DRB)-min(nba19$DRB))
nba19$AST <- (nba19$AST - min(nba19$AST))/(max(nba19$AST)-min(nba19$AST))
nba19$STL <- (nba19$STL - min(nba19$STL))/(max(nba19$STL)-min(nba19$STL))
nba19$BLK <- (nba19$BLK - min(nba19$BLK))/(max(nba19$BLK)-min(nba19$BLK))
nba19$TOV <- (nba19$TOV - min(nba19$TOV))/(max(nba19$TOV)-min(nba19$TOV))
nba19$PTS <- (nba19$PTS - min(nba19$PTS))/(max(nba19$PTS)-min(nba19$PTS))

rownames(nba19) <- nba19$Player

d <- dist(select(nba19, -Player), method='euclidean')
hc1 <- hclust(d, method='complete')
plot(hc1, cex=0.6, hang=-1)

clust <- cutree(hc1, k=10)
nba19$Hierarchical <- clust

fviz_cluster(list(data = select(nba19, -Player), cluster = clust))
