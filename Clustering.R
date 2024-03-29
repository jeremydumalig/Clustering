library(tidyverse)
library(cluster)
library(factoextra)
library(dendextend)
library(ggplot2)
rm(list=ls())

nba19 <- read_csv(file='nba_2019.csv')
nba19 <- 
  nba19 %>%
  na.omit() %>%
  select(Player, `FG%`, FGA, `3P%`, `3PA`, `FT%`, ORB, DRB, AST, STL, BLK, TOV, PTS)

#data normalization
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

#hierarchical clustering model
d <- dist(select(nba19, -Player), method='euclidean')
hc1 <- hclust(d, method='complete')
plot(hc1, cex=0.6, hang=-1)

clust <- cutree(hc1, k=10)
nba19$Hierarchical <- clust

#ggplot theme
theme_borders <- 
  theme_linedraw() +
  theme(
    plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"),
    plot.background = element_rect(
      fill = "grey90",
      color = "black"
    ),
    legend.box.background = element_rect(size=0.75),
    title = element_text(size=20),
    legend.title = element_text(size=10),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15)
  )

#cluster visualization
fviz_cluster(list(data = select(nba19, -Player), cluster = clust), labelsize=0) + 
  theme_borders +
  theme(legend.position = "none") +
  ggtitle("Hierarchical Clustering Results") +
  xlab("") + ylab("")
