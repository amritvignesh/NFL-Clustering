install.packages("gsisdecoder")
install.packages("cluster")
install.packages("factoextra")
install.packages("gghighlight")

library(tidyverse)
library(nflfastR)
library(vip)
library(ggimage)
library(ggplot2)
library(gt)
library(nflreadr)
library(dplyr)
library(nflplotR)
library(ggrepel)
library(gsisdecoder)
library(cluster)
library(factoextra)
library(gghighlight)

pbp <- load_pbp(2022)
overall <- calculate_player_stats(pbp, weekly=FALSE)

colnames(overall)

qb_stats <- overall %>%
  filter(position == "QB") %>%
  filter(attempts >= 100) %>%
  mutate(cmppct = completions/attempts * 100, tdpct = passing_tds/attempts * 100, intpct = interceptions/attempts * 100, skpct = sacks/(attempts + sacks) * 100, epaatt = passing_epa/attempts, anyatt = (passing_yards - sack_yards + 20 * passing_tds - 45 * interceptions)/(attempts + sacks), passrtg = ((completions/attempts - 0.3) * 5 + (passing_yards/attempts - 3) * 0.25 + (passing_tds/attempts) * 20 + 2.375 - (interceptions/attempts * 25))/6 * 100) %>%
  select(name = player_display_name, cmppct, tdpct, intpct, skpct, pacr, adjepacpoe = dakota, epaatt, anyatt, passrtg)
  
qb_stats_nums <- scale(qb_stats[,c(2:10)])

fviz_nbclust(qb_stats_nums, kmeans, method = "silhouette", k.max=15)

set.seed(0)
kmeans_qbs <- kmeans(qb_stats_nums, centers = 2, nstart = 25, iter.max = 20)
kmeans_centers <- as.data.frame(kmeans_qbs$centers)

kmeans_centers$cluster <- c('Cluster 1', 'Cluster 2')

kmeans_centers <- kmeans_centers %>%
  rename(c('Adj EPA+CPOE'='adjepacpoe', 'ANY/A'='anyatt', 'CMP%'='cmppct', 'EPA/ATT'='epaatt', 'INT%'='intpct', 'PACR'='pacr', 'Passer RTG'='passrtg', 'SK%'='skpct', 'TD%'='tdpct')) %>%
  pivot_longer(!cluster, names_to = 'statname', values_to = 'statvalue')
                             
kmeans_centers %>%
  ggplot(aes(x=statname, y=statvalue, color=cluster)) +
  geom_point() +
  facet_wrap(~ cluster, ncol = 2) +
  labs(x = "Statistic Predictor", y = "Scaled Statistical Value Center Per Cluster",
       title = "Cluster Compositions for 2022 NFL QBs (100+ Pass Attempts)") +
  theme(legend.position = "none", strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), 
        panel.grid.minor = element_blank(), plot.title = element_text(size=14, hjust=0.5, face="bold"), axis.title = element_text(face="bold"))

pca_qbs <- prcomp(qb_stats_nums)
pca_summary <- summary(pca_qbs)

twopcas <- as.data.frame(pca_qbs$x[,1:2])
twopcas$cluster <- as.factor(kmeans_qbs$cluster)
variance_1 <- 100 *round(pca_summary$importance[2,1], 4) 
variance_2 <- 100 *round(pca_summary$importance[2,2], 4) 

twopcas %>%
  ggplot(aes(x=PC1, y=PC2, color= cluster)) + 
  geom_point(alpha=0.3) + 
  stat_ellipse(level=(2/3)) + 
  labs(x = paste0('PC1 (Accounts for ', variance_1, '% of Variance)'), 
       y = paste0('PC2 (Accounts for ', variance_2, '% of Variance)'), 
       title = 'K-Means Cluster Differences for NFL QBs in the 2022 Season (100+ Pass Attempts)') +
  theme(plot.title = element_text(size=14, hjust=0.5, face="bold"), axis.title = element_text(face="bold"))

name_cluster <- data.frame(name=qb_stats$name, cluster=kmeans_qbs$cluster)

name_cluster <- name_cluster %>%
  mutate(adj = ifelse(cluster == 1, "GOOD", "BAD"))

name_cluster %>% gt() %>%
  cols_align(
    align = "center",
    columns = c(name, cluster, adj)
  ) %>%
  data_color(
    columns = cluster,
    method = "numeric",
    palette = "viridis"
  ) %>%
  cols_label(
    name = md("**QB Name**"),
    cluster = md("**Cluster**"),
    adj = md("**Designation**")
  ) %>%
  tab_header(
    title = md("**Clustering and Designation of 2022 NFL QBs With 100+ Pass Attempts**"),
    subtitle = "Utilizing K-Means Clustering, The Silhouette Method, and Principal Component Analysis"
  ) 