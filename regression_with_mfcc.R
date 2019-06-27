library(dplyr)
library(ISLR)

musics_data <- read.csv("/home/danielgondim/workspace-new/phd/experiments/qualificacao/logistic_mfcc_8tracks_without_users_complete_v2.csv")
musics_data_2 <- read.csv("/home/danielgondim/workspace-new/phd/experiments/qualificacao/logistic_mfcc_8tracks.csv")

for(i in c(1,2:ncol(musics_data))) {
  print(i)
  musics_data[,i] <- as.numeric(as.character(musics_data[,i]))
}

rownames(relaxed_8tracks) <- relaxed_8tracks[,1]
relaxed_8tracks <- relaxed_8tracks[,2:34]
head(relaxed_8tracks)

relax_column <- c(musics_data[,34])

musics_data_scaled <- scale(musics_data[,1:33])

musics_data_scaled <- cbind(musics_data_scaled, data.frame(relax = c(relax_column)))

fit <- lm(relax ~ ., data=musics_data)
summary(fit)

#com todas as variaveis independentes
smp_siz = floor(0.80*nrow(musics_data))
set.seed(171)
train_ind = sample(seq_len(nrow(musics_data)),size = smp_siz)
train =musics_data[train_ind,]
test=musics_data[-train_ind,]
model <- glm(relax~.,family=binomial(link='logit'),data=train)
summary(model)

#com todas as variaveis independentes (normalizado)
smp_siz = floor(0.80*nrow(musics_data_scaled))
set.seed(171)
train_ind = sample(seq_len(nrow(musics_data_scaled)),size = smp_siz)
train =musics_data_scaled[train_ind,]
test=musics_data_scaled[-train_ind,]
model_scaled <- glm(relax~.,family=binomial(link='logit'),data=train)
summary(model_scaled)

#k-means para 8tracks relaxed
relaxed_8tracks <- read.csv("/home/danielgondim/workspace-new/phd/experiments/qualificacao/logistic_mfcc_8tracks_relax_v2.csv")

for(i in 2:ncol(relaxed_8tracks)) {
  relaxed_8tracks[,i] <- as.numeric(as.character(relaxed_8tracks[,i]))
}

rownames(relaxed_8tracks) <- relaxed_8tracks[,1]
relaxed_8tracks <- relaxed_8tracks[,2:34]
head(relaxed_8tracks)

relaxed_8tracks_scaled <- scale(relaxed_8tracks)

library(factoextra)
library(NbClust)

fviz_nbclust(relaxed_8tracks_scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)+
  labs(subtitle = "Método do Joelho", title = "Número Ótimo de Grupos", x = "Número de grupos K", y = "Soma total dos erros quadrados")

fviz_nbclust(relaxed_8tracks_scaled, kmeans, method = "silhouette")+
  labs(subtitle = "Método da Silhueta", title = "Número Ótimo de Grupos", x = "Número de grupos K", y = "Largura média da silhueta")

set.seed(123)
fviz_nbclust(relaxed_8tracks_scaled, kmeans, nstart = 25, iter.max=30, method = "gap_stat", nboot = 100)+
  labs(subtitle = "Estatística Gap", title = "Número Ótimo de Grupos", x = "Número de grupos K", y = "Estatística gap (k)")

clusters_8tracks <- kmeans(relaxed_8tracks_scaled, 5)
str(clusters_8tracks)
clusters_8tracks$centers

centroids_8tracks <- clusters_8tracks$centers

library(MASS)
parcoord(clusters_8tracks$centers, var.label=TRUE, col = rownames(clusters_8tracks$centers))

clusters_8tracks$cluster
clusters_8tracks$size

relaxed_8tracks_clustered <- relaxed_8tracks
relaxed_8tracks_clustered$cluster <- clusters_8tracks$cluster

out_example <- split(relaxed_8tracks_clustered, f=relaxed_8tracks_clustered$cluster)
cluster_one <- out_example$`1`
cluster_two <- out_example$`2`
cluster_three <- out_example$`3`
cluster_four <- out_example$`4`
cluster_five <- out_example$`5`

cluster_one <- cluster_one[,1:33]
cluster_two <- cluster_two[,1:33]
cluster_three <- cluster_three[,1:33]
cluster_four <- cluster_four[,1:33]
cluster_five <- cluster_five[,1:33]

write.csv(cluster_one, file = "/home/danielgondim/workspace-new/phd/experiments/qualificacao/tese/8tracks_cluster_one.csv")
write.csv(cluster_two, file = "/home/danielgondim/workspace-new/phd/experiments/qualificacao/tese/8tracks_cluster_two.csv")
write.csv(cluster_three, file = "/home/danielgondim/workspace-new/phd/experiments/qualificacao/tese/8tracks_cluster_three.csv")
write.csv(cluster_four, file = "/home/danielgondim/workspace-new/phd/experiments/qualificacao/tese/8tracks_cluster_four.csv")
write.csv(cluster_five, file = "/home/danielgondim/workspace-new/phd/experiments/qualificacao/tese/8tracks_cluster_five.csv")

not_relax_8tracks <- musics_data_2[5887:11772,]

#modelo de regressao pro Cluster 1 (503 usuários)
cluster_one_regression <- read.csv("/home/danielgondim/workspace-new/phd/experiments/qualificacao/tese/relaxing-playlists-analysis/data/8tracks_cluster_one_v2.csv")
relax_column <- c(cluster_one_regression[,35])
cluster_one_regression_scaled <- scale(cluster_one_regression[,2:34])
cluster_one_regression_scaled <- cbind(cluster_one_regression_scaled, data.frame(relax = c(relax_column)))
smp_siz = floor(0.80*nrow(cluster_one_regression_scaled))
set.seed(171)
train_ind = sample(seq_len(nrow(cluster_one_regression_scaled)),size = smp_siz)
train =cluster_one_regression_scaled[train_ind,]
test=cluster_one_regression_scaled[-train_ind,]
model_cluster_one <- glm(relax~.,family=binomial(link='logit'),data=train)
summary(model_cluster_one)

#modelo de regressao pro Cluster 2 (591 usuários)
cluster_two_regression <- read.csv("/home/danielgondim/workspace-new/phd/experiments/qualificacao/tese/relaxing-playlists-analysis/data/8tracks_cluster_two_v2.csv")
relax_column <- c(cluster_two_regression[,35])
cluster_two_regression_scaled <- scale(cluster_two_regression[,2:34])
cluster_two_regression_scaled <- cbind(cluster_two_regression_scaled, data.frame(relax = c(relax_column)))
smp_siz = floor(0.80*nrow(cluster_two_regression_scaled))
set.seed(171)
train_ind = sample(seq_len(nrow(cluster_two_regression_scaled)),size = smp_siz)
train =cluster_two_regression_scaled[train_ind,]
test=cluster_two_regression_scaled[-train_ind,]
model_cluster_two <- glm(relax~.,family=binomial(link='logit'),data=train)
summary(model_cluster_two)

#modelo de regressao pro Cluster 3 (1961 usuários)
cluster_three_regression <- read.csv("/home/danielgondim/workspace-new/phd/experiments/qualificacao/tese/relaxing-playlists-analysis/data/8tracks_cluster_three_v2.csv")
relax_column <- c(cluster_three_regression[,35])
cluster_three_regression_scaled <- scale(cluster_three_regression[,2:34])
cluster_three_regression_scaled <- cbind(cluster_three_regression_scaled, data.frame(relax = c(relax_column)))
smp_siz = floor(0.80*nrow(cluster_three_regression_scaled))
set.seed(171)
train_ind = sample(seq_len(nrow(cluster_three_regression_scaled)),size = smp_siz)
train =cluster_three_regression_scaled[train_ind,]
test=cluster_three_regression_scaled[-train_ind,]
model_cluster_three <- glm(relax~.,family=binomial(link='logit'),data=train)
summary(model_cluster_three)

#modelo de regressao pro Cluster 4 (2221 usuários)
cluster_four_regression <- read.csv("/home/danielgondim/workspace-new/phd/experiments/qualificacao/tese/relaxing-playlists-analysis/data/8tracks_cluster_four_v2.csv")
relax_column <- c(cluster_four_regression[,35])
cluster_four_regression_scaled <- scale(cluster_four_regression[,2:34])
cluster_four_regression_scaled <- cbind(cluster_four_regression_scaled, data.frame(relax = c(relax_column)))
smp_siz = floor(0.80*nrow(cluster_four_regression_scaled))
set.seed(171)
train_ind = sample(seq_len(nrow(cluster_four_regression_scaled)),size = smp_siz)
train =cluster_four_regression_scaled[train_ind,]
test=cluster_four_regression_scaled[-train_ind,]
model_cluster_four <- glm(relax~.,family=binomial(link='logit'),data=train)
summary(model_cluster_four)

#modelo de regressao pro Cluster 5 (610 usuários)
cluster_five_regression <- read.csv("/home/danielgondim/workspace-new/phd/experiments/qualificacao/tese/relaxing-playlists-analysis/data/8tracks_cluster_five_v2.csv")
relax_column <- c(cluster_five_regression[,35])
cluster_five_regression_scaled <- scale(cluster_five_regression[,2:34])
cluster_five_regression_scaled <- cbind(cluster_five_regression_scaled, data.frame(relax = c(relax_column)))
smp_siz = floor(0.80*nrow(cluster_five_regression_scaled))
set.seed(171)
train_ind = sample(seq_len(nrow(cluster_five_regression_scaled)),size = smp_siz)
train =cluster_five_regression_scaled[train_ind,]
test=cluster_five_regression_scaled[-train_ind,]
model_cluster_five <- glm(relax~.,family=binomial(link='logit'),data=train)
summary(model_cluster_five)
