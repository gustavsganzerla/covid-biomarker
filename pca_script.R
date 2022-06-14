####This script is intended to perform a PCA in order to reduce the dimensionality of cytokines
####in order to predict outcome of covid-19 patients

##data
cleveland <- read.csv("~/Documents/covid/COVID_AI/irish/covid_non_covid_all.csv")

cleveland <- na.omit(cleveland)

##pca 
pca <- prcomp(cleveland[, 2:30], scale = TRUE, center = TRUE)

summary(pca)
attributes(pca)

pca$center

plot(pca, type = "l")
biplot(pca, scale = 0)

cleveland2 <- cbind(cleveland, pca$x[, 1:3])

##
library(ggplot2)

ggplot(data = cleveland, aes(PC1, PC2, col = Clinical.Outcome, fill = Clinical.Outcome))+
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5)+
  geom_point(shape = 21, col = "black")

library(factoextra)
fviz_eig(pca)
fviz_pca_ind(pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)



###Determining loading scores
###this means the variables that have a higher impact in each component

loading_scores <- pca$rotation[,2] ##the 2 means the component we are checking
scores <- abs(loading_scores)  

scores_rank <- sort(scores, decreasing = TRUE)
top_ten <- names(scores_rank[1:10])

top_ten
