#ACP----
res.pca <- FactoMineR::PCA(player.attribute.final,  graph = FALSE)

#scree plot
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))

# graph des contributions des  variables
# Contributions of variables to PCx
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
#visualisation des axes 1 et 2
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)

#visualisation des axes 1 et 2 en limitant aux 10 variables qui contribuent le plus
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             select.var=list(contrib=10)
)

#graph des individus
#axe 1 et 2
fviz_pca_ind(res.pca, col.ind="cos2", geom = "point",alpha.ind=.5) +theme_minimal()
#axe 1 et 3 
fviz_pca_ind(res.pca,axes=c(1,3), col.ind="cos2", geom = "point",alpha.ind=.5) +theme_minimal()
#axe 2 et 3 
fviz_pca_ind(res.pca,axes=c(2,3), col.ind="cos2", geom = "point",alpha.ind=.5) +theme_minimal()

#kmeans ----
df.kmeans<-as.data.frame(res.pca$ind$coord) # 5 axes par defaut
#on teste avec k = 3
clusters <- kmeans(df.kmeans, 3, nstart = 100)