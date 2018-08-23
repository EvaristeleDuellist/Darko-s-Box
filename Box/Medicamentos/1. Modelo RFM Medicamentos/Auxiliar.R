tmp <- BD[1:50000,] %>%
    select(Id_Cliente, Fecha,Venta_Bruta) %>%
    mutate(Recencia=as.numeric(difftime(as.Date("2018-04-30"),Fecha,units="days"))) %>%
    group_by(Id_Cliente) %>%
    summarise(Recencia = min(Recencia),
              Frecuencia=n_distinct(Fecha),
              Monto=sum(Venta_Bruta, na.rm = T)/Frecuencia
    ) %>%
    mutate(R_Score=cut2(Recencia, g = 5),
           F_Score=cut2(Frecuencia, g = 5),
           M_Score=cut2(Monto, g = 5))

levels(tmp$R_Score)<-seq(5,1, by = -1)
levels(tmp$F_Score)<-seq(1,5)
levels(tmp$M_Score)<-seq(1,5)


Groceries
trans<-tmp %>% select(Id_Cliente, Categoria, prod_nombre) %>% 
    
    
dummy(trans$prod_nombre)

letters <- c( "a", "a", "b", "c", "d", "e", "f", "g", "h", "b", "b" )
dummy( as.character(trans$prod_nombre) )
%>% 
    mutate(Categoria=as.factor(Categoria),
           prod_nombre=as.factor(prod_nombre)) %>% 
    as("transactions")

summary(trans)
rules <- apriori(trans, parameter = list(supp = 0.00001, conf = 0.8))
options(digits=2)
inspect(rules[1:5])

itemFrequency(trans[1:5,])
itemFrequencyPlot(Groceries, topN=20)
itemFrequencyPlot(trans, topN=20)

grules = apriori(trans, parameter = list(support = 0.0001))
summary(grules)

class(Groceries)
plot(grules, method="grouped", measure="lift")







aux1<-tmp %>% 
    mutate(ID=seq(1:nrow(tmp))) %>% 
    select(-Id_Cliente)
aux1<-as.data.frame(sapply(aux1[, c(1:7)], as.numeric))
row.names(aux1)<-aux1$ID
pca <- prcomp(aux1[,4:6],  scale = T, center = T)
summary(pca)

km_clusters <- kmeans(pca$x,5)
# fviz_pca_biplot(pca, geom = "point", habillage =  as.factor(km_clusters$cluster), addEllipses=TRUE, ellipse.level=0.95)

aux1$Cluster=as.factor(km_clusters$cluster)

aux2 <-aux1 %>% group_by(cluster) %>% 
    summarise(Recencia=mean())

ggplot(aux1, aes(x=Cluster, y=Recencia)) +
    geom_boxplot()+
    geom_hline(yintercept = mean(aux1$Recencia), color="darkgreen", linetype="dashed")+
    coord_flip()+
    theme_minimal()+
    labs(title="Comparación de recencias por Clúster")


rm(km_clusters, pca, tmp, x1, aux1)




?kmeans
??viz_cluster
