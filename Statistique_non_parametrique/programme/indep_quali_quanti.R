######### Fonction pour tester l'indépendence (quali-quanti).
# Généralisation du test KS à deux échantillons au cas où il y a plusieurs 
# échantillons déterminés par une variable qualitatives.
# A l'aide de la fonction ci-dessous, tester l'indépendance du Salaire horaire avec 
# la région d'habitation, le niveau d'études (on fera des regroupements de classe),
# et la catégorie professionnelle.
# Tester aussi l'égalité des espérances du Salaire horaire sachant ces variables qualitatives, 
# à l'aide de la commande oneway.test

AnovaF=function(X,F, N)
{
  X<-suppressWarnings(tiebreak(X))
  F<-as.factor(F)
  F<-as.numeric(F)
  p<-max(F)
  n<- length(X)
  Table<-table(F)
  T1 <- vector(mode = "numeric", length = N)
  for(i in 1:N)
  {
    Y<-runif(n)
    D<-vector(mode = "numeric", length = p)
    for(k in 1:p)
    {D[k]<-suppressWarnings(ks.test(Y,Y[F==k])$statistic)}
    T1[i]<-sum(Table*D^2)
  }
  FR<-ecdf(T1)
  D<-vector(mode = "numeric", length = p)
  for(i in 1:p)
  {D[i]<-suppressWarnings(ks.test(X,X[F==i])$statistic)}
  T<-sum(Table*D^2)
  Pval<- 1-FR(T)
  cat("Statistique de test :", T, "\n")
  cat("p-valeur :", Pval, "\n")
}

AnovaF(data$SAL_HOR, data$SEXE, 20000)