#On voulait expliquer le prduit national d'Haiti pour la periode allant de 1987 a 2021, on a choisi trois variables qui sont l'investissement direct etranger, le commerce net qu'on designe comme l'exportation nette et le taux d'inflation. on attend en relation positive entre les exportations nettes et le taux de croissance du PIB, tout comme entre les investissements directs etrangers. mais entre le taux d'inflation et le taux de croissance du PIB il existe une relation negative. on a decide de choisir ces trois variables parce qu'on pense qu'elles ont beaucoup a faire dans la mauvaise performance de l'econmie haitienne. 
#les investissements directs etrangers devraient ameliorer l'economie en mettant a la disposition des agents economiques plus de fonds. Les exportations nettes devraient apporter de devises dans l'economie mais on attend a n'importe quoi puisqu'on sait que l'economie importe plus q'elle exporte. Pour le taux d'inflation, il devrait influence le PIB negativement puisqu'il fait diminuer le pouvoir d'achat des agents des lors il decourage les demandeurs.

library(wbstats)
#Importation
taux_croiss <- wb_data(indicator = "NY.GDP.MKTP.KD.ZG" , country = "HTI" , start_date = 1987 , end_date = 2021)
IDE <- wb_data(indicator = "BX.KLT.DINV.WD.GD.ZS" , country = "HTI" , start_date = 1987 , end_date = 2021)
inflation <- wb_data(indicator = "FP.CPI.TOTL.ZG" , country = "HTI" , start_date = 1987 , end_date = 2021)
commerce_net <- wb_data(indicator = "BN.GSR.GNFS.CD" , country = "HTI" , start_date = 1987 , end_date = 2021) 

#Cleaning up
taux_croissance <- taux_croiss[ ,c(3,4,5)]
IDE <- IDE[ ,c(5)]
commerce_net <- commerce_net[ ,c(5)]
inf <- inflation[ ,c(5)]

#Forme matricielle des donnees
mat <- cbind(taux_croissance,IDE,commerce_net,inf)

#Renommons les colonnes
colnames(mat) <- c("Pays","Annee","Taux_de_croissance","Investissement_direct_etranger","Exportations_nettes","Taux_dinflation")

#Realisons la regression lineaire
reg_dj <- lm(Taux_de_croissance~Investissement_direct_etranger+Exportations_nettes+Taux_dinflation,mat)
reg_gt <- summary(reg_dj)
reg_coefficient <-coefficients(reg_gt)
residu_de_regression <-residuals(reg_gt)

#Trouver f statistics
ftics <-reg_gt$fstatistic
rtics <-reg_gt$r.squared
stats_tab <- cbind(ftics,rtics,reg_coefficient)


#Installation du package car
library(car)

#Realisons un nuage de points pour chacune des variables independantes
scatterplot(Taux_de_croissance~Investissement_direct_etranger,mat,regLine=list(method=lm,lty=1,lwd=2,col="yellow"))
scatterplot(Taux_de_croissance~Exportations_nettes,mat,regLine=list(method=lm,lty=1,lwd=2,col="orange"))
scatterplot(Taux_de_croissance~Taux_dinflation,mat,regLine=list(method=lm,lty=1,lwd=2,col="green"))


#Realiser un graphique en nuage de points mettant en lien les valeurs residuelles et les valeurs estimeees
plot(reg_dj,which=1)

#CE graphe de nuge de points des valeurs residuelles(difference entre valeurs observees et valeur estimes)est a tendance positve et non lineaire.Ce graphe presente une homocedasticite des points, c est a dire a dispersion egale.