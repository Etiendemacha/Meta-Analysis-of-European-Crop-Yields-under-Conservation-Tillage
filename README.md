# Meta-Analysis-of-European-Crop-Yields-under-Conservation-Tillage
#In Web of Science, major search terms were implemented in the following format: ridge-till* OR strip-till* OR mulch-till* OR no-till* OR direct drill* OR conservation-tillage. Since the study area is Europe, inclusion of countries was as well considered in the search. As the search option of Web of Science limits search terms to 50, the search was divided initially using the above search terms in combination (AND) with: (Croatia OR Hungary OR Romania OR Denmark OR Bulgaria OR Europe* OR San-Marino OR Serbia OR Slovakia). Other combinations (AND) of major search terms were performed with: (Slovenia OR Spain OR Sweden OR Switzerland OR Latvia OR Liechtenstein OR  Lithuania OR Luxembourg OR Albania OR Andorra OR Armenia OR Austria OR Azerbaijan OR Belarus OR Belgium OR Bosnia and Herzegovina OR Bulgaria OR Cyprus OR Czech* OR Denmark OR Estonia OR Finland OR France OR Georgia OR Germany OR Greece OR Hungary OR Iceland OR Ireland OR Italy OR Kazakhstan OR Kosovo OR Malta OR Moldova OR Monaco OR Montenegro OR Netherlands OR North-Macedonia OR Norway OR Poland OR Portugal OR Russia OR Turkey OR Ukraine OR United-Kingdom). 
#This selection leads to a totality of studies which are then sorted through for the data used in this meta-analysis.

#Setting up the source to read the data from the file location
source('C:/Users/Achankeng Etiendem/Desktop/Met-Analysis/Datasets/exerxcise3.R')
setwd("C:/Users/Achankeng Etiendem/Desktop/Model")

#The package metafor was utilized for the metaanalysis
library(metafor)
dat<- read.csv("datta.csv")

#This code was used to calculate the effect size (ratio of means). The full abbreviations can be found by downloading the excel sheet with data
es<- escalc(measure="ROM", n1i=SScs, m1i=mCS, sd1i=SDcs, n2i=SSct, m2i=mCT,sd2i=SDct,data=dat)

#Now I'll employ the below codes to run a random model a random model on strip till
#Then a mixed model by incoorporating variables to see whether it explains some of the heterogeneity seen in the random model
st<-dat[-c(1:120),]
View(st)
es<-escalc(measure="ROM", m1i=mCS, sd1i=SDcs, n1i=SScs, m2i=mCT, sd2i=SDct, n2i=SSct, data=st)
es

res<- rma(yi, vi, data=es)#same code for tillage data on ridge and no-till
res
res<- rma(yi, vi, mods= ~ Rotation*Texture-1, data=es)
res
res<- rma(yi, vi, mods= ~ Rotation*Crop-1, data=es)
res
res<- rma(yi, vi, mods= ~ Rotation:Crop-1, data=es)
res
res<- rma(yi, vi, mods= ~ Rotation:Texture-1, data=es)
res
res<- rma(yi, vi, mods= ~ Rotation:TD-1, data=es)
res
res<- rma(yi, vi, mods= ~ Rotation:RHSD-1, data=es)
res
res<- rma(yi, vi, mods= ~ Rotation:Duration-1, data=es)
res
res<- rma(yi, vi, mods= ~ Texture:Crop-1, data=es)
res<- rma(yi, vi, mods= ~ Texture:RHSD-1, data=es)
res
res<- rma(yi, vi, mods= ~ Texture:Duration -1, data=es)
res
res<- rma(yi, vi, mods= ~ Texture:TD -1, data=es)
res
res<- rma(yi, vi, mods= ~ TD:RHSD -1, data=es)
res<- rma(yi, vi, mods= ~ TD:Duration -1, data=es)
res
res<- rma(yi, vi, mods= ~ RHSD:Duration -1, data=es)
res

#This two way variable comparisons were done for all variables
#Also, the same procedure is folowed for the other tillage methods

par(mfrow=c(1,2))
forest(st$yi, cex= 0.9, slab=st$name, ci.lb =st$ci.lb, ci.ub =st$ci.ub, atransf=exp, xlab =" Response Ratio (RR)", data=st)
op<- par(cex=1, fonts=2)
text(-0.25,8.4, "Strip Till Factors")
text(0.6,8.4, "RR [95% CI]")

#I leave these raw codes that shows how excatly I tried different inputs to come up
#Its a repitive task to sort of get the best looking forest plots.

forest(rt$yi, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$name, atransf=exp, xlab = "Response Ratio (RR)",cex=0.9)
text(-1, 8.5, " Texture factor")
text(1, 9, " Texture factor")
text(1, 7.5, " Texture factor")
text(1, 8, " Texture factor")
text(1.15, 8.4, " Texture factor")
forest(rt$yi, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$name, atransf=exp, xlab = "Response Ratio (RR)",cex=0.9)
text(1.15, 8.4, " RR [95% CI]")
text(-1, 8.5, " Texture factor")
rt<-read.csv("rght.csv")
rt<-read.csv("rhght.csv")
forest(rt$yi, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$X, atransf=exp, xlab = "Response Ratio (RR)",cex=0.9)
forest(rt$estimate, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$X, atransf=exp, xlab = "Response Ratio (RR)",cex=0.9)
text(-1, 6, " Ridge Height")
text(-1, 8.5, " Ridge Height")
text(-1, 7.3, " Ridge Height")
text(-1.2, 7.3, " Ridge Height")
text(-0.9, 7.3, " Ridge Height")
text(1.1, 7.3, " Ridge Height")
text(1.05, 7.3, " Ridge Height")
forest(rt$estimate, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$X, atransf=exp, xlab = "Response Ratio (RR)",cex=0.9)
text(-0.9, 7.3, " Ridge Height")
text(1.05, 7.3, " RR [95% CI]")
rt<- read.csv("rotatn.csv")
forest(rt$yi, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$name, atransf=exp, xlab = "Response Ratio (RR)",cex=1)
text(-2.3, 18, " Rotation")
text(-2.5, 22.5, " Rotation")
text(2.5, 22.5, " RR [95% CI]")
text(2.5, 22, " RR [95% CI]")
text(2.4, 22.5, " RR [95% CI]")
forest(rt$yi, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$name, atransf=exp, xlab = "Response Ratio (RR)",cex=1)
text(2.4, 22.5, " RR [95% CI]")
text(-2.5, 22.5, " Rotation")
rt<- read.csv("Resid.csv")
forest(rt$estimate, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$X, atransf=exp, xlab = "Response Ratio (RR)",cex=1.1)
text(-2.5, 22.5, " Residue")
text(-1.3., 18.5, " Residue")
text(-1.3, 18.5, " Residue")
text(-1, 19, " Residue")
text(-2.2, 19, " Residue")
text(-2.2, 20, " Residue")
text(-0.8, 8.5, " Residue")
text(-0.9, 12, " Residue")
text(-1.3, 14.5, " Residue")
text(-1.5, 15, " Residue")
text(-1.7, 15.5, " Residue")
text(-1.8, 15.5, " Residue")
text(1.8, 15.5, " RR [95% CI]")
text(2, 15.5, " RR [95% CI]")
text(1.5,15.5 , " RR [95% CI]")
text(1.3,15.5 , " RR [95% CI]")
text(1,15.5 , " RR [95% CI]")
forest(rt$estimate, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$X, atransf=exp, xlab = "Response Ratio (RR)",cex=1.1)
text(1,15.5 , " RR [95% CI]")
text(-1.9, 15.5, " Residue")
rt<- read.csv("rotatn.csv")
forest(rt$yi, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$name, atransf=exp, xlab = "Response Ratio (RR)",cex=1.1)
text(2.4, 22.5, " RR [95% CI]") text(-2.5, 22.5, " Rotation")
text(2.4, 22.5, " RR [95% CI]")
text(-2.5, 22.5, " Rotation")
rt<- read.csv("crop.csv")
forest(rt$yi, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$name, atransf=exp, xlab = "Response Ratio (RR)",cex=1.1)
text(-2.5, 22.5, " Crop")
text(-1, 15, " Crop")
text(-1.5, 15.4, " Crop")
text(-2, 15.4, " Crop")
text(-2.5, 15.4, " Crop")
text(2.5, 15.4, " RR [95% CI]")
text(2.3, 15.4, " RR [95% CI]")
text(2.2, 15.4, " RR [95% CI]")
forest(rt$yi, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$name, atransf=exp, xlab = "Response Ratio (RR)",cex=1.1)
text(2.3, 15.4, " RR [95% CI]")
text(-2.5, 15.4, " Crop")
forest(rt$yi, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$name, atransf=exp, xlab = "Response Ratio (RR)",cex=1.1)
text(-2.5, 15.4, " Crop")
text(2.15, 15.4, " RR [95% CI]")
rt<- read.csv("texture.csv")
forest(rt$yi, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$name, atransf=exp, xlab = "Response Ratio (RR)",cex=1.1)
text(2.15, 15.4, " RR [95% CI]")
text(2.15, 22.4, " RR [95% CI]")
text(2.15, 18.4, " RR [95% CI]")
text(2.15, 18.9, " RR [95% CI]")
text(2.15, 19, " RR [95% CI]")
text(2.15, 19.4, " RR [95% CI]")
text(-2.15, 19.4, " RR [95% CI]")
forest(rt$yi, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$name, atransf=exp, xlab = "Response Ratio (RR)",cex=1.1)
text(-2.15, 19.4, " Texture")
text(2.15, 19.4, " RR [95% CI]")
rt<- read.csv("Climate.csv")
forest(rt$yi, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$name, atransf=exp, xlab = "Response Ratio (RR)",cex=1.1)
text(0.5, 8.5, " RR [95% CI]")
text(0.9, 9, " RR [95% CI]")
text(1.1, 9.5, " RR [95% CI]")
text(1.3, 9.5, " RR [95% CI]")
text(-1.3, 9.5, " RR [95% CI]")
text(-1.8, 9.5, " RR [95% CI]")
forest(rt$yi, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$name, atransf=exp, xlab = "Response Ratio (RR)",cex=1.1)
text(-1.8, 9.5, " RR [95% CI]")
text(1.3, 9.5, " RR [95% CI]")
text(1.5, 9.5, " RR [95% CI]")
forest(rt$yi, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$name, atransf=exp, xlab = "Response Ratio (RR)",cex=1.1)
text(1.5, 9.5, " RR [95% CI]")
text(-2, 9.5, " Climate")
rt<- read.csv("texture.csv")
forest(rt$yi, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$name, atransf=exp, xlab = "Response Ratio (RR)",cex=1.1)
forest(rt$yi, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$name, atransf=exp, xlab = "Response Ratio (RR)",cex=1)
forest(rt$yi, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$name, atransf=exp, xlab = "Response Ratio (RR)",cex=1.1)
rt<-read.csv("texture.csv")
forest(rt$yi, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$name, atransf=exp, xlab = "Response Ratio (RR)",cex=1.1)
rt<-read.csv("texture.csv")
forest(rt$yi, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$name, atransf=exp, xlab = "Response Ratio (RR)",cex=1.1)
rt<-read.csv("texture.csv")
forest(rt$yi, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$name, atransf=exp, xlab = "Response Ratio (RR)",cex=1.1)
text(-2.15, 19.4, " Texture")
text(2.15, 19.4, " RR [95% CI]")
rt<- read.csv("Crop.csv")
forest(rt$yi, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$name, atransf=exp, xlab = "Response Ratio (RR)",cex=1.1)
text(-2.5, 15.4, " Crop")
forest(rt$yi, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$name, atransf=exp, xlab = "Response Ratio (RR)",cex=1.1)
text(-2.5, 18, " Crop")
text(2.5, 18, "RR [95% CI]")
text(2.3, 18, "RR [95% CI]")
forest(rt$yi, ci.lb = rt$ci.lb, ci.ub = rt$ci.ub, slab=rt$name, atransf=exp, xlab = "Response Ratio (RR)",cex=1.1)
text(2.2, 18, "RR [95% CI]")
text(-2.5, 18, " Crop")

