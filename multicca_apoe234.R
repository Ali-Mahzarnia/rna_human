library(dplyr)

# noreadcsf=c(148,152,161,314,318,327) # dont read csf already in matlab
load('connectivity.rda')
load('RNA_data.rda')
load('response.rda')


temp=connectivity[,,1]
#temp=connectivity[,,1]
indexlower=lower.tri(temp, diag=FALSE)
indexlowertrue=which(indexlower==TRUE)
temp=temp[indexlower]
len=sum(indexlower)  



#riskfactors=matrix(NA,  dim(response)[1], (dim(response)[2]-1))
riskfactors=matrix(NA,  (dim(response)[1]), (dim(response)[2]-2)) #
riskfactors= response[,2:(dim(response)[2]-1)]


riskfactors=riskfactors%>%select(genotype, sex,age, risk_for_ad)




Gene=riskfactors$genotype
unique(Gene)
# Gene[Gene=="APOE22"]=2
Gene[Gene=="APOE23"]=2
Gene[Gene=="APOE33"]=3
Gene[Gene=="APOE34"]=4
Gene[Gene=="APOE44"]=4

riskfactors$genotype=as.numeric(Gene)

Sex=riskfactors$sex
Sex[Sex=="M"]=-1
Sex[Sex=="F"]=1
riskfactors$sex=as.numeric(Sex)

# Diet=riskfactors$Diet
# Diet[Diet=="Control"]=-1
# Diet[Diet=="HFD"]=1
# riskfactors$Diet=as.numeric(Diet)

# riskfactors$Age_Months=as.numeric(riskfactors$Age_Months)

riskfactors=as.data.frame(riskfactors)
riskfactors[] <- lapply(riskfactors, as.numeric)
class(riskfactors)






RNA_data=as.data.frame(RNA_data)
RNA_data[] <- lapply(RNA_data, as.numeric)
class(RNA_data)





RNA_data_orig=RNA_data
inddrna=0
for (i in 1:dim(RNA_data)[2]) if(sd(RNA_data[,i])==0 ) {inddrna=rbind(inddrna,i);  cat ( i , sd(RNA_data[,i]), "\n" );}
if (length(inddrna)>1){
  inddrna=inddrna[2:dim(inddrna)[1]]
  RNA_data=RNA_data[,-inddrna] }



inddz=0
for (i in 1:dim(riskfactors)[2]) if(sd(riskfactors[,i])==0 ) {inddz=rbind(indd,i);  cat ( i , sd(riskfactors[,i]), "\n" );}
if (length(inddz)>1){
  inddz=inddz[2:dim(inddz)[1]]
  riskfactors=riskfactors[,-inddz]
}


temp=connectivity[,,1]
temp=connectivity[,,1]
indexlower=lower.tri(temp, diag=FALSE)
indexlowertrue=which(indexlower==TRUE)
temp=temp[indexlower]
len=sum(indexlower)  

image=matrix(NA,  dim(connectivity)[3], len) # -6 becasue of cfs removal

for (i in 1:dim(connectivity)[3]){
  temp=connectivity[,,i]
  temp=connectivity[,,i]
  indexlower=lower.tri(temp, diag=FALSE)
  temp=temp[indexlower]
  image[i,]=temp
}
dim(image)





indd=0
for (i in 1:dim(image)[2]) if(sd(image[,i])==0 ) {indd=rbind(indd,i);  cat ( i , sd(image[,i]), "\n" );}
if (length(indd)>1){
  indd=indd[2:dim(indd)[1]]
  image=image[,-indd] }









#lets run
## Not run:
#install.packages("PMA")
#install.packages("https://gitlab.oit.duke.edu/am983/PMA2/-/archive/master/PMA2-master.tar.gz", repos = NULL, type="source")
library(PMA)
xlist= list (riskfactors, image, RNA_data)
#perm.out <- MultiCCA.permute(xlist, nperm=100)

#perm.out$penalties
###or load the data after this
# penalty=perm.out$bestpenalties
# penalty[2]=sqrt(dim(image)[2])*2.5
# penalty[2]=sqrt(dim(riskfactors)[2])*2.5

# penalty=c(sqrt(dim(riskfactors)[2]),sqrt(dim(riskfactors)[2])*1.5,4*sqrt(dim(riskfactors)[2]) )
# penalty=c(sqrt(dim(riskfactors)[2]),sqrt(dim(riskfactors)[2])*1.5,4*sqrt(dim(riskfactors)[2]) )

# penalty=c(sqrt(dim(riskfactors)[2]), 2, 2)

######penalty finder:
set.seed(238)
xlam=sqrt(dim(xlist[[1]])[2])
ylam=sqrt(dim(xlist[[2]])[2])
zlam=sqrt(dim(xlist[[3]])[2])
penalty=c(xlam,ylam,zlam)
out <- MultiCCA(xlist, type=c("standard", "standard", "standard"),
                penalty=penalty, ncomponents=1,  standardize = TRUE)

zerox=mean(out$ws[[1]]==0)


while (zerox==0) {
  oldxlam=xlam
  xlam=0.95*xlam
  penalty=c(xlam,ylam,zlam)
  out <- MultiCCA(xlist, type=c("standard", "standard", "standard"),
                  penalty=penalty, ncomponents=1,  standardize = TRUE)
  zerox=mean(out$ws[[1]]==0)
  print(zerox)
}
xlam=oldxlam



zeroz=mean(out$ws[[3]]==0)

desired_sparsity_z=0.9922 #.9995 and 0.9905343

while (zeroz<desired_sparsity_z) {
  oldzlam=zlam
  zlam=0.95*zlam
  penalty=c(xlam,ylam,zlam)
  out <- MultiCCA(xlist, type=c("standard", "standard", "standard"),
                  penalty=penalty, ncomponents=1,  standardize = TRUE)
  zeroz=mean(out$ws[[3]]==0)
  print(zeroz)
}



# zeroy=mean(out$ws[[2]]==0)
nonzeroy=sum(out$ws[[2]]!=0)
desired_non_sparsity_y=30
while (nonzeroy>desired_non_sparsity_y) {
  oldylam=ylam
  ylam=0.95*ylam
  penalty=c(xlam,ylam,zlam)
  out <- MultiCCA(xlist, type=c("standard", "standard", "standard"),
                  penalty=penalty, ncomponents=1,  standardize = TRUE)
  nonzeroy=sum(out$ws[[2]]!=0)
  cat(" ", nonzeroy,"  " ,out$cors, "\n" )
}
#ylam=oldylam

####bad results cor of 1.45 and only 4 roi with 200 rna



# penalty=c(1.039577, 1.32360850 , 13.045145)

out <- MultiCCA(xlist, type=c("standard", "standard", "standard"),
                penalty=penalty, ncomponents=1,  standardize = TRUE)






print(out$cors)


out$penalty

ws=out$ws
mean(ws[[3]]==0)
sum(out$ws[[3]]!=0)



#perm.out2 <- MultiCCA.permute(xlist, nperm=500, penalties = as.matrix(penalty) )


dim(ws[[3]])
dim(RNA_data)

RNA_result=as.data.frame(cbind(colnames(RNA_data)[ws[[3]]!=0],as.numeric(ws[[3]][ws[[3]]!=0] )))
RNA_result_ordered=RNA_result[order(abs(as.numeric(RNA_result$V2)), decreasing=T),]

library(xlsx)
write.xlsx2(RNA_result_ordered, "RNA_result_ordered.xlsx" )

u=out$ws[[2]]

length(u)
dim(image)


sum(u==0)
#len=length(u)
sum(u!=0)

sum(u==0)/len #sparsity 

uout=matrix(NA, length(u)+length(indd),1 )
#put those zeros back
uout[indd]=0
uout[-indd]=u
#make it square again





indd
indexlowertrue=which(indexlower==TRUE)
temp[indd]
indexlowertrue[indd]

connectivityexample=connectivity[,,1]

connectivityexample[indexlowertrue[indd]] ##yes the're them
connectivityexample[indexlowertrue[indd]]="zeros" # lest make them known for a special word
indexofzeros=which(connectivityexample=="zeros", arr.ind=TRUE)

indexofzeros[,1]
indexofzeros

# #lest check really quick:
# for (j in 1:dim(indexofzeros)[1]) { 
# for (i in 1:dim(connectivity)[3]) {  cat(  "subject", i, "at position", indexofzeros[j,] , connectivity[matrix(c(indexofzeros[j,],i),1,3)] , "\n")
# }
# } ## yes theyre all odly zeros

#results of connectivities that matter:
nonzeroindex=which(uout!=0)

connectivityexample=connectivity[,,1]
connectivityexample[]=0
connectivitvals=connectivityexample
nonzerouout=uout[uout!=0]
for (i in 1:length(nonzeroindex)) {
  connectivityexample[indexlowertrue[nonzeroindex[i]]]=c("nonzero") # lest make them known for a special word
  connectivitvals[indexlowertrue[nonzeroindex[i]]]=nonzerouout[i] #store their coefitient values
}



library('igraph');
connectivitvalsones=connectivitvals
t=which(connectivitvalsones!=0, arr.ind=TRUE)
t <- cbind(t, connectivitvals[which(connectivitvals!=0,arr.ind=TRUE)]) 
t.graph=graph.data.frame(t,directed=F)
E(t.graph)$color <- ifelse(E(t.graph)$V3 > 0,'blue','red') 
#t.names <- colnames(cor.matrix)[as.numeric(V(t.graph)$name)]
minC <- rep(-Inf, vcount(t.graph))
maxC <- rep(Inf, vcount(t.graph))
minC[1] <- maxC[1] <- 0
l <- layout_with_fr(t.graph, minx=minC, maxx=maxC,
                    miny=minC, maxy=maxC)      

pathnames='/Users/ali/Desktop/Aug23/human_rna/rna_human/anatomyInfo_whiston_new.csv'
datanmes=read.csv(pathnames, header = TRUE, sep = ",", quote = "")
datanmes$ROI

# noreadcsf=c(148,152,161,314,318,327) # dont read csf already in matlab

#datanmes=datanmes[-noreadcsf]

datanmess=datanmes$ROI#[-noreadcsf] # remove csf
#datanmess=datanmes$ROI



par(mfrow=c(1,1))

#set.vertex.attribute(t.graph, "name", value=datanmes$ROI   )


jpeg("nets.jpeg", units="in", width=15, height=7, res=300)  

plot(t.graph, layout=l, 
     rescale=T,
     asp=0,
     edge.arrow.size=0.1, 
     vertex.label.cex=0.8, 
     vertex.label.family="Helvetica",
     vertex.label.font=4,
     #vertex.label=t.names,
     vertex.shape="circle", 
     vertex.size=5, 
     vertex.color="white",
     vertex.label.color="black", 
     #edge.color=E(t.graph)$color, ##do not need this since E(t.graph)$color is already defined.
     edge.width=as.integer(cut(abs(E(t.graph)$V3), breaks = 5)))

dev.off()
connectivitvals=connectivitvals+t(connectivitvals) #symetric


nonzeroposition=which(connectivityexample=="nonzero", arr.ind=TRUE)
getwd()
filename=paste(getwd(), "/", "valandpos.mat", sep = "")
#writeMat(filename, nonzeroposition = nonzeroposition, connectivitvals = connectivitvals , oddzeroposition=indexofzeros)




subnets=groups(components(t.graph))
subnetsresults=vector(mode = "list", length = length(subnets))
colsumabs=colSums(abs(connectivitvals))
colsum=colSums(connectivitvals)

leftright=datanmes$Bigpart#[-noreadcsf]




####################3
for (i in 1:length(subnets)) {
  temp=subnets[[i]]
  temp=as.numeric(temp)
  net=matrix(NA,8,length(temp) )
  net[1,]=as.numeric(temp)
  tt=as.numeric(net[1,])
  #tt=c(1,200)
  #indofleftright=tt>=164
  #net[5,][indofleftright]="Right"
  #net[5,][!indofleftright]="Left"
  
  
  net[2,]=datanmess[temp]
  net[5,]=leftright[temp]
  net[1,]=paste(net[1,],net[5,])
  net[3,]= as.numeric( colsum[temp]   )
  net[4,]= as.numeric( colsumabs[temp]   )
  net[6,]=sum(as.numeric(net[4,]))
  net[7,]=sum(as.numeric(net[3,]))
  for (j in 1:length( net[8,])) {
    tempindex=which(datanmes$ROI %in% net[2,j]  )
    if (net[5,j]=="Right" ) {net[8,j]= max(tempindex) } else { net[8,j]=min(tempindex) }
  }
  subnetsresults[[i]]=net 
}

#install.packages("xlsx")
library(xlsx)


for (i in 1:length(subnetsresults)){
  net=t(subnetsresults[[i]])
  write.xlsx2(net, "nets.xlsx", sheetName =  paste0(i), append=TRUE )
}


net_new=matrix(NA, length(subnetsresults),4)


for (j in 1:dim(net_new)[1]) {
  temps=subnetsresults[[j]]
  net_new[j,1]=j
  net_new[j,2]= paste(temps[8,], collapse = ", ")
  net_new[j,3] = paste(paste(temps[5,],temps[2,]), collapse = ", ")
  net_new[j,4] = paste(temps[7,1])
  
}
colnames(net_new)=c("Sub-Network", "Region Number", "Region Name", "Sub-Network Weight")
write.xlsx2(net_new, "net_new.xlsx" )



# install.packages("vioplot")
library("vioplot")


for (i in 1:length(subnets)) {
  temp=subnets[[i]]
  temp=as.numeric(temp)
  net=matrix(NA,8,length(temp) )
  net[2,]=datanmess[temp]
  net[1,]=as.numeric(temp)
  net[3,]= as.numeric( colsumabs[temp]   )
  net[4,]= as.numeric( colsum[temp]   )
  tt=as.numeric(net[1,])
  #tt=c(1,200)
  indofleftright=tt>=164
  net[5,][indofleftright]="Right"
  net[5,][!indofleftright]="Left"
  net[6,]=sum(as.numeric(net[4,]))
  net[7,]=sum(as.numeric(net[3,]))
  for (j in 1:length( net[8,])) {
    tempindex=which(datanmes$ROI %in% net[2,j]  )
    if (net[5,j]=="Right" ) {net[8,j]= max(tempindex) } else { net[8,j]=min(tempindex) }
  }
  subnetsresults[[i]]=net
}





for (i in 1:length(subnets)) {
  temp=subnets[[i]]
  temp=as.numeric(temp)
  net=matrix(NA,8,length(temp) )
  net[2,]=datanmess[temp]
  net[1,]=as.numeric(temp)
  net[3,]= as.numeric( colsumabs[temp]   )
  net[4,]= as.numeric( colsum[temp]   )
  tt=as.numeric(net[1,])
  #tt=c(1,200)
  indofleftright=tt>=164
  net[5,][indofleftright]="Right"
  net[5,][!indofleftright]="Left"
  net[6,]=sum(as.numeric(net[4,]))
  net[7,]=sum(as.numeric(net[3,]))
  for (j in 1:length( net[8,])) {
    tempindex=which(datanmes$ROI %in% net[2,j]  )
    if (net[5,j]=="Right" ) {net[8,j]= max(tempindex) } else { net[8,j]=min(tempindex) }
  }
  subnetsresults[[i]]=net 
}



#for (i in 1:length(subnetsresults)) {
#  net=subnetsresults[i]
#  print(net[[1]][1:2,])
#}


for (i in 1:length(subnetsresults)) {
  net=subnetsresults[i]
  cat( i,'th sub-net: the summation of all edges in this sub-net is' ,sum(as.numeric(net[[1]][4,])), 'and summation of absolut values of all edges in this subnet is', sum(as.numeric(net[[1]][3,])),'\n')
  cat(  'the fsirst row is the Region #, second row is the name of Region, the third row is the sum of absulote values of the edges of each region, and the last row is the sum of edges of each region \n')
  print(net)
  cat( '\n \n \n')
}


capture.output(subnetsresults, file = "subnet.txt")


write.csv(subnetsresults, row.names = T)




#install.packages("xlsx")
library(xlsx)

# 
# for (i in 1:length(subnetsresults)){
#   net=subnetsresults[[i]]
#   write.xlsx2(net, "ssir.xlsx", sheetName =  paste0(i), append=TRUE )
# }


# install.packages("vioplot")
library("vioplot")





# path3="/Users/ali/Desktop/mar/mice/apoe234/resultsresponse_arrayraw.mat"
# data3=readMat(path3)
# responseraw=data3$response.arrayraw
# 
# riskfactors=matrix(NA,  dim(responseraw)[1], (dim(responseraw)[2]-1))
#sum(riskfactors[,2]==3)

# riskfactors=response

# #subjnameofconnectivity=data$subjlist
# 
# for (i in 1:dim(riskfactors)[1]) {
#   ind=which(response[i,1]==subjnameofconnectivity)
#   #if (i!=ind) cat("here", i)
#   riskfactors[ind,]=responseraw[ind, 2:(dim(response)[2])     ]
# }



### histograms of nets
histdata=matrix(0,length(subnetsresults),dim(connectivity)[3])
#t

for (j in 1:length(subnetsresults)){
  net=subnetsresults[[j]]
  subnetsuperset=as.numeric(net[1,])
  for (i in 1:dim(t)[1]){
    if ( t[i,][1]%in%subnetsuperset){
      for (k in 1:dim(connectivity)[3]) {
        temp=connectivity[,,k]
        histdata[j,k]=histdata[j,k]+ temp[t[i,][1],t[i,][2]]+temp[t[i,][2],t[i,][1]]
      }
    }
  }
}
histdata=cbind(seq(1,length(subnetsresults)),histdata)



##split plots.
histdatasplit=histdata[,2:dim(histdata)[2]]
#uniqgeneafterreg=unique(riskfactors[,5])
apoe2=histdatasplit[,riskfactors[,5]>2] 
apoe3=histdatasplit[,riskfactors[,5]==3]
apoe4=histdatasplit[,riskfactors[,5]==4]

library(plotrix)
GenoTypes=as.factor(riskfactors$genotype)
#GenoTypes[GenoTypes==2]="APOE22";GenoTypes[GenoTypes==3]="APOE33";GenoTypes[GenoTypes==4]="APOE44";
Sex=as.factor(riskfactors$sex); #Sex[Sex==1]="1M"; Sex[Sex==2]="2F"; 
#Diet=as.factor(riskfactors$Diet)
#Dietname=Diet; Diet[Diet==1]="Control"; Diet[Diet==2]="HFD";
#Sexname=Sex; Sexname[Sexname=="1M"]="Male" ;  Sexname[Sexname=="2F"]="Female";

###### here specify   
xaxisvar=GenoTypes; 
xaxis="GenoTypes" ;
xaxisvarnames=GenoTypes;
brightnessvar=Sex; 
brightness="GenoTypes";
#######


names(xaxisvar)=xaxisvarnames











jpeg("violin.jpeg", units="in", width=12, height=10, res=300)  

sqrt=sqrt(length(subnetsresults))
if (sqrt>4) sqrt=3
par(mfrow = c(ceiling(sqrt), ceiling(length(subnetsresults)/sqrt)))
for (j in 1:length(subnetsresults)){
  #cols <- brewer.pal(8,'Set2')[6:8]
  cols=c(rgb(0,0,1),rgb(0,1,0),rgb(1,0,0,))
  cols[length(unique(xaxisvar))]=rgb(1,0,0,)
  cols=adjustcolor(cols, alpha.f = 0.1)
  legend=paste0("Net ",j, ". Networks medians: ( ");
  for (m in 1:length(unique(xaxisvar))) {
    if (m==1) {legend=paste(legend,  round(median(histdatasplit[j,xaxisvar==sort(unique(xaxisvar))[m]]),2), sep =""  )}
    else {legend=paste(legend,  round(median(histdatasplit[j,xaxisvar==sort(unique(xaxisvar))[m]]),2), sep =","  )}
  }
  legend=paste(legend,")")
  vioplot(histdatasplit[j,]~ xaxisvar , plotCentre = "dot", col =cols,  ylab="net weight" ,
          main = legend, xlab ="" )
  a=subnetsresults[[j]]; a=as.table(a);a= as.numeric(a[1,]); 
  #mtext(paste("R",a, collapse=', '), side=4, cex=0.5)
  
  for (l in 1:length(sort(unique(brightnessvar), decreasing=T))) {
    cols=c(rgb(0,0,1),rgb(0,1,0),rgb(1,0,0,))
    cols[length(unique(xaxisvar))]=rgb(1,0,0,)
    tempnum=length(sort(unique(brightnessvar), decreasing=T))
    cols=adjustcolor(cols, alpha.f = l/length(sort(unique(brightnessvar), decreasing=T)))
    stripchart(histdatasplit[j,brightnessvar==sort(unique(brightnessvar), decreasing=T)[l]]~xaxisvar[brightnessvar==sort(unique(brightnessvar), decreasing=T)[l]], vertical = TRUE, method = "jitter", points=50,
               pch = (17:length(sort(unique(brightnessvar), decreasing=T))), add = TRUE, col =cols , offset=0, cex = 1.2)
    cat(sort(unique(brightnessvar), decreasing=T)[l],"  "  ,l/length(unique(brightnessvar, "\n") )  )
  }
  
  mtext(paste("Dark Jitt=", sort(unique(brightnessvar), decreasing=T)[l], brightness) , side=1, cex=0.6)
  
  for (k in 1:length(unique(xaxisvar))) {
    ablineclip(h=median(histdatasplit[j,xaxisvar==sort(unique(xaxisvar))[k]]), col=cols[k], lwd = 1, x1=0.4, x2=k, lty="dotted")
    
  }
}

dev.off()










##################################3
##############################3
############################3
####### making graph match the original atlas with csf

# 
# library('igraph');
# #connectivitvalsones=connectivitvals
# t=which(connectivitvalsones!=0, arr.ind=TRUE)
# 
# for (i in 1:(dim(t)[1]*dim(t)[2])) {
#   for (j in noreadcsf) {
#     if (j <= t[i]) {
#       t[i] = t[i] + 1
#     }
#   }
#   #t[i] = t[i] - 1
# }
# 
# t <- cbind(t, connectivitvals[which(connectivitvals!=0,arr.ind=TRUE)]) 
# t.graph=graph.data.frame(t,directed=F)
# E(t.graph)$color <- ifelse(E(t.graph)$V3 > 0,'blue','red') 
# #t.names <- colnames(cor.matrix)[as.numeric(V(t.graph)$name)]
# minC <- rep(-Inf, vcount(t.graph))
# maxC <- rep(Inf, vcount(t.graph))
# minC[1] <- maxC[1] <- 0
# l <- layout_with_fr(t.graph, minx=minC, maxx=maxC,
#                     miny=minC, maxy=maxC)      
# 
# pathnames='/Users/ali/Desktop/Jul/apoe/mouse_anatomy.csv'
# datanmes=read.csv(pathnames, header = TRUE, sep = ",", quote = "")
# datanmes$ROI
# 
# noreadcsf=c(148,152,161,314,318,327) # dont read csf already in matlab
# 
# #datanmes=datanmes[-noreadcsf]
# 
# datanmess=datanmes$ROI[-noreadcsf] # remove csf
# #datanmess=datanmes$ROI
# 
# 
# 
# par(mfrow=c(1,1))
# 
# #set.vertex.attribute(t.graph, "name", value=datanmes$ROI   )
# 
# 
# jpeg("nets2.jpeg", units="in", width=10, height=5, res=300)  
# 
# plot(t.graph, layout=l, 
#      rescale=T,
#      asp=0,
#      edge.arrow.size=0.1, 
#      vertex.label.cex=0.8, 
#      vertex.label.family="Helvetica",
#      vertex.label.font=4,
#      #vertex.label=t.names,
#      vertex.shape="circle", 
#      vertex.size=5, 
#      vertex.color="deepskyblue2",
#      vertex.label.color="black", 
#      #edge.color=E(t.graph)$color, ##do not need this since E(t.graph)$color is already defined.
#      edge.width=as.integer(cut(abs(E(t.graph)$V3), breaks = 5)))
# 
# dev.off()
# .

library(brainconn2)

brainconn(atlas ="Desikan84", conmat=connectivitvals, 
          view="ortho", node.size =2, 
          node.color = "pink", 
          edge.width = 2, edge.color="blue", 
          edge.alpha = 0.65,
          edge.color.weighted = T,
          scale.edge.width=T,
          labels = F,
          all.nodes =F, 
          show.legend = T, 
          label.size=6, background.alpha=1, 
          label.edge.weight=F)
library(ggplot2)
ggsave("mtcars.png", plot = last_plot(), device='png', scale=3, width=4, height=6, unit=c("in"), dpi=200)


##############new net

