#FIRSTLY IMPORT THE DATASET#
#I am adding new column in excel file with name com_name because it is clearly seen 
#that 1-105:- CAFE(C)
#     106-240:- SALON(S)
#     241-340:- YOGA(Y)
df= read.csv("Data Test - Sheet1.csv")
names(df)#gives the names of the variables present in the dataset
#NEGLECTING ADDRESS, PHONE, LINK
df2=df[,c(-3,-4,-5)]
#Encoding the com_name as 1,2,3#
df2$com_name = factor(df2$com_name,
                         levels = c('c', 's', 'y'),
                    labels = c(1, 2, 3))
head(df2) 

#GROUPING CATEGORIES OF PINCODE and visualize them as which pin code have how much number of companies
tpin=table(as.factor(df2$Pin.Code))#HERE WE SEE THE FREQUENCY OF THE PIN CODE
Pin_names=dimnames(tpin)[[1]]#CHARACTERISING THE PINCODE
#count of success class for each PIN code
c_PINcode=NULL
for (x in Pin_names) {
  c_PINcode=c(c_PINcode,length(which(as.character(df2$Pin.Code)==x&df2$com_name==1)))# HERE PINCODE WHICH HAS COM_NAME= cafe
}
range(c_PINcode)
#here bar plot of the pin code which has cafe companies
barplot(c_PINcode,names.arg = Pin_names,las=3,xlab = "PINCODE",ylab = "com_name",ylim = c(0,12),cex.names = 0.6)

c_PINcode1=NULL
for (x in Pin_names) {
  c_PINcode1=c(c_PINcode1,length(which(as.character(df2$Pin.Code)==x&df2$com_name==2)))# HERE PINCODE WHICH HAS COM_NAME= saloon
}
range(c_PINcode1)
#here bar plot of the pin code which has saloon companies

barplot(c_PINcode1,names.arg = Pin_names,las=3,xlab = "PINCODE",ylab = "SALOON",ylim = c(0,12),cex.names = 0.6)


c_PINcode2=NULL
for (x in Pin_names) {
  c_PINcode2=c(c_PINcode2,length(which(as.character(df2$Pin.Code)==x&df2$com_name==3)))# HERE PINCODE WHICH HAS COM_NAME= yoga
}
range(c_PINcode2)
#here bar plot of the pin code which has YOGA companies

barplot(c_PINcode2,names.arg = Pin_names,las=3,xlab = "PINCODE",ylab = "YOGA",ylim = c(0,12),cex.names = 0.6)
###plotting the graphs for rating and numriview
frame=c(mean(df2[which(df2$com_name=="1"),]$Rating),mean(df2[which(df2$com_name=="2"),]$Rating),
        mean(df2[which(df2$com_name=="3"),]$Rating))
range(frame)
Com=c("1","2","3") 
par(mar=c(8,4,4,2)+0.1)
barplot(frame,names.arg = Com,xlab = "Com_label",ylab = "Rating",ylim = c(2,6),col="red",xlim = c(0,4)) 
  
frame1=c(mean(df2[which(df2$com_name=="1"),]$NumReview),mean(df2[which(df2$com_name=="2"),]$NumReview),
        mean(df2[which(df2$com_name=="3"),]$NumReview))
range(frame1)
Com1=c("1","2","3") 
par(mar=c(8,4,4,2)+0.1)
barplot(frame1,names.arg = Com1,xlab = "Com_label",ylab = "Numreview",ylim = c(150,3800),xlim = c(0,4),col = "green") 



#HERE I AM APPLYING VERY POPULAR UNSUPERVISED LEARNING MRTHOD#
########k-means cluster######
df3=df2[,c(3,4,6,7)];head(df3)#Taking the variable Rating; NumReview; Latitude; & Longitude
#now applying elbow method for finding the optimum k VALUE
set.seed(6)
wcss = vector()#WITHIN CLUSTER SUM SO SQUARE
for (i in 1:10) wcss[i] = sum(kmeans(df3, i)$withinss)
plot(1:10,wcss,type = 'b',main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

#AFTER VISUALISING THE PLOT; WE SEE K=3 WHERE THE GRAPH IS BENDING##
head(df3)
#NORMALIZING THE VARIABLES AND PUT IN DF4 NEW DATAFRAME 
df4=as.data.frame(scale(df2[,-c(1,2,5)],center = T,scale = T))
head(df4)
mod3=kmeans(df4,3)#HERE APPLYING K-MEANS CLUSTER WITH FULL DATASET
#PLOTTING THE CLUSTERS
library(cluster)
#here we see the clusters where various points are overlapp to each other because this is a 2d representaion of whole cluster
#
clusplot(df4,mod3$cluster,main = "2D representation of cluster",
         color = T,lines = 0,shade = T,labels = 2)

cc=mod3$centers#CLUSTER CENTROID
rownames(cc)=c("centroid1","centroid2","centroid3")#CHANGING ROW NAMES
#Distance of observations from cluster centroids in normalized coordinates
#matrix size=340 observ. * (3 cluster + 1 more column). Here 1 more column for
#ClusterID, On THIS COLUMN WE SEE WHICH OBSERV. ASSIGN TO WHICH CLUSTER

#initialize (340*4) matirx
DM6= matrix(NA,340, 4)
head(DM6)
rownames(DM6)=df2$Company.Name
colnames(DM6)=c("CLUSTERID","Dist.Clster1","Dist.Clster2","Dist.Clster3")
head(DM6)
#CLUSTER ASSIGNMENT
DM6[,1]=mod3$cluster;head(DM6)
##COMPUTE DISTANCES using euclidean distance##
for(i in 1:340){
  DM6[i,2]=dist(rbind(df4[i,],cc[1,]),method = "euclidean")
  DM6[i,3]=dist(rbind(df4[i,],cc[2,]),method = "euclidean")
  DM6[i,4]=dist(rbind(df4[i,],cc[3,]),method = "euclidean")
}
head(DM6)
###ORDER OF CLUSTER ID###
head(DM6[order(DM6[,1]),])
##CLUSTER CENTROID IN ORIGINAL SCALE
meanv=apply(df3[,c(1,2,3,4)],2,mean)
sdv=apply(df3[,c(1,2,3,4)],2,sd)
cc.org=t(apply(cc, 1, function(r)r*sdv+meanv))
cc.org

##distance between cluster centroid## HOW CLUSTERS ARE SEPARATED FROM EACH OTHER
dist(cc,method = "euclidean",diag = T,upper = T)#NORMALIZED SCALE, 
#CHECK THE SMALLER DISTANCE AMD SEE HOW FAR AWAY TO EACH OTHER
dist(cc.org,method = "euclidean",diag = T,upper = T)#ORIGINAL SCALE

##SUMMARY OF CLUSTER DISTANCE##
DM7=matrix(NA,4,2);DM7
rownames(DM7)=c("centroid1","centroid2","centroid3","overall")
colnames(DM7)=c("observation","Avg.dist_in_cluster")#IT ALSO GIVES THE IDEA ABOUT THE DISPERSION WITHIN THE PARTICULAR CLUSTER

DM7[1:3,1]=mod3$size
DM7[4,1]=sum(DM7[1:3,1])
for (i in 1:3) {
  DM7[i,2]=mean(DM6[which(DM6[,1]==i),])
  
}
DM7[4,2]=mean(DM7[1:3,2])
DM7#HERE WE SEE SMALLEST AVERAGE DISTANCE (SHOW MORE HOMOGENEOUS BECAUSE OF SMALLER DISPERSION)




###NOW SIMILARLY FOR ORIGINAL COORDINATE##
#SUMMARY OF CLUSTER DISTANCES IN ORIGINAL COORDINATE
#DISTANCE OD OBSERVATIONS FROM CLUSTER CENTROID IN ORIGINAL COORDINATE

DM8= matrix(NA,340, 4)
rownames(DM8)=df2$Company.Name
colnames(DM8)=c("CLUSTERID","Dist.Clster1","Dist.Clster2","Dist.Clster3")
#CLUSTER ASSIGNMENT
DM8[,1]=mod3$cluster;head(DM8)
head(df2)
df0=df2[,c(3,4,6,7)]
##COMPUTE DISTANCES##
for(i in 1:340){
  DM8[i,2]=dist(rbind(df0[i,],cc.org[1,]),method = "euclidean")
  DM8[i,3]=dist(rbind(df0[i,],cc.org[2,]),method = "euclidean")
  DM8[i,4]=dist(rbind(df0[i,],cc.org[3,]),method = "euclidean")
}
head(DM8)
DM7=cbind(DM7,Avg.dist_in_cluster_ORG=NA);DM7

for (i in 1:3) {
  DM7[i,3]=mean(DM8[which(DM8[,1]==i),])
  
}
DM7[4,3]=mean(DM7[1:3,3]);DM7
DM7#HERE WE SEE SMALLEST AVERAGE DISTANCE (SHOW MORE HOMOGENEOUS BECAUSE OF SMALLER DISPERSION)


#NOW ANALYSE THIS RESULT GRAPHICALLY
#USING PARALLEL COORDINATE PLOT:-
#THIS PARTICULAR PLOT IS GOING TO HELP US IN TERMS OF CHARACTERISING THESE CLUSTERS,

#SO FOR THIS WE HAVE TO SCALE CLUSTER CENTROID TO [0,1]

maxv=apply(cc.org,2,max)
minv=apply(cc.org,2,min)
cc.so1=as.data.frame(scale(cc.org,center = minv,scale = maxv-minv))

cc.so1

library(MASS)
par(mar=c(7.1,2.1,0.4,8.1),xpd=T,las=2,cex.axis=0.7)

parcoord(cc.so1,col = gray(0:5/6),lty = c(1:6),lwd=2,las=3)
axis(2,at=c(0,0.5,1),labels = c(0,0.5,1))
title(ylab = "scaled cluster centroids")
legend("bottomright",inset=c(-0.1,0),
       c("centroid1","centroid2","centroid3"),lty=c(1:3),cex = 0.6,x.intersp = 0.3,
       y.intersp = 0.5,col = gray(0:5/6),bty = "n",bg = "gray90")

#THIS PARTICULAR PLOT WE CAN SEE  3 LINES, EACH REPRESENTING CENTROIDS OF THEIR CLUUSTER,
#







































