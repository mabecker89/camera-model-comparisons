# R Compare camera models from paired comparisons in 2019

d<-read.csv("data/Mammals by series 2019 May 2020.csv")
t<-read.csv("data/Camera operating days since 2009 ALL May 2020.csv")
d$Deployment<-as.character(d$Deployment)
jul<-format(as.Date("1Jan09",format="%d%b%y")+0:(ncol(t)-2),"%j")  # Julian day for each column (other than the first = Deployment-Year) in t
d$Julian<-format(as.Date(as.character(d$DateTime)),"%j")
d$DaysSince2009<-as.numeric(floor(julian(strptime(d$DateTime,format="%Y-%m-%d %H:%M:%S",tz="GMT"), origin=strptime("2009-01-01 00:00:00",format="%Y-%m-%d %H:%M:%S"))))
d$Season<-ifelse(d$Julian>=106 & d$Julian<=288,"Summer","Winter")

# Find new model deployments and their old-model matches
# Assumes the pair comparison cameras' deployment names end in the camera make/model
j<-substr(d$Deployment,nchar(d$Deployment)-2,nchar(d$Deployment))
dep.HF2<-sort(unique(d$Deployment[j=="HF2"]))
i<-regexpr("_",dep.HF2)
dep.HF2.match<-substr(dep.HF2,1,i-1)
dep.CUDDE<-sort(unique(d$Deployment[j=="DDE"]))
i<-regexpr("_",dep.CUDDE)
dep.CUDDE.match<-substr(dep.CUDDE,1,i-1)
dep.HP2X<-sort(unique(d$Deployment[j=="P2X"]))
i<-regexpr("_",dep.HP2X)
dep.HP2X.match<-substr(dep.HP2X,1,i-1)

# Figure out common operating times for each pair and summarize
common.time.HF2<-array(0,c(length(dep.HF2),dim(t)[2]-1))  # To store common operating time of each pair (days since 2009)
time.model.HF2<-array(0,c(length(dep.HF2),2))  # To store total operating days of {original, new model}
rownames(common.time.HF2)<-rownames(time.model.HF2)<-dep.HF2.match
common.time.CUDDE<-array(0,c(length(dep.CUDDE),dim(t)[2]-1))
time.model.CUDDE<-array(0,c(length(dep.CUDDE),2))  # To store total operating days of {original, new model}
rownames(common.time.CUDDE)<-rownames(time.model.CUDDE)<-dep.CUDDE.match
common.time.HP2X<-array(0,c(length(dep.HP2X),dim(t)[2]-1))
time.model.HP2X<-array(0,c(length(dep.HP2X),2))  # To store total operating days of {original, new model}
rownames(common.time.HP2X)<-rownames(time.model.HP2X)<-dep.HP2X.match
for (i in 1:length(dep.HF2)) {
  t1<-t[t$X==paste(dep.HF2[i],"2019",sep="_"),]
  time.model.HF2[i,2]<-sum(as.numeric(t1[2:ncol(t1)]))
  t2<-t[t$X==paste(dep.HF2.match[i],"2019",sep="_"),]
  time.model.HF2[i,1]<-sum(as.numeric(t2[2:ncol(t2)]))
  common.time.HF2[i,]<-as.numeric(t1[2:ncol(t1)])*as.numeric(t2[2:ncol(t2)])  # Only 1 where both are 1
}
q.HF2<-data.frame(Deployment=dep.HF2.match,Comparison.Model="HF2",Days.PC900=time.model.HF2[,1],Days.HF2=time.model.HF2[,2],Combined.total.days=rowSums(common.time.HF2),Combined.summer.days=rowSums(common.time.HF2[,which(jul>=106 & jul<=288)]),Combined.winter.days=rowSums(common.time.HF2[,which(jul<106 | jul>288)]))
for (i in 1:length(dep.CUDDE)) {
  t1<-t[t$X==paste(dep.CUDDE[i],"2019",sep="_"),]
  time.model.CUDDE[i,2]<-sum(as.numeric(t1[2:ncol(t1)]))
  t2<-t[t$X==paste(dep.CUDDE.match[i],"2019",sep="_"),]
  time.model.CUDDE[i,1]<-sum(as.numeric(t2[2:ncol(t2)]))
  common.time.CUDDE[i,]<-as.numeric(t1[2:ncol(t1)])*as.numeric(t2[2:ncol(t2)])  # Only 1 where both are 1
}
q.CUDDE<-data.frame(Deployment=dep.CUDDE.match,Comparison.Model="CUDDE",Days.PC900=time.model.CUDDE[,1],Days.CUDDE=time.model.CUDDE[,2],Combined.total.days=rowSums(common.time.CUDDE),Combined.summer.days=rowSums(common.time.CUDDE[,which(jul>=106 & jul<=288)]),Combined.winter.days=rowSums(common.time.CUDDE[,which(jul<106 | jul>288)]))
for (i in 1:length(dep.HP2X)) {
  t1<-t[t$X==paste(dep.HP2X[i],"2019",sep="_"),]
  time.model.HP2X[i,2]<-sum(as.numeric(t1[2:ncol(t1)]))
  t2<-t[t$X==paste(dep.HP2X.match[i],"2019",sep="_"),]
  time.model.HP2X[i,1]<-sum(as.numeric(t2[2:ncol(t2)]))
  common.time.HP2X[i,]<-as.numeric(t1[2:ncol(t1)])*as.numeric(t2[2:ncol(t2)])  # Only 1 where both are 1
}
q.HP2X<-data.frame(Deployment=dep.HP2X.match,Comparison.Model="HP2X",Days.PC900=time.model.HP2X[,1],Days.HP2X=time.model.HP2X[,2],Combined.total.days=rowSums(common.time.HP2X),Combined.summer.days=sum(common.time.HP2X[,which(jul>=106 & jul<=288)]),Combined.winter.days=sum(common.time.HP2X[,which(jul<106 | jul>288)]))

# Extract series for each pair in common time and convert into [deployment, species] by season
# 1. HF2
d1<-d[d$Deployment %in% dep.HF2.match==TRUE & d$Year==2019,]
i1<-NULL
for (i in 1:nrow(d1)) {  # Check each series for being within common time for that pair
  if (common.time.HF2[d1$Deployment[i],d1$DaysSince2009[i]]==1) i1<-c(i1,i)  # Series to keep
}
d.HF2.match<-d1[i1,]  # Series for all old models in paired deployments, at common times only
d2<-d[d$Deployment %in% dep.HF2==TRUE & d$Year==2019,]
i1<-NULL
for (i in 1:nrow(d2)) {  # Check each series for being within common time for that pair
  if (common.time.HF2[dep.HF2.match[match(d2$Deployment[i],dep.HF2)],d2$DaysSince2009[i]]==1) i1<-c(i1,i)  # Series to keep.  Matching needed because rownames for common.time... are the deployment name without the new model added
}
d.HF2<-d2[i1,]  # Series for all new models in paired deployments, at common times only
# Corrections - found (I think...) by looking at any paired series that had different species
d.HF2$Species[d.HF2$Deployment=="ABMI-0542-NE_HF2" & d.HF2$DaysSince2009==3766]<-"Moose"  # Correct this one mistake
# Add deer to white-tailed deer (only deer species present here) and Wolves, Coyotes and Allies to Coyote (just because they are more common - no direct match in paired camera for that ID)
d.HF2$Species<-ifelse(as.character(d.HF2$Species)=="Deer","White-tailed Deer",as.character(d.HF2$Species))
d.HF2.match$Species<-ifelse(as.character(d.HF2.match$Species)=="Deer","White-tailed Deer",as.character(d.HF2.match$Species))
d.HF2$Species<-ifelse(as.character(d.HF2$Species)=="Wolves, Coyotes and Allies","Coyote",as.character(d.HF2$Species))
d.HF2.match$Species<-ifelse(as.character(d.HF2.match$Species)=="Wolves, Coyotes and Allies","Coyote",as.character(d.HF2.match$Species))
# 2. CUDDE
d1<-d[d$Deployment %in% dep.CUDDE.match==TRUE & d$Year==2019,]
i1<-NULL
for (i in 1:nrow(d1)) {  # Check each series for being within common time for that pair
  if (common.time.CUDDE[d1$Deployment[i],d1$DaysSince2009[i]]==1) i1<-c(i1,i)  # Series to keep
}
d.CUDDE.match<-d1[i1,]  # Series for all old models in paired deployments, at common times only
d2<-d[d$Deployment %in% dep.CUDDE==TRUE & d$Year==2019,]
i1<-NULL
for (i in 1:nrow(d2)) {  # Check each series for being within common time for that pair
  if (common.time.CUDDE[dep.CUDDE.match[match(d2$Deployment[i],dep.CUDDE)],d2$DaysSince2009[i]]==1) i1<-c(i1,i)  # Series to keep.  Matching needed because rownames for common.time... are the deployment name without the new model added
}
d.CUDDE<-d2[i1,]  # Series for all new models in paired deployments, at common times only

# Summarize number of photos and duration*individuals for each deployment, species (by season)
# 1. HF2
d1.1<-aggregate(Photos~Species+Season+Deployment,data=d.HF2.match,sum)
d1.2<-aggregate(I(Duration*Individuals)~Species+Season+Deployment,data=d.HF2.match,sum)
colnames(d1.2)[ncol(d1.2)]<-"DurInd"
d2.1<-aggregate(Photos~Species+Season+Deployment,data=d.HF2,sum)
d2.2<-aggregate(I(Duration*Individuals)~Species+Season+Deployment,data=d.HF2,sum)
colnames(d2.2)[ncol(d1.2)]<-"DurInd"
d2.1$DepMatch<-dep.HF2.match[match(d2.1$Deployment,dep.HF2)]  # Deployments named without the "_HF2" addition
d2.2$DepMatch<-dep.HF2.match[match(d2.2$Deployment,dep.HF2)]  # Deployments named without the "_HF2" addition
x<-sort(unique(c(paste(d1.1$Deployment,d1.1$Species,d1.1$Season,sep="_"),paste(d2.1$DepMatch,d2.1$Species,d2.1$Season,sep="_"),
                 paste(d1.2$Deployment,d1.1$Species,d1.1$Season,sep="_"),paste(d2.2$DepMatch,d2.1$Species,d2.1$Season,sep="_"))))  # Deployment-species-seasons in all summary files (so that 0's are included in aggregated file)
q<-data.frame(DepSpSeas=x,Photos.PC900=d1.1$Photos[match(x,paste(d1.1$Deployment,d1.1$Species,d1.1$Season,sep="_"))], Photos.HF2=d2.1$Photos[match(x,paste(d2.1$DepMatch,d2.1$Species,d2.1$Season,sep="_"))],
              DurInd.PC900=d1.2$DurInd[match(x,paste(d1.2$Deployment,d1.2$Species,d1.2$Season,sep="_"))], DurInd.HF2=d2.2$DurInd[match(x,paste(d2.2$DepMatch,d2.2$Species,d2.2$Season,sep="_"))])
for (i in 2:5) q[,i]<-ifelse(is.na(q[,i]),0,q[,i])
q$Photos.HF2<-ifelse(is.na(q$Photos.HF2),0,q$Photos.HF2)
q$Photos.PC900<-ifelse(is.na(q$Photos.PC900),0,q$Photos.PC900)
q$Photos.HF2<-ifelse(is.na(q$Photos.HF2),0,q$Photos.HF2)
q$DurInd.PC900<-ifelse(is.na(q$DurInd.PC900),0,q$DurInd.PC900)
q$DurInd.HF2<-ifelse(is.na(q$DurInd.HF2),0,q$DurInd.HF2)
q$DepSpSeas<-as.character(q$DepSpSeas)
j<-unlist(lapply(gregexpr("_",q$DepSpSeas),function(x) x[1]))  # Parse deployment and sp-season
q$Deployment<-substr(q$DepSpSeas,1,j-1)
q$SpSeas<-substr(q$DepSpSeas,j+1,nchar(q$DepSpSeas))
# Then summarize camera model differences for each SpSeas
dep.list<-sort(unique(q$Deployment))
spseas.list<-sort(unique(q$SpSeas))

# Bootstrap CIs with deployment pair as unit
niter<-1000
bs1<-array(0,c(length(spseas.list),4,niter))  # For spseas, {photos 2 models, durind 2 models}, each iteration
dimnames(bs1)[[1]]<-spseas.list
for (iter in 1:niter) {
  s<-sample(1:length(dep.list),length(dep.list),replace=TRUE)
  q1<-NULL
  for (j in 1:length(s))
  q1<-rbind(q1,q[q$Deployment==dep.list[s[j]],])
  x<-by(q1$Photos.PC900,q1$SpSeas,mean)
  bs1[names(x),1,iter]<-as.numeric(x)
  x<-by(q1$Photos.HF2,q1$SpSeas,mean)
  bs1[names(x),2,iter]<-as.numeric(x)
  x<-by(q1$DurInd.PC900,q1$SpSeas,mean)
  bs1[names(x),3,iter]<-as.numeric(x)
  x<-by(q1$DurInd.HF2,q1$SpSeas,mean)
  bs1[names(x),4,iter]<-as.numeric(x)
}
bs.sum<-array(0,c(length(spseas.list),2,3))  # SpSeas, HF2 as % of PC900 photos and durind {median, q5, q95}
dimnames(bs.sum)<-list(spseas.list,c("Photos.pc","DurInd.pc"),c("Median","q5","q95"))
for (sp in 1:length(spseas.list)) {
  ratio1<-bs1[sp,2,]/bs1[sp,1,]*100  # HF2 photos as percent of PC900 photos
  ratio1<-ratio1[!is.na(ratio1)]  # NAs for 0/0
  bs.sum[sp,1,]<-quantile(ratio1,c(0.5,0.05,0.95))
  ratio2<-bs1[sp,4,]/bs1[sp,3,]*100  # HF2 DurInd as percent of PC900 DurInd
  ratio2<-ratio2[!is.na(ratio2)]  # NAs for 0/0
  bs.sum[sp,2,]<-quantile(ratio2,c(0.5,0.05,0.95))
}
match(spseas.list,sort(unique(q$SpSeas)))  # Make sure these are in same order
q.sum<-data.frame(SpSeas=sort(unique(q$SpSeas)),
                  nPairs=as.numeric(by(q$Deployment,q$SpSeas,length)),Photos.PC900.total=as.numeric(by(q$Photos.PC900,q$SpSeas,sum)),Photos.HF2.total=as.numeric(by(q$Photos.HF2,q$SpSeas,sum)),
                  Photos.HF2.as.pc.of.PC900.median=bs.sum[,1,1],Photos.HF2.as.pc.of.PC900.q5=bs.sum[,1,2],Photos.HF2.as.pc.of.PC900.q95=bs.sum[,1,3],
                  DurInd.PC900.total=as.numeric(by(q$DurInd.PC900,q$SpSeas,sum)),DurInd.HF2.total=as.numeric(by(q$DurInd.HF2,q$SpSeas,sum)),
                  DurInd.HF2.as.pc.of.PC900.median=bs.sum[,2,1],DurInd.HF2.as.pc.of.PC900.q5=bs.sum[,2,2],DurInd.HF2.as.pc.of.PC900.q95=bs.sum[,2,3])
write.table(q.sum,file="C:/Dave/ABMI/Cameras/2020 analysis/Camera model comparison/Camera model comparison HF2 versus PC900 May 2020.csv",sep=",",row.names=FALSE)
# 2. CUDDE
d1.1<-aggregate(Photos~Species+Season+Deployment,data=d.CUDDE.match,sum)
d1.2<-aggregate(I(Duration*Individuals)~Species+Season+Deployment,data=d.CUDDE.match,sum)
colnames(d1.2)[ncol(d1.2)]<-"DurInd"
d2.1<-aggregate(Photos~Species+Season+Deployment,data=d.CUDDE,sum)
d2.2<-aggregate(I(Duration*Individuals)~Species+Season+Deployment,data=d.CUDDE,sum)
colnames(d2.2)[ncol(d1.2)]<-"DurInd"
d2.1$DepMatch<-dep.CUDDE.match[match(d2.1$Deployment,dep.CUDDE)]  # Deployments named without the "_CUDDE" addition
d2.2$DepMatch<-dep.CUDDE.match[match(d2.2$Deployment,dep.CUDDE)]  # Deployments named without the "_CUDDE" addition
x<-sort(unique(c(paste(d1.1$Deployment,d1.1$Species,d1.1$Season,sep="_"),paste(d2.1$DepMatch,d2.1$Species,d2.1$Season,sep="_"),
                 paste(d1.2$Deployment,d1.1$Species,d1.1$Season,sep="_"),paste(d2.2$DepMatch,d2.1$Species,d2.1$Season,sep="_"))))  # Deployment-species-seasons in all summary files (so that 0's are included in aggregated file)
q<-data.frame(DepSpSeas=x,Photos.PC900=d1.1$Photos[match(x,paste(d1.1$Deployment,d1.1$Species,d1.1$Season,sep="_"))], Photos.CUDDE=d2.1$Photos[match(x,paste(d2.1$DepMatch,d2.1$Species,d2.1$Season,sep="_"))],
              DurInd.PC900=d1.2$DurInd[match(x,paste(d1.2$Deployment,d1.2$Species,d1.2$Season,sep="_"))], DurInd.CUDDE=d2.2$DurInd[match(x,paste(d2.2$DepMatch,d2.2$Species,d2.2$Season,sep="_"))])
for (i in 2:5) q[,i]<-ifelse(is.na(q[,i]),0,q[,i])
q$Photos.CUDDE<-ifelse(is.na(q$Photos.CUDDE),0,q$Photos.CUDDE)
q$Photos.PC900<-ifelse(is.na(q$Photos.PC900),0,q$Photos.PC900)
q$DurInd.CUDDE<-ifelse(is.na(q$DurInd.CUDDE),0,q$DurInd.CUDDE)
q$DurInd.PC900<-ifelse(is.na(q$DurInd.PC900),0,q$DurInd.PC900)
q$DepSpSeas<-as.character(q$DepSpSeas)
j<-unlist(lapply(gregexpr("_",q$DepSpSeas),function(x) x[1]))  # Parse deployment and sp-season
q$Deployment<-substr(q$DepSpSeas,1,j-1)
q$SpSeas<-substr(q$DepSpSeas,j+1,nchar(q$DepSpSeas))
# Then summarize camera model differences for each SpSeas
dep.list<-sort(unique(q$Deployment))
spseas.list<-sort(unique(q$SpSeas))
# Bootstrap CIs with deployment pair as unit
niter<-1000
bs1<-array(0,c(length(spseas.list),4,niter))  # For spseas, {photos 2 models, durind 2 models}, each iteration
dimnames(bs1)[[1]]<-spseas.list
for (iter in 1:niter) {
  s<-sample(1:length(dep.list),length(dep.list),replace=TRUE)
  q1<-NULL
  for (j in 1:length(s)) 	q1<-rbind(q1,q[q$Deployment==dep.list[s[j]],])
  x<-by(q1$Photos.PC900,q1$SpSeas,mean)
  bs1[names(x),1,iter]<-as.numeric(x)
  x<-by(q1$Photos.CUDDE,q1$SpSeas,mean)
  bs1[names(x),2,iter]<-as.numeric(x)
  x<-by(q1$DurInd.PC900,q1$SpSeas,mean)
  bs1[names(x),3,iter]<-as.numeric(x)
  x<-by(q1$DurInd.CUDDE,q1$SpSeas,mean)
  bs1[names(x),4,iter]<-as.numeric(x)
}
bs.sum<-array(0,c(length(spseas.list),2,3))  # SpSeas, CUDDE as % of PC900 photos and durind {median, q5, q95}
dimnames(bs.sum)<-list(spseas.list,c("Photos.pc","DurInd.pc"),c("Median","q5","q95"))
for (sp in 1:length(spseas.list)) {
  ratio1<-bs1[sp,2,]/bs1[sp,1,]*100  # CUDDE photos as percent of PC900 photos
  ratio1<-ratio1[!is.na(ratio1)]  # NAs for 0/0
  bs.sum[sp,1,]<-quantile(ratio1,c(0.5,0.05,0.95))
  ratio2<-bs1[sp,4,]/bs1[sp,3,]*100  # CUDDE DurInd as percent of PC900 DurInd
  ratio2<-ratio2[!is.na(ratio2)]  # NAs for 0/0
  bs.sum[sp,2,]<-quantile(ratio2,c(0.5,0.05,0.95))
}
match(spseas.list,sort(unique(q$SpSeas)))  # Make sure these are in same order
q.sum<-data.frame(SpSeas=sort(unique(q$SpSeas)),nPairs=as.numeric(by(q$Deployment,q$SpSeas,length)),Photos.PC900.total=as.numeric(by(q$Photos.PC900,q$SpSeas,sum)),Photos.CUDDE.total=as.numeric(by(q$Photos.CUDDE,q$SpSeas,sum)),
                  Photos.CUDDE.as.pc.of.PC900.median=bs.sum[,1,1],Photos.CUDDE.as.pc.of.PC900.q5=bs.sum[,1,2],Photos.CUDDE.as.pc.of.PC900.q95=bs.sum[,1,3],
                  DurInd.PC900.total=as.numeric(by(q$DurInd.PC900,q$SpSeas,sum)),DurInd.CUDDE.total=as.numeric(by(q$DurInd.CUDDE,q$SpSeas,sum)),
                  DurInd.CUDDE.as.pc.of.PC900.median=bs.sum[,2,1],DurInd.CUDDE.as.pc.of.PC900.q5=bs.sum[,2,2],DurInd.CUDDE.as.pc.of.PC900.q95=bs.sum[,2,3])
# DON'T USE, because the WildTrax data is wrong for 1 of the 3 Cuddeback cameras!  I don't remember what the issue was, or whether it was resolved, but the Cuddeback cameras clearly miss most small mammals anyway
#write.table(q.sum,file="C:/Dave/ABMI/Cameras/2020 analysis/Camera model comparison/Camera model comparison CUDDE versus PC900 May 2020.csv",sep=",",row.names=FALSE)




