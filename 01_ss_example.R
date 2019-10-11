#install.packages ("www.mbr-pwrc.usgs.gov/software/bin/RPresence_2.12.25.tar.gz")

library(RPresence)
bob<-read.csv("Bobcat-3day.csv")
head(bob)
tail(bob)
#we just need the survey part for our detection history
dethist<- bob[,-1] #remove sites
#lets create a df for sites only
sitenames<- bob[,1]
nsites=nrow(dethist)#number of sites

nsrvys= ncol(dethist)

###reading covariate data
cov1<- read.csv("Bobcat_site_covar_3Day_class.csv",as.is = T,nrows=nsites) #site specific covariates
###same as grid specific in oour examples
head(cov1)

tail(cov1)
##remove sitename
cov1<-as.data.frame( cov1[,-1])

cov2<- data.frame(SRVY=as.factor(rep(1:nsrvys,each=nsites))) #creating a survey specific
#covariate that repeats number of surveys (1 to 7) nsites (1839) times. That means eac
#site was surveyed seven times and prob of detection may vary with each survey for some
#reason, cloudy day, obserevr etc

##create a presence dataframe
data=createPao(dethist,unitcov=cov1,survcov=cov2,title="Bobcat example",unitnames=sitenames)
######
###############3
mods=list();i=0
i=i+1; mods[[i]] =occMod(model = list(psi~1,p~1),data=data, type="so") #dot model

###lets check some results
##print design matrix
print(mods[[i]]$dmat$p,quote=F)
###lets write a covariate model where detection varies between detection in trails versus nontrails

i=i+1; mods[[i]] =occMod(model = list(psi~1,p~Trail),data=data, type="so") #dot model

print(mods[[i]]$dmat$p,quote=F)
##lets look at estimates
unique(mods[[i]]$real$psi)
unique(mods[[i]]$real$p) ##which one is higher
unique(mods[[i]]$beta$p) ##sign of the covariate
##which model is better supported
results<- createAicTable(mods)
print(summary(results))## trail model is better supported by a lot
##########detection varies  by distance 
i=i+1; mods[[i]]=occMod(model=list(psi~1,p~Det_dist),data=data,paoname=NULL,type="so")
print(mods[[i]]$dmat$p,quote=F)
print(mods[[i]]$beta$p)     #  look at betas
###detection decreases with distance

##lets plot
dist=cov1$Det_dist         #  distance covariate for each site
b1=mods[[i]]$beta$p$est[1]
b2=mods[[i]]$beta$p$est[2]  #  beta (b1,b2) estimates from model
p=plogis(b1 + b2*dist)#  estimated p1 for each site 
plot(dist,p,ylim=c(0,1))


####try other covariates. 