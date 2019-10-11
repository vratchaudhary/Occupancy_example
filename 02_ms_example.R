rm(list=ls())
##multi-season model
skink=read.csv('GrandSkinks.csv',skip=2,header=F,stringsAsFactors=F)    #  read detection-history and habitat covariate from csv
head(skink)
###Tor is the sitename; pasture or not is 18th column
#second row tells us about the five surveys
##we need to skip first two rows

dethist=skink[,2:16]  #  detection histories are in cols 2-16
nsites=nrow(dethist)   
unitnames=skink[,1]                                   #  unitnames=sitenames in col 1
nsurveyseason=rep(3,5)                              #  3 surveys per season for 5 seasons
habitat=rep('pasture',nsites)                       #  set up habitat covariate, assigning all as 'pasture'
habitat[skink[,18]==0]='tussock'                      #  set habitat='tussock' for sites with covariate=0
dethist[dethist=='-']=NA                            #  change '-' in det hist data to NA

data=createPao(dethist,unitcov=data.frame(habitat=as.factor(habitat)),nsurveyseason=nsurveyseason,title="Grand Skinks")
mods=list();i=0
i=i+1; mods[[i]] =occMod(model =list( psi~1,gamma~1,epsilon~1,p~1),data=data,type="do.1") #  run model (psi()gam()eps()p())
print(mods[[i]]$dmat,quote=F)
unique(mods[[i]]$real$psi)
unique(mods[[i]]$real$epsilon) #(Pr(occ site becomes unocc))
unique(mods[[i]]$real$gamma) #pr(site goes extinct)
print(unique(mods[[i]]$real$p))
#########model with covariates
i=i+1; mods[[i]]= occMod(model=list(psi~habitat,gamma~1,epsilon~1,p~1),data=data,type="do.1"); # run model psi(hab)...

print(mods[[i]]$dmat,quote=F)
unique(mods[[i]]$real$psi) 
#lets look at betas
unique(mods[[i]]$beta$psi)
unique(mods[[i]]$real$epsilon) #(Pr(occ site becomes unocc))
unique(mods[[i]]$real$gamma) #pr(site goes extinct)
print(unique(mods[[i]]$real$p)) 

##lets see which model is better
results<- createAicTable(mods)
print(summary(results))

##we can do model average if AIC are close together
modAvg(results,'psi')




