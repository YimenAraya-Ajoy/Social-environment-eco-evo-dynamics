# SIMULATE DATA
IBsim <- function(x){
  # initialize lists
  dAll<-list(IndData=list(), PopData=list())
  d<-list(Ind=list(), Pop=list())  # 
  dyears<-list() # a list of ind data for each year 
  
  
  #---- Among year variation ----
  # Simulate among year variation for each of n.years where
  # VYs is the among year variation in survival
  # VYr is the among year variation in reproduction
  # VYrs is the among-year covariance between survival and reproduction
  MY<-matrix(c(x$VYs, x$VYrs, x$VYrs, x$VYr),2,2)  # generate variance-covariance matrix
  Y<- as.data.frame(cbind(1:x$n.years, mvrnorm(x$n.years, c(0,0), MY)))
  colnames(Y) <- c("year", "Ys", "Yr")   # Ys/Yr are annual variation in survival and repro
  
  
  ##############################
  # Create founder population
  ##############################
  
  #---- Intrinsic values ----
  # Simulate individual intrinsic values for the founder population
  # nA is the number of adults in the founder population
  # VIr is the among individual variance in number of recruits
  # VIrs is the among individual variance in survival
  # VIrs is a pheniotype that could affect reproduction or survival
  
  # adults:
  df1<-data.frame(ID=1:(x$nA+x$nJ),                      # individual ID
                  Ir=rnorm(x$nA+x$nJ, 0, x$VIr),       # intrinsic reproductive value    
                  Is=rnorm(x$nA+x$nJ, 0, x$VIs),       # intrinsic survival value
                  z=rnorm(x$nA+x$nJ, 2, sqrt(x$Vaz)), # phenotypic trait value
                  age=c(rep(1, each=x$nA),rep(0, each=x$nJ)))                          # set age of adults to 1  
  
  # first breeding year individuals: 
  
  # Combine the adult and juvenile data into a data frame for year 1:
  d1 <- df1 %>%                   # combine adult and juvenile data 
    mutate(z_bar=mean(z),                     # calculate avg phenotype in year 1
           year=1,                            # year 
           N=nrow(.))
  d1$n<-log(d1$N)
  # total population size in year 1
  
  
  #---- Residual values ----
  # Simulate the stochasticity that affects an individual's rep. or survival in a given year 
  MR<-matrix(c(x$VEs, x$VErs, x$VErs, x$VEr),2,2) # variance-covariance matrix
  E<-mvrnorm(nrow(d1), c(0,0), MR)
  colnames(E) <- c("Es", "Er")                    # added column names for subsetting (matches)
  
  
  
  #---- Model survival ----
  # calculate survival on the latent scale: 
  d1$sm <- x$s +             # average population survival 
    d1$Is +           # intrinsic survival value
    x$Bzs*(d1$z) +    # effect of phenotype on survival
    Y$Ys[1]  +        # annual variation in survival
    d1$n*x$Bns +      # density regulation
    x$as*d1$age +     # age effect
    E[,"Es"]          # residual effect
  
  d1$pr = (1/(1+exp(-d1$sm))) # transform to individual survival probabilities
  d1$surv<-rbinom(length(d1$sm),1,d1$pr) # realization for survival
  
  
  #---- Model reproduction ----
  # calculate survival on the latent scale (following same logic as survival): 
  d1$rm<- x$r + d1$Ir + x$Bzr*(d1$z) + Y$Yr[1]  + d1$n*x$Bnr + x$ar*d1$age + E[,"Er"]
  
  # the exponent of the latent trait defines the mean for the poisson process
  d1$recruits<-rpois(length(d1$rm), exp(d1$rm)) 
  
  
  #---- Calculate fitness ----
  d1$w<-d1$recruits + d1$surv  # individual fitness (the sum of survivial and no. recruits)
  d1$rw<-d1$w/mean(d1$w)       # relative fitness
  
  
  #---- Store data for year 1 ----
  # add individual data for year 1 to list dyears
  dyears[[1]]<-d1 
  
  # initialize a dataframe of population-level data for year 1
  pop <- data.frame(scenID=x$scenID,                                # scenario ID
                    year=1,                                         # year
                    N=x$nJ+x$nA,
                    n=log(x$nJ+x$nA),# total population size
                    lambda=log(sum(d1$recruits+d1$surv)/d1$N[1]),   # growth rate 
                    z_bar=d1$z_bar[1],                              # mean phenotype (previously "z")
                    deltaz=d1$z_bar[1]+cov(d1$z, d1$rw))            # expected change in mean phenotype
  
  
  ##############################
  # Iterate over n.years
  ##############################
  
  
  for(i in 2:x$n.years){
    
    # Create temporary data with individuals of the previous generation 
    dtmp<-as.data.frame(dyears[[i-1]])
    
    if(sum(dtmp$surv)==0 & sum(dtmp$recruits)==0){
      break
    }  # if no individuals survive or reproduce, stop the loop 
    
    
    #---- Surviving adults ----    
    # Only the surviving adults make it to the current generation 
    dA2<-dtmp[dtmp$surv==1, c("ID", "Ir", "Is", "z")]
    dA2$age<-1 + dtmp$age[dtmp$surv==1] # increase their age
    
    #---- Create new recruits ----
    # Recruits enter the current generation, they have the same characteristics of the parents   
    Ir=rep(dtmp$Ir, dtmp$recruits)
    Is=rep(dtmp$Is, dtmp$recruits)
    z=rep(dtmp$z, dtmp$recruits)
    
    Vz<-mean((c(z + dtmp$z)-mean(c(z + dtmp$z)))^2)
    # Add intrinsic survival and reproduction merits for the new recruits 
    if (length(Ir)>0) {
      # generate unique ID for new recruits: year - parent ID - within-year ID
      dJ2<-data.frame(ID=(max(dtmp$ID)+1):(max(dtmp$ID)+sum(dtmp$recruits)))
      # calculate intrinsic value: parental value * heritability + error
      dJ2$Ir=Ir
      dJ2$Is=Is
      # calculate phenotypic value for new recruits 
      if(Vz>x$Vaz){
        dJ2$z= z 
      }else{
        dJ2$z= z + rnorm(length(z), 0, sqrt(x$Vaz-Vz))
      }
      # age of new recruits = 0
      dJ2$age<-0
    } # end recruit loop
    
    
    #---- Store individual data for the current year ---- 
    
    #--- hide these clauses for now and see if we get an error
    #--- ... see rbind() below
    # If some individuals survive and some reproduced combines the data into d2
    #if(nrow(dA2)>0 & length(Ir)>0){
    #  d2<-rbind(dA2,  dJ2)
    #}
    
    # If some individuals survive but nobody reproduced, d2 is composed by only adults of previous year 
    #if(nrow(dA2)>0 & length(Ir)==0){
    #  d2<-dA2
    #}
    # If no adult survives but some reproduce, d2 is composed of only juveniles:     
    #if(nrow(dA2)==0 & length(Ir)>0){
    #  d2<-dJ2
    #}
    
    d2 <- rbind(dA2, dJ2) %>%
      mutate(year=i, 
             N=nrow(.)) # Population size in current year
    d2$n<-log(d2$N)
    
    # Simulate stochastic variation for individual yearly survival and reproduction
    MR<-matrix(c(x$VEs, x$VErs, x$VErs, x$VEr),2,2)
    E<-matrix(mvrnorm(nrow(d2), c(0,0), MR),nrow(d2),2)
    colnames(E) <- c("Es", "Er")
    
    
    # Age as a categorical value and mean phenotype
    age2<-d2$age
    age2[d2$age>0]<-1
    d2$z_bar<-mean(c(dA2$z,dJ2$z))
    
    
    #---- Calculate survival and reproduction ----
    # Follows same logic as year 1, 
    # but with extra terms reflecting the interactions between population size and mean phenotype
    
    # calculate survival on the latent scale:
    d2$sm <- x$s + x$as*age2  + d2$Is + Y$Ys[i] + x$Bzs*d2$z +     d2$n*x$Bns +  E[,"Es"]
    x$BZs*d2$z_bar +             # social selection 
      x$BZns*d2$n*d2$z_bar +     # phenotype-dependent density regulation
      x$Bzns*d2$n*d2$z  +        # density-dependent selection
      x$BZzs*d2$z*d2$z_bar +     # frequency-dependent selection
      x$BZzns*d2$z*d2$z_bar*d2$n # link between density- and frequency-dependent selection
    
    # probability of survival: 
    d2$pr = (1/(1+exp(-d2$sm))) 
    
    # realized survivial: 
    d2$surv<-rbinom(length(d2$sm),1,d2$pr) # realization
    
    # calculcate reproduction on the latent scale
      d2$rm <- x$r + x$ar*age2 +  d2$Ir + Y$Yr[i] + x$Bzr*d2$z + 
      x$Bzr2*d2$z^2 + d2$n*x$Bnr + E[,"Er"] + 
      x$BZr*d2$z_bar + 
      x$BZnr*d2$n*d2$z_bar + 
      x$Bznr*d2$n*d2$z  + 
      x$BZzr*d2$z*d2$z_bar+ 
      x$BZznr*d2$z*d2$z_bar*d2$n  
    
    # realized reproduction
    d2$recruits<-rpois(length(d2$rm), exp(d2$rm)) 
    
    # calculate fitness 
    d2$w<-d2$recruits + d2$surv    
    d2$rw<-d2$w/mean(d2$w)
    
    # add individual data for year i to list dyears 
    dyears[[i]]<-d2
    
    # add population-level data for current year to pop dataframe 
    pop <- pop %>% 
      add_row(scenID=x$scenID,                                  # scenario ID
              year=i,                                           # year
              N=sum(dtmp$recruits + dtmp$surv),                 # total population size
              n=log(sum(dtmp$recruits + dtmp$surv)), 
              z_bar=d2$z_bar[1],                                # mean phenotype (previously "z")
              lambda=log(sum(d2$recruits + d2$surv)/d2$N[1]),   # growth rate
              deltaz=mean(d2$z)+cov(d2$z, d2$rw))               # expected change in mean phenotype
  } # end loop
  
  
  #---- Format data ----
  dtmp1<-do.call(rbind.data.frame, dyears)  # combine ind. data for all years into a dataframe 
  dtmp1$simID<- x$sim                         # simulation ID
  dtmp1$scenID<- x$scenID 
  pop$simID <- x$sim                          # column for simulationID
  pop$scenID <- x$scenID
  # column for simulation 
  
  dAll$IndData<-dtmp1                       # add yearly data to a list
  dAll$PopData<-pop                         # add population data to a list
  dAll$Param<-x
  dAll
}


#Simulate populations
sim.pops <- function(x, n.sims, n.Cores=detectCores()-4, seed=FALSE){  
  lists<-list()  # initialize list of parameter values for each scenario
  for(j in 1:n.sims){
    lists[[j]]<- cbind(x, sim=j)  # create list of paramter values (all same) and simulation ID (unique)
  }    
  
  res <- mclapply(lists, IBsim, mc.cores=n.Cores) 
  res
}

make.dataframe<-function(x){
  d<- list()
  d$Ind<-do.call(rbind.data.frame, lapply(x, '[[', 1))
  d$Pop<-do.call(rbind.data.frame, lapply(x, '[[', 2)) 
  d$Params<-as.data.frame(x$params)
  d
}

create.x <- function(scenID, n.years=200, s=-0.1, r=1, Bns=0, Bnr=-0.01, nJ=20, nA=20,ar=0, as=0, h2r=0, h2s=0, h2rs=1, VYs=0, VYr=0, VYrs=0, VIr=0, VIs=0,  Vaz=3, VEs=0, VEr=0, VErs=0, Bzr=0, Bzr2=0, Bzs=0, BZr=0, BZs=0, BZnr=0, BZns=0, Bznr=0, Bzns=0, BZzr=0,  BZzs=0, BZznr=0,  BZzns=0){
  data.frame(scenID=scenID, n.years=n.years, s=s, r=r, Bns=Bns, Bnr=Bnr, nJ=nJ, nA=nA,
             ar=ar, as=as, h2r=h2r, h2s=h2s, h2rs=h2rs,
             VYs=VYs, VYr=VYr, VYrs=VYrs, VIs=VIs, VIr=VIr, Vaz=Vaz, VEs=VEs, VEr=VEr, 
             VErs=VErs, Bzr=Bzr, Bzr2=Bzr2, Bzs=Bzs, BZr=BZr, BZs=BZs, BZnr=BZnr, BZns=BZns,
             Bznr=Bznr, Bzns=Bzns, BZzr=BZzr, BZzs=BZzs, BZznr=BZznr, BZzns=BZzns)
}







