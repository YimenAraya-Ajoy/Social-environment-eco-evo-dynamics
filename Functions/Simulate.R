
# =====================================================
# IBsim: Simplified Individual-Based Simulation Function
# Purpose: Simulates trait evolution and demography with trait-based reproduction
# =====================================================

IBsim <- function(x, theta){
  # initialize lists
  dyears<-list()  
   
  # Start IBS
   for(i in 1:x$n.years){
    # Founder generation     
     if(i == 1) {
     d1<-data.frame(ID=1:x$n)           # individual ID
     d1$a<-rnorm(nrow(d1), x$Ez, sqrt(x$Vaz))   # phenotypic trait value
     d1$z<-d1$a + rnorm(nrow(d1), 0, sqrt(1-x$Vaz)) 
     } else {     
   # Previous year data
    dtmp<-as.data.frame(dyears[[i-1]])

   # Check if population persists     
    if(sum(dtmp$surv)==0 & sum(dtmp$recruits)==0){
      break
    }    
    #---- Create new recruits ---- 
    # Phenotype of dams of each recruit
    a_m <- rep(dtmp$a, dtmp$recruits)  
    
    if (length(a_m)>0) {
      # generate unique ID for new recruit
      dJ<-data.frame(ID=(max(dtmp$ID)+1):(max(dtmp$ID)+sum(dtmp$recruits)))

      # sires drawn from same distribution as succesfull modthers dams
      a = (a_m + rnorm(nrow(dJ), mean(a_m), sd(a_m)))/2 # mid-parental value 
      mend_dev =  rnorm(nrow(dJ), 0, sqrt(x$Vaz/2))     # mendelian deviations  
      mut_dev =   rnorm(nrow(dJ), 0, sqrt((x$Vaz^2)*(x$gamma/2)))  # mutational deviations        
      dJ$a = a + mend_dev + mut_dev                     # phenotypes of recruits 
    } 
       
    # Surviving adults  
    dA <- dtmp[dtmp$surv==1, c("ID", "a")]
       
    # All individuals
    d1 <- rbind(dA, dJ)
    }

    d1$z<- d1$a + rnorm(nrow(d1), 0, sqrt(1-x$Vaz)) 
    d1$z_bar<-mean(d1$z)  
    d1$year<-i
    d1$N <- nrow(d1)
    d1$n <-log(d1$N)        
    d1$Y<-rnorm(1,0,sqrt(0))
    #---- Model reproduction ----
    d1$r<- x$r_max -
    x$gamma/2*(theta[i]-d1$z)^2 +
    d1$n*x$Bn + 
    x$BZ*d1$z_bar + 
    x$BZn*d1$n*d1$z_bar + 
    x$Bzn*d1$n*d1$z  + 
    x$BZz*d1$z*d1$z_bar+ 
    x$BzminZ*(d1$z-d1$z_bar)+
    x$BzminZ2*(d1$z-d1$z_bar)^2+
    x$BZzn*d1$z*d1$z_bar*d1$n 
  
  d1$recruits<-rpois(nrow(d1), exp(d1$r)) # poisson process

  #---- Model survival, individual demograpic contribution and relative fitness ----
  d1$sm <- x$s                      # average population survival 
  d1$pr = (1/(1+exp(-d1$sm)))       # transform to individual survival probabilities
  d1$surv<-rbinom(nrow(d1),1,d1$pr) # realization for survival    
  d1$w<-d1$recruits + d1$surv       # individual demographic contribution
  d1$rw<-d1$w/mean(d1$w)            # relative fitness
  
  #---- Store data in list ----
  dyears[[i]]<-d1 
   }
    
  #---- Format data ---- 
  res1<-do.call(rbind.data.frame, dyears)  # combine ind. data for all years into a dataframe 
  res1$simID  <- x$sim                      # simulation ID
  res1$scenID <- x$scenID 
  res1
}


create.x <- function(scenID="base", n.years=250, n=200, Ez=0,  Vaz=0.5,
                     s=0, r_max=1.4,  gamma=0.2, Bn=-0.375,
                     BZ=0,  BZn=0,  Bzn=0, BZz=0,  BzminZ=0, BzminZ2=0, BZzn=0){

              data.frame(scenID=scenID, n.years=n.years, n=n, Ez=Ez, Vaz=Vaz,
              s=s, r_max=r_max, gamma=gamma, Bn=Bn,
              BZ=BZ, BZn=BZn, Bzn=Bzn, BZz=BZz, BzminZ=BzminZ, BzminZ2=BzminZ2, BZzn=BZzn)
} 



sim.pops <- function(x, n.sims, theta, n.Cores=detectCores()-4, seed=FALSE){  
  lists<-list()  # initialize list of parameter values for each scenario
  for(j in 1:n.sims){
    lists[[j]]<- cbind(x, sim=j)  # create list of paramter values (all same) and simulation ID (unique)
  }    
  
  res <- mclapply(lists, IBsim, theta, mc.cores=n.Cores) 
  res
}
