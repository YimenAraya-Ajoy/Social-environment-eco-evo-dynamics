# this function combines functions Nbar() and zbar()
# accepts as arguments the output of sim.pops for three scenarios (a, b,c) and calculates the avg yearly population size and phenotype accross all simulations in each scenario
calc.simAvg <- function(a){
  rbind(a$Pop) %>%                # combine data from all scenarios
    group_by(year) %>%                    # for each year in each scenario, calculate... 
    summarize(Nbar = mean(N, na.rm=TRUE), 
              zbar = mean(z_bar, na.rm=TRUE),     # mean phenotype
              sigma2z = var(z_bar, na.rm=TRUE)) 
}


# FunctionExtinction
# a function to calculate extinction (replaces Ext())
calc.ext <- function(a, b, c){
  rbind(a$Pop, b$Pop, c$Pop) %>%
    group_by(scenID, sim) %>% 
    summarize(max.year=max(year), .group='keep') %>%
    mutate(prop.ext=sum(max.year<100)/max(sim)) %>%
    group_by(scenID) %>% summarize(ext=mean(prop.ext), )
}



##Analyze the data
coefs2<-function(x, formula){
  x$z2<-x$z^2
  m<-glm(recruits ~ z +z2 + n + n:z + offset(log(rep(0.47, nrow(x)))), data=x, family="poisson")
  coefs<-data.frame(B0=coef(m)[1], "n"=0, "z"=0, "z2"=0, "z_bar"=0, "n:z_bar"=0, "z:n"=0, "z:z_bar"=0, "n:z_bar:z"=0)
  nam<-gsub(":", ".", names(coef(m)[2:length(coef(m))]))
  coefs[match(nam, colnames(coefs))]<-coef(m)[2:length(coef(m))]
  colnames(coefs)<-c("B0", "Bn", "Bz", "Bq", "Bz_bar", "Bnz_bar", "Bnz", "Bz_barz", "Bnz_barz")
  coefs$Eq_z=mean(x$z[x$year==max(x$year)]) 
  coefs$Eq_n=mean(x$n[x$year==max(x$year)])
  coefs
  }


coefs<-function(x, formula){
  x$z2<-x$z^2
  m<-glm(formula, data=x, family="poisson")
  coefs<-data.frame(B0=coef(m)[1], "n"=0, "z"=0, "z2"=0, "z_bar"=0, "n:z_bar"=0, "n:z"=0, "z:z_bar"=0, "n:z:z_bar"=0)
  nam<-gsub(":", ".", names(coef(m)[2:length(coef(m))]))
  coefs[match(nam, colnames(coefs))]<-coef(m)[2:length(coef(m))]
  colnames(coefs)<-c("B0", "Bn", "Bz", "Bq", "Bz_bar", "Bnz_bar", "Bnz", "Bz_barz", "Bnz_barz")
  coefs$Eq_z=mean(x$z[x$year==max(x$year)]) 
  coefs$Eq_n=mean(x$n[x$year==max(x$year)])
  coefs
}



est<-function(x, formula){
  est_res<-do.call(rbind.data.frame,mclapply(lapply(x, '[[', 1), coefs, formula, mc.cores=detectCores()-3))
  est_res
  }


est_Eq<-function(est_res){
  est_res$E_z<-(est_res$Bz +  est_res$Bnz*est_res$Eq_n) /-(2*est_res$Bq + est_res$Bz_barz + est_res$Bnz_barz*est_res$Eq_n)
  est_res$E_n=(est_res$B0 + (est_res$Bz + est_res$Bz_bar)*est_res$E_z  + est_res$Bq*(est_res$E_z^2 + 3))/-(est_res$Bn + est_res$Bnz*est_res$E_z + est_res$Bnz_bar*est_res$E_z + est_res$Bnz_barz*est_res$E_z^2) 
  est_res
}


est_m<-function(est_res){
  m<-apply(est_res,2,mean)
  est_m_res<-data.frame(matrix(m,1))
  colnames(est_m_res)<-names(m)
  est_m_res
}


est_Eq_z<-function(est_m_res){
  E_z<-seq(0,20,0.01)
  E_n=-(est_m_res$B0 + (est_m_res$Bz + est_m_res$Bz_bar)*E_z  + est_m_res$Bq*(E_z^2 + 3) + est_m_res$Bz_barz*E_z^2)/(est_m_res$Bn + est_m_res$Bnz*E_z + est_m_res$Bnz_bar*E_z +  est_m_res$Bnz_barz*E_z^2) 
  est_Eq_z_res<-data.frame(E_z,  E_n)
  est_Eq_z_res
}


fitness_func<-function(est_m_res){
  E_z<-seq(0,25,0.01)
  E_v=est_m_res$B0 + est_m_res$Bz*E_z  + est_m_res$Bz_bar*E_z  + est_m_res$Bq*E_z^2
  est_v_z_res<-data.frame(E_z,  E_v)
  est_v_z_res
  }



est_v_n<-function(est_m_res){
  n<-seq(0,10,0.01)
  E_w=(est_m_res$B0 + (est_m_res$Bz + est_m_res$Bz_bar)*est_m_res$Eq_z  + est_m_res$Bq*(est_m_res$Eq_z^2 + 3) + est_m_res$Bz_barz*est_m_res$Eq_z^2) + (est_m_res$Bn + est_m_res$Bnz*est_m_res$Eq_z + est_m_res$Bnz_bar*est_m_res$Eq_z + est_m_res$Bnz_barz*est_m_res$Eq_z^2) * n
  est_v_n_res<-data.frame(n,  E_w)
  est_v_n_res
}


  est_all<-function(s, formula){
  est_res<-est(s,formula)
  est_Eq_res<-est_Eq(est_res)
  est_m_res<-est_m(est_Eq_res)
  est_Eq_z_res<-est_Eq_z(est_m_res)
  est_v_n_res<-est_v_n(est_m_res)
  fitness_func_res<-fitness_func(est_m_res)
  list(est_res=est_res, est_Eq_res=est_Eq_res, est_m_res=est_m_res, est_Eq_z_res=est_Eq_z_res, est_v_n_res=est_v_n_res, fitness_func_res=fitness_func_res)
}
  
  
  eqsS2<-function(est_m_res, Bnz){
    n<-seq(0,20,by=0.01)
    E_z<-seq(0,20,0.01)
    est_Eq_n_res<-matrix(NA,length(E_z),length(Bnz))
    est_v_res<-matrix(NA,length(n),length(Bnz))
    max_z<-rep(NA, length(Bnz))
    max_n<-rep(NA, length(Bnz))
    
    
    for(i in 1:length(Bnz)){
      E_n=-(est_m_res$B0[1] + (est_m_res$Bz[1] + est_m_res$Bz_bar[1])*E_z  
            + est_m_res$Bq[1]*(E_z^2 + 3))/(est_m_res$Bn[1] + est_m_res$Bnz_barz[1]*E_z^2
                                            + est_m_res$Bnz_bar[1]*E_z + Bnz[i]*E_z) 
      est_Eq_n_res[,i]<-E_n
      max_n[i]<-max(E_n)
      max_z[i]<-E_z[max(E_n)==E_n]
    }
    
    
    Eq_z2<-matrix(NA,length(n), length(Bnz))
    for(i in 1:length(Bnz)){
      Eq_z2[,i]<- -(est_m_res$Bz[1] +  Bnz[i]*n)/(2*est_m_res$Bq[1] + est_m_res$Bz_barz[1] + est_m_res$Bnz_barz[1]*n  
                                     )
    }
    
    
    Eqs_l<-list()
    for(i in 1:length(Bnz)){
      d1<-data.frame(n=round(est_Eq_n_res[,i],2), E_z=E_z, Bnz=Bnz[i])
      d1$ID<-paste(d1$n, d1$E_z)
      d2<-data.frame(n=n, E_z=round(Eq_z2[,i],2), Bnz=Bnz[i])
      d2$ID<-paste(d2$n, d2$E_z)
      d3<-d1[match(d2$ID, d1$ID),]   
      d3<-d3[complete.cases(d3$n),]  
      Eqs_l[[i]]<-d3
    }
    
    eqs<-do.call(rbind.data.frame, Eqs_l)
    
    
    for(i in 1:length(Bnz)){
      E_z2<-eqs$E_z[i]
      v<-est_m_res$B0[1] + (est_m_res$Bz[1] + est_m_res$Bz_bar[1])*E_z2  + est_m_res$Bq[1]*(E_z2^2 + 3) + (est_m_res$Bn[1] 
                                                   + est_m_res$Bnz_barz[1]*E_z2^2 +  est_m_res$Bnz_bar[1]*E_z2 + Bnz[i]*E_z2)*n
      est_v_res[,i]<-v 
    }
    
    res<-list(est_Eq_n_res=as.data.frame(est_Eq_n_res), E_z=as.data.frame(E_z), Eq_z2=as.data.frame(Eq_z2), max_n=max_n, max_z=max_z, n=as.data.frame(n), eqs=eqs, est_v_res=as.data.frame(est_v_res))
    res
  }
  
  eqsS6<-function(est_m_res, Bnz_barz){
    n<-seq(0,20,by=0.01)
    E_z<-seq(0,20,0.01)
    est_Eq_n_res<-matrix(NA,length(E_z),length(Bnz_barz))
    est_v_res<-matrix(NA,length(n),length(Bnz_barz))
    max_z<-rep(NA, length(Bnz_barz))
    max_n<-rep(NA, length(Bnz_barz))
    
    
    for(i in 1:length(Bnz_barz)){
      E_n=-(est_m_res$B0[1] + (est_m_res$Bz[1] + est_m_res$Bz_bar[1])*E_z  
            + est_m_res$Bq[1]*(E_z^2 + 3))/(est_m_res$Bn[1] + est_m_res$Bnz[1]*E_z
                                            + est_m_res$Bnz_bar[1]*E_z + Bnz_barz[i]*E_z^2) 
      est_Eq_n_res[,i]<-E_n
      est_Eq_n_res[,i]<-E_n
      max_n[i]<-max(E_n)
      max_z[i]<-E_z[max(E_n)==E_n]
    }
    
    
    Eq_z2<-matrix(NA,length(n), length(Bnz_barz))
    for(i in 1:length(Bnz_barz)){
      Eq_z2[,i]<--(est_m_res$Bz[1] +est_m_res$Bnz[1]*n)/(2*est_m_res$Bq[1] + est_m_res$Bz_barz[1] + Bnz_barz[i]*n)
                                      
    }
    
    
    Eqs_l<-list()
    for(i in 1:length(Bnz_barz)){
      d1<-data.frame(n=round(est_Eq_n_res[,i],2), E_z=E_z, Bnz_barz=Bnz_barz[i])
      d1$ID<-paste(d1$n, d1$E_z)
      d2<-data.frame(n=n, E_z=round(Eq_z2[,i],2), Bnz_barz=Bnz_barz[i])
      d2$ID<-paste(d2$n, d2$E_z)
      d3<-d1[match(d2$ID, d1$ID),]   
      d3<-d3[complete.cases(d3$n),]  
      Eqs_l[[i]]<-d3
    }
    
    eqs<-do.call(rbind.data.frame, Eqs_l)
    
    
    for(i in 1:length(Bnz_barz)){
      E_z2<-eqs$E_z[i]
      v<-est_m_res$B0[1] + (est_m_res$Bz[1] + est_m_res$Bz_bar[1])*E_z2  + est_m_res$Bq[1]*(E_z2^2 + 3) + (est_m_res$Bn[1] + est_m_res$Bnz[1]*E_z2 +  est_m_res$Bnz_bar[1]*E_z2 + Bnz_barz[i]*E_z2^2)*n
      est_v_res[,i]<-v 
    }
    
    res<-list(est_Eq_n_res=as.data.frame(est_Eq_n_res), E_z=as.data.frame(E_z), Eq_z2=as.data.frame(Eq_z2), max_n=max_n, max_z=max_z, n=as.data.frame(n), eqs=eqs, est_v_res=as.data.frame(est_v_res))
    res
  }
  
  
  

