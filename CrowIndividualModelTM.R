## Mariana Crow Stochastic Model ##

# The following script runs a stochastic simulation for the Mariana crow. It requires the popbio package.
# The input file is multiple models which contain the various demographic information per scenario. All models are output as csv files in the working directory with the associated scenario number.

rm(list = ls()) # Remove all past worksheet variables

#runName<-"modelTest_110Pairs_1999_50years_RR_22Apr2014"
scenario<-read.csv("data/Scenarios.csv", sep=",")# Settings for individual scenarios
strtPop<-110 # Initial Population (Pairs)
strtYear<-1999 # This assigns a year to the start of your simulation
numYear<-30 # Number of years
iteR<-1000 # Iterations
carryCap<-205 # Number of pairs (based on available habitat)

library(popbio)

sNumT<-dim(scenario)[2]
sNum<-sNumT-1
for(z in 1:sNum){
  tableC<-z+1
  ## Breeding Demographic Data ##
  breedP<-scenario[1,tableC] # Percentage breeding per year 90% from Morton et al. 1999
  breedPSD<-scenario[2,tableC] # 0.1
  fledgS<-scenario[3,tableC] # Fledging success 0.59 - Zarones et al. 2013 - Dropping 1997 typhoon year - 0.53 with 1997
  fledgSSD<-scenario[4,tableC] # 0.11 - Zarones et al. 2013 - Dropping 1997 typhoon year - 0.17 with 1997
  meanOff<-scenario[5,tableC] # 1.25 - mean number of offspring - Zarones et al. 2013
  sdOff<-scenario[6,tableC]# 0.4 - standard deviation of the number of offspring
  maxOff<-scenario[7,tableC] # 2 - Maximum number of fledglings observed
  sexRatio<-scenario[8,tableC] # 0.6 - percentage of offspring that are females - 0.6 - Ha and Ha 2011, Morton et al. 1999

  ## Survival Demographic Data ##
  juveFS<-scenario[9,tableC] # Juve Female survival 0.75 - Ha et al. 2010, Ha and Ha 2011
  juveFSSD<-scenario[10,tableC] # 0.11 - Ha et al. 2010, Ha and Ha 2011
  juveMS<-scenario[11,tableC] # Juve Male survival 0.5 - Ha et al. 2010, Ha and Ha 2011
  juveMSSD<-scenario[12,tableC] # 0.13 - Ha et al. 2010, Ha and Ha 2011
  adultFS<-scenario[13,tableC] # Adult Female Survival 0.84 - Ha et al. 2010, 0.83 - Ha and Ha 2011
  adultFSSD<-scenario[14,tableC] # .03 - Ha et al. 2010
  adultMS<-scenario[15,tableC] # Adult Male Survival 0.84 - Ha et al. 2010 and Ha and Ha 2011
  adultMSSD<-scenario[16,tableC] # 0.03 - Ha et al. 2010

  ## Management Scenarios ##
  capStart<-scenario[17,tableC] # Number of model years when captive population starts
  harvStart<-scenario[18,tableC] # Number of model years when begin collecting crows for rear and release
  harvStop<-scenario[19,tableC] # Number of model years when stop collecting crows for rear and release
  mortStart<-scenario[20,tableC] # When mortality control measures begin
  rSite<-scenario[21,tableC] # 0 - Rota Only, 1 - Captive Only, 2 - Alt Island Only

  # Mortality Reduction #
  newAdSurv<-scenario[22,tableC] # New adult survival after mortality control measures are implemented
  newJuvSurv<-scenario[23,tableC] # Same as above except juvenile survival

  # Rear/Release + Captive #
  harvEgg<-scenario[24,tableC]  # Number of eggs harvested
  harvNestling<-scenario[25,tableC] # Number of nestling harvested
  nest1Succ<-scenario[26,tableC] # Renesting success after harvest 0.27 - Zarones et al., unpubl
  eggSucc<-scenario[27,tableC] # Egg hatching success in captivity 0.25 - DAWR, unpubl. data
  nestlingSucc<-scenario[28,tableC] # Nestling success in captivity 0.90 - DAWR, unpubl. data
  juveFS2nd<-scenario[29,tableC] # Juvenile female survival in captivity 0.84 - 0-1 year old Alala
  juveMS2nd<-scenario[30,tableC] # Juvenile male survival in captivity 0.84 - 0-1 year old Alala
  subFS2nd<-scenario[31,tableC] # Subadult female survival in captivity 0.88 - 1-3 year old Alala
  subMS2nd<-scenario[32,tableC] # Subadult male survival in captivity 0.78-0.80 - 1-3 year old Alala
  adFS2nd<-scenario[33,tableC] # Adult female survival in captivity 0.95 - > 3 year old Alala
  adMS2nd<-scenario[34,tableC] # Adult male survival in captivity 0.94 - > 3 year old Alala
  nestSucc2nd<-scenario[35,tableC] # Captive pair nest success Range 0.24-0.40 in captive Alala
  carryCap2<-scenario[36,tableC] # Carrying capacity of captive population

  ## Estimate Stable Age Distribution ##
  MFec<-(((1-sexRatio)*meanOff)*breedP)*fledgS
  FFec<-(((sexRatio)*meanOff)*breedP)*fledgS
  #MatrixF<-matrix(c(0,0,femaleFec,juveFS,0,0,0,adultFS,adultFS), nrow=3, ncol=3, byrow=TRUE)
  MatrixF<-matrix(c(0,0,0,FFec,FFec,FFec,FFec,FFec,FFec,FFec,FFec,FFec,FFec,FFec,FFec,FFec,FFec,FFec,FFec,FFec,
                    juveFS,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,adultFS,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,adultFS,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,adultFS,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,adultFS,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,adultFS,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,adultFS,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,adultFS,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,adultFS,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,adultFS,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,adultFS,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,adultFS,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,adultFS,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,adultFS,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,adultFS,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,adultFS,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,adultFS,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,adultFS,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,adultFS,0), nrow=20, ncol=20, byrow=TRUE)
  #MatrixM<-matrix(c(0,0,maleFec,juveMS,0,0,0,adultMS,adultMS), nrow=3, ncol=3, byrow=TRUE)
  MatrixM<-matrix(c(0,0,0,MFec,MFec,MFec,MFec,MFec,MFec,MFec,MFec,MFec,MFec,MFec,MFec,MFec,MFec,MFec,MFec,MFec,
                    juveMS,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,adultMS,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,adultMS,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,adultMS,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,adultMS,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,adultMS,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,adultMS,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,adultMS,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,adultMS,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,adultMS,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,adultMS,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,adultMS,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,adultMS,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,adultMS,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,adultMS,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,adultMS,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,adultMS,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,adultMS,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,adultMS,0), nrow=20, ncol=20, byrow=TRUE)
  feigen<-eigen.analysis(MatrixF)
  fstable<-as.matrix(feigen$stable.stage)
  #totalF<-strtPop/fstable[3,1]
  totalF<-strtPop/sum(fstable[c(4:20),1])
  meigen<-eigen.analysis(MatrixM)
  mstable<-as.matrix(meigen$stable.stage)
  #totalM<-strtPop/mstable[3,1]
  totalM<-strtPop/sum(mstable[c(4:20),1])

  ## Run Population Projection Models ##
  ptm0 <- proc.time()# Start the clock!
  for(x in 1:iteR){
    # Start New Population Matrix #
    numYear2<-numYear+1
    mMatrix<-matrix(0, nrow=3, ncol=numYear2, byrow=TRUE)
    fMatrix<-matrix(0, nrow=3, ncol=numYear2, byrow=TRUE)
    mMatrix[1,1]<-mstable[1,]*totalM
    mMatrix[2,1]<-mstable[2,]*totalM
    mMatrix[3,1]<-mstable[3,]*totalM
    fMatrix[1,1]<-fstable[1,]*totalF
    fMatrix[2,1]<-fstable[2,]*totalF
    fMatrix[3,1]<-fstable[3,]*totalF

    # 2nd Population Matrix #
    mMatrix2<-matrix(0, nrow=3, ncol=numYear2, byrow=TRUE)
    fMatrix2<-matrix(0, nrow=3, ncol=numYear2, byrow=TRUE)

    # Start Yearly Population Estimates #
    for(y in 1:numYear){
      column<-y
      column2<-y+1
      column3<-y+3 # This is release year - change the 3 to a variable for input above if desired

      ## Part 1 - Mortality ##
      if(mortStart<=y){
        adultMS<-newAdSurv
        adultFS<-newAdSurv
        juveMS<-newJuvSurv
        juveFS<-newJuvSurv
      }
      M1<-mMatrix[1,column]*betaval(juveMS,juveMSSD)
      mMatrix[2,column2]<-round(M1, digits=0)
      M2<-mMatrix[2,column]*betaval(adultMS,adultMSSD)
      M3<-mMatrix[3,column]*betaval(adultMS,adultMSSD)
      M4<-mMatrix[3,column2]*betaval(juveMS,juveMSSD) # These are released adult males subjected to juvenile survival rate for release year
      mMatrix[3,column2]<-round(M2+M3+M4, digits=0)
      F1<-fMatrix[1,column]*betaval(juveFS,juveFSSD)
      fMatrix[2,column2]<-round(F1, digits=0)
      F2<-fMatrix[2,column]*betaval(adultFS,adultFSSD)
      F3<-fMatrix[3,column]*betaval(adultFS,adultFSSD)
      F4<-fMatrix[3,column2]*betaval(juveFS, juveFSSD) # These are released females subjected to juvenile survival rate for release year
      fMatrix[3,column2]<-round(F2+F3+F4, digits=0)

      ## Part 2 - Harvest and Release ##
      if(y>=harvStart && y<=harvStop){
          # Harvest #
          juveHarv1<-harvEgg*nest1Succ
          juveHarv2<-harvNestling*nest1Succ
          juveHarv<-juveHarv1+juveHarv2
          juveFHarv<-round((juveHarv*sexRatio), digits=0)
          juveMHarv<-round((juveHarv*(1-sexRatio)), digits=0)
          # Release - Note: These are stored in matrix for future calculations#
          releaseEggs<-harvEgg*eggSucc
          releaseNestlings<-harvNestling*nestlingSucc
          releaseAdults<-releaseEggs+releaseNestlings
          if(rSite==0){
            fMatrix[3,column3]<-round((releaseAdults*sexRatio), digits=0) # Stores release birds for mortality estimate above
            mMatrix[3,column3]<-round((releaseAdults*(1-sexRatio)), digits=0) # Stores released birds for mortality estimate above
          }else{
            fMatrix2[1,column2]<-round((releaseAdults*sexRatio), digits=0) # Stores captive reared birds in 2nd pop matrix
            mMatrix2[1,column2]<-round((releaseAdults*(1-sexRatio)), digits=0) # Stores captive reared birds in 2nd pop matrix
          }
      }else{
        juveFHarv<-0
        juveMHarv<-0
      }

      ## Part 3 - Reproduction ##
      mRep<-mMatrix[3,column2] # Determine number of adult males (>= 3 years old)
      fRep<-fMatrix[3,column2] # Determine number of adult females (>= 3 years old)
      if(fRep>mRep){
        pairs<-mRep # Number of pairs = number of adult males
      }else{
        pairs<-fRep # Number of pairs = number of adult females
      }
      # Reduce Pairs to Carrying Capacity #
      if(pairs>carryCap){
        pairs<-carryCap
      }
      pairBreed<-pairs*betaval(breedP, breedPSD) # Determine number of pairs that breed that year
      pairSuccess<-pairBreed*betaval(fledgS, fledgSSD) # Determine number of breeding pairs that successfuly fledge young
      breedOut<-pairSuccess*stretchbetaval(meanOff,sdOff, 1, maxOff, runif(1))
      fFledge1<-round((breedOut*sexRatio),digits=0)
      fFledge2<-fFledge1-juveFHarv
      fMatrix[1,column2]<-fFledge2
      mFledge1<-round((breedOut-fFledge1), digits=0)
      mFledge2<-mFledge1-juveMHarv
      mMatrix[1,column2]<-mFledge2

      ## Part 4 - 2nd Population ##
      # 2nd Pop Mortality #
      M21<-mMatrix2[1,column]*juveMS2nd
      mMatrix2[2,column2]<-round(M21, digits=0)
      M22<-mMatrix2[2,column]*subMS2nd
      M23<-mMatrix2[3,column]*adMS2nd
      mMatrix2[3,column2]<-round(M22+M23, digits=0)
      F21<-fMatrix2[1,column]*juveFS2nd
      fMatrix2[2,column2]<-round(F21, digits=0)
      F22<-fMatrix2[2,column]*subFS2nd
      F23<-fMatrix2[3,column]*adFS2nd
      fMatrix2[3,column2]<-round(F22+F23, digits=0)
      # 2nd Pop Reproduction #
      mRep2<-mMatrix2[3,column2] # Determine number of adult males (>= 3 years old)
      fRep2<-fMatrix2[3,column2] # Determine number of adult females (>= 3 years old)
      if(fRep2>mRep2){
        pairs2<-mRep2 # Number of pairs = number of adult males
      }else{
        pairs2<-fRep2 # Number of pairs = number of adult females
      }
      # Reduce Pairs to Carrying Capacity #
      if(pairs2>carryCap2){
        pairs2<-carryCap2
      }
      pairBreed2<-pairs2#*betaval(breedP, breedPSD) # Determine number of pairs that breed that year
      pairSuccess2<-pairBreed2*nestSucc2nd # Determine number of breeding pairs that successfuly fledge young
      breedOut2<-pairSuccess2*stretchbetaval(meanOff,sdOff, 1, maxOff, runif(1))
      fFledge21<-round((breedOut2*sexRatio),digits=0)
      fFledge22<-fMatrix2[1,column2] # These are the birds brought in from the wild
      fMatrix2[1,column2]<-fFledge21+fFledge22
      mFledge21<-round((breedOut2-fFledge21), digits=0)
      mFledge22<-mMatrix2[1,column2] # These are the birds brought in from the wild
      mMatrix2[1,column2]<-mFledge21+mFledge22
    }# End of year run loop
    if(x==1){
      # Wild Pop #
      juvMMatrix<-mMatrix[1,]
      juvFMatrix<-fMatrix[1,]
      subMMatrix<-mMatrix[2,]
      subFMatrix<-fMatrix[2,]
      adMMatrix<-mMatrix[3,]
      adFMatrix<-fMatrix[3,]
      # 2nd Pop #
      juvMMatrix2<-mMatrix2[1,]
      juvFMatrix2<-fMatrix2[1,]
      subMMatrix2<-mMatrix2[2,]
      subFMatrix2<-fMatrix2[2,]
      adMMatrix2<-mMatrix2[3,]
      adFMatrix2<-fMatrix2[3,]
    }else{
      # Wild Pop #
      juvMMatrix<-rbind(juvMMatrix, mMatrix[1,])
      juvFMatrix<-rbind(juvFMatrix, fMatrix[1,])
      subMMatrix<-rbind(subMMatrix, mMatrix[2,])
      subFMatrix<-rbind(subFMatrix, fMatrix[2,])
      adMMatrix<-rbind(adMMatrix, mMatrix[3,])
      adFMatrix<-rbind(adFMatrix, fMatrix[3,])
      # 2nd Pop #
      juvMMatrix2<-rbind(juvMMatrix2, mMatrix2[1,])
      juvFMatrix2<-rbind(juvFMatrix2, fMatrix2[1,])
      subMMatrix2<-rbind(subMMatrix2, mMatrix2[2,])
      subFMatrix2<-rbind(subFMatrix2, fMatrix2[2,])
      adMMatrix2<-rbind(adMMatrix2, mMatrix2[3,])
      adFMatrix2<-rbind(adFMatrix2, fMatrix2[3,])
    }
  } # End of iteration loop
  ptm1=proc.time() - ptm0
  jnk=as.numeric(ptm1[3])
  jnk=jnk/60
  cat('\n','It took ', jnk, "minutes to output simulation ", tableC)

  ## Create Population Summary Tables ##
  jMaleMean<-apply(juvMMatrix,2,FUN=mean)
  jMaleSD<-apply(juvMMatrix,2,FUN=sd)
  jFemaleMean<-apply(juvFMatrix,2,FUN=mean)
  jFemaleSD<-apply(juvFMatrix,2,FUN=sd)
  subMaleMean<-apply(subMMatrix,2,FUN=mean)
  subMaleSD<-apply(subMMatrix,2,FUN=sd)
  subFemaleMean<-apply(subFMatrix,2,FUN=mean)
  subFemaleSD<-apply(subFMatrix,2,FUN=sd)
  adMaleMean<-apply(adMMatrix,2,FUN=mean)
  adMaleSD<-apply(adMMatrix,2,FUN=sd)
  adFemaleMean<-apply(adFMatrix,2,FUN=mean)
  adFemaleSD<-apply(adFMatrix,2,FUN=sd)
  outTable<-as.data.frame(cbind(jMaleMean, jMaleSD, jFemaleMean, jFemaleSD, subMaleMean, subMaleSD,
                              subFemaleMean, subFemaleSD, adMaleMean, adMaleSD, adFemaleMean, adFemaleSD))
  dimTable<-dim(outTable)[1]
  potPairs<-list()
  for(m in 1:dimTable){
    potPairs[[m]]<-min(outTable[m,c(9,11)])
  }
  potPairs<-unlist(potPairs)
  potPairs<-as.data.frame(potPairs)
  outTable<-cbind(outTable, potPairs)

  year<-data.frame(c(0:numYear))
  year2<-year+strtYear
  names(year2)<-"Year"
  outTable<-cbind(outTable, year2)

  # 2nd Pop Table #
  jMaleMean2<-apply(juvMMatrix2,2,FUN=mean)
  jMaleSD2<-apply(juvMMatrix2,2,FUN=sd)
  jFemaleMean2<-apply(juvFMatrix2,2,FUN=mean)
  jFemaleSD2<-apply(juvFMatrix2,2,FUN=sd)
  subMaleMean2<-apply(subMMatrix2,2,FUN=mean)
  subMaleSD2<-apply(subMMatrix2,2,FUN=sd)
  subFemaleMean2<-apply(subFMatrix2,2,FUN=mean)
  subFemaleSD2<-apply(subFMatrix2,2,FUN=sd)
  adMaleMean2<-apply(adMMatrix2,2,FUN=mean)
  adMaleSD2<-apply(adMMatrix2,2,FUN=sd)
  adFemaleMean2<-apply(adFMatrix2,2,FUN=mean)
  adFemaleSD2<-apply(adFMatrix2,2,FUN=sd)
  outTable2<-as.data.frame(cbind(jMaleMean2, jMaleSD2, jFemaleMean2, jFemaleSD2, subMaleMean2, subMaleSD2,
                              subFemaleMean2, subFemaleSD2, adMaleMean2, adMaleSD2, adFemaleMean2, adFemaleSD2))
  dimTable<-dim(outTable2)[1]
  potPairs<-list()
  for(m in 1:dimTable){
    potPairs[[m]]<-min(outTable2[m,c(9,11)])
  }
  potPairs<-unlist(potPairs)
  potPairs<-as.data.frame(potPairs)
  outTable2<-cbind(outTable2, potPairs)

  year<-data.frame(c(0:numYear))
  year2<-year+strtYear
  names(year2)<-"Year"
  outTable2<-cbind(outTable2, year2)

  runName<-"MarianaCrow_PopulationModel_RotaPopulation_Simulation_"
  tableName<-paste("data/",runName,tableC,'.csv', sep="")
  write.csv(outTable, file=tableName, row.names=FALSE)

  if(rSite>0){
    runName<-"MarianaCrow_PopulationModel_CaptivePopulation_Simulation_"
    tableName<-paste("data/",runName,tableC,'.csv', sep="")
    write.csv(outTable2, file=tableName, row.names=FALSE)
  }

} # End of scenarios loop




# Move all below to a Markdown script that displays a report
outTable<-read.csv(paste("data/","MarianaCrow_PopulationModel_RotaPopulation_Simulation_10.csv", sep="" ))
## Output Plots ##
ymax<-max(outTable$adFemaleMean+outTable$adFemaleSD)
plot(outTable$Year, outTable$adFemaleMean, type="o", col="red", xlab="Year", ylab="Adult Aga", ylim=c(0,ymax))
lines(outTable$Year, outTable$adFemaleMean+outTable$adFemaleSD, lty=2, col="red")
lines(outTable$Year, outTable$adFemaleMean-outTable$adFemaleSD, lty=2, col="red")
lines(outTable$Year, outTable$adMaleMean, type="o", col="blue")
lines(outTable$Year, outTable$adMaleMean+outTable$adMaleSD, lty=2, col="blue")
lines(outTable$Year, outTable$adMaleMean-outTable$adMaleSD, lty=2, col="blue")
legend(2020, 40, c("Female", "Male"), lty=c(1,1), col=c("red","blue"), bty="n", cex=0.75)

ymax<-max(outTable$subFemaleMean+outTable$subFemaleSD)
plot(outTable$Year, outTable$subFemaleMean, type="o", col="red", xlab="Year", ylab="Subadult Aga", ylim=c(0,ymax))
lines(outTable$Year, outTable$subFemaleMean+outTable$subFemaleSD, lty=2, col="red")
lines(outTable$Year, outTable$subFemaleMean-outTable$subFemaleSD, lty=2, col="red")
lines(outTable$Year, outTable$subMaleMean, type="o", col="blue")
lines(outTable$Year, outTable$subMaleMean+outTable$subMaleSD, lty=2, col="blue")
lines(outTable$Year, outTable$subMaleMean-outTable$subMaleSD, lty=2, col="blue")
legend(2020, 8, c("Female", "Male"), lty=c(1,1), col=c("red","blue"), bty="n", cex=0.75)

ymax<-max(outTable$jFemaleMean+outTable$jFemaleSD)
plot(outTable$Year, outTable$jFemaleMean, type="o", col="red", xlab="Year", ylab="Juvenile Aga", ylim=c(0,ymax))
lines(outTable$Year, outTable$jFemaleMean+outTable$jFemaleSD, lty=2, col="red")
lines(outTable$Year, outTable$jFemaleMean-outTable$jFemaleSD, lty=2, col="red")
lines(outTable$Year, outTable$jMaleMean, type="o", col="blue")
lines(outTable$Year, outTable$jMaleMean+outTable$jMaleSD, lty=2, col="blue")
lines(outTable$Year, outTable$jMaleMean-outTable$jMaleSD, lty=2, col="blue")
legend(2020, 10, c("Female", "Male"), lty=c(1,1), col=c("red","blue"), bty="n", cex=0.75)

plot(outTable2$Year, outTable2$potPairs, type="o", xlab="Year", ylab="Pairs", ylim=c(0,200))
plot(outTable2$Year, outTable2$potPairs, type="o", xlab="Year", ylab="Pairs")
lines(outTable$Year, outTable$potPairs, type="o", col="blue")
legend(2020, 80, c("No Management", "Management"), lty=c(1,1), col=c("black","blue"), bty="n", cex=0.75)
points(2008, 80, col="red", pch=19)
text(2008, 80, "2008", pos=4)
points(2012, 60, col="red", pch=19)
text(2012, 60, "2012", pos=2)

if(harvStart>0){
  abline(v=(harvStart+strtYear))
  abline(v=(harvStop+strtYear))
}

if(rSite>0){
  tableName<-paste("data/",runName,'_2ndPop.csv', sep="")
  write.csv(outTable2, file=tableName, row.names=FALSE)

  ## Output Plots ##
  ymax<-max(outTable2$adFemaleMean+outTable$adFemaleSD)
  plot(outTable2$Year, outTable2$adFemaleMean2, type="o", col="red", xlab="Year", ylab="Adult Aga", ylim=c(0,ymax))
  lines(outTable2$Year, outTable2$adFemaleMean2+outTable2$adFemaleSD2, lty=2, col="red")
  lines(outTable2$Year, outTable2$adFemaleMean2-outTable2$adFemaleSD2, lty=2, col="red")
  lines(outTable2$Year, outTable2$adMaleMean2, type="o", col="blue")
  lines(outTable2$Year, outTable2$adMaleMean2+outTable2$adMaleSD2, lty=2, col="blue")
  lines(outTable2$Year, outTable2$adMaleMean2-outTable2$adMaleSD2, lty=2, col="blue")
  legend(2020, 40, c("Female", "Male"), lty=c(1,1), col=c("red","blue"), bty="n", cex=0.75)

  ymax<-max(outTable$subFemaleMean+outTable$subFemaleSD)
  plot(outTable2$Year, outTable2$subFemaleMean2, type="o", col="red", xlab="Year", ylab="Subadult Aga", ylim=c(0,ymax))
  lines(outTable2$Year, outTable2$subFemaleMean2+outTable2$subFemaleSD2, lty=2, col="red")
  lines(outTable2$Year, outTable2$subFemaleMean2-outTable2$subFemaleSD2, lty=2, col="red")
  lines(outTable2$Year, outTable2$subMaleMean2, type="o", col="blue")
  lines(outTable2$Year, outTable2$subMaleMean2+outTable2$subMaleSD2, lty=2, col="blue")
  lines(outTable2$Year, outTable2$subMaleMean2-outTable2$subMaleSD2, lty=2, col="blue")
  legend(2020, 40, c("Female", "Male"), lty=c(1,1), col=c("red","blue"), bty="n", cex=0.75)

  ymax<-max(outTable$jFemaleMean+outTable$jFemaleSD)
  plot(outTable2$Year, outTable2$jFemaleMean2, type="o", col="red", xlab="Year", ylab="Juvenile Aga", ylim=c(0,ymax))
  lines(outTable2$Year, outTable2$jFemaleMean2+outTable2$jFemaleSD2, lty=2, col="red")
  lines(outTable2$Year, outTable2$jFemaleMean2-outTable2$jFemaleSD2, lty=2, col="red")
  lines(outTable2$Year, outTable2$jMaleMean2, type="o", col="blue")
  lines(outTable2$Year, outTable2$jMaleMean2+outTable2$jMaleSD2, lty=2, col="blue")
  lines(outTable2$Year, outTable2$jMaleMean2-outTable2$jMaleSD2, lty=2, col="blue")
  legend(2020, 55, c("Female", "Male"), lty=c(1,1), col=c("red","blue"), bty="n", cex=0.75)

  plot(outTable2$Year, outTable2$potPairs, type="o", xlab="Year", ylab="Pairs")
}

# Create one row summary table for year one of population
for(i in 1:iterR){
  ## Population Run Loop ##
  for(j in 1:numYear){
    # Determine if typhoon occurred (and add what intensity) and when (not breed, early breeding vs late)
    # Note: Need to calculate likelihood of typhoon based on time of year
    # If typhoon > 0, select random birds lost
    # If typhoon > 0, reduced number of fledglings (based on timing and intensity of storm)
    # Note: Determine percentage of nests per breeding time zone (early vs late)
    # Note: If typhoon late, percentage of nests early equals percent fledglings
    # Note: If typhoon early, percentage of nests early equals percent fledglings
    # Note: If typhoon early and late, use percentage from Morton study
    # Note: repro done after survival because repro output based on years attempts
    # Check harvest table and reduce fledglings by harvest amount
    # Add number of rows equal to fledglings - assign them a value of 1
    # Check release table, add numbers by age class as indicated in release table
  } # End of population run loop
  # Sum number of individuals in each sex and age class per year
} # End of iteration loop

## Export Table and Figures ##
# Calculate mean, sd, and CI for all runs per year, sex and age class
# Export population summary table as csv
# Plot mean and individual runs
