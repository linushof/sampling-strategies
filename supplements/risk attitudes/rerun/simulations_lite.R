#Set working directory
pathname<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(pathname)


#Properties of the lottery problem
out.safe <- 10
p.safe <- 1

#No rare event
#medium risk
#out.risky <- c(15,5)
#p.risky <- c(.5,.5)
#high risk
#out.risky <- c(19,1)
#p.risky <- c(.5,.5)

#out.risky <- c(17,3)
#p.risky <- c(.5,.5)

#out.risky <- c(10,10)
#p.risky <- c(.5,.5)


#Rare event is attractive
#out.risky <- c(19,9)
#p.risky <- c(.1,.9)
# in a few cases, the risky option has a advantage if the attractive event is sampled
# this advantage is smallest if accumulation is very balanced (closest approximation to EV max), 
### or highly biased (random choice). 
### in between, it may be that the risky option is slightly more likely to finish under bias than the safe option

#Rare event is unattractive
out.risky <- c(1,11)
p.risky <- c(.1,.9)
# in a few cases, the risky option has a disadvantage if the unattractive event is sampled
# this advantage is smallest if accumulation is very balanced (closest approximation to EV max), 
### or highly biased (random choice). 
### in between, it may be that the risky option is slightly less likely to finish under bias, than the safe option
# however, this should be the case in only a small fraction of trials, thus the little bump


#############################################################
###### Sampling strategy with summary comparison rule

# Define tested parameter values
psi.store <- c(.1,.3,.5,.7,.9)
theta.store <- c(15,45,75)

choice.store <- matrix(NA, ncol=length(psi.store),nrow = length(theta.store))

for (k in 1:length(theta.store)){
  for (j in 1:length(psi.store)){
    psi <- psi.store[j] #Switching probability
    theta <- theta.store[k]
    nRuns <- 10000
    choice <- vector(length = nRuns)
    
    for (i in 1:nRuns){
      
      #Initialize variables
      currentOption <- NA
      DV.safe <- 0
      DV.risky <- 0
      nSamples.safe <- 0
      nSamples.risky <- 0 
      
      # Determine from which option samples are drawn first
      initial.option <- rbinom(1,1,.5) # 1=safe,0=risky 
      if (initial.option == 1) {
        currentOption <- 1 #safe option
      } else {
        currentOption <- 0 #risky option
      }
      
      #Start sampling
      if (currentOption==1){
        nSamples.safe <- nSamples.safe + 1
        DV.safe <- DV.safe + out.safe[rbinom(1,1,p.safe)] 
      } else {
        nSamples.risky <- nSamples.risky + 1
        DV.risky <- DV.risky + out.risky[rbinom(1,1,p.risky[2])+1] 
      }
      
      while (abs(DV.safe-DV.risky)<theta){
        # Determine whether to switch or stay
        switch <- rbinom(1,1,psi) 
        if (switch==1){
          currentOption <- abs(currentOption-1)
        } else {
          currentOption <- currentOption
        }
        
        
        # Draw sample
        if (currentOption==1){
          nSamples.safe <- nSamples.safe + 1
          DV.safe <- DV.safe + out.safe[rbinom(1,1,p.safe)] 
        } else {
          nSamples.risky <- nSamples.risky + 1
          DV.risky <- DV.risky + out.risky[rbinom(1,1,p.risky[2])+1] 
        }
      }
      
      if (DV.safe>DV.risky){
        choice[i] <- 1 #safe option chosen
      } else {
        choice[i] <- 0 #risky option chosen
      }
    }
    choice.store[k,j] <- mean(choice)
  }
}
saveRDS(choice.store, file = "summary_no rare_mediumrisk.rds")
saveRDS(choice.store, file = "summary_rare attractive.rds")
saveRDS(choice.store, file = "summary_rare unattractive.rds")

#5*7
plot(psi.store,choice.store[1,],type = "p",ylim = c(0.4,0.6), xlab = expression(paste("Switching probability (", psi, ")")), ylab = "Proportion of choices of the safe option", pch=16, col= "green",
     main = c(paste("(",out.safe,", ",p.safe,"); (", out.risky[1],",",p.risky[1], "; ", out.risky[2], ", ", p.risky[2],")")))
abline(h=.5,lty=2)
points(psi.store,choice.store[2,], pch=17, col= "blue")
points(psi.store,choice.store[3,], pch=19, col= "red")
legend(.1,.6, legend = c(expression(paste(theta, " = 15")),expression(paste(theta, " = 45")), expression(paste(theta, " = 75"))), pch = c(16,17,19),col= c("green","blue","red"),bty="n",y.intersp = 1.2, title = "Decision threshold")


choice.store <- readRDS("summary_no rare_highrisk.rds")
choice.store.norare <- readRDS("summary_no rare.rds")
choice.store.attract <- readRDS("summary_rare attractive.rds")
choice.store.unattract <- readRDS("summary_rare unattractive.rds")

psi.store <- c(.1,.3,.5,.7,.9)
par(mfrow = c(3,1)) #10*3.8
plot(psi.store,choice.store.norare[1,],type = "p",ylim = c(0,1), xlab = expression(paste("Switching probability (", psi, ")")), ylab = "Proportion of choices of the safe option", pch=16, col= "green", main = "Summary comparison rule: No rare event")
abline(h=.5,lty=2)
points(psi.store,choice.store.norare[2,], pch=17, col= "blue")
points(psi.store,choice.store.norare[3,], pch=19, col= "red")
legend(.17,1, legend = c(expression(paste(theta, " = 15")),expression(paste(theta, " = 45")), expression(paste(theta, " = 75"))), pch = c(16,17,19),col= c("green","blue","red"),bty="n",y.intersp = .5, title = "Decision threshold")


plot(psi.store,choice.store.attract[1,],type = "p",ylim = c(.4,.6), xlab = expression(paste("Switching probability (", psi, ")")), ylab = "Proportion of choices of the safe option", pch=16, col= "green", main = "Summary comparison rule: Rare event attractive")
abline(h=.5,lty=2)
points(psi.store,choice.store.attract[2,], pch=17, col= "blue")
points(psi.store,choice.store.attract[3,], pch=19, col= "red")


plot(psi.store,choice.store.unattract[1,],type = "p",ylim = c(.4,.6), xlab = expression(paste("Switching probability (", psi, ")")), ylab = "Proportion of choices of the safe option", pch=16, col= "green", main = "Summary comparison rule: Rare event unattractive")
abline(h=.5,lty=2)
points(psi.store,choice.store.unattract[2,], pch=17, col= "blue")
points(psi.store,choice.store.unattract[3,], pch=19, col= "red")




#############################################################
###### Sampling strategy with roundwise comparison rule


#Properties of the lottery problem
out.safe <- 10
p.safe <- 1

#No rare event
out.risky <- c(15,5)
p.risky <- c(.5,.5)

#Rare event is attractive
#out.risky <- c(19,9)
#p.risky <- c(.1,.9)

#Rare event is unattractive
out.risky <- c(1,11)
p.risky <- c(.1,.9)


# Define tested parameter values
psi.store <- c(.1,.3,.5,.7,.9)
theta.store <- c(1,3,5)

choice.store <- matrix(NA, ncol=length(psi.store),nrow = length(theta.store))

for (k in 1:length(theta.store)){
  for (j in 1:length(psi.store)){
    psi <- psi.store[j] #Switching probability
    theta <- theta.store[k] #Decision threshold

    #psi <- .5 #Switching probability
    #theta <- 3 #Decision threshold
    
    nRuns <- 100000
    choice <- vector(length = nRuns)
    
    for (i in 1:nRuns){
      
      #Initialize variables
      currentOption <- NA
      samples.safe <- 0
      samples.risky <- 0
      DV <- 0
      nSamples.safe <- 0
      nSamples.risky <- 0 
      nRoundSamples.safe <- 0
      nRoundSamples.risky <- 0 
      
      # Determine from which option samples are drawn first
      initial.option <- rbinom(1,1,.5) # 1=safe,0=risky 
      if (initial.option == 1) {
        currentOption <- 1 #safe option
      } else {
        currentOption <- 0 #risky option
      }
      
      #Start sampling
      if (currentOption==1){
        nSamples.safe <- nSamples.safe + 1
        nRoundSamples.safe <- nRoundSamples.safe + 1
        samples.safe <- samples.safe + out.safe[rbinom(1,1,p.safe)] 
      } else {
        nSamples.risky <- nSamples.risky + 1
        nRoundSamples.risky <- nRoundSamples.risky + 1
        samples.risky <- samples.risky + out.risky[rbinom(1,1,p.risky[2])+1] 
      }
      
      roundstart <- TRUE
      
      while (abs(DV)<theta){
        # Determine whether to switch or stay
        switch <- rbinom(1,1,psi) 
        if (switch==1){
          currentOption <- abs(currentOption-1)
          roundstart <- FALSE
        } else {
          currentOption <- currentOption
        }
        
        # Draw sample
        if (currentOption==1){
          nSamples.safe <- nSamples.safe + 1
          nRoundSamples.safe <- nRoundSamples.safe + 1
          samples.safe <- samples.safe + out.safe[rbinom(1,1,p.safe)] 
        } else {
          nSamples.risky <- nSamples.risky + 1
          nRoundSamples.risky <- nRoundSamples.risky + 1
          samples.risky <- samples.risky + out.risky[rbinom(1,1,p.risky[2])+1] 
        }
        
        if (roundstart==FALSE){
          DV <- DV + ((samples.safe/nRoundSamples.safe-samples.risky/nRoundSamples.risky)>0)*1 + ((samples.safe/nRoundSamples.safe-samples.risky/nRoundSamples.risky)<0)*-1
          roundstart <- TRUE
          nRoundSamples.safe <- 0
          nRoundSamples.risky <- 0 
        } 
        
      }
      
      if (DV>0){
        choice[i] <- 1 #safe option chosen
      } else {
        choice[i] <- 0 #risky option chosen
      }
    }
    choice.store[k,j] <- mean(choice)
  }
}
View(choice)

saveRDS(choice.store, file = "roundwise_no rare.rds")
saveRDS(choice.store, file = "roundwise_rare attractive.rds")
saveRDS(choice.store, file = "roundwise_rare unattractive.rds")


plot(psi.store,choice.store[1,],type = "p",ylim = c(0,1), xlab = expression(paste("Switching probability (", psi, ")")), ylab = "Proportion of choices of the safe option", pch=16, col= "green")
abline(h=.5,lty=2)
points(psi.store,choice.store[2,], pch=17, col= "blue")
points(psi.store,choice.store[3,], pch=19, col= "red")
legend(.1,1, legend = c(expression(paste(theta, " = 1")),expression(paste(theta, " = 3")), expression(paste(theta, " = 5"))), pch = c(16,17,19),col= c("green","blue","red"),bty="n",y.intersp = .8, title = "Decision threshold")



choice.store.norare <- readRDS("roundwise_no rare.rds")
choice.store.attract <- readRDS("roundwise_rare attractive.rds")
choice.store.unattract <- readRDS("roundwise_rare unattractive.rds")


psi.store <- c(.1,.3,.5,.7,.9)
par(mfrow = c(3,1))#10*3.8
plot(psi.store,choice.store.norare[1,],type = "p",ylim = c(0,1), xlab = expression(paste("Switching probability (", psi, ")")), ylab = "Proportion of choices of the safe option", pch=16, col= "green", main = "Roundwise comparison rule: No rare event")
abline(h=.5,lty=2)
points(psi.store,choice.store.norare[2,], pch=17, col= "blue")
points(psi.store,choice.store.norare[3,], pch=19, col= "red")
legend(.17,1, legend = c(expression(paste(theta, " = 1")),expression(paste(theta, " = 3")), expression(paste(theta, " = 5"))), pch = c(16,17,19),col= c("green","blue","red"),bty="n",y.intersp = .5, title = "Decision threshold")


plot(psi.store,choice.store.attract[1,],type = "p",ylim = c(0,1), xlab = expression(paste("Switching probability (", psi, ")")), ylab = "Proportion of choices of the safe option", pch=16, col= "green", main = "Roundwise comparison rule: Rare event attractive")
abline(h=.5,lty=2)
points(psi.store,choice.store.attract[2,], pch=17, col= "blue")
points(psi.store,choice.store.attract[3,], pch=19, col= "red")


plot(psi.store,choice.store.unattract[1,],type = "p",ylim = c(0,1), xlab = expression(paste("Switching probability (", psi, ")")), ylab = "Proportion of choices of the safe option", pch=16, col= "green", main = "Roundwise comparison rule: Rare event unattractive")
abline(h=.5,lty=2)
points(psi.store,choice.store.unattract[2,], pch=17, col= "blue")
points(psi.store,choice.store.unattract[3,], pch=19, col= "red")


