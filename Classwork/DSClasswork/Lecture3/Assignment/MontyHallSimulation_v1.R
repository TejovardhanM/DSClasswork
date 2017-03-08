rm(list=ls())
require (plyr)
require(ggplot2)
require(scales)

MontyHall.Problem.Simulation <- function (noofgates, r, strategy)
{
  dfmontymatrix <- data.frame(matrix(0 ,ncol = 6, nrow = r))
  cnames <- c("Gatechosen", "PrizeGate","MontySelection","Strategy", "Result", "Noofsimulations")
  colnames(dfmontymatrix) <- cnames
  #noofgates <- 1:3

  dfmontymatrix[, "Noofsimulations"] <- r  # No of simulations.
  for (ri in 1: r)
  {
    indexprize <- sample(noofgates, 1, replace = TRUE)
    Gatechosen <- sample(noofgates, 1, replace = TRUE)
    
    if(indexprize != Gatechosen)
      {
      MontySelection <- noofgates[-c(indexprize, Gatechosen)]
    } else 
      { 
          MontySelection <- sample(noofgates[-c(indexprize, Gatechosen)], 1)
      } 
    
    dfmontymatrix[ri, "Gatechosen"] <- Gatechosen
    dfmontymatrix[ri, "PrizeGate"] <- indexprize
    dfmontymatrix[ri, "MontySelection"] <- MontySelection
    #dfmontymatrix[ri, "Strategy"] <- sample(c("stay", "switch"), 1, replace = TRUE) 
    dfmontymatrix[ri, "Strategy"] <- sample(strategy, 1, replace = TRUE) 
    
    if (dfmontymatrix[ri,"Strategy"] == "stay")
    {
      if (dfmontymatrix[ri,"Gatechosen"] == dfmontymatrix[ri, "PrizeGate"])
      {
        dfmontymatrix[ri, "Result"] = "win"
      } 
      else 
      {
        dfmontymatrix[ri, "Result"] = "loss"
      } 
    } else
    {
      
      if (dfmontymatrix[ri,"Gatechosen"] != dfmontymatrix[ri, "PrizeGate"])
      {
        dfmontymatrix[ri, "Result"] = "win"
      } 
      else 
      {
        dfmontymatrix[ri, "Result"] = "loss"
      }   
    }
  }
  
  return (with (dfmontymatrix, table(Noofsimulations, Strategy, Result)))
  
}
################################
##Visualization 1:
r <- c( 1000, 10000, 20000)
strategy <- c("stay", "switch") 
noofgates <- 1:3

sampledata <- lapply(r, function(r) MontyHall.Problem.Simulation(noofgates, r, strategy))

final.output <- ldply (sampledata, data.frame)
final.output$Noofsimulations <- as.integer(as.character(final.output$Noofsimulations))
final.output <- mutate(final.output, 
                       Percentage = percent(Freq/ Noofsimulations))

ggplot(final.output, aes(Result,Freq, fill = Strategy ))+
  geom_bar( stat = "identity")+
  geom_text(aes(label = Percentage))+
  facet_wrap(~Noofsimulations, scales = "free" )+
  labs(title = "Analysis of strategies involved for Monty Hall problem ", y = "No of Simulations", x = "Strategy Result")

print (final.output)

##Visualization 2:
strategy <-  "switch" 

sampledata <- lapply(r, function(r) MontyHall.Problem.Simulation(noofgates, r, strategy))

final.output <- ldply (sampledata, data.frame)
final.output$Noofsimulations <- as.integer(as.character(final.output$Noofsimulations))
final.output <- mutate(final.output, 
                       Percentage = percent(Freq/ Noofsimulations))


ggplot(final.output, aes(Result,Freq, fill = Strategy ))+
  geom_bar( stat = "identity")+
  geom_text(aes(label = Percentage))+
  facet_wrap(~Noofsimulations, scales = "free" )+
  labs(title = "Analysis of switching strategy for Monty Hall problem ", y = "No of Simulations", x = "Strategy Result")


##Visualization 3:
strategy <-  "stay" 

sampledata <- lapply(r, function(r) MontyHall.Problem.Simulation(noofgates, r, strategy))

final.output <- ldply (sampledata, data.frame)
final.output$Noofsimulations <- as.integer(as.character(final.output$Noofsimulations))
final.output <- mutate(final.output, 
                       Percentage = percent(Freq/ Noofsimulations))

ggplot(final.output, aes(Result,Freq, fill = Strategy ))+
  geom_bar( stat = "identity")+
  geom_text(aes(label = Percentage))+
  facet_wrap(~Noofsimulations, scales = "free" )+
  labs(title = "Analysis of not switching strategy for Monty Hall problem ", y = "No of Simulations", x = "Strategy Result")

