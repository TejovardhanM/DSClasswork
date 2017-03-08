rm(list=ls())
simulatemonty <- function (r)
{
    dfmontymatrix <- data.frame(matrix(0 ,ncol = 7, nrow = r))
    cnames <- c("Gate1" , "Gate2" , "Gate3" ,"Gatechosen"   ,"Stayorswitch", "status", "Noofsimulations")
    colnames(dfmontymatrix) <- cnames
    noofgates <- 1:3
    dfmontymatrix[, "Noofsimulations"] <- r  # No of simulations.
  
  for (ri in 1: r)
  {
    indexprize <- sample(noofgates, 1, replace = TRUE)
    dfmontymatrix[ri, indexprize] <- 1   #assign
    dfmontymatrix[ri, "Gatechosen"] <- sample(noofgates, 1, replace = TRUE)   # Gates Chosen/ Initial Selection
    dfmontymatrix[ri, "Stayorswitch"] <- sample(c("stay", "switch"), 1, replace = TRUE) 
  
    
    if (dfmontymatrix[ri,dfmontymatrix[ri,"Gatechosen"]] == 1 && dfmontymatrix[ri,"Stayorswitch"] == "stay")
    {
      dfmontymatrix[ri, "status"] = "win"
    }
    else if (dfmontymatrix[ri,dfmontymatrix[ri,"Gatechosen"]] == 1 && dfmontymatrix[ri,"Stayorswitch"] == "switch")
    {
      dfmontymatrix[ri, "status"] = "loss"
    }
    else  if (dfmontymatrix[ri,dfmontymatrix[ri,"Gatechosen"]] == 0 && dfmontymatrix[ri,"Stayorswitch"] == "switch")
    {
      dfmontymatrix[ri, "status"] = "win"
    }
    else  if (dfmontymatrix[ri,dfmontymatrix[ri,"Gatechosen"]] == 0 && dfmontymatrix[ri,"Stayorswitch"] == "stay")
    {
      dfmontymatrix[ri, "status"] = "loss"
    }   
    
  }

    output.dataframe <- table(dfmontymatrix$Stayorswitch, dfmontymatrix$status)
    barplot(output.dataframe, legend = rownames(output.dataframe), text = "test")  
  return (output.dataframe)
  
}

r <- c(100, 1000, 10000)#, 100000)
sampledata <- lapply(r, function(r) simulatemonty(r))


barplot(sampledata[[1]], legend = rownames(sampledata[[1]]), title(main =  "test"))  
