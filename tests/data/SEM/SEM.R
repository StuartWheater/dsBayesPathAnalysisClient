#!/usr/bin/R

library(readr)
library(rBSEM)

blockList <- list(c(9:28), c(1:5), c(6:8))
varType   <- NULL
SEMGraph  <- matrix(data = c(0,1,1,0,0,1,0,0,0), byrow = TRUE, nrow = 3, ncol = 3)

res <- rBSEM::rHESS_SEM(inFile = "sem_data.txt", outFilePath = "output/", blockList = blockList, varType = varType, SEMGraph = SEMGraph, autoAddIntercept = TRUE,
                        gammaInit = "S", nIter = 200, burnin = 0, nChains = 1, seed = 0, method = 1, writeOutputLevel = 1)

res <- rBSEM::rHESS_SEM(inFile = "na_sem_data.txt", outFilePath = "output/", blockList = blockList, varType = varType, SEMGraph = SEMGraph, autoAddIntercept = TRUE,
                        gammaInit = "S", nIter = 200, burnin = 0, nChains = 1, seed = 0, method = 1, writeOutputLevel = 1)