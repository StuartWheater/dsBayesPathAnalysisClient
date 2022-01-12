#!/usr/bin/R

library(readr)
library(rBSEM)

blockList <- list(c(1:10), c(11:20), c(21:40), c(41:50), c(51:60), c(61))
varType   <- NULL
SEMGraph  <- matrix(data = c(0,1,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0), byrow = TRUE, nrow = 6, ncol = 6)

res <- rBSEM::rHESS_SEM(inFile = "synth_data.txt", outFilePath = "output/", blockList = blockList, varType = varType, SEMGraph = SEMGraph, autoAddIntercept = TRUE,
                        gammaInit = "S", nIter = 10000, burnin = 1000, nChains = 1, seed = 0, method = 1, writeOutputLevel = 1)