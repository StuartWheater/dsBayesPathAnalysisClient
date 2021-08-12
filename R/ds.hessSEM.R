#'
#' @title Combines values into a vector or list in the server-side
#' @description Concatenates objects into one vector.
#' @details To avoid combining the character names and not the 
#' 
#' @param resource.name is the name of the shell resource on the server which will be used to
#' obtains the variables on the columns and observations on the rows.
#' @param inFile is the name of the file, obtained via the shell resource on the server, which
#' contains the variables on the columns and observations on the rows.
#' @param blockList.name is the name of the variable on the server which contains the list of
#' blocks in the model; each element of the list contains the (column) indices of the variables
#' in each block, with respect to the data file
#' @param varType.name is the name of the variable on the server which contains variable type
#' for each column in the data file; coded as: 0 - continuous, 1- binary, 2 - categorical. Note
#' that categorical variables cannot be imputed
#' @param SEMGraph.name is the name of the variable on the server which contains graph adjacency
#' matrix representing the SEM structure between the blocks. Edges represented as 2 indicate
#' variables that will be always included in the regression model
#' @param autoAddIntercept is the should the c++ code automatically add an intercept to every
#' equation? Value should be TRUE or FALSE
#' @param gammaInit is the gamma initialisation to either all-zeros ("0"), all ones ("1"),
#' randomly ("R") or (default) MLE-informed ("MLE")
#' @param nIter is the number of iterations for the MCMC procedure
#' @param burnin is the number of iterations (or fraction of iterations) to discard at the
#' start of the chain
#' @param nChains is the number of parallel chains to run
#' @param seed is the pRNG seed
#' @param method is the gamma sampling method, where 0=MC^3 and 1=Thompson-sampling inspired
#' novel method
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return a list from each server/study containing the results of that server/study. Each results in the
#' list is named with the name allocated to the server/study during the login process. The results from
#' each server/study is a list containing the following named items:
#' HESS_beta_1: 
#' HESS_beta_2_MCMC: 
#' HESS_beta_2: 
#' HESS_gamma_1_MCMC: 
#' HESS_gamma_1: 
#' HESS_gamma_2_MCMC: 
#' HESS_gamma_2: 
#' HESS_logP: 
#' HESS_R2_complete_cases_1_MCMC: (if input contains NAs) 
#' HESS_R2_complete_cases_2_MCMC: (if input contains NAs) 
#' HESS_R2_full_data_1_MCMC: 
#' HESS_R2_full_data_2_MCMC: 
#' @author DataSHIELD Development Team
#' @import DSI
#' @export
ds.hessSEM <- function(resource.name, inFile, blockList.name, varType.name, SEMGraph.name, autoAddIntercept, gammaInit, nIter, burnin, nChains, seed, method, datasources=NULL){
    # look for DS connections
    if(is.null(datasources)){
        datasources <- DSI::datashield.connections_find()
    }

    if(is.null(resource.name) || (! is.character(resource.name)) || (length(resource.name) != 1)){
        stop("Please provide the name of the resources to be used", call.=FALSE)
    }

    if (is.null(blockList.name) || (! is.character(blockList.name)) || (length(blockList.name) != 1)){
        stop("Please provide the name of the block list to be used", call.=FALSE)
    }

    if (is.null(varType.name) || (! is.character(varType.name)) || (length(varType.name) != 1)){
        stop("Please provide the name of the var type to be used", call.=FALSE)
    }

    if (is.null(SEMGraph.name) || (! is.character(SEMGraph.name)) || (length(SEMGraph.name) != 1)){
        stop("Please provide the name of the SEM graph to be used", call.=FALSE)
    }

    resource.name.transfer    <- resource.name
    inFile.transfer           <- inFile
    blockList.name.transfer   <- blockList.name
    varType.name.transfer     <- varType.name
    SEMGraph.name.transfer    <- SEMGraph.name
    autoAddIntercept.transfer <- autoAddIntercept
    gammaInit.transfer        <- gammaInit
    nIter.transfer            <- nIter
    burnin.transfer           <- burnin
    nChains.transfer          <- nChains
    seed.transfer             <- seed
    method.transfer           <- method

    # call the server side function that does the job
    calltext <- call("hessSEMDS", as.symbol(resource.name.transfer), inFile.transfer, as.symbol(blockList.name.transfer), as.symbol(varType.name.transfer), as.symbol(SEMGraph.name.transfer), autoAddIntercept.transfer, gammaInit.transfer, nIter.transfer, burnin.transfer, nChains.transfer, seed.transfer, method.transfer)
    DSI::datashield.aggregate(datasources, calltext)
}
