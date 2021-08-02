#'
#' @title Combines values into a vector or list in the server-side
#' @description Concatenates objects into one vector.
#' @details To avoid combining the character names and not the 
#' 
#' @param resource.name a vector of character string providing the names of the objects to be combined.
#' @param inFile is the ...
#' @param blockList.name is the name of ...
#' @param varType.name is the name of...
#' @param SEMGraph.name is the name of ...
#' @param autoAddIntercept is the ...
#' @param gammaInit is the ...
#' @param nIter is the ...
#' @param burnin is the ...
#' @param nChains is the ...
#' @param seed is the ...
#' @param method is the ...
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
