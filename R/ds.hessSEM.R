#'
#' @title Combines values into a vector or list in the server-side
#' @description Concatenates objects into one vector.
#' @details To avoid combining the character names and not the 
#' 
#' @param resourcename a vector of character string providing the names of the objects to be combined.
#' @param infile is the ...
#' @param nIter is the ...
#' @param nChains is the ...
#' @param seed is the ...
#' @param method is the ...
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return  \code{ds.c} returns the vector of concatenating R
#' @author DataSHIELD Development Team
#' @import DSI
#' @export
ds.hessSEM <- function(resourcename, infile, nIter, nChains, seed, method, datasources=NULL){
    # look for DS connections
    if(is.null(datasources)){
        datasources <- DSI::datashield.connections_find()
    }

    if(is.null(resourcename)){
        stop("Please provide the names of the resources to concatenate!", call.=FALSE)
    }

    resourcename.transfer <- resourcename
    infile.transfer       <- infile
    nIter.transfer        <- nIter
    nChains.transfer      <- nChains
    seed.transfer         <- seed
    method.transfer       <- method

    # call the server side function that does the job
    calltext <- call("hessSEMDS", resourcename.transfer, infile.transfer, nIter.transfer, nChains.transfer, seed.transfer, method.transfer)
    DSI::datashield.aggregate(datasources, calltext)
}
