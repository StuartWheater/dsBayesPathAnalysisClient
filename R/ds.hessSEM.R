#'
#' @title Combines values into a vector or list in the server-side
#' @description Concatenates objects into one vector.
#' @details To avoid combining the character names and not the 
#' 
#' @param x a vector of character string providing the names of the objects to be combined.
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return  \code{ds.c} returns the vector of concatenating R
#' @author DataSHIELD Development Team
#' @import DSI
#' @export
ds.hessSEM <- function(x=NULL, datasources=NULL){
    # look for DS connections
    if(is.null(datasources)){
        datasources <- DSI::datashield.connections_find()
    }

    if(is.null(x)){
        stop("x=NULL. Please provide the names of the objects to concatenate!", call.=FALSE)
    }

    x.transfer <- x

    # call the server side function that does the job
    calltext <- call("hessSEMDS", x.transfer)
    DSI::datashield.aggregate(datasources, calltext)
}
