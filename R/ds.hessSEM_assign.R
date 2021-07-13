#'
#' @title Combines values into a vector or list in the server-side
#' @description Concatenates objects into one vector.
#' @details To avoid combining the character names and not the 
#' 
#' @param x a vector of character string providing the names of the objects to be combined.
#' @param newobj a character string that provides the name for the output object 
#' that is stored on the data servers. Default \code{c.newobj}.
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return  \code{ds.c} returns the vector of concatenating R
#' @author DataSHIELD Development Team
#' @import DSI
#' @export
ds.hessSEM_assign <- function(x=NULL, newobj=NULL, datasources=NULL){
    # look for DS connections
    if(is.null(datasources)){
        datasources <- DSI::datashield.connections_find()
    }

    if(is.null(x)){
        stop("x=NULL. Please provide the names of the objects to concatenate!", call.=FALSE)
    }

    # create a name by default if user did not provide a name for the new variable
    if(is.null(newobj)){
        newobj <- "hassSEM.newobj"
    }

    x.transfer <- x

    # call the server side function that does the job
    calltext <- call("hessSEM.assignDS", x.transfer)
    DSI::datashield.assign(datasources, newobj, calltext)
}
