#-------------------------------------------------------------------------------
# Copyright (c) 2021 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials are made available under
# the terms of the Apache License, Version 2.0.
#-------------------------------------------------------------------------------

# SEM dataset

connect.sem.dataset.sem <- function(ds.test.env, variables)
{
    if (is.null(ds.test.env))
    {
        stop("No test specification environment.")
    }

    logout.server(ds.test.env)

    if (ds.test.env$driver == "OpalDriver")
    {
        builder <- DSI::newDSLoginBuilder(.silent = TRUE)
        builder$append(server = "sem1", url = ds.test.env$url_1, user = ds.test.env$user_1, password = ds.test.env$password_1, table = "SEM.SEM", options=ds.test.env$options_1)
        ds.test.env$login.data <- builder$build()
    }
    else
    {
        stop(paste0("Unknown Driver: ", ds.test.env$driver))
    }
    ds.test.env$stats.var  <- variables

    login.server(ds.test.env)
}

disconnect.sem.dataset.sem <- function(ds.test.env)
{
    logout.server(ds.test.env)
}

# SEM_NA dataset

connect.sem.dataset.semna <- function(ds.test.env, variables)
{
    if (is.null(ds.test.env))
    {
        stop("No test specification environment.")
    }

    logout.server(ds.test.env)

    if (ds.test.env$driver == "OpalDriver")
    {
        builder <- DSI::newDSLoginBuilder(.silent = TRUE)
        builder$append(server = "sem1", url = ds.test.env$url_1, user = ds.test.env$user_1, password = ds.test.env$password_1, table = "SEM.SEMNA", options=ds.test.env$options_1)
        ds.test.env$login.data <- builder$build()
    }
    else
    {
        stop(paste0("Unknown Driver: ", ds.test.env$driver))
    }
    ds.test.env$stats.var  <- variables

    login.server(ds.test.env)
}

disconnect.sem.dataset.semna <- function(ds.test.env)
{
    logout.server(ds.test.env)
}
