#-------------------------------------------------------------------------------
# Copyright (c) 2021 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials are made available under
# the terms of the Apache License, Version 2.0.
#-------------------------------------------------------------------------------

connection.init <- function()
{
    path            <- getwd()
    sub.folder.name <- "/connection_to_datasets/"
    file.name       <- "local_settings.csv"

    file.path <- paste(path, sub.folder.name, file.name, sep="")

    if (file.exists(file.path))
    {
        content <- read.csv(file.path, header = FALSE)
        ip.address <- as.character(content[[1]][1])
    }
    else
    {
        ip.address <- "127.0.0.1"
    }

    ds.test_env <- new.env()

    # this option helps DSIList to find the connection objects by looking in the right environment
    options(datashield.env=ds.test_env)

    # switch tetween "DSLiteDriver" and "OpalDriver" to test
    # ds.test_env$driver <- "OpalDriver"
    # ds.test_env$driver <- "DSLiteDriver"
    ds.test_env$driver <- "OpalDriver"

    ds.test_env$server_ip_address <- ip.address
  
    # opal_url <- paste("http://", ds.test_env$server_ip_address, ":8080", sep="")
    opal_url <- paste("https://", ds.test_env$server_ip_address, ":8443", sep="")
    # opal_url <- paste("http://", ds.test_env$server_ip_address, ":8080", sep="")

    #This TCP/IP address is required to test a connect to the server. 
    ds.test_env$ping_address <- opal_url
    # ds.test_env$ping_config  <- config(timeout=5)
    ds.test_env$ping_config  <- config(timeout=5, ssl_verifyhost=0, ssl_verifypeer=0)

    ds.test_env$url_1 <- opal_url

    ds.test_env$user_1 <- getOption("opal.user", "administrator")

    ds.test_env$password_1 <- getOption("opal.password", "datashield_test&")

    ds.test_env$options_1 <- "list(ssl_verifyhost=0, ssl_verifypeer=0)"

    ds.test_env$secure_login_details <- TRUE
    ds.test_env$tolerance            <- 10^-6

    return (ds.test_env)
}

login.server <- function(ds.test.env)
{
    if (length(ds.test.env$stats.var) == 0) {
        ds.test.env$connections <- datashield.login(logins = ds.test.env$login.data, assign = TRUE, symbol = "D")
    } else {
        ds.test.env$connections <- datashield.login(logins = ds.test.env$login.data, assign = TRUE, variables = ds.test.env$stats.var, symbol = "D")
    }
}

logout.server <- function(ds.test.env)
{
    if (exists("connections", envir = ds.test.env))
    {
        datashield.logout(ds.test.env$connections)
        rm("connections", envir = ds.test.env)
    }
    rm(list = ls())
    gc()
}
