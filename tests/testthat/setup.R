#-------------------------------------------------------------------------------
# Copyright (c) 2021 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials are made available under
# the terms of the Apache License, Version 2.0.
#-------------------------------------------------------------------------------
#
# Datashield test suite set up
#

context("setup - start")

library("httr")
library("DSOpal")
library("DSI")
library("dsBaseClient")

source("dstest_functions/ds_expect_variables.R")

source("connection_to_datasets/connection_utils.R")
source("connection_to_datasets/connect_sem_datasets.R")

ds.test.env <- connection.init()

context("setup - done")
