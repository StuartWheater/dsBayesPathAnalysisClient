#-------------------------------------------------------------------------------
# Copyright (c) 2021 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials are made available under
# the terms of the Apache License, Version 2.0.
#-------------------------------------------------------------------------------

#
# Set up
#

context("ds.hessSEM::smk::setup")

connect.sem.dataset.sem(ds.test.env, list("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25", "v26", "v27", "v28"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.hessSEM::smk")
test_that("simple hessSEM", {
    res <- ds.hessSEM("D", 1, 1, 0, 200)

    expect_length(res, 1)
})

#
# Done
#

context("ds.hessSEM::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.sem.dataset.sem(ds.test.env)

context("ds.hessSEM::smk::done")
