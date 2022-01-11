#-------------------------------------------------------------------------------
# Copyright (c) 2021 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials are made available under
# the terms of the Apache License, Version 2.0.
#-------------------------------------------------------------------------------

#
# Set up
#

context("ds.hessSEM.assign::smk::setup")

connect.sem.resource.sem(ds.test.env)

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.hessSEM.assign::smk")
test_that("simple hessSEM.assign", {
    # list(c(9:28), c(1:5), c(6:8))
    ds.assign(toAssign = "c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)", newobj = "blockList_1")
    ds.assign(toAssign = "c(1,2,3,4,5)", newobj = "blockList_2")
    ds.assign(toAssign = "c(6,7,8)", newobj = "blockList_3")
    ds.list(c("blockList_1", "blockList_2", "blockList_3"), newobj = "blockList")

    # NULL
    ds.assign(toAssign="NULL", newobj = "varType")

    # matrix(c(0, 1, 1, 0, 0, 1, 0, 0, 0), byrow = TRUE, ncol = 3, nrow = 3)
    ds.matrix(mdata = c(0,1,1,0,0,1,0,0,0), byrow = TRUE, nrows.scalar = 3, ncols.scalar = 3, newobj = "SEMGraph")

    res <- ds.hessSEM.assign("D", "sem_data.txt", "blockList", "varType", "SEMGraph", TRUE, "S", 200, 0, 1, 0, 1)

    expect_length(res, 0)

    res_class <- ds.class("hessSEM.newobj")

    expect_length(res_class, 1)
    expect_equal(res_class$sem1, "list")
})

context("ds.hessSEM.assign::smk with newobj")
test_that("simple hessSEM.assign, with newobj", {
    # list(c(9:28), c(1:5), c(6:8))
    ds.assign(toAssign = "c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)", newobj = "blockList_1")
    ds.assign(toAssign = "c(1,2,3,4,5)", newobj = "blockList_2")
    ds.assign(toAssign = "c(6,7,8)", newobj = "blockList_3")
    ds.list(c("blockList_1", "blockList_2", "blockList_3"), newobj = "blockList")

    # NULL
    ds.assign(toAssign="NULL", newobj = "varType")

    # matrix(c(0, 1, 1, 0, 0, 1, 0, 0, 0), byrow = TRUE, ncol = 3, nrow = 3)
    ds.matrix(mdata = c(0,1,1,0,0,1,0,0,0), byrow = TRUE, nrows.scalar = 3, ncols.scalar = 3, newobj = "SEMGraph")

    res <- ds.hessSEM.assign("D", "sem_data.txt", "blockList", "varType", "SEMGraph", TRUE, "S", 200, 0, 1, 0, 1, newobj = "hassSEM.obj")

    expect_length(res, 0)

    res_class <- ds.class("hassSEM.obj")

    expect_length(res_class, 1)
    expect_equal(res_class$sem1, "list")
})

#
# Done
#

context("ds.hessSEM.assign::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "blockList", "blockList_1", "blockList_2", "blockList_3", "varType", "SEMGraph", "hessSEM.newobj", "hassSEM.obj"))
})

disconnect.sem.resource.sem(ds.test.env)

context("ds.hessSEM.assign::smk::done")
