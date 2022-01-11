#-------------------------------------------------------------------------------
# Copyright (c) 2021 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials are made available under
# the terms of the Apache License, Version 2.0.
#-------------------------------------------------------------------------------

#
# Set up
#

context("ds.hessSEM::smk_expt::setup")

connect.sem.resource.sem(ds.test.env)

#
# Tests
#

context("ds.hessSEM::smk_expt::no na 1")
test_that("no na hessSEM 1", {
    # list(c(9:28), c(1:5), c(6:8))
    ds.assign(toAssign = "c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)", newobj = "blockList_1")
    ds.assign(toAssign = "c(1,2,3,4,5)", newobj = "blockList_2")
    ds.assign(toAssign = "c(6,7,8)", newobj = "blockList_3")
    ds.list(c("blockList_1", "blockList_2", "blockList_3"), newobj = "blockList")

    # NULL
    ds.assign(toAssign="NULL", newobj = "varType")

    # matrix(c(0, 1, 1, 0, 0, 1, 0, 0, 0), byrow = TRUE, ncol = 3, nrow = 3)
    ds.matrix(mdata = c(0,1,1,0,0,1,0,0,0), byrow = TRUE, nrows.scalar = 3, ncols.scalar = 3, newobj = "SEMGraph")

    res <- ds.hessSEM("D", "sem_data.txt", "blockList", "varType", "SEMGraph", TRUE, "S", 200, 0, 1, 0, 1)

    expect_equal_to_reference(res, 'smk_expt-results/ds.hessSEM-nona-1.rds')
})

context("ds.hessSEM::smk_expt::no na 2")
test_that("no na hessSEM 2", {
    # list(c(9:28), c(1:5), c(6:8))
    ds.assign(toAssign = "c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)", newobj = "blockList_1")
    ds.assign(toAssign = "c(1,2,3,4,5)", newobj = "blockList_2")
    ds.assign(toAssign = "c(6,7,8)", newobj = "blockList_3")
    ds.list(c("blockList_1", "blockList_2", "blockList_3"), newobj = "blockList")

    # NULL
    ds.assign(toAssign="NULL", newobj = "varType")

    # matrix(c(0, 1, 1, 0, 0, 1, 0, 0, 0), byrow = TRUE, ncol = 3, nrow = 3)
    ds.matrix(mdata = c(0,1,1,0,0,1,0,0,0), byrow = TRUE, nrows.scalar = 3, ncols.scalar = 3, newobj = "SEMGraph")

    res <- ds.hessSEM("D", "sem_data.txt", "blockList", "varType", "SEMGraph", TRUE, "S", 2000, 0, 1, 0, 1)

    expect_equal_to_reference(res, 'smk_expt-results/ds.hessSEM-nona-2.rds')
})

context("ds.hessSEM::smk_expt::na 1")
test_that("na hessSEM 1", {
    # list(c(9:28), c(1:5), c(6:8))
    ds.assign(toAssign = "c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)", newobj = "blockList_1")
    ds.assign(toAssign = "c(1,2,3,4,5)", newobj = "blockList_2")
    ds.assign(toAssign = "c(6,7,8)", newobj = "blockList_3")
    ds.list(c("blockList_1", "blockList_2", "blockList_3"), newobj = "blockList")

    # NULL
    ds.assign(toAssign="NULL", newobj = "varType")

    # matrix(c(0, 1, 1, 0, 0, 1, 0, 0, 0), byrow = TRUE, ncol = 3, nrow = 3)
    ds.matrix(mdata = c(0,1,1,0,0,1,0,0,0), byrow = TRUE, nrows.scalar = 3, ncols.scalar = 3, newobj = "SEMGraph")

    res <- ds.hessSEM("D", "na_sem_data.txt", "blockList", "varType", "SEMGraph", TRUE, "S", 200, 0, 1, 0, 1)

    expect_equal_to_reference(res, 'smk_expt-results/ds.hessSEM-na-1.rds')
})

context("ds.hessSEM::smk_expt::na 2")
test_that("na hessSEM 2", {
    # list(c(9:28), c(1:5), c(6:8))
    ds.assign(toAssign = "c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)", newobj = "blockList_1")
    ds.assign(toAssign = "c(1,2,3,4,5)", newobj = "blockList_2")
    ds.assign(toAssign = "c(6,7,8)", newobj = "blockList_3")
    ds.list(c("blockList_1", "blockList_2", "blockList_3"), newobj = "blockList")

    # NULL
    ds.assign(toAssign="NULL", newobj = "varType")

    # matrix(c(0, 1, 1, 0, 0, 1, 0, 0, 0), byrow = TRUE, ncol = 3, nrow = 3)
    ds.matrix(mdata = c(0,1,1,0,0,1,0,0,0), byrow = TRUE, nrows.scalar = 3, ncols.scalar = 3, newobj = "SEMGraph")

    res <- ds.hessSEM("D", "na_sem_data.txt", "blockList", "varType", "SEMGraph", TRUE, "S", 2000, 0, 1, 0, 1)

    expect_equal_to_reference(res, 'smk_expt-results/ds.hessSEM-na-2.rds')
})

#
# Done
#

context("ds.hessSEM::smk_expt::shutdown")

disconnect.sem.resource.sem(ds.test.env)

context("ds.hessSEM::smk_expt::done")
