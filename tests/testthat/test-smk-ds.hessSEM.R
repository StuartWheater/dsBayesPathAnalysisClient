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

connect.sem.resource.sem(ds.test.env)

#
# Tests
#

context("ds.hessSEM::smk::no na 1")
test_that("no na hessSEM 1", {
    # list(c(9:28), c(1:5), c(6:8))
    ds.assign(toAssign = "c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)", newobj = "blockList_1")
    ds.assign(toAssign = "c(1,2,3,4,5)", newobj = "blockList_2")
    ds.assign(toAssign = "c(6,7,8)", newobj = "blockList_3")
    ds.list(c("blockList_1", "blockList_2", "blockList_3"), newobj = "blockList")

    # NULL
    ds.assign(toAssign="NULL", newobj = "varType")

    # matrix(c(0, 1, 1, 0, 0, 1, 0, 0, 0), byrow = TRUE, ncol = 3, nrow = 3)
    ds.matrix(mdata = c(0,1,1,0,0,1,0,0,0), nrows.scalar = 3, ncols.scalar = 3, newobj = "SEMGraph")

    res <- ds.hessSEM("D", "sem_data.txt", "blockList", "varType", "SEMGraph", TRUE, "S", 200, 0, 1, 0, 1)

    expect_length(res, 1)
    expect_length(res$sem1, 11)

    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_beta_1_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_beta_1)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_beta_2_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_beta_2)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_gamma_1_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_gamma_1)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_gamma_2_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_gamma_2)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_logP)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_R2_full_data_1_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_R2_full_data_2_MCMC)))
})

context("ds.hessSEM::smk::no na 2")
test_that("no na hessSEM 2", {
    # list(c(9:28), c(1:5), c(6:8))
    ds.assign(toAssign = "c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)", newobj = "blockList_1")
    ds.assign(toAssign = "c(1,2,3,4,5)", newobj = "blockList_2")
    ds.assign(toAssign = "c(6,7,8)", newobj = "blockList_3")
    ds.list(c("blockList_1", "blockList_2", "blockList_3"), newobj = "blockList")

    # NULL
    ds.assign(toAssign="NULL", newobj = "varType")

    # matrix(c(0, 1, 1, 0, 0, 1, 0, 0, 0), byrow = TRUE, ncol = 3, nrow = 3)
    ds.matrix(mdata = c(0,1,1,0,0,1,0,0,0), nrows.scalar = 3, ncols.scalar = 3, newobj = "SEMGraph")

    res <- ds.hessSEM("D", "sem_data.txt", "blockList", "varType", "SEMGraph", TRUE, "S", 2000, 0, 1, 0, 1)

    expect_length(res, 1)
    expect_length(res$sem1, 11)

    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_beta_1_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_beta_1)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_beta_2_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_beta_2)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_gamma_1_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_gamma_1)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_gamma_2_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_gamma_2)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_logP)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_R2_full_data_1_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_R2_full_data_2_MCMC)))
})

context("ds.hessSEM::smk::na 1")
test_that("na hessSEM 1", {
    # list(c(9:28), c(1:5), c(6:8))
    ds.assign(toAssign = "c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)", newobj = "blockList_1")
    ds.assign(toAssign = "c(1,2,3,4,5)", newobj = "blockList_2")
    ds.assign(toAssign = "c(6,7,8)", newobj = "blockList_3")
    ds.list(c("blockList_1", "blockList_2", "blockList_3"), newobj = "blockList")

    # NULL
    ds.assign(toAssign="NULL", newobj = "varType")

    # matrix(c(0, 1, 1, 0, 0, 1, 0, 0, 0), byrow = TRUE, ncol = 3, nrow = 3)
    ds.matrix(mdata = c(0,1,1,0,0,1,0,0,0), nrows.scalar = 3, ncols.scalar = 3, newobj = "SEMGraph")

    res <- ds.hessSEM("D", "na_sem_data.txt", "blockList", "varType", "SEMGraph", TRUE, "S", 200, 0, 1, 0, 1)

    expect_length(res, 1)
    expect_length(res$sem1, 13)

    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_beta_1_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_beta_1)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_beta_2_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_beta_2)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_gamma_1_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_gamma_1)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_gamma_2_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_gamma_2)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_logP)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_R2_complete_cases_1_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_R2_complete_cases_2_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_R2_full_data_1_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_R2_full_data_2_MCMC)))
})

context("ds.hessSEM::smk::na 2")
test_that("na hessSEM 2", {
    # list(c(9:28), c(1:5), c(6:8))
    ds.assign(toAssign = "c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)", newobj = "blockList_1")
    ds.assign(toAssign = "c(1,2,3,4,5)", newobj = "blockList_2")
    ds.assign(toAssign = "c(6,7,8)", newobj = "blockList_3")
    ds.list(c("blockList_1", "blockList_2", "blockList_3"), newobj = "blockList")

    # NULL
    ds.assign(toAssign="NULL", newobj = "varType")

    # matrix(c(0, 1, 1, 0, 0, 1, 0, 0, 0), byrow = TRUE, ncol = 3, nrow = 3)
    ds.matrix(mdata = c(0,1,1,0,0,1,0,0,0), nrows.scalar = 3, ncols.scalar = 3, newobj = "SEMGraph")

    res <- ds.hessSEM("D", "na_sem_data.txt", "blockList", "varType", "SEMGraph", TRUE, "S", 2000, 0, 1, 0, 1)

    expect_length(res, 1)
    expect_length(res$sem1, 13)

    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_beta_1_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_beta_1)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_beta_2_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_beta_2)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_gamma_1_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_gamma_1)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_gamma_2_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_gamma_2)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_logP)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_R2_complete_cases_1_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_R2_complete_cases_2_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_R2_full_data_1_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$HESS_R2_full_data_2_MCMC)))
})

#
# Done
#

context("ds.hessSEM::smk::shutdown")

disconnect.sem.resource.sem(ds.test.env)

context("ds.hessSEM::smk::done")
