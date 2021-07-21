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

context("ds.hessSEM::smk::no na")
test_that("no na hessSEM", {
    res <- ds.hessSEM("D", "sem_data.txt", 200, 1, 0, 1)

    expect_length(res, 1)
    expect_length(res$sem1, 11)

    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$sem_data_HESS_beta_1_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$sem_data_HESS_beta_1)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$sem_data_HESS_beta_2_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$sem_data_HESS_beta_2)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$sem_data_HESS_gamma_1_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$sem_data_HESS_gamma_1)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$sem_data_HESS_gamma_2_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$sem_data_HESS_gamma_2)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$sem_data_HESS_logP)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$sem_data_HESS_R2_full_data_1_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$sem_data_HESS_R2_full_data_2_MCMC)))
})

context("ds.hessSEM::smk::na")
test_that("na hessSEM", {
    res <- ds.hessSEM("D", "na_sem_data.txt", 200, 1, 0, 1)

    expect_length(res, 1)
    expect_length(res$sem1, 13)

    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$na_sem_data_HESS_beta_1_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$na_sem_data_HESS_beta_1)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$na_sem_data_HESS_beta_2_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$na_sem_data_HESS_beta_2)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$na_sem_data_HESS_gamma_1_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$na_sem_data_HESS_gamma_1)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$na_sem_data_HESS_gamma_2_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$na_sem_data_HESS_gamma_2)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$na_sem_data_HESS_logP)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$na_sem_data_HESS_R2_complete_cases_1_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$na_sem_data_HESS_R2_complete_cases_2_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$na_sem_data_HESS_R2_full_data_1_MCMC)))
    expect_true(all(c("spec_tbl_df", "tbl_df", "tbl", "data.frame") %in% class(res$sem1$na_sem_data_HESS_R2_full_data_2_MCMC)))
})

#
# Done
#

context("ds.hessSEM::smk::shutdown")

disconnect.sem.resource.sem(ds.test.env)

context("ds.hessSEM::smk::done")
