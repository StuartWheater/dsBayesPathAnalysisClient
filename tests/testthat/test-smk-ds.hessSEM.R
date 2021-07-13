#-------------------------------------------------------------------------------
# Copyright (c) 2019-2021 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Set up
#

context("ds.hessSEM::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.hessSEM::smk")
test_that("simple c", {
    res <- ds.hessSEM("D$LAB_TSC")

    expect_length(res, 0)
})

#
# Done
#

context("ds.hessSEM::smk::shutdown")

test_that("setup", {
    ds_expect_variables(c("D", "c.newobj"))
})

disconnect.studies.dataset.cnsim()

context("ds.hessSEM::smk::done")
