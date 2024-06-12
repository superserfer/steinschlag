library(testthat)
library(future.apply)
library(dplyr)

source("../monte_carlo.R")

# Write the test case
test_that("kin_energy over 1200 should return 1", {
  df <- data.frame(
    kin_energy = c(1200),
    time_diff = c(1),
    mass = c(100)
  )
  expect_equal(monte_carlo(df, nrow(df)), 1)
})

test_that("kin_energy over 600 with previous 1 2000 kg withing 24h", {
  df <- data.frame(
    kin_energy = c(0, 600),
    time_diff = c(24),
    mass = c(2000)
  )
  expect_equal(monte_carlo(df, nrow(df)), 1)
})


test_that("kin_energy over 600 with previous 1*2000 kg with over 24h time diff", {
  df <- data.frame(
    kin_energy = c(0, 600),
    time_diff = c(25),
    mass = c(2000)
  )
  expect_equal(monte_carlo(df, nrow(df)), 0)
})

test_that("kin_energy over 600 with previous 2*1000 kg withing 24h", {
  df <- data.frame(
    kin_energy = c(0, 0, 600),
    time_diff = c(0, 12, 12),
    mass = c(1000, 1000, 0)
  )
  expect_equal(monte_carlo(df, nrow(df)), 1)
})

test_that("kin_energy over 600 with previous 2*900 kg withing 24h", {
  df <- data.frame(
    kin_energy = c(0, 0, 600),
    time_diff = c(0, 12, 12),
    mass = c(900, 900, 0)
  )
  expect_equal(monte_carlo(df, nrow(df)), 0)
})

test_that("kin_energy over 600 with previous 2*1000 kg with greater time diff then 24h", {
  df <- data.frame(
    kin_energy = c(0, 0, 600),
    time_diff = c(0, 12, 13),
    mass = c(100, 100, 0)
  )
  expect_equal(monte_carlo(df, nrow(df)), 0)
})

test_that("kin_energy 1200 2 times", {
  df <- data.frame(
    kin_energy = c(1200, 1200),
    time_diff = c(0, 0),
    mass = c(0, 0)
  )
  expect_equal(monte_carlo(df, nrow(df)), 2)
})

test_that("kin_energy 1200 with mass 2000 and kin_energy 600", {
  df <- data.frame(
    kin_energy = c(1200, 600),
    time_diff = c(0, 12),
    mass = c(2000, 0)
  )
  expect_equal(monte_carlo(df, nrow(df)), 2)
})

test_that("kin_energy 1200 with mass 1000 and kin_energy 600", {
  df <- data.frame(
    kin_energy = c(1200, 600),
    time_diff = c(0, 12),
    mass = c(1999, 0)
  )
  expect_equal(monte_carlo(df, nrow(df)), 1)
})


