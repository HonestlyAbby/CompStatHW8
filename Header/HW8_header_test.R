library(testthat)

source("~/Header/HW8_header.R")

#Problem 1

test_that('value correct',{
  expect_equal(def_cdf_inv(pnorm, mean = 100, sd = 25)(p=.1, mean=100, sd=25), qnorm(mean = 100, sd = 25, p = .1))
  expect_equal(def_cdf_inv(pnorm, mean = 100, sd = 25)(p=.2, mean=100, sd=25), qnorm(mean = 100, sd = 25, p = .2))
  expect_equal(def_cdf_inv(pnorm, mean = 100, sd = 25)(p=.3, mean=100, sd=25), qnorm(mean = 100, sd = 25, p = .3))
  expect_equal(def_cdf_inv(pnorm, mean = 100, sd = 25)(p=.4, mean=100, sd=25), qnorm(mean = 100, sd = 25, p = .4))
  expect_equal(def_cdf_inv(pnorm, mean = 100, sd = 25)(p=.5, mean=100, sd=25), qnorm(mean = 100, sd = 25, p = .5))
  expect_equal(def_cdf_inv(pnorm, mean = 100, sd = 25)(p=.6, mean=100, sd=25), qnorm(mean = 100, sd = 25, p = .6))
  expect_equal(def_cdf_inv(pnorm, mean = 100, sd = 25)(p=.7, mean=100, sd=25), qnorm(mean = 100, sd = 25, p = .7))
  expect_equal(def_cdf_inv(pnorm, mean = 100, sd = 25)(p=.8, mean=100, sd=25), qnorm(mean = 100, sd = 25, p = .8))
  expect_equal(def_cdf_inv(pnorm, mean = 100, sd = 25)(p=.9, mean=100, sd=25), qnorm(mean = 100, sd = 25, p = .9))
})

#Problem 2

test_that('less than target', {
  expect_lt(def_env_const(dnorm, function(x) 1/pi/(1+x^2)), 1.521)
  expect_lt(def_env_const(dexp, function(x) 1/(1+x^2)), 1.472)
})