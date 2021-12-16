library(testthat)

source("~/Main/HW8_main")

#Problem 1
test_that('same distribution, full info',{
  set.seed(1234567890)
  x = inverse_sampling(100,pnorm,qnorm,mean=100,sd=25)
  ks_test_out = ks.test(x,pnorm,mean=100,sd=25)
  expect_gt(ks_test_out$p.value,0.01)
})

test_that('same distribution, half info',{
  set.seed(1234567890)
  x = inverse_sampling(100,pnorm,mean=100,sd=25)
  ks_test_out = ks.test(x,pnorm,mean=100,sd=25)
  expect_gt(ks_test_out$p.value,0.01)
})

test_that('same distribution', {
  set.seed(1234567890)
  x <- rejection_sampling(100,dnorm,function(x) dt(x,1), function(n) rt(n,1),1.52)
  ks_test_out = ks.test(x,pnorm,mean=100,sd=25)
  expect_gt(ks_test_out$p.value,0.01)
})

test_that('same distribution', {
  set.seed(1234567890)
  x <- rejection_sampling(100,dnorm,function(x) 1/2*exp(-abs(x)),function(n) rexp(n,1)*(2*rbinom(n,1,0.5)-1),1.32)
  ks_test_out = ks.test(x,pnorm,mean=100,sd=25)
  expect_gt(ks_test_out$p.value,0.01)
})

test_that('same distribution', {
  set.seed(1234567890)
  x <- rejection_sampling(100,dnorm,function(x) dt(x,1), function(n) rt(n,1))
  ks_test_out = ks.test(x,pnorm,mean=100,sd=25)
  expect_gt(ks_test_out$p.value,0.01)
})

test_that('same distribution', {
  set.seed(1234567890)
  x <- rejection_sampling(100,dnorm,function(x) 1/2*exp(-abs(x)),function(n) rexp(n,1)*(2*rbinom(n,1,0.5)-1))
  ks_test_out = ks.test(x,pnorm,mean=100,sd=25)
  expect_gt(ks_test_out$p.value,0.01)
})