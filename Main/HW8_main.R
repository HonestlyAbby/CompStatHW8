source("~/Header/HW8_header.R")


#Problem 1

inverse_sampling <- function(n, cdf, cdf_inv, ...){
  
  if((missing(cdf) && missing(cdf_inv) == TRUE)){
    stop("argument must include either cdf or cdf_inv")
  } 
  if(missing(cdf_inv) == TRUE){
    cdf_inv <- def_cdf_inv(cdf, ...)
  }
  
  
  if(is.integer(n) == FALSE){
    if(is.numeric(n) == TRUE){
      as.integer(n)
      warning("type of n changed to integer")
    }else{
      stop("n must be a positive integer")
    }
    
    if(n < 0){
      n <- n*-1
      warning("sign of n changed to positive")
    }
    if(n == 0){
      stop("n must be a non-zero positive integer")
    }
  }
  
  V_cdf_inv <- Vectorize(cdf_inv, vectorize.args = names(formals(cdf_inv))[1])
  
  return(V_cdf_inv(runif(n), ...))
}

#Problem 2

rejection_sampling <- function(n, targ_pdf, prop_pdf, prop_ran, env_const){
  
  if(is.integer(n) == FALSE){
    if(is.numeric(n) == TRUE){
      as.integer(n)
      warning("type of n changed to integer")
    }else{
      stop("n must be a positive integer")
    }
    
    if(n < 0){
      n <- n*-1
      warning("sign of n changed to positive")
    }
    if(n == 0){
      stop("n must be a non-zero positive integer")
    }
  }
  
  if(missing(env_const) == TRUE){
    env_const <- def_env_const(targ_pdf, prop_pdf)
    warning("enveloping constant missing, derived numerically NOTE: results may be inaccurate")
  }
  
  accept <- c()
  
  while(TRUE){
    U <- runif(1)
    prop_val <- prop_ran(1)
    
    if(U*env_const*prop_pdf(prop_val) <= targ_pdf(prop_val)){
      accept <- c(accept, prop_val)
    }
    
    if(length(accept >= n)){
      break
    }
  }
  
  if(length(accept) != n){
    errorCondition("acceptance incomplete, lower n or change conditions")
  } 
  
  return(accept)  
}

#Problem 3

h <- function(x,mu,sigma){
  -(x-mu)^2/(2*sigma^2)-0.5*log(2*pi*sigma^2)
}
interval=c(-Inf,Inf)
mu = 3
sigma = 2
x = mu+c(-1,1)*sigma

adaptive_rejection_sampling <- function(n, h, interval, x, ...){
  envelope_env <- new.env()
  initialize_envelope_info(envelope_env, h, interval, x, mu=mu, sigma=sigma)
  
  accept <- c()
  
  while(TRUE){
    x_new = upper_envelope_sample(envelope_env)
    y_new = h(x_new,mu,sigma)
    v <- runif(1)
    
    if(log(v) < log(lower_envelope_evaluate(x_new, envelope_env, log = FALSE)) - (upper_envelope_evaluate(x_new, envelope_env, log = FALSE))){
      accept <- c(accept, x_new)
    } else {
      update_environment_info(envelope_env, h, x_new, y_new)
      if(log(v) < h(x_new, ...) - log(upper_envelope_evaluate(x_new))){
        accept <- c(accept, x_new)
      }
      
      if(accept >= n){
        break
      }
    }
  }
  return(accept)
}