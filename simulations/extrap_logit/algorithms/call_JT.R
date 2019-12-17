source("algorithms/orig/jt_code.R")
#source("performance.R")

# Fits the Joint trained linear framework from Ryan and Culp (2015)
# params: alp, lam, gam, tau
call_JT = function(train, valid, test, params){
  JT = jointrls(y = c(train$yL, rep(NA, nrow(train$xU))), 
                x = rbind(train$xL, train$xU), 
                alp = params[1], 
                lam = params[2], 
                gam = params[3], 
                tau = params[4])
  
  beta = JT$bsemi[-1]
  intercept = JT$bsemi[1]
  
  # Compute predictions (validation data)
  ypred = intercept + valid$xL %*% beta
  perf.valid = performance_measures(valid, ypred, "regression", "valid")
  
  # Compute predictions (test data)
  ypred = intercept + test$xL %*% beta
  perf.test = performance_measures(test, ypred, "regression", "test")
  
  return(c(perf.valid, perf.test))
}


## Example
# data("auto_mpg")
# idx = sample(length(auto_mpg$P1$yU), 200)
# train = s2Data(xL = auto_mpg$P1$xL, yL = auto_mpg$P1$yL, xU = auto_mpg$P1$xU[idx, ])
# valid = s2Data(xL = auto_mpg$P1$xU[-idx, ], yL = auto_mpg$P1$yU[-idx], preprocess = train)
# test = s2Data(xL = auto_mpg$P1$xU[idx, ], yL = auto_mpg$P1$yU[idx], preprocess = train)
# 
# params = c(0.5, 0.01, 0.1, 100)
# 
# call_JT(train, valid, test, params)

