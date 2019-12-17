library(s2net)
#source("performance.R")

# Wrapper for s2net
# params: lambda1, lambda2, gamma1, gamma2, gamma3
call_s2net = function(train, valid, test, params, type = "regression"){
  switch (type,
    regression = {
      model = new(s2net, train, 0)
      model$fit(s2Params(lambda1 = params[1],
                           lambda2 = params[2],
                           gamma1 = params[3],
                           gamma2 = params[4],
                           gamma3 = params[5]),
                frame = 1,
                proj = 2)
      ypred = model$predict(valid$xL, 0)
      perf.valid = performance_measures(valid, ypred, "regression", "valid")
      ypred = model$predict(test$xL, 0)
      perf.test = performance_measures(test, ypred, "regression", "test")
    },
    classification = {
      model = new(s2net, train, 1)
      model$fit(s2Params(lambda1 = params[1],
                           lambda2 = params[2],
                           gamma1 = params[3],
                           gamma2 = params[4],
                           gamma3 = params[5]),
                frame = 1,
                proj = 2)
      
      ypred = model$predict(valid$xL, 0)
      perf.valid = performance_measures(valid, ypred, "classification", "valid")
      ypred = model$predict(test$xL, 0)
      perf.test = performance_measures(test, ypred, "classification", "test")
    }
  )
  return(c(perf.valid, perf.test))
}

# data = simulate_extra(n = 100, p = 15, response = "logit")
# train = s2Data(data$xL, data$yL, data$xU[1:50,])
# valid = s2Data(data$xU[1:20,], data$yU[1:20], preprocess = train)
# test = s2Data(data$xU, data$yU)
# 
# params = c(0.01, 0.01, 0.01, 100, 0.01)
# 
# call_s2net(train, valid, test, params, "classification")
