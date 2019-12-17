library(glmnet)
#source("performance.R")

# Wrapper for `glmnet`
# params: alpha, lambda
call_glmnet = function(train, valid, test, params, type = "regression"){
  switch (type,
    regression = {
      model = glmnet(x = train$xL, y = train$yL, family = "gaussian", 
                     alpha = params[1],
                     lambda = params[2])
      
      ypred = predict(model, valid$xL, type = "response")
      perf.valid = performance_measures(valid, ypred, "regression", "valid")
      ypred = predict(model, test$xL, type = "response")
      perf.test = performance_measures(test, ypred, "regression", "test")
    },
    classification = {
      model = glmnet(x = train$xL, y = train$yL, family = "binomial", 
                     alpha = params[1],
                     lambda = params[2])
      
      ypred = predict(model, valid$xL, type = "response")
      perf.valid = performance_measures(valid, ypred, "classification", "valid")
      ypred = predict(model, test$xL, type = "response")
      perf.test = performance_measures(test, ypred, "classification", "test")
    }
  )
  
  c(perf.valid, perf.test)
}

# ## Example
# data("auto_mpg")
# idx = sample(length(auto_mpg$P1$yU), 200)
# train = s2Data(xL = auto_mpg$P1$xL, yL = auto_mpg$P1$yL, xU = auto_mpg$P1$xU[idx, ])
# valid = s2Data(xL = auto_mpg$P1$xU[-idx, ], yL = auto_mpg$P1$yU[-idx], preprocess = train)
# test = s2Data(xL = auto_mpg$P1$xU[idx, ], yL = auto_mpg$P1$yU[idx], preprocess = train)
# 
# params = c(0.5, 0.01, 0.1, 100)
# 
# call_glmnet(train, valid, test, params)
# 
# data = simulate_extra(n = 100, p = 15, response = "logit")
# train = s2Data(data$xL, data$yL, data$xU[1:50,])
# valid = s2Data(data$xU[1:20,], data$yU[1:20], preprocess = train)
# test = s2Data(data$xU, data$yU)
# 
# call_glmnet(train, valid, test, params, "classification")
