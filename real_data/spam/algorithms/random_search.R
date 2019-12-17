# Random Search..
source("algorithms/performance.R")

loss = function(ypred, ytrue){
  LogLoss(ypred, ytrue)
}

random_search_s2net = function(train, valid, test, points = 1000){
  obj = new(s2net, train, 1)
  
  best_loss = Inf
  performance = NULL
  best_params = NULL
  
  for (iter in 1:points) {
    params = s2Params(lambda1 = 2^runif(1, -8, 1),
                        lambda2 = 2^runif(1, -8, 1),
                        gamma1 = 2^runif(1, -8, 1),
                        gamma2 = 2^runif(1, -1, 10),
                        gamma3 = 2^runif(1, -8, 1))
    params[sample(c(T,F), 5, replace = T, prob = c(0.05, 0.95))] = 0
    
    obj$fit(params, 1, 2)
    ypred = obj$predict(valid$xL, 2)
    L = loss(ypred, valid$yL)
    if(L <= best_loss){
      best_loss = L
      best_params = params
      
      perf.valid = performance_measures(valid, ypred, "classification", "valid")
      
      ypred = obj$predict(test$xL, 2)
      perf.test = performance_measures(test, ypred, "classification", "test")
      performance = cbind(perf.valid, perf.test)
    }
  }
  return(performance)
}

random_search_s2net_baseline = function(train, valid, test, points = 1000){
  obj = new(s2net, train, 1)
  
  best_loss = Inf
  performance = NULL
  best_params = NULL
  
  for (iter in 1:points) {
    params = s2Params(lambda1 = 2^runif(1, -8, 1),
                        lambda2 = 2^runif(1, -8, 1),
                        gamma1 = 0,
                        gamma2 = 1,
                        gamma3 = 0)
    params[sample(c(T,F), 5, replace = T, prob = c(0.05, 0.95))] = 0
    
    obj$fit(params, 1, 2)
    ypred = obj$predict(valid$xL, 2)
    L = loss(ypred, valid$yL)
    if(L <= best_loss){
      best_loss = L
      best_params = params
      
      perf.valid = performance_measures(valid, ypred, "classification", "valid")
      
      ypred = obj$predict(test$xL, 2)
      perf.test = performance_measures(test, ypred, "classification", "test")
      performance = cbind(perf.valid, perf.test)
    }
  }
  return(performance)
}


random_search_glmnet = function(train, valid, test, points = 1000){
  best_loss = Inf
  performance = NULL
  best_params = NULL
  
  for (iter in 1:points) {
    params = c(
      runif(1, 0, 1),
      2^runif(1, -8, 1)
    )
    params[sample(c(T,F), 2, replace = T, prob = c(0.05, 0.95))] = 0
    
    obj = glmnet(train$xL, train$yL, "binomial", alpha = params[1], lambda = params[2])
    
    ypred = predict(obj, valid$xL, type = "response")
    L = loss(ypred, valid$yL)
    if(L <= best_loss){
      best_loss = L
      best_params = params
      
      perf.valid = performance_measures(valid, ypred, "classification", "valid")
      
      ypred = predict(obj, test$xL, type = "response")
      perf.test = performance_measures(test, ypred, "classification", "test")
      performance = cbind(perf.valid, perf.test)
    }
  }
  return(performance)
}

# Wrapper for the s4pm from Culp and Ryan (2018), R package `SemiSupervised`
# params: hs, lams[1:2], gams
random_search_s4pm = function(train, valid, test, points = 1000){
  # convert to data.frame
  data = data.frame(y = c(train$yL, rep(NA, nrow(train$xU))), x = rbind(train$xL, train$xU))
  
  best_loss = Inf
  performance = NULL
  best_params = NULL
  
  for (iter in 1:points) {
    set.seed(Sys.time()) #little hack 'cause s4pm calls(?) set.seed internally
    params = c(
      2^runif(1, -8, 1),
      2^runif(1, -8, 1),
      2^runif(1, -8, 1),
      2^runif(1, -8, 1)
    )
    params[sample(c(T,F), 4, replace = T, prob = c(0.05, 0.95))] = 0
    # model = s4pm(y~., data, type = "c")
    # 
    model = s4pm(y~., data, type = "c",
                 hs = params[1],
                 lams = params[2:3],
                 gams = params[4])

    ypred = predict(model, data.frame(x = valid$xL), type = "prob")
    L = loss(ypred, valid$yL)
    if(is.na(L)){L = Inf}
    if(L <= best_loss){
      best_loss = L
      best_params = params
      
      perf.valid = performance_measures(valid, ypred, "classification", "valid")
      
      ypred = predict(model, data.frame(x = test$xL), type = "prob")
      perf.test = performance_measures(test, ypred, "classification", "test")
      performance = cbind(perf.valid, perf.test)
    }
  }
  return(performance)
}

# Wrapper for the `agraph` from Culp and Ryan (2018), R package `SemiSupervised`
# params: lams[1:2], gams
random_search_agraph = function(train, valid, test, points = 1000){
  # convert to data.frame
  data = data.frame(y = c(train$yL, rep(NA, nrow(train$xU))), x = rbind(train$xL, train$xU))
  
  best_loss = Inf
  performance = NULL
  best_params = NULL
  
  for (iter in 1:points) {
    set.seed(Sys.time()) #little hack 'cause s4pm calls(?) set.seed internally
    params = c(
      2^runif(1, -8, 1),
      2^runif(1, -8, 1),
      2^runif(1, -8, 1)
    )
    params[sample(c(T,F), 3, replace = T, prob = c(0.05, 0.95))] = 0
    
    model = agraph(y~., data, type = "c", 
                   lams = params[1:2],
                   gams = params[3])
    
    ypred = predict(model, data.frame(x = valid$xL), type="prob")
    L = loss(ypred, valid$yL)
    if(is.na(L)){L = Inf}
    if(L <= best_loss){
      best_loss = L
      best_params = params
      
      perf.valid = performance_measures(valid, ypred, "classification", "valid")
      
      ypred = predict(model, data.frame(x = test$xL), type = "prob")
      perf.test = performance_measures(test, ypred, "classification", "test")
      performance = cbind(perf.valid, perf.test)
    }
  }
  return(performance)
}

random_search_ICLS = function(train, valid, test, points = 1000){
  best_loss = Inf
  performance = NULL
  best_params = NULL
  
  for (iter in 1:points) {
    params = c(
      2^runif(1, -8, 1),
      2^runif(1, -8, 1)
    )
    params[sample(c(T,F), 2, replace = T, prob = c(0.05, 0.95))] = 0
    
    model = ICLeastSquaresClassifier(X = train$xL, y = factor(train$yL), X_u = train$xU, 
                                     lambda1 = params[1], lambda2 = params[2])
    
    ypred = as.numeric(predict(model, valid$xL, probs = F)) - 1 # negative probabilities(?)
    L = loss(ypred, valid$yL)
    if(is.na(L)){L = Inf}
    if(L <= best_loss){
      best_loss = L
      best_params = params
      
      perf.valid = performance_measures(valid, ypred, "classification", "valid")
      
      ypred = as.numeric(predict(model, test$xL))-1
      perf.test = performance_measures(test, ypred, "classification", "test")
      performance = cbind(perf.valid, perf.test)
    }
  }
  return(performance)
}
