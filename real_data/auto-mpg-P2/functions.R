
# Functions to be used globally
source("algorithms/random_search.R")

wlog = function(text,...){
  cat(paste0(date(),"	", text,...,"
"), file="log.txt", append = T)
}


get_data = function(data, nU.train = 100, nU.valid = 20){
  
  ids = 1:nrow(data$xU)
  
  idx.U.train = sample(ids, nU.train)
  ids = setdiff(ids, idx.U.train)
  
  idx.U.valid = sample(ids, nU.valid)
  
  data = list(
    train = list(
      xL = data$xL,
      yL = data$yL,
      xU = data$xU[idx.U.train, ]
    ),
    valid = list(
      xU = data$xU[idx.U.valid, ],
      yU = data$yU[idx.U.valid]
    ),
    test = list(
      xU = data$xU,
      yU = data$yU
    )
  )
  return(data)
}

generate_data = function(R){
  
  data("auto_mpg")
  
  nU.train = 100
  nU.valid = 20
  
  foreach(run = 1:R)%do%{
    data = get_data(auto_mpg$P2, nU.train, nU.valid)
    
    train = s2Data(data$train$xL, data$train$yL, data$train$xU, preprocess = TRUE)
    valid = s2Data(data$valid$xU, data$valid$yU, preprocess = train)
    test = s2Data(data$test$xU, data$test$yU, preprocess = train)
    
    save(train, valid, test, file = paste0("data/", run, ".RData"))
  }
}

compute = function(method, run, R){
  load(file = paste0("data/",run,".RData"))
  
  switch (method,
          s4pm = {
            perf = random_search_s4pm(train, valid, test, R)
          },
          agraph = {
            perf = random_search_agraph(train, valid, test, R)
          },
          JT = {
            perf = random_search_JT(train, valid, test, R)
          },
          s2net = {
            perf = random_search_s2net(train, valid, test, R)
          },
          baseline = {
            perf = random_search_s2net_baseline(train, valid, test, R)
          },
          glmnet = {
            perf = random_search_glmnet(train, valid, test, R)
          }
  )
  return(perf)
}