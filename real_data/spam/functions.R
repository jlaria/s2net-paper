
# Functions to be used globally
source("algorithms/random_search.R")

wlog = function(text,...){
  cat(paste0(date(),"	", text,...,"
"), file="log.txt", append = T)
}


get_data = function(data, nL.train = 100, nU.train = 100, nU.valid = 20){
  
  idx.L.train = sample(nrow(data$xL), nL.train)
  
  ids = 1:nrow(data$xU)
  idx.U.valid = sample(ids, nU.valid)
  ids = setdiff(ids, idx.U.valid)
  idx.U.train = sample(ids, nU.train)
  
  
  data = list(
    train = list(
      xL = data$xL[idx.L.train, ],
      yL = data$yL[idx.L.train],
      xU = data$xU[idx.U.train, ]
    ),
    valid = list(
      xU = data$xU[idx.U.valid, ],
      yU = data$yU[idx.U.valid]
    ),
    test = list(
      xU = data$xU[-idx.U.valid, ],
      yU = data$yU[-idx.U.valid]
    )
  )
  return(data)
}

generate_data = function(R){
  
  load("data/spam.RData")
  
  nL.train = 100
  nU.train = 500
  nU.valid = 20
  
  foreach(run = 1:R)%do%{
    data = get_data(spam, nL.train, nU.train, nU.valid)
    
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
          },
          ICLS = {
            perf = random_search_ICLS(train, valid, test, R)
          }
  )
  return(perf)
}