
# Functions to be used globally
source("algorithms/random_search.R")

wlog = function(text,...){
  cat(paste0(date(),"	", text,...,"
"), file="log.txt", append = T)
}


get_idrc2002_data = function(df, nL.train = 135, nU.train = 200, nU.valid = 20){
  ids = unique(df$id)
  
  idx.L.train = sample(ids, nL.train)
  ids = setdiff(ids, idx.L.train)
  idx.U.train = sample(ids, nU.train)
  ids = setdiff(ids, idx.U.train)
  
  idx.U.valid = sample(ids, nU.valid)
  ids = setdiff(ids, idx.U.valid)
  
  mean.yL.train = mean(df$label[df$id %in% idx.L.train & df$instrument=="1"])
  
  data = list(
    train = list(
      xL = as.matrix(df[df$id %in% idx.L.train & df$instrument=="1", -(1:4)]),
      yL = as.matrix(df$label[df$id %in% idx.L.train & df$instrument=="1"] - mean.yL.train),
      xU = as.matrix(df[df$id %in% idx.U.train & df$instrument=="2", -(1:4)])
    ),
    valid = list(
      xU = as.matrix(df[df$id %in% idx.U.valid & df$instrument=="2", -(1:4)]),
      yU = as.matrix(df$label[df$id %in% idx.U.valid & df$instrument=="2"] - mean.yL.train)
    ),
    test = list(
      xU = as.matrix(df[df$id %in% c(idx.U.train, ids) & df$instrument=="2", -(1:4)]),
      yU = as.matrix(df$label[df$id %in% c(idx.U.train, ids) & df$instrument=="2"] - mean.yL.train)
    )
  )
  return(data)
}

generate_data = function(R){
  load("data/irdc2002.Rdata")
  nL.train = 135
  nU.train = 200
  nU.valid = 20
  
  foreach(run = 1:R)%do%{
    data = get_idrc2002_data(df, nL.train, nU.train, nU.valid)
    
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