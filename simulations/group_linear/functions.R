
# Functions to be used globally

source("algorithms/random_search.R")



wlog = function(text,...){
  cat(paste0(date(),"	", text,...,"
"), file="log.txt", append = T)
}

generate_data = function(N, settings){
  for (j in 1:nrow(settings)) {
    n_source = settings[j, "n_source"]
    n_target = settings[j, "n_target"]
    n_vars = settings[j, "n_vars"]
    for (n in 1:N) {
        train = simulate_groups(n_source, n_target, p = n_vars, response = "linear")
        valid = simulate_groups(n_source = 2, n_target = 20, p = n_vars, response = "linear")
        test = simulate_groups(n_source = 2, n_target = 800, p = n_vars, response = "linear")
        
        train = s2Data(train$xL, train$yL, train$xU, preprocess = T)
        valid = s2Data(valid$xU, valid$yU, preprocess = train)
        test = s2Data(test$xU, test$yU, preprocess = valid)
        
        save(train, valid, test, file = paste0("data/",n,"-",n_source,"-",n_target,"-",n_vars,".rdata"))
    }
  }
}

compute = function(n, n_source, n_target, n_vars, method, R){
  
  load(file = paste0("data/",n,"-",n_source,"-",n_target,"-",n_vars,".rdata"))
  
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


