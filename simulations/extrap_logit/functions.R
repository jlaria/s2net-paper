
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
    scenario = as.character(settings[j, "scenario"])
    sigma2 = settings[j, "sigma2"]
    for (n in 1:N) {
        train = simulate_extra(n_source, n_target, p = n_vars, shift = .1, scenario = scenario, response = "logit")
        valid = simulate_extra(n_source = 2, n_target = 20, p = n_vars, shift = .1, scenario = scenario, response = "logit")
        test = simulate_extra(n_source = 2, n_target = 100, p = n_vars, shift = .1, scenario = scenario, response = "logit")
        
        scol = sample(n_vars)
        
        train = s2Data(train$xL[,scol], train$yL, train$xU[,scol], preprocess = T)
        valid = s2Data(valid$xU[,scol], valid$yU, preprocess = train)
        test = s2Data(test$xU[,scol], test$yU, preprocess = valid)
        
        save(train, valid, test, file = paste0("data/",n,"-",n_source,"-",n_target,"-",n_vars,"-",scenario,"-",sigma2,".rdata"))
    }
  }
}

compute = function(n, n_source, n_target, n_vars, scenario, sigma2, method, R){
  
  load(file = paste0("data/",n,"-",n_source,"-",n_target,"-",n_vars,"-",scenario,"-",sigma2,".rdata"))
  
  switch (method,
    s4pm = {
      perf = random_search_s4pm(train, valid, test, R)
    },
    agraph = {
      perf = random_search_agraph(train, valid, test, R)
    },
    ICLS = {
      perf = random_search_ICLS(train, valid, test, R)
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


