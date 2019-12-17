
#----------------------------------------
# Log file                              |
#---------------------------------------/
writeLines(c(""), "log.txt")
wlog("Starting simulations")
wlog("Cluster intialized successfully!")
#---------------------------------------|


#---------------------------------------
# Global libraries                      |
#---------------------------------------/
packages = c("glmnet", "SemiSupervised", "s2net", "dplyr")
sapply(packages, function(l){library(l, character.only = TRUE)})

#---------------------------------------|


#---------------------------------------
# Parameters                            |
#---------------------------------------/
set.seed(0)
R = 1000 # runs (random search)
methods = c( "baseline", "glmnet", "s2net", "s4pm", "agraph", "JT")
N = 100
settings = expand.grid(
  n_source = c(50),
  n_target = c(50),
  n_vars = c(100),
  scenario = c("unlucky"),
  sigma2 = c(2.5)
)
    
  
#---------------------------------------|


#---------------------------------------
# Generate data                         |
#---------------------------------------/
wlog("Generating data...")
generate_data(N, settings) #This
#---------------------------------------|


#---------------------------------------
# Parallel loop                         |
#---------------------------------------/
wlog("Entering foreach loop")

total.threads = N*nrow(settings)*length(methods)

results = foreach(n = 1:N, .packages = packages, .combine = "rbind")%:%
  foreach(set = 1:nrow(settings),.packages = packages, .combine = "rbind")%:%
  foreach(method = methods, .packages=packages, .combine = "rbind")%dopar%{
    
    out = compute(n, 
                  settings[set, "n_source"],
                  settings[set, "n_target"],
                  settings[set, "n_vars"],
                  as.character(settings[set, "scenario"]),
                  settings[set, "sigma2"],
                  method,
                  R)
    
    this.thread = round((n*set*length(methods)-1)/total.threads*100, 0)
    
    wlog("[", this.thread, " %] method: ", method, " n: ", n, " settings: ", paste0(settings[set,], collapse = " "), " loss: ", out["test.loss"])

    data.frame(N = n,
               method = method,
               n_source = settings[set, "n_source"],
               n_target = settings[set, "n_target"],
               n_vars = settings[set, "n_vars"],
               scenario = settings[set, "scenario"],
               sigma2 = settings[set, "sigma2"],
               out)
  }

#---------------------------------------|
wlog("Simulations finished!")
#---------------------------------------|