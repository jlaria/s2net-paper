
#---------------------------------------
# Log file                              |
#---------------------------------------/
writeLines(c(""), "log.txt")
wlog("Starting simulations")
wlog("Cluster intialized successfully!")
#---------------------------------------|


#---------------------------------------
# Global libraries                      |
#---------------------------------------/
packages = c("glmnet", "s2net", "SemiSupervised", "RSSL")
sapply(packages, function(l){library(l, character.only = TRUE)})
#---------------------------------------|


#---------------------------------------
# Parameters                            |
#---------------------------------------/
set.seed(0)
R = 10 # random search
methods = c("baseline", "glmnet", "s2net", "s4pm", "agraph", "ICLS")
#methods = c("glmnet", "s2net")

N = 1 # runs
#---------------------------------------|


#---------------------------------------
# Generate data                         |
#---------------------------------------/
wlog("Generating data...")
generate_data(N) #This
#---------------------------------------|


#---------------------------------------
# Parallel loop                         |
#---------------------------------------/
wlog("Entering foreach loop")

total.threads = N*length(methods)

results = foreach(n = 1:N, .packages = packages, .combine = "rbind")%:%
  foreach(method = methods, .packages=packages, .combine = "rbind")%dopar%{
    
    out = compute(method,
                  n,
                  R)
    
    this.thread = round((n*length(methods)-1)/total.threads*100, 0)
    
    wlog("[", this.thread, " %] method: ", method, " n: ", n, " loss: ", out["test.loss"])
    
    data.frame(N = n,
               method = method,
               out)
  }

#---------------------------------------|
wlog("Simulations finished!")
#---------------------------------------|
