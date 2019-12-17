
# Computes performance measures of a model in some data
# test: s2Data object
# ypred: predictions associated with test$xL

performance_measures = function(test, ypred, type = "regression", fold = "."){
  # predictions: ypred (if type==classification: probabilities)
  switch (type,
          regression = {
            perf = data.frame(loss =  mse(ypred, test$yL),
                     mae = mae(ypred, test$yL))
          },
          classification = {
            accuracy = acc(ypred, test$yL)
            perf = data.frame(loss = LogLoss(ypred, test$yL),
                     acc = accuracy,
                     auc = auc(ypred, test$yL)
                     )
          }
  )
  
  colnames(perf) = paste0(fold,".", colnames(perf))
  return(perf)
}

mse = function(ypred, ytrue){
  return(mean((ypred - ytrue)^2))
}

mae = function(ypred, ytrue){
  return(mean(abs(ypred - ytrue)))
}

acc = function(ypred, ytrue){
  return(mean(((ypred > 0.5)+0) == ytrue))
}

auc = function(ppred, ytrue){
  if(length(unique(ppred))==1)return(0)
  pred = ROCR::prediction(ppred, ytrue)
  unlist(ROCR::performance(pred, "auc")@y.values)
}

LogLoss = function (y_pred, y_true) 
{
  eps <- 1e-15
  y_pred <- pmax(pmin(y_pred, 1 - eps), eps)
  LogLoss <- -mean(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))
  return(LogLoss)
}