library(mgcv)

## sampling weights:
# fire postiviely correlated with temperature
# water positively correlated with percipitation
# grass positively correlated with humidity
# flying negatively correlated with wind speed

func_fire <- function(temp){
  probs <- c(0, 0, 0.05, 0.15, 0.4, 0.95, 1, 1, 1, 1, 1)
  preds <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
  
  mod <- gam(probs ~ s(preds))
  
  pred_prob <- round(predict(mod, data.frame(preds = temp)), 2)
  pred_prob <- max(0, pred_prob)
  pred_prob <- min(1, pred_prob)
  
  return(pred_prob)
}

func_water <- function(rainfall){
  probs <- c(0, 0, 0.1, 0.5, 0.9, 1, 1, 1, 1, 1, 1)
  preds <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
  
  mod <- gam(probs ~ s(preds))
  
  pred_prob <- round(predict(mod, data.frame(preds = rainfall)), 2)
  pred_prob <- max(0, pred_prob)
  pred_prob <- min(1, pred_prob)
  
  return(pred_prob)
}

func_grass <- function(humid){
  probs <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
  preds <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
  
  mod <- gam(probs ~ s(preds))
  
  pred_prob <- round(predict(mod, data.frame(preds = humid)), 2)
  pred_prob <- max(0, pred_prob)
  pred_prob <- min(1, pred_prob)
  
  return(pred_prob)
}

func_flying <- function(wind){
  probs <- c(1, 1, 0.95, 0.9, 0.7, 0.3, 0.1, 0.05, 0, 0)
  preds <- c(0, 10, 15, 20, 25, 30, 35, 40, 45, 50)
  
  mod <- gam(probs ~ s(preds))
  
  pred_prob <- round(predict(mod, data.frame(preds = wind)), 2)
  pred_prob <- max(0, pred_prob)
  pred_prob <- min(1, pred_prob)
  
  return(pred_prob)
}