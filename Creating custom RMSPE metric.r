

rfFuncs$summary
?postResample()

newSumm <- function (data, lev = NULL, model = NULL) 
{
  if (is.character(data$obs)) 
    data$obs <- factor(data$obs, levels = lev)
  mypostResample(data[, "pred"], data[, "obs"])
}

mypostResample <- function (pred, obs) 
{
  isNA <- is.na(pred)
  pred <- pred[!isNA]
  obs <- obs[!isNA]
  
  mspe <- mean(((obs - pred)/obs)^2)
  out <- sqrt(mspe)
  names(out) <- "RMSPE"
  
  if (any(is.nan(out))) 
    out[is.nan(out)] <- NA
  out
}

# keep old settings for future use
oldSumm <- rfFuncs$summary 

# update with new function
rfFuncs$summary <- newSumm

rfFuncs$summary
mypostResample
