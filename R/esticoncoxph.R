esticon.coxph <-
  function (obj, cm, beta0, conf.int = TRUE, level = 0.95, joint.test = FALSE) 
{
  if (joint.test == TRUE) {
    .wald(obj, cm, beta0)
  }
  else {
    cf <- summary(obj)$coefficients
    vcv <- obj$var
    stat.name <- "X2.stat"
    df <- 1
    .esticonCore(obj, cm, beta0, conf.int = conf.int, level = level, 
                 cf, vcv, df, stat.name)
  }
}
