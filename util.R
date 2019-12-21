
predictvals <- function(model, xvar, yvar, xrange = NULL, samples = 100, ...) {
  if(is.null(xrange)) {
    if(any(class(model) %in% c("lm", "glm")))
      xrange <- range(model$model[[xvar]])
    else if (any(class(model) %in% "loess"))
      xrange <- model$x
  }
  
  newdata <- data.frame(x = seq(xrange[1], xrange[2], length.out = samples))
  names(newdata) <- xvar
  newdata[[yvar]] <- predict(model, newdata = newdata, ...)
  newdata
}



lm_labels <- function(dat, x, y) {
  mod <- lm(as.formula(sprintf("%s ~ %s",y, x)), data=dat)
  formula <- if (is.na(coef(mod)[1]) || is.na(coef(mod)[2]) ) {
    "NA"  
  } else {
    sprintf("italic(y) == %.2f %+.2f * italic(x)", 
            round(coef(mod)[1],2), round(coef(mod)[2], 2))
  }
  r <- cor(dat[[y]], dat[[x]])
  
  r2 <- if(is.na(r)) "NA" else sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r2 = r2, stringsAsFactors = FALSE)
}
