box95 <- function(x,dat=NULL,col='grey',lty=1,names=NULL,...)
{
  bx1 <- boxplot(x,plot=F, ...)
  bx1$stats[c(1,5),] <- as.matrix(as.data.frame(lapply(x,quantile,probs=c(.025,.975),na.rm=T)))
  ##bx1$stats[c(1,5),] <- lapply(x,quantile,probs=c(.025,.975))
  ## bx1$stats[c(1,5)] <- quantile(x, probs = c(.025,.975))
  bx1$out = NULL
  bx1$group = NULL
  if(!is.null(names)) {
    bx1$names <- names
  }
  pars <- list(boxfill=col,boxlty=1,...)
  if (any(c("black", "#000000") %in% col)) {
    if (!"border" %in% names(pars)) par("fg" = "gray") 
  }
  bxp(bx1,pars=pars,lty=1,border = "gray", ...)
  out <- bx1$stats[c(1,3,5),]
  rownames(out) <- c("lower95","median","upper95")
  return(out)
}
