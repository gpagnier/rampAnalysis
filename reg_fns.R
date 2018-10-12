error.bar <- function(x, y, upper, lower=upper, length=0, color='black', lwd=1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, lwd=lwd, col=color,...)
}

sig.stars <- function(x,pvals){
  for (jj in 1:length(pvals)) {
    if (pvals[jj] < .1 && pvals[jj] >= 0.05) {
      starvec <- '.'
    } else if (pvals[jj] < .05 && pvals[jj] >= 0.01) {
      starvec <- '*'
    } else if (pvals[jj] < 0.01 && pvals[jj] >= 0.001) {
      starvec <- '**'
    } else if (pvals[jj] < 0.001) {
      starvec <- '***'
    } else {
      starvec <- ''
    }
    mtext(starvec,side=3,at=x[jj],cex=1.2)
  }
}

coef_plt <- function(mlr_model) {
  coeffs <- coefficients(mlr_model)
  mp <- barplot(coeffs, col="#3F97D0", xaxt='n', main="Regression Coefficients")
  lablist <- names(coeffs)
  text(mp, par("usr")[3], labels = lablist, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
}

fixefPlt <- function(mlm,lbls,ylb) {
  coeffs <- as.vector(coefficients(mlm)[,"Estimate"])
  stdErrs <- as.vector(coefficients(mlm)[,"Std. Error"])
  pvals <- as.vector(coefficients(mlm)[,grep("Pr",colnames(coefficients(mlm)))])
  mp <- barplot(coeffs, col="#3F97D0", xaxt='n',ylab=ylb,ylim=c(1.1*min(coeffs),1.1*max(coeffs)),border=NA)
  lablist <- row.names(coefficients(mlm))
  text(mp, par("usr")[3], labels = lbls, srt = 30, adj = c(1.1,1.1), xpd = TRUE, cex=1.0)
  error.bar(mp,coeffs,stdErrs,lwd=2)
  sig.stars(mp,pvals)
}