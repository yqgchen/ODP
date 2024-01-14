library(frechet)
library(fdadensity)
load("~/Documents/data/mortality data/dxCounts.RData")

## extracting the quantile functions for 2000 for males
yr <- 2000
gender <- "male"
dx <- paste0("mort.dx.",gender) %>% get
dx <- sapply(seq_along(year), function(i) {
  dx[[i]][,which(year[[i]] == yr)]
})
ageIdx <- 1:100
dSup <- seq(min(ageIdx)-1,max(ageIdx),0.25)
qSup <- qbeta(seq(0,1,length.out = 1001),1/2,1/2)
qt <- apply(dx[ageIdx,], 2, function(x) {
  dens <- CreateDensity(
    freq = x, bin = c(min(ageIdx)-1,ageIdx),
    optns = list(outputGrid = dSup)
  )
  dens2quantile(
    dens = dens$y, dSup = dSup,
    qSup = qSup, useSplines = FALSE
  )
}) %>% t
mort <- list(qt = qt, qSup = qSup)
rownames(mort$qt) <- country

save( mort, file = file.path( dirname( rstudioapi::getSourceEditorContext()$path ), 'mort.Rda') )
