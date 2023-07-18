# Precompile vignettes which are too slow.
# Must compile and install the `mmrm` package before knitting the vignette.

library(knitr)

knit("vignettes/mmrm_review_methods.Rmd.orig", output = "vignettes/mmrm_review_methods.Rmd")
system("mv *png vignettes/")
