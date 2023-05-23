library(dplyr)
library(usethis)

# generate the BCVA dataset
set.seed(510)
bcva_data <- rct_dgp_fun()
use_data(bcva_data, overwrite = TRUE)
