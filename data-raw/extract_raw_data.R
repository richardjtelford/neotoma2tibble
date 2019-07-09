#extract some test data
#packages
library(neotoma)

Hordaland  <- get_download(c(20042, 20050))

devtools::use_data(Hordaland)
