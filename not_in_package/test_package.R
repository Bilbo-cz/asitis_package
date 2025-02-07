#install.packages("devtools")
#install.packages("roxygen2")

remotes::install_github("Bilbo-cz/test_package")

library(testPackage)
my_name(T)
