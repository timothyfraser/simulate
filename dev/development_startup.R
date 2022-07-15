# Startup
#https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/

##################################
# Getting Started
##################################
#install.packages("devtools")
#install.packages("roxygen2")

library(devtools)
library(roxygen2)

setwd("/cloud/project")
#create("simulate")

# Click Definitely

##################################
# Installing
##################################

# Process Documentation
#setwd("./simulate")
setwd(".")
document()

# Formally install
setwd("..")
install("/cloud/project/")


# Test
library(simulate)
#cat_function(love = FALSE)
#get_dist()

#####################################
# Revise & Re-install
#####################################

# Remove, Rinse, and Repeat
detach("package:simulate", unload = TRUE)
uninstall("simulate")
.rs.restartR()
library(devtools)
library(roxygen2)

# Process Documentation
#setwd("..")
setwd("/cloud/project/")
document()
# Formally install
setwd("..")
install("/cloud/project/")
# Load
library(simulate)

#######################
# Make a vignette
#######################
#usethis::use_vignette("simulate")

#setwd("/cloud/project/")

######################
# Put on Github
######################


################################
# Test from Github
################################

library(devtools)
install_github("timothyfraser/simulate")



