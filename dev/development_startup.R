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
# It works!
install_github("timothyfraser/simulate")

library(simulate)
vignette("simulate")


##############
# Trial Run
#################
data("starwars")

starwars %>%
  lm(formula = height ~ mass + eye_color) %>%
  equate() %>%
  tabulate()


starwars %>%
  lm(formula = height ~ mass + eye_color) %>%
  equate() %>%
  illustrate(qi = "mass")

#######################
# Add Package Vignette
#######################
library(tidyverse)
library(betareg)
m <- mtcars %>% 
  mutate(mpg = mpg / 100, 
         cyl = factor(cyl)) %>%
  betareg(formula = mpg  ~ cyl + drat)

m$coefficients$mean <- equations[i,2:(ncol(equations)-1)] %>% unlist()
m$coefficients$phi <- equations[i, ncol(equations)]
m$coefficients

equations <- m %>% 
  equate() %>% 
  calculate(setx = list(cyl = c("4", "8")))


class(m)
is.null(family(m))

m <- mtcars %>%
  glm(formula = mpg ~ 1, family = Gamma(link = "log"))
m
m %>% equate()




