# Startup
#https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/

##################################
# Getting Started
##################################
install.packages("devtools")
install.packages("roxygen2")

library(devtools)
library(roxygen2)

setwd("/cloud/project")
create("simulate")
# Click Definitely

##################################
# Installing
##################################

# Process Documentation
setwd("./simulate")
document()

# Formally install
setwd("..")
install("simulate")


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
setwd("./simulate")
document()
# Formally install
setwd("..")
install("simulate")
# Load
library(simulate)

################################
# Test from Github
################################

library(devtools)
install_github("timothyfraser/simulate")

#####################################
# Test
#####################################
library(tidyverse)
library(simulate)

m <- mtcars %>% lm(formula = mpg ~ disp + cyl)

#?tabulate()
# Generate simulated coefficient table
e <- m %>%
  equations() %>%
  get_coeftable()

e <- m %>%
  equations()

#  get_table()
e %>% tabulate()


m %>%
  get_newdata(setx = list(disp = c(100, 200, 300), cyl = c(6)))

mysim <- m %>%
  equations() %>%
  calculate(setx = list(disp = c(100, 200))) %>%
  simulate() 

mysim %>% 
  group_by(case) %>% 
  tabulate(qi = "ev")

mysim %>% 
  pivot_wider(id_cols = c(replicate), 
              names_from = case, 
              values_from = ev, 
              names_prefix = "ev") %>%
  # Calculate first differences
  mutate(fd = ev2 - ev1) %>%
  tabulate(qi = "fd", mu = 0, two.tailed = TRUE)


mysim %>% bands(qi = "ev")
mysim %>% group_by(case) %>% bands(qi = "ev")



