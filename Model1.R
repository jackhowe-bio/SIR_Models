## This script runs the model that we build in Part 1 and 2 of the exercises.
# It runs a model with a constant population size

# It is not necessary for understanding the model, but is useful for visualisation 


# set up the packages
my_packages <- c("shinySIR")                                        # Specify your packages
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]    # Extract not installed packages
if(length(not_installed)) install.packages(not_installed)   # Install not installed packages
library(shinySIR)

mySIRS <- function(t,y,params) {
  with(as.list(c(y,params)),{
    
    #change in S
    dS = -r*I*S
    
    #change in infected
    dI = r*I*S - a*I
    
    #change in removed
    dR = I*a
    

  return(list(c(dS, dI,dR)))
  })
}

run_shiny(model = "SIR Model (w/out demography)", 
          neweqns = mySIRS,
          ics = c(S = 999, I = 1, R = 0),
          parm0 = c(r = 0.0001, a = 0.3),
          parm_names = c("Transmission rate", "Removal Rate rate"),
          parm_min = c(r = 0, a = 0),
          parm_max = c(r = 0.005, a = 0.5),
          tmax = 200, 
          values = c('cadetblue4','darkred','darkorchid1'))

