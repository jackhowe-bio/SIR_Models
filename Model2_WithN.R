
# set up the packages
my_packages <- c("shinySIR")                                        # Specify your packages
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]    # Extract not installed packages
if(length(not_installed)) install.packages(not_installed)   # Install not installed packages
library(shinySIR)

mySIRS <- function(t,y,params) {
  with(as.list(c(y,params)),{
    
    #change in S
    dX = a*(X + Y + Z) - b*X - beta*X*Y + gamma*Z
    
    #change in infected
    dY = beta*X*Y - (alpha + b + v)*Y
    
    #change in removed
    dZ = v*Y - (b + gamma)*Z
    
    
    dN = (a-b)*(X+Y+Z) - alpha*Y 
    
    dD = alpha*Y
    
    return(list(c(dX, dY, dZ, dN)))
  })
}

run_shiny(model = "SIR Model (Anderson & May)", 
          neweqns = mySIRS,
          ics = c(X = 999, Y = 1, Z = 0, N = 1000),
          parm0 = c(a = 0.014 , b = 0.01, beta = 0.0001, alpha = 0.029, gamma = 0.015, v = 0.044),
          parm_names = c("birth rate", "background death rate", "rate of infection","diseased induced mortality","loss of immunity", " recovery rate"),
          parm_min = c(a = 0 , b = 0, beta = 0, alpha = 0, gamma = 0, v = 0),
          parm_max = c(a = 0.1 , b = 0.1, beta = 0.01, alpha = 0.1, gamma = 0.1, v = 0.1),
          tmax = 1000, 
          values = c('cadetblue4','darkred','darkorchid1', 'grey'))
