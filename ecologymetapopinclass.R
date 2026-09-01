library(deSolve)

#Introduction start:
#we need to use a differential equation solver to simulate dynamics for our coupled equations
#this solver has a particular syntax. first we make a function for our model, then set the parameters and the time interval, then run the solver

#make function for simple Lotka Volterra predator prey dynamics
levins <- function(t, y, params) {
  p <- y[1] #initial pop of predators
  with(as.list(params), {
    dp.dt <- ((c*p)*(1-p))-e*p
    return(list(c(dp.dt)))
  })
}

c=0.15
e=0.05

params <- c(c=c, e=e)
time=seq(0, 100, by =0.1)
output <- ode(c(0.01), time, levins, params)

plot(y=output[,2], x=output[,1], ylab='proportion occupied', xlab='time')

#we see logistic type dynamics because as more patches get occupied, it slows down

#how do we modify model to incorport habitat destroyed - reduces habitat that is available to be colonized 
#cp(1-p-D)-ep

levins2 <- function(t, y, params2) {
  p <- y[1] #initial pop of predators
  with(as.list(params2), {
    dp.dt <- ((c*p)*(1-p-D))-e*p
    return(list(c(dp.dt)))
  })
}

c=0.15
e=0.05

params2 <- c(c=c, e=e, D=D[i])

output<- list()
D<- seq(0, 1, 0.1)

for (i in 1:11) {
  params<-c(c=c, e=e, D=D[i])
  output[[i]] <- ode(c(0.01), levins2, time, params)
  
}

time = seq(0, 100, by =0.1)
output <- ode(c(0.01), time, levins, params)

plot(y=output[[i]][,2], x=output[[i]][,1], ylab='proportion occupied', xlab='time')

#with more habitat destroyed, the occupancy of patches goes down - equilibrium gets reduced. when D is 0.667, there is extinction debt and equilibrium of 0, so pop goes extinct. 