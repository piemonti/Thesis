library(sharp)
oldpar <- par(no.readonly = TRUE)
par(mar = c(5, 5, 5, 5))

## Regression models
# Data simulation
set.seed(1)
simul <- SimulateRegression(n = 100, pk = 50)

# Stability selection
stab <- VariableSelection(xdata = simul$xdata, ydata = simul$ydata)
CalibrationPlot(stab)
summary(stab)
SelectedVariables(stab)
plot(stab)

## Graphical models
# Data simulation
set.seed(1)
simul <- SimulateGraphical(n = 100, pk = 20, topology = "scale-free")

# Stability selection
stab <- GraphicalModel(xdata = simul$data)
CalibrationPlot(stab)
summary(stab)
plot(stab)


## PCA models
if (requireNamespace("elasticnet", quietly = TRUE)) {
  # Data simulation
  set.seed(1)
  simul <- SimulateComponents(pk = c(5, 3, 4))
  plot(simul)
  
  # Stability selection
  stab <- BiSelection(
    xdata = simul$data,
    ncomp = 3,
    implementation = SparsePCA
  )
  CalibrationPlot(stab)
  summary(stab)
  SelectedVariables(stab)
}


## PLS models
if (requireNamespace("sgPLS", quietly = TRUE)) {
  # Data simulation
  set.seed(1)
  simul <- SimulateRegression(n = 50, pk = c(10, 20, 30), family = "gaussian")
  
  # Stability selection
  stab <- BiSelection(
    xdata = simul$xdata, ydata = simul$ydata,
    family = "gaussian", ncomp = 3,
    implementation = SparsePLS
  )
  CalibrationPlot(stab)
  summary(stab)
  plot(stab)
}

par(oldpar)