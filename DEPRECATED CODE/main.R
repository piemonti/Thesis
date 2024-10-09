# Create a sample dataset
mydata <- data.frame(
  ID = 1:5,
  Name = c("Alice", "Bob", "Charlie", "David", "Eve"),
  Age = c(25, 30, 22, 35, 28)
)

# Variable name to check
variable_name <- "Name"

# Check if the variable exists in the dataset
if (exists(variable_name, where = environment()) &&
    !is.null(get(variable_name, envir = environment()))) {
  print(paste("Variable", variable_name, "exists in the dataset."))
} else {
  print(paste("Variable", variable_name, "does not exist in the dataset."))
}
