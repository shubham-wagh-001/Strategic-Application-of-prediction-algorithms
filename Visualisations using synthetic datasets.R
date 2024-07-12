#Visualizations on Synthetic Dataset

install.packages("ggplot2")
library(ggplot2)

# Function to generate data
generate_data = function(n, relation, params) {
  x = runif(n, min=params$x_min, max=params$x_max)
  epsilon = rnorm(n, mean=0, sd=1)
  
  if (relation == "linear") {
    y = params$a * x + params$b + epsilon
  } else if (relation == "vshape") {
    y = params$a * abs(x - params$x0) + params$b + epsilon
  } else if (relation == "xshape") {
    y = ifelse(runif(n) <= 0.5, params$a1 * x + params$b1 + epsilon, 
                params$a2 * x + params$b2 + epsilon)
  } else if (relation == "piecewise") {
    y = ifelse(x <= params$x0, params$a1 * x + params$b1 + epsilon, 
                params$a2 * x + params$b2 + epsilon)
  } else {
    stop("Invalid relation type")
  }
  
  return(data.frame(x=x, y=y))
}

# Setting arbitrary parameters for each relationship type
linear_params = list(x_min=0, x_max=10, a=2, b=3)
vshape_params = list(x_min=0, x_max=10, a=1, b=5, x0=4.5)
xshape_params = list(x_min=-10, x_max=10, a1=1, b1=3, a2=4, b2=0)
piecewise_params = list(x_min=0, x_max=10, a1=2, b1=1, a2=1, b2=4, x0=5)

# Generating data for each relationship
linear_data = generate_data(100, "linear", linear_params)
vshape_data = generate_data(100, "vshape", vshape_params)
xshape_data = generate_data(100, "xshape", xshape_params)
piecewise_data = generate_data(100, "piecewise", piecewise_params)

# Creating visualizations for each dataset with least squares regression lines

ggplot(linear_data, aes(x, y)) +
  geom_point(color="blue") +
  geom_smooth(method="lm", se=FALSE, color="black") +
  labs(title="Linear Relationship", x="x", y="y") 

ggplot(vshape_data, aes(x, y)) +
  geom_point(color="darkgreen") +
  geom_smooth(method="lm", se=FALSE, color="black") +
  labs(title="V-Shape Relationship", x="x", y="y") 

ggplot(xshape_data, aes(x, y)) +
  geom_point(color="red") +
  geom_smooth(method="lm", se=FALSE, color="black") +
  labs(title="X-Shape Relationship", x="x", y="y") 

ggplot(piecewise_data, aes(x, y)) +
  geom_point(color="purple") +
  geom_smooth(method="lm", se=FALSE, color="black") +
  labs(title="Piecewise Relationship", x="x", y="y") 











