# Load necessary libraries

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)


# Load the data
data <- read.csv("sales_data.csv")

# View the first few rows of the data
head(data)

# Scale the numeric columns
numeric_columns <- c("Price", "Number.of.products.sold", "Revenue.generated", 
                     "Stock.levels", "Lead.times", "Order.quantities", 
                     "Shipping.times", "Shipping.costs", "Manufacturing.costs", 
                     "Defect.rates", "Costs", "Holding.costs", "Ordering.costs", 
                     "Stockout.costs")

for (column in numeric_columns) {
  data[[column]] <- scale(data[[column]])
}

# View the summary statistics of the scaled data
summary(data)

# Check for missing values
sapply(data, function(x) sum(is.na(x)))

# Check for duplicates
duplicated_rows <- duplicated(data)
if (any(duplicated_rows)) {
  print("Duplicate rows found:")
  print(data[duplicated_rows, ])
} else {
  print("No duplicate rows found.")
}

# Check for data entry errors (e.g., invalid dates)
# This will depend on the specific data and columns

# Check for consistency in formatting (e.g., dates)
# This will depend on the specific data and columns

# Check for outliers
boxplot(data$Price)
boxplot(data$Number.of.products.sold)
# ... and so on for each numeric column





# Set parameters
lead_time <- 7  # days
order_quantity <- 100
holding_cost <- 0.5  # per unit per day
stockout_cost <- 10  # per unit per day
demand_mean <- 20  # units per day
demand_sd <- 5  # units per day

# Initialize variables
inventory <- 0
orders <- 0
days <- 30
inventory_levels <- numeric(days)
orders_placed <- numeric(days)

# Simulation loop
for (i in 1:days) {
  # Generate demand
  demand <- rnorm(1, mean = demand_mean, sd = demand_sd)
  
  # Update inventory
  inventory <- inventory - demand
  
  # Check if inventory is low
  if (inventory <= 0) {
    # Place order
    orders <- orders + 1
    orders_placed[i] <- 1
    
    # Update inventory (after lead time)
    if (i >= lead_time) {
      inventory <- inventory + order_quantity
    }
  }
  
  # Calculate costs
  holding_cost_today <- holding_cost * max(inventory, 0)
  stockout_cost_today <- stockout_cost * max(-inventory, 0)
  
  # Update inventory levels
  inventory_levels[i] <- inventory
}

# Plot inventory levels
plot(inventory_levels, type = "l", xlab = "Day", ylab = "Inventory Level")

# Print total costs
total_holding_cost <- sum(holding_cost * max(inventory_levels, 0))
total_stockout_cost <- sum(stockout_cost * max(-inventory_levels, 0))
print(paste("Total holding cost:", total_holding_cost))
print(paste("Total stockout cost:", total_stockout_cost))


lead_time_mean <- 7  # days
lead_time_sd <- 2  # days
demand_mean <- 20  # units per day
demand_sd <- 5  # units per day
holding_cost <- 0.5  # per unit per day
stockout_cost <- 10  # per unit per day
reorder_point <- 50  # units
reorder_quantity <- 100  # units
eoq <- sqrt((2 * demand_mean * stockout_cost) / holding_cost)  # Economic Order Quantity

# Initialize variables
inventory <- 0
orders <- 0
days <- 30
inventory_levels <- numeric(days)
orders_placed <- numeric(days)
lead_times <- numeric(days)

# Simulation loop
for (i in 1:days) {
  # Generate demand
  demand <- rnorm(1, mean = demand_mean, sd = demand_sd)
  
  # Generate lead time
  lead_time <- rnorm(1, mean = lead_time_mean, sd = lead_time_sd)
  lead_times[i] <- lead_time
  
  # Update inventory
  inventory <- inventory - demand
  
  # Check inventory management policy
  if (inventory <= reorder_point) {
    # Reorder point policy
    orders <- orders + 1
    orders_placed[i] <- 1
    inventory <- inventory + reorder_quantity
  } else if (inventory <= eoq) {
    # Economic Order Quantity policy
    orders <- orders + 1
    orders_placed[i] <- 1
    inventory <- inventory + eoq
  } else {
    # Just-In-Time policy
    inventory <- demand
  }
  
  # Calculate costs
  holding_cost_today <- holding_cost * max(inventory, 0)
  stockout_cost_today <- stockout_cost * max(-inventory, 0)
  
  # Update inventory levels
  inventory_levels[i] <- inventory
}

# Plot inventory levels
plot(inventory_levels, type = "l", xlab = "Day", ylab = "Inventory Level")

# Print total costs
total_holding_cost <- sum(holding_cost * max(inventory_levels, 0))
total_stockout_cost <- sum(stockout_cost * max(-inventory_levels, 0))
print(paste("Total holding cost:", total_holding_cost))
print(paste("Total stockout cost:", total_stockout_cost))



#experiemnt 1 

# Set parameters
lead_time_mean <- 7
lead_time_sd <- 2
demand_mean <- 20
demand_sd <- 5
holding_cost <- 0.5
stockout_cost <- 10
reorder_points <- c(25, 50, 75, 100)
order_quantity <- 100
safety_stock <- 0
days <- 30

# Run experiment
results <- data.frame()
for (rop in reorder_points) {
  inventory <- 0
  orders <- 0
  inventory_levels <- numeric(days)
  orders_placed <- numeric(days)
  lead_times <- numeric(days)
  for (i in 1:days) {
    demand <- rnorm(1, mean = demand_mean, sd = demand_sd)
    lead_time <- rnorm(1, mean = lead_time_mean, sd = lead_time_sd)
    inventory <- inventory - demand
    if (inventory <= rop) {
      orders <- orders + 1
      orders_placed[i] <- 1
      inventory <- inventory + order_quantity
    }
    inventory_levels[i] <- inventory
    lead_times[i] <- lead_time
  }
  holding_costs <- sum(holding_cost * max(inventory_levels, 0))
  stockout_costs <- sum(stockout_cost * max(-inventory_levels, 0))
  service_level <- mean(inventory_levels >= 0)
  results <- rbind(results, data.frame(Reorder_Point = rop, Holding_Costs = holding_costs, Stockout_Costs = stockout_costs, Service_Level = service_level))
}
print(results)


#Experiment2
# Set parameters
lead_time_mean <- 7
lead_time_sd <- 2
demand_mean <- 20
demand_sd <- 5
holding_cost <- 0.5
stockout_cost <- 10
reorder_point <- 50
order_quantities <- c(50, 100, 150, 200)
safety_stock <- 0
days <- 30

# Run experiment
results <- data.frame()
for (oq in order_quantities) {
  inventory <- 0
  orders <- 0
  inventory_levels <- numeric(days)
  orders_placed <- numeric(days)
  lead_times <- numeric(days)
  for (i in 1:days) {
    demand <- rnorm(1, mean = demand_mean, sd = demand_sd)
    lead_time <- rnorm(1, mean = lead_time_mean, sd = lead_time_sd)
    inventory <- inventory - demand
    if (inventory <= reorder_point) {
      orders <- orders + 1
      orders_placed[i] <- 1
      inventory <- inventory + oq
    }
    inventory_levels[i] <- inventory
    lead_times[i] <- lead_time
  }
  holding_costs <- sum(holding_cost * max(inventory_levels, 0))
  stockout_costs <- sum(stockout_cost * max(-inventory_levels, 0))
  service_level <- mean(inventory_levels >= 0)
  results <- rbind(results, data.frame(Order_Quantity = oq, Holding_Costs = holding_costs, Stockout_Costs = stockout_costs, Service_Level = service_level))
}
print(results)


#experiment 3
lead_time_mean <- 7
lead_time_sd <- 2
demand_mean <- 20
demand_sd <- 5
holding_cost <- 0.5
stockout_cost <- 10
reorder_point <- 50
order_quantity <- 100
safety_stocks <- c(0, 25, 50, 75)
days <- 30

# Run experiment
results <- data.frame()
for (ss in safety_stocks) {
  inventory <- ss
  orders <- 0
  inventory_levels <- numeric(days)
  orders_placed <- numeric(days)
  lead_times <- numeric(days)
  for (i in 1:days) {
    demand <- rnorm(1, mean = demand_mean, sd = demand_sd)
    lead_time <- rnorm(1, mean = lead_time_mean, sd = lead_time_sd)
    inventory <- inventory - demand
    if (inventory <= reorder_point) {
      orders <- orders + 1
      orders_placed[i] <- 1
      inventory <- inventory + order_quantity
    }
    inventory_levels[i] <- inventory
    lead_times[i] <- lead_time
  }
  holding_costs <- sum(holding_cost * max(inventory_levels, 0))
  stockout_costs <- sum(stockout_cost * max(-inventory_levels, 0))
  service_level <- mean(inventory_levels >= 0)
  results <- rbind(results, data.frame(Safety_Stock = ss, Holding_Costs = holding_costs, Stockout_Costs = stockout_costs, Service_Level = service_level))
}
print(results)


# Create a data frame to store the results of the ROP experiment
results_rop <- data.frame(
  Reorder_Point = c(25, 50, 75, 100),
  Holding_Costs = c(61.70761, 70.74573, 87.09452, 94.70189),
  Stockout_Rate = c(0, 0, 0, 0)
)

# Create a data frame to store the results of the OQ experiment
results_oq <- data.frame(
  Order_Quantity = c(50, 100, 150, 200),
  Holding_Costs = c(48.39371, 74.28410, 96.40043, 120.32794),
  Stockout_Rate = c(0, 0, 0, 0)
)

# Create a data frame to store the results of the SS experiment
results_ss <- data.frame(
  Safety_Stock = c(0, 25, 50, 75),
  Service_Level = c(1, 1, 1, 1),
  Holding_Costs = c(74.26412, 73.16656, 72.75115, 73.05680)
)

library(ggplot2)

ggplot(results_rop, aes(x = Reorder_Point, y = Holding_Costs)) +
  geom_line() +
  labs(title = "Holding Costs vs. Reorder Point", x = "Reorder Point", y = "Holding Costs")

ggplot(results_oq, aes(x = Order_Quantity, y = Holding_Costs)) +
  geom_line() +
  labs(title = "Holding Costs vs. Order Quantity", x = "Order Quantity", y = "Holding Costs")

ggplot(results_ss, aes(x = Safety_Stock, y = Service_Level)) +
  geom_line() +
  labs(title = "Service Level vs. Safety Stock", x = "Safety Stock", y = "Service Level")

ggplot(results_rop, aes(x = Reorder_Point, y = Stockout_Rate)) +
  geom_line() +
  labs(title = "Stockout Rate vs. Reorder Point", x = "Reorder Point", y = "Stockout Rate")

ggplot(results_rop, aes(x = Reorder_Point, y = Stockout_Rate)) +
  geom_line() +
  labs(title = "Stockout Rate vs. Reorder Point", x = "Reorder Point", y = "Stockout Rate")
library(lpSolveAPI)

# Define the objective function coefficients
obj.fun <- c(2.4683064, 1.4149174, 1.1612636)

# Define the constraint matrix coefficients
const.mat <- matrix(c(1, 1, 1, 
                      1, 0, 1, 
                      0, 1, 0), 
                    nrow = 3, byrow = TRUE)

# Define the constraint directions and right-hand sides
const.dir <- c(">=", "<=", "<=")
const.rhs <- c(0.95, 100, 50)

# Create a new linear programming problem
lp_model <- make.lp(nrow = 3, ncol = 3)

# Set the objective function coefficients
set.objfn(lp_model, obj.fun)

# Add constraints to the linear programming model
add.constraint(lp_model, const.mat[1,], const.dir[1], const.rhs[1])
add.constraint(lp_model, const.mat[2,], const.dir[2], const.rhs[2])
add.constraint(lp_model, const.mat[3,], const.dir[3], const.rhs[3])

# Set the variable bounds
set.bounds(lp_model, lower = c(0, 0, 0), upper = c(100, 100, 100))

# Solve the linear programming problem
solve(lp_model)

# Get the solution
get.objective(lp_model)
get.variables(lp_model)


# Define the objective function coefficients
obj.fun <- function(x) {
  # Cost objective
  cost <- c(2.4683064, 1.4149174, 1.1612636)
  
  # Service level objective
  service <- c(1, 1, 1)
  
  # Weighted objective function
  weighted.obj <- 0.6 * cost - 0.4 * service
  
  return(weighted.obj)
}

# Define the constraint matrix coefficients
const.mat <- matrix(c(1, 1, 1, 
                      1, 0, 1, 
                      0, 1, 0), 
                    nrow = 3, byrow = TRUE)

# Define the constraint directions and right-hand sides
const.dir <- c(">=", "<=", "<=")
const.rhs <- c(0.95, 100, 50)

# Create a new linear programming problem
lp_model <- make.lp(nrow = 3, ncol = 3)

# Set the objective function coefficients
set.objfn(lp_model, c(2.4683064, 1.4149174, 1.1612636))

# Add constraints to the linear programming model
add.constraint(lp_model, const.mat[1,], const.dir[1], const.rhs[1])
add.constraint(lp_model, const.mat[2,], const.dir[2], const.rhs[2])
add.constraint(lp_model, const.mat[3,], const.dir[3], const.rhs[3])

# Set the variable bounds
set.bounds(lp_model, lower = c(0, 0, 0), upper = c(100, 100, 100))

# Solve the linear programming problem
solve(lp_model)

# Get the solution
get.objective(lp_model)
get.variables(lp_model)
