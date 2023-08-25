library(randomForest)


data <- read.csv("/Users/jinkim/Library/CloudStorage/OneDrive-Personal/Aucklanduni/2023 S2/ECON 723/Group Project/data_nz.csv")

# find bond_closing_yields_1year missing data
missing_index <- which(is.na(data$bond_closing_yields_1year))

# check Swap_rates_1year data
data <- data[!is.na(data$Swap_rates_1year), ]

# use Swap_rates_1year predict bond_closing_yields_1year missing value
# separate as two part: bond_closing_yields_1year data and bond_closing_yields_1year missing data

data_with_values <- data[!is.na(data$bond_closing_yields_1year), ]
data_missing_values <- data[is.na(data$bond_closing_yields_1year), ]

# use randomForest
rf_model <- randomForest(bond_closing_yields_1year ~ Swap_rates_1year, data=data_with_values, ntree=10000)

# predict missing data
predicted_values <- predict(rf_model, data_missing_values)

# fill data
data$bond_closing_yields_1year[missing_index] <- predicted_values






# find bond_closing_yields_2year missing data
missing_index <- which(is.na(data$bond_closing_yields_2year))

# check Swap_rates_2year data
data <- data[!is.na(data$Swap_rates_2year), ]

# use Swap_rates_2year predict bond_closing_yields_2year missing value
# separate as two part: bond_closing_yields_2year data and bond_closing_yields_2year missing data

data_with_values <- data[!is.na(data$bond_closing_yields_2year), ]
data_missing_values <- data[is.na(data$bond_closing_yields_2year), ]

# use randomForest
rf_model1 <- randomForest(bond_closing_yields_2year ~ Swap_rates_2year, data=data_with_values, ntree=10000)

# predict missing data
predicted_values1 <- predict(rf_model1, data_missing_values)

# fill data
data$bond_closing_yields_2year[missing_index] <- predicted_values1



# save data
write.csv(data, "/Users/jinkim/Library/CloudStorage/OneDrive-Personal/Aucklanduni/2023 S2/ECON 723/Group Project/nz.csv", row.names=FALSE)
