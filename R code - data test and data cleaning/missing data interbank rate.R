library(randomForest)


data <- read.csv("/Users/jinkim/Library/CloudStorage/OneDrive-Personal/Aucklanduni/2023 S2/ECON 723/Group Project/interbank.csv")

# find bond_closing_yields_1year missing data
missing_index <- which(is.na(data$bond_closing_yields_1year))

# check interbank1 data
data <- data[!is.na(data$interbank1), ]

# use interbank1 predict bond_closing_yields_1year missing value
# separate as two part: bond_closing_yields_1year data and bond_closing_yields_1year missing data

data_with_values <- data[!is.na(data$bond_closing_yields_1year), ]
data_missing_values <- data[is.na(data$bond_closing_yields_1year), ]

# use randomForest
rf_model <- randomForest(bond_closing_yields_1year ~ interbank1, data=data_with_values, ntree=10000)

# predict missing data
predicted_values <- predict(rf_model, data_missing_values)

# fill data
data$bond_closing_yields_1year[missing_index] <- predicted_values



# save data
write.csv(data, "/Users/jinkim/Library/CloudStorage/OneDrive-Personal/Aucklanduni/2023 S2/ECON 723/Group Project/nz.csv", row.names=FALSE)
