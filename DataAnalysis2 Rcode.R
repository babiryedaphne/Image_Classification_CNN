# Load necessary package
library(stringr)

# Define path to the directory
path_to_directory <- "kvasir-dataset-v2-features/kvasir-dataset-v2-features"

# Get the list of folders
folders <- list.dirs(path = path_to_directory, full.names = TRUE, recursive = FALSE)

# Get the list of files in the first folder
files <- list.files(path = folders[1], full.names = TRUE)

# Read the first file
file_content <- readLines(files[1])

# Split the file content by line breaks
features_data <- str_split(file_content, "\n", simplify = TRUE)

# Create an empty list to store the results
features_values_count <- list()

# Loop over the features
for (i in 1:length(features_data)) {
  # Split the feature name and values
  feature_name_values <- str_split(features_data[i], ":", simplify = TRUE)
  
  # Get the feature name
  feature_name <- feature_name_values[1]
  
  # Get the feature values and count them
  feature_values <- str_split(feature_name_values[2], ",", simplify = TRUE)
  feature_values_count <- length(feature_values)
  
  # Store the result
  features_values_count[[feature_name]] <- feature_values_count
}

# Print the result
print(features_values_count)

#Create the feature vector
#-----------------------------------------------------------------
#create column names 

# Initialize an empty list to store the vectors
feature_vectors <- list()

# For each feature in the list
for (feature in names(features_values_count)) {
  # Repeat the feature name according to the number indicated next to the feature
  # Add a number on the feature name indicating the number the feature name has been repeated
  feature_vectors[[feature]] <- paste0(feature, 1:features_values_count[[feature]])
}

# Combine all vectors into one
combined_vector <- unlist(feature_vectors)

# Print the combined vector
print(combined_vector)

feature_vector1 <- unname(combined_vector)
feature_vector1


# create a vector of the feature values 
#-----------------------------------------------------------------------------------

# Define path to the directory
path_to_directory <- "kvasir-dataset-v2-features/kvasir-dataset-v2-features"

# Get the list of folders
folders <- list.dirs(path = path_to_directory, full.names = TRUE, recursive = FALSE)

# Get the list of files in the first folder
files <- list.files(path = folders[1], full.names = TRUE)

# Read the first file
lines <- readLines(files[1])

# Initialize an empty list to store the extracted numbers
numbers_list <- list()

# Loop over the lines
for (line in lines) {
  # Split the line by the colon
  parts <- strsplit(line, ":")[[1]]
  
  # The numbers are in the second part, split it by the commas
  numbers <- strsplit(parts[2], ",")[[1]]
  
  # Convert the numbers to numeric and store them in the list
  numbers_list <- c(numbers_list, as.numeric(numbers))
}

# Convert the list to a one-dimensional array
numbers_array <- unlist(numbers_list)


#Create a data frame with the feature names and values 
#-----------------------------------------------------------
# Create a data frame
df <- data.frame(matrix(unlist(numbers_array), nrow=length(numbers_array[[1]]), byrow=TRUE))
names(df) <- unlist(feature_vector1)

# Add index column
df$index <- 1:nrow(df)

# Optionally, if you want the index column to be the first column:
df <- df[, c(ncol(df), 1:(ncol(df)-1))]
head(df)

library(xlsx)
write.xlsx2(df,'df1.xlsx', row.names = T)


#--------------------------------------------------------------------------------------
#create a data frame reading all files 
#--------------------------------------------------------------------------------------

# Define path to the directory
path_to_directory <- "kvasir-dataset-v2-features/kvasir-dataset-v2-features"

# Get the list of folders
folders <- list.dirs(path = path_to_directory, full.names = TRUE, recursive = FALSE)

# Initialize an empty data frame to store the results
df <- data.frame()

# Loop over the folders
for (folder in folders) {
  # Get the list of files in the current folder
  files <- list.files(path = folder, full.names = TRUE)
  
  # Loop over the files
  for (file in files) {
    # Read the file
    lines <- readLines(file)
    
    # Initialize an empty list to store the extracted numbers
    numbers_list <- list()
    
    # Loop over the lines
    for (line in lines) {
      # Split the line by the colon
      parts <- strsplit(line, ":")[[1]]
      
      # The numbers are in the second part, split it by the commas
      numbers <- strsplit(parts[2], ",")[[1]]
      
      # Convert the numbers to numeric and store them in the list
      numbers_list <- c(numbers_list, as.numeric(numbers))
    }
    
    # Convert the list to a one-dimensional array
    numbers_array <- unlist(numbers_list)
    
    # Check if the number of values is the same as the length of feature_vector1
    if (length(numbers_array) != length(feature_vector1)) {
      print(paste("The file", file, "in folder", folder, "does not have the same number of values as the length of feature_vector1."))
      next  # Skip to the next file
    }
    
    # Create a data frame for this file
    df_file <- data.frame(matrix(numbers_array, nrow=1, ncol=length(numbers_array), byrow=TRUE))
    names(df_file) <- feature_vector1
    
    # Add a target column
    df_file$target <- basename(folder)
    
    # Append this data frame to the overall data frame
    df <- rbind(df, df_file)
  }
}

# Add an index column
df$index <- 1:nrow(df)

# Reorder the columns to put the index and target columns first
df <- df[, c("index", "target", feature_vector1)]


#---------------------------------------------------------
#reproduce the ‘6 GF Random Forrest Model
#------------------------------------------------------------
library(randomForest)
install.packages('mlbench')
library(mlbench)
library(caret)
library(e1071)
library(ModelMetrics)
library(MLmetrics)

#split the data into train and test
#change the target to factor

df$target = make.names(df$target)



library(caTools) # for sample. split function
set.seed(283)
sample = sample.split(df, SplitRatio = 0.8)
train = subset(df, sample == TRUE)
train = train[,2:1187]# remove index column
test  = subset(df, sample == FALSE)
test = test[, 2:1187]# remove index column

#source:https://towardsdatascience.com/a-guide-to-using-caret-in-r-71dec0bda208


# define the target and predictors

#train
x_train <- train[,2:1186]
y_train <- train[,1]

#test
x_test <- test[,2:1186]
y_test <- test[,1]


# Build the Rf model

#2 folds cross validation repeat 2 times

## Create the training control
control <- trainControl(method='repeatedcv', 
                        number=2, 
                        repeats=2,
                        verboseIter = FALSE,
                        classProbs = TRUE,
                        summaryFunction = multiClassSummary,
                        selectionFunction = "best",
                        savePredictions = TRUE
                        )


#Metric compare model is Accuracy
#metrics <- c("Accuracy", "Precision", "Recall", "Spec", "MCC", "F1")
metric = 'Accuracy'

set.seed(283)

#randomly selected variable  is mtry
mtry <- sqrt(ncol(x_train))
mtry # its 34.42383

tunegrid <- expand.grid(.mtry=mtry)
rf_base <- train(target~., 
                    data=train, 
                    method='rf', 
                    metric= 'Accuracy', 
                    tuneGrid=tunegrid, 
                    trControl=control)
print(rf_base)


plot(rf_base)



#Source: https://rpubs.com/phamdinhkhanh/389752

#--------------------------------------------------------------------
#Model Tuning to improve Performance
#-------------------------------------------------------------------

# use gride search to tune mtry

#Create control function for training with 3 folds and keep 2 folds for training. search method is grid.
control <- trainControl(method='repeatedcv', 
                        number=2, 
                        repeats=1, 
                        search='grid',
                        verboseIter = FALSE,
                        classProbs = TRUE,
                        summaryFunction = multiClassSummary,
                        selectionFunction = "best",
                        savePredictions = TRUE)

#create tunegrid with 15 values from 1:15 for mtry to tunning model. Our train function will change number of entry variable at each split according to tunegrid. 
tunegrid <- expand.grid(.mtry = (32:36)) 

rf_gridsearch <- train(target ~ ., 
                       data = train,
                       method = 'rf',
                       metric = 'Accuracy',
                       tuneGrid = tunegrid,
                       trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

# Model Evaluation
#-------------------------------------------
# Use the model to make predictions on the test set
predictions <- predict(rf_base, newdata = x_test)

# Import the required libraries for computing the metrics
# Convert the predictions to factors
predictions <- as.factor(predictions)

y_test<- as.factor(y_test)


# Create a confusion matrix
cm <- caret::confusionMatrix(as.factor(predictions), as.factor(y_test))

# Now let's check the class again
print(class(cm))

# Print the confusion matrix
print(cm)

# Compute Accuracy
accuracy <- sum(predictions == y_test) / length(y_test)
cat("Accuracy: ", accuracy, "\n")

# Compute weighted Precision, Recall, F1 Score
result <- cm$byClass
weights <- table(y_test) / length(y_test)

weighted_precision <- sum(result[,"Pos Pred Value"] * weights)
weighted_recall <- sum(result[,"Sensitivity"] * weights)
weighted_F1 <- sum(result[,"F1"] * weights)
weighted_accuracy <- sum(result[,'Balanced Accuracy']*weights)#0.88
weighted_specificity <- sum(result[,'Specificity']*weights)#0.97
result

cat("Weighted Precision: ", weighted_precision, "\n")
cat("Weighted Recall: ", weighted_recall, "\n")
cat("Weighted F1 Score: ", weighted_F1, "\n")

