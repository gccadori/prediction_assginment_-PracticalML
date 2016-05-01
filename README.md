## Prediction Assignment for Practical ML - Coursera Data Science Specialization
# Random Forest Algorithm

# Reading in data
data <- read.csv("pml-training.csv", na.strings=c("NA",""), strip.white=T)


# Cleaning data
qt_NA <- apply(data, 2, function(x) { sum(is.na(x)) })

final_data <- subset(data[, which(qt_NA == 0)], 
                     select=-c(X, user_name, new_window, num_window, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp))


# Data partitioning - creating training and testing data sets
inTrain <- createDataPartition(final_data$classe, 
                                   p=0.65, list=FALSE)
training <- final_data[inTrain,]
testing <- final_data[-inTrain,]


# Modeling data
ctrl <- trainControl(allowParallel=TRUE, method="cv", number=4)
model <- train(classe ~ ., data=training, model="rf", trControl=ctrl)
predictions <- predict(model, newdata=testing)


# Assessing model accuracy
sum(predictions == testing$classe) / length(predictions)

confusionMatrix(testing$classe, predictions)$table


# Predicting from data - Quiz 3 Solution
test_data <- read.csv("pml-testing.csv", na.strings=c("NA",""), strip.white=TRUE)
used_test_data <- subset(test_data[, which(qt_NA == 0)], 
                        select=-c(X, user_name, new_window, num_window, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp))
predict(model, newdata=used_test_data)

