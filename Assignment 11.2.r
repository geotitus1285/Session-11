for(level in unique(mydata$job)){
mydata[paste("job", level, sep = "_")] <- ifelse(mydata$job == level, 1, 0)
}
set.seed(1)
training_size <- floor(0.80 * nrow(mydata))
train_ind <- sample(seq_len(nrow(mydata)), size = training_size)
training <- mydata[train_ind, ]
testing <- mydata[-train_ind, ]
preProcValues <- preProcess(training, method = c("center", "scale"))
scaled.training <- predict(preProcValues, training)
scaled.testing <- predict(preProcValues, testing)
down_training <- downSample(x = scaled.training[, -ncol(scaled.training)], y = scaled.training$Class)
up_training <- upSample(x = scaled.training[, -ncol(scaled.training)], y = scaled.training$Class)
smote_training <- SMOTE(Class~., data = scaled.training)
rose_training <- ROSE(Class~., data = scaled.training, seed=2)$data
set.seed(1)
training_size <- floor(0.80 * nrow(mydata))
train_ind <- sample(seq_len(nrow(mydata)), size = training_size)
training <- mydata[train_ind, ]
testing <- mydata[-train_ind, ]
