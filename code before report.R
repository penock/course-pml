
library(caret)

#### Load data ----
setwd("C:/Dropbox/DSL/ML/Practical Machine Learning/PML Course Project, weightlifting")
training <- read.csv('pml-training.csv')
validation <- read.csv('pml-testing.csv')

#### Find and discard variables with no info in the validation set... they won't be useful for prediction ----
nzvResult <- caret::nearZeroVar(validation, saveMetrics=T)
# Examine just the near-zero variables in Excel, to make extra sure they don't have useful data
write.csv(validation[, nzvResult$zeroVar==T], 'nzv.csv')
shell.exec('nzv.csv') # Found: They're all zero
all(nzvResult$zeroVar == nzvResult$nzv) # Found: the zero vars are the same as nonzero vars

# Discard the zero variables
uselessCols <- colnames(validation)[nzvResult$zeroVar==T]
validation <- validation[, !(colnames(validation) %in% uselessCols)] 
training <- training[, !(colnames(training) %in% uselessCols)] 

any(is.na(training)) # Found: There are no more NAs anywhere

# Also discard factors other than classe, because random forests can't use factors
validation <- validation[ , sapply(validation, is.factor) == F | names(validation) == 'classe']
training <- training[ , sapply(training, is.factor) == F | names(training) == 'classe']

# Also discard X, the column that has row numbers
validation$X <- NULL
training$X <- NULL

str(training) # Found: I have 19622 rows with 56 predictors and classe outcome variable

# training.play <- training[sample(1:nrow(training), 300), ]

modFit <- train(classe ~ .,
                method = 'rf',
                trControl = trainControl(method = 'cv'),
                data = training)
print(modFit)
finalModel <- modFit$finalModel
finalModel

predictedOutcomes <- predict(finalModel, newdata = validation)
predictedOutcomes

save(modFit, file = 'modFit.Rdata')


#### Write the answer files using the course-provided function ----
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

setwd("answer files")

pml_write_files(as.character(predictedOutcomes))
