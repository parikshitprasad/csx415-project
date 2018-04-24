library('ProjectTemplate')
load.project()

library('rpart')
library('rpart.plot')

trainRpartModel <- train %>% select("Id", "GrLivArea", "LotArea", "Neighborhood", "SaleCondition", "SalePrice")

rpartModelWithNeighborhood <- rpart(SalePrice ~ GrLivArea + LotArea + Neighborhood, trainRpartModel)

trainRpartModel$priceEstimate <- predict(rpartModelWithNeighborhood)

trainRpartModel$percError = abs(trainRpartModel$priceEstimate-trainRpartModel$SalePrice)*100/trainRpartModel$SalePrice

trainRpartModel$sqError = (trainRpartModel$priceEstimate - trainRpartModel$SalePrice)^2

errorMetrics <- trainRpartModel %>% summarize(medianError = median(percError),
                                               rmse = sum(sqError)/n())  