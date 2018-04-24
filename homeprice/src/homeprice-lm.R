library('ProjectTemplate')
load.project()

library('lme4')

trainLinearModel <- train %>% select("Id", "GrLivArea", "LotArea", "Neighborhood", "SaleCondition", "SalePrice")

linearModelWithNeighborhood <- lmList(SalePrice ~ GrLivArea + LotArea | Neighborhood, trainLinearModel)

trainLinearModel$priceEstimate <- predict(linearModelWithNeighborhood)

trainLinearModel$percError = abs(trainLinearModel$priceEstimate-trainLinearModel$SalePrice)*100/trainLinearModel$SalePrice

trainLinearModel$sqError = (trainLinearModel$priceEstimate - trainLinearModel$SalePrice)^2

errorMetrics <- trainLinearModel %>% summarize(medianError = median(percError),
                                            rmse = sum(sqError)/n())  


