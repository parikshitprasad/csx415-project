library('ProjectTemplate')
load.project()

trainLinearModel <- train %>% select("Id", "GrLivArea", "LotArea", "Neighborhood", "SaleCondition", "SalePrice")

linearModelWithNeighborhood <- lmList(SalePrice ~ GrLivArea + LotArea | Neighborhood, trainLinearModel)

trainLinearModel$priceEstimate <- predict(linearModel)

trainLinearModel$percError = abs(trainLinearModel$priceEstimate-trainLinearModel$SalePrice)*100/trainLinearModel$SalePrice

trainLinearModel$sqError = (trainLinearModel$priceEstimate - trainLinearModel$SalePrice)^2

errorMetrics <- trainLinearModel %>% summarize(medianError = median(percError),
                                            rmse = sum(sqError)/n())  


