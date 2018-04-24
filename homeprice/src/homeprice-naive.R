library('ProjectTemplate')
load.project()

# Generate average of price / sq.ft. for each neighborhood
avgPricePerSqFt <- train %>%
  group_by(Neighborhood) %>%
  summarize(avgPricePerSqFt = mean(SalePrice/GrLivArea))

# Estimate price based on average price / sq.ft
priceEstimate <- train %>%
  select("Id", "Neighborhood", "GrLivArea", "SalePrice") %>%
  inner_join(avgPricePerSqFt, by = "Neighborhood") %>%
  mutate(salePriceEstimate = GrLivArea * avgPricePerSqFt,
         percError = abs(salePriceEstimate-SalePrice)*100/SalePrice,
         sqError = (salePriceEstimate-SalePrice)^2)

# Find median percentage error
errorMetrics <- priceEstimate %>% summarize(medianError = median(percError),
                                                     rmse = sum(sqError)/n())

