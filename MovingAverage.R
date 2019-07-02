Moving_Average <- function(dataset)
{
  price_inf <- as.data.frame(movavg(dataset$list_price,3,"s"))
  names(price_inf) <- "Moving_Average"
  merge(dataset, price_inf, by="row.names", all.x=TRUE)
}