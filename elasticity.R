elasticityFun <- function(train,grlm,plotHeading)
{

# estimating the elasticity coefficients

elasticity <- function(var)
{
  elax1 <-
    as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1);
} 

var_list <- list()

for(i in 2:length(grlm$coefficients))
{
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
}

elasticity.outputs <-
  data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", 
                                       "Negative")
View(elasticity.outputs)

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle(plotHeading) +xlab("Variables")
}