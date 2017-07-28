## Big picture, have a function that can call other functions for interpretation based on "type"
# Regression Interpretation
#
# Interpreter <- function(type = c("lm", "mlm", "ttest"))) {
#     .Call(function of type)
#     return(description of output for type)
#}
#
#
#
library(ISLR)
library(MASS)

data(Boston)
data(Auto)

Regression <- function(dataset = Auto, outcome = 1, predictor = 3, pred2 = NULL) {
  
  if(is.null(pred2)) {
    
      fit <- lm(dataset[,outcome] ~ dataset[,predictor])
      
        } else {
          
             fit <- lm(dataset[,outcome] ~ dataset[,predictor] + dataset[,pred2])
        }
  
  stats <- summary.lm(fit, correlation = TRUE)
  
  
  selection <- function() {
   
     output <- data.frame(ID = 1:length(stats), Statistics = names(stats))
    print(output)
    
        x <- readline("Which output would you like to see? ")
        x <- as.numeric(x)
  

  if(is.numeric(x) & x <= length(stats)) { 
        
        print(noquote(paste("You selected number", x, "which is", output[x,2])))
        message(noquote(paste("Retrieving", paste0(output[x, 2], "..."))))
        # Input a function that will take output as arg
        # Will use arg, go to stats[[x]] interpret it, and return it all here
        return(stats[[x]])
        
        } else {

       message("Please pick from the ID numbers shown")
          x <- NULL
          Regression()
          
        }
  }
  print(selection())
  
  y <- 1
  
  while(y != "y" & y != "n") {
    
    y <- readline("Would you like to see another? [y/n]")
    
  if(y == "Y" | y == "y") {
      result <- selection()
         print(result)
         
        } else if(y == "N" | y == "n") {
            print("Ok then, thanks for learning")
              break
          
            }  else {
                print("I do not understand")
                  next
    }
  }
}

Regression()
Regression(dataset = Boston, outcome = "medv", "lstat", "age")
