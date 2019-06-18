#---------------- Helper functions for analyzing datasets -----------
#
# Goal: define helper functions to be used in datasets.
# binFun: convert a numerical variable into a 0/1
# lmp: return the p-value of a linear model
# modelAuto: run a t.test/lm/aov on a selected number of explanatory and
# response variables
# 
# June 17, 2019

# Binary function: if the result is above 3, then yield a 1, if below, then a 0
binFun <- function(x){
  if(is.na(x)){y = NA}
  
  else {
    if(x >= 3){y = 1}
    
    if(x < 3){y = 0}
  }
  return(y)
}


# Return the p value of a model object
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}


# Run a linear t-test/model/aov
modelAuto <- function(response, explanatory, responsedata, explanatorydata,
                       my_color = c("cadetblue3", "cadetblue4"),
                       labels = c("Nie", "Selten", "Mehrfach\nmonatlich",
                                  "Mehrfach\nwöchentl.", "Täglich"),
                      likertscale = 5,
                      method = "t-test"){ # t-test by default
  
  # Goal: Run a t-test/lm/aov on two given vectors of columnnames and do
  # - a barplot for the t-test
  # - a line plot for the linear model
  # - a boxplot for the anova
  
  # Response & explanatory should both be coded like this:
  #response <- c("literal name 1", "literal name 2")
  #names(response) <- c("name in df 1", "name in df 2)
  
  ## Error handling
  # Correct method
  if(!(method %in% c("t-test", "lm", "aov"))){
    cat("Please enter a valid method: t-test, lm or aov.\n")
  }
  
  if (is.null(names(response))){ # error handling if no column names are given
    cat("Warning: Consider using names() for the response variable.\n")
  } 
  if (is.null(names(explanatory))){ # error handling if no column names are given
    cat("Warning: Consider using names() for the explanatory variable.\n")
  } 
  
  
  results <- c()
  
  for (j in 1:length(explanatory)){ # for all explanatory vars
    for (i in 1:length(response)){ # for all means of response vars
      
      if (!is.null(names(response))){ # error handling if no column names are given
        columnnameI = names(response)[i]
      } else{
        columnnameI = response[i]
      }
      

      if (!is.null(names(explanatory))){ # error handling if no column names are given
        columnnameJ = names(explanatory)[j]
      } else{
        columnnameJ = explanatory[j]
      }
      
      explLevels = levels(explanatorydata[,columnnameJ])
      
      ### Models
      if (method == "t-test"){
        
        t <- t.test(responsedata[,columnnameI] ~ explanatorydata[,columnnameJ])
        
        # Save the mean and standard deviation for later depending on expl 0/1
        means <- tapply(responsedata[,columnnameI], explanatorydata[,columnnameJ], mean, na.rm = TRUE)
        sds <- tapply(responsedata[,columnnameI], explanatorydata[,columnnameJ], sd, na.rm = TRUE)
        
        results <- rbind(results, c(columnnameI, columnnameJ, t$p.value, means))
        colnames(results) <- c("resp", "expl", "p-value", "mean non users", "mean users")
        
        
        if(t$p.value < 0.05){ # if significant, do a barplot!
          
          # do a barplot
          mp = barplot(means, col = my_color, border = my_color[2],
                       main = paste0("Usage of ", (response)[i] ," by ", (explanatory)[j], " users"),
                       names.arg = c(paste0("non ",(explanatory)[j], " users"),
                                     paste0((explanatory)[j]," users")),
                       ylim = c(1,likertscale), xpd = FALSE, # only 1-5 answers were given
                       yaxt = "n") # no y axis, will be defined below
          
          # fake error bars
          segments(mp, means, mp, means + sds, col = "darkgrey", lwd = 1.5)
          
          # Add correct axis names
          axis(2, at = seq(1,likertscale, by = 1),
               labels = labels,
               lwd = 0.5,
               cex.axis = 0.8,
               las = 1)
        }
      }
      
      if(method == "lm"){
        l <- lm(responsedata[,columnnameI] ~ explanatorydata[,columnnameJ])
        results <- rbind(results, c(columnnameI, columnnameJ, lmp(l)))
        colnames(results) <- c("resp", "expl", "p-value")
        
        if(lmp(l) < 0.05){ # if significant, do a line plot!
          plot(responsedata[,columnnameI] ~ explanatorydata[,columnnameJ],
                  main = paste0((explanatory)[j]," and its influence\non ",(response)[i]),
               xlab = explanatory[j], ylab = response[i]) 
        }
      }
      
      if(method == "aov"){

        a <- summary(aov(responsedata[,columnnameI] ~ explanatorydata[,columnnameJ]))
        results <- rbind(results, c(columnnameI, columnnameJ, a[[1]]$'Pr(>F)'[1]))
        colnames(results) <- c("resp", "expl", "p-value")

        if(a[[1]]$'Pr(>F)' < 0.05){ # if significant, do a boxplot!
          boxplot(responsedata[,columnnameI] ~ explanatorydata[,columnnameJ],
                  main = paste0((explanatory)[j]," and its influence\non the use of ",(response)[i]),
                  names = explLevels)    
        }
      }      
    }  
  }
  
  View(results)
  
  cat(paste0("Significant results:\n"))
  print(results[results[,3]<0.05,])
  return(results)
}
