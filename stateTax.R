stateTax <-
  function (state, status, wage) {
    
    library(dplyr)
    
    stateTax <- tbl_df(read.csv("stateTax.csv", stringsAsFactors=FALSE))
#    stateTax <- tbl_df(read.csv("~/Dropbox/stateTax.csv", stringsAsFactors=FALSE))
    stateTax$state <- gsub(" \\(.+\\)", "", stateTax$state, perl=TRUE)
    stateTax$state <- gsub("\\(.+\\)", "", stateTax$state, perl=TRUE)
    
    name <- state
    request <- stateTax %>% filter(state==name, type==status) %>% select(rate, bracket)
    bracket <- request$bracket
    rate <- as.numeric(request$rate)
    
    window <- c(NA, tail(bracket,-1)-head(bracket,-1))
    
    diff <- sapply(bracket, function(x) wage-x)
    end <- diff+window
    
    n <- 0
    for (i in rev(end)) {
      if (i > 0) {
        last <- i
        ind <- length(bracket)-n
        break
      }
      n <- n+1
    }
    
    window[1] <- bracket[1]
    window[ind] <- last
    sum(window[1:ind]*rate[1:ind])
    
  }

# Example call:
#
# stateTax("Maryland", "single", 100000)
