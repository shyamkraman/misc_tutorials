
## This tutorial code provides a basic approach to
## the gTrendsR package which allows for scraping
## of google trends data using a curl script


  ## Install latest version of gTrendsR, restart

    devtools::install_github("PMassicotte/gtrendsR")

  ## Load appropriate packages

    library(gtrendsR) ## For google trends
    library(maps) ## For list of state abbreviations
    library(tidyverse) ## For cleaning
    library(rio) ## For import/export
  
  ## The below function produces a pause of time
  ## so the server doesn't think we are attacking it.
  ## This is necessary since the scraping is effectively
  ## taking data from the server repeatedly
    
    pause <- function(x){
      p1 <- proc.time()
      Sys.sleep(x)
      proc.time() - p1 # The cpu usage should be negligible
    }
    
    ## Set up macros to loop over
    
      data("state.fips")
      states <- state.fips$abb
      
      startdate <- "2020-01-31" ## must be in YYYY-MM-DD, change to desired
      enddate <- "2020-03-31" ## must be in YYYY-MM-DD, change to desired
      dates <- paste(startdate, enddate, sep = " ")

    ## Create vector of keywords (no more than 5, no less than 1)
    
      keywords <- c("these", "are", "keywords")
      
    ## Create empty base df
      
      trenddat <- data.frame()
    
    ## Loop over states for trends data
      
      for(state in states){
        
        print(paste("Now extracting google trends data for " ,state, sep = ""))
        
        ## Pull trends data for specified keywords and state
        
          holder <- gtrends(keyword = keywords,
                            time = dates,
                            geo = paste("US-", state, sep = ""))
    
        ## Clean data pull and prep for appending
        
          tempdat <- holder$interest_over_time
          
          tempdat <- tempdat %>%
                        select(c(date, hits, geo, keyword)) %>%
                        mutate(state = substr(geo, 4,5), 
                               date = as.Date(substr(date, 1, 10)))
        
        ## Append listed state to overall dataset, remove temp
        
          trenddat <- rbind(trenddat, tempdat)
          rm(tempdat)
          
        ## Use above function to create pause of time
          
          pause(1)
          
      }
      
      View(trenddat)
    
    
    
    
    
    
    
    
    
    