## ---------------------------------------------------------------------------------
##
## Script name: tidyRegressions.R
##
## Purpose of script: How to run a brief set of tidy regressions in R 
##
## Project name: Miscellani
##
## Author: Shyam Raman
##
## Date Created: 2022-04-14
##
## ---------------------------------------------------------------------------------
##
## Notes: Dependencies are pacman, fixest, tidyverse, vtable, and wooldridge
##   
##
## ---------------------------------------------------------------------------------
    


        pacman::p_load(fixest, wooldridge, tidyverse, vtable)
        
        
        data('countymurders') # data from Wooldridge Intro Metrics
        
        depvars <- c('murdrate', 'arrestrate', 'execrate') # murder,arrest, execution rate per 1e4
        
        ctrls <- names(countymurders)[str_detect(names(countymurders), 'perc')] # control variables
        
        tidyregs <- countymurders %>% 
          fixest::fepois(.[depvars] ~ .[ctrls] # runs on each depvar separately
                         | countyid + year, # county + year FEs
                         cluster = ~countyid, # county clustered SEs
                         weights = ~popul) # population weighted
        
        %>% # map to data frame from list created above
          
          map_dfr(broom::tidy, # run broom::tidy() on each model object in list
                  conf.int = T, # add options after -- we want 95% CIs too
                  .id = 'depvar') # create column named 'depvar' from list names
        
        
        
        ## Stata: di _b[per1019]
        
        tidyregs %>% 
          filter(depvar == 'murdrate', term == 'perc1019') %>% 
          pull(estimate)
        
        
        
        
        tidyregs %>% 
          
          ggplot(aes(x = estimate, y = term)) +
          
          geom_vline(aes(xintercept = 0)) + # zero line
          
          geom_linerange(aes(xmin = conf.low, xmax = conf.high)) + # CIs
          
          geom_point() + # actual estimates
          
          theme_minimal() + # add a theme
          
          facet_wrap(~depvar, # create a facet for each dependent variable
                     nrow = 1, # one row only
                     scales = 'free_x') # x axis can be different, keep a single y axis
        
