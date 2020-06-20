library(readxl)
library(rvest)
library(foreign)
library(stringr)
library(data.table)
library(dplyr)
library(tidyr)
library(curl)
library(magrittr)


### scraping all year links from champcarstats site, coercing into a dataframe and naming columns

url <- "http://www.champcarstats.com/Year.htm"

year_links <- read_html(url) %>%
      html_nodes("td:nth-child(1) a") %>%
      html_attr("href") %>%
      as.data.frame()
colnames(year_links) <- c("url")
year_links <- year_links[!(year_links$url %in% c("http://www.champcarstats.com/year/1998i.htm")),] %>% 
      as.data.frame()
colnames(year_links) <- c("url")


#Create empty data frame with 'URLs' column
indycar_race_results <- data.frame(url = character(),
                                   year = numeric(),
                                   race_date = character(), 
                                   race_name = character(), 
                                   track_name = character(), 
                                   race_miles = numeric(), 
                                   track_type = character(), 
                                   race_winner = character())

#For loop from the year_links dataframe, scraping each year for metadata
for( i in 1:nrow(year_links))
{ holder_url <- paste(year_links[i,"url"])
holder_webpage <- read_html(holder_url)

#Scraping for the yearly schedules
holder_table <- holder_webpage %>%
  html_node("div table") %>%
  html_table(header = TRUE)

#Scraping for race links by identifying 'href' attribute in yearly sched tables
links <- holder_webpage %>%
  html_nodes("div tr+ tr td:nth-child(2) a")%>%
  html_attr("href")%>%
  as.data.frame()

#Coercing URL values to dataframe and naming column 'URLs' for appending
colnames(links) <- c('url')

#Naming columns for holder_table
var_names <- c('race_date', 'race_name', 'track_name', 'race_miles', 'track_type', 'race_winner')
setnames(holder_table, var_names)

#Cleaning duplicates from links table
links$url <- tolower(links$url)%>%
  trimws()
links <- subset(links, !duplicated(links$url))

#Adding an ID column for merging and dropping ID column after merge
links$ID <- seq.int(nrow(links))
holder_table$ID <- seq.int(nrow(holder_table))
x <- merge(links, holder_table, by = 'ID')
x$ID <- NULL

#appending new results into overall results table
indycar_race_results <- rbind(indycar_race_results, x)


# Create a pause of time, so the server doesn't think we are attacking it.
testit <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}
testit(1)

} 

######################################

##Scraping data for 1998 IRL due to difference in HTML node
holder_webpage <- read_html("http://www.champcarstats.com/year/1998i.htm")

#Scraping for the yearly schedule
holder_table <- holder_webpage %>%
  html_node(xpath = '//*[(@id = "table4")]') %>%
  html_table(header = TRUE)

#Scraping for race links by identifying 'href' attribute in yearly sched tables
links <- holder_webpage %>%
  html_nodes("#table4 tr+ tr td:nth-child(2) a")%>%
  html_attr("href")%>%
  as.data.frame()

#Coercing URL values to dataframe and naming column 'URLs' for appending
colnames(links) <- c('url')
#links$year <- '1998i'

#creating holder value for data table and assigning column names
var_names <- c('race_date', 'race_name', 'track_name', 'race_miles', 'track_type', 'race_winner')
setnames(holder_table, var_names)
#Cleaning duplicates from links table
links$url <- tolower(links$url)%>%
  trimws()
links <- subset(links, !duplicated(links$url))

#Generating ID var and merging by ID
links$ID <- seq.int(nrow(links))
holder_table$ID <- seq.int(nrow(holder_table))
x <- merge(links, holder_table, by = 'ID')
x$ID <- NULL
#x <- assign(paste('race_results_', '1998i'), data.frame(x))

#appending 1998i results to overall dataset
indycar_race_results <- rbind(indycar_race_results, x)

####################################

#Generating unique race ID by substringing url and eliminating ".htm" ending,
#then converting all to lowercase and trimming whitespace
#Subsetting results to only unique obs based on id 

indycar_race_results$id <- substr(indycar_race_results$url, 36, 60) %>%
  strsplit(".htm") %>%
  tolower() %>%
  trimws()
indycar_race_results <- subset(indycar_race_results, !duplicated(indycar_race_results$id))


#####################################
#creating empty data frame for metadata 
indycar_race_metadata <- data.frame(total_laps_driven = numeric(),
                                    track_name = character(),
                                    track_details = character(),
                                    race_laps = numeric(),
                                    race_date = character(),
                                    id = numeric())

#creating race url dataframe and removing id races with issues - html nodes are not scraping at all
race_urls <- indycar_race_results[c("url", "id")]
race_urls <- race_urls[!(race_urls$id %in% c(192114, 191116, 191119, 191118,191117, 191310, 192114, 192803,195208, "1953n03", 
                                              195801, 195811, 196202, 197101, 197102, 198011, 198110, 198705, 199307, 199518, 
                                              199601, 199609, 199701, 199704, 199709, 199716, 199718, 
                                              199801, 199804, 199809, 199810, 199816, 199901, 199904, 199908, 199909, 
                                              199913, 199915, 199917, 199920, 200001, 200005, 200008, 
                                              200012, 200016, 200101, 200106, 200107, 200108, 200111, 200117, 
                                              200118, 200120, "2001i01", 200302, 200306, 200309, 200312, 200313, 200314, 
                                              200315, 200316, 200317, 200610, 201104)),]

#For loop to scrape metadata from indycar races
for( i in 1:nrow(race_urls))
{holder_url <- paste(race_urls[i,"url"])
url <- read_html(holder_url)

#scraping race metadata from each url in race_urls
race_metadata <- url %>%
  html_node("font+ font")%>%
  html_text() %>%
  strsplit("\n")%>%
  as.data.frame()%>%
  t()%>%
  as.data.frame()

### cleaning race metadata and separating the track name into details and 
###separating the length into laps and miles
colnames(race_metadata) <- c('track_name', 'track_details', 'race_length')
race_metadata <- race_metadata[c('track_name', 'track_details', 'race_length')]
race_metadata <- separate(race_metadata, race_length, c("race_laps", "race_miles"), "Laps/")

### scraping race from each link, adding variable names, and applying
race <- html_table(url, header = TRUE, fill = TRUE) [1] %>%
        as.data.frame()
varnames <- c("finish_position", "start_position", "driver_name", "car_number",
              "sponsor_name", "chassis", "engine", "laps_driven", "driver_status",
              "pts")
colnames(race) <- varnames

### coercing laps_driven into a numeric vector and aggregating that vector for each race,
### creating total_laps_driven and coercing into data frame
race$laps_driven <- as.numeric(race$laps_driven)
total_laps_driven <- sum(race[,c("laps_driven")], na.rm=TRUE) %>%
  as.data.frame()
race <- total_laps_driven
colnames(race) <- c("total_laps_driven")

### adding other metadata to total_laps_driven to receive race metadata
race$track_name <- race_metadata$track_name
race$track_details <- race_metadata$track_details
race$race_laps <- race_metadata$race_laps
race$race_date <- race_metadata$race_date
race$id <- race_urls[i, "id"]

#appending the running metadata df with the new race metadata
indycar_race_metadata <- rbind(indycar_race_metadata, race)



testit <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}
testit(1)

} 


########################################
## Edited code for selected id obs which had issue with html node

race_urls <- indycar_race_results[c("url", "id")]

#creating race url dataframe and removing id races with issues - html nodes are not scraping at all
race_urls <- race_urls[(race_urls$id %in% c(198705, 199307, 199518, 199601, 199609, 199701, 199704, 199709, 199716, 199718, 
                                             199801, 199804, 199809, 199810, 199816, 199901, 199904, 199908, 199909, 
                                             199913, 199915, 199917, 199920, 200001, 200005, 200008, 
                                             200012, 200016, 200101, 200106, 200107, 200108, 200111, 200117, 
                                             200118, 200120, "2001i01", 200302, 200306, 200309, 200312, 200313, 200314, 
                                             200315, 200316, 200317)),]

#For loop to scrape metadata from indycar races
for( i in 1:nrow(race_urls))
{holder_url <- paste(race_urls[i,"url"])
url <- read_html(holder_url)

#scraping race metadata from each url in race_urls
race_metadata <- url %>%
  html_node("b+ font")%>%
  html_text() %>%
  strsplit("\n")%>%
  as.data.frame()%>%
  t()%>%
  as.data.frame()


### cleaning race metadata and separating the track name into details and 
### separating the length into laps and miles
colnames(race_metadata) <- c('track_name', 'track_details', 'race_length')
race_metadata <- race_metadata[c('track_name', 'track_details', 'race_length')]
race_metadata <- separate(race_metadata, race_length, c("race_laps", "race_miles"), "Laps/")

### scraping race from each link, adding variable names, and applying
race <- html_table(url, header = TRUE, fill = TRUE) [1] %>%
  as.data.frame()
varnames <- c("finish_position", "start_position", "driver_name", "car_number",
              "sponsor_name", "chassis", "engine", "laps_driven", "driver_status",
              "pts")
colnames(race) <- varnames

### coercing laps_driven into a numeric vector and aggregating that vector for each race,
### creating total_laps_driven and coercing into data frame
race$laps_driven <- as.numeric(race$laps_driven)
total_laps_driven <- sum(race[,c("laps_driven")], na.rm=TRUE) %>%
  as.data.frame()
race <- total_laps_driven
colnames(race) <- c("total_laps_driven")

### adding other metadata to total_laps_driven to receive race metadata
race$track_name <- race_metadata$track_name
race$track_details <- race_metadata$track_details
race$race_laps <- race_metadata$race_laps
race$race_date <- race_metadata$race_date
race$id <- race_urls[i, "id"]

#appending the running metadata df with the new race metadata
indycar_race_metadata <- rbind(indycar_race_metadata, race)



testit <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}
testit(1)

} 

##########################
#code to merge race results to metadata
indycar_race_results$track_name <- NULL
indycar_race_metadata$race_laps <- NULL
indycar_race_results$track_type <- NULL
indycar_races <- merge(indycar_race_results, indycar_race_metadata, by = "id")
indycar_races <- separate(indycar_races, track_details, c("track_miles", "track_type"), "Mile")
indycar_races$track_miles <- as.numeric(indycar_races$track_miles)
indycar_races$total_miles_driven <- indycar_races$track_miles * indycar_races$total_laps_driven
indycar_races$track <- tolower(indycar_races$track_name)
indycar_races$track_name <- NULL

############################
#hard geocoding of tracks

indycar_tracks <- table(indycar_races$track) %>% as.data.frame()
colnames(indycar_tracks) <- c("track", "freq")
indycar_tracks$freq <- NULL
indycar_tracks$track_id <- seq.int(indycar_tracks$track)
indycar_tracks$lat[indycar_tracks$track_id ==1] <- 41.0814447
indycar_tracks$lat[indycar_tracks$track_id ==2] <- 40.60153
indycar_tracks$lat[indycar_tracks$track_id ==3] <- 37.7381738
indycar_tracks$lat[indycar_tracks$track_id ==4] <- 40.635898
indycar_tracks$lat[indycar_tracks$track_id ==5] <- 40.208018
indycar_tracks$lat[indycar_tracks$track_id ==6] <- 33.4681044
indycar_tracks$lat[indycar_tracks$track_id ==7] <- 32.748448
indycar_tracks$lat[indycar_tracks$track_id ==8] <- 33.864444
indycar_tracks$lat[indycar_tracks$track_id ==9] <- 33.383494
indycar_tracks$lat[indycar_tracks$track_id ==10] <- 33.7489954
indycar_tracks$lat[indycar_tracks$track_id ==11] <- 39.603333
indycar_tracks$lat[indycar_tracks$track_id ==12] <- 34.0888203
indycar_tracks$lat[indycar_tracks$track_id ==13] <- 19.406111
indycar_tracks$lat[indycar_tracks$track_id ==14] <- 30.85652
indycar_tracks$lat[indycar_tracks$track_id ==15] <- 35.292778
indycar_tracks$lat[indycar_tracks$track_id ==16] <- 39.1043406
indycar_tracks$lat[indycar_tracks$track_id ==17] <- 39.2840725
indycar_tracks$lat[indycar_tracks$track_id ==18] <- 33.5325
indycar_tracks$lat[indycar_tracks$track_id ==19] <- 42.988775
indycar_tracks$lat[indycar_tracks$track_id ==20] <- 37.543361
indycar_tracks$lat[indycar_tracks$track_id ==21] <- 40.0237408
indycar_tracks$lat[indycar_tracks$track_id ==22] <- 36.8400549
indycar_tracks$lat[indycar_tracks$track_id ==23] <- 41.003698
indycar_tracks$lat[indycar_tracks$track_id ==24] <- 46.4175532
indycar_tracks$lat[indycar_tracks$track_id ==25] <- 51.3584877
indycar_tracks$lat[indycar_tracks$track_id ==26] <- 37.940283
indycar_tracks$lat[indycar_tracks$track_id ==27] <- 36.2185234
indycar_tracks$lat[indycar_tracks$track_id ==28] <- 40.463061
indycar_tracks$lat[indycar_tracks$track_id ==29] <- 41.5115689
indycar_tracks$lat[indycar_tracks$track_id ==30] <- 36.1161685
indycar_tracks$lat[indycar_tracks$track_id ==31] <- 34.0888203
indycar_tracks$lat[indycar_tracks$track_id ==32] <- 38.5928473
indycar_tracks$lat[indycar_tracks$track_id ==33] <- 36.1519347
indycar_tracks$lat[indycar_tracks$track_id ==34] <- 42.085205
indycar_tracks$lat[indycar_tracks$track_id ==35] <- 27.0011275
indycar_tracks$lat[indycar_tracks$track_id ==36] <- 35.351547
indycar_tracks$lat[indycar_tracks$track_id ==37] <- 34.1006069
indycar_tracks$lat[indycar_tracks$track_id ==38] <- 41.4749519
indycar_tracks$lat[indycar_tracks$track_id ==39] <- 41.4749519
indycar_tracks$lat[indycar_tracks$track_id ==40] <- 39.103437
indycar_tracks$lat[indycar_tracks$track_id ==41] <- 39.0528275
indycar_tracks$lat[indycar_tracks$track_id ==42] <- 45.5032189
indycar_tracks$lat[indycar_tracks$track_id ==43] <- 50.9904979
indycar_tracks$lat[indycar_tracks$track_id ==44] <- 39.9491735
indycar_tracks$lat[indycar_tracks$track_id ==45] <- 40.2341722
indycar_tracks$lat[indycar_tracks$track_id ==46] <- 38.3293416
indycar_tracks$lat[indycar_tracks$track_id ==47] <- 41.416667
indycar_tracks$lat[indycar_tracks$track_id ==48] <- 34.0211224
indycar_tracks$lat[indycar_tracks$track_id ==49] <- 34.0129539
indycar_tracks$lat[indycar_tracks$track_id ==50] <- 34.2973206
indycar_tracks$lat[indycar_tracks$track_id ==51] <- 29.185169
indycar_tracks$lat[indycar_tracks$track_id ==52] <- 29.185169
indycar_tracks$lat[indycar_tracks$track_id ==53] <- 38.4686222
indycar_tracks$lat[indycar_tracks$track_id ==54] <- 38.9120167
indycar_tracks$lat[indycar_tracks$track_id ==55] <- 39.6464419
indycar_tracks$lat[indycar_tracks$track_id ==56] <- 39.7485109
indycar_tracks$lat[indycar_tracks$track_id ==57] <- 41.616059
indycar_tracks$lat[indycar_tracks$track_id ==58] <- 42.341102
indycar_tracks$lat[indycar_tracks$track_id ==59] <- 39.1896
indycar_tracks$lat[indycar_tracks$track_id ==60] <- 38.31982
indycar_tracks$lat[indycar_tracks$track_id ==61] <- 37.9803259
indycar_tracks$lat[indycar_tracks$track_id ==62] <- 53.5751016
indycar_tracks$lat[indycar_tracks$track_id ==63] <- 30.190003
indycar_tracks$lat[indycar_tracks$track_id ==64] <- 30.190003
indycar_tracks$lat[indycar_tracks$track_id ==65] <- -22.975556
indycar_tracks$lat[indycar_tracks$track_id ==66] <- 40.9193015
indycar_tracks$lat[indycar_tracks$track_id ==67] <- 44.4958109
indycar_tracks$lat[indycar_tracks$track_id ==68] <- 51.53456
indycar_tracks$lat[indycar_tracks$track_id ==69] <- 43.6349629
indycar_tracks$lat[indycar_tracks$track_id ==70] <- 38.661108
indycar_tracks$lat[indycar_tracks$track_id ==71] <- 40.526258
indycar_tracks$lat[indycar_tracks$track_id ==72] <- 36.720759
indycar_tracks$lat[indycar_tracks$track_id ==73] <- 40.947816
indycar_tracks$lat[indycar_tracks$track_id ==74] <- 38.6508
indycar_tracks$lat[indycar_tracks$track_id ==75] <- 38.6508
indycar_tracks$lat[indycar_tracks$track_id ==76] <- 41.5320644
indycar_tracks$lat[indycar_tracks$track_id ==77] <- 41.394722
indycar_tracks$lat[indycar_tracks$track_id ==78] <- 39.095566
indycar_tracks$lat[indycar_tracks$track_id ==79] <- 42.180312
indycar_tracks$lat[indycar_tracks$track_id ==80] <- 35.836753
indycar_tracks$lat[indycar_tracks$track_id ==81] <- 42.735908
indycar_tracks$lat[indycar_tracks$track_id ==82] <- 36.3177446
indycar_tracks$lat[indycar_tracks$track_id ==83] <- 40.386261
indycar_tracks$lat[indycar_tracks$track_id ==84] <- 35.6956952
indycar_tracks$lat[indycar_tracks$track_id ==85] <- 25.4517775
indycar_tracks$lat[indycar_tracks$track_id ==86] <- 29.684902
indycar_tracks$lat[indycar_tracks$track_id ==87] <- 40.7456005
indycar_tracks$lat[indycar_tracks$track_id ==88] <- 39.8316054
indycar_tracks$lat[indycar_tracks$track_id ==89] <- 39.8270341
indycar_tracks$lat[indycar_tracks$track_id ==90] <- 39.7953542
indycar_tracks$lat[indycar_tracks$track_id ==91] <- 39.7953542
indycar_tracks$lat[indycar_tracks$track_id ==92] <- 38.1608659
indycar_tracks$lat[indycar_tracks$track_id ==93] <- 41.674667
indycar_tracks$lat[indycar_tracks$track_id ==94] <- 33.37487
indycar_tracks$lat[indycar_tracks$track_id ==95] <- 29.682222
indycar_tracks$lat[indycar_tracks$track_id ==96] <- 29.682222
indycar_tracks$lat[indycar_tracks$track_id ==97] <- 42.2777142
indycar_tracks$lat[indycar_tracks$track_id ==98] <- 42.2777142
indycar_tracks$lat[indycar_tracks$track_id ==99] <- 39.1154928
indycar_tracks$lat[indycar_tracks$track_id ==100] <- 39.1154928
indycar_tracks$lat[indycar_tracks$track_id ==101] <- 38.912596
indycar_tracks$lat[indycar_tracks$track_id ==102] <- 38.7117649
indycar_tracks$lat[indycar_tracks$track_id ==103] <- 41.4821319
indycar_tracks$lat[indycar_tracks$track_id ==104] <- 40.1771484
indycar_tracks$lat[indycar_tracks$track_id ==105] <- 36.2722847
indycar_tracks$lat[indycar_tracks$track_id ==106] <- 36.322824
indycar_tracks$lat[indycar_tracks$track_id ==107] <- 39.7953542
indycar_tracks$lat[indycar_tracks$track_id ==108] <- 46.194114
indycar_tracks$lat[indycar_tracks$track_id ==109] <- 33.81251
indycar_tracks$lat[indycar_tracks$track_id ==110] <- 40.7368102
indycar_tracks$lat[indycar_tracks$track_id ==111] <- 34.09091
indycar_tracks$lat[indycar_tracks$track_id ==112] <- 33.9415889
indycar_tracks$lat[indycar_tracks$track_id ==113] <- 33.4592138
indycar_tracks$lat[indycar_tracks$track_id ==114] <- 35.351547
indycar_tracks$lat[indycar_tracks$track_id ==115] <- 36.6340671
indycar_tracks$lat[indycar_tracks$track_id ==116] <- 36.5842902
indycar_tracks$lat[indycar_tracks$track_id ==117] <- 40.8116428
indycar_tracks$lat[indycar_tracks$track_id ==118] <- 42.7551167
indycar_tracks$lat[indycar_tracks$track_id ==119] <- 25.76168
indycar_tracks$lat[indycar_tracks$track_id ==120] <- 25.7859438
indycar_tracks$lat[indycar_tracks$track_id ==121] <- 42.0674728
indycar_tracks$lat[indycar_tracks$track_id ==122] <- 42.489253
indycar_tracks$lat[indycar_tracks$track_id ==123] <- 40.6906568
indycar_tracks$lat[indycar_tracks$track_id ==124] <- 44.981921
indycar_tracks$lat[indycar_tracks$track_id ==125] <- 41.9319414
indycar_tracks$lat[indycar_tracks$track_id ==126] <- 40.8522006
indycar_tracks$lat[indycar_tracks$track_id ==127] <- 44.054188
indycar_tracks$lat[indycar_tracks$track_id ==128] <- 41.399578
indycar_tracks$lat[indycar_tracks$track_id ==129] <- 36.046245
indycar_tracks$lat[indycar_tracks$track_id ==130] <- 40.727941
indycar_tracks$lat[indycar_tracks$track_id ==131] <- 43.36268
indycar_tracks$lat[indycar_tracks$track_id ==132] <- 43.073217
indycar_tracks$lat[indycar_tracks$track_id ==133] <- 29.893235
indycar_tracks$lat[indycar_tracks$track_id ==134] <- 36.1845626
indycar_tracks$lat[indycar_tracks$track_id ==135] <- 35.31017
indycar_tracks$lat[indycar_tracks$track_id ==136] <- 40.9800601
indycar_tracks$lat[indycar_tracks$track_id ==137] <- 34.0748043
indycar_tracks$lat[indycar_tracks$track_id ==138] <- 30.2815833
indycar_tracks$lat[indycar_tracks$track_id ==139] <- 47.320672
indycar_tracks$lat[indycar_tracks$track_id ==140] <- 41.2477981
indycar_tracks$lat[indycar_tracks$track_id ==141] <- 37.804667
indycar_tracks$lat[indycar_tracks$track_id ==142] <- 25.678056
indycar_tracks$lat[indycar_tracks$track_id ==143] <- 39.748611
indycar_tracks$lat[indycar_tracks$track_id ==144] <- 33.37487
indycar_tracks$lat[indycar_tracks$track_id ==145] <- 38.8402619
indycar_tracks$lat[indycar_tracks$track_id ==146] <- 38.591059
indycar_tracks$lat[indycar_tracks$track_id ==147] <- 40.3561805
indycar_tracks$lat[indycar_tracks$track_id ==148] <- 41.054344
indycar_tracks$lat[indycar_tracks$track_id ==149] <- 32.70003
indycar_tracks$lat[indycar_tracks$track_id ==150] <- 40.5356431
indycar_tracks$lat[indycar_tracks$track_id ==151] <- 45.5930851
indycar_tracks$lat[indycar_tracks$track_id ==152] <- 45.4812455
indycar_tracks$lat[indycar_tracks$track_id ==153] <- 37.601689
indycar_tracks$lat[indycar_tracks$track_id ==154] <- 36.9182469
indycar_tracks$lat[indycar_tracks$track_id ==155] <- 40.3820267
indycar_tracks$lat[indycar_tracks$track_id ==156] <- 42.2384918
indycar_tracks$lat[indycar_tracks$track_id ==157] <- 37.328056
indycar_tracks$lat[indycar_tracks$track_id ==158] <- 29.6847219
indycar_tracks$lat[indycar_tracks$track_id ==159] <- 44.0541878
indycar_tracks$lat[indycar_tracks$track_id ==160] <- 53.575556
indycar_tracks$lat[indycar_tracks$track_id ==161] <- 37.592392
indycar_tracks$lat[indycar_tracks$track_id ==162] <- 37.592392
indycar_tracks$lat[indycar_tracks$track_id ==163] <- 40.922509
indycar_tracks$lat[indycar_tracks$track_id ==164] <- 35.143752
indycar_tracks$lat[indycar_tracks$track_id ==165] <- 43.805456
indycar_tracks$lat[indycar_tracks$track_id ==166] <- 36.234764
indycar_tracks$lat[indycar_tracks$track_id ==167] <- 34.974181
indycar_tracks$lat[indycar_tracks$track_id ==168] <- 40.7424482
indycar_tracks$lat[indycar_tracks$track_id ==169] <- 32.8819532
indycar_tracks$lat[indycar_tracks$track_id ==170] <- 37.7799643
indycar_tracks$lat[indycar_tracks$track_id ==171] <- 37.7749295
indycar_tracks$lat[indycar_tracks$track_id ==172] <- 37.303261
indycar_tracks$lat[indycar_tracks$track_id ==173] <- 37.304499
indycar_tracks$lat[indycar_tracks$track_id ==174] <- 45.528904
indycar_tracks$lat[indycar_tracks$track_id ==175] <- 34.0194543
indycar_tracks$lat[indycar_tracks$track_id ==176] <- 33.8071745
indycar_tracks$lat[indycar_tracks$track_id ==177] <- -23.516389
indycar_tracks$lat[indycar_tracks$track_id ==178] <- 31.9922666
indycar_tracks$lat[indycar_tracks$track_id ==179] <- 38.16006
indycar_tracks$lat[indycar_tracks$track_id ==180] <- 47.320672
indycar_tracks$lat[indycar_tracks$track_id ==181] <- 40.787222
indycar_tracks$lat[indycar_tracks$track_id ==182] <- 40.8654975
indycar_tracks$lat[indycar_tracks$track_id ==183] <- 41.665859
indycar_tracks$lat[indycar_tracks$track_id ==184] <- 52.078611
indycar_tracks$lat[indycar_tracks$track_id ==185] <- 42.573381
indycar_tracks$lat[indycar_tracks$track_id ==186] <- 44.77755
indycar_tracks$lat[indycar_tracks$track_id ==187] <- 35.8275
indycar_tracks$lat[indycar_tracks$track_id ==188] <- 30.301316
indycar_tracks$lat[indycar_tracks$track_id ==189] <- 27.77084
indycar_tracks$lat[indycar_tracks$track_id ==190] <- 27.77084
indycar_tracks$lat[indycar_tracks$track_id ==191] <- 36.107778
indycar_tracks$lat[indycar_tracks$track_id ==192] <- 38.696238
indycar_tracks$lat[indycar_tracks$track_id ==193] <- -27.9863581
indycar_tracks$lat[indycar_tracks$track_id ==194] <- -27.9863581
indycar_tracks$lat[indycar_tracks$track_id ==195] <- 33.8858978
indycar_tracks$lat[indycar_tracks$track_id ==196] <- 25.7498405
indycar_tracks$lat[indycar_tracks$track_id ==197] <- 30.5379229
indycar_tracks$lat[indycar_tracks$track_id ==198] <- 30.5379229
indycar_tracks$lat[indycar_tracks$track_id ==199] <- 43.0204523
indycar_tracks$lat[indycar_tracks$track_id ==200] <- 42.341102
indycar_tracks$lat[indycar_tracks$track_id ==201] <- 41.9814297
indycar_tracks$lat[indycar_tracks$track_id ==202] <- 39.1154928
indycar_tracks$lat[indycar_tracks$track_id ==203] <- 39.1154928
indycar_tracks$lat[indycar_tracks$track_id ==204] <- 40.815014
indycar_tracks$lat[indycar_tracks$track_id ==205] <- 45.3242824
indycar_tracks$lat[indycar_tracks$track_id ==206] <- 52.9583015
indycar_tracks$lat[indycar_tracks$track_id ==207] <- 32.0362718
indycar_tracks$lat[indycar_tracks$track_id ==208] <- 32.3656626
indycar_tracks$lat[indycar_tracks$track_id ==209] <- 36.531954
indycar_tracks$lat[indycar_tracks$track_id ==210] <- 39.877222
indycar_tracks$lat[indycar_tracks$track_id ==211] <- 45.4408474
indycar_tracks$lat[indycar_tracks$track_id ==212] <- 43.596432
indycar_tracks$lat[indycar_tracks$track_id ==213] <- 28.3935911
indycar_tracks$lat[indycar_tracks$track_id ==214] <- 42.3382811
indycar_tracks$lat[indycar_tracks$track_id ==215] <- 43.0494572
indycar_tracks$lat[indycar_tracks$track_id ==216] <- 40.155193
indycar_tracks$lat[indycar_tracks$track_id ==217] <- 40.1740028
indycar_tracks$lat[indycar_tracks$track_id ==218] <- 36.1247721
indycar_tracks$lon[indycar_tracks$track_id ==1] <- -81.5190053
indycar_tracks$lon[indycar_tracks$track_id ==2] <- -75.49625
indycar_tracks$lon[indycar_tracks$track_id ==3] <- -121.5631377
indycar_tracks$lon[indycar_tracks$track_id ==4] <- -78.295847
indycar_tracks$lon[indycar_tracks$track_id ==5] <- -80.257919
indycar_tracks$lon[indycar_tracks$track_id ==6] <- -112.0976042
indycar_tracks$lon[indycar_tracks$track_id ==7] <- -97.069558
indycar_tracks$lon[indycar_tracks$track_id ==8] <- -118.289167
indycar_tracks$lon[indycar_tracks$track_id ==9] <- -84.317856
indycar_tracks$lon[indycar_tracks$track_id ==10] <- -84.3879824
indycar_tracks$lon[indycar_tracks$track_id ==11] <- -74.741111
indycar_tracks$lon[indycar_tracks$track_id ==12] <- -117.5005002
indycar_tracks$lon[indycar_tracks$track_id ==13] <- -99.0925
indycar_tracks$lon[indycar_tracks$track_id ==14] <- -83.9397796
indycar_tracks$lon[indycar_tracks$track_id ==15] <- -119.256389
indycar_tracks$lon[indycar_tracks$track_id ==16] <- -76.7906478
indycar_tracks$lon[indycar_tracks$track_id ==17] <- -76.6202094
indycar_tracks$lon[indycar_tracks$track_id ==18] <- -86.618889
indycar_tracks$lon[indycar_tracks$track_id ==19] <- -78.146584
indycar_tracks$lon[indycar_tracks$track_id ==20] <- -122.297739
indycar_tracks$lon[indycar_tracks$track_id ==21] <- -78.5204672
indycar_tracks$lon[indycar_tracks$track_id ==22] <- -94.3436686
indycar_tracks$lon[indycar_tracks$track_id ==23] <- -76.4549457
indycar_tracks$lon[indycar_tracks$track_id ==24] <- -94.2849789
indycar_tracks$lon[indycar_tracks$track_id ==25] <- 0.2524683
indycar_tracks$lon[indycar_tracks$track_id ==26] <- -93.397678
indycar_tracks$lon[indycar_tracks$track_id ==27] <- -96.5905982
indycar_tracks$lon[indycar_tracks$track_id ==28] <- -80.043331
indycar_tracks$lon[indycar_tracks$track_id ==29] <- -81.6899155
indycar_tracks$lon[indycar_tracks$track_id ==30] <- -115.174499
indycar_tracks$lon[indycar_tracks$track_id ==31] <- -117.5005002
indycar_tracks$lon[indycar_tracks$track_id ==32] <- -121.4376522
indycar_tracks$lon[indycar_tracks$track_id ==33] <- -95.977805
indycar_tracks$lon[indycar_tracks$track_id ==34] <- -76.278512
indycar_tracks$lon[indycar_tracks$track_id ==35] <- -82.1844286
indycar_tracks$lon[indycar_tracks$track_id ==36] <- -80.686551
indycar_tracks$lon[indycar_tracks$track_id ==37] <- -117.858353
indycar_tracks$lon[indycar_tracks$track_id ==38] <- -88.0573003
indycar_tracks$lon[indycar_tracks$track_id ==39] <- -88.0573003
indycar_tracks$lon[indycar_tracks$track_id ==40] <- -84.511479
indycar_tracks$lon[indycar_tracks$track_id ==41] <- -84.4114585
indycar_tracks$lon[indycar_tracks$track_id ==42] <- -73.5266876
indycar_tracks$lon[indycar_tracks$track_id ==43] <- 5.257984
indycar_tracks$lon[indycar_tracks$track_id ==44] <- -82.9528444
indycar_tracks$lon[indycar_tracks$track_id ==45] <- -104.978021
indycar_tracks$lon[indycar_tracks$track_id ==46] <- -122.7096666
indycar_tracks$lon[indycar_tracks$track_id ==47] <- -87.366667
indycar_tracks$lon[indycar_tracks$track_id ==48] <- -118.3964665
indycar_tracks$lon[indycar_tracks$track_id ==49] <- -118.4025246
indycar_tracks$lon[indycar_tracks$track_id ==50] <- -79.9051516
indycar_tracks$lon[indycar_tracks$track_id ==51] <- -81.070528
indycar_tracks$lon[indycar_tracks$track_id ==52] <- -81.070528
indycar_tracks$lon[indycar_tracks$track_id ==53] <- -75.5609124
indycar_tracks$lon[indycar_tracks$track_id ==54] <- -75.5732462
indycar_tracks$lon[indycar_tracks$track_id ==55] <- -104.8472861
indycar_tracks$lon[indycar_tracks$track_id ==56] <- -104.990479
indycar_tracks$lon[indycar_tracks$track_id ==57] <- -93.619447
indycar_tracks$lon[indycar_tracks$track_id ==58] <- -82.979763
indycar_tracks$lon[indycar_tracks$track_id ==59] <- -75.53031
indycar_tracks$lon[indycar_tracks$track_id ==60] <- -86.876421
indycar_tracks$lon[indycar_tracks$track_id ==61] <- -89.2254248
indycar_tracks$lon[indycar_tracks$track_id ==62] <- -113.5186931
indycar_tracks$lon[indycar_tracks$track_id ==63] <- -97.064568
indycar_tracks$lon[indycar_tracks$track_id ==64] <- -97.064568
indycar_tracks$lon[indycar_tracks$track_id ==65] <- -43.395
indycar_tracks$lon[indycar_tracks$track_id ==66] <- -73.8645107
indycar_tracks$lon[indycar_tracks$track_id ==67] <- -73.1224836
indycar_tracks$lon[indycar_tracks$track_id ==68] <- 13.92844
indycar_tracks$lon[indycar_tracks$track_id ==69] <- -79.4126855
indycar_tracks$lon[indycar_tracks$track_id ==70] <- -90.036304
indycar_tracks$lon[indycar_tracks$track_id ==71] <- -74.853563
indycar_tracks$lon[indycar_tracks$track_id ==72] <- -119.694891
indycar_tracks$lon[indycar_tracks$track_id ==73] <- -90.37124
indycar_tracks$lon[indycar_tracks$track_id ==74] <- -90.13537
indycar_tracks$lon[indycar_tracks$track_id ==75] <- -90.13537
indycar_tracks$lon[indycar_tracks$track_id ==76] <- -81.6172083
indycar_tracks$lon[indycar_tracks$track_id ==77] <- -74.326944
indycar_tracks$lon[indycar_tracks$track_id ==78] <- -94.581135
indycar_tracks$lon[indycar_tracks$track_id ==79] <- -73.363349
indycar_tracks$lon[indycar_tracks$track_id ==80] <- -80.23671
indycar_tracks$lon[indycar_tracks$track_id ==81] <- -78.818399
indycar_tracks$lon[indycar_tracks$track_id ==82] <- -119.6354027
indycar_tracks$lon[indycar_tracks$track_id ==83] <- -80.0948331
indycar_tracks$lon[indycar_tracks$track_id ==84] <- -81.2691817
indycar_tracks$lon[indycar_tracks$track_id ==85] <- -80.4088955
indycar_tracks$lon[indycar_tracks$track_id ==86] <- -95.410734
indycar_tracks$lon[indycar_tracks$track_id ==87] <- -74.0086726
indycar_tracks$lon[indycar_tracks$track_id ==88] <- -89.6406375
indycar_tracks$lon[indycar_tracks$track_id ==89] <- -86.1362195
indycar_tracks$lon[indycar_tracks$track_id ==90] <- -86.2353006
indycar_tracks$lon[indycar_tracks$track_id ==91] <- -86.2353006
indycar_tracks$lon[indycar_tracks$track_id ==92] <- -122.4546942
indycar_tracks$lon[indycar_tracks$track_id ==93] <- -93.012968
indycar_tracks$lon[indycar_tracks$track_id ==94] <- -112.311221
indycar_tracks$lon[indycar_tracks$track_id ==95] <- -95.408611
indycar_tracks$lon[indycar_tracks$track_id ==96] <- -95.408611
indycar_tracks$lon[indycar_tracks$track_id ==97] <- -85.5436823
indycar_tracks$lon[indycar_tracks$track_id ==98] <- -85.5436823
indycar_tracks$lon[indycar_tracks$track_id ==99] <- -94.8306604
indycar_tracks$lon[indycar_tracks$track_id ==100] <- -94.8306604
indycar_tracks$lon[indycar_tracks$track_id ==101] <- -76.9418223
indycar_tracks$lon[indycar_tracks$track_id ==102] <- -84.9106434
indycar_tracks$lon[indycar_tracks$track_id ==103] <- -81.799481
indycar_tracks$lon[indycar_tracks$track_id ==104] <- -74.8827621
indycar_tracks$lon[indycar_tracks$track_id ==105] <- -115.0102761
indycar_tracks$lon[indycar_tracks$track_id ==106] <- -115.267957
indycar_tracks$lon[indycar_tracks$track_id ==107] <- -86.2353006
indycar_tracks$lon[indycar_tracks$track_id ==108] <- -74.6106252
indycar_tracks$lon[indycar_tracks$track_id ==109] <- -118.193363
indycar_tracks$lon[indycar_tracks$track_id ==110] <- -73.7549567
indycar_tracks$lon[indycar_tracks$track_id ==111] <- -117.885342
indycar_tracks$lon[indycar_tracks$track_id ==112] <- -118.40853
indycar_tracks$lon[indycar_tracks$track_id ==113] <- -112.0649224
indycar_tracks$lon[indycar_tracks$track_id ==114] <- -80.686551
indycar_tracks$lon[indycar_tracks$track_id ==115] <- -79.8516593
indycar_tracks$lon[indycar_tracks$track_id ==116] <- -121.7534634
indycar_tracks$lon[indycar_tracks$track_id ==117] <- -74.0677451
indycar_tracks$lon[indycar_tracks$track_id ==118] <- -71.1726868
indycar_tracks$lon[indycar_tracks$track_id ==119] <- -80.19179
indycar_tracks$lon[indycar_tracks$track_id ==120] <- -80.2169685
indycar_tracks$lon[indycar_tracks$track_id ==121] <- -84.2448364
indycar_tracks$lon[indycar_tracks$track_id ==122] <- -83.502561
indycar_tracks$lon[indycar_tracks$track_id ==123] <- -82.6375867
indycar_tracks$lon[indycar_tracks$track_id ==124] <- -93.1731168
indycar_tracks$lon[indycar_tracks$track_id ==125] <- -83.4619101
indycar_tracks$lon[indycar_tracks$track_id ==126] <- -73.8507279
indycar_tracks$lon[indycar_tracks$track_id ==127] <- -78.675365
indycar_tracks$lon[indycar_tracks$track_id ==128] <- -71.476763
indycar_tracks$lon[indycar_tracks$track_id ==129] <- -86.41041
indycar_tracks$lon[indycar_tracks$track_id ==130] <- -75.319834
indycar_tracks$lon[indycar_tracks$track_id ==131] <- -71.460573
indycar_tracks$lon[indycar_tracks$track_id ==132] <- -76.224743
indycar_tracks$lon[indycar_tracks$track_id ==133] <- -90.189828
indycar_tracks$lon[indycar_tracks$track_id ==134] <- -94.1245715
indycar_tracks$lon[indycar_tracks$track_id ==135] <- -97.593345
indycar_tracks$lon[indycar_tracks$track_id ==136] <- -96.395384
indycar_tracks$lon[indycar_tracks$track_id ==137] <- -117.5808784
indycar_tracks$lon[indycar_tracks$track_id ==138] <- -81.3895371
indycar_tracks$lon[indycar_tracks$track_id ==139] <- -122.147423
indycar_tracks$lon[indycar_tracks$track_id ==140] <- -96.0743036
indycar_tracks$lon[indycar_tracks$track_id ==141] <- -122.446667
indycar_tracks$lon[indycar_tracks$track_id ==142] <- -100.283333
indycar_tracks$lon[indycar_tracks$track_id ==143] <- -105.0075
indycar_tracks$lon[indycar_tracks$track_id ==144] <- -112.311221
indycar_tracks$lon[indycar_tracks$track_id ==145] <- -105.0441348
indycar_tracks$lon[indycar_tracks$track_id ==146] <- -104.677175
indycar_tracks$lon[indycar_tracks$track_id ==147] <- -80.1100556
indycar_tracks$lon[indycar_tracks$track_id ==148] <- -75.511279
indycar_tracks$lon[indycar_tracks$track_id ==149] <- -117.2466838
indycar_tracks$lon[indycar_tracks$track_id ==150] <- -77.3894655
indycar_tracks$lon[indycar_tracks$track_id ==151] <- -122.6881306
indycar_tracks$lon[indycar_tracks$track_id ==152] <- -122.7431999
indycar_tracks$lon[indycar_tracks$track_id ==153] <- -121.719546
indycar_tracks$lon[indycar_tracks$track_id ==154] <- -93.8986069
indycar_tracks$lon[indycar_tracks$track_id ==155] <- -76.0088806
indycar_tracks$lon[indycar_tracks$track_id ==156] <- -71.1299976
indycar_tracks$lon[indycar_tracks$track_id ==157] <- -121.890556
indycar_tracks$lon[indycar_tracks$track_id ==158] <- -95.4107074
indycar_tracks$lon[indycar_tracks$track_id ==159] <- -78.675365
indycar_tracks$lon[indycar_tracks$track_id ==160] <- -113.522917
indycar_tracks$lon[indycar_tracks$track_id ==161] <- -77.419459
indycar_tracks$lon[indycar_tracks$track_id ==162] <- -77.419459
indycar_tracks$lon[indycar_tracks$track_id ==163] <- -72.7045085
indycar_tracks$lon[indycar_tracks$track_id ==164] <- -90.134591
indycar_tracks$lon[indycar_tracks$track_id ==165] <- -87.991723
indycar_tracks$lon[indycar_tracks$track_id ==166] <- -81.62963
indycar_tracks$lon[indycar_tracks$track_id ==167] <- -79.610419
indycar_tracks$lon[indycar_tracks$track_id ==168] <- -73.5963609
indycar_tracks$lon[indycar_tracks$track_id ==169] <- -117.1647586
indycar_tracks$lon[indycar_tracks$track_id ==170] <- -122.3983805
indycar_tracks$lon[indycar_tracks$track_id ==171] <- -122.4194155
indycar_tracks$lon[indycar_tracks$track_id ==172] <- -121.853842
indycar_tracks$lon[indycar_tracks$track_id ==173] <- -121.876774
indycar_tracks$lon[indycar_tracks$track_id ==174] <- -72.881827
indycar_tracks$lon[indycar_tracks$track_id ==175] <- -118.4911912
indycar_tracks$lon[indycar_tracks$track_id ==176] <- -118.0438396
indycar_tracks$lon[indycar_tracks$track_id ==177] <- -46.647222
indycar_tracks$lon[indycar_tracks$track_id ==178] <- -81.0878724
indycar_tracks$lon[indycar_tracks$track_id ==179] <- -122.4594
indycar_tracks$lon[indycar_tracks$track_id ==180] <- -122.147423
indycar_tracks$lon[indycar_tracks$track_id ==181] <- -76.870278
indycar_tracks$lon[indycar_tracks$track_id ==182] <- -73.5327997
indycar_tracks$lon[indycar_tracks$track_id ==183] <- -95.313349
indycar_tracks$lon[indycar_tracks$track_id ==184] <- -1.016944
indycar_tracks$lon[indycar_tracks$track_id ==185] <- -96.523164
indycar_tracks$lon[indycar_tracks$track_id ==186] <- -69.729093
indycar_tracks$lon[indycar_tracks$track_id ==187] <- -78.610556
indycar_tracks$lon[indycar_tracks$track_id ==188] <- -81.745083
indycar_tracks$lon[indycar_tracks$track_id ==189] <- -82.6335623
indycar_tracks$lon[indycar_tracks$track_id ==190] <- -82.6335623
indycar_tracks$lon[indycar_tracks$track_id ==191] <- -115.2525
indycar_tracks$lon[indycar_tracks$track_id ==192] <- -93.26247
indycar_tracks$lon[indycar_tracks$track_id ==193] <- 153.4274036
indycar_tracks$lon[indycar_tracks$track_id ==194] <- 153.4274036
indycar_tracks$lon[indycar_tracks$track_id ==195] <- -117.5085143
indycar_tracks$lon[indycar_tracks$track_id ==196] <- -80.3754417
indycar_tracks$lon[indycar_tracks$track_id ==197] <- -96.2190137
indycar_tracks$lon[indycar_tracks$track_id ==198] <- -96.2190137
indycar_tracks$lon[indycar_tracks$track_id ==199] <- -88.0129032
indycar_tracks$lon[indycar_tracks$track_id ==200] <- -82.979763
indycar_tracks$lon[indycar_tracks$track_id ==201] <- -71.8327764
indycar_tracks$lon[indycar_tracks$track_id ==202] <- -94.8306604
indycar_tracks$lon[indycar_tracks$track_id ==203] <- -94.8306604
indycar_tracks$lon[indycar_tracks$track_id ==204] <- -91.175861
indycar_tracks$lon[indycar_tracks$track_id ==205] <- -75.777762
indycar_tracks$lon[indycar_tracks$track_id ==206] <- 6.522342
indycar_tracks$lon[indycar_tracks$track_id ==207] <- -110.7949208
indycar_tracks$lon[indycar_tracks$track_id ==208] <- -92.0744194
indycar_tracks$lon[indycar_tracks$track_id ==209] <- 140.2253966
indycar_tracks$lon[indycar_tracks$track_id ==210] <- -79.710833
indycar_tracks$lon[indycar_tracks$track_id ==211] <- 12.3155151
indycar_tracks$lon[indycar_tracks$track_id ==212] <- -72.970345
indycar_tracks$lon[indycar_tracks$track_id ==213] <- -81.574866
indycar_tracks$lon[indycar_tracks$track_id ==214] <- -76.9263577
indycar_tracks$lon[indycar_tracks$track_id ==215] <- -88.0075875
indycar_tracks$lon[indycar_tracks$track_id ==216] <- -77.033423
indycar_tracks$lon[indycar_tracks$track_id ==217] <- -85.0268764
indycar_tracks$lon[indycar_tracks$track_id ==218] <- -80.2548705

indycar_races <- merge(indycar_races, indycar_tracks, by = "track")





