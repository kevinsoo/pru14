#######################################################
# Scrape GE 14 election results from https://undi.info/
# Author: Kevin Soo
#######################################################

#### Load packages
library(xml2)
library(rvest)
library(tidyverse)
library(tools)
library(ggthemes)

#### Load coordinates of seats
locations <- read_csv("Data/seat_locations.csv") %>%
    select(State = Negeri,
           Parliament = X3,
           DUN = X4,
           lat = X11,
           long = X12) %>%
    mutate(State = toTitleCase(tolower(State)),
           Parliament = toTitleCase(tolower(Parliament)),
           DUN = toTitleCase(tolower(DUN))) %>%
    select(State, Parliament, DUN, lat, long) %>%
    filter(State != "Singapore")

#### How many DUNs in each seat?
dun_counts <- locations %>% 
    group_by(State, Parliament) %>% 
    count()

#### How many parliament-level seats in each state?
parliament_counts <- dun_counts %>%
    select(-n) %>%
    group_by(State) %>%
    count()

#### Scrape parliament-level results (past two elections)
for (i in 1:sum(parliament_counts$n)) {
    # Page
    page <- read_html(paste0("https://undi.info/embed/?", tolower("P"), i))
    
    # Scrape page info
    scrapeDetails <- page %>%
        html_nodes("#mdl-2013~ .mdl-card .vt-mt span:nth-child(1) , .vt-mt~ .vt-mt+ .vt-mt span:nth-child(3) , .vt-mt~ .vt-mt+ .vt-mt span:nth-child(2) , #mdl-2013~ .mdl-card .vt-mt span:nth-child(3) , #mdl-2013~ .mdl-card span:nth-child(2) , .vt-mt~ .vt-mt+ .vt-mt span:nth-child(1) , #mdl-2013+ label .yrtotal , #mdl-2018+ label .yrtotal , .modal-seat-flag p , h5") %>%
        html_text() %>%
        str_trim()

    # Temporary string for parliament
    par <- str_remove(scrapeDetails[1], " \\*")
    par <- str_split(par, "\t")[[1]]
    par <- par[par != ""][2]
    
    # Get details of seat
    tmp <- tibble(Year = c(2018, 2013),
                  State = scrapeDetails[2],
                  Parliament = par,
                  ParliamentCode = paste0("P", i),
                  Registered = c(parse_number(scrapeDetails[3]), parse_number(scrapeDetails[7])),
                  Malay = c(parse_number(scrapeDetails[4]), parse_number(scrapeDetails[8]))/100,
                  Chinese = c(parse_number(scrapeDetails[5]), parse_number(scrapeDetails[9]))/100,
                  Indian = c(parse_number(scrapeDetails[6]), parse_number(scrapeDetails[10]))/100)
    
    #### Scrape 2018 results
    scrape2018 <- page %>%
        html_nodes("#mdl-2018~ .mdl-card .vote-figures , #mdl-2018~ .mdl-card .candidate-name , #mdl-2018~ .mdl-card .party-name") %>%
        html_text() %>%
        str_trim()
    # How many candidates?
    n2018 <- length(scrape2018)/3
    # Results tibble
    results2018 <- tibble(Year = 2018,
                          Party = str_to_upper(scrape2018[seq(1, length(scrape2018), 3)]),
                          Candidate = str_to_title(scrape2018[seq(2, length(scrape2018), 3)]),
                      Votes = parse_number(scrape2018[seq(3, length(scrape2018), 3)]))

    #### Scrape 2013 results
    scrape2013 <- page %>%
        html_nodes("#mdl-2013~ .mdl-card .vote-figures , #mdl-2013~ .mdl-card .candidate-name , #mdl-2013~ .mdl-card .party-name") %>%
        html_text() %>%
        str_trim()
    # How many candidates?
    n2013 <- length(scrape2013)/3
    # Results tibble
    results2013 <- tibble(Year = 2013,
                          Party = str_to_upper(scrape2013[seq(1, length(scrape2013), 3)]),
                          Candidate = str_to_title(scrape2013[seq(2, length(scrape2013), 3)]),
                          Votes = parse_number(scrape2013[seq(3, length(scrape2013), 3)]))
        
    # Combine with details
    tmp2018 <- tmp %>%
        filter(Year == 2018) %>%
        inner_join(results2018)
    tmp2013 <- tmp %>%
        filter(Year == 2013) %>%
        inner_join(results2013)
    
    # Combine all
    parlimen <- tmp2018 %>%
        bind_rows(tmp2013)
    
    # Save
    if (i == 1) { parlimenResults <- parlimen }
    else { parlimenResults <- bind_rows(parlimenResults, parlimen) }
    
    # Progress
    print(i)
}

#### Clean up race breakdown

# Don't use race breakdown for Sabah/Sarawak
parlimenResults <- parlimenResults %>%
    mutate(Malay = ifelse((State == "SW") | (State == "SB"), NA, Malay),
           Chinese = ifelse((State == "SW") | (State == "SB"), NA, Chinese),
           Indian = ifelse((State == "SW") | (State == "SB"), NA, Indian))

# For some seats, Indian/Chinese count is rounded down to 0
parlimenResults <- parlimenResults %>%
    mutate(Chinese = ifelse(Chinese > 1, 0, Chinese),
           Indian = ifelse(Indian > 1, 0, Indian))

#### Add ID
parlimenResults <- parlimenResults %>%
    mutate(ID = parse_number(ParliamentCode)) %>%
    select(ID, everything())

#### Save data
save(parlimenResults, file = "Data/parlimenResults.Rda")
