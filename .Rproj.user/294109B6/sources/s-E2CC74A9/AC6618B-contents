#######################################################
# Clean GE 14 election results from https://undi.info/
# Author: Kevin Soo
#######################################################

#### Load packages
library(tidyverse)
library(tools)
library(ggthemes)

#### Load data
load(file = "Data/parlimenResults.Rda")
# write_csv(parlimenResults, "Data/parlimenResults.csv")

#### Recode states
parlimenResults <- parlimenResults %>%
    mutate(State = ifelse(State == "PL", "Perlis",
                          ifelse(State == "KD", "Kedah",
                                 ifelse(State == "KE", "Kelantan",
                                        ifelse(State == "TR", "Terengganu",
                                               ifelse(State == "PN", "Pulau Pinang",
                                                      ifelse(State == "PR", "Perak",
                                                             ifelse(State == "PH", "Pahang",
                                                                    ifelse(State == "SL", "Selangor",
                                                                           ifelse(State == "WP", "Wilayah Persekutuan",
                                                                                  ifelse(State == "NS", "Negeri Sembilan", 
                                                                                         ifelse(State == "MK", "Melaka",
                                                                                                ifelse(State == "JH", "Johor",
                                                                                                       ifelse(State == "SB", "Sabah",
                                                                                                              ifelse(State == "SW", "Sarawak", State)))))))))))))))

#### Code coalitions
parlimenResults$Coalition <- ifelse(parlimenResults$Party %in% c("UMNO", "MCA", "MIC", "PBB", "SUPP", "GERAKAN", "MYPPP", "PBS", "PDP", "PRS"), "BN", parlimenResults$Party)
parlimenResults$Coalition <- ifelse(parlimenResults$Party == "IND", "IND", parlimenResults$Coalition)

for (i in 1:nrow(parlimenResults)) {
    # GE13
    if (parlimenResults$Year[i] == 2013) {
        parlimenResults$Coalition[i] <- ifelse(parlimenResults$Party[i] %in% c("PKR", "DAP", "PAS", "WARISAN"), "Pakatan", parlimenResults$Coalition[i])
    }
    # GE14
    else if (parlimenResults$Year[i] == 2018) {
        parlimenResults$Coalition[i] <- ifelse(parlimenResults$Party[i] %in% c("PKR", "DAP", "AMANAH", "BERSATU", "WARISAN"), "Pakatan", parlimenResults$Coalition[i])
    }
}

#### Compute statistics

# Total votes cast, turnout, vote share, winner and majority
parlimenResults <- parlimenResults %>%
    group_by(Year, ParliamentCode) %>%
    mutate(Total = sum(Votes),
           Turnout = Total/Registered) %>%
    ungroup() %>%
    mutate(VoteShare = Votes/Total) %>%
    group_by(Year, ParliamentCode) %>%
    mutate(Winner = ifelse(VoteShare == max(VoteShare), 1, 0),
           Majority = ifelse(Winner == 1, max(Votes) - sort(Votes, decreasing = TRUE)[2], Votes - max(Votes)),
           Margin = Majority/Total,
           SpoilerPAS = ifelse((Year == 2018) & (Coalition %in% c("PAS")), 1, 0),
           SpoilerPAS = ifelse(length(unique(SpoilerPAS)) > 1, 1, 0)) %>%
    ungroup()

# BEBAS winners aligned with PH
parlimenResults <- parlimenResults %>%
    mutate(Coalition = ifelse((Party == "BEBAS") & (Winner == 1), "Pakatan", Coalition))

# Change statistics
changeStats <- parlimenResults %>%
    group_by(ID, Year, State, Parliament, ParliamentCode, Malay, Chinese, Registered, Total, Turnout) %>%
    count() %>%
    arrange(ID, Year) %>%
    ungroup() %>%
    mutate(ChangeMalay = Malay - lag(Malay),
           ChangeMalay = ifelse(Year == 2013, NA, ChangeMalay),
           ChangeChinese = Chinese - lag(Chinese),
           ChangeChinese = ifelse(Year == 2013, NA, ChangeChinese),
           ChangeRegistered = Registered - lag(Registered),
           ChangeRegistered = ifelse(Year == 2013, NA, ChangeRegistered),
           ChangeTotal = Total - lag(Total),
           ChangeTotal = ifelse(Year == 2013, NA, ChangeTotal),
           ChangeTurnout = Turnout - lag(Turnout),
           ChangeTurnout = ifelse(Year == 2013, NA, ChangeTurnout))

# Combine stats and organize
parlimenResults <- parlimenResults %>%
    left_join(changeStats) %>%
    arrange(ID, Coalition, Year) %>%
    mutate(ChangeMajority = ifelse((Year == 2018) & (Coalition %in% c("BN", "Pakatan")), Majority - lag(Majority), NA),
           ChangeMajority = ifelse((Year == 2018) & (Coalition == "PAS") & (Winner == 1), Majority, ChangeMajority),
           ChangeMargin = ifelse((Year == 2018) & (Coalition %in% c("BN", "Pakatan")), Margin - lag(Margin), NA),
           ChangeMargin = ifelse((Year == 2018) & (Coalition == "PAS") & (Winner == 1), Margin, ChangeMargin),
           ChangeVoteShare = ifelse((Year == 2018) & (Coalition %in% c("BN", "Pakatan")), VoteShare - lag(VoteShare), NA),
           ChangeVoteShare = ifelse((Year == 2018) & (Coalition == "PAS") & (Winner == 1), VoteShare, ChangeVoteShare),
           ChangeWinner = ifelse((Year == 2018) & (Coalition %in% c("BN", "Pakatan")), Winner - lag(Winner), NA),
           ChangeWinner = ifelse((Year == 2018) & (Coalition == "PAS") & (Winner == 1), 1, ChangeWinner)) %>%
    select(ID, State, ParliamentCode, Parliament, Malay:Indian, Registered, Total, Turnout, 
           Year, Coalition, Party, Candidate, SpoilerPAS, Winner, Votes, VoteShare, Majority, Margin,
           ChangeMalay, ChangeChinese, ChangeRegistered, ChangeTotal, ChangeTurnout,
           ChangeWinner, ChangeMajority, ChangeMargin, ChangeVoteShare)

# Compute wasted votes
wasted <- parlimenResults %>%
    group_by(ID, Year) %>%
    arrange(ID, Year) %>%
    mutate(Parties = max(row_number())) %>%
    select(ID, Year, Coalition, Party, Candidate, Winner, Parties, Votes, Majority, Total) %>%
    arrange(ID, Year, -Votes) %>%
    mutate(Rank = row_number()) %>%
    mutate(WastedVotes = ifelse(Winner == 0, Votes,
                                ifelse(Winner == 1, Votes - (Total/Parties), NA)))

# Combine
parlimenResults <- parlimenResults %>%
    left_join(wasted)

# Save processed data
save(parlimenResults, file = "Data/parlimenResults_stats.Rda")
