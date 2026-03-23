
## We compute a plausible value for nBallots from the published results of
## Auckland local body elections. Note that this is an upper-bounding value. The
## actual number of ballots may be much smaller than the sum of the reported
## votes, as the votes of an eliminated candidate are reported as at the round
## of their elimination, and their transferable votes will also be reported for
## other candidates.  A lower-bounding value for nBallots is
## validVotes/npos+informalVotes+blankVotes.
##
## We also report informalVotes and blankVotes as attributes of the count,
## rather than as pseudo-candidates -- for consistency with how other local
## bodies (such as Hamilton) report their results, and for ease of analysis on
## the safety of rankings of candidates from their reported votes in progress
## and preliminary results.

library(tidyverse)
library(readxl)

auckland2022 <- read_xlsx(
  "data-raw/auckland-2022.xlsx",
  col_types =
    c(
      "text",
      "text",
      "numeric",
      "text",
      "text",
      "numeric",
      "numeric",
      "text"
    )
)

ia <- auckland2022 %>% dplyr::filter(Candidate=="Informal") %>%
  dplyr::select(Count,Office,Votes) %>%
  dplyr::rename(informalVotes=Votes)

ba <- auckland2022 %>% 
  dplyr::filter(Candidate=="Blank") %>% 
  dplyr::select(Count,Office,Votes) %>% 
  dplyr::rename(blankVotes=Votes)

auckland2022 <- auckland2022 %>%
  dplyr::filter(Candidate != "Informal" & Candidate != "Blank") %>%
  dplyr::left_join(ba, join_by(Count, Office)) %>%
  dplyr::left_join(ia, join_by(Count, Office)) %>%
  dplyr::group_by(Count, Office) %>%
  dplyr::mutate(validVotes = sum(Votes)) %>%
  dplyr::mutate(nBallots = validVotes+informalVotes+blankVotes)

rm(ia,ba)

usethis::use_data(auckland2022)

## Note: other datasets were copied from vote_2.3.2 (https://arxiv.org/abs/2102.05801)
## and from other online sources, as noted in data.R of this distribution
