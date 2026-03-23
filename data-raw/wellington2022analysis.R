# An analysis of the published results for the Wellington 2022 election,
# focussing on comparing the total votes reported to the number valid ballots 
# counted.

# Note that the transferable votes reported for an eliminated candidate are
# also reported against some other candidate.  Dividing the excess votes
# by the initial quota is thus a measure of the "closeness" of an
# election.
#
# A small number of votes will be "lost" or "added", due to roundoff errors and
# fuzzed arithmetic.

library(openxlsx)
wellington2022 <- read.xlsx("data-raw/Wellington 2022.xlsx")
library(tidyverse)
wellington2022analysis <- wellington2022 %>% 
  group_by(City,Office,Count,npos,nBallots,nBlanks,nInformals) %>% 
  summarise(sumVotes=sum(VotesReceived), maxNTV=max(NTV)) %>%
  mutate(excessVotes = sumVotes - nBallots + nBlanks + nInformals) %>%
  mutate(closeness = excessVotes / (1+(nBallots - nBlanks - nInformals)/(npos+1)))
usethis::use_data(wellington2022analysis)
