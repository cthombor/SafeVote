# An analysis of the published results from the Hamilton 2022 local body 
# election. This script may be helpful as an example of a "tidy" analysis.

# Note that the transferable votes reported for an eliminated candidate are also
# reported against some other candidate.  Dividing the excess votes by the
# initial quota is (at least arguably) a valid measure of the "closeness" of an
# election -- and thus could be a sanity-check on the "safety" metric we
# calculate in SafeVote::stv().
#
# Note that a small number of votes may be "lost" or "added" during the
# counting process, due to roundoff errors and the fuzzed arithmetic of
# STV counting method we inherited from vote::stv().

library(tidyverse)
data(hamilton2022)
hamilton2022analysis <- hamilton2022 %>% 
  group_by(City,Office,Count,npos,nBallots,nBlanks,nInformals) %>% 
  summarise(sumVotes=sum(VotesReceived), maxNTV=max(NTV)) %>%
  mutate(excessVotes = sumVotes - nBallots + nBlanks + nInformals) %>%
  mutate(closeness = excessVotes / (1+(nBallots - nBlanks - nInformals)/(npos+1)))
usethis::use_data(hamilton2022analysis)
