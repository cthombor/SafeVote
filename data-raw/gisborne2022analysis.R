# An analysis of votes reported in the Gisborne 2022 election, in excess of
# the valid ballots counted.  This is definitely not an unbiased measure of
# the informal ballots which were not counted, because the votes reported
# for an eliminated candidate will be counted at least one more time (and
# possibly many times, if the voter's next-preferred candidate is eliminated
# many times).
#
# Also: an analysis of ranks as computed from votes[cand], vs ranks computed
# from the election & elimination order.  We break ties in the elimination order
# by comparing votes, because votes are comparable within the same round of an
# STV ballot count.
#
# Note that the transferable votes reported for an eliminated candidate are also
# reported against some other candidate.  Dividing the excess votes by the
# initial quota is thus a measure of the "closeness" of an election.
#
# Also note that fractional votes will be "added" or "lost" due to rounding
# errors and fuzzed arithmetic.  See e.g. the comment in Algorithm 123 (Hill et
# al., 1987): "If it is valid to do so, print quota instead of votes[cand]
# because the latter might have a small rounding error that would confuse
# unsophisticated users."  The determination of ties in a ranking of candidates
# is thus necessarily heuristic, unless heroic efforts are made to bound the
# round-off errors (e.g. by using arbitrary-precision arithmetic or by symbolic
# manipulation of algebraic expressions of arbitrary length).

library(openxlsx)
gisborne2022 <- read.xlsx("data-raw/Gisborne-2022-detail.xlsx")
library(tidyverse)
gisborne2022 <- gisborne2022 %>%
  dplyr::group_by(Count,Office) %>% 
  dplyr::mutate(nRounds=max(Iteration)) %>% 
  dplyr::mutate(RankByRound=
                  rank(ifelse(is.na(Rank),
                              nRounds-Iteration+npos-Votes.Received/nBallots,
                              Rank))) %>% 
  dplyr::mutate(RankByVote=rank(-Votes.Received))

usethis::use_data(gisborne2022)

gisborne2022analysis <- gisborne2022 %>% 
  group_by(City,Office,Count,npos,nBallots,nBlanks,nInformals) %>% 
  summarise(sumVotes=sum(VotesReceived), maxNTV=max(NTV)) %>%
  mutate(excessVotes = sumVotes - nBallots + nBlanks + nInformals) %>%
  mutate(closeness = 
           excessVotes / (1+(nBallots - nBlanks - nInformals)/(npos+1)))

usethis::use_data(gisborne2022analysis)
