#' Sample data for testing SafeVote
#' 
#' @title Food Election
#'
#' @docType data
#'
#' @usage data(food_election)
#'
#' @format A data frame with 20 observations and 5 candidates (Oranges, 
#'   Pears, Chocolate, Strawberries, Sweets). Each record corresponds to 
#'   one ballot with ranking for each of the candidates. 
#'
#' @keywords datasets
"food_election"

#' @title Dublin West
#' 
#' @docType data
#'
#' @usage data(dublin_west)
#'
#' @description Dataset containing ranked votes for the Dublin West 
#' constituency in 2002, Ireland. Results of that STV election can be viewed
#' at https://en.wikipedia.org/wiki/Dublin_West#2002_general_election.
#' 
#' @format A data frame with 29988 observations and 9 candidates. Each record 
#' corresponds to one ballot with candidates being ranked between 1 and 9 with 
#' zeros allowed.
#'
#' @keywords datasets
"dublin_west"

#' @title IMS Election
#' 
#' @docType data
#'
#' @usage data(ims_election)
#'
#' @description Datasets containing anonymized votes for a past Council 
#' election of the Institute of Mathematical Statistics (IMS). The dataset 
#' ims_election is the original dataset used with single transferable vote, 
#' where candidate names have been changed.
#' 
#' @format A data frame with 620 observations and 10 candidates (names were 
#' made up). Each record corresponds to one ballot. The IMS Council voting 
#' is done using the STV method, and thus the ims_election dataset contains 
#' ballots with candidates being ranked between 1 and 10 with zeros allowed.
#'
#' @keywords datasets
"ims_election"

#' @title IMS STV
#' 
#' @docType data
#'
#' @usage data(ims_election)
#'
#' @description Copy of ims_election, included for backwards compatibility.
#' 
#' @format A data frame with 620 observations and 10 candidates (names were 
#' made up). Each record corresponds to one ballot. The IMS Council voting 
#' is done using the STV method, and thus the ims_election dataset contains 
#' ballots with candidates being ranked between 1 and 10 with zeros allowed.
#'
#' @keywords datasets
"ims_stv"

#' @title IMS Approval
#' 
#' @docType data
#'
#' @usage data(ims_approval)
#'
#' @description Modified version of ims_election, for use in approval voting.
#' 
#' @format A data frame with 620 observations and 10 candidates (names were 
#' made up). Each record corresponds to one ballot, with 0 indicating
#' disapproval of a candidate and 1 indicating approval.
#'
#' @keywords datasets
"ims_approval"

#' @title IMS Score
#' 
#' @docType data
#'
#' @usage data(ims_score)
#'
#' @description Modified version of ims_election, for use in score voting.
#' 
#' @format A data frame with 620 observations and 10 candidates (names were 
#' made up). Each record corresponds to one ballot, with higher values
#' indicating the more-preferred candidates.
#'
#' @keywords datasets
"ims_score"

#' @title IMS Plurality
#' 
#' @docType data
#'
#' @usage data(ims_plurality)
#'
#' @description Modified version of ims_election, for use in plurality voting.
#' 
#' @format A data frame with 620 observations and 10 candidates (names were 
#' made up). Each record corresponds to one ballot, with 1 against
#' the voter's most-preferred candidate and 0 against all other candidates.
#'
#' @keywords datasets
"ims_plurality"

#' @title Yale Faculty Senate 2016
#'
#' @usage data(yale_ballots)
#' 
#' @description This data follows the structure of a 2016 Yale
#'   Faculty Senate election, with candidate names anonomyised and permuted.
#'   Imported to SafeVote from [STV v1.0.2](https://github.com/jayemerson/STV),
#'   after applying the `STV::cleanBallots` method to remove the ten empty
#'   rows.
#'   
#' @format A data frame with 479 observations and 44 candidates. 
#'   
#' @keywords datasets
"yale_ballots"

#' @title UK Labour Party Leader 2010
#' 
#' @usage data(uk_labour_2010)
#' 
#' @description These are the ballots cast by Labour MPs and MEPs in an election
#'   of their party's leader in 2010, as published by the Manchester Guardian.
#'   The names of the electors have been suppressed in this file, but are
#'   available at [Rangevoting.org](https://rangevoting.org/LabourUK2010.html)
#'   along with extensive commentary on the election.
#'   
#' @format A data frame with 266 observations and 5 candidates.
#' 
#' @keywords datasets
"uk_labour_2010"

#' @title Tideman a3_hil
#' 
#' @usage data(a3_hil)
#' 
#' @description This data is one of 87 sets of ballots from the [Tideman data
#'   collection](https://rangevoting.org/TidemanData.html), as curated by [The
#'   Center for Range Voting](https://rangevoting.org/).
#'
#'   This set of ballots was collected in 1987 by Nicolaus Tideman, with support
#'   from NSF grant SES86-18328. "The data are records of ballots from elections
#'   of British organizations (mostly trade unions using PR-STV or IRV voting)
#'   in which the voters ranked the candidates. The data were gathered under a
#'   stipulation that the organizations involved would remain anonymous."
#'
#'   The ballots were encoded in David Hill's format, and have been converted to
#'   the preference-vector format of this package.  The file
#'   [A4.HIL](https://rangevoting.org/TiData/A3.HIL) contains eight blank ballot
#'   papers (1, 616, 619, 620, 685, 686, 687, 688) which we have retained. This
#'   set may be counted by `stv(a3_hil,nseats=attr(a3_hil,"nseats"))`.
#'   
#' @format A data frame with attribute "nseats" = 7, consisting of 989
#'   observations and 15 candidates.
#' 
#' @keywords datasets
"a3_hil"

#' @title Tideman a4_hil
#' 
#' @usage data(a4_hil)
#' 
#' @description This data is one of 87 sets of ballots from the [Tideman data
#'   collection](https://rangevoting.org/TidemanData.html), as curated by [The
#'   Center for Range Voting](https://rangevoting.org/).
#'
#'   This set of ballots was collected in 1987 by Nicolaus Tideman, with support
#'   from NSF grant SES86-18328. "The data are records of ballots from elections
#'   of British organizations (mostly trade unions using PR-STV or IRV voting)
#'   in which the voters ranked the candidates. The data were gathered under a
#'   stipulation that the organizations involved would remain anonymous."
#'
#'   The ballots were encoded in David Hill's format, and have been converted to
#'   the preference-vector format of this package.
#'   
#' @format A data frame with attribute "nseats" = 2, consisting of 43
#'   observations and 14 candidates.
#' 
#' @keywords datasets
"a4_hil"

#' @title Tideman a53_hil
#' 
#' @usage data(a53_hil)
#' 
#' @description This data is one of 87 sets of ballots from the [Tideman data
#'   collection](https://rangevoting.org/TidemanData.html), as curated by [The
#'   Center for Range Voting](https://rangevoting.org/).
#'
#'   This set of ballots was collected in 1988 by Nicolaus Tideman, with support
#'   from NSF grant SES86-18328. "The data are records of ballots from elections
#'   of British organizations (mostly trade unions using PR-STV or IRV voting)
#'   in which the voters ranked the candidates. The data were gathered under a
#'   stipulation that the organizations involved would remain anonymous."
#'
#'   The ballots were encoded in David Hill's format, and have been converted to
#'   the preference-vector format of this package.  Candidates have been renamed
#'   to letters of the alphabet, for ease of comparison with Table 3 of Tideman,
#'   "Better voting methods through technology: The refinement-manageability
#'   trade-off in the single transferable vote", Public Choice 103: 13-34, 2000,
#'   DOI: [10.1023/A:1005082925477](http://dx.doi.org/10.1023/A:1005082925477).
#'   As noted in this table, it is a very close race between candidates D, F,
#'   and B in the final rounds of a Meek count of `a53_hil`.  
#'   
#'   Tideman's implementation of Meek's method excludes B (on 59.02 votes), then
#'   elects D in the final round (on 88.33 votes) with a margin of 0.95 votes
#'   ahead of F (on 87.38 votes).  
#'   
#'   In v1.0, `stv(a53.hil,quota.hare=TRUE)` excludes F (on 56.418 votes), then
#'   elects D in the final round (on 79.705 votes) with a winning margin of
#'   0.747 votes ahead of B (on 78.958 votes). The result of the election is the
#'   same but the vote counts and winning margins differ significantly; so we
#'   conclude that `stv(quota.hare=TRUE)` in SafeVote v1.0 is *not* a reliable
#'   proxy for Tideman's implementation of Meek's algorithm.
#'   
#'   Future researchers may wish to adjust the quota calculation of `vote.stv()`
#'   so that it is no longer biased upward by a "fuzz" of 0.001, to see if this
#'   change significantly reduces the discrepancies with Tideman's
#'   implementation of Meek.
#'   
#'   As noted in [(Wichmann,
#'   2000)](https://www.votingmatters.org.uk/RES/MKVAL.pdf), "... an exact
#'   comparison \\[ between two implementations of Meek \\] cannot be expected
#'   due both to differences in the specification and due to the breaking of
#'   ties."
#'   
#'   In June 2020, a postgraduate student at Edinburgh presented a
#'   [slideshow](https://web.inf.ed.ac.uk/sites/default/files/atoms/files/aiai_seminar_22_june_slides.pdf)
#'   describing progress toward a formal specification of Meek's algorithm. This
#'   specification, when complete, would support proofs of important properties
#'   -- such as convergence -- and would allow an implementation to be formally
#'   verified for correctness.
#'   
#' @format A data frame with attribute "nseats" = 4, consisting of 460
#'   observations and 10 candidates.
#' 
#' @keywords datasets
"a53_hil"
