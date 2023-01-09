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
