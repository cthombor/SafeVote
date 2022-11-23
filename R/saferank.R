#' Assess the safety of a preliminary result for an election
#'
#' Ballots are deleted at random from the ballot-box, with election results
#' computed once per 'dinc' ballot-deletions.  The experiment terminates after a
#' specified number of ballots have been deleted, or a specified number of
#' ballot-counts have occurred.  Note: these ballot-counts are correlated.  Use
#' testFraction() to experiment with independently-drawn samples from the
#' ballot-box.
#'
#' @param votes A set of ballots
#' @param countMethod The name of a function which will count the ballots
#' @param countArgs List of args to be passed to countMethod (in addition to
#'   votes)
#' @param rankMethod Name of a ranking attribute in the output of countMethod
#' @param dlimit Maximum number of ballots to delete
#' @param dstart Number of ballots to be deleted after the initial ballot-count
#' @param dinc Number of ballots to be deleted in subsequent steps
#' @param drep Maximum number of elections (required if dinc=0)
#' @param exptName stem-name of experimental units e.g. "E"
#' @param quiet TRUE to suppress all output
#' @param verbose TRUE to produce diagnostic output
#' @return SafeRankExpt object, describing this experiment and its results
#' @export
#' @examples
#' data(food_election)
#' testDeletions(food_election)
#' testDeletions(food_election, countMethod="stv",
#'   countArgs=list(complete.ranking=TRUE))
#' 

testDeletions <-
  function(votes = "food_election",
           countMethod = "condorcet",
           countArgs = NULL,
           dlimit = NULL,
           dstart = NULL,
           dinc = NULL,
           drep = NULL,
           rankMethod = "safeRank",
           exptName = NULL,
           quiet = FALSE,
           verbose = FALSE) {
   
    result <- new_SafeRankExpt(
      nBallots = integer(0),
      ranks = matrix(
        nrow = 0,
        ncol = ncol(votes),
        dimnames = list(NULL, colnames(votes))
      ),
      margins = matrix(
        nrow = 0,
        ncol = ncol(votes),
        dimnames = list(NULL, colnames(votes))
      ),
      countMethod = countMethod,
      rankMethod = rankMethod,
      datasetName = deparse1(substitute(votes)),
      experimentalMethod = "testDeletions",
      otherFactors = list(
        dlimit = dlimit,
        dstart = dstart,
        dinc = dinc,
        drep = drep,
        countArgs = countArgs
      )
    )
    
    if (is.null(exptName)) {
      exptName <-
        paste(LETTERS[sample(length(LETTERS), 3, replace = TRUE)],
              collapse = "")
    }
    
    ## suppress all output from counting unless verbose=TRUE
    cArgs <-
      append(countArgs, list(quiet = !verbose, verbose = verbose))
    
    ## an initial count of all ballots
    cr <- do.call(countMethod, append(cArgs, list(votes = votes)))
    if (!rankMethod %in% attributes(cr)$names) {
      stop(paste("countMethod", countMethod, "does not produce a",
                 rankMethod))
    }

    if (nrow(votes) != nrow(cr$data)) {
      warning(paste(
        nrow(votes) - nrow(cr$data),
        "informal ballots were deleted."
      ))
    }
    ballots <- cr$data ## ballots are numbered and corrected
    nb <- nrow(ballots)
    
    ## include the initial ballot count in the experimental record
    ## TODO: refactor into result <- recordResult(result,...)
    ## TODO: consider a major refactoring into result <- doExperiment(result)
    exptID = paste0(exptName,0)
    result$nBallots <- append(result$nBallots, structure(nb,names=exptID))
    crRank <- extractRank(rankMethod,countMethod,cr)
    result$ranks <- rbind(result$ranks,crRank)
    rownames(result$ranks)[1] <- exptID
    result$margins <- rbind(result$margins,cr$margins)
    rownames(result$margins)[1] <- exptID
 
    if (!quiet) {
      cat("Number of ballots counted by", countMethod, ":\n", nb)
    }
    
    ## stv() throws an error if there are fewer than two ballots
    dlimit = min(dlimit, nb - 2) 
    result$otherFactors$dlimit<-dlimit
    
    if (is.null(dstart)) {
        dstart <- 0
        result$otherFactors$dstart<-dstart
    }
    
    if (is.null(dinc)) {
      dinc <- (dlimit-dstart+4) %/% 10  ## deciles (roughly)
      if (dinc==0) dinc=1
    }
    stopifnot(dinc >= 0)
    result$otherFactors$dinc<-dinc

    if (is.null(drep)) {
      stopifnot(dinc != 0)
      drep <- trunc(dlimit / dinc) + 1
      result$otherFactors$drep<-drep
    }
    
    if (dinc == 0) {
      nbv <- rep(nb - dstart, drep-1)
    } else {
      if (dstart == 0) { ## special case: we have already counted all ballots
        nbv <- nb - dstart - dinc * (1:drep)
      } else {
        nbv <- nb - dstart - dinc * (0:(drep-1))
      }
      nbv <- nbv[nbv > 1]
    }
    
    nrep <- 0
    for (nBallots in nbv) {

      nrep <- nrep + 1
      exptID <- paste0(exptName,nrep)
      if (!quiet) {
        cat(ifelse((nrep %% 10) == 0, ",\n", ","), nBallots)
      }
      
      rvn <- sample(nrow(ballots), nBallots)
      ballots <- ballots[rvn,]
      cr <-
        do.call(countMethod, append(cArgs, list(votes = ballots)))
      
      result$nBallots <-
        append(result$nBallots, structure(nBallots, names = exptID))
      crRank <- extractRank(rankMethod, countMethod, cr)
      result$ranks <- rbind(result$ranks, crRank)
      rownames(result$ranks)[nrep+1]<-exptID
      result$margins <- rbind(result$margins,cr$margins)
      rownames(result$margins)[nrep+1] <- exptID
      
    }
    
    if (!quiet) {
      cat("\n\nExperimental results:\n")
      print(result)
    }
    return(invisible(result))
  }

#' Test the sensitivity of a result to tactical voting.
#'
#' Ballots are added until a specified number of simulated elections (`arep`)
#' have been held   A tactic of "plumping" is used when stuffing the ballot
#' box, if a `favoured` candidate is specified.  Alternatively, a
#' `tacticalBallot` may be specified.
#'
#' @param votes A set of ballots
#' @param countMethod The name of a function which will count the ballots
#' @param countArgs List of args to be passed to countMethod (in addition to
#'   votes)
#' @param rankMethod Name of a ranking attribute in the output of countMethod
#' @param favoured Name of the candidate being "plumped".  If NULL, a random
#'   candidate is selected from among the candidates not initially top-ranked.
#'   All other candidates are fully-ranked at random, with an identical ballot
#'   paper being stuffed multiple times.  An integer value for 'favoured' is
#'   interpreted as an index into the candidate names.
#' @param tacticalBallot A ballot paper i.e. a vector of length ncol(ballots).
#'   If this argument is non-null, it takes precedence over 'favoured' when the
#'   ballot box is being stuffed.
#' @param ainc Number of ballots to be added in each step
#' @param arep Maximum number of ballot-stuffed elections to run
#' @param exptName stem-name of experimental units e.g. "E"
#' @param quiet TRUE to suppress all output
#' @param verbose TRUE to produce diagnostic output
#' @return A matrix of experimental results, of dimension n by 2m+1, where n is
#'   the number of elections and m is the number of candidates.  The first
#'   column is named "nBallots".  Other columns indicate the ranking of the
#'   eponymous candidate, and their margin over the next-lower-ranked candidate.
#' @export
#' @examples
#' data(food_election)
#' testAdditions(food_election, countArgs=list(complete.ranking=TRUE))
#' testAdditions(food_election, tacticalBallot=c(1,2,3,4,5), arep=2)
#' 
testAdditions <- function(votes,
                          ainc = 1,
                          arep = NULL,
                          favoured = NULL,
                          tacticalBallot = NULL,
                          rankMethod = "safeRank",
                          countMethod = "stv",
                          countArgs = NULL,
                          exptName = NULL,
                          quiet = FALSE,
                          verbose = FALSE) {

  if (is.null(arep)) {
    arep <- 1
  }
  stopifnot(arep > 0)
  if (is.null(ainc)) {
    ainc <- round(sqrt(nrow(votes)))
  }
  stopifnot(ainc >= 0)
  
  result <- new_SafeRankExpt(
    nBallots = integer(0),
    ranks = matrix(
      nrow = 0,
      ncol = ncol(votes),
      dimnames = list(NULL, colnames(votes))
    ),
    margins = matrix(
      nrow = 0,
      ncol = ncol(votes),
      dimnames = list(NULL, colnames(votes))
    ),
    countMethod = countMethod,
    rankMethod = rankMethod,
    datasetName = deparse1(substitute(votes)),
    experimentalMethod = "testAdditions",
    otherFactors = list(
      ainc = ainc,
      arep = arep,
      countArgs = countArgs
    )
  )
  
  if (is.null(exptName)) {
    exptName <-
      paste(LETTERS[sample(length(LETTERS), 3, replace = TRUE)],
            collapse = "")
  }
  
  ## Suppress all output from counting unless verbose=TRUE
  cArgs <-
    append(countArgs, list(quiet = !verbose, verbose = verbose))
  
  cr <- do.call(countMethod, append(cArgs, list(votes = votes)))
  if (!rankMethod %in% attributes(cr)$names) {
    stop(paste("countMethod", countMethod, "does not produce a",
               rankMethod))
  }
  
  ## include the initial ballot count in the experimental record
  ## TODO: refactor to avoid duplicating this code in this module
  exptID = paste0(exptName,0)
  result$nBallots <-
    append(result$nBallots, structure(nrow(votes), names = exptID))
  crRank <- extractRank(rankMethod, countMethod, cr)
  result$ranks <- rbind(result$ranks, crRank)
  rownames(result$ranks)[1]<-exptID
  result$margins <- rbind(result$margins, cr$margins)
  rownames(result$margins)[1] <- exptID
  
  if (!quiet && verbose) {
    cat("Initial ranking by", countMethod, ":\n")
    print(crRank)
  }
  
  nc <- ncol(votes)
  nv <- nrow(votes)
  svotes <- votes ## simulated ballot box
  
  if (is.null(tacticalBallot)) {
    if (!is.null(favoured)) {
      fc <- ifelse(is.character(favoured),
                   which(names(crRank) == favoured),
                   favoured)
      stopifnot((fc >= 1) || (fc <= nc))
      favoured <- colnames(cr$data)[fc]
    } else {
      cl <- colnames(votes) ## choose a random non-winner to favour
      cl <- cl[-which(cl==names(crRank[which(crRank==1)]))]
      favoured <- cl[sample(length(cl), size=1)]
      fc <- which(colnames(votes) == favoured)
    }
    ## henceforth, favoured is a string and fc is an integer
    fb <- sample(1:nc, nc) # random ballot
    fb[fc] <- 0
    fb <- rank(fb) # favoured is most-preferred, other prefs are random
  } else {
    fb <- tacticalBallot
    stopifnot((length(fb) == nc))
  }
  names(fb) <- colnames(votes)
  if (!quiet) {
    cat("\nAdding up to",
        arep * ainc,
        countMethod,
        "ballots = (",
        fb,
        ")\n")
  }
  
  result$otherFactors <- append(result$otherFactors, list(tacticalBallot=fb))
  
  if (!quiet)
    cat("Testing progress: ")
  
  nadd <- 0
  for (repct in 1:arep) {
    svotes <- rbind(svotes,
                   matrix(
                     fb,
                     nrow = ainc,
                     ncol = nc,
                     byrow = TRUE,
                     dimnames = list(c((nv+nadd+1):(nv+nadd+ainc)), names(fb))
                   )) ## stuffing the ballot box!
    nadd <- nadd + ainc
    if (!quiet) {
      cat(paste0(" ", repct))
      cat(ifelse(repct < arep, ifelse((repct %% 10) == 0, ",\n", ","), ""))
    }
    cr <- do.call(countMethod,
                     append(cArgs, list(votes = svotes)))

    exptID = paste0(exptName, repct)
    result$nBallots <-
      append(result$nBallots, structure(nrow(svotes), names = exptID))
    crRank <- extractRank(rankMethod, countMethod, cr)
    result$ranks <- rbind(result$ranks, crRank)
    rownames(result$ranks)[repct+1] <- exptID
    result$margins <- rbind(result$margins, cr$margins)
    rownames(result$margins)[repct+1] <- exptID
    
  }
  
  if (!quiet) {
    cat("\nExperimental results:\n")
    print(result)
  }
  return(invisible(result))
}


#' Bootstrapping experiment, with fractional counts of ballots.
#'
#' Starting from some number ('astart') of randomly-selected ballots,
#' increasingly larger number of randomly-selected ballots are counted. The
#' ballots are chosen independently for each experiment. The rankings and
#' margins produced by each simulated election are returned as a matrix with one
#' row per election.
#'
#' @param votes A set of ballots
#' @param countMethod The name of a function which will count the ballots, e.g.
#'   "stv", "condorcet".
#' @param countArgs List of args to be passed to countMethod (in addition to
#'   votes)
#' @param rankMethod Name of a ranking attribute in the output of countMethod,
#'   e.g. "elected", "ranking", "safeRank".
#' @param astart Starting number of ballots (min 2)
#' @param ainc Number of ballots to be added in each step
#' @param arep Limit on the number of repetitions of the test. Required to be
#'   non-null if ainc==0.
#' @param exptName stem-name of experimental units e.g. "E"
#' @param quiet TRUE to suppress all output
#' @param verbose TRUE to produce diagnostic output
#' @return a matrix of experimental results, of dimension n by 2m+1, where n is
#'   the number of elections and m is the number of candidates.  The first
#'   column is named "nBallots".  Other columns indicate the ranking and margin
#'   of each candidate in each election.
#' @export
#' @examples
#' data(food_election)
#' testFraction(food_election, countMethod="condorcet",
#'              countArgs=list(safety=0.5,complete.ranking=TRUE))
#' testFraction(dublin_west, astart=20, ainc=20, arep=2, countMethod="stv",
#'              rankMethod="elected", quiet=FALSE)
testFraction <- function(votes,
                         astart = NULL,
                         ainc = NULL,
                         arep = NULL,
                         rankMethod = "safeRank",
                         countMethod = "stv",
                         countArgs = NULL,
                         exptName = NULL,
                         quiet = FALSE,
                         verbose = FALSE) {
  nv <- nrow(votes)
  nc <- ncol(votes)
  
  if (is.null(astart) || (astart < 2)) {
    astart = 2
  }
  if (is.null(ainc)) {
    ainc <- nv %/% 10  ## deciles (roughly)
  } else {
    if (ainc == 0) {
      stopifnot(!is.null(arep))
    }
  }
  if (is.null(arep)) {
    arep <- trunc((nv - astart) / ainc)
  }
  
  ## Suppress all output from counting unless verbose=TRUE
  cArgs <-
    append(countArgs, list(quiet = !verbose, verbose = verbose))
  
  nb <- astart + ainc * (1:arep)
  if ((ainc != 0) && (arep > trunc((nv - astart) / ainc))) {
    nb <- nb[-which(nb >= nv)]
    nb <- c(nb, nv) ## use all ballots in the last experimental run
  }
  
  if (verbose && !quiet) {
    cat(
      "\nSelecting an increasingly-large fraction of",
      nv,
      "ballots until the ultimate ranking is found.\n"
    )
  }
  if (!quiet) {
    cat("Fraction of", countMethod, "ballots:\n")
  }
  
  if (is.null(exptName)) {
    exptName <-
      paste(LETTERS[sample(length(LETTERS), 3, replace = TRUE)],
            collapse = "")
  }
  
  result <- new_SafeRankExpt(
    nBallots = integer(0),
    ranks = matrix(
      nrow = 0,
      ncol = ncol(votes),
      dimnames = list(NULL, colnames(votes))
    ),
    margins = matrix(
      nrow = 0,
      ncol = ncol(votes),
      dimnames = list(NULL, colnames(votes))
    ),
    countMethod = countMethod,
    rankMethod = rankMethod,
    datasetName = deparse1(substitute(votes)),
    experimentalMethod = "testFraction",
    otherFactors = list(
      astart = astart,
      ainc = ainc,
      arep = arep,
      countArgs = countArgs
    )
  )
  
  nreps <- 0
  for (nBallots in nb) {
    nreps <- nreps + 1
    if (!quiet) {
      cat(paste0(" ", format(round(
        nBallots / nv * 100, 1
      ), digits = 4), "%"))
      cat(ifelse(nreps < length(nb),
                 ifelse((nreps %% 10) == 0, ",\n", ","), 
                 ""))
    }
    
    selBallots <- sample(nv, nBallots)
    cr <- do.call(countMethod,
                     append(cArgs, list(votes = votes[selBallots,])))
    
    exptID = paste0(exptName, nreps)
    result$nBallots <-
      append(result$nBallots, structure(nBallots, names = exptID))
    crRank <- extractRank(rankMethod, countMethod, cr)
    result$ranks <- rbind(result$ranks, crRank)
    rownames(result$ranks)[nreps] <- exptID
    result$margins <- rbind(result$margins, cr$margins)
    rownames(result$margins)[nreps] <- exptID
    
  }
  
  if (!quiet) {
    cat("\nExperimental results:\n")
    print(result)
  }
  
  return(invisible(result))
}

#' Extract a ranking vector by name from the results of a ballot count
#'
#' @param rankMethod e.g. "elected"
#' @param countMethod e.g. "stv"
#' @param cr structure returned by a ballot-counting method
#'
#' @return a numeric ranking vector, in order of colnames(cr$data)
extractRank <- function(rankMethod, countMethod, cr) {
  if (!rankMethod %in% attributes(cr)$names) {
    stop(paste("countMethod",
               countMethod,
               "does not produce a",
               rankMethod))
  }
  if (rankMethod == "elected") {
    ## convert a list of names to a 1-2 ranking vector
    ranks <- c(ifelse(colnames(cr$data) %in% cr$elected,2,1))
    names(ranks) <- colnames(cr$data)
  } else {
    ## rearrange a numeric ranking vector, if necessary
    ranks <- cr[[rankMethod]][colnames(cr$data)]
  }
  return(ranks)
}

#' Constructor for the results of a SafeRank experiment
#'
#' @param nBallots primary factor: number of ballots counted in this
#'   experimental unit
#' @param ranks a matrix with colnames(ballots), one row per experimental unit
#' @param margins a matrix with colnames(ballots), one row per unit
#' @param countMethod secondary factor: counting method e.g. "stv"
#' @param rankMethod secondary factor: ranking method e.g. "elected"
#' @param datasetName secondary factor: name of the dataset of ballots
#' @param experimentalMethod secondary factor: name of the method which
#'   simulated these elections e.g. "testFraction"
#' @param otherFactors other secondary factors, e.g. parameters to
#'   experimentalMethod, a timestamp.
#'   
#' @return object of class SafeRankExpt
new_SafeRankExpt <-
  function(nBallots = integer(0),
           ranks = matrix(),
           margins = matrix(),
           countMethod = NULL,
           rankMethod = NULL,
           datasetName = NULL,
           experimentalMethod = NULL,
           otherFactors = list()) {
    structure(
      list(
        nBallots = nBallots,
        ranks = ranks,
        margins = margins,
        countMethod = countMethod,
        rankMethod = rankMethod,
        datasetName = datasetName,
        experimentalMethod = experimentalMethod,
        otherFactors = otherFactors
      ),
      class = "SafeRankExpt"
    )
  }

##TODO: summary() method for SafeRankExpt class
