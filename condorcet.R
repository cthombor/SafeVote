condorcet <- function(votes, runoff = FALSE, safety = 1.0, fsep = '\t', quiet = FALSE, ...) {
    compare.two.candidates <- function(v1, v2) {
        i.wins <- sum(v1 < v2)
        j.wins <- sum(v1 > v2)
        c(i.wins > j.wins, i.wins < j.wins, i.wins, j.wins)
    }
    compute.wins <- function(dat, ncan, cnam){
        p <- array(0, dim=c(ncan, ncan, 2), dimnames = list(cnam, cnam, list("Wins","Prefs")))
        for(i in 1:(ncan-1)){
            for(j in ((i+1):ncan)){
                pair.run <- compare.two.candidates(dat[,i], dat[,j])
                p[i,j,"Wins"] <- pair.run[1]
                p[i,j,"Prefs"] <- pair.run[3]
                p[j,i,"Wins"] <- pair.run[2]
                p[j,i,"Prefs"] <- pair.run[4]
            }
        }
        p
    }
    votes <- prepare.votes(votes, fsep=fsep)
    nc <- ncol(votes)
    cnames <- colnames(votes)

    corvotes <- correct.ranking(votes, quiet = quiet)

    x <- check.votes(corvotes, "condorcet", quiet = quiet)

    corrected <- which(rowSums(corvotes != votes) > 0 & rownames(votes) %in% rownames(x))
    corrected.votes <- NULL
    if(length(corrected) > 0) {
        corrected.votes <- list(original = votes[corrected,], new = corvotes[corrected, ],
                                index = as.numeric(corrected))
    }
    check.nseats(1, ncol(x))
    x2 <- x
    x2[x2 == 0] <- max(x2) + 1 # unranked candidates are lowest= preference
    points <- compute.wins(x2, nc, cnames)
    cdc.winner <- apply(points[,,"Wins"], 1, function(p) sum(p) == nc-1)
    cdc.loser  <- apply(points[,,"Wins"], 2, function(p) sum(p) == nc-1)
    cdc.scores <- points[,,"Prefs"]     # Strength of a pairwise preference
    runoff.winner <- ro.part <- ro.part.first <- NULL
    if(sum(cdc.winner) == 0 && runoff) { # run-off
        nwins <- rowSums(points[,,"Wins"])
        winner.exists <- FALSE
        cand.names <- cnames
        ncro <- nc
        while(!winner.exists) {
            most.wins <- nwins == max(nwins)
            if(sum(most.wins) < 2) # second most wins
                most.wins <- (most.wins |
		     nwins == max(nwins[nwins < max(nwins)])) & nwins > 0
            ro.part <- cand.names[most.wins]
            ## keep the list of the original run-off participants
            if(is.null(ro.part.first)) ro.part.first <- ro.part
            ## run-off must have less candidates than the original set
            if(length(ro.part) == ncro || length(ro.part) <= 1) {
                if(length(ro.part) == 1) runoff.winner <- ro.part
                ## only one run-off participant
                break
            }
            if(sum(most.wins) == 2) { # run-off between two candidates
                pair.run <-
                    compare.two.candidates(x2[,which(most.wins)[1]],
                                           x2[,which(most.wins)[2]])
                runoff.winner <-
                    cand.names[which(most.wins)[which(pair.run == TRUE)]]
            } else { # run-off between more than two candidates
                x3 <- x2[, most.wins]
                p.runoff <- compute.wins(x3, ncol(x3), colnames(x3))
                runoff.winner <-
                    colnames(x3)[apply(p.runoff[,,"Wins"], 1,
                                       function(p) sum(p) == ncol(x3)-1)]
            }
            if(length(runoff.winner) > 0) {
                winner.exists <- TRUE
                break
            }
            if(sum(most.wins) == 2) break
            nwins <- rowSums(p.runoff[,,"Wins"])
            x2 <- x3
            cand.names <- colnames(x2)
            ncro <- ncol(x2)
        }
    }

    bordaScore <- rowSums(cdc.scores)
    bordaRank <- rank(-cdc.scores, ties.method="min")
    nv <- nrow(x2)
    fuzz <- safety * nc * sqrt(nv)/2
    ## safety == 1.0 puts a "fuzz" of approx 1 s.d. on each Borda score,
    ## when votes are i.u.d. permutations

    ## unidirectional cluster heuristic
    ## safeWins <- array(dim=nc,dimnames=list(cnames))
    ## for( i in 1:nc ) {
    ##    safeWins[i] <- sum(bordaScore[i]+fuzz > bordaScore) - 1
    ## }
    ## safeRank <- rank(-safeWins, ties.method="min")

    ## bidirectional cluster heuristic
    safeRank <- rank(-bordaScore, ties.method="min")
    sortedBordaScore <- sort(bordaScore,decreasing=TRUE)
    bordaGaps <- sortedBordaScore - append(sortedBordaScore[-1],0)
    for( i in 2:nc ) {
        if( bordaGaps[i-1] < fuzz ) {
            ## uprank the candidate with i-th highest score
            safeRank[ names(sortedBordaScore[i]) ] <-
                safeRank[ names(sortedBordaScore[i-1]) ]
        }
    }

    ## An extension of Condorcet's criterion to partial rankings:
    ## * Candidate i is more highly ranked than Candidate j
    ## * only if a majority of voters agree with this.
    for( i in 1:nc ){
        for( j in 1:nc ){
            if( safeRank[i] < safeRank[j] ){ # i ranked higher than j
                if( cdc.scores[i,j] < cdc.scores[j,i] ){
                    warning(
                        paste( "Condorcet violation: SafeRank of",
                              cnames[i], "is above", cnames[j], "but",
                              cdc.scores[j,i], "ballots prefer",
                              cnames[j], "and only", cdc.scores[i,j],
                              "ballots prefer", cnames[i] ) )
                }
            }
        }
    }

    result <- structure(list(elected = if(sum(cdc.winner) > 0) cnames[which(cdc.winner)] else NULL,
                             totals = points[,,"Wins"], scores = points[,,"Prefs"], SafeRank = safeRank,
                             fuzz = fuzz, data = x,
                             invalid.votes = votes[setdiff(rownames(votes), rownames(x)),
                                                 , drop = FALSE],
                             corrected.votes = corrected.votes,
                             loser = if(sum(cdc.loser) > 0) cnames[which(cdc.loser)] else NULL,
                             runoff.winner = if(length(runoff.winner) > 0) runoff.winner else NULL,
                             runoff.participants = ro.part.first),
                        class="vote.condorcet")
    if(!quiet) print(summary(result))
    invisible(result)
}

summary.vote.condorcet <- function(object, ...) {
    df <- data.frame(object$totals, stringsAsFactors=FALSE)
    df$Total <- rowSums(object$totals)
    attr(df, "align") <- rep("r", ncol(df))

    df$Score <- rowSums(object$scores) # Borda's scoring
    df$BordaRank <- rank(-df$Score, ties.method="min")
    df$SafeRank <- object$SafeRank
    attr(df, "fuzz") <- object$fuzz

    if(!is.null(object$elected)) {
        df$Winner <- rep("", nrow(df))
        df[object$elected, "Winner"] <- "x"
        attr(df, "align") <- c(attr(df, "align"), "c")
    }
    if(!is.null(object$loser)) {
        df$Loser <- rep("", nrow(df))
        df[object$loser, "Loser"] <- "x"
        attr(df, "align") <- c(attr(df, "align"), "c")
    }
    if(!is.null(object$runoff.participants)) {
        df$Runoff <- rep("", nrow(df))
        df[setdiff(object$runoff.participants, object$runoff.winner), "Runoff"] <- "o"
        if(!is.null(object$runoff.winner))
            df[object$runoff.winner, "Runoff"] <- "x"
        attr(df, "align") <- c(attr(df, "align"), "c")
    }
    attr(df, "number.of.votes") <- nrow(object$data)
    attr(df, "number.of.invalid.votes") <- nrow(object$invalid.votes)
    attr(df, "number.of.candidates") <- nrow(object$totals)
    attr(df, "number.of.seats") <- length(object$elected)
    attr(df, "condorcet.winner") <- object$elected
    attr(df, "condorcet.loser") <- object$loser
    attr(df, "runoff.winner") <- object$runoff.winner
    attr(df, "runoff.participants") <- object$runoff.participants

    class(df) <- c('summary.vote.condorcet', class(df))
    return(df)
}

print.summary.vote.condorcet <- function(x, ...) {
    cat("\nResults of Condorcet voting")
    cat("\n===========================")
    election.info(x)
    print(kable(x, align = attr(x, "align"), ...))

    sortedBordaScore <- sort(x$Score,decreasing=TRUE)
    bordaGaps <- sortedBordaScore - append(sortedBordaScore[-1],0)
    cat("\nSafeRank fuzz on Borda scores:", attr(x, "fuzz"))
    cat("\nGaps in Borda scores: min", min(bordaGaps), "mean",
        mean(bordaGaps), "max", max(bordaGaps), "; all",
        sort(bordaGaps) )

    if(is.null(attr(x, "condorcet.winner")))
        cat("\nThere is no condorcet winner (no candidate won over all other candidates).")
    else
        cat("\nCondorcet winner:", attr(x, "condorcet.winner"))
    if(is.null(attr(x, "condorcet.loser")))
        cat("\nThere is no condorcet loser (no candidate lost to all other candidates).")
    else
        cat("\nCondorcet loser:", attr(x, "condorcet.loser"))
    if(!is.null(attr(x, "runoff.winner")))
        cat("\nRun-off winner:", attr(x, "runoff.winner"))

    cat("\n\n") }

view.vote.condorcet <- function(object, ...)
    view.vote.approval(object, ...)

image.vote.condorcet <- function(x, ...)
    image.vote.stv(x, ...)

