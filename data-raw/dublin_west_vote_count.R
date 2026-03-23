# regression testing on dublin_west is very time-consuming, due to
# inefficiencies in vote::stv(), so we regress against a stored result

tic("vote::stv count of dublin_west ballots")
dublin_west_vote_count <- 
  vote::stv(dublin_west, complete.ranking = TRUE)
toc()

usethis::use_data(dublin_west_vote_count)
