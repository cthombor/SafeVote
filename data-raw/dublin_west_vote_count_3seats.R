# regression testing on dublin_west is very time-consuming, due to
# inefficiencies in vote::stv(), so we regress against a stored result

tic("vote::stv count of dublin_west ballots with 3 seats")
dublin_west_vote_count_3seats <- 
  vote::stv(dublin_west, nseats = 3, complete.ranking = TRUE)
toc()

usethis::use_data(dublin_west_vote_count_3seats)
