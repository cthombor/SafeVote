library(data.table)
x <- data.table(0)
setattr(x,"xa",0)
y <- data.table(1)
# setattr(y,"ya",1)
z <- rbindlist(list(x,y))
attributes(x)
attributes(z)
# I had expected attributes(z) to include at least "xa", or both "xa" and "ya".