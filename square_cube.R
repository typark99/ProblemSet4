n <- 7


cat("number     square      cube\n\n")
for(i in 1:n){
  square <- i^2
  cube <- i^3
  cat(format(i, width=6),
      format(square, width=10),
      format(cube, width=9),
      "\n", sep="")
}


