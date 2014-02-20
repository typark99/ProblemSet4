mtable <- matrix(NA, 9, 9)
for (i in 1:9){
  for (j in 1:9){
    mtable[i,j] <- i*j
  }
}
show(mtable)
