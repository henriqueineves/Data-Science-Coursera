##Adding a small change in this code.
data <- rpois(1000000, lambda = 5)

tbl <- as.data.frame(matrix(ncol = 3))
colnames(tbl) <- c("Mean", "SD", "N")

a <- 0; i <- 10000
smp_mn <- c()
while (a < i){
  smp <- sample(data, 10, replace = TRUE)
  mn <- mean(smp)
  smp_mn <- c(smp_mn, mn)
  a <- a+1
}
tbl <- rbind(tbl, c(mean(smp_mn), sd(smp_mn), i))

hist(smp_mn, col = "red", main = "10000 smp of 10"); 
abline(v = 125, col = "green", lwd = 5); abline(v = mean(smp_mn), col = "blue", lwd = 5)
