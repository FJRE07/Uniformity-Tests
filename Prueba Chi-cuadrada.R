alfa<-.05
num<-1000
set.seed(1);gen<-runif(10000)
pruebaJi<-function(alfa,num,gen){
  n<-length(gen)
  ord<-sort(gen,decreasing = FALSE)
  Int<- c(1:num)
  result <- matrix(0, nrow= n, ncol= num)
  prueba <- matrix(0, nrow= 1, ncol= 2)
for (i in 1:num) {
  for(j in 1:n)
    if (ord[j] > ((i-1)/num) && ord[j] <= (i/num)) {
      result[j,i] <- 1
    } else {
      result[j,i] <- 0
    }
}
  conteo <- t(as.matrix(apply(result, 2, sum)))
  colnames(conteo) <- Int
  ji_obs <- (num/n)*sum((conteo - (n/num))^2)
  ji_real <- qchisq(alfa, num-1, lower.tail= F)
  colnames(prueba) <- c("Chi Simulada", "Chi Teórica")
  prueba[1,1] <- ji_obs
  prueba[1,2] <- ji_real
  if( ji_obs > ji_real) {
    print("Se Rechaza H0") } else { print("NO se Rechaza H0") }
    print(prueba)
}
pruebaJi(alfa,num,gen)