#Vect is a vector from pseudorandom numbers
metvarianza<-function(alfa,vect){
  a<-var(vect);n<-length(vect)
  op_izq <- qchisq(alfa/2, n-1, lower.tail= T) / (12*(n-1))
  op_der <- qchisq(1-alfa/2, n-1, lower.tail= T) / (12*(n-1))
  sum<-c(0)
  for (i in 1:length(vect)) {
    sum <- sum + (vect[i] - mean(vect))^2
  }
  S2 <- (1/(length(vect)-1))*sum
  
  if(S2 >= op_izq && S2 <= op_der) {
    b<-paste("NO SE RECHAZA H0","",sep = "") } else { b<-paste("SE RECHAZA H0","",sep = "") }
  ans<-list(S2,c(op_izq*(12*(n-1)),op_der*(12*(n-1))),b,S2*(12)*(n-1))
  return(ans)
}
alfa<-.05
set.seed(1);vect<-runif(10000)
metvarianza(alfa ,vect)