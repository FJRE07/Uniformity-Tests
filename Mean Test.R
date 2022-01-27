#MÉTODO DE LA MEDIA
#Vect is a vector of pseudorandom numbers
metmedia<-function(alfa,vect){
  a<-mean(vect);N<-length(vect)
  z<-qnorm(1-(alfa/2),0,1)
  stat<-abs((mean(a)-(1/2)))*sqrt(12*N)
  if(stat>z){
    b<-paste("Rechazamos Ho, con est= ",round(stat,6),sep = "")
  } else{
    b<-paste("No rechazamos Ho, con est= ",round(stat,6),sep = "")
  }
  return(b)
}
metmedia(.05,  vect)
