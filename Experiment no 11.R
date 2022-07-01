#Programming in R
library(readxl)
df_1 = read_excel("Autocorrelation_3.xlsx")
View(df_1)
ut=c(7,6,-6,-4,3,-4,-5,-8,10,10,6,-4,-4,-7,-2,6,17,24,13,1,1)
Mean=mean(ut)
Mean
N=length(ut)
N
#Calculation of sample auto-covariance and auto-correlation at lag1 and lag2
c0=var(ut)*((N-1)/N)
c0
c_k=mat.or.vec(2,1)
r_k=mat.or.vec(2,1)
for(i in 1:2){
  c_k[i]=(sum((ut[1:(N-i)]-Mean)*(ut[(1+i):N]-Mean)))/N
  r_k[i]=c_k[i]/c0
}
c_k
r_k
#auto-covariance at lag1 and lag2
cov1=c_k[1]
cov2=c_k[2]
#auto-correlation at lag1 and lag2
r1=r_k[1]
r2=r_k[2]
#Calculation of coefficients phai1 and phai2
A=array(c(r1,r2),dim=c(2,1))
B=array(c(1,r1,r1,1),dim=c(2,2))
coeff=solve(B)%*%A
coeff
phai_1=coeff[1,1]
phai_1
phai_2=coeff[2,1]
phai_2
#Calculation of period of AR(2) process  
theta=acos(phai_1/(2*sqrt(abs(phai_2))))
theta
pi=22/7
period=(2*pi)/theta
period
#Here, in R propgram pi represents 22/7 value
#Calculation of 1st and 2nd term of our fitted AR(2) model
first_term=phai_1*ut[2:20]
second_term=phai_2*ut[1:19]
ut_des_est=first_term+second_term
ut_des_est
ut_est=ut_des_est+Mean
ut_est
df_a = append(0,ut_est)
df_2 = append(0,df_a)
View(df_2)
Data = cbind(df_1,df_2)
Data
#using ggplot to plot the graph
library(ggplot2)
ggp = ggplot(NULL, mapping = aes(t)) +
  geom_point(data = Data, mapping = aes(y=ut), col = "black") + geom_line(data = Data, mapping = aes(y=ut), col = "orange", size = 1) +
  geom_point(data = Data, mapping = aes(y=df_2), col = "blue") + geom_line(data = Data, mapping = aes(y=df_2), col = "green", size = 1) +
  labs(
    title = paste("Plotting of observed ut and estimated ut against t"),
    subtitle = paste("orange_line=observed ut and green_line=estimated ut"),
    caption = "Data from Model",
    x = "t",
    y = "observed_ut_and_estimated_ut"
  )
ggp  
