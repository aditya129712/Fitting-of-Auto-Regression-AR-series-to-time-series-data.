#Programming in R
library(readxl)
df_1 = read_excel("Autocorrelation_2.xlsx")
View(df_1)
ut = c(5.5933,-2.4558,-7.4251,-4.0501,5.1414,5.614,-0.0622,0.7227,0.7997,2.6251,-2.1324,-0.854,-0.7279,2.0284,7.967,6.1003,-1.0224,-2.5796,-2.7597,-5.755,-0.5003,3.533,2.6143,0.7917)
ut
N = length(ut)
N
ut_bar = mean(ut)
ut_bar
c0 = sum((ut-ut_bar)^2)/N
c1 = (sum((ut[1:23]-ut_bar)*(ut[2:24]-ut_bar)))/N
c2 = (sum((ut[1:22]-ut_bar)*(ut[3:24]-ut_bar)))/N
r1 = c1/c0
r2 = c2/c0
A = array(c(r1,r2),dim=c(2,1))
B = array(c(1,r1,r1,1),dim=c(2,2))
coeff = solve(B)%*%A
coeff
theta = acos(coeff[1,1]/(2*sqrt(abs(coeff[2,1]))))
period = (2*pi)/theta
period
f_term = coeff[1,1]*ut[2:23]
s_term = coeff[2,1]*ut[1:22]
utp_est = f_term+s_term
ut_est = utp_est+ut_bar
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



#Programming in R
library(readxl)
df_1 = read_excel("Autocorrelation_2.xlsx")
View(df_1)
ut = c(5.5933,-2.4558,-7.4251,-4.0501,5.1414,5.614,-0.0622,0.7227,0.7997,2.6251,-2.1324,-0.854,-0.7279,2.0284,7.967,6.1003,-1.0224,-2.5796,-2.7597,-5.755,-0.5003,3.533,2.6143,0.7917)
ut
N = length(ut)
N
ut_bar = mean(ut)
ut_bar
c0=sum((ut-ut_bar)^2)/N
c1=(sum((ut[1:23]-ut_bar)*(ut[2:24]-ut_bar)))/N
c2=(sum((ut[1:22]-ut_bar)*(ut[3:24]-ut_bar)))/N
c3=(sum((ut[1:21]-ut_bar)*(ut[4:24]-ut_bar)))/N
r1=c1/c0
r2=c2/c0
r3=c3/c0
A=array(c(r1,r2,r3),dim=c(3,1))
B=array(c(1,r1,r2,r1,1,r1,r2,r1,1),dim=c(3,3))
coeff=solve(B)%*%A
coeff
f_term=coeff[1,1]*ut[3:23]
s_term=coeff[2,1]*ut[2:22]
t_term=coeff[3,1]*ut[1:21]
utp_est=f_term+s_term+t_term
ut_est=utp_est+ut_bar
ut_est
df_a = append(0,ut_est)
df_b = append(0,df_a)
df_2 = append(0,df_b)
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



