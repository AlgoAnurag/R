# Q1 (a)
a = 0
b=60
p=1/(b-a)

p_x_graeterthan_45 = p*(b-45)
p_x_graeterthan_45
#(b)
P_20_and_30 = p* (30-20)
P_20_and_30

##################################Q2##################################

#a
lambda <- 1/2
x <- 3
pdf_value <- lambda * exp(-lambda * x)
pdf_value

#b
lambda <- 1/2
curve(lambda * exp(-lambda * x), from = 0, to = 5, col = "blue", lwd = 2,
      xlab = "x", ylab = "Density", main = "Exponential Distribution (λ = 1/2)")
#c 
lambda <- 1/2
x <- 3
cdf_value <- 1 - exp(-lambda * x)
cdf_value

#d
lambda <- 1/2
curve(1 - exp(-lambda * x), from = 0, to = 5, col = "red", lwd = 2,
      xlab = "x", ylab = "Cumulative Probability", main = "Cumulative Exponential Distribution (λ = 1/2)")
#e
lambda <- 1/2
n <- 1000
simulated_data <- rexp(n, rate = lambda)

# Plot histogram of the simulated data
hist(simulated_data, breaks = 30, col = "lightblue", main = "Simulated Exponential Distribution",
     xlab = "Repair Time", ylab = "Frequency")


#Q3***************************************************

# Parameters
alpha <- 2  # Shape
beta <- 1/3 # Rate

# (a)(i) Probability density at X = 3
x <- 3
pdf_at_3 <- dgamma(x, shape = alpha, rate = 1/beta)
pdf_at_3

# (a)(ii) Probability that X >= 1
x <- 1
prob_gte_1 <- 1 - pgamma(x, shape = alpha, rate = 1/beta)
prob_gte_1

# (b) Find c such that P(X <= c) >= 0.70
threshold <- 0.70
c <- qgamma(threshold, shape = alpha, rate =1/ beta)
c
#ass6
#Q1 
#i
library(pracma)

f_xy <- function(x, y) {
  return(2 * (2*x + 3*y)/5)  
}
result <- integral2(f_xy, 0, 1, 0, 1)$Q
result

#ii
g_x_at_1 <- function(y) {
  return((2 * (2 + 3 * y)) / 5)
}
result_g_1 <- integral(g_x_at_1, 0, 1) 
result_g_1

#iii
h_y_at_0 = function(x,y){4*x/5}

integral(h_y_at_0,0,1)
#iv
f_xy <- function(x, y) {
  return(x*y*2 * (2*x + 3*y)/5)  
}
result <- integral2(f_xy, 0, 1, 0, 1)$Q
result


#Q2****************************************************
#i
func <- function(x,y){
  return ((x+y)/30)
}
jointWaaliMatrix <- matrix(c(func(0,0:2),func(1,0:2),func(2,0:2),func(3,0:2)),nrow = 4,ncol = 3,byrow=TRUE)
jointWaaliMatrix

#ii
sum(jointWaaliMatrix)

#iii , iv
g=apply(jointWaaliMatrix,1,sum)
h=apply(jointWaaliMatrix,2,sum)       # 1 for sum of row and 2 is column
g
h
#v
jointWaaliMatrix[1,2]/apply(jointWaaliMatrix,2,sum)[2]
#vi

x<-c(0:3)
EX<-sum(x*g)
EX

y<-c(0:2)
EY<-sum(y*h)
EY


fun2<-function(x,y){
  x*y*(x+y)/30
}
mat2<-matrix(c(fun2(0,0:2),fun2(1,0:2),fun2(2,0:2),fun2(3,0:2)),nrow=4,ncol=3,byrow=TRUE)
eXY<-sum(mat2)
eXY        

EX_square<-sum(x*x*g)
VarX=EX_square-(EX)*EX
VarX

EY_square<-sum(y*y*h)
VarY=EY_square-(EY)*EY
VarY

fun2<-function(x,y){
  x*y*(x+y)/30
}
mat2<-matrix(c(fun2(0,0:2),fun2(1,0:2),fun2(2,0:2),fun2(3,0:2)),nrow=4,ncol=3,byrow=TRUE)
eXY<-sum(mat2)

covariance<-eXY-EX*EY
covariance

correlation<-covariance/(sqrt(VarX*VarY))
correlation
#ass7
#Q1

f=rt(100,99)
hist(f)

hist(rt(100,99))
#Q2
hist(rchisq(100,2))
hist(rchisq(100,10))
hist(rchisq(100,25))

#Q3
seq(-6,6,length=100)
t_df1 <- dt(x, df = 1)
t_df4 <- dt(x, df = 4)
t_df10 <- dt(x, df = 10)
t_df30 <- dt(x, df = 30)

plot(x,dt(x,30),type = "l",col="red")
lines(x,dt(x,1),type = "l",col = "green")
lines(x,dt(x,4),type = "l",col = "pink")
lines(x,dt(x,10),type = "l",col = "blue")

#Q4
#i
percentile_95 <- qf(0.95, 10, 20)
percentile_95
#ii
v1 <- 10
v2 <- 20
area_0_to_15 = pf(15, v1, v2)
area_15_to_inf = 1 - area_0_to_15
area_0_to_15
area_15_to_inf
#iii
qf(0.25,10,20)
qf(0.5,10,20)
qf(0.75,10,20)
qf(0.999,10,20)
#iv
hist(rf(1000,10,20))

#ass8
#Q1
data=read.csv("C:/Users/ragsi/OneDrive/Desktop/5 th SEM/Probability/Clt-data.csv")
head(data,10)
nrow(data)
mean(data$Wall.Thickness)
hist(data$Wall.Thickness)
# Add a vertical line for the mean
abline(v = mean(data$Wall.Thickness), col = "red", lwd = 2, lty = 2)

#Q2
age = c(58, 69, 43, 39, 63, 52, 47, 31, 74, 36)
cholesterol = c(189, 235, 193, 177, 154, 191, 213, 165, 198, 181)
plot(age,cholesterol)
model =lm(cholesterol ~ age) 
abline(model, col = "red", lwd = 2)

predict(model,age_60)

#Q3
before <- c(145, 173, 158, 141, 167, 159, 154, 167, 145, 153)
after <- c(155, 167, 156, 149, 168, 162, 158, 169, 157, 161)

differences <- after - before
differences
t.test(differences)






