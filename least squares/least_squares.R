#create noisy data

n<-20 #number of points
noise_mean<-1 #mean of noise
noise_sd<-10 #sd of noise

x<-c(1:n)
noise<-rnorm(n, mean=noise_mean, sd=noise_sd)
y <- x + noise

plot(x,y)

df<-data.frame(x,y)

#use method of least squares to fit regression line
model <- lm(y ~ x, data=df)

#view regression model summary
summary(model)

abline(model)

#or ggplot

library(ggplot2)

ggplot(df, aes(x=x, y=y)) + 
  geom_point(color='#2980B9', size =3) + 
  geom_smooth(method=lm, color='#2C3E50')