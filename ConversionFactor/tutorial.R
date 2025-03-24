x<-1
y<-2

x+y

z<-x+y

z

x<-c(1,2,3,4,5)
y<-c(2,3,2,2,1)

plot(x, y, type = "b")
plot(x, y, type = "b", ylim=c(0,3))



for (x in 1:10) {
  print(x)
}



fruits <- list("apple", "banana", "cherry")

for (x in fruits) {
  print(x)
}



my_function <- function(fname) {
  paste(fname, "Drgon")
}

my_function("Tomas")