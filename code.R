A<-matrix(c(1,2,0,2,1,0,0,0,1), 3)
w <- c(1,0,0)
T <- cbind(w, A%*%w, A%*%A%*%w);
rref(T)

w <- c(0,0,1)
T <- cbind(w, A%*%w, A%*%A%*%w);
rref(T)

w <- c(0,1,0)
T <- cbind(w, A%*%w, A%*%A%*%w);
rref(T)

w<-c(0,1,1)
T <- cbind(w, A%*%w, A%*%A%*%w, A%*%A%*%A%*%w);
rref(T)

p <- function(t) t^2 - 2*t -3
curve(p(x), from = -1, to = 4); abline(h=0, col = "red")

p <- function(t) t^3-3*t^2-t+3
curve(p(x), from = -1, to = 4); abline(h=0, col = "red")


w<-c(1,0,0)
I <- diag(c(1,1,1))
v0 <- (A - I)%*%(A - 3*I)%*%w; v0
A%*%v0; -1*v0      #eigenvector for eigenvalue -1
v2 <- (A + I)%*%(A - I)%*%w; v2
A%*%v2; 3*v2      #eigenvector for eigenvalue 3

w<-c(0,0,1)
v1 <- (A + I)%*%(A - 3*I)%*%w; v1
B%*%v1; 1*v1      #eigenvector for eigenvalue 1

#2.
A<-matrix(c(4,-3,6,12,-11,24,6,-6,13), 3)

w <- c(1,0,0)
T <- cbind(w, A%*%w, A%*%A%*%w);
rref(T)

w <- c(0,0,1)
T <- cbind(w, A%*%w, A%*%A%*%w);
rref(T)

w <- c(0,1,0)
T <- cbind(w, A%*%w, A%*%A%*%w);
rref(T)


#p(t) = (t-4)(t-1)

w<-c(1,0,0)
I <- diag(c(1,1,1))
v0 <- (A - 4*I)%*%w; v0
A%*%v0; 1*v0      #eigenvector for eigenvalue 1
v1 <- (A - I)%*%w; v1
A%*%v1; 4*v1      #eigenvector for eigenvalue 4

#3. 
A<-matrix(c(1,-1,-2,-2,1,1,2,0,-1,2,1,1),4); A
B<-t(A)%*%A; B
C<-A%*%t(A); C

w <- c(1,0,0)
T <- cbind(w, B%*%w, B%*%B%*%w, B%*%B%*%B%*%w);
rref(T)

w <- c(0,1,0)
T <- cbind(w, B%*%w, B%*%B%*%w, B%*%B%*%B%*%w);
rref(T)

w <- c(0,0,1)
T <- cbind(w, B%*%w, B%*%B%*%w, B%*%B%*%B%*%w);
rref(T)


p <- function(t) t^3-23*t^2+98*t-92
curve(p(x), from = -1, to = 20); abline(h=0, col = "red")
eigen(B)$value

w <- c(1,0,0,0)
T <- cbind(w, C%*%w, C%*%C%*%w, C%*%C%*%C%*%w, C%*%C%*%C%*%C%*%w);
rref(T)

w <- c(0,1,0,0)
T <- cbind(w, C%*%w, C%*%C%*%w, C%*%C%*%C%*%w, C%*%C%*%C%*%C%*%w);
rref(T)

w <- c(0,0,1,0)
T <- cbind(w, C%*%w, C%*%C%*%w, C%*%C%*%C%*%w, C%*%C%*%C%*%C%*%w);
rref(T)

w <- c(0,0,0,1)
T <- cbind(w, C%*%w, C%*%C%*%w, C%*%C%*%C%*%w, C%*%C%*%C%*%C%*%w);
rref(T)

p <- function(t) t^3-23*t^2+98*t-92
curve(p(x), from = -1, to = 20); abline(h=0, col = "red")
eigen(B)$value

