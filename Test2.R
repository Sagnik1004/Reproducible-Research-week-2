a<- 1:4
names(a)


b<- c(1,2,3)
names(b)

c<- c(a= 1, b= 2)
names(c)
c

d<- 1:3
names(d)<- c("Tom", "JERRY", "Spike")
d

e= matrix(1:4, nrow = 2, ncol = 2)
dimnames(e)<- list(c('A', 'B'), c('C', 'D'))
e

f<- "fuck"
g<- 'fuck again 3'