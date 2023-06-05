#!/usr/bin/r

# 3 Basic Properties of Matrices
# the deﬁnition of the inverse. In Section 3.6, we will discuss various types of
# generalized inverses and show that A − exists for any n × m matrix A. Here
# we will consider some properties of any generalized inverse.
# From equation (3.149), we see that
A <- c(a = 3.6, b = 3.6, c = 3.6)
A + sin(3.149)

x <- seq(-3, 7, by = 1/8)
tx <- cbind(x, cos(pi*x), cospi(x), sin(pi*x), sinpi(x),
               tan(pi*x), deparse.level=2)
op <- options(digits = 4, width = 90) # for nice formatting
head(tx)
tx[ (x %% 1) %in% c(0, 0.5) ,]
options(op)
