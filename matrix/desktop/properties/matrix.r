#!/usr/bin/r

# There are other useful properties, which we mention below. There are also
# some interesting properties of certain important random matrices partitioned
# in this way. For example, suppose A 22 is k × k and A is an m × m Wishart
# matrix with parameters n and Σ partitioned like A in equation (3.142). (This
# of course means A is symmetrical, and so A 12 = A T
# 21 .) Then Z has a Wishart
# −1
# Σ 12 , and is inde-
# distribution with parameters n − m + k and Σ 22 − Σ 21 Σ 11
# pendent of A 21 and A 11 . (See Exercise 4.8 on page 171 for the probability
# density function for a Wishart distribution.)
A22 <- c(k = 3.142, k = 3.142, k = 3.142)
A <- c(m = 3.142, m = 3.142, m = 3.142)
A12 <- c(A = 3.142, T = 3.142, Z= 3.142)

A22 - 22
A - 21
A12 - 11

# 3.4.1 Inverses of Partitioned Matrices
# Suppose A is nonsingular and can be partitioned as above with both A 11 and
# A 22 nonsingular. It is easy to see (Exercise 3.13, page 141) that the inverse of
# A is given by
A11 <- c(A22, strict = TRUE)
A22 + A11
page(141)
page(utils::page)
fil <- tempfile()
## Write an ASCII version of the 'base' function mean() to our temp file, ..
dput(base::mean, fil)
## ... read it back into 'bar' and confirm it is the same
bar <- dget(fil)
stopifnot(all.equal(bar, base::mean))

## Create a function with comments
baz <- function(x) {
  # Subtract from one
  1-x
}
## and display it
dput(baz)
## and now display the saved source
dput(baz, control = "useSource")

## Numeric values:
xx <- pi^(1:3)
dput(xx)
dput(xx, control = "digits17")
dput(xx, control = "hexNumeric")
dput(xx, fil); dget(fil) - xx # slight rounding on all platforms
dput(xx, fil, control = "digits17")
dget(fil) - xx # slight rounding on some platforms
dput(xx, fil, control = "hexNumeric"); dget(fil) - xx
unlink(fil)

xn <- setNames(xx, paste0("pi^",1:3))
dput(xn) # nicer, now "niceNames" being part of default 'control'
dput(xn, control = "S_compat") # no names
## explicitly asking for output as in R < 3.5.0:
dput(xn, control = c("keepNA", "keepInteger", "showAttributes"))
# 3 Basic Properties of Matrices
# where Z is the Schur complement of A 11 .
Z <- A11
require(stats)

ts(1:20)  #-- print is the "Default function" --> print.ts(.) is called
for(i in 1:3) print(1:i)

## Printing of factors
attenu$station ## 117 levels -> 'max.levels' depending on width

## ordered factors: levels  "l1 < l2 < .."
esoph$agegp[1:12]
esoph$alcgp[1:12]

## Printing of sparse (contingency) tables
set.seed(521)
t1 <- round(abs(rt(200, df = 1.8)))
t2 <- round(abs(rt(200, df = 1.4)))
table(t1, t2) # simple
print(table(t1, t2), zero.print = ".") # nicer to read

## same for non-integer "table":
T <- table(t2,t1)
T <- T * (1+round(rlnorm(length(T)))/4)
print(T, zero.print = ".") # quite nicer,
print.table(T[,2:8] * 1e9, digits=3, zero.print = ".")
## still slightly inferior to  Matrix::Matrix(T)  for larger T

## Corner cases with empty extents:
table(1, NA) # < table of extent 1 x 0 >
# and is partitioned as in equation (3.43) on page 61 and X is of full column
# rank, then the Schur complement of X T X in [X y] T [X y] is
eq <- c(a = 3.43, x = 3.43, y = 3.43)
rank(eq * A11 * A22)
length(eq)

## Not run:
# use xedit on the function mean and assign the changes
mean <- edit(mean, editor = "xedit")

## End(Not run)
# This particular partitioning is useful in linear regression analysis, where this
# Schur complement is the residual sum of squares and the more general Wishart
# distribution mentioned above reduces to a chi-squared one. (Although the
# expression is useful, this is an instance of a principle that we will encounter
# repeatedly: the form of a mathematical expression and the way the expression
# should be evaluated in actual practice may be quite diﬀerent.)
exp(c(eq, A11, A22))

# 3.4.2 Determinants of Partitioned Matrices
# If the square matrix A is partitioned as
A + exp(c(eq, A11, A22))
length(A)

# and A 11 is square and nonsingular, then
A11 + sqrt(A)
length(A11)

# that is, the determinant is the product of the determinant of the principal
# submatrix and the determinant of its Schur complement.
drop(A11)

# This result is obtained by using equation (3.29) on page 54 and the fac-
# torization
eq + c(a = 3.29, b = 3.29, c = 3.29)

# The factorization in equation (3.148) is often useful in other contexts as well.
eq + c(a = 3.148, p = 3.148, p = 3.148)

# 3.5 Linear Systems of Equations
# Some of the most important applications of matrices are in representing and
# solving systems of n linear equations in m unknowns,
line(eq, y = NULL, iter = 1)

# where A is an n × m matrix, x is an m-vector, and b is an n-vector. As
# we observed in equation (3.59), the product Ax in the linear system is a
# linear 
# combination of the columns of A; that is, if a j is the j th column of A,
# m
# Ax = j=1 x j a j .
A + c(n = 3.59, m = 3.59, ax = 3.59)
Ax <- c(j = 1, x = 2, j = 3)
A + Ax

# If b = 0, the system is said to be homogeneous. In this case, unless x = 0,
# the columns of A must be linearly dependent.
b <- 0
if (b != 0) {
    line(b, y = NULL, iter = 1)
}

# 3.5.1 Solutions of Linear Systems
# When in the linear system Ax = b, A is square and nonsingular, the solution is
# obviously x = A −1 b. We will not discuss this simple but common case further
# here. Rather, we will discuss it in detail in Chapter 6 after we have discussed
# matrix factorizations later in this chapter and in Chapter 5.
Ax + b + A
x <- A - 1 + b
factorial(x)

# When A is not square or is singular, the system may not have a solution or
# may have more than one solution. A consistent system (see equation (3.105))
# has a solution. For consistent systems that are singular or not square, the
# generalized inverse is an important concept. We introduce it in this section
# but defer its discussion to Section 3.6.
buffer <- x

# Underdetermined Systems
# A consistent system in which rank(A) < m is said to be underdetermined.
# An underdetermined system may have fewer equations than variables, or the
# coeﬃcient matrix may just not be of full rank. For such a system there is
# more than one solution. In fact, there are inﬁnitely many solutions because if
# the vectors x 1 and x 2 are solutions, the vector wx 1 + (1 − w)x 2 is likewise a
# solution for any scalar w.
m <- outer(eq, eq, FUN = "*")
rank(A) < m
as.vector(x, mode = "any")
buffer + m
wx <- c(a = 1, c(b = 1, w = -1), x = 2)
wx + scale(m)
