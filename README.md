# Difference Equations.

Along the time, there's been a number of phenomena that human beings have been attracted to. One of the forms, we found are difference equations, one of the most known examples is Fibonacci Series, which consists on summing the two previous terms of the series:

$$
F_n = F_(n - 1) + F_(n - 2)
$$

This type of series have an inconvenient, we need to calculate all the terms before our nth or in such case, we'd need to know an itermediate pair of terms belonged to the series. Here's where the method i want to discuss come which consists on, using the difference equation, get a universal formula which is in function of the nth term and not to the previous terms.

### First Step:

Knowing the difference equation we want to work with, we apply a little trick which consists on defining a new variable, in this case let's call it:

$$
b_n = F_(n - 1)
$$

So that, now we have a system of equations:

$$
\begin{cases} 
F_n = F_(n - 1) + b_(n - 1) \\ b_n = F_(n - 1) \end{cases}
$$

Once we have this system, using linear algebra we can obtain a matrix of coefficients and the system will be reexpressed as follows:

$$
\begin{pmatrix} F_n \\ b_n  \end{pmatrix} = \begin{pmatrix} 1 && 1 \\ 1 && 0 \end{pmatrix} \begin{pmatrix} F_(n - 1) \\ b_(n-1) \end{pmatrix}
$$

### Second Step:

Then, since we have already a equation with its coefficient matrix. We can use this last one and make a diagonalization of the matrix so that, we would have an equavalent equation of the one ew already have. To do so, we need to get three matrices: **C**, **D**, and **C_inv**. To get C, we need to get the eigenvectors, so that we need the eigenvalues. In this example, i'll be relying on R language:

```R language
A <- matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)

eigenvalues <- eigen(A)$values
> [1] 1.618034 -0.618034

Prime <- function(n){
    isPrime <- TRUE

    if(n == 2){
        return(TRUE)
    }

    if(n <= 1){
        isPrime <- FALSE;
        return(FALSE)
    } else {
        for( i in 2:(n - 1)){
            if(n %% i == 0){
                isPrime <- FALSE
                break
            }
            }
    }
    return(isPrime)
} # The intention of defining a function which detects prime numbers is to 
# be able of solving the system of equations in a manual way.

Primes_1_100 <- c();
for(i in 1:100){
    if( Prime(i) == TRUE){
        print(paste("The number ", i, " is prime"))
        Primes_1_100 <- append(Primes_1_100, i)
    }
}

MCM <- function(n, m){
    mcm <- 0
    divisor <- 1
    if (n > m){
        while(Primes_1_100[divisor] < n){
            if((n %% Primes_1_100[divisor] == 0) && (m %% Primes_1_100[divisor])){
                mcm <- Primes_1_100[divisor]
            } else {
                divisor <- divisor + 1
            }
        }
    } else if(m > n){
        while(Primes_1_100[divisor] < m){
            if((n %% Primes_1_100[divisor] == 0) && (m %% Primes_1_100[divisor])){
                mcm <- Primes_1_100[divisor]
            } else {
                divisor <- divisor + 1
            }
        }
    }
    return(mcm)
}

EigenVectors <- function(A, eigenvalues){
    m1 <- c()
    m2 <- c()
    for(eigenvalue in eigenvalues){
        # print(paste("THIS IS THE EIGENVALUE:", eigenvalue))
        for(i in 1:nrow(A)){
            for(j in 1:ncol(A)){
                if(eigenvalue == eigenvalues[1]){
                    if( i == j){
                        m1 <- append(m1, A[i, j] - eigenvalue)
                    } else {
                        m1 <- append(m1, A[i, j])
                    }
                }
                if(eigenvalue == eigenvalues[2]){
                    if( i == j){
                        m2 <- append(m2, A[i, j] - eigenvalue)
                    } else {
                        m2 <- append(m2, A[i, j])
                    }
                }
            }
        }
    }
    # print(paste("This is the matrix respectively to the eigenvalue: ", eigenvalues[1]))
    m1 <- matrix(m1, nrow = 2, ncol = 2, byrow = TRUE)
    # print(m1)

    # print(paste("This is the matrix respectively to the eigenvalue: ", eigenvalues[2]))
    m2 <- matrix(m2, nrow = 2, ncol = 2, byrow = TRUE)
    # print(m2)

    # Until this point i've reached the idea of getting the matrices according to each of the eigenvalues. Then i'll continue to get the respective vectors.

    for(matrix in 1:2){
        if(matrix == 1){
            print("These are vectors from the first matrix")
            m1_1 <- as.vector(m1[1, ])
            m1_2 <- as.vector(m1[2, ])
            print(m1_1)
            print(m1_2)

          
        } else {
            print("These vectors are from the second matrx")
            m2_1 <- as.vector(m2[1, ])
            m2_2 <- as.vector(m2[2, ])
            print(m2_1)
            print(m2_2)
        }
    }
}
```
