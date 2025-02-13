library(MASS)


# We define the coefficient matrix A.

A <- matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2, byrow=TRUE)

eigenvalues <- eigen(A)$values

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


(3 - sqrt(5))/2
(-sqrt(5) - 1)/2

B <- matrix(c(0.381966, 1, 1, -1.61834), nrow = 2, ncol = 2, byrow = TRUE)

solve(B)


a <- matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2, byrow= TRUE)

b <- matrix(c(1, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)

a_eigen <- eigen(a)
a_eigenvalues <- a_eigen$values
a_eigenvectors <- a_eigen$vectors

d <- diag(a_eigenvalues)

c <- a_eigenvectors

c_inv <- solve(c)

c %*% d %*% c_inv

eigen(t(b))

m <- matrix(c(1, 1, 1, 2), nrow = 2, ncol = 2, byrow = TRUE)

eigen(m)