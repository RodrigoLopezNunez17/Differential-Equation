# install.packages("glue")
# install.packages("Ryacas") 
# install.packages("Stringi")
# This is the way you'd install any of the libraries if necessary.

library(Ryacas) # I import the Ryacas library, so that i'll be able to make simbolic calculus.
library(glue) # I import the glue library, so that i'll be able to handle with strings like f strings in Python.
library(stringi)

# I define the coefficient matrix A througout a function which takes
# the matrix elements directly of the user. 1, 2, 1, 0
DefineMatrix <- function(){
    matrixElements <- c()
    for(i in 1:4){
        matrixElements <- append(matrixElements, as.integer(readline(prompt = glue("What's the {i}th element of the matrix?"))))
    }

    matrix <- matrix(data = matrixElements, ncol = 2, nrow = 2, byrow = TRUE)
    print("This is your matrix:")
    print(matrix)
    return(matrix)
}

A <- DefineMatrix()

# I define the first tow elements of the series.
a1 <- as.integer(readline(prompt = "What's the first element of the series?")) # 1
a2 <- as.integer(readline(prompt = "What's the second element of the series?")) # 2

# The function is created with the intention of make a diagonalization of a matrix, returning a list of the C, D, and C_inv matrices. 
# You can access them, by "$C", "$D" and "$C_inv" respectively.
DiagonalizeMatrix <- function(A){
    eigenvalues <- eigen(A)$values
    eigenvectors <- eigen(A)$vectors

    D <- diag(eigenvalues)

    C <- eigenvectors

    C_inv <- solve(C)

    A_diag <- C %*% D %*% C_inv

    return(list(C = C, D = D, C_inv = C_inv))
}

# I define the diagonalization of the A matrix.
A_diag <- DiagonalizeMatrix(A)

# Constructing the equivalent expression of the initial equation. Furthermore, here we're using the "glue" library, so that we can handle 
# strings in a comfortable way, at least that's my opinion.
C_exp <- glue("{ {<<A_diag$C[1, 1]>> , <<A_diag$C[1, 2]>>} , {<<A_diag$C[2, 1]>>,  <<A_diag$C[2, 2]>>} }", .open = "<<", .close = ">>")
D_exp <- glue("{ {<<A_diag$D[1, 1]>>^(n - 2) , 0} , {0 , <<A_diag$D[2, 2]>>^(n - 2)} }", .open = "<<", .close = ">>")
C_inv_exp <- glue("{ {<<A_diag$C_inv[1, 1]>>, <<A_diag$C_inv[1, 2]>>}, {<<A_diag$C_inv[2, 1]>>, <<A_diag$C_inv[2, 2]>>}}", .open = "<<", .close = ">>")
v_exp <- glue("{ {<<a2>>, 0}, {<<a1>>, 0}}", .open = "<<", .close = ">>")

eq <- glue("{C_exp} * {D_exp} * {C_inv_exp} * {v_exp}")

# Here, we use the "Ryacas" library so that we can reduce the expression, previously created, doing symbolic calculus.
result <- yac_str(glue("Simplify({eq})"))

# Here, i define a function which takes as a parameter a string, and this function will be reducing the last expression to the general formula,
# this is considering that the result is already in the result variable, but in a vector form. Here, we're just taking the upper element and 
# reassining it to a new variable. The decision of creating a function and not a simple "foor loop" is to make the code reusable, so that 
# may be helpful in the future.
GeneralFormula <- function(string){
    closeBrakets <- 0
    index <- 1
    ans <- ""
    for(i in strsplit(string, "")[[1]]){
        index <- index + 1
        ans <- paste(ans, i)

        if(i == "}"){
            closeBrakets <- closeBrakets + 1
        }

        if(closeBrakets == 1){
            break
        }
    }

    ans <- stri_sub(ans, from = 1, length = index + 6)
    ans <- stri_replace_all_fixed(ans, "{", "")
    ans <- stri_replace_all_fixed(ans, " ", "")
    return(ans)
}

# Defining the last version of the general formula.
formula <- GeneralFormula(result)

print(paste("A general formula for the difference equation is: ", formula))

# Continuing, i'll prove that this procedure was correctly developed and that the 
# formula is the right.

# Here, i define manually the first 36 elements of the series and save them in a vector.
SeriesElements <- c(1, 2)
for(i in 1:34){
    a_n_1 <- SeriesElements[length(SeriesElements)]
    a_n_2 <- SeriesElements[length(SeriesElements) - 1]
    SeriesElements <- append(SeriesElements, a_n_1 + (2 * a_n_2))
}

# Here, we are defining a function which will compare all the values of the "SeriesElements"
# ,which is the vector with all the elements gotten manually; angainst the value that the
# formula return. If the case is true for all the values the function will print "All the 
# values are equal." and return "TRUE"; otherwise will print "The values aren't the same" and
# will return "FALSE".
Comprobation <- function(){
    for(n in length(SeriesElements)){
        nth_formula <- stri_replace_first_fixed(formula, "n", n)
        nth_formula <- eval(parse(text = nth_formula)) # eval(parse()) help us execute code in string form.
        
        nth_manually <- SeriesElements[n]

        if(nth_formula ==! nth_manually){
            print("The values aren't the same")
            return(FALSE)
        }
    }
    print("All the values are equal.")
    return(TRUE)
}

print(paste("In conclusion, the the method is", Comprobation()))