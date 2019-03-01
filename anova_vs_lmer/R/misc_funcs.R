#' Make Correlation Matrix
#'
#' \code{cormat} makes a correlation matrix from a vector
#'
#' @param cors the correlations among the variables (can be a single number, vars\*vars matrix, vars\*vars vector, or a vars\*(vars-1)/2 vector)
#' @param vars the number of variables in the matrix
#' 
#' @return matrix
#' @examples
#' cormat(.5, 3)
#' cormat(c( 1, .2, .3, .4,
#'          .2,  1, .5, .6, 
#'          .3, .5,  1, .7,
#'          .4, .6, .7,  1), 4)
#' cormat(c(.2, .3, .4, .5, .6, .7), 4)
#' @export
cormat <- function(cors = 0, vars = 3) {
  # correlation matrix
  if (class(cors) == "numeric" & length(cors) == 1) {
    if (cors >=-1 & cors <=1) {
      cors = rep(cors, vars*(vars-1)/2)
    } else {
      stop("cors must be between -1 and 1")
    }
  }
  
  if (class(cors) == "matrix") { 
    if (!is.numeric(cors)) {
      stop("cors matrix not numeric")
    } else if (dim(cors)[1] != vars || dim(cors)[2] != vars) {
      stop("cors matrix wrong dimensions")
    } else if (sum(cors == t(cors)) != (nrow(cors)^2)) {
      stop("cors matrix not symmetric")
    } else {
      cor_mat <- cors
    }
  } else if (length(cors) == vars*vars) {
    cor_mat <- matrix(cors, vars)
  } else if (length(cors) == vars*(vars-1)/2) {
    # generate full matrix from vector of upper right triangle
    
    cor_mat <- matrix(nrow=vars, ncol = vars)
    upcounter = 1
    lowcounter = 1
    for (col in 1:vars) {
      for (row in 1:vars) {
        if (row == col) {
          # diagonal
          cor_mat[row, col] = 1
        } else if (row > col) {
          # lower left triangle
          cor_mat[row, col] = cors[lowcounter]
          lowcounter <- lowcounter + 1
        }
      }
    }
    for (row in 1:vars) {
      for (col in 1:vars) {
        if (row < col) {
          # upper right triangle
          cor_mat[row, col] = cors[upcounter]
          upcounter <- upcounter + 1
        }
      }
    }
  }
  
  # check matrix is positive definite
  tol <- 1e-08
  ev <- eigen(cor_mat, only.values = TRUE)$values
  if (sum(ev < tol)) {
    stop("correlation matrix not positive definite")
  }
  
  return(cor_mat)
}

#' Multiple Normally Distributed Vectors
#'
#' \code{rnorm_multi} makes multiple normally distributed vectors with specified relationships
#'
#' @param n the number of samples required
#' @param vars the number of variables to return
#' @param cors the correlations among the variables (can be a single number, vars\*vars matrix, vars\*vars vector, or a vars\*(vars-1)/2 vector)
#' @param mu a vector giving the means of the variables (numeric vector of length 1 or vars)
#' @param sd the standard deviations of the variables (numeric vector of length 1 or vars)
#' @param varnames optional names for the variables (string vector of length vars) defaults if cors is a matrix with column names
#' @param empirical logical. If true, mu, sd and cors specify the empirical not population mean, sd and covariance 
#' 
#' @return dataframe of vars vectors
#' @examples
#' rnorm_multi(100, 3, c(0.2, 0.4, 0.5), varnames=c("A", "B", "C"))
#' rnorm_multi(100, 3, c(1, 0.2, -0.5, 0.2, 1, 0.5, -0.5, 0.5, 1), varnames=c("A", "B", "C"))
#' @export

rnorm_multi <- function(n, vars = 3, cors = 0, mu = 0, sd = 1, 
                        varnames = NULL, empirical = FALSE) {
  # error handling
  if ( !is.numeric(n) || n %% 1 > 0 || n < 3 ) {
    stop("n must be an integer > 2")
  }
  
  if (!(empirical  %in% c(TRUE, FALSE))) {
    stop("empirical must be TRUE or FALSE")
  }
  
  if (length(mu) == 1) {
    mu <- rep(mu, vars)
  } else if (length(mu) != vars) {
    stop("the length of mu must be 1 or vars");
  }
  
  if (length(sd) == 1) {
    sd <- rep(sd, vars)
  } else if (length(sd) != vars) {
    stop("the length of sd must be 1 or vars");
  }
  
  cor_mat <- cormat(cors, vars)
  
  sigma <- (sd %*% t(sd)) * cor_mat
  bvn <- MASS::mvrnorm(n, mu, sigma, empirical = empirical)
  df <- data.frame(bvn)
  
  if (length(varnames) == vars) {
    names(df) <- varnames
  } else if (!is.null(colnames(cor_mat))) {
    # if cors was a matrix with names, use that
    names(df) <- colnames(cor_mat)
  }
  
  df
}



cohen_d <- function(x, y, paired = TRUE) {
  # https://t.co/GmRX4y7gCl
  # adapted from https://github.com/Lakens/anchor_based_methods_SESOI/blob/master/effect_size_d_paired_function.R
  
  m_diff <- mean(y-x) # mean difference
  sd1 <- sd(x) #standard deviation of measurement 1
  sd2 <- sd(y) #standard deviation of measurement 2
  s_diff <- sd(y-x) #standard deviation of the difference scores
  N <- length(x) #number of observations of measurement 1
  N2 <- length(y) #number of observations of measurement 2
  
  # design-specific pooled standard deviation and
  # bias correction (unb)
  if (paired) {
    s_av <- sqrt((sd1^2+sd2^2)/2) 
    unb <- 1-(3/(4*(N-1)-1))
  } else {
    ss_x <- sum((x - mean(x))^2)
    ss_y <- sum((y - mean(y))^2)
    Ns <- N + N2 - 2
    s_av <- sqrt((ss_x + ss_y)/Ns)
    unb <- 1-(3/(4*(N+N2)-9))
  }
  
  #Cohen's d_av, using s_av as standardizer
  d_av <- m_diff/s_av
  d_av_unb <- unb*d_av

  d_av_unb
}