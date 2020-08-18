library(nloptr)

find_bound <-  function(a, b, fT, gT, D, constraints, opts, nlopts) {
  # Check for bad inputs
  if (opts$grid_bound <= 0 | opts$grid_fine <= 0) {
    stop('grid_bound and grid_fine must be positive.')
  }
  
  if (opts$alpha2 < 0 | opts$alpha2 > 0.05) {
    stop('alpha2 must lie between 0 and 0.05.')
  }
  
  # Generate significance level of constraints
  contot <- 0
  for (item in constraints) {
    if (item[[1]] %in% c('resp','covmean')) { contot <- contot+1 }
  }
  if (contot > 0) {
    alpha1 <- (0.025-opts$alpha2/2)/contot
    zstat <- qnorm(1-alpha1)
  }
  
  # Format the selection variables
  N <- NROW(D)
  
  # Normalise selection variables between 0 and 1
  D <- as.matrix(apply(X=D, MARGIN=2, FUN=function(x) {
    x <- (x-min(x))/(max(x)-min(x))
  }))
  
  # Add constant term
  D <- cbind(as.matrix(rep(1,N), ncol=1),D)
  colnames(D)[1] <- 'cons'
  p <- as.numeric(NCOL(D))
  names_alpha <- colnames(D)
  
  # Define the constraint functions
  hin <- function(alpha) {
    # Generate 2^(p-1) combinations of 0 and 1
    A_input <- vector(mode='list', length=(p-1))
    for(k in 1:(p-1)) {
      A_input[[k]] <- 0:1
    }
    A <- as.matrix(expand.grid(A_input))
    
    OUTPUT <- rbind(-A%*%alpha[2:p]-alpha[1]+log(1/a-1), alpha[1]+A%*%alpha[2:p]-log(1/b-1))
    
    for (item in constraints) {
      if (item[[1]] == 'direc') {
        if (item[[3]]=='+') { 
          s <- -1 
        } else if (item[[3]]=='-') { 
          s <- 1 
        } else stop(paste(item[[3]], 'is an invalid direction.',sep=" "))
        index <- which(names_alpha == item[[2]])
        OUTPUT <- rbind(OUTPUT, s*alpha[index])
      }
      
      else if (item[[1]] == 'resp') {
        w <- 1+exp(D%*%alpha)
        r <- item[[2]]
        cn = zstat^2/N
        OUTPUT <- rbind(OUTPUT, -(1+cn)*mean(w-1/r)^2 + cn*mean((w-1/r)^2))
      }
      
      else if (item[[1]] == 'covmean') {
        w <- 1+exp(D%*%alpha)
        Q <- item[[2]]
        qbar <- item[[3]]
        cn = zstat^2/N
        OUTPUT <- rbind(OUTPUT, -(1+cn)*mean(w*(Q - qbar))^2 + cn*mean(w^2*(Q - qbar)^2))
      }
      
      else stop(paste(item[[1]], 'is an invalid option.',sep=" "))
    }
    return(OUTPUT)
  }
  
  hinjac <- function(alpha) {
    # Generate 2^(p-1) combinations of 0 and 1
    A_input <- vector(mode='list', length=(p-1))
    for(k in 1:(p-1)) {
      A_input[[k]] <- 0:1
    }
    A <- as.matrix(expand.grid(A_input))
    
    OUTPUT <- rbind(cbind(-1,-A), cbind(1,A))
    
    for (item in constraints) {
      if (item[[1]] == 'direc') {
        if (item[[3]]=='+') { 
          s <- -1 
        } else if (item[[3]]=='-') { 
          s <- 1 
        } else stop(paste(item[[3]], 'is an invalid direction.',sep=" "))
        index <- which(names_alpha == item[[2]])
        OUTPUT <- rbind(OUTPUT, s*sign(alpha[index])*as.numeric(names_alpha == item[[2]]))
      }
      
      else if (item[[1]] == 'resp') {
        w <- as.vector(1+exp(D%*%alpha))
        wgrad <- D*as.vector(exp(D%*%alpha))
        r <- item[[2]]
        cn = zstat^2/N
        OUTPUT <- rbind(OUTPUT, -2*(1+cn)*mean(w-1/r)*colMeans(wgrad) + 2*cn*colMeans(wgrad*(w-1/r)))
      }
      
      else if (item[[1]] == 'covmean') {
        w <- as.vector(1+exp(D%*%alpha))
        wgrad <- D*as.vector(exp(D%*%alpha))
        Q <- item[[2]]
        qbar <- item[[3]]
        cn = zstat^2/N
        OUTPUT <- rbind(OUTPUT, -2*(1+cn)*mean(w*(Q - qbar))*colMeans(wgrad*(Q - qbar)) + 
                          2*cn*colMeans(w*wgrad*(Q - qbar)^2))
      }
      
      else stop(paste(item[[1]], 'is an invalid option.',sep=" "))
    }
    return(OUTPUT)
  }
  
  # Define domain of parameters
  domain_full <- expand.grid(rep(list(seq(-opts$grid_bound,opts$grid_bound,
                                          opts$grid_fine)),p))
  
  print(paste('Finding the nearest valid parameters from', nrow(domain_full), 
              'starting values...'))
  
  # Find solutions over all feasible starting parameters
  # using the Gauss-Newton algorithm
  alpha_start <- NULL
  for (i in 1:nrow(domain_full)) {
    alpha <- unlist(domain_full[i,])
    j <- 0; cont <- T
    while(TRUE) {
      # Break if constraint is satisfied
      if(all(hin(alpha) >= 0)) break
      
      J <- hinjac(alpha)
      Jinner <- t(J)%*%J
      
      # Break if inner product matrix is singular
      if (rcond(Jinner) < .Machine$double.eps) {
        cont <- F; break
      }
      
      Jinv <- solve(Jinner)
      
      # Break if maxeval reached
      j <- j+1
      if(j == opts$maxeval) {
        cont <- F; break
      }
      
      # Otherwise return feasible alpha
      alpha <- alpha - opts$newton_step*Jinv%*%t(J)%*%hin(alpha)
    }
    
    if (cont) alpha_start <- rbind(alpha_start, t(alpha))
  }
  
  # Check that we found at least one solution
  if (length(alpha_start) == 0) {
    stop('No valid parameters found. Try increasing grid_bound or 
          decreasing grid_fine. If this error persists, there may be 
          no feasible solution to the constraints you have chosen.')
  } else {
    print(paste(
      'Found ', nrow(alpha_start), ' valid starting parameter(s). Finding bounds...',
       sep=''
      ))
  }
  
  # Define functions for NLOPT
  for (bound in c('max','min')) {
    s <- 2*as.numeric(bound=='min')-1
    
    fn <- function(alpha) { 
      w <- as.vector(1+exp(D%*%alpha))
      return(s*sum(w*fT)/sum(w*gT))
    }
    
    gr <- function(alpha) { 
      w <- as.vector(1+exp(D%*%alpha))
      wgrad <- D*as.vector(exp(D%*%alpha))
      return(s*(colSums(wgrad*fT)*sum(w*gT) - colSums(wgrad*gT)*sum(w*fT))/sum(w*gT)^2)
    }
    
    alpha_solve <- t(apply(X=alpha_start, MARGIN=1, FUN=function(x) {
      # Find the optimal solution using Augmented Lagrangian
      suppressMessages(results <- auglag(x0=x, fn=fn, gr=gr, lower=NULL, upper=NULL, 
                                         hin=hin, hinjac=hinjac, localsolver=c("SLSQP"), 
                                         nl.info=F, control=nlopts))
      
      if (any(hin(results$par) < -opts$tol)) stop('Solution is not feasible.')
      
      return(results$par)
    }))
    
    # Pick the solution with the minimum value of the objective
    alpha <- alpha_solve[which.min(apply(X=alpha_solve, MARGIN=1, FUN=fn)),]
    
    # Output solution, beta and standard error
    g <- 1+exp(D%*%alpha) 
    beta <- sum(g*fT)/sum(g*gT)
    assign(paste('beta_', bound, sep=''), beta)
    assign(paste('se_', bound, sep=''), sqrt(mean(g^2*(fT - beta*gT)^2)/(N*mean(g*gT)^2)))
  }
  
  OUTPUT <- list(
    beta_min=beta_min,
    se_min=se_min,
    beta_max=beta_max,
    se_max=se_max
  )
  return(OUTPUT)
}
