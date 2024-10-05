#########################################
### Background parameter "gamma" ########
#########################################



### the maximum dose is assigned as a upper bound of gamma.
### if gamma hit its upper bound, users will be warned.

max_bound_gamma <- function(){

   ### get the original dose
   dose <-data.char$data[,colnames(data.char$data)==required['conc']];
   dose <-as.numeric(dose);

   ### 0 was replaced by 1e-10 to avoid being deleted due to log
   dose[dose==1e-10]<-0.;

   ### upper bound of gamma
   max_gamma <- min(dose[dose!=0]);

   if(gammam==max_gamma || gammam==as.character(max_gamma)){
     cat("\nWarning: Gamma hit its maximum bound!!!\n", file = stderr());
   }
}


### Estimate gamma

est_gamma <- function(data){

  ldose  <- data$x[,grep("LG10CONC", colnames(data$x))];
  dose <- 10^ldose;

  ### to get an upper bound for gamma
  upper <-data.char$data[,colnames(data.char$data)==required['conc']];
  upper <-as.numeric(upper);
  upper[upper==1e-10]<-0.;

  ### an arbitrary lower bound
  lower_bound_gamma <- 0.001;
  ### an arbitrary upper bound
  upper_bound_gamma <-  min(upper[upper!=0]);

  beg <- lower_bound_gamma;
  end <- upper_bound_gamma;

  ### number of intervals to narrow down MLE of gamma
  step <- 60;           

  ### 1st column is gamma; 2nd is deviance
  deviance <- matrix(0, nrow=step+1, ncol=2);  

  ### minumum deviance is equivalent to maximum likelihood
  min_sp <- 0;  

  for(j in 1:5){

    ### narrow down by decreasing interval
    int <- (end-beg)/step;   

    old_dev <- min_sp;    

    for(i in 0:step){

      ### a candidate of gamma  
      gamma <- beg+int*i;  

      ### its corresponding deviance 
      dev <- devianceg(data, pmodel, total_entry, gamma);  
      deviance[i+1,1] <- gamma;
      deviance[i+1,2] <- dev;
    }
    ### fit a spline
    sp <- spline(deviance[,1],deviance[,2]);  
    ### find a minimu value of the spline
    min_sp <- min(sp$y)                       
    ### fina a correponding gamma
    min_gamma <- median(sp$x[sp$y==min_sp])   

    ### assign a new lower of interval
    beg <- ifelse(min_gamma-int>0, min_gamma-int, lower_bound_gamma);    

    ### assign a new upper of interval
    end <- ifelse(min_gamma+int<upper_bound_gamma, min_gamma+int,upper_bound_gamma);  

    ### convergence criterion
    cr <- (min_sp - old_dev)/min_sp;  

    if(cr<0.00001) break;

  }

  return(min_gamma)

}

################################
# Return a deviance using "Init" #
################################

devianceg <- function(data, pmodel, total_entry, gamma.temp) {

  ### keep adding a new gamma in the design matrix and fitting model with lgo()
  ldose  <- data$x[,grep("LG10CONC", colnames(data$x))];

  ### dose 0 was replaced by 1e-10, so need to be substracted
  ldoseg <- log10(exp(ldose*log(10))+gamma.temp - 10^(-10)*(ldose==-10));

  ### update log(dose+gamma) with a new gamma
  data$x[,grep("LG10CONC", colnames(data$x))] <- ldoseg;

  fits <- lgo(data, pmodel, total_entry);

  return(fits$deviance);
}



###########################################################
# "lgo":                                                  #
# to calculte deviance all menus are turned off           #
# modified from "go"                                      # 
###########################################################

lgo <- function(data, model="cumulative odds model", total_entry,  getvar=TRUE, verbose=TRUE) 
{

  ylo <- data$y$ylo
  yhi <- data$y$yhi
  nsevcat <- max(c(data$y$yhi, data$y$ylo)) + 1
  X <- as.matrix(data$x)

  if (ncol(X) > 1) {
    Xnames <- dimnames(data$x)[[2]][-1]
    X <- as.matrix(X[,-1])
  }  else {
    X <- Xnames <- NULL
  }

  gpscale <- data$gpscale

  if (length(gpscale)==0) {
    gpscale <- rep(1, length(ylo))
  }

  layer.names <- data$study.label
  weight <- data$weight
  link <- data$link

  if(nsevcat != length(unique(c(yhi, ylo)))){
     miss <- seq(nsevcat) - 1
     miss <- miss[is.na(match(miss, unique(c(yhi, ylo))))]
  }

  if(is.null(data$strata.factors)){
    ref.cell <- NULL
  }  else {
    ref.cell <- data$strata.coefnames[1,"INTERCEPT"]
  }

  layers <- charmatch(layer.names, unique(layer.names))

  startcoefs <- lpseudo(ylo,yhi,nsevcat,X,link,weight,gpscale,model,layers)
  sevnames <- paste("SEV",seq(nsevcat-1),sep="")
  names(startcoefs$coef)[seq(length(sevnames))] <- sevnames


  ret <- c(licreg(ylo, yhi, nsevcat, layers, X, Xnames, total_entry, detailinfo, order_con,constraint, 
           startcoefs$coef, weight, link, model, gpscale, getvar), list(nsevcat=nsevcat, ref.cell=ref.cell))
  ret
}

######################################################################
# "lpseudo":                                                         #
#   modified form "pseudo"                                           #
#   take off all user options while calculating deviance iteratively #
######################################################################

lpseudo<- function(ylo,yhi,nsevcat,X,link = "logit", weight=rep(1,length(ylo)),
                   linkscale=NULL, model="cumulative odds model", cluster)
{
  if (!is.null(X)) {
    X <- as.matrix(X)
    dimX2 <- dim(X)[2]
    dimXX2 <- dimX2*(nsevcat-1)
  }
  ypseudo <- NULL
  xpseudo <- NULL
  wpseudo <- NULL
  cpseudo <- NULL
  scalink <- NULL

  for (ss in 1:(nsevcat-1)) {

    yindx1 <- ifelse(yhi  < ss, 1,0) + ifelse(ylo >= ss, 1,0)
    sumyindx1 <- sum(yindx1)
    yindx2 <- ifelse(ylo >= ss, 1,0)

    if(sumyindx1 > 0) {
      ypseudo <- c(ypseudo,yindx2[yindx1 == 1])	
      cpseudo <- c(cpseudo, cluster[yindx1 == 1])
      xtmp <- matrix(0, nrow = sumyindx1, ncol = nsevcat - 1)
      xtmp[,ss] <- 1
      if (!is.null(X)) {
        if (model=="cumulative odds model" || model=="conditional odds model"){
          XX <- X[yindx1==1,]
        } else {
          XX <- matrix(0,sumyindx1,dimXX2)
          XX[,(1:dimX2)+(ss-1)*dimX2] <- X[yindx1==1,]
        }
  	xtmp <- cbind(xtmp,XX)
      }
      xpseudo <- rbind(xpseudo, xtmp)
      wpseudo <- c(wpseudo, weight[yindx1 == 1])
      if (!is.null(linkscale)){
        scalink <- c(scalink, linkscale[yindx1 == 1])
      }
    }
  }

  if(is.null(linkscale))
    scalink <- rep(1, length(ypseudo))

  x <- xpseudo*scalink
  y <- ypseudo

  ret <- glm(y ~ x-1, family = binomial(link=link), weights = wpseudo, x = FALSE)

  var <- vcov(ret)

  c(ret, list(var=var, ypseudo = ypseudo, xpseudo = xpseudo * scalink, wpseudo =
              wpseudo, cpseudo = cpseudo))
}

######################################################################
# "licreg":                                                          #
#   modified form "icreg"                                            #
#   take off all user options while calculating deviance iteratively #
######################################################################

licreg <- function(ylo,yhi,nsevcat,layers,X,Xnames, total_entry,  detailinfo, order_con, constraint,
                coefs, weight=rep(1,length(ylo)),link="logit",
                model = "cumulative odds model", linkscale=NULL, getvar=TRUE,info)
{
  Smax <- nsevcat - 1

  df.residual <- Smax*total_entry - length(coefs); 

  A <- A.inv <-  diag(Smax)
  A[col(A) == row(A) + 1] <- - 1
  A.inv[row(A) < col(A)] <- 1
  transcoefs <- c(A %*% coefs[1:Smax], coefs[-c(1:Smax)])

  intnms <- paste("SEV",seq(Smax), sep ="")
  if (model=="unrestricted cumulative model") {
    Xnames_temp <- paste(rep(Xnames,Smax),
    rep(intnms,rep(length(Xnames),Smax)),sep=":")
  } else { 
    Xnames_temp <- Xnames
  }

  names <- c(intnms,Xnames_temp)


  fit <- optim(transcoefs, fn = hfdeviance, gr = hfdeviance.grad, method = "BFGS", 
               hessian = FALSE, lower = -Inf, upper = Inf, ylo = ylo, yhi = yhi, 
               nsevcat = nsevcat, X = X, weight = weight, link = link, model=model, 
               linkscale=linkscale, control=list(reltol=max(1.5e-8, sqrt(.Machine$double.eps))))

  gradient <- NULL
  hessian <- NULL
  variance <- NULL

  transcoefs <- fit$par
  coefs <- c(A.inv %*% transcoefs[1:Smax], transcoefs[ - c(1:Smax)])

  intnms <- paste("SEV",seq(Smax), sep ="")
  Xnames.gp <- NULL

  if (model=="unrestricted cumulative model") {
    Xnames.gp <- rep(1:length(Xnames),Smax)
    Xnames <- paste(rep(Xnames,Smax),
    rep(intnms,rep(length(Xnames),Smax)),sep=":")
  }
  nms <- c(intnms,Xnames)
  names(coefs) <- nms

  if (getvar) {
    sand <- multiscore(coefs,ylo, yhi, nsevcat, layers, X, weight, link, model, linkscale)
    gradient <- sand$gradient
    hessian <- sand$hessian
    covar <- sand$covar
    hessian.inv <- solve(hessian)
    variance <- hessian.inv %*% covar %*% hessian.inv
    names(gradient) <- nms
    dimnames(hessian) <- dimnames(variance) <- list(nms,nms)
  }

  return(list(deviance = 2 * fit$value, df.residual = df.residual,
              message = fit$message, coefficients = coefs, gradient = gradient,
              hessian = hessian, sand.var = variance, itr.num = fit$counts, 
              f.evals = fit$f.evals, g.evals = fit$g.evals, scale = fit$scale,
              link=link, model=model, xnames=Xnames,xnames.gp=Xnames.gp))

}


###########################################################

# Final Fitting

###########################################################
# "go":                                                   #
#    The driver to get the intial value and               #
#       to run the icreg program.                         #
#
###########################################################

fgo <- function(data, model="cumulative odds model", total_entry=total_entry, getvar=TRUE, verbose=TRUE) {

  ylo <- data$y$ylo
  yhi <- data$y$yhi
  nsevcat <- max(c(data$y$yhi, data$y$ylo)) + 1
  X <- as.matrix(data$x)
  if (ncol(X) > 1) {
    Xnames <- dimnames(data$x)[[2]][-1]
    X <- as.matrix(X[,-1])
  }  else {
    X <- Xnames <- NULL
  }
  gpscale <- data$gpscale
  if (length(gpscale)==0) {
    cat("\n\nGroup scale missing - assuming 1...\n")
    gpscale <- rep(1, length(ylo))
  }
  layer.names <- data$study.label
  weight <- data$weight

  link <- data$link

  if(nsevcat != length(unique(c(yhi, ylo)))){
     miss <- seq(nsevcat) - 1
     miss <- miss[is.na(match(miss, unique(c(yhi, ylo))))]
     warning(paste("Severity category '",miss, "' is completely missing!"
         , sep = ""))
  }

  if(is.null(data$strata.factors))
    ref.cell <- NULL
  else
    ref.cell <- data$strata.coefnames[1,"INTERCEPT"]

  layers <- charmatch(layer.names, unique(layer.names))

  startcoefs <- pseudo(ylo,yhi,nsevcat,X,link,weight,gpscale,model,layers)
  sevnames <- paste("SEV",seq(nsevcat-1),sep="")
  names(startcoefs$coef)[seq(length(sevnames))] <- sevnames

  cat("\nOptimizing...\n")

  ret <- c(ficreg(ylo, yhi, nsevcat, layers, X, Xnames, total_entry,
           startcoefs$coef, weight, link, model, gpscale, getvar),
           list(nsevcat=nsevcat, ref.cell=ref.cell))
  ret
}


################################################
# "ficreg":                                    #
#     modified from "icreg"                    #
#       for fitting with gamma                 #
################################################

ficreg <- function(ylo,yhi,nsevcat,layers,X,Xnames, total_entry=total_entry, coefs,
                weight=rep(1,length(ylo)),link="logit",
                model = "cumulative odds model", linkscale=NULL, getvar=TRUE,info) {

  Smax <- nsevcat - 1

  df.residual <- Smax*total_entry - length(coefs)-1; # Added by C. Ahn on 03/30/05

  A <- A.inv <-  diag(Smax)
  A[col(A) == row(A) + 1] <- - 1
  A.inv[row(A) < col(A)] <- 1
  transcoefs <- c(A %*% coefs[1:Smax], coefs[-c(1:Smax)])

  intnms <- paste("SEV",seq(Smax), sep ="")
  if(model=="unrestricted cumulative model") {
    Xnames_temp <- paste(rep(Xnames,Smax), rep(intnms,rep(length(Xnames),Smax)),sep=":")
  } else { 
    Xnames_temp <- Xnames
  }

  names <- c(intnms,Xnames_temp)

  fit <- optim(transcoefs, fn = hfdeviance, gr = hfdeviance.grad, method = "BFGS", 
               hessian = FALSE, lower = -Inf, upper = Inf, ylo = ylo, yhi = yhi, 
               nsevcat = nsevcat, X = X, weight = weight, link = link, model=model, 
               linkscale=linkscale, control=list(reltol=max(1.5e-8, sqrt(.Machine$double.eps))))

  cat("\n", fit$message, "\n")
  gradient <- NULL
  hessian <- NULL
  variance <- NULL

  transcoefs <- fit$par
  coefs <- c(A.inv %*% transcoefs[1:Smax], transcoefs[ - c(1:Smax)])
  coefs <- c(coefs, gammam);

  intnms <- paste("SEV",seq(Smax), sep ="")
  Xnames.gp <- NULL
  if(model=="unrestricted cumulative model") {
    Xnames.gp <- rep(1:length(Xnames),Smax)
    Xnames <- paste(rep(Xnames,Smax), rep(intnms,rep(length(Xnames),Smax)),sep=":")
  }
  nms <- c(intnms,Xnames, "Gamma")
  names(coefs) <- nms

  if (getvar) {
    cat("\nComputing estimated covariance matrix...\n")
    coefsg <- coefs[-length(coefs)];
    sand <- fmultiscore(coefsg,ylo, yhi, nsevcat, layers, X, weight, link, model, linkscale)
    gradient <- sand$gradient
    hessian <- sand$hessian
    covar <- sand$covar
    hessian.inv <- solve(hessian)
    variance <- hessian.inv %*% covar %*% hessian.inv
    names(gradient) <- nms
    dimnames(hessian) <- dimnames(variance) <- list(nms,nms)
  }

  return(list(deviance = 2 * fit$value, df.residual = df.residual,
              message = fit$message, coefficients = coefs, gradient = gradient,
              hessian = hessian, sand.var = variance,
              itr.num = fit$counts, f.evals = fit$f.evals,
              g.evals = fit$g.evals, scale = fit$scale,
              link=link, model=model, xnames=Xnames,xnames.gp=Xnames.gp))

}


############################################################
#"fmultiscore":                                            #
#   modified from "multiscore"                             #
#     for fitting model with gamma                         #
############################################################
fmultiscore<-function(coefs, ylo, yhi, nsevcat, layers, X, weight, link, model, linkscale=NULL)
{

  hi <- fpr.exceed(yhi + 1, nsevcat, X, coefs, link, model, derivs =2, linkscale)
  lo <- fpr.exceed(ylo, nsevcat, X, coefs, link, model, derivs =2, linkscale)
  likes  <-  lo$pr-hi$pr
  grads <- lo$dpr - hi$dpr
  hesses <- lo$dpr2 - hi$dpr2
  likes_tmp<-likes
  likes[likes==0] <- exp(-80)       

  Gradnt <- sweep(grads, 2, (weight/likes), "*")
  Gradient <- apply(Gradnt,1,sum)
  Hessian <- sweep(grads, 2, ((weight^.5)/likes), "*")
  Hessian <- apply(sweep(hesses,3,(weight/likes),"*"),c(1,2),sum) - Hessian %*% t(Hessian)

  scores <- NULL
  mxlay <- max(layers)

  if( (!is.na(mxlay))  & (mxlay > 1) ) {
    for (lay in 1:mxlay) {
      scores <- cbind(scores,apply(as.matrix(Gradnt[,layers == lay]),1,sum))
    }
    covar <- var(t(scores))*(mxlay-1)
  } else {
    cat("\n\n Note: Single study analysis. \n\n")
    covar <-  - Hessian
  }

  list(gradient =  Gradient, hessian = Hessian, covar = covar)

}

########################################
# "fpr.exceed":                        #
#    modified from "pr.exceed"         #
#      for fitting a model with gamma  #
########################################

fpr.exceed <- function(sev, nsevcat, X, coefs, link, model, derivs=0, linkscale=NULL)
{

  xx <- matrix(0, nrow = length(sev), ncol = nsevcat - 1);
  xx[sweep(col(xx),1, sev, ">=") & sweep(col(xx),1,sev, "<=")] <- 1;

  if (!is.null(X))
    if (model=="cumulative odds model"){
      xx <- cbind(xx,X)
    } else if (model=="unrestricted cumulative model") {
      X <- as.matrix(X);
      Xm.indx <- matrix(rep(1:(nsevcat-1),rep(length(X),nsevcat-1)), nrow=length(sev));
      Xm <- matrix(rep(X,nsevcat-1), nrow=length(sev));
      Xm.indx <- sweep(Xm.indx,1,sev,"==");
      Xm[!Xm.indx] <- 0;
      xx <- cbind(xx,Xm);
    }

  if (!is.null(linkscale)) xx <- xx*linkscale

  xlin <- xx %*% coefs

  pr <- ifelse(sev == nsevcat, 0, ifelse(sev == 0, 1, fs(xlin, link, deriv = 0)));

  if ( derivs == 0 ) {
    return(pr) 
  } else if ( derivs == 1) {
    dpr <- c(ifelse(sev == nsevcat | sev == 0, 0, fs(xlin,link, deriv = 1)));
    dpr <- t(sweep(xx,1,dpr,"*"));
    return(list(pr, dpr)) 
  } else if ( derivs == 2) {
    dpr <- ifelse(sev == nsevcat | sev == 0, 0, fs(xlin,link,deriv = 1));
    dpr2 <- ifelse(sev == nsevcat | sev == 0, 0, fs(xlin,link, deriv = 2));

    crossprod <- function(z){
          z <- as.matrix(z)
          z %*% t(z)
    }

    if((data.char$xlog=="C" || data.char$xlog=="CT")){
      slope <- coefs[grep("LG10CONC",names(coefs))]

      if(pmodel=="cumulative odds model") slope <- rep(slope,max(nsevcat)-1);

      slope <- c(0,slope);

      y <- rep(0, nrow(data$x));

      for(i in 1:(max(nsevcat)-1)) y <- y+i*xx[,i];

      dslope <- slope[y+1];

      ### Consider gamma when calculating gradient and covariance matrix
      Gamma <- dslope/(log(10)*10^xx[,max(nsevcat)])
      xxg <- cbind(xx,Gamma)

      dpr <- sweep(t(xxg), 2, dpr, "*")
      dpr2 <- sweep(array(apply(xxg, 1, crossprod), c(ncol(xxg), ncol(xxg), nrow(xxg))), 3,dpr2,"*");

    } else {
      dpr <- sweep(t(xx), 2, dpr, "*")
      dpr2 <- sweep(array(apply(xx, 1, crossprod), c(ncol(xx), ncol(xx), nrow(xx))), 3,dpr2,"*");
    }
    return(list(pr = pr, dpr = dpr, dpr2 = dpr2)) 
  }

}

