###################################################
# This file is sourced by the "ecdata()" command. #
# Optionally called from catreg.R                 #
# 7/08/99            				  #
# R version of the file                           #
# Chunhua Liu. 02/19/02				  #
# Last Updated by C. Ahn: 01/20/06                #
######################################################
# Modified by GyoungJin Ahn : 02/08/2006             #
# Only when the number of severity level >2,         #
# user can choose the severity level for caculating  #
# the EC values                                      #
#######################################################
# Modified by GyoungJin Ahn : 02/10/2006              #
# Only when time variable's name is "Hours" or "Hour",#
# print out the ec table at 1, 4, 8, 24               #
#######################################################


##############################
# Check for model-fit object #
##############################
#ecdataRun <- function() {
if (is.na(charmatch("fits", objects(pattern="fits")))) {
  cat("\n>>>> No fitted model - run catreg() first.\n\n")
  stop()
}

#######################################################
# Query user for q value and desired ERC100q line and #
# calculate the ERC100q lines.                        #
#######################################################

######################################################
# Query user for ERC calculation                     #
# calculate the ERC                                  #
######################################################

### User option for risks
### Use total risk when log concentration and gamma are not used
if(data.char$xlog=="C" || data.char$xlog=="CT"){
  if (zero_bg=="n"|| zero_bg=="N"){ 
    prisks="extra risk";
  } else {
    prisks="total risk";
  }
} else {
  prisks="extra risk";
}


### User option for q
if(exists("xVars")) {
	pct = xVars$info$Bmr * 100
} else {
cat("\n\nPercentile for ERC? (default=10): ")
pct <- readline()
pct <- ifelse(pct=="", 10, as.numeric(pct))
}

### Which severity level?
if(exists("xVars")) {
  sevs <- xVars$info$sevs;
  # Deal with possibility that the highest severity level was filtered out.
  iMaxSev <- max(data$y$ylo) # Highest sev after filtering
  if (sevs > iMaxSev) {
    cat(strwrap(paste0("Not calculating ERC for " ,paste0("SEV", sevs)
              ," because those input rows were filtered out.\n")
              ,width=72, initial=" * " ,prefix="   "), sep="\n")
    return(invisible())
  } # if (sevs > iMaxSev)
} else {
	if(fits$nsevcat==2) { 
	  sevs <- 1; 
	} else {
	  cat("\nSeverity level for ERC? (default = 1): ")
	  sevs <- readline()
	  sevs <- ifelse(sevs=="", 1, sevs)
	}

	if(fits$nsevcat>2) { 
	  while(sevs > max(data$y$ylo) || sevs <= min(data$y$ylo)){
		cat("\nError: The severity level is not within limits\n");
		cat("\nEnter the new severity level in (",min(data$y$ylo)+1,",",max(data$y$ylo),"): ");
		sevs <- readline();
	  }
	}
}

### User option for the level of 1-sided CI
if(exists("xVars")) {
	pct_ci = xVars$info$ConfidenceLevel
} else {
cat("\nPercentile for 1-sided upper and lower confidence intervals? (default=95): ")
pct_ci <- readline()
pct_ci <- ifelse(pct_ci=="", 95, as.numeric(pct_ci))
}

### Return ECline for further caculation
if(fits$model=="unrestricted conditional model" ||
  fits$model=="conditional odds model") {
  ECline <- ectable.cp(severities=sevs,
                  times=seq(data$time.range[1],data$time.range[2], length=20),
                  data$strata.coefnames,
                  data$strata.factors,
                  data$strata.categories,
                  coefs0=fits$coefficients,
                  sand.var0=fits$sand.var,
                  link=fits$link,
                  model=fits$model,
                  nsevcat=fits$nsevcat,
                  q=pct/100,
                  xlog=data.char$xlog)
} else
ECline <- ectable(severities=sevs,
                  times=seq(data$time.range[1],data$time.range[2], length=20),
                  data$strata.coefnames,
                  data$strata.factors,
                  data$strata.categories,
                  coefs0=fits$coefficients,
                  sand.var0=fits$sand.var,
                  link=fits$link,
                  model=fits$model,
                  q=pct/100,
		  risks=prisks,
                  q_ci=pct_ci/100,
                  xlog=data.char$xlog)
rm(pct,sevs,prisks)


#################################################
# output ERC10 or EC10 data to ascii files 	#
#################################################

cat("\nERC data computed...\n")

if(exists("xVars")){
	ec3table(duration=xVars$info$duration, output=TRUE, printout=TRUE)
} else {
if(length(grep("hour", required['time'], ignore.case=TRUE))>0) {
  ec3table(duration=c(1,4,8,24), output=TRUE, printout=TRUE )
}
}

### user option about whether to write text files for ERC or not
if(exists("xVars")) {
	ans <- "n"
} else {
	cat("\nWrite ERC data to text files? (n):")
	ans <- readline()
}
if(ans=="y" || ans=="Y") {
   cat("\n\nWriting ERC data to ascii files...\n")
   output(ECline$tables, logfile=outfile, xlog=ECline$xlog)
   cat("\n\nDone - see", outfile, "for file names.\n\n")
}

##########################################
# Plotting instructions 		 #
# modified to handle both ERC and ERD-T #
##########################################

cat("\n>>>> Type `prplot(time=x)' or `prplot(conc=x)' to\n")
cat(">>>> graph probability with time or conc fixed at x.\n")
cat(">>>>\n")
cat(">>>> Type `catplot()' to display the ERC line and\n")
cat(">>>> the confidence lines for a particular stratum.\n")
cat(">>>>\n")
cat(">>>> Type `stratplot()' to display the ERC lines\n")
cat(">>>> for all strata.\n")
cat(">>>>\n")
cat(">>>> Type `confplot(10)' to display ERC confidence\n")
cat(">>>> intervals for duration=10.\n")
cat(">>>>\n")
cat(">>>> Type 'dataplot()' to plot points on duration-versus-concentration\n")
cat(">>>> axes without a response probability curve.\n")
cat(">>>>\n")
cat(">>>> Type 'devplot()' to provide a diagnostic plot of generalized deviance\n")
cat(">>>> residuals versus observation number, concentration, or duration.\n")
cat(">>>>\n")
cat(">>>> To remove previous plots type `rmplots()'.\n")
cat(">>>>\n")
cat(">>>> To change percentile or severity type `ecdata()'.\n\n")

#}