##########################################
# This file is sourced by the "catreg()" #
# command  7/08/99                       #
##########################################

################
# get filename #
################

options(show.error.messages=TRUE)

source("winstall.R")

# Instal all necessary functions for Log concentration
source("Logconc.R")

# flag for writing output to csv
outputCSV = TRUE

# Output the current version of CatReg on the Screen by C. Ahn on 06/13/2005
version(); 

################################################
# get information about the data and the model #
################################################
if(exists("xVars")) { 
	data.char <- getData() 
} else {
	cat("\nName of data file? ")
	infile <- readline()
	data.char <- askfordata(infile)
} 


#####################################################
# query for type of model - default is proportional #
# odds model.                                       #
#####################################################

nsev <- length(unique(data.char$data[,colnames(data.char$data)=='SevLo']))

### For the binary model (nsev==2), model option is not necessary

if(nsev==2){ 
  pmodel <- "cumulative odds model" 
} else {
  if(exists("xVars")) { pmodel = xVars$info$ModelForm 
  } else {
  models <- c("cumulative odds model", "unrestricted cumulative model") 
  cat("\n")
  pmodel <- models[menu(models, graphics = FALSE, title = "Model?")]
  pmodel <- ifelse(length(pmodel) > 0 , pmodel, "cumulative odds model")
  }
}

### Begin Modified by Ken Brown 6/9/06 to drop the requirement of no stratification to use gamma can be used with log(concentration) 
### User option for background will appear, only if log(concentration) is used
##### Modified by GyoungJin Ahn : 04/14/2006
#### So, only for the model with log scale, ask zero background or not.

if(data.char$xlog=="C" || data.char$xlog=="CT"){
  if(exists("xVars")) {zero_bg = xVars$info$ZeroBackResponse 
  } else {
     cat("\n\nAssume zero background risk \n(i.e. response cannot occur at zero concentration)? (y): ");
     zero_bg <- readline();

     if(zero_bg == "") zero_bg <- "Y"
     }
}
### End Modified by Ken Brown 6/9/06

### When Log concentration is used without gamma, observations with conc=0 will be deleted.
if((data.char$xlog=="C" || data.char$xlog=="CT") && !(zero_bg =="N" || zero_bg =="n")){ 
  data.char$data <- data.char$data[data.char$data[,colnames(data.char$data)==required['conc']]!="1e-10",];
}

##################
# start log file #
##################

# default filename is the name of the data with extension 'out'.
if(exists("xVars")) {outfile_default <- outname(data.char$infile)
} else { outfile_default <- outname(infile) }

if(exists("xVars")) {outfile = xVars$info$outputFile
} else {
# user can input the output filename.
cat("\n\nName of the output? (", outfile_default,"): ");
outfile_temp <- readline();

# if user does not input output filename, default name will be used.

if(outfile_temp==""){
  outfile <- outfile_default;
} else {
  outfile <- outfile_temp;
}

# put extension 'out'
outfile <- paste(c(outfile,"out"), collapse=".")

# get rid of temporary variables
rm(outfile_default, outfile_temp)
}

cat("\nOutput file is", outfile)

### Add the version information, name of the input file
### and Cluster information to the Summary Output

sink(file=outfile, append=FALSE)
version() # Output the current version of CatReg by C. Ahn on 06/13/2005
if(exists("xVars")) {
	cat("\nRun Date: ", xVars$info$Date, "\n")
	cat("\nSource data file:", data.char$infile, "\n")
} else {cat("\nSource data file:", infile, "\n")}


#if(is.null(data.char$cluster.var)) {
#   cat("\n>>>> Single Study Analysis\n")
#} else {
#   cat("\n>>>> Multi-study Analysis:")
#   cat("\n>>>>   Clusters defined by ",
#       paste(data.char$cluster.var, collapse=" "), "\n")
#}
sink()

#########################################################
# Construct design matrix, low and high response scores #
# and study labels                                      #
#########################################################
data <- makestrata(data.char)

#if (length(data$strata.counts)>1) {
#  Stratum <- data$strata.categories
#  counts <- data.frame(Stratum, Count=as.factor(data$strata.counts),
#                       row.names=seq(length(data$strata.counts)))
#} else {
#      counts <- data$strata.counts
#      dimnames(counts) <- NULL
#      names(counts) <- "Unstratified: Count="
#}

############################################
# Report number of records in each stratum #
############################################

#cat("\nStrata and frequencies:\n")

if(!exists("xVars")) {
cat("\nContinue? (y): ")
ans <- readline()
if(ans == "n") stop()
}

#####################################################
# query for type of analysis - default is to censor #
# uncertain responses                               #
#####################################################

### yhi should be higher than ylo
if (min(data$y$yhi - data$y$ylo) < 0)
   stop(message=paste("Value for ", required["loscore"],
    " cannot be larger than for ", required["hiscore"], ".", sep=""))

### if yhi is different from ylo, user option for worst-case analysis
if (max(data$y$yhi - data$y$ylo)) {
  if(pmodel=="unrestricted conditional model"
  || pmodel=="conditional odds model") {
    cat("\n\nWorst-case analysis of interval-censored responses.\n")
    worstcase <- TRUE
  }
  else {
	if(exists("xVars")) { worstcase <- (xVars$info$WorstCaseAnalysis == "y" )
	} else {
    cat("\n\nWorst-case analysis of interval-censored responses? (n): ")
    ans <- readline()
    worstcase <- (ans == "y")
    }
  }
} else worstcase <- FALSE

### Worst-case analysis
data <- c(data, worstcase=worstcase)
if (worstcase) data$y$ylo <- data$y$yhi

####################################
# send information to the log file #
####################################

sink(file=outfile, append=TRUE)

#cat("\nStrata and frequencies:\n\n")
#print(counts)

cat("\nType of analysis:", ifelse(worstcase, "Worst-case", "Censored"), "\n")
sink()


#########################################################
# Degrees of Freedom
# Change the format of data to calculate df
# call count_entry funtion in Winstall
#########################################################
if(exists("xVars") && !is.null(xVars[["info"]][["ngroups"]])){
  total_entry <- xVars$info$ngroups
} else {
  total_entry <- count_entry(data.char$data)
}

###############################
# Model Fitting               #
###############################


### suppressing warnings from optim()
options(warn=-1)

if((data.char$xlog=="C" || data.char$xlog=="CT") && (zero_bg=="n" || zero_bg=="N")){

### Model with the background parameter gamma

  ### Estimating gamma
  gammam <- est_gamma(data);

  ### Add gamma to the design matrix
  ldose  <- data$x[,colnames(data$x)=="LG10CONC"];
  ldoseg <- log10(exp(ldose*log(10))+gammam-10^(-10)*(ldose==-10));
  data$x[,2] <- ldoseg; 

  ### With the added gamma, fit a final model
  fits <- fgo(data, pmodel, total_entry);

} else {
  ### Model without the background parameter gamma
  #trace("go", quote(browser()), exit = quote(browser()))
  fits <- go(data, pmodel, total_entry)
  #untrace("go")
}

### make warning options back
options(warn=0)

###########################################
# Display Covariance matrix               #
###########################################
if(exists("xVars")) { short <- FALSE
} else {
	cat("\nPrintout covariance matrix? (n): ")
	covans <- readline()
	if (covans=="y"||covans=="Y"){
	  cat("\n...The covariance matrix printed out in output file\n")
	  short <- FALSE
	} else {
	  short <- TRUE
	}
}
	
###########################################
# Display summary statistics and log them #
###########################################

catreg.summary(datax = data.char)
catreg.summary(file=outfile,short=short,append=TRUE,datax = data.char)
if (outputCSV) {
  catreg.csv(file=outfile,short=short,append=TRUE,datax = data.char)
}

#############################################
### Deviance Table
### user option for 'lof(lack of fit)'
################################################

#cat("\n\nInclude a term for lack of model fit in the analysis of deviance table?  (n) : \n")
#cat("(Usually better to use the default unless there is substantial replication\n in the data)");
#lof <- readline();

#### Modified by GyoungJin Ahn : 3/10/2006
# Original : log <- "n";
lof <-"n";

### suppressing warnings from optim()
#options(warn=-1)

if(lof=="Y" || lof=="y"){
  deviance.fit(lof=TRUE, sink.value=TRUE);
} else {
  deviance.fit(sink.value=TRUE)
}

### make warning option back
#options(warn=0)

#########################################################
# effective concentration estimates and standard errors #
# - optional - type "ecdata()" to do this manually      #
#########################################################

cat("\nCalculating extra risk concentrations... ")

### Call a function to calculate EC values

#ecdata()

# run ec data for all severity levels
startSev = xVars$info$sevs

iMaxSev <- max(data$y$ylo) # Highest sev after filtering
while(xVars$info$sevs > 0){
  # Deal with possibility that the highest severity level was filtered out.
  if (xVars$info$sevs <= iMaxSev) ecdata()
  xVars$info$sevs = xVars$info$sevs - 1
}

xVars$info$sevs = startSev
