#############################################################
# winstall.R  
# Source Codes Converted from S Plus to R by Chunhua Liu 
# Last Updated: 12/07/2005 by C. Ahn
# 
# Install the graphics functions
#############################################################
#############################################################
# Modification description
# Changed logical "T" and "F" to TRUE and FALSE respectively
# Changed illegal character "\?" to "\n"
# Fix $ operator is invalid for atomic vectors in strata
# Last Updated: 01/06/2011 by G. Nonato
# 
#############################################################
#############################################################
# Modification description
# 
# Fix the logic in deviance.fit which cause the lose of strata data
# Fix count_entry function to eliminate the warning "NAs introduced by coercion"
# Last Updated: 01/21/2011 by G. Nonato
# 
#############################################################

### Indicate the current version of CatReg


# Version modified : 04/13/2006 : 1.1 -> 2.0
version <- function() {
  #cat("\nCatReg - R Beta Version 2.0 (06/22/06)\n")
  #cat("\nCatReg - R Beta Version 2.1 (01/06/11)\n")
  #cat("\nCatReg - R Beta Version 2.2 (01/21/11)\n")
  cat("\nCatReg Version 3.1\n")
}

### Indicate the current working directory
 currentdir <- getwd()

### Check whether "CatReg" is in the current working directory 
### If there is not, set its subdirectory "CatReg" as the "newdir"

newdir <- currentdir

setwd(newdir)

sourcefile <- paste (newdir, "\\Winstall.R",sep="")

loadfile <- paste (newdir, "\\.RData", "\n", sep="")

### Install the plot utilities
source("winplot.R")

### Install the data conversion utilities
source("util.R")

### load the "MASS" package for the R-version.
library(MASS)

### If a user types run() by mistake, make him/her type 'catreg()' instead.
run <- function() cat("\n>>>> Type 'catreg()' instead.\n\n")

### run catreg
#catreg <- function(file = "catreg.R", local = FALSE) source(file, local=local)

### function to compute effective concentration estimates and
### standard errors

ecdata <- function(file = "ecdata.R", local = FALSE) source(file, local=local)

### Default names for required variables:

if(exists("xVars")) { 
	required = xVars$required 
} else {
# Begin. Ken Brown. 6/19/06 "Weight" removed as a required variable
	 if(!exists("required")) {
	 #  required <- c("mg/m3", "Hours", "SevLo", "SevHi", "Incid", "Weight", "Nsub")
	 required <- c("mg/m3", "Hours", "SevLo", "SevHi", "Incid","Nsub")

	 #  names(required) <- c("conc", "time", "loscore", "hiscore", "incid","weight", "nsub")
	 names(required) <- c("conc", "time", "loscore", "hiscore", "incid","nsub")
	 #} else if(length(required)==7 && 
	 #          sum(names(required)==c("conc","time","loscore","hiscore","incid", "weight", "nsub"))==7) {
	 } else if(length(required)==6 && 
			  sum(names(required)==c("conc","time","loscore","hiscore","incid", "nsub"))==6) {
	 
	 required <- required
	 } else {
	 #  required <- c("mg/m3", "Hours", "SevLo", "SevHi", "Incid", "Weight", "Nsub")
	 #  names(required) <- c("conc", "time", "loscore", "hiscore", "incid", "weight", "nsub")
	  required <- c("mg/m3", "Hours", "SevLo", "SevHi", "Incid", "Nsub")
	  names(required) <- c("conc", "time", "loscore", "hiscore", "incid", "nsub")
	 }
# End. Ken Brown. 6/19/06
}

#########################################################################
#########################################################################
##  PART I: Read data from ascii file, one record per row,             ##
##  comma separated                                                    ##
##  The first line should contain the variable names (comma separated) ##
#########################################################################
#########################################################################

### Obtain the output filename from the input file name
#outname <- function(filename) {

#  nc <- nchar(filename)
#  dot <- charmatch(".", substring(filename, 1:nc, 1:nc))
#  if (is.na(dot)) {
#     outname <- paste(c(filename, "out"), collapse=".")
#  } else {
#     ndots <- length(dot)
#     outname <- substring(filename, 1, dot[ndots]-1)
#  }
#  outname
#}
################################################################
# Modified by GyoungJin Ahn : 03/02/2006              
# when all path is used for input data file, it can be handled.
################################################################
outname <- function(filename) {
	fname <- unlist(strsplit(filename, "\\."))
	lf <- length(fname)
	if(length(grep("csv",fname[lf], ignore =TRUE) ) >0) {
		if(lf==2) {
			outname <- fname[1]
		} else {
			tmp <- NULL
			outname <- fname[1]
			for( i in 2:(lf-1)) {
				outname <- paste(outname,fname[i],sep=".")
			}
		}
	} else if(length(grep("out",fname[lf], ignore =TRUE) ) >0) {
		if(lf==2) {
			outname <- fname[1]
		} else {
			tmp <- NULL
			outname <- fname[1]
			for( i in 2:(lf-1)) {
				outname <- paste(outname,fname[i],sep=".")
			}
		}
	} else {
		outname <- paste(c(filename, "out"), collapse=".")
	}
	outname
}
#***********************************************************************




### Generate "data.char"

askfordata <- function(infile, var.name=required){

  ### check record lengths

  reclength <- count.fields(infile, sep = ",")
  fncol <- reclength[1]
  recflag <- seq(length(reclength))[reclength != fncol]
  if(length(recflag) > 0) stop(message = paste(c(
    ">>>> Records", paste(as.character(recflag), collapse = ", "),
    "have missing or extra fields!"), collapse = " "))

  ### read the header and data into a character matrix

  a <- matrix(scan(infile, what = character(), sep = ","), ncol = fncol, byrow = TRUE)
  ## remove empty rows
  a[!apply(a == "", 1, all),]

  ### extract column names from first row

  dimnames(a) <- list(NULL, c(a[1,  ]))
  a <- a[-1,]


  ### check for required fields

  implied <- var.name[is.na(match(var.name, dimnames(a)[[2]]))]
  missreq <- match(var.name["loscore"],implied)

  is.conc <- is.na(match(var.name["conc"], implied))
  is.time <- is.na(match(var.name["time"], implied))
  is.loscore <- is.na(match(var.name["loscore"], implied))
  is.hiscore <- is.na(match(var.name["hiscore"], implied))
# Begin. Ken Brown. 6/19/06 Remove "weight" from being set by input variable "Weight".
#  is.weight <- is.na(match(var.name["weight"], implied))
# End. Ken Brown. 6/19/06
  is.nsub <- is.na(match(var.name["nsub"], implied))

  is.incid <- is.na(match(var.name["incid"], implied))
  is.gpsize <- is.na(match("GpSize", colnames(a)))


  ### stop if no severity score
  if (!is.loscore)
    stop(message=paste(c("Required input variable SevLo was not found.",
         implied[missreq[!is.na(missreq)]]), collapse=" "))
  if (!is.conc)
    stop(message=paste(c("Required input variable mg/m3 was not found.",
         implied[missreq[!is.na(missreq)]]), collapse=" "))
  if (!is.incid)
    stop(message=paste(c("Required input variable Incid was not found.",
         implied[missreq[!is.na(missreq)]]), collapse=" "))
  if (!is.nsub)
    stop(message=paste(c("Required input variable Nsub was not found.",
         implied[missreq[!is.na(missreq)]]), collapse=" "))

  ### generate a matrix "a" which will be "char.data" later
  if (!is.time) {
    a <- cbind(a, "1")
    dimnames(a)[[2]][dim(a)[2]] <- var.name["time"]
  }
  if (!is.hiscore) {
    a <- cbind(a, a[ ,var.name["loscore"]])
    dimnames(a)[[2]][dim(a)[2]] <- var.name["hiscore"]
  }
# Begin. Ken Brown. 6/19/06. Remove "weight" from being set by input variable "Weight".
#  if (!is.weight) {
    ww <- a[,dimnames(a)[[2]]=='Incid'];
    a <- cbind(a, ww)

  # Begin Ken Brown. 6/20/06
  #    dimnames(a)[[2]][dim(a)[2]] <- var.name["weight"]
       dimnames(a)[[2]][dim(a)[2]] <- var.name["incid"]
  # End Ken Brown. 6/20/06

#  }
# End. Ken Brown. 6/19/06
  if (is.gpsize) {
    a <- cbind(a, "1")
    dimnames(a)[[2]][dim(a)[2]] <- "GpSize"
  }

  ### remove implied variables from selection list

  namelist <- dimnames(a)[[2]]
  for (val in implied) namelist <- namelist[namelist != val]

  ### data filtering option

  select.on <- query(wordlist=namelist, desc="Variables:",
                     ask="Filter the data on which variables? (none):")
  exclude <- NULL

  if(length(select.on) > 0) {
    exclude <- list(names = select.on)
    for(i in seq(length(select.on))) {
      omit <- query(wordlist=unique(a[, select.on[i]]),
                    desc=paste("Categories of ", select.on[i], ":", sep=""),
                    ask="Categories to exclude? (none):")
      if(length(omit) > 0) {
         for (val in omit){
           a <- a[a[,select.on[i]] != val,]
         }
      }
      exclude <- c(exclude, list(omit))
    }
  }

  ### Define clusters
  cluster.on <- query(wordlist=namelist, desc="Variables:", 
                      ask="Define clusters using which variable(s)? (none):")

  ### names for primary variables
  cnm <- var.name["conc"]
  tnm <- var.name["time"]

  ### select strata
  ### Stratify intercept
  istrata <- query(wordlist=namelist, desc="Variables:", 
                   ask="Stratify intercepts on which variables? (none):")

  ### Stratify concentration
  if (is.conc){
    cstrata <- query(wordlist=namelist, desc="Variables:", 
                     ask=paste("Stratify concentration (", cnm,
                     ") on which variables? (none):", sep=""), addlist="omit")
  } else cstrata <- NULL

  ### Stratify time
  if (is.time){
    tstrata <- query(wordlist=namelist, desc="Variables:", 
                     ask=paste("Stratify time (", tnm,
                     ") on which variables? (none):", sep=""), addlist="omit")
  } else tstrata <- NULL


  ### select log or linear scale for quantitative variables

  lgcnm <- paste("log10(", cnm, ")", sep="")
  lgtnm <- paste("log10(", tnm, ")", sep="")

  xclog <- xtlog <- ""

  if (is.na(match(var.name["conc"], implied)))
    xclog <- c("", "C")[menu(c(cnm, lgcnm), graphics = FALSE,
             title = paste("\nLog or linear scale for ", cnm, "? \nChoices:", sep=""))]

  if (is.na(match(var.name["time"], implied)))
    xtlog <- c("", "T")[menu(c(tnm, lgtnm), graphics = FALSE,
             title = paste("\nLog or linear scale for ", tnm, "? \nChoices:", sep=""))]

  xlog <- paste(xclog, xtlog, sep="")


  ### check for missing response and zero exposure
	data <- cleandata(a, xlog = xlog)
	
	

  ### select link

  links <- c("logit", "probit", "cloglog")
  cat("\n")
  plink <- links[menu(links, graphics = FALSE, title ="Link function?")]
  plink <- ifelse(length(plink) > 0, plink, "logit")

  ### function value

  list(data = data$data, excluded = exclude, cluster.var=cluster.on, 
       strata = list(intercept = istrata, conc=cstrata, time =tstrata), 
       xlog = xlog, link = plink, gpcorr=0, org_gpcorr=0, 
       missing.row = data$missing.row, zero.row=data$zero.row, infile = infile)
}

query <- function(wordlist, desc="Options:", ask="Enter your selection(s)", default=NULL, addlist=NULL) {
  notdone <- TRUE
  while (notdone) {
    cat("\n\n", desc, "\n", wordlist, "\n")
    cat("\n", ask, "\n")
    select <- scan(what = "")
    if (length(select) == 0) {
      notdone <- FALSE
      select <- default
    } else {
      matched <- charmatch(select, c(wordlist, addlist))
      if (any(is.na(matched))) {
        cat("\n>>>> No match for", paste(select[is.na(matched)],collapse=", "), ".\n")
      } else 
      if (any(matched ==0)) {
        cat("\n>>>>", paste(select[matched == 0],collapse=", "), "ambiguous.", "\n")
      } else {
        notdone <- FALSE
        select <- c(wordlist, addlist)[matched]
      }
    }
  } 
  select
}


### Delete zero concentration or time to avoid log(0)
### If the background parameter gamma is used, zero concentration will not be deleted

cleandata <- function(a, xlog="CT", var.name=required) {

  ### flag zero exposure if log scale
  zero.row <- rep(FALSE, dim(a)[1])
  zero.row <- (a[,var.name["conc"]]=="0") | (a[,var.name["time"]]=="0")
  zero.row.conc <- (a[,var.name["conc"]]=="0")
  zero.row.time <- (a[,var.name["time"]]=="0")
  temp <- sum(zero.row)  

  
  
  
  ### flag missing exposure or response

  missing.row <-
    (a[,var.name["conc"]]=="")      | (a[,var.name["time"]]=="") |
    (a[,var.name["hiscore"]]=="-9") | (a[,var.name["hiscore"]]=="") |
    (a[,var.name["loscore"]]=="-9") | (a[,var.name["loscore"]]=="")	  
   
  if(xlog != "" & temp != 0){

    zero.exposure <- c("Deletion", "Substitution by User Specified number")

    ### if gamma is used, log(conc==0) is not deleted
    if(xlog =="CT" || xlog =="C"){
      zero.exposure.choice <- 0  # do not deleted log(0)
    } else {
      zero.exposure.choice <- 1  # delete observations with log(0)
    }
  

    if (zero.exposure.choice == 1){    # xlog = "T" case
      # remove flagged rows
      
#### Modified by GyoungJin Ahn : 3/10/2006      
# Original :    a <- a[(!zero.row & !missing.row), ]   
## Since zero.exposure.choice ==1 case is just xlog="T", delete "zero.row.time =1" row
      a <- a[(!zero.row.time & !missing.row), ]   
      zero.row <- seq(length(zero.row))[zero.row]
    } else { 
      temp.conc <- sum(zero.row.conc) 

    
      
      ### add a small number (10e-10) for 0 concentration to avoid being deleted
      ### 10e-10 will be subtracted later
      
#### Modified by GyoungJin Ahn : 3/10/2006      
# Original :
#      if (temp.conc > 0) input.a <- 0.0000000001
#      cat("\n")
#      a[(zero.row.conc), var.name["conc"]] <- input.a

     
      if (temp.conc > 0) {
         input.a <- 0.0000000001
         cat("\n")
         a[(zero.row.conc), var.name["conc"]] <- input.a
      }


#### Modified by GyoungJin Ahn : 3/10/2006      
#  Original :     zero.row <- rep(NULL, dim(a)[1])
#####  Begin : Added by GyoungJin Ahn : 3/10/2006           
      if(xlog == "T" || xlog == "CT")    a <- a[(!zero.row.time & !missing.row), ]   # If time ==0 then just delete
      else a <- a[(!missing.row), ]
      zero.row <- seq(length(zero.row))[zero.row]
#####  End : Added by GyoungJin Ahn : 3/10/2006           


    }
  }
     
  ### report back problem rows   
  missing.row <- seq(length(missing.row))[missing.row]
  

  list(data=a, zero.row=zero.row, missing.row=missing.row)
}


###########################################
# Construct the design matrix and extract #
# pertinent information. The input is an  #
# object with components:                 #
#    $data - data stored as.character     #
#          - assumed to contain columns   #
#            named:                       #
#                  required["conc"]       #
#                  required["time"]       #
#                  required["loscore"]    #
#                  required["hiscore"]    #
#                  required["gpsize"]     #
#                  required["weight"]     #
#    $strata - list with components:      #
#                  $intercept             #
#                  $conc                  #
#                  $time                  #
#            - character or NULL          #
#    $xlog - one of the following:        #
#             "CT", "C", "T", ""          #
#    $link - a function name              #
#    $cluster.var - variables defining    #
#                   clusters              #
###########################################

makestrata <- function(data.char, var.name=required) {

  ### obtain information about strata
  istrata <- data.char$strata[1]$intercept
  cstrata <- data.char$strata[2]$conc
  tstrata <- data.char$strata[3]$time
  # Change to fix   $ operator is invalid for atomic vectors for newer version of R - by GLN
  #istrata <- data.char$intestrata
  #cstrata <- data.char$concstrata
  #tstrata <- data.char$timestrata

  y <- list(ylo=as.numeric(data.char$data[,var.name["loscore"]]),
           yhi=as.numeric(data.char$data[,var.name["hiscore"]]))

  ### obtain design matrix
  tmp <- designx(data.char$data, istrata, cstrata, tstrata,
                 xlog=data.char$xlog, data.char$cluster.var, var.name)
  strata.obj <- obs.strata(data.char$data, istrata, cstrata, tstrata,
                     iname=tmp$iname, cname=tmp$cname,tname=tmp$tname)
  cat("\n\nStratification complete...\n")

  list(x=tmp$x, y=y, gpsize=rep(1, length(y$ylo)), gpscale=rep(1, length(y$ylo)), 
       study.label=tmp$study.label, link=data.char$link,
       strata.coefnames=strata.obj$coefnames,
       strata.factors=strata.obj$factors,
       strata.categories=strata.obj$categories,
       strata.counts=strata.obj$counts,
       strata.strata=strata.obj$strata,
       time.range=tmp$time.range,
# Begin. Ken Brown. 6/20/06. Remove "weight" and replace with "incid"
#       weight=as.numeric(data.char$data[, var.name["weight"]]))
        weight=as.numeric(data.char$data[, var.name["incid"]]))

}

######################## stratx ##############################
# x is a vector of inputs to be stratified.                  #
# cats is a vector of labels on which to stratify x.         #
##############################################################
stratx <- function(cats, x=rep(1,length(cats)), xname="") {
  ucats <- sort(unique(cats))
  num <- length(ucats)
  len <- length(cats)
  xx <- matrix(cats,len,num)
  xx <- ifelse(xx == matrix(ucats, len, num, byrow=TRUE), 1, 0)
  xx <- xx*x
  dimnames(xx) <- list(names(x), paste(ucats,xname, sep=ifelse(xname=="", "", ":")))
  xx
}
######################## cross ###############################
# Cross character valued factors:                            #
# if cfactors is a vector it is returned as the value;       #
# if cfactor is a matrix its columns are crossed to          #
# create a new factor with as many categories as the         #
# number of combinations of factor categories across factors.#
##############################################################
cross <- function(cfactors,sep=":") {
  ff <- as.matrix(cfactors)
  newfactor <- c(ff[,1])
  for(i in seq(length(newfactor)))
    newfactor[i] <- paste(ff[i,],collapse=sep)
  newfactor
}
######################### designx ############################
# Construct design matrix with intercepts, conc and duration #
# factors stratified on the factors identified in the        #
# vectors istrata, cstrata & tstrata                         #
##############################################################
designx <- function(chdata, istrata = NULL, cstrata = NULL,
                    tstrata = NULL, xlog="CT", cluster.var=NULL, var.name=required){

  n <- dim(chdata)[1]

  ### Get cluster labels

  if(is.null(cluster.var)) study.label <- rep("1",n)
  else study.label <- cross(chdata[, cluster.var])

  iname <- "INTERCEPT"

  ### extract concentration and time - log transform if requested

  if (length(grep("C", xlog)) > 0) {
    xconc <- log10(as.numeric(chdata[, var.name["conc"]]))
    cname <- "LG10CONC"
  } else {
    xconc <- as.numeric(chdata[, var.name["conc"]])
    cname <- "CONC"
  }

  if (length(grep("T", xlog)) > 0) {
    xtime <- log10(as.numeric(chdata[, var.name["time"]]))
    tname <- "LG10TIME"
  } else {
    xtime <- as.numeric(chdata[, var.name["time"]])
    tname <- "TIME"
  }
  time.range <- c(min(xtime),max(xtime))

  ### stratify intercepts

  if(length(istrata)==0) {
    xint <- as.matrix(rep(1,n))
    dimnames(xint) <- list(dimnames(chdata)[[1]], iname) 
  } else { 
    if(istrata[1]=="omit")
      xint <- iname <- NULL
    else
      #xint <- stratx(cross(chdata[,istrata]), xname=iname)
	  xint <- stratx(cross(chdata[,istrata]), xname=iname)
  }

  ### stratify concentration

  if(length(cstrata)==0) {
    xconc <- as.matrix(xconc)
    dimnames(xconc) <- list(dimnames(chdata)[[1]], cname)
    if(length(unique(xconc))==1) xconc <- cname <- NULL
  } else {
    if(cstrata[1]=="omit") {
      xconc <- cname <- NULL
    } else
      xconc <- stratx(cross(chdata[,cstrata]),xconc, xname=cname)
  }

  ### stratify duration

  if(length(tstrata)==0) {
    xtime <- as.matrix(xtime)
    dimnames(xtime) <- list(dimnames(chdata)[[1]], tname)
    if(length(unique(xtime))==1) {
      xtime <- tname <- NULL}
    } else {
      if(tstrata[1]=="omit") {
        xtime <- tname <- NULL
    } else xtime <- stratx(cross(chdata[,tstrata]), xtime, xname=tname)}

  ### combine and check for full rank

  x <- cbind(xint, xconc, xtime)

  if(qr(x)$rank < dim(x)[2])
    warning(paste("Redundant variables in the model"))

  list(x=x, study.label=study.label, time.range=time.range,
       iname=iname, cname=cname, tname=tname)
}

##########################################
# Find all (observed) combinations of    #
# intercept, concentration and duration  #
# strata - useful for generating         #
# effective concentration plots.         #
# Also determines frequencies of the     #
# the observed strata.                   #
# Reads stratum labels from the character#
# data matrix                            #
##########################################
obs.strata <- function(chdata, istrata = NULL, cstrata = NULL, 
                       tstrata = NULL, iname=NULL, cname = NULL, tname = NULL) {

  n <- dim(chdata)[1]

  ### stratification factors, categories and counts

  stratum <- rep("NONE", n)
  categories <- NULL
  findstrata <- permute <- 1
  counts <- n
  #factors <- c(istrata, cstrata, tstrata)
  factors <- c(istrata, cstrata, tstrata)
  omit <- grep("omit", as.character(factors))
  if(length(omit) > 0) factors <- factors[-omit]
  if(length(factors) > 0) {
    factors <- unique(factors)
    stratum <- cross(chdata[,factors])
    findstrata <- seq(length(stratum))[!duplicated(stratum)]
    permuted <- order(stratum[findstrata])
    categories <- chdata[findstrata[permuted],factors]
    counts <- table(stratum)
  }
  ### construct coefficient names

  ### Intercept strata
  label.int <- NULL
  if(!is.null(iname)) {
    label.int <- as.matrix(rep(iname, n))
    if(length(istrata) > 0) {
      label.int <- cross(cbind(chdata[, istrata], label.int))}
  }
  ### Concentration strata
  label.conc <- NULL
  if(!is.null(cname)) {
    label.conc <- as.matrix(rep(cname, n))
    if(length(cstrata) > 0) {
      label.conc <- cross(cbind(chdata[, cstrata], label.conc))}
  }
  ### Duration Strata
  label.time <- NULL
  if(!is.null(tname)) {
    label.time <- as.matrix(rep(tname, n))
    if(length(tstrata) > 0) {
      label.time <- cross(cbind(chdata[, tstrata], label.time))}
  }
  ### Extract unique combinations
  if(length(findstrata) > 1) {
    triplets <- as.matrix(cbind(label.int, label.conc, 
                        label.time)[findstrata[permuted],])

    dimnames(triplets) <- list(paste("[", seq(dim(triplets)[1]), ",]", sep=""),
                               c(iname, cname, tname))
  } else triplets <- c(iname, cname, tname)

  list(coefnames = triplets, factors = factors,
       categories = categories, counts = counts, strata=stratum)
}

######################################
# Function to compress stratum names #
# (for usage see ectable)            #
######################################

shortnm <- function(word, sep=":", omit=c("INTERCEPT", "LG10CONC", "LG10TIME", "CONC", "TIME")) {
  nc <- nchar(word)
  cuts <- grep(sep, substring(word,1:nc,1:nc))
  lcuts <- length(cuts)
  #short <- unique(substring(word, first=c(0,cuts)+1, last=c(cuts-1,nc)))
  short <- substring(word, first=c(0,cuts)+1, last=c(cuts-1,nc))
  paste(short[is.na(match(short,omit))],collapse=sep)
}

#########################################################
# Degrees of Freedom
# Change the format of data to calculate df
# Count the number of entries
#########################################################

count_entry <- function(dataname) {

  df.dat<-dataname; 
  if(exists("xVars")) {
	df.data<-cbind(df.dat[,xVars$info$nsub], df.dat[,"GpSize"], df.dat[,xVars$info$incid], df.dat[,xVars$info$loscore])
  } else {
	df.data<-cbind(df.dat[,"Nsub"], df.dat[,"GpSize"], df.dat[,"Incid"], df.dat[,"SevLo"]) 
  }
  colnames(df.data)<-c("Nsub", "GpSize", "Incid", "SevLo")
  mode(df.data)<-"numeric"
  df.dat<-as.data.frame(df.data);
 
  rm(df.data)

  ### initialization
  num_a <- 0;
  num_i <- 0;
  sn    <- 0;

  ### if sum of incidence in each severity is the same as Nsub, then increase entry size
  for(i in 1:nrow(df.dat)) {

      sn = sn + df.dat[i,3];

      if(sn == df.dat[i,1]) {
        num_i <- num_i + 1;
        sn <- 0;
      } else if(sn > df.dat[i,1]) {
        cat("Error detected in input, on or near line", i, ".\n");
        cat("Total incidence for a data entry does not equal number of subjects.\n");
        cat("CatReg will close (disregard the following line).\n");
        stop();
      }
  }
  return(num_i);
}


###########################################################################
###########################################################################
##  PART II: MODEL FITTING
###########################################################################
###########################################################################

###########################################################
# "go":                                                   #
#    The driver to get the intial value and               #
#    to run the icreg program.                            #
###########################################################

go <- function(data, model="cumulative odds model", total_entry=total_entry, sigFigsFit=10, getvar=TRUE, verbose=TRUE) {

  ylo <- data$y$ylo
  yhi <- data$y$yhi

  ### number of severity levels
  nsevcat <- max(c(data$y$yhi, data$y$ylo)) + 1

  ### design matrix
  X <- as.matrix(data$x)

  ### Xnames is column name of design matrix
  if (ncol(X) > 1) {
    Xnames <- dimnames(data$x)[[2]][-1]
    X <- as.matrix(X[,-1])
  }  else {
    X <- Xnames <- NULL
  }

  gpscale <- data$gpscale

  ### if group scale is missing, just put 1
  if (length(gpscale)==0) {
    cat("\n\nGroup scale missing - assuming 1...\n")
    gpscale <- rep(1, length(ylo))
  }
  layer.names <- data$study.label
  weight <- data$weight
  link <- data$link

  ### check for missing categories
  if(nsevcat != length(unique(c(yhi, ylo)))){
     miss <- seq(nsevcat) - 1
     miss <- miss[is.na(match(miss, unique(c(yhi, ylo))))]
     warning(paste("Severity category '",miss, "' is completely missing!", sep = ""))
  }

  ### get reference cell for stratified intercept parameters

  if(is.null(data$strata.factors)) ref.cell <- NULL
  else ref.cell <- data$strata.coefnames[1,"INTERCEPT"]

  ### Translate charactor names of layers to numbers
  layers <- charmatch(layer.names, unique(layer.names))

  ### Get starting value for icreg function:
  startcoefs <- pseudo(ylo,yhi,nsevcat,X,link,weight,gpscale,model,layers)
  sevnames <- paste("SEV",seq(nsevcat-1),sep="")
  names(startcoefs$coef)[seq(length(sevnames))] <- sevnames
 
  ### Use icreg function to fit the model
  cat("\nOptimizing...")

  ## This sub method filters warnings thrown by the icreg function.  Any warnings found by grepl are replaced with a custom message. 
  ##  All other warnings are thrown with default message
   icregWarn <- function(icreg_warn) {
      restart = FALSE
      if (any( grepl ( "NaNs produced", icreg_warn))) {
		  restart = TRUE
		  }	
	  ## add other warning conditions here, same as above "if" statement
      if(restart) invokeRestart("muffleWarning")		  
	  }
	  
	withCallingHandlers( ret <- c(icreg(ylo, yhi, nsevcat, layers, X, Xnames, total_entry,
           startcoefs$coef, weight, link, model, gpscale, getvar, sigFigsFit=sigFigsFit),
           list(nsevcat=nsevcat, ref.cell=ref.cell)),
		   warning = icregWarn)

  # ret <- c(icreg(ylo, yhi, nsevcat, layers, X, Xnames, total_entry,
           # startcoefs$coef, weight, link, model, gpscale, getvar),
           # list(nsevcat=nsevcat, ref.cell=ref.cell))

  ret
  

}

########################################################
# "pseudo":                                            #
#      get the starting values for "icreg" function.   #
# Remarks:                                             #
#      We adopt the "pseudo-strata" thechnique and     #
#    ignore the censored case whenever it is hard to   #
#    splid to neither "0" or "1" side. This approach   #
#    might give biased estimate(starting values) when  #
#    both mortality and severity studies exsist in the #
#    data base.                                        #
########################################################
pseudo<- function(ylo,yhi,nsevcat,X,link = "logit",weight=rep(1,length(ylo)),
                  linkscale=NULL,model="cumulative odds model", cluster) {

  ### Define pseudo-responses:
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
  
  ### for each severity level
  for (ss in 1:(nsevcat-1)) {
    if (model=="unrestricted conditional model") {
      yindx1 <- ifelse(yhi <= ss, 1,0)
      sumyindx1 <- sum(yindx1)
      yindx2 <- ifelse(yhi == ss, 1,0)
    } else {
      yindx1 <- ifelse(yhi  < ss, 1,0) + ifelse(ylo >= ss, 1,0)
      sumyindx1 <- sum(yindx1)
      yindx2 <- ifelse(ylo >= ss, 1,0)
    }
    if(sumyindx1 > 0) {
      ypseudo <- c(ypseudo,yindx2[yindx1 == 1])	
      cpseudo <- c(cpseudo, cluster[yindx1 == 1])
      xtmp <- matrix(0, nrow = sumyindx1, ncol = nsevcat - 1)
      xtmp[,ss] <- 1
      if (!is.null(X)) {
        if (model=="cumulative odds model" || model=="conditional odds model")
           XX <- X[yindx1==1,]
        else {
          XX <- matrix(0,sumyindx1,dimXX2)
          XX[,(1:dimX2)+(ss-1)*dimX2] <- X[yindx1==1,]
        }
        xtmp <- cbind(xtmp,XX)
      }
      xpseudo <- rbind(xpseudo, xtmp)
      wpseudo <- c(wpseudo, weight[yindx1 == 1])
      if (!is.null(linkscale))
        scalink <- c(scalink, linkscale[yindx1 == 1])
    }
  }

  if(is.null(linkscale)) 
    scalink <- rep(1, length(ypseudo))

  ### Binary regression with glm() to get the starting value:
  ### return the whole object
  x <- xpseudo*scalink
  y <- ypseudo


  ## This sub method filters warnings thrown by the glm function.  Any warnings found by grepl are replaced with a custom message. 
  ##  All other warnings are thrown with default message
  glmWarn <- function(glm_warn) {
      restart = FALSE
      if (any( grepl ( "fitted probabilities numerically 0 or 1 occurred", glm_warn))) {
		  fitWarn <<- "\nA problem occurred while fitting the generalized linear models, resulting in extreme link scores.  Results may not be accurate.  A different link function may offer a better fitting."
		  warning(fitWarn)
		  restart = TRUE
		  }	
	  ## add other warning conditions here, same as above "if" statement
      if(restart) invokeRestart("muffleWarning")		  
	  }
	  
	withCallingHandlers( ret <- glm(y ~ x-1, family = binomial(link=link), weights = wpseudo, x = FALSE), warning = glmWarn)
	
  # ret <- glm(y ~ x-1, family = binomial(link=link), weights = wpseudo, x = FALSE)

  ### glm() in R does not automatically return the var-cov matrix of the 
  ### coefficients. Add "var = vcov(ret)" in R to get them and 
  ### output it to the "startcoefs" object

  var <- vcov(ret)
  c(ret, list(var=var, ypseudo = ypseudo, xpseudo = xpseudo * scalink, 
              wpseudo = wpseudo, cpseudo = cpseudo))
}

################################################
# "icreg":                                     #
#     fit parallel ordinal model               #
#       for interval censored categorical      #
#       data.                                  #
# Functions directely called in this program:  #
#      "hfdeviance", "hfdeviance.grad",        #
#      "multiscore"                            #
################################################

icreg <- function(ylo,yhi,nsevcat,layers,X,Xnames, total_entry=total_entry,
                  coefs, weight=rep(1,length(ylo)),link="logit",
                  model = "cumulative odds model", linkscale=NULL, getvar=TRUE,info, sigFigsFit=10){

  Smax <- nsevcat - 1
  df.residual <- Smax*total_entry - length(coefs);

  ### Reparameterize coefs to positive constrained:
  ### increment paramenters
  A <- A.inv <-  diag(Smax)
  A[col(A) == row(A) + 1] <- - 1
  A.inv[row(A) < col(A)] <- 1
  transcoefs <- c(A %*% coefs[1:Smax], coefs[-c(1:Smax)])


  ### get names
  intnms <- paste("SEV",seq(Smax), sep ="")
  if (model=="unrestricted cumulative model") {
    Xnames_temp <- paste(rep(Xnames,Smax), rep(intnms,rep(length(Xnames),Smax)),sep=":")
  } else { 
    Xnames_temp <- Xnames
  }
  names <- c(intnms,Xnames_temp)
  
  #check transcoeffs for values close to zero and adjust these by 10^-10
  epsFit <- 10^(-1*sigFigsFit)
  adj <- abs(transcoefs) < epsFit
  sgn <- sign(transcoefs)
  sgn <- ifelse(sgn<0, -1*epsFit,epsFit)
  if (!anyNA(transcoefs)) { transcoefs[adj] <- transcoefs[adj] + sgn[adj] }


  ### call optim to get MLEs
  fit <- optim(transcoefs, fn = hfdeviance, gr = hfdeviance.grad, method = "BFGS", 
               hessian = FALSE, lower = -Inf, upper = Inf, ylo = ylo, yhi = yhi, 
               nsevcat = nsevcat, X = X, weight = weight, link = link, model=model, 
               linkscale=linkscale, control=list(reltol=max(1.5e-8, sqrt(.Machine$double.eps))))

  ### tolerance increased from default
  cat("\n", fit$message, "\n")
  gradient <- NULL
  hessian <- NULL
  variance <- NULL

  ### get parameter estimates and reparameterize them back to the original form:
  transcoefs <- fit$par
  coefs <- c(A.inv %*% transcoefs[1:Smax], transcoefs[ - c(1:Smax)])

  ### label names:
  intnms <- paste("SEV",seq(Smax), sep ="")
  Xnames.gp <- NULL
  if (model=="unrestricted cumulative model") {
     Xnames.gp <- rep(1:length(Xnames),Smax)
     Xnames <- paste(rep(Xnames,Smax), rep(intnms,rep(length(Xnames),Smax)),sep=":")
  }
  nms <- c(intnms,Xnames)
  names(coefs) <- nms

  ### Compute sandwich estimate of variance by treating
  ### clustered observations (layers) as multivariate observation
  if (getvar) {
    cat("\nComputing estimated covariance matrix...\n")
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

################################################
# "icreg.cp":                                  #
#     construct return data structure          #
#         unrestricted conditional model       #
# Functions directely called in this program:  #
#      "sandwich.glm"                          #
#
# !!! Currently, it is not used
################################################

icreg.cp <- function(ylo, yhi, nsevcat, layers, X, Xnames, total_entry, coefs,
        weight = rep(1, length(ylo)), link = "logit",
        model = "cumulative odds model", linkscale = NULL, getvar = TRUE)
{
  Smax <- nsevcat - 1
  intnms <- paste("SEV", seq(Smax), sep = "")
  Xnames.gp <- NULL

  if(model == "unrestricted conditional model") {
    Xnames.gp <- rep(1:length(Xnames), Smax)
    Xnames <- paste(rep(Xnames, Smax), rep(intnms, rep(length(Xnames),Smax)), sep = ":")
  }
  nms <- c(intnms, Xnames)
  names(coefs$coef) <- nms
    
  df <- Smax*total_entry - length(coefs$coef); 

  tracevalue=FALSE
  
  if(is.null(data.char$cluster.var)) {

    # the glm() function does not provide coefs$var as one of the returned
    # values automatically. Since $var was added by vcov(), here still use
    # coefs$var.

    dimnames(coefs$var) <- list(names(coefs$coef), names(coefs$coef))

    ret <- list(deviance = coefs$deviance, df.residual = df,
                message = NULL, coefficients = coefs$coef, 
        	gradient =NULL, hessian = NULL, 
		sand.var = coefs$var, itr.num = NULL, 
		f.evals = NULL, g.evals = NULL, scale = NULL,
                link = link, model = model, xnames = Xnames, xnames.gp = Xnames.gp)
  } else {
    xx <- coefs$xpseudo
    yy <- coefs$ypseudo
    dimnames(xx) <- NULL
               
    options(digits = 12)
    fit.obj <- glm(yy ~ xx - 1, family = binomial(link=link),  
                   weights = coefs$wpseudo, x = TRUE,trace=tracevalue,epsilon = 1e-14)

    sand.var <- sandwich.glm(fit.obj, coefs$cpseudo, coefs$wpseudo)

    dimnames(sand.var$var) <- list(names(coefs$coef), names(coefs$coef))

    ret <- list(deviance = coefs$deviance, df.residual = df, message = NULL, 
               	coefficients = coefs$coef, gradient = NULL, hessian = NULL, 
                sand.var = sand.var$var, itr.num = NULL, f.evals = NULL, 
                g.evals = NULL, scale = NULL, link = link, model = model, 
                xnames = Xnames, xnames.gp = Xnames.gp)	
    options(digits=7)
  }

  ret
        
}


##################################################
# "sandwich.glm":                                #
#   compute covariance matrix for clustered data #
# Function called:                               #
# "sandwich"                                     #
##################################################

sandwich.glm <- function(glm.obj, clust.label = NULL, weight)
{
  ### compute covariance matrix using sandwich formula
  X <- model.matrix(glm.obj)
  X <- X * sqrt(weight)
  if(is.null(clust.label))
    clust.label <- rep(1, dim(X)[1])
  resid.wk <- residuals(glm.obj, type = "response")

  ### obtain correlation coefs
  temp <- summary(glm.obj, correlation = TRUE)
  psd <- temp$coefficients[, 2]
  pcorr <- temp$correlation

  ### calculate sandwich covariance matrix
  naive.cov <- diag(psd) %*% pcorr %*% diag(psd)
  sand <- sandwich(X, resid.wk, naive.cov, clust.label)
  list(var = sand$gee.cov, naive.cov = sand$naive)
}


##################################################
# "sandwich":                                    #
#    called by "sandwich.glm"                    #
# compute sandwich covariance matrix             #
################################################## 

sandwich <- function(X, resid.wk, naive.cov = diag(ncol(X)), clust.label = rep(1, nrow(X))) {

  ucats <- unique(clust.label)
  score.matrix <- NULL	
  XX <- X * resid.wk
  n <- dim(X)[1]/length(ucats)
  score.matrix <- matrix(0, length(ucats), dim(X)[2])
  for(i in 1:n)
    score.matrix <- score.matrix + XX[((i - 1) * length(ucats) + 1): (i * length(ucats)),]

  nclust <- dim(score.matrix)[1]
  nparam <- dim(score.matrix)[2]
  score.cov <- nclust * var(score.matrix)
  gee.cov <- naive.cov %*% score.cov %*% naive.cov
  
  list(gee.cov = gee.cov, score.cov = score.cov, naive.cov = naive.cov,	score.matrix)
}


#########################################################
# "hfdeviance":                                         #
#   objective function for "optim" in "icreg"           #
#   half deviance = - logliklihood                      #
#   (proptional odds model with various links)          #
# Function called:                                      #
#    "pr.exceed"                                        #
#                                                       #
# To avoid "NaN" for hfdev, zeros in likelihood values  #
# are replaced with small number, exp(-80)              #
#########################################################

hfdeviance <- function(transcoefs, ylo, yhi, nsevcat, X, weight, link, model, linkscale=NULL) {

  ### Reparameterization
  Smax <-  (nsevcat - 1)
  A <- A.inv <- diag(Smax)
  A[col(A) == row(A) + 1] <- -1
  A.inv[row(A) < col(A)] <- 1
  coefs <- c(A.inv%*%transcoefs[1:Smax], transcoefs[ - c(1:Smax)])

  ### hfdeviance calculation
  likes <- pr.exceed(ylo, nsevcat, X, coefs, link, model, derivs = 0, linkscale) - 
           pr.exceed(yhi + 1, nsevcat, X, coefs,link, model, derivs = 0, linkscale);

  likes_tmp<-likes
  likes[likes==0] <- exp(-80)       

  hfdev <- sum(- weight*log(likes))

  #if(hfdev=="NA")
  #if(!is.finite(hfdev))
  #if(is.na(hfdev))
  if(hfdev=="NA")
    stop("fail to get deviance, the result may not reliable\n")

  hfdev
}

############################################################
# "hfdeviance.grad"                                        #
#     called by nlmin in icreg                             #
#     Gradient of half deviance                            #
#     Gradient of half deviance                            #
#     (cumulative odds model with various links)           #
# Function called:                                         #
#      "pr.exceed"                                         #
#                                                          #
# To avoid "NaN" for hfdev, zeros in likelihood values are #
# replaced with small number, exp(-80)                     # 
############################################################

hfdeviance.grad <- function(transcoefs, ylo, yhi, nsevcat, X, weight, link, model, linkscale=NULL)
{
  ### Reparameterization
  Smax <-  (nsevcat - 1)
  A <- A.inv <- diag(Smax)
  A[col(A) == row(A) + 1] <- -1
  A.inv[row(A) < col(A)] <- 1
  coefs <- c(A.inv%*%transcoefs[seq(Smax)], transcoefs[ - seq(Smax)])

  ### Gradient calculation with parameter transfered back to coefs
  hi <- pr.exceed(yhi + 1, nsevcat, X, coefs, link, model, derivs = 1, linkscale)
  lo <- pr.exceed(ylo, nsevcat, X, coefs, link, model, derivs = 1, linkscale)

  likes <- lo$pr - hi$pr

  likes_tmp<-likes
  likes[likes==0] <- exp(-80)       

  ### obtain gradient summing up all gradient of individual density function
  Gradnt <- sweep(lo$dpr - hi$dpr, 2, (weight/likes), "*")
  gradient <- apply(Gradnt, 1, sum)
  gradient[seq(nsevcat-1)]<-as.vector(gradient[seq(nsevcat -1)]%*%A.inv)

  return(gradient = (-1) * gradient)
}

############################################################
#"multiscore":                                             #
#   supply gradient vector, hessian matrix  and multiscore #
#   covariance matrix for clustered observations (layers)  #
#   (cumulative odds model with various links)             #
# Function called:                                         #
#   "pr.exceed"                                            #
# ---------------------------------------------------------#
# To avoid "NaN" for hfdev, zeros in likelihood values     #
# are replaced with small number, exp(-80)                 #
############################################################
multiscore<-function(coefs, ylo, yhi, nsevcat, layers, X, weight, link, model, linkscale=NULL){

  ### Gradient vector, hessian matrix and multiscore covariance:
  hi <- pr.exceed(yhi + 1, nsevcat, X, coefs, link, model, derivs =2, linkscale)
  lo <- pr.exceed(ylo, nsevcat, X, coefs, link, model, derivs =2, linkscale)
  likes  <-  lo$pr-hi$pr
  grads <- lo$dpr - hi$dpr
  hesses <- lo$dpr2 - hi$dpr2

  likes_tmp<-likes
  likes[likes==0] <- exp(-80)       

  ### obtain Hessian matrix
  Gradnt <- sweep(grads, 2, (weight/likes), "*")
  Gradient <- apply(Gradnt,1,sum)
  Hessian <- sweep(grads, 2, ((weight^.5)/likes), "*")
  Hessian <- apply(sweep(hesses,3,(weight/likes),"*"),c(1,2), sum) - Hessian %*% t(Hessian)

  scores <- NULL
  mxlay <- max(layers)
  if ( (!is.na(mxlay))  & (mxlay > 1) ) {
    for (lay in 1:mxlay) {
      scores <- cbind(scores,apply(as.matrix(Gradnt[,layers == lay]),1,sum))
    }
    covar <- var(t(scores))*(mxlay-1)
  } else {
#    if(zero_bg!="n" && zero_bg!="N"){
#      cat("\n\n Note: Single study analysis. \n\n")
#    }
    covar <-  - Hessian
  }

  list(gradient =  Gradient, hessian = Hessian, covar = covar)
}

########################################
# "pr.exceed":                         #
#    calculate exceedence probability, #
#    first and second derivative.      #
# Function called:                     #
#    "fs"                              #
########################################

pr.exceed <- function(sev, nsevcat, X, coefs, link, model, derivs=0, linkscale=NULL){

  if(model == "unrestricted conditional model" || model == "conditional odds model") {
    if(derivs != 0)
      stop("the derivative is not available\n")
    HH <- NULL
    for(i in sev[1]:(nsevcat - 1)) {
      xx <- matrix(0, nrow = length(sev), ncol = nsevcat - 1)
      xx[sweep(col(xx), 1, i, ">=") & sweep(col(xx), 1, i, "<=")] <- 1

      if(!is.null(X))
        if(model=="conditional odds model")
          xx <- cbind(xx,X)
        else if(model=="unrestricted conditional model") {
          X <- as.matrix(X)
          Xm.indx <- matrix(rep(1:(nsevcat - 1), rep(length(X), nsevcat - 1)), nrow = length(sev))
          Xm <- matrix(rep(X, nsevcat - 1), nrow = length(sev))
          Xm.indx <- sweep(Xm.indx, 1, i, "==")
          Xm[!Xm.indx] <- 0
          xx <- cbind(xx, Xm)
        }
      if(!is.null(linkscale))
        xx <- xx * linkscale
      xlin <- xx %*% coefs
      HH <- cbind(HH, fs(xlin, link, deriv = 0))
    }
    pr <- ifelse(sev == nsevcat, 0, ifelse(sev == 0, 1, 1-apply(1-HH, 1, prod)))
    return(pr)
  } else {
    xx <- matrix(0, nrow = length(sev), ncol = nsevcat - 1);
    xx[sweep(col(xx),1, sev, ">=") & sweep(col(xx),1,sev, "<=")] <- 1;

    if (!is.null(X))
      if (model=="cumulative odds model")
        xx <- cbind(xx,X)
      else if (model=="unrestricted cumulative model") {
        X <- as.matrix(X);
        Xm.indx <- matrix(rep(1:(nsevcat-1),rep(length(X),nsevcat-1)), nrow=length(sev));
        Xm <- matrix(rep(X,nsevcat-1), nrow=length(sev));
        Xm.indx <- sweep(Xm.indx,1,sev,"==");
        Xm[!Xm.indx] <- 0;
        xx <- cbind(xx,Xm);
      }

    ### xlin is the linear part of Generalized Linear Models
    if (!is.null(linkscale)) xx <- xx*linkscale
    xlin <- xx %*% coefs

    pr <- ifelse(sev == nsevcat, 0, ifelse(sev == 0, 1, fs(xlin, link, deriv = 0)));
 
    ### return E(y)
    if (derivs == 0 ) {
      return(pr) 
    } else 

    ### return 1st derivative
    if (derivs == 1) {
      dpr <- c(ifelse(sev == nsevcat | sev == 0, 0, fs(xlin,link, deriv = 1)));
      dpr <- t(sweep(xx,1,dpr,"*"));
      return(list(pr=pr, dpr=dpr))
    } else 

    ### return 2nd derivative
    if (derivs == 2) {
      dpr <- ifelse(sev == nsevcat | sev == 0, 0, fs(xlin,link,deriv = 1));
      dpr2 <- ifelse(sev == nsevcat | sev == 0, 0, fs(xlin,link, deriv = 2));

      crossprod <- function(z) {
        z <- as.matrix(z)
        z %*% t(z)
      }

      dpr <- sweep(t(xx), 2, dpr, "*")
      dpr2 <- sweep(array(apply(xx, 1, crossprod), c(ncol(xx), ncol(xx), nrow(xx))), 3,dpr2,"*");
  
      return(list(pr = pr, dpr = dpr, dpr2 = dpr2))     
    }
  }
}

###############################################
# "fs":                                       #
#    define link function, its 1st, 2nd order #
#      derivative functions  and its inverse  #
#      function                               #
###############################################

fs <- function(x, link = "logit", deriv = 0) {

  ### define link function, first and second derivatives:               
  link.int <- charmatch(link, c("identity", "log", "logit", "sqrt", "inverse","probit", "cloglog"))               
  if(is.na(link.int))  stop("invalid link type")
  else if(link.int == 0)  stop("Ambiguous link type")


  ### E(y) in Generalized Linear Models
  if (deriv == 0 ) {
    switch(link.int, x, exp(x),
           {
              z <- exp(ifelse(x > 80, 80, ifelse(x < -80, -80, x)))
              z/(z + 1)
            }, x^2, 1/x, pnorm(x),
            {
               z <- exp(ifelse(x > 80, 80, ifelse(x < -80,  -80, x)))
               1 - exp( - z)
            }
           )
  } else 

  ### 1st derivatives
  if (deriv == 1) {
    switch(link.int, rep(1, length(x)), exp(x),
           {
              z <- exp(ifelse(x > 80, 80, ifelse(x < -80, -80, x)))
              z/((z + 1)^2)
           }, 2 * x, -1/(x^2), 0.3989422 * exp(-0.5 * x^2),
           {
              z <- exp(ifelse(x > 80, 80, ifelse(x < -80, -80,x)))
              z * exp( - z)
           }
          )
  } else 

  ### 2nd derivatives
  if (deriv == 2) {
    switch(link.int, rep(0, length(x)), exp(x),
           {
             z <- exp(ifelse(x > 180, 180, ifelse(x < -180, -180, x)))
             (z - (z^2))/((z + 1)^3)
           }, rep(2, length(x)), 2/(x^3), -0.3989422 *  x * exp(-0.5 * x^2),
           {
             z <- exp(ifelse(x > 80, 80, ifelse(x < -80, -80,x)))
             z*(1 - z) * exp( - z)
           }
          )
  } else 

  ### inverse function
  if(deriv == -1) {
    switch(link.int, x, log(x), log(x/(1-x)), sqrt(x), 1/x, qnorm(x), log(-log(1-x)))
  } else {
    cat("\n Error occurred while calling 'fs' function\n")
    stop()
  }
}


########################################################################
########################################################################
##
## PART III: calculate EC100q lines and standard deviations
##
########################################################################
########################################################################

ectable <- function(severities, times, coefnames, factors, categories, coefs0, sand.var0, 
                    link, model, q=.1, q_ci=.95, risks, xlog, plots = FALSE) {

  coefnames1 <- severities

  ldur <- times

  ### add Gamma to coefficient name
  if((xlog=="C" || xlog=="CT") && (zero_bg=="n"|| zero_bg=="N") && model=="cumulative odds model") {   
    tmp <- ifelse(length(categories)>0, (length(categories)/length(factors)), 1)
    coefnames <- c(coefnames,rep("Gamma",tmp));
  }

  coefnames <- matrix(coefnames, byrow=FALSE,nrow=ifelse(length(categories)>0,
               (length(categories)/length(factors)),1));

  if (length(coefnames) == nrow(coefnames)) coefnames <- t(coefnames)

  ### sevnms is the name for SEV
  sevnms <- names(coefs0)[grep("SEV*",names(coefs0))]

  if(length(grep(":SEV", sevnms)) > 0) sevnms <- sevnms[ - grep(":SEV", sevnms)]        

  tmp <- length(sevnms)

  coefs <- c(coefs0[sevnms],0,coefs0[-seq(tmp)])
  sand.var <- cbind(sand.var0[,sevnms],0,sand.var0[,-seq(tmp)])
  sand.var <- rbind(sand.var[sevnms,],0,sand.var[-seq(tmp),])

  xnames <- NULL
  for(icol in seq(ncol(coefnames))) {
    xnames <- c(xnames, sort(unique(coefnames[,icol])))
  }

  if(model=="unrestricted cumulative model") {
    xnames <- c(xnames[1],paste(rep(xnames[-1],length(sevnms)),
                rep(sevnms,rep(length(xnames[-1]),length(sevnms))),sep=":"))

    ### add Gamma to coefficient name
    if((xlog=="C" || xlog=="CT") && (zero_bg=="n"|| zero_bg=="N") ) xnames <- c(xnames,"Gamma"); 
  }
  coefsnms <- names(coefs) <- c(sevnms, xnames)

  dimnames(sand.var) <- list(coefsnms,coefsnms)

  lngth <- length(coefnames1)
  if(lngth == 0) {
    coefnames1 <- "SEV1"
    lngth <- 1
  }
  sevcats <- coefnames1

  tables <- NULL
  categ <- NULL
  sev <- NULL

  if (length(grep("CONC",coefsnms))>0)

  for ( k in 1:lngth) {
    if(is.matrix(categories)) 
      categ <- rbind(categ, categories)
    else categ <- c(categ, categories)

    if( length(grep("SEV", as.character(coefnames1[k])))==0) {
      coefnamesk <- as.numeric(coefnames1[k])
      coefnamesk <- paste("SEV",coefnamesk,sep ="")
    }
    else coefnamesk <- coefnames1[k]

    coefnamesk <- cbind(coefnamesk,coefnames)

    if(model == "unrestricted cumulative model") {
      coefnamesk[,3:dim(coefnamesk)[2]]<-paste(coefnamesk[,3:dim(coefnamesk)[2]],coefnamesk[1,1],sep =":")

      ### add Gamma to coefficient name
      if((xlog=="C" || xlog=="CT") && (zero_bg=="n"|| zero_bg=="N")) coefnamesk <- cbind(coefnamesk,"Gamma"); 

      if(length(grep(":INTERCEPT",coefnamesk[, 2])) >0 && dim(coefnamesk)[1]>1)
         coefnamesk[2:dim(coefnamesk)[1],2]<-paste(coefnamesk[2:dim(coefnamesk)[1],2],coefnamesk[1,1],sep=":")
    }
    lngth1 <- nrow(coefnamesk)

    for ( j in 1:lngth1 ) {
      sev <- c(sev, severities[k])
      mtch <-  match(coefnamesk[j,],coefsnms)
      coefsj <- coefs[mtch]
      varj <- sand.var[mtch,mtch]

      if(length(grep("TIME",coefsnms))==0) {
        coefsj <- c(coefsj, 0)
        varj <- rbind(cbind(varj,0),0)
      }

      tablesj <- ecq(ldur,coefs=coefsj,covar = varj,q=q,q_ci,link=link,risks=risks,xlog=xlog)        

      ### to keep the linear scale of time in output, when log scale is used for time
      if((xlog=="T" || xlog=="CT")){
        original_time <- 10^times;
      } else {
        original_time <- times;
      }

      ### obtain ERC and CI
      tablesj <- cbind(original_time,tablesj$ecq,tablesj$sd,tablesj$lbn,tablesj$ubn)             

      ### provide names for ERC and CI
      cname <- "ERC";
      tname <- "Time";

      dimnames(tablesj)[[2]]<-c(tname,paste(cname,100*q,sep=""),"sd", 
                                paste("LB",100*q_ci,sep=""), paste("UB",100*q_ci,sep=""))

      tables <- c(tables,list(tablesj))
      names(tables)[[(k-1)*lngth1+j]] <- shortnm(paste(coefnamesk[j,], collapse = ":"))

    }
  }

  list(tables=tables, link=link, sev=sev, factors=factors,
       categories=categ, pct=100*q, qci=100*q_ci, risks=risks, xlog=xlog)
}


#########################################################################
# Function "ecq" is used to calculate the ECp values(lines)
# 'alpha1': estimate of severity intercept;
# 'alpha2': estimate of stratified intercept;
# 'beta1': estimate of the log10-concentration parameter;
# 'beta2': estimate of the log10-duration parameter;
# 'covar': 4 X 4 covarience matrix extracted from sandwich covariance
#                       matrix corresponding to above four parameters.
#########################################################################


### inverse of link function to get linear part of Generalized Linear Models
## for extra risk
logitc <- function(q,alpha1, alpha2, beta2, dur, link)
{ 
  p0 <- fs(alpha1+alpha2+beta2*dur, link, deriv=0);
  pc <- p0+(1-p0)*q;
  logit <- fs(pc, link, deriv = -1);

  return(logit);
}
## for added risk
logita <- function(q,alpha1, alpha2, beta2, dur, link)
{ 
  p0 <- fs(alpha1+alpha2+beta2*dur, link, deriv=0);
  pc <- p0+q;
  logit <- fs(pc, link, deriv = -1);
  return(logit);
}
## With Gamma
logitg <- function(q,alpha1, alpha2, beta1, beta2, gamma, dur, link)
{ 
  p0 <- fs(alpha1+alpha2+beta1*log10(gamma)+beta2*dur, link, deriv=0);
  pc <- p0+(1-p0)*q;
  logit <- fs(pc, link, deriv = -1);
  return(logit);
}

### ERC and CI
ecq <- function(dur,coefs,covar, q = 0.1, q_ci=0.95, link="logit", risks, xlog) {

  ### to approximate derivatives
  h <- 0.00000001;

  alpha1 = coefs[1]; alpha2 = coefs[2]; 
  beta1 = coefs[3];  

  ### with or without time variable
  if(length(unique(data.char$data[,colnames(data.char$data)==required['time']]))==1){
    beta2 = 0; 
  } else {
    beta2 = coefs[4];
  }

  ### When Gamma is used
  if((xlog=="C" || xlog=="CT") && risks=="extra risk") { 

    if(length(unique(data.char$data[,colnames(data.char$data)==required['time']]))==1){
      gamma = coefs[4]; 
    } else {
      gamma = coefs[5];
    }

    # EC value
    ecq <- exp((logitg(q,alpha1,alpha2,beta1,beta2,gamma,dur,link)
               - alpha1 - alpha2 - beta2*dur)/beta1*log(10))-gamma;

    ##################################
    # approximate derivatives        #
    # f`(x) = {f(x+h/2)-f(x-h/2)}/h  #
    ##################################
             
    df1 <- (logitg(q,alpha1+h/2,alpha2,beta1,beta2,gamma,dur,link)-
            logitg(q,alpha1-h/2,alpha2,beta1,beta2,gamma,dur,link))/h;
    df2 <- (logitg(q,alpha1,alpha2+h/2,beta1,beta2,gamma,dur,link)-
            logitg(q,alpha1,alpha2-h/2,beta1,beta2,gamma,dur,link))/h;
    df3 <- (logitg(q,alpha1,alpha2,beta1+h/2,beta2,gamma,dur,link)-
            logitg(q,alpha1,alpha2,beta1-h/2,beta2,gamma,dur,link))/h;

    if(length(unique(data.char$data[,colnames(data.char$data)==required['time']]))==1){
      df4 <- (logitg(q,alpha1,alpha2,beta1,beta2,gamma+h/2,dur,link)-
              logitg(q,alpha1,alpha2,beta1,beta2,gamma-h/2,dur,link))/h;
    } else {
      df4 <- (logitg(q,alpha1,alpha2,beta1,beta2+h/2,gamma,dur,link)-
              logitg(q,alpha1,alpha2,beta1,beta2-h/2,gamma,dur,link))/h;
      df5 <- (logitg(q,alpha1,alpha2,beta1,beta2,gamma+h/2,dur,link)-
              logitg(q,alpha1,alpha2,beta1,beta2,gamma-h/2,dur,link))/h;
    }

    D1 <- (ecq+gamma)*log(10)/beta1*(df1-1);
    D2 <- (ecq+gamma)*log(10)/beta1*(df2-1);
    D3 <- (ecq+gamma)*(log(10)/beta1*df3-log(ecq+gamma)/beta1);

    if(length(unique(data.char$data[,colnames(data.char$data)==required['time']]))==1){
      D4 <- (ecq+gamma)*log(10)/beta1*df4-1;
      D5 <- 0;
    } else {
      D4 <- (ecq+gamma)*log(10)/beta1*(df4-dur);
      D5 <- (ecq+gamma)*log(10)/beta1*df5-1;
    }

    ### Gradient(first derivatives) for Delta method
    D <- cbind(D1,D2,D3,D4,D5);

    ### square root of Covariance Matrix is calculated by Delta method
    sd <- sqrt(diag(D%*% covar%*% t(D)));

  } else {

    #### ERC calculated by Total Risk
    if(risks=="total risk"){
      ecq <- (fs(q, link, deriv = -1) - alpha1 - alpha2 - beta2 * dur)/beta1
      D <- cbind(-1,-1, - ecq, - dur)/beta1
      sd <- sqrt(diag(D%*% covar%*% t(D)))
    } else

    ### ERC calculated by Added Risk
    if(risks=="added risk"){      
      ecq <- (logita(q,alpha1,alpha2,beta2,dur,link) - alpha1 - alpha2 - beta2*dur)/beta1;
      D1 <- ((logita(q,alpha1+h,alpha2,beta2,dur,link)-logita(q,alpha1,alpha2,beta2,dur,link))/h-1)/beta1;
      D2 <- ((logita(q,alpha1,alpha2+h,beta2,dur,link)-logita(q,alpha1,alpha2,beta2,dur,link))/h-1)/beta1;
      D3 <- -ecq/beta1;
      D4 <- ((logita(q,alpha1,alpha2,beta2+h,dur,link)-logita(q,alpha1,alpha2,beta2,dur,link))/h-dur)/beta1;
      D <- cbind(D1,D2,D3,D4);
      sd <- sqrt(diag(D%*% covar%*% t(D)));
    } else

    ### ERC calculated by Extra Risk
    if(risks=="extra risk"){            
      p0 <- fs(alpha1 + alpha2 + beta2*dur, link, deriv = 0);
      ecq <- (logitc(q,alpha1,alpha2,beta2,dur,link) - alpha1 - alpha2 - beta2*dur)/beta1;

      ### suppress ERC value with unrealistic background probability
      ecq[p0>0.9] <- Inf;
      D1 <- ((logitc(q,alpha1+h,alpha2,beta2,dur,link)-logitc(q,alpha1,alpha2,beta2,dur,link))/h-1)/beta1;
      D2 <- ((logitc(q,alpha1,alpha2+h,beta2,dur,link)-logitc(q,alpha1,alpha2,beta2,dur,link))/h-1)/beta1;
      D3 <- -ecq/beta1;
      D4 <- ((logitc(q,alpha1,alpha2,beta2+h,dur,link)-logitc(q,alpha1,alpha2,beta2,dur,link))/h-dur)/beta1;
      D <- cbind(D1,D2,D3,D4);
      sd <- sqrt(diag(D%*% covar%*% t(D)));
    }
  }

  ### warning for negative standard error
  ### print error message only when p0 < 0.9
  if(sum(ecq==Inf, na.rm=TRUE)==0) if(length(ecq[sd <0])!=0) cat("\nError: There exists negative standard error for EC!\n")

  ### one-sided CI for EC
  lbn <- ecq - qnorm(q_ci)*sd;
  ubn <- ecq + qnorm(q_ci)*sd;

  ### When log scale is used, make it back to the linear scale
  if((xlog=="C" || xlog=="CT") && risks=="total risk") { 
    ecq <- 10^ecq;
    lbn <- 10^lbn;
    ubn <- 10^ubn;
    sd  <- sd*log(10)*ecq;
  } 

  ### warning for negative EC value
  if(length(ecq[ecq<0])!=0) cat("\nError: There exists negative EC value!\n")

  list(ecq = ecq, sd = sd, lbn=lbn, ubn=ubn);
}

#######################################
# Function ectable.cp
#  compute effective concentration for
#  unrestricted conditional model
#
# !!! Currently, it is not used
#######################################

ectable.cp <- function(severities, times, coefnames, factors,
        categories, coefs0, sand.var0,
	link, model, nsevcat, q = 0.1, xlog, plots = FALSE)
{

  coefnames1 <- severities
  ldur <- times
  coefnames <- as.matrix(coefnames)
  if(length(coefnames) == nrow(coefnames)) coefnames <- t(coefnames)	
  sevnms <- names(coefs0)[grep("SEV", names(coefs0))]
  if(length(grep(":SEV", sevnms)) > 0) sevnms <- sevnms[ - grep(":SEV",sevnms)]	
  tmp <- length(sevnms)	

  coefs <- c(coefs0[sevnms], 0, coefs0[ - seq(tmp)])
  sand.var <- cbind(sand.var0[, sevnms], 0, sand.var0[,  - seq(tmp)])
  sand.var <- rbind(sand.var[sevnms,  ], 0, sand.var[ - seq(tmp),  ])
  xnames <- NULL
  for(icol in seq(ncol(coefnames))) {
    xnames <- c(xnames, sort(unique(coefnames[, icol])))
  }

  if(model == "unrestricted conditional model") 
    xnames <- c(xnames[1], paste(rep(xnames[-1], length(sevnms)),
      rep(sevnms, rep(length(xnames[-1]), length(sevnms))), sep = ":"))
  coefsnms <- names(coefs) <- c(sevnms, xnames)
  dimnames(sand.var) <- list(coefsnms, coefsnms)	
  lngth <- length(coefnames1)
  if(lngth == 0) {
    coefnames1 <- "SEV1"
    lngth <- 1 }
  sevcats <- coefnames1	#
  tables <- NULL
  categ <- NULL
  sev <- NULL

# leave tables null if no concentration variable..
  if(length(grep("CONC", coefsnms)) > 0)
  for(k in 1:lngth) {
	# 5/20/1999 - lmfu
    coefnamesk.tmp <- NULL
    for(ii in as.numeric(severities[k]):(nsevcat - 1)) {
	if(is.matrix(categories))
	  categ <- rbind(categ, categories)
	else categ <- c(categ, categories)

#   make coefnames1[k] as a character.
       if(length(grep("SEV", as.character(coefnames1[k]))) == 0) {
	  coefnamesk <- as.numeric(coefnames1[k])
	  coefnamesk <- paste("SEV", ii, sep = "")
	}
	else coefnamesk <- coefnames1[k]
	coefnamesk <- cbind(coefnamesk, coefnames)
	# 3/25/1999 - lmfu
        if(model == "unrestricted conditional model") {
          coefnamesk[, 3:dim(coefnamesk)[2]] <-
          paste(coefnamesk[, 3:dim(coefnamesk)[2]],coefnamesk[1, 1], sep = ":")
  if(length(grep(":INTERCEPT", coefnamesk[, 2])) > 0 && dim(coefnamesk)[1] > 1)
          coefnamesk[2:dim(coefnamesk)[1], 2] <-
          paste(coefnamesk[2:dim(coefnamesk)[1], 2],coefnamesk[1, 1], sep = ":")
        }
        coefnamesk.tmp <- cbind(coefnamesk.tmp, coefnamesk)
    }
    coefnamesk <- coefnamesk.tmp
    lngth1 <- nrow(coefnamesk)
    cat("\nComputing ")

	temp2 = NULL
    for(j in 1:lngth1) {
	sev <- c(sev, severities[k])
	mtch <- match(coefnamesk[j,  ], coefsnms)
	coefsj <- coefs[mtch]
	varj <- sand.var[mtch, mtch]
	# added 11/14/95 - in case TIME is not in the model
	if(length(grep("TIME", coefsnms)) == 0) {
	  tmp.indx <- (1:3) + rep(4 * (0:(length(coefsj)/3 - 1)),
		rep(3, length(coefsj)/3))
	  tmp <- rep(0, length(coefsj)/3 * 4)
	  tmp[tmp.indx] <- coefsj
	  coefsj <- tmp
	  tmp <- matrix(0, length(coefsj), length(coefsj))
	  tmp[tmp.indx, tmp.indx] <- varj
	  varj <- tmp
	}
	coefsj <- matrix(coefsj, byrow = TRUE, ncol = 4)

	  tablesj <- ecq.cp(ldur, alpha1 = coefsj[, 1],
	    alpha2 = coefsj[, 2], beta1 = coefsj[, 3],
	    beta2 = coefsj[, 4], covar = varj, q = q,
	    link = link)  
	    
	    # Check whether the sd is negative!

        if (tablesj$sd[1]<0) { 
	        temp1 <- FALSE
	        temp2 <- c(temp2,temp1)
        }      
        
	tablesj <- cbind(ldur, tablesj$ecq, tablesj$sd, tablesj$lbn90, tablesj$ubn90,tablesj$lbn95, tablesj$ubn95)
        cname <-
        ifelse(length(grep("C", xlog)) > 0, "LG10EC", "EC")
        tname <-
        ifelse(length(grep("T", xlog)) > 0, "LG10TIME", "TIME")
        dimnames(tablesj)[[2]] <- c(tname,
             paste(cname,100*q,sep=""),"sd","LB90", "UB90","LB95", "UB95")
        tables <- c(tables,list(tablesj))
        names(tables)[[(k-1)*lngth1+j]] <-
             shortnm(paste(coefnamesk[j,1:3], collapse = ":"))
    }
    cat("\n")
  }
  if (min(temp2)==0)  {
	  cat("\n##### !!! Warning - Standard Deviation is negative !!! #####", file = stderr())	  
  }
  list(tables = tables, link = link, sev = sev, factors = factors,
		categories = categ, pct = 100 * q, xlog = xlog)
}


#########################################################################
# Function "ecq.cp" is used to calculate the ECp values(lines)
#   for unrestricted conditional model
# 'alpha1': estimate of severity intercept;
# 'alpha2': estimate of stratified intercept;
# 'beta1': estimate of the log10-concentration parameter;
# 'beta2': estimate of the log10-duration parameter;
# 'covar': 4 X 4 covarience matrix extracted from sandwich covariance
#                       matrix corresponding to above four parameters.
#
# !!! Currently, it is not used
#########################################################################

ecq.cp <- function(dur, alpha1, alpha2, beta1, beta2, covar, q = 0.1, link = "logit"){

  # Effective concentration:
	flag <- 0
	ecq <- rep(0, length(dur))
	for(i in 1:length(unique(dur))) {
		cat(".")
		try <- seq( - 10^6, 10^6, length = 10)
		try.out <- ecq.cp.fn(try, q = q, dur = unique(dur)[i], alpha1
			 = alpha1, alpha2 = alpha2, beta1 = beta1, beta2 =
			beta2, link = link)
		up <- min(try[try.out < 0])
		lo <- max(try[try.out > 0])
		if(up == "NA" || lo == "NA") {
			flag <- 1
			ecq0 <- NA
		}
		else ecq0 <- uniroot(ecq.cp.fn, interval = c(lo, up),
			q = q, dur = dur[i], alpha1 = alpha1,
			alpha2 = alpha2, beta1 = beta1,
			beta2 = beta2, link = link)$root
		ecq[dur == unique(dur)[i]] <- ecq0
	}
# Standard errors:
	HH <- NULL
	HP <- NULL
	for(i in 1:length(alpha1)) {
		xx <- alpha1[i] + alpha2[i] + beta1[i] * ecq + beta2[i] * dur
		HH <- cbind(HH, fs(xx, link = link, deriv = 0))
		HP <- cbind(HP, fs(xx, link = link, deriv = 1))
	}
	PG <-  - HP/(1 - HH)
	GEC <- apply(t( - PG) * beta1, 2, sum)
        D <- NULL
        for(i in 1:ncol(PG))
                D <- cbind(D, PG[, i], PG[, i], PG[, i] * ecq, PG[, i] * dur)
	sd <- D %*% covar %*% t(D)
	sd <- sqrt(diag(sd))/GEC

# Begin Added by C. Ahn on 06/08/2005

        lbn95 <- ecq - qnorm(0.975)*sd;
        ubn95 <- ecq + qnorm(0.975)*sd;

        lbn90 <- ecq - qnorm(0.95)*sd;
        ubn90 <- ecq + qnorm(0.95)*sd;

	if(flag == 1)
	  cat("\nthe effective concentration may not exist or not be unique\n")

	# list(ecq = ecq, sd = sd)

	list(ecq = ecq, sd = sd, lbn90=lbn90, ubn90=ubn90,lbn95=lbn95, ubn95=ubn95);

# End Changed by C. Ahn on 06/08/2005

}


#########################################################################
# Function "ecq.cp.fn" is the objective function for computing EC values
#   for unrestricted conditional model
#
# !!! Currently, it is not used
#########################################################################

ecq.cp.fn <- function(ecq, q, dur, alpha1, alpha2, beta1, beta2, link)
{
	ret <- NULL
	for(i in 1:length(ecq)) {
		xx <- alpha1 + alpha2 + beta1 * ecq[i] + beta2 * dur
		HH <- fs(xx, link = link, deriv = 0)
		ret <- c(ret, prod(1 - HH) - (1 - q))
	#		cat("ecq", ecq, "ret", ret, "\n")
	}
	ret
}


##########################################
# write ERC data to file                 #
# add codes to handle the ERD output     #
##########################################
output <- function(out=ECline$tables, logfile=NULL, xlog=ECline$xlog) {

  if(length(logfile)>0) sink(file=logfile, append=TRUE)
  num <- length(out)
  strata <- names(out)[seq(num)]

  tname <- "Time";

  ### ERC
  if(length(grep("total", ECline$risks, ignore.case=TRUE))>0  && 
     length(grep("gamma", names(fits$coefficients), ignore.case=TRUE)) ==0 ) {
     risk.case <- "extra risk"
  } else {
    risk.case <- ECline$risks
  }          
     
  erdans<-ifelse(length(grep("extra", risk.case, ignore.case=TRUE))>0,"Y", "N")
  if (erdans=="y" || erdans=="Y")
    tmp_ec <- "ERC"
  else
    tmp_ec <- "EC"  
  
  ### output ERC for each stratum
  for(i in seq(num)) {

    ECout <- paste(paste(outname(outfile),as.character(i),sep=""),"txt",sep=".")

    ### precision of ERC and confidence interval
    rout <- round(out[[i]], 2);
    df <- data.frame(rout);
    colnames(df)[2] <-  paste(tmp_ec,ECline$pct, sep="")
    write.table(df, ECout, row.names=FALSE, sep="\t");

    tmp1 <- paste("\n \n ** Stratum :",strata[i], sep="")
    tmp2 <- paste("\n ** Risk Type :", risk.case, sep="")
    tmp3 <- paste("\n ** Above UB",ECline$qci," and LB",ECline$qci,":",sep="")
    tmp4 <- paste("    These One-sided ", ECline$qci,"% lower bound and One-sided ",
                  ECline$qci ,"% upper bound ", sep="")
    tmp4.1 <- c("confidence Intervals are equivalent to ")
    tmp5 <- paste("the lower bound and upper bound of Two-sided ",
                 (1-(1-ECline$qci/100)*2)*100,"% confidence intervals.", sep="")
                 
    tmp6 <- NULL
################################################################
# Modified by GyoungJin Ahn : 03/02/2006              
#  Show some footnotes for Infinite, NaN("Not a Number" (R value)), NA("Missing value") 
# in the output file. 
# marple 
################################################################
# Begin: added by GyoungJin Ahn : 03/02/2006
    if(!is.finite(max(df[,2]))) {
    tmp6 <- paste(c("\n\n ** \"Inf\", \"NA\" ane \"NaN\" indicates that ERC was not calculated ")
                  ,"\n because estimated background risk "
                  ,"( i.e., probability of response at zero concentration) \nexceeds 0.9."
                  , sep="")
    }
                 
    tmp <- rbind(tmp1,tmp2,tmp3,  tmp4,tmp4.1,tmp5, tmp6)
# End: added by GyoungJin Ahn : 03/02/2006

    tmp <- data.frame(tmp)

    write.table(tmp, ECout,append=TRUE,col.names =FALSE,row.names=FALSE,quote =FALSE, sep="\n")

    cat("\nFile:",ECout,"  Stratum:",strata[i],"\n")
  }
  if(length(logfile)>0) sink()
}


###############################
# Summary function for catr eg #
###############################

catreg.summary <- function (ecfits=NULL,file=NULL,short=TRUE,append=FALSE,
		            xlog = data.char$xlog,var.name=required, datax = data.char, sigFigsFit=10)
{
  if (!is.null(ecfits))
    fits <- ecfits

  ### name output file
  ### default name is the data name with extension "out"
  if (!is.null(file)) {
    cat ("\nWriting summary data to file:", file)
    sink(file=file, append=append)
  }

  ### Informational stuff
  cat ("\n")
  cat ("Input file   :", data.char$infile,   "\n")
  cat("\nModel Variable Mapping:\n")
  #cat(paste("    Dose: ", xVars$info$conc, "\n", sep=""))
  #cat(paste("    Time: ", xVars$info$time, "\n", sep=""))
  #cat(paste("    Incidence: ", xVars$info$incid, "\n", sep=""))
  #cat(paste("    N: ", xVars$info$nsub, "\n", sep=""))
  #cat(paste("    Sev Lo: ", xVars$info$loscore, "\n", sep=""))
  #cat(paste("    Sev Hi: ", xVars$info$hiscore, "\n", sep=""))
  cat("    Dose: ", xVars$info$conc, "\n")
  if (xVars$info$mappedTime){
    cat("    Time: ", xVars$info$time, "\n")
  } else {
    cat("    Time: [Not Used]\n")
  }
  cat("    Incidence: ", xVars$info$incid, "\n")
  cat("    N: ", xVars$info$nsub, "\n")
  cat("    Sev Lo: ", xVars$info$loscore, "\n")
  if (xVars$info$mappedSevHi){
    cat("    Sev Hi: ", xVars$info$hiscore, "\n")
  } else {
    cat("    Sev Hi: [Not Used]\n")
  }
  cat("\n")
  
  cat("Model Specifications:\n")
  cat ("    Model: ", fits$model, "\n")
  cat ("    Link: ", fits$link,   "\n")
  cat ("    Assume Zero Background Response: ", xVars$info$ZeroBackResponse, "\n")
  cat ("    Worst Case Analysis: ", xVars$info$WorstCaseAnalysis, "\n")
  if (length(grep("C",xlog)) >0)
    cat ("    Scale for Dose(Concentration):", "log10(", var.name["conc"],")", "\n")
  else  
    cat ("    Scale for Dose(Concentration):", var.name["conc"], "\n")
  if (length(grep("T",xlog)) >0)
    cat ("    Scale for Time(Duration):", "log10(", var.name["time"], ")", "\n")
  else
    cat ("    Scale for Time(Duration):", var.name["time"], "\n")
  cat("\n")
  
  cat("BMD Specifications:\n")
  cat("    Risk: ", xVars$info$Risks, "\n")
  cat("    BMR: ", xVars$info$Bmr, "\n")
  cat("    Confidence Level(%): ", xVars$info$ConfidenceLevel, "\n")
  cat("    Time: ", xVars$info$duration, "\n")  
  cat("\n")
  
  cat ("Filtered data: ");
  if (is.null(data.char$excluded))
    cat ("none\n")
  else {
    cat (data.char$excluded$names, ": ")
    i <- 2  
    while (i <= length(data.char$excluded)){
      cat(data.char$excluded[[i]], " ")
      i <- i+1}
     cat ("\n")
   } 
  cat ("Clustering   : ");

  if (is.null(data.char$cluster.var))  cat ("none\n")
  else cat (data.char$cluster.var, "\n")
  
  cat ("Stratification:\n");

  #THIS CODE WAS REMARKED TO FIX $ operator is invalid for atomic vectors for newer R version - by GLN
  if(is.null(data.char$strata$intercept) && is.null(data.char$strata$conc) && 
     is.null(data.char$strata$time) ) {
    cat ("     No Stratification on Intercept, Concentration and Duration.    \n");     
  } else {
    cat ("     Intercept    :", data.char$strata$intercept, "\n");
    cat ("     Dose(Concentration):", data.char$strata$conc,      "\n");
    cat ("     Time(Duration)     :", data.char$strata$time,      "\n");
  }
  cat("\n")
   

  sev<-fits$coefficients[1:(fits$nsevcat-1)];
  slope <- fits$coefficients[fits$nsevcat:length(fits$coefficients)]

  ### AIC 
  
  digits=sigFigsFit-1  #number of digits to round sev for comparision
  #order.df <- length(sev) - length(unique(sev));
  order.df <- length(sev) - length(unique(round(sev,digits=digits)));
  nonneg.df <- sum(slope==0);
  num.para <- length(fits$coefficients) - order.df - nonneg.df;

  df.corrected <- fits$df.residual + order.df + nonneg.df;

  AIC = fits$deviance+2*num.para;

  

  cat ("Message      :", fits$message,       "\n")
  cat ("Iterations   :", fits$itr.num,       "\n")
  cat ("Deviance     :", fits$deviance,      "\n")
  cat ("Residual DF  :", df.corrected,   "\n")
  cat ("AIC          :", AIC,  "\n\n")

  

 #Changed to fix   $ operator is invalid for atomic vectors for newer R version - by GLN
 #if(is.null(datax$intestrata) && is.null(datax$concstrata) && is.null(datax$timestrata) ) {
 #cat ("     No Stratification on Intercept, Concentration and Duration.    \n");
 #} else {
#	 catintestrata=""
#	 catconcstrata=""
#	 cattimestrata=""
#	if(is.null(datax$intestrata)) {
#		catintestrata="None"
#	} else {
#		if(length(datax$intestrata) > 0) {
#			for(val in datax$intestrata) {
#				if(catintestrata == "") catintestrata = val
#				else catintestrata = paste(catintestrata, val, sep = " ")
#			}
#		}
#	}
#	if(is.null(datax$concstrata)) {
#		catconcstrata="None"
#	} else {
#		if(length(datax$concstrata) > 0) {
#			for(val in datax$concstrata) {
#				if(catconcstrata == "") catconcstrata = val
#				else catconcstrata = paste(catconcstrata, val, sep = " ")
#			}
#		}
#	}
#	if(is.null(datax$timestrata)) {
#		cattimestrata="None"
#	} else {
#		if(length(datax$timestrata) > 0) {
#			for(val in datax$timestrata) {
#				if(cattimestrata == "") cattimestrata = val
#				else cattimestrata = paste(cattimestrata, val, sep = " ")
#			}
#		}
#	}
#	cat ("     Intercept    :", catintestrata, "\n");
#	cat ("     Concentration:", catconcstrata, "\n");
#	cat ("     Duration     :", cattimestrata, "\n");
# }

  ### Coefficients & statistics...
  stats <-  matrix(nrow=length(coef(fits)),ncol=4)
  dimnames(stats) <-  list(names(coef(fits)), c("  Estimate",
    "Std. Error","  Z-Test=0","   p-value"))
  stats[,1] <- coef(fits)
  if (!is.null(fits$sand.var)) {
    stats[,2] <- sqrt(diag(fits$sand.var))
    stats[,3] <- stats[,1]/stats[,2]

  ### correction for two-tailed test
    pvalue <- pmin(1, 2*pnorm(-abs(stats[,3])))
    stats[,4] <- round(pvalue,digits=5)
    stats[pvalue<.00001,4] <- .00001
  }
  if(!is.null(fits$ref.cell)) {
    stats <- rbind(stats[seq(fits$nsevcat-1),], c(0,0,NA,NA),
                   stats[-seq(fits$nsevcat-1),])
    dimnames(stats)[[1]][fits$nsevcat] <- fits$ref.cell
  }

  cat ("\nCoefficients:\n")
  print (stats)


  if((data.char$xlog=="C" || data.char$xlog=="CT") && (zero_bg=="n" || zero_bg=="N")){

    ### check whether gamma estimate hits its maximum bound or not
    max_bound_gamma()
  }

  cat("\n");

  names<-names(fits$coefficients);

# Modified by GyoungJin Ahn : 03/02/2006  
  error.content <- NULL
  ### Check order constraints
  index.order=0; index.order.zero=0;
  
  if(fits$nsevcat>2){
    for(i in 1:(fits$nsevcat-2)){ 
      if(sev[i] < sev[i+1]){
        # cat("Sev", i, " < ", "Sev", i+1, "!\n");
        error.content <- paste("Sev", i, " < ", "Sev", i+1, "!\n", sep="");
        index.order=index.order+1; 
      } else if(sev[i]==sev[i+1]){
        # cat("Sev", i, " = ", "Sev", i+1, "!\n");      
        error.content <- paste("Sev", i, " = ", "Sev", i+1, "!\n", sep="");      
        index.order.zero=index.order.zero+1; 
      }
    }
  }

  ### turn off error message due to stop when order constraint or non-negativity constraint is not satisfied
  #options(show.error.messages=FALSE)

  ### Check whether order constraint is satisfied or not
  if(index.order){ 
    #cat("Estimates of severity parameters do not satisfy constraint on order of parameters.\n");
    #cat("Incorrectly ordered severity estimates is evidence of too many severity levels in the data.\n");
    #cat("This run will terminate.\n\n"); 
    
     error.content <- cbind( error.content
                            ,"Estimates of severity parameters do not satisfy constraint on order of parameters.\n"
                            , "Incorrectly ordered severity estimates is evidence of too many severity levels in the data.\n"
                            , "This run will terminate.\n\n" )
     stop(error.content, call.=FALSE);
  }


  TC <- sort(c(grep("CONC",names),grep("TIME",names)));

  # Modified by GyoungJin Ahn : 03/02/2006  
  error.content <- NULL
  
  index.negative=0;
  index.negative.zero=0;
  for(i in 1:length(TC))
    if(fits$coefficients[TC[i]]<0){
      # cat(names[TC[i]], " is negative!\n");
      error.content <- paste(names[TC[i]], " is negative!\n", sep="")
      index.negative=index.negative+1;
    } else if(fits$coefficients[TC[i]]==0){
      # cat(names[TC[i]], " is zero!\n");
      error.content <- paste(names[TC[i]], " is zero!\n", sep="")
      index.negative.zero=index.negative.zero+1;
    } 

  ### Check whether non-negativity constraint is satisfied or not
  if(index.negative){
    #cat("Estimates of coefficient parameters do not satisfy non-negativity constraint on the parameters.\n");
    #cat("A negative estimate is evidence of no effect.  This run will terminate.\n\n");

     error.content <- cbind( error.content
                            ,"Estimates of coefficient parameters do not satisfy non-negativity constraint on the parameters.\n"     
                            ,"A negative estimate is evidence of no effect.  This run will terminate.\n\n")
    stop(error.content, call.=FALSE);
      ### turn on error message before the stop 
  # from order constraint or non-negativity constraint is not satisfied

  }

  ### Other statictics

  if (!short) {
    cat ("Variance:\n")
    print (fits$sand.var)
    cat("\n");
  }

  ### Output stuff to terminal again
  if (!is.null(file))
    sink()

  invisible()
    ### turn on error message before the stop 
  # from order constraint or non-negativity constraint is not satisfied

}

######################
# function to add escaped quotes around string"
######################
escquote <- function (string)
{
  return (paste("\"",string,"\"",sep=""))
}


###############################
# CSV Summary function for catr eg #
###############################

catreg.csv <- function (ecfits=NULL,file=NULL,short=TRUE,append=FALSE,
		            xlog = data.char$xlog,var.name=required, datax = data.char, sigFigsFit=10)
{
  file = gsub(".otx","_summary.csv",file)
  
  cat("Writing file to: ", file)

  sink(file=file, append=FALSE)
  version() # Output the current version of CatReg by C. Ahn on 06/13/2005
  if(exists("xVars")) {
	cat(paste("\nRun Date:,",escquote(xVars$info$Date),"\n",sep=""))
	cat(paste("Source data file:,",escquote(data.char$infile), "\n",sep=""))
  } else {cat(paste("\nSource data file:,", escquote(infile), "\n",sep=""))}

  cat("Type of analysis:,", ifelse(worstcase, "Worst-case", "Censored"), "\n")

  ### Informational stuff
  cat ("\n")
  cat("\nModel Variable Mapping:\n")
  cat(paste(",Dose:,", escquote(xVars$info$conc), "\n",sep=""))
  if (xVars$info$mappedTime){
    cat(paste(",Time:,", escquote(xVars$info$time), "\n",sep=""))
  } else {
    cat(",Time:,[Not Used]\n")
  }
  cat(paste(",Incidence:,", escquote(xVars$info$incid), "\n",sep=""))
  cat(paste(",N:,", escquote(xVars$info$nsub), "\n",sep=""))
  cat(paste(",Sev Lo:,", escquote(xVars$info$loscore), "\n",sep=""))
  if (xVars$info$mappedSevHi){
    cat(paste(",Sev Hi:,", escquote(xVars$info$hiscore), "\n",sep=""))
  } else {
    cat(",Sev Hi:,[Not Used]\n")
  }
  cat("\n")

  cat("Model Specifications:\n")
  cat (",Model:,", fits$model, "\n")
  cat (",Link:,", fits$link,   "\n")
  cat (",Assume Zero Background Response:,", xVars$info$ZeroBackResponse, "\n")
  cat (",Worst Case Analysis:,", xVars$info$WorstCaseAnalysis, "\n")
  if (length(grep("C",xlog)) >0)
    cat (",Scale for Dose(Concentration):,", "log10(", var.name["conc"],")", "\n")
  else  
    cat (",Scale for Dose(Concentration):,", var.name["conc"], "\n")
  if (length(grep("T",xlog)) >0)
    cat (",Scale for Time(Duration):,", "log10(", var.name["time"], ")", "\n")
  else
    cat (",Scale for Time(Duration):,", var.name["time"], "\n")
  cat("\n")

  cat("BMD Specifications:\n")
  cat(",Risk:,", xVars$info$Risks, "\n")
  cat(",BMR:,", xVars$info$Bmr, "\n")
  cat(",Confidence Level(%):,", xVars$info$ConfidenceLevel, "\n")
  cat(",Time:,", paste(xVars$info$duration,collapse=","), "\n")  
  cat("\n")
  
  if (is.null(data.char$excluded))
    cat ("Filtered data:,none\n")
  else {
    cat ("Filtered data:\n")
	for (i in c(1:(length(data.char$excluded)-1))){
	  cat(paste(",",escquote(data.char$excluded$names[i]),":",sep=""))
	  cat(paste(",\"",paste(data.char$excluded[[i+1]],collapse="\",\""),sep=""))
	  cat("\"\n")
	}
   } 
  cat ("Clustering:");

  if (is.null(data.char$cluster.var))  {
    cat (",none\n")
  }else {
    i<-1
    for (i in c(1:length(data.char$cluster.var))){
      cat(paste(",",escquote(data.char$cluster.var[i]),sep=""))
	  i<-i+1
	}
  }
  
  cat ("\nStratification:,");

  #THIS CODE WAS REMARKED TO FIX $ operator is invalid for atomic vectors for newer R version - by GLN
  if(is.null(data.char$strata$intercept) && is.null(data.char$strata$conc) && 
     is.null(data.char$strata$time) ) {
    cat ("\"No Stratification on Intercept, Concentration and Duration.\"\n");     
  } else {
    cat ("Intercept:,", data.char$strata$intercept,"\n");
    cat (",Dose(Concentration):,", data.char$strata$conc,"\n");
    cat (",Time(Duration):,", data.char$strata$time,"\n");
  }
  cat("\n")
  
  sev<-fits$coefficients[1:(fits$nsevcat-1)];
  slope <- fits$coefficients[fits$nsevcat:length(fits$coefficients)]
  ### AIC 
  
  digits=sigFigsFit-1  #number of digits to round sev for comparision
  #order.df <- length(sev) - length(unique(sev));
  order.df <- length(sev) - length(unique(round(sev,digits=digits)));
  nonneg.df <- sum(slope==0);
  num.para <- length(fits$coefficients) - order.df - nonneg.df;
  df.corrected <- fits$df.residual + order.df + nonneg.df;
  AIC = fits$deviance+2*num.para;

  cat (paste("Message:,", escquote(fits$message),"\n",sep=""))
  cat ("Iterations:,", paste(fits$itr.num,collapse=","),"\n")
  cat ("Deviance:,",fits$deviance,"\n")
  cat ("Residual DF:,",df.corrected,"\n")
  cat ("AIC:,",AIC,"\n\n")
  
  
  ### Coefficients & statistics...
  stats <-  matrix(nrow=length(coef(fits)),ncol=4)
  dimnames(stats) <-  list(names(coef(fits)), c("  Estimate",
    "Std. Error","  Z-Test=0","   p-value"))
  stats[,1] <- coef(fits)
  if (!is.null(fits$sand.var)) {
    stats[,2] <- sqrt(diag(fits$sand.var))
    stats[,3] <- stats[,1]/stats[,2]

  ### correction for two-tailed test
    pvalue <- pmin(1, 2*pnorm(-abs(stats[,3])))
    stats[,4] <- round(pvalue,digits=5)
    stats[pvalue<.00001,4] <- .00001
  }
  if(!is.null(fits$ref.cell)) {
    stats <- rbind(stats[seq(fits$nsevcat-1),], c(0,0,NA,NA),
                   stats[-seq(fits$nsevcat-1),])
    dimnames(stats)[[1]][fits$nsevcat] <- fits$ref.cell
  }

  cat ("\nCoefficients:\n")
  #print (stats)
  cat(",,",paste(colnames(stats),collapse=","),"\n")
  for (i in  c(1:length(rownames(stats)))){
     cat(paste(",",rownames(stats)[i],",", paste(stats[i,],collapse=","),"\n",sep=""))
  }
  
  
  if((data.char$xlog=="C" || data.char$xlog=="CT") && (zero_bg=="n" || zero_bg=="N")){

    ### check whether gamma estimate hits its maximum bound or not
    max_bound_gamma()
  }
  cat("\n");
  names<-names(fits$coefficients);
    
  error.content <- NULL
  ### Check order constraints
  index.order=0; index.order.zero=0;
  
  if(fits$nsevcat>2){
    for(i in 1:(fits$nsevcat-2)){ 
      if(sev[i] < sev[i+1]){
        # cat("Sev", i, " < ", "Sev", i+1, "!\n");
        error.content <- paste("Sev", i, " < ", "Sev", i+1, "!\n", sep="");
        index.order=index.order+1; 
      } else if(sev[i]==sev[i+1]){
        # cat("Sev", i, " = ", "Sev", i+1, "!\n");      
        error.content <- paste("Sev", i, " = ", "Sev", i+1, "!\n", sep="");      
        index.order.zero=index.order.zero+1; 
      }
    }
  }

  ### Check whether order constraint is satisfied or not
  if(index.order){ 
    
     error.content <- cbind( error.content
                            ,"Estimates of severity parameters do not satisfy constraint on order of parameters.\n"
                            , "Incorrectly ordered severity estimates is evidence of too many severity levels in the data.\n"
                            , "This run will terminate.\n\n" )
     stop(error.content, call.=FALSE);
  }

  TC <- sort(c(grep("CONC",names),grep("TIME",names)));

  # Modified by GyoungJin Ahn : 03/02/2006  
  error.content <- NULL
  
  index.negative=0;
  index.negative.zero=0;
  for(i in 1:length(TC))
    if(fits$coefficients[TC[i]]<0){
      error.content <- paste(names[TC[i]], " is negative!\n", sep="")
      index.negative=index.negative+1;
    } else if(fits$coefficients[TC[i]]==0){
      error.content <- paste(names[TC[i]], " is zero!\n", sep="")
      index.negative.zero=index.negative.zero+1;
    } 

  ### Check whether non-negativity constraint is satisfied or not
  if(index.negative){
     error.content <- cbind( error.content
                            ,"Estimates of coefficient parameters do not satisfy non-negativity constraint on the parameters.\n"     
                            ,"A negative estimate is evidence of no effect.  This run will terminate.\n\n")
    stop(error.content, call.=FALSE);
      ### turn on error message before the stop 
  }
  
   ### Other statictics

  if (!short) {
    cat ("Variance:\n")
	cat(",,",paste(colnames(fits$sand.var),collapse=","),"\n")
     for (i in  c(1:length(rownames(fits$sand.var)))){
      cat(paste(",",rownames(fits$sand.var)[i],",", paste(fits$sand.var[i,],collapse=","),"\n",sep=""))
     }
    cat("\n");
  }
  

sink()
}


##########################################
# Hypothesis testing functions           #
##########################################
partest <- function(test.ze=NULL, test.eq=NULL, test.egroup=NULL,
                    type=NULL, testmatrix=NULL, coefs=fits$coefficients,
                    covar=fits$sand.var, popup=FALSE, do.summary=TRUE, file=NULL,
                    short=TRUE, append=FALSE, parallel=FALSE) {
 
  # Work around an issue related to the CatReg GUI...
  # When filtering an explanatory variable (e.g. Species), it is possible that
  # all occurrences of a given severity level might be filtered out. Running a
  # test against this severity will then produce an error. Eventually, the
  # CatReg GUI should handle this situation, but for now we deal with it here.

  if (exists("xVars") && !is.null(xVars)) {
    # We only worry about the highest severity level. If this happens with an
    # intermediate severity, the R code gives a reasonable error.
    iMaxSev <- max(data$y$ylo) # Highest sev after filtering
    if (xVars$info$sevs > iMaxSev) {
      sFiltSev <- paste0("SEV", xVars$info$sevs)
      if (!is.null(test.ze)) {
        if (sFiltSev %in% test.ze) {
          cat(strwrap(paste0("Not running 'test for zero' for ("
              ,paste0(test.ze, collapse=", "), ") because all ", sFiltSev
              ," rows were filtered out.\n"), width=72, initial="*** Note: "
              ,prefix="          "), sep="\n")
          return(invisible())
        } # if (sFiltSev %in% test.ze)
      } # !is.null(test.ze)
      if (!is.null(test.eq)) {
        if (sFiltSev %in% test.eq) {
          cat(strwrap(paste0("Not running equality test for ("
              ,paste0(test.eq, collapse=", "), ") because all ", sFiltSev
              ," rows were filtered out.\n"), width=72, initial="*** Note: "
              ,prefix="          "), sep="\n")
          return(invisible())
        } # if (sFiltSev %in% test.eq)
      } # !is.null(test.eq)
    } # xVars$info$sevs > iMaxSev
}
  ### initialize the hypothesis

  select.par.ze <- test.ze
  select.par.eq <- test.eq
  select.par.egroup <- test.egroup
  select.type <- type
  select.matrix <- testmatrix
  select.coefs <- coefs
  select.covar <- covar
  select.par <- c(select.par.ze, select.par.eq)

  ### if the hypothesis is unspecified, query the user to specify it

  if (is.null(select.matrix)) {
    if (is.null(select.par)) {

      choices <- names(select.coefs)

      select.par.ze <- query(
         wordlist=choices,
         desc="Parameters:",
         ask="Enter parameters to test for removal (none):")

      drop <- match(choices, select.par.ze)
      choices <- choices[is.na(drop)]

      select.par.eq <- NULL
      select.par.egroup <- NULL
      egroup <- 0
      ask1 <- "Enter group of parameters to test for equality (none):"
      askmore <- "Enter another group to test for equality (none):"
      done <- FALSE
      while(!done) {
        egroup <- egroup + 1
        tmp.eq <- query(
           wordlist=choices,
           desc="Parameters:",
           ask=ifelse(egroup==1, ask1, askmore))
        if (is.null(tmp.eq)) done <- TRUE
        if (length(tmp.eq) ==1)
          stop(message="You need at least 2 parameters to test equality.")
        if (!is.null(tmp.eq)) {
          select.par.eq <- c(select.par.eq, tmp.eq)
          select.par.egroup <- c(select.par.egroup, rep(egroup, length(tmp.eq)))
          drop <- match(choices, tmp.eq)
          choices <- choices[is.na(drop)]
        }
      }

      if (is.null(select.par.eq) & is.null(select.par.ze))
        stop(message="No parameters selected.")

      select.par <- c(select.par.ze, select.par.eq)
    }

    select.coefs <- select.coefs[select.par]
    select.covar <- select.covar[select.par, select.par]

    select.matrix <- NULL
    cdim <- zdim <- length(select.par.ze)
    if(zdim>0) select.matrix <- diag(1, zdim, zdim)
    if(length(select.par.eq)>1) {
      grps <- unique(select.par.egroup)
      for (g in grps) {
        edim <- length(select.par.egroup[select.par.egroup==g]) - 1
        if(edim>0) {
          select.matrix <-
            blockbind(select.matrix, rbind(diag(1, edim, edim), -1))
        } 
      } 
    }

    deg.fr <- ncol(select.matrix)
    dimnames(select.matrix) <-
      list(names(select.coefs),
         paste("[,", seq(deg.fr), "]", sep=""))
  }

  ### use waldtest() to get test statistics and p-value
  tmp <- waldtest(select.matrix, select.coefs, select.covar)

  results <- list(test.ze=select.par.ze,
                  test.eq=select.par.eq,
                  test.eq.gp=select.par.egroup,
                  type=select.type,
                  estimate=select.coefs,
                  covar=select.covar,
                  testmatrix=select.matrix,
                  chisq=tmp$waldtest,
                  df=deg.fr,
                  p.obs=tmp$p.obs)

  if (do.summary) {
    if (!parallel)
      testsummary(results, file, short, append)
    else 
      paralleltestsummary(results, file, short, append)   
    invisible() 
  } else
  results
}

### function for creating block diagonal matrix
blockbind <- function(amatrix=NULL, bmatrix=NULL) {
  a <- amatrix
  b <- bmatrix
  if(is.null(a)) b -> c else
    if(is.null(b)) a -> c else c <-
       rbind(cbind(a, matrix(0, nrow(a), ncol(b))),
             cbind(matrix(0, nrow(b), ncol(a)), b))
  c
}



# Function for performing a Wald test -
# "testmatrix" is a contrast matrix whose columns
# correspond to the contrasts to be tested.

waldtest <- function(testmatrix, coef = fits$coefficients, coefvar = fits$sand.var) {
  ### estimates
  est <- t(testmatrix) %*% coef
  ### its variance
  covar <- t(testmatrix) %*% coefvar %*% testmatrix
  ### Wald statistic
  wtst <- t(est) %*% solve(covar) %*% est
  ### Degrees of freedom
  df <- ncol(as.matrix(testmatrix))
  ### p-value
  pobs <- 1 - pchisq(wtst, df)
  list(waldtest = wtst, df=df, p.obs = pobs, estimate = est, covar = covar)
}


### summary function for partest
testsummary <- function (results, file=NULL, short=TRUE, append=FALSE) {

  ### Save output in a file?
  if (!is.null(file)) {
    cat ("\nWriting summary data to file:", file, "\n")
    sink(file=file, append=append)
  }

  ### Print out results
  cat ("\n\n")
  cat ("Hypothesis Test Results\n")
  cat ("-----------------------\n")

  ### Informational stuff...
  cat ("\nCoefficients tested for removal:  ")
  if(is.null(results$test.ze)) cat("None\n")
  else cat(results$test.ze, "\n")

  cat ("\nCoefficients tested for equality:  ")
  if (is.null(results$test.eq)) cat("None\n")
  else {
    cat("\n")
    egrps <- unique(results$test.eq.gp)
    for (g in egrps) cat ("\n   Group", g, ":  ", results$test.eq[results$test.eq.gp==g], "\n")
  }
  if (!short) {

    cat ("\nParameter estimates:\n\n")
    print(results$estimate)

    cat ("\nCovariance matrix:\n\n")
    print (results$covar)

    cat ("\nTest matrix:\n\n")
    print (results$testmatrix)

  }

  ### Coefficients & statistics...
  stats <- matrix(nrow=1,ncol=3)
  dimnames(stats) <- list("", c("  Chisquare", "    df", "   p-value"))
  stats[,1] <- results$chisq
  stats[,2] <- results$df
  stats[,3] <- round(results$p.obs,digits=5)
  stats[results$p.obs<.00001,3] <- .00001

  cat("\nTest statistics:\n\n")
  print (stats)

  ### Add explanatory comments here.
  pvalue =  round(results$p.obs,digits=5)
  
  cat ("\n##############################################################################", "\n")

  ## Need to specify each test
  if (is.null(results$test.ze)) { # no test for removal, must have a test for equality
    if (pvalue <= 0.05) {
      cat ("\nThe P Value of the equality test is <= 0.05. This is generally considered", "\n")
      cat ("significant, indicating that all the tested parameters should be retained", "\n")
      cat ("in the model.", "\n")
    } else {
      cat ("\nThe P Value of the equality test is > 0.05. This is generally considered", "\n")
      cat ("not significant, indicating that the tested parameters are equal to each", "\n")
      cat ("other. There is no need to keep all of them in the model.", "\n")
    }
    
  }
 
  else if (is.null(results$test.eq)) { # no test for equality, must have a test for removal
    if (pvalue <= 0.05) {
      cat ("\nThe P Value of the removal test is <= 0.05. This is generally considered", "\n")
      cat ("significant, indicating that the tested parameters should not be removed", "\n")
      cat ("from the model.", "\n")
    } else {
      cat ("\nThe P Value of the removal test is > 0.05. This is generally considered", "\n")
      cat ("not significant, indicating that the tested parameters could be removed", "\n")
      cat ("from the model.", "\n")
    }
  }
  else { # have both removal and equality test
      cat ("The  test is  for both parameter removal and equality, care is needed", "\n") 
      cat ("in interpreting the significance test.", "\n") 
      cat ("(See Section 5.1 of \"CatReg Softare Documentation\")", "\n")   
  }


  cat ("\n##############################################################################", "\n")


  cat("\n")

  # Output stuff to terminal again.
  if (!is.null(file)) sink()
  
  invisible()
}

paralleltestsummary <- function (results, file=NULL, short=TRUE, append=FALSE) {

  ### Save output in a file?
  if (!is.null(file)) {
    cat ("\nWriting summary data to file:", file, "\n")
    sink(file=file, append=append)
  }

  ### Print out results
  cat ("\n\n")
  cat ("Hypothesis Test Results\n")
  cat ("-----------------------\n")

  ### Informational stuff...
  cat ("\nCoefficients tested for removal:  ")
  if(is.null(results$test.ze)) cat("None\n")
  else cat(results$test.ze, "\n")

  cat ("\nCoefficients tested for equality:  ")
  if (is.null(results$test.eq)) cat("None\n")
  else {
    cat("\n")
    egrps <- unique(results$test.eq.gp)
    for (g in egrps) cat ("\n   Group", g, ":  ",
      results$test.eq[results$test.eq.gp==g], "\n")
  }
  if (!short) {

    cat ("\nParameter estimates:\n\n")
    print(results$estimate)

    cat ("\nCovariance matrix:\n\n")
    print (results$covar)

    cat ("\nTest matrix:\n\n")
    print (results$testmatrix)

  }

  ### Coefficients & statistics...

  stats <-  matrix(nrow=1,ncol=3)
  dimnames(stats) <- list("", c("  Chisquare", "    df", "   p-value"))
  stats[,1] <- results$chisq
  stats[,2] <- results$df
  stats[,3] <- round(results$p.obs,digits=5)
  stats[results$p.obs<.00001,3] <- .00001

  cat("\nTest statistics:\n\n")
  print (stats)

  ### Add explanatory comments here

  pvalue =  round(results$p.obs,digits=5)
  
  cat ("\n##############################################################################", "\n")

  ### handle for two different unrestricted models
  if (fits$model == "unrestricted cumulative model") {
    if (pvalue <= 0.05) {
      cat ("\nThe P Value of the equality test is <= 0.05. This is generally considered", "\n")
      cat ("significant, indicating that the unrestricted cumulative model is an", "\n") 
      cat ("appropriate model to use with this dataset. ", "\n\n")     
    } else {
      cat ("\nThe P Value of the equality test is > 0.05. This is generally considered", "\n")
      cat ("not significant, indicating that it would be more appropriate to use the", "\n") 
      cat ("simpler restricted cumulative model.", "\n\n")      
    }
  } else { # must be "unrestricted conditional model"
     if (pvalue <= 0.05) {
      cat ("\nThe P Value of the equality test is <= 0.05. This is generally considered", "\n")
      cat ("significant, indicating that the unrestricted conditional model is an ", "\n") 
      cat ("appropriate model to use with this dataset. ", "\n\n")
    } else {
      cat ("\nThe P Value of the equality test is > 0.05. This is generally considered", "\n")
      cat ("not significant, indicating that it would be more appropriate to use the", "\n") 
      cat ("simpler restricted conditional model.", "\n\n")      
    }
  }
  
  
# Modified by GyoungJin Ahn : 04/13/2006   
# in version 2.0, conditional odds model is not used.
#  cat ("Note: It is sometimes worthwhile to test parallelism for both the unrestricted", "\n")
#  cat ("cumulative model and the unrestricted conditional model, as one or the other", "\n")
#  cat ("form may be more appropriate for a given dataset. The model which produces", "\n") 
#  cat ("less deviance is more appropriate.", "\n")
  cat ("(See Section 5.2 of \"CatReg Software Documentation\") ", "\n")
  cat ("\n##############################################################################", "\n")

  cat("\n")

  ### Output stuff to terminal again.

  if (!is.null(file)) sink()

  invisible()
}

##############################################
# Testing for proportional odds assumption   #
##############################################

parallel.test <- function(fitobj=fits)
{
  if(fitobj$model != "unrestricted cumulative model" &&
     fitobj$model != "unrestricted conditional model")
    stop("should fit the unrestricted cumulative model first.\n")

  ### Input ordering is changed for partest()
  ### For parallel.test(), only coefficients for concentration and time are included as Ken Brown requested. 
  ### Before change, stratified intercept terms are also included in parallel.test().

  xnames <- NULL
  xnames.gp <- NULL
  
  grps <- unique(c(fitobj$xnames.gp[grep("conc",fitobj$xnames,ignore.case = TRUE)]
                  ,fitobj$xnames.gp[grep("time",fitobj$xnames,ignore.case = TRUE)] ))

  cat("grps:",grps,"\n")

  for ( g in grps ) {
    xnames <- cbind(xnames, fitobj$xnames[fitobj$xnames.gp==g] )
    xnames.gp <- cbind(xnames.gp, fitobj$xnames.gp[fitobj$xnames.gp==g] )
  }

  if(min(xnames.gp) > 1) xnames.gp=xnames.gp-min(xnames.gp)+1
  else xnames.gp=xnames.gp
    
  ### perform parallel test using partest()
  partest(test.ze=NULL, test.eq=xnames, test.egroup=xnames.gp, type=NULL,
          testmatrix=NULL, coefs=fitobj$coefficients, covar=fitobj$sand.var,
          popup=FALSE, do.summary=TRUE, file=NULL, short=TRUE, append=FALSE, parallel=TRUE)
         
  invisible()
}


### deviance residuals ####################

diagnose <- function(data.obj=data, fit.obj=fits) {

  ### extract data, link information and fitted model

  X <- data.obj$x[,-1]
  ylo <- data.obj$y$ylo
  yhi <- data.obj$y$yhi
  weight <- data.obj$weight
  link <- fit.obj$link
  model <- fit.obj$model
  linkscale <- fit.obj$scale
  coefs <- fit.obj$coefficients

  ### compute components of generalized deviance

  nsevcat <- max(ylo, yhi) - min(ylo, yhi) + 1
  devs <- pr.exceed(ylo, nsevcat, X, coefs, link, model, derivs = 0, linkscale) - 
          pr.exceed(yhi + 1, nsevcat, X, coefs, link, model, derivs = 0, linkscale)
  devs <- weight * log(devs) * (-2)

  list(resid.dev=devs)
}

### Function to compute analysis of deviance table.
### Option to partition residual deviance into
### "lack-of-fit" and "pure error": set "lof=TRUE"

deviance.fit <- function(lof=FALSE, chdata.obj=data.char, data.obj=data, ecline=ECline,
                         fit.obj=fits, strata=names(ecline$tables), xlog=ECline$xlog, 
                         fits0=NULL, fits1=NULL, var.name=required,sink.value=FALSE) {

  ### Compute null deviance and generalized R-squared

  vnms <- var.name
  vnms["conc"] <- "omit"
  vnms["time"] <- "omit"
  
  nchdata.obj <- chdata.obj
  nchdata.obj$data <- cbind(nchdata.obj$data, "omit"="1")
  nchdata.obj$strata <- list(intercept=NULL, conc=NULL, time=NULL)
  
  ndata.obj <- makestrata(nchdata.obj, vnms)
  ndata.obj <- c(ndata.obj, list(worstcase=data.obj$worstcase))
  nfit.obj <- fits0

    ### fit a null model
  if(is.null(nfit.obj)){
      nfit.obj <- go(ndata.obj,getvar=FALSE, verbose=FALSE, model=fit.obj$model, total_entry=total_entry)
  }
 
  ### null deviance
  deviance.null <- nfit.obj$deviance
  ### residual deviance
  deviance.residual <- fit.obj$deviance
  ### difference between residual deviance and null deviance
  deviance.model <- deviance.null - deviance.residual

  ### get the difference of degrees of freedom
  df.null <- nfit.obj$df.residual
  df.residual <- fit.obj$df.residual
  df.model <- df.null - df.residual

  ### get p-value
  Gen.R2 <- deviance.model/deviance.null
  Gen.F <- (deviance.model/deviance.residual)*(df.residual/df.model)
  Gen.F.pvalue <- 1 - pf(Gen.F, df.model, df.residual)

  ### Compute deviance for unconstrained model if lof=TRUE
  ### (to partition residual deviance into lack-of-fit and "error")
  if (lof) {
    all.nms <- dimnames(data.obj$x)[[2]]
    istrata <- NULL

    xc.nm <- all.nms[grep("CONC", all.nms)]
    xt.nm <- all.nms[grep("TIME", all.nms)]

	#Change to fix $ operator is invalid for atomic vectors - by GLN 01/06/2011
    #if (length(xc.nm)>0) istrata <- c(istrata, var.name["conc"])
    #if (length(xt.nm)>0) istrata <- c(istrata, var.name["time"])
    #nchdata.obj$strata <- list(intercept=istrata, conc=NULL, time=NULL)
	if (length(xc.nm)>0) istrata <- c(istrata, var.name["conc"])
    if (length(xt.nm)>0) istrata <- c(istrata, var.name["time"]) 
	nchdata.obj$intestrata <- istrata
	nchdata.obj$concstrata <- NULL
	nchdata.obj$timestrata <- NULL
	

    pdata.obj <- makestrata(nchdata.obj, vnms)
    pdata.obj <- c(pdata.obj, list(worstcase=data.obj$worstcase))
    pecfits <- fits1

    if(is.null(pecfits))
      pecfits <- go(pdata.obj, getvar=FALSE, verbose=FALSE, model=fit.obj$model, total_entry=total_entry)

    df.error <- pecfits$df.residual
    df.lof <- df.residual - df.error
    deviance.error <- pecfits$deviance
    deviance.lof <- deviance.residual - deviance.error
    Gen.F.model <- (deviance.model/deviance.error)*(df.error/df.model)
    Gen.F.lof <- (deviance.lof/deviance.error)*(df.error/df.lof)
    Gen.model.pvalue <- 1 - pf(Gen.F.model, df.model, df.error)
    Gen.lof.pvalue <- 1 - pf(Gen.F.lof, df.lof, df.error)
  }

  ### Analysis of deviance table
  if (lof) {
    ### table with lof=TRUE
    results <- data.frame(
      DF=c(df.model, df.lof, df.error, df.null),
      Deviance=c(deviance.model, deviance.lof, deviance.error, deviance.null),
      Mean.Dev=c(as.character(round(deviance.model/df.model,3)),
                 as.character(round(deviance.lof/df.lof,3)),
                 as.character(round(deviance.error/df.error,3)), ""),
      Gen.F=c(as.character(round(Gen.F.model,3)),
              as.character(round(Gen.F.lof,3)), "", ""),
      pvalue=c(as.character(round(Gen.model.pvalue,4)),
               as.character(round(Gen.lof.pvalue,4)), "", ""))
    dimnames(results)[[1]] <- c("Model", "Lack of fit", "Error", "Total")
  } else {
    ### table with lof=FALSE
    results <- data.frame(
      DF=c(df.model, df.residual, df.null),
      Deviance=c(deviance.model, deviance.residual, deviance.null),
      Mean.Dev=c(as.character(round(deviance.model/df.model,3)),
                 as.character(round(deviance.residual/df.residual,3)), ""),
      Gen.F=c(as.character(round(Gen.F,3)), "", ""),
      pvalue=c(as.character(round(Gen.F.pvalue,4)), "", ""))
    dimnames(results)[[1]] <- c("Model", "Residual", "Total")
  }

#######  Added by GyoungJin Ahn : 04/21/2006  
#######  For terminal printout
  cat("Analysis of Deviance Statistics:\n")
    if (exists("fitWarn")){
	 cat(paste("\n*****************************   Warning   *****************************",fitWarn,"\n************************************************************************\n"))
	 cat(fitWarn)
  }
  cat("\nGeneralized R-squared:", round(Gen.R2,3), "\n\n")

  print(results)

  ### R-square and p-value
  Rsq =  round(Gen.R2,3)*100
  pvalue= round(Gen.F.pvalue,4)


  ### add the explanatory comments about deviance.fit()and deviance.fit(lof=TRUE)

  cat ("\n#########################################################################", "\n")
  if (lof){   
    cat ("Note: About ", Rsq, "% of the variation in the response is accounted", "\n")
    cat ("for by the explanatory variables in the current model fit.", "\n\n")
    lof.pvalue = round(Gen.lof.pvalue,4)


    if ( pvalue <= 0.05 && lof.pvalue > 0.05) {
	cat ("The p-value of the model fit is <= 0.05. This is generally considered", "\n")
        cat ("significant. The p-value for \"Lack of fit\" is > 0.05, which is generally ", "\n")       
	cat ("considered not significant. So, the current model fit is acceptable.", "\n")
	cat ("(See section 5.3 of \"CatReg Software Documentation\")", "\n")	
    } else {
	cat ("The p-value of the model fit is > 0.05. This is generally considered", "\n")
        cat ("not significant. The p-value for \"Lack of fit\" is < 0.05, which is ", "\n")       
	cat ("generally considered significant. So, the current model fit is not .", "\n")				
	cat ("acceptable. You have to fit a model with more explanatory variables," , "\n") 
	cat ("i.e. filtering fewer variables.", "\n")
	cat ("(See section 5.3 of \"CatReg Software Documentation\")", "\n")
    }
  } else {	
    cat ("Note: About ", Rsq, "% of the variation in the response is accounted", "\n")
    cat ("for by the explanatory variables in the current model fit.", "\n\n")

    if ( pvalue <= 0.05) {
	cat ("The p-value of the model fit is <= 0.05. This is generally considered ", "\n")
	cat ("significant, indicating that the current model fit is acceptable.", "\n")
    } else {
	cat ("The p-value of the model fit is > 0.05. This is generally considered ", "\n")
	cat ("not significant, indicating that the current model fit is not acceptable.", "\n")	
	cat ("You have to fit a model with more explanatory variables," , "\n") 
	cat ("i.e. filtering fewer variables.", "\n\n")
#       cat ("Unless there is substantial replication in the data, it is usually better ", "\n")
#       cat ("to use the default lof=FALSE option.", "\n") 
        cat ("(See section 5.3 of \"CatReg Software Documentation\")", "\n")
#       cat ("To see the effects of Lack of fit, try \"deviance.fit(lof=TRUE)\"", "\n")
    } 
  } 
  cat ("#########################################################################", "\n\n")
  
  
  
  
  
  ### Add Deviance Analysis to the output file
  if(sink.value){
  sink(file=outfile, append=TRUE)

  cat("Analysis of Deviance Statistics:\n")
  cat("\nGeneralized R-squared:", round(Gen.R2,3), "\n\n")

  print(results)

  ### R-square and p-value
  Rsq =  round(Gen.R2,3)*100
  pvalue= round(Gen.F.pvalue,4)


  ### add the explanatory comments about deviance.fit()and deviance.fit(lof=TRUE)
  if (exists("fitWarn")){
	 cat(paste("\n*****************************   Warning   *****************************\n",fitWarn,"\n\n************************************************************************\n"))
  }
  cat ("\n#########################################################################", "\n")
  if (lof){   
    cat ("Note: About ", Rsq, "% of the variation in the response is accounted", "\n")
    cat ("for by the explanatory variables in the current model fit.", "\n\n")
    lof.pvalue = round(Gen.lof.pvalue,4)


    if ( pvalue <= 0.05 && lof.pvalue > 0.05) {
	cat ("The p-value of the model fit is <= 0.05. This is generally considered", "\n")
        cat ("significant. The p-value for \"Lack of fit\" is > 0.05, which is generally ", "\n")       
	cat ("considered not significant. So, the current model fit is acceptable.", "\n")
    } else {
	cat ("The p-value of the model fit is > 0.05. This is generally considered", "\n")
        cat ("not significant. The p-value for \"Lack of fit\" is < 0.05, which is ", "\n")       
	cat ("generally considered significant. So, the current model fit is not .", "\n")				
	cat ("acceptable. You have to fit a model with more explanatory variables," , "\n") 
	cat ("i.e. filtering fewer variables.", "\n")
	     }
  } else {	
    cat ("Note: About ", Rsq, "% of the variation in the response is accounted", "\n")
    cat ("for by the explanatory variables in the current model fit.", "\n\n")

    if ( pvalue <= 0.05) {
	cat ("The p-value of the model fit is <= 0.05. This is generally considered ", "\n")
	cat ("significant, indicating that the current model fit is acceptable.", "\n")
    } else {
	cat ("The p-value of the model fit is > 0.05. This is generally considered ", "\n")
	cat ("not significant, indicating that the current model fit is not acceptable.", "\n")	
	cat ("You have to fit a model with more explanatory variables," , "\n") 
	cat ("i.e. filtering fewer variables.", "\n\n")
#       cat ("Unless there is substantial replication in the data, it is usually better ", "\n")
#       cat ("to use the default lof=FALSE option.", "\n") 
#       cat ("(See section 5.3 of \"CatReg Software Documentation\")", "\n")
#       cat ("To see the effects of Lack of fit, try \"deviance.fit(lof=TRUE)\"", "\n")
    } 
  } 
  cat ("#########################################################################", "\n")

  sink()
  
  #output to csv
  if (sink.value && outputCSV){
    sink(file = gsub(".otx","_summary.csv",outfile), append=TRUE)
	cat("Analysis of Deviance Statistics:\n")
    cat("\n,Generalized R-squared:,", round(Gen.R2,3), "\n\n")
	
	cat(",,",paste(colnames(results),collapse=","),"\n")
     for (i in  c(1:length(rownames(results)))){
      cat(paste(",",rownames(results)[i],",", paste(results[i,],collapse=","),"\n",sep=""))
     }
    cat("\n");
	
	  ### add the explanatory comments about deviance.fit()and deviance.fit(lof=TRUE)
  if (exists("fitWarn")){
	 cat(paste("\n*****************************   Warning   *****************************\n",fitWarn,"\n\n************************************************************************\n"))
  }
  cat ("\n#########################################################################", "\n")
  if (lof){   
    cat ("Note: About ", Rsq, "% of the variation in the response is accounted for by the explanatory variables in the current model fit.", "\n")
    lof.pvalue = round(Gen.lof.pvalue,4)


    if ( pvalue <= 0.05 && lof.pvalue > 0.05) {
	cat ("The p-value of the model fit is <= 0.05. This is generally considered significant. The p-value for \"Lack of fit\" is > 0.05, which is generally considered not significant. So, the current model fit is acceptable.", "\n")
    } else {
	cat ("The p-value of the model fit is > 0.05. This is generally considered not significant. The p-value for \"Lack of fit\" is < 0.05, which is generally considered significant. So, the current model fit is not acceptable. You have to fit a model with more explanatory variables, i.e. filtering fewer variables.", "\n")
	     }
  } else {	
    cat ("Note: About ", Rsq, "% of the variation in the response is accounted for by the explanatory variables in the current model fit.", "\n")

    if ( pvalue <= 0.05) {
	cat ("The p-value of the model fit is <= 0.05. This is generally considered significant, indicating that the current model fit is acceptable.", "\n")
    } else {
	cat ("The p-value of the model fit is > 0.05. This is generally considered not significant, indicating that the current model fit is not acceptable.  You have to fit a model with more explanatory variables, i.e. filtering fewer variables.", "\n")
    } 
  } 
  cat ("#########################################################################", "\n")
	
	sink()
  }

}
  
    
  invisible()
}


getData <- function(var.name=required){
  
  infile = xVars$info$InputFile
  
  ### check record lengths

  reclength <- count.fields(infile, sep = ",")
  fncol <- reclength[1]
  recflag <- seq(length(reclength))[reclength != fncol]
  if(length(recflag) > 0) stop(message = paste(c(
    ">>>> Records", paste(as.character(recflag), collapse = ", "),
    "have missing or extra fields!"), collapse = " "))

  ### read the header and data into a character matrix

  a <- matrix(scan(infile, what = character(), sep = ","), ncol = fncol, byrow = TRUE)
  
  ## remove empty rows
  a <- a[!apply(a == "", 1, all),]

  ### extract column names from first row

  dimnames(a) <- list(NULL, c(a[1,  ]))
  a <- a[-1,]


  ### check for required fields

  implied <- var.name[is.na(match(var.name, dimnames(a)[[2]]))]

  missreq <- match(var.name["loscore"],implied)

  is.conc <- is.na(match(var.name["conc"], implied))
  is.time <- is.na(match(var.name["time"], implied))
  is.loscore <- is.na(match(var.name["loscore"], implied))
  is.hiscore <- is.na(match(var.name["hiscore"], implied))

  is.nsub <- is.na(match(var.name["nsub"], implied))

  is.incid <- is.na(match(var.name["incid"], implied))
  is.gpsize <- is.na(match("GpSize", colnames(a)))


  ### stop if no severity score
  if (!is.loscore)
    stop(message=paste(c("Required input variable SevLo was not found.",
         implied[missreq[!is.na(missreq)]]), collapse=" "))
  if (!is.conc)
    stop(message=paste(c("Required input variable mg/m3 was not found.",
         implied[missreq[!is.na(missreq)]]), collapse=" "))
  if (!is.incid)
    stop(message=paste(c("Required input variable Incid was not found.",
         implied[missreq[!is.na(missreq)]]), collapse=" "))
  if (!is.nsub)
    stop(message=paste(c("Required input variable Nsub was not found.",
         implied[missreq[!is.na(missreq)]]), collapse=" "))

  ### generate a matrix "a" which will be "char.data" later
  if (!is.time) {
    a <- cbind(a, "1")
    dimnames(a)[[2]][dim(a)[2]] <- var.name["time"]
  }
  if (!is.hiscore) {
    a <- cbind(a, a[ ,var.name["loscore"]])
    dimnames(a)[[2]][dim(a)[2]] <- var.name["hiscore"]
  }

    #ww <- a[,dimnames(a)[[2]]=='Incid']; by GLN, to fix column assignments of required data col
	ww <- a[,dimnames(a)[[2]]==var.name["incid"]];
    a <- cbind(a, ww)

    dimnames(a)[[2]][dim(a)[2]] <- var.name["incid"]

  if (is.gpsize) {
    a <- cbind(a, "1")
    dimnames(a)[[2]][dim(a)[2]] <- "GpSize"
  }

  ### remove implied variables from selection list

  namelist <- dimnames(a)[[2]]
  for (val in implied) namelist <- namelist[namelist != val]

  ### data filtering option
	exclude <- NULL
	if(exists("xVars")) {
		if(xVars$info$filterCnt > 0)
		{
			exclude <- list(names = xVars$Filter$selected)
			for(i in seq(length(xVars$Filter$selected))) {
				omitVector <- as.vector(c(xVars$Filter[xVars$Filter$selected[i]], recursive=TRUE))
				for(n in seq(length(omitVector))) {
					a <- a[a[,xVars$Filter$selected[i]] != omitVector[n],]
				}
				exclude <- c(exclude, list(omitVector))
			}
		}
	}

  ### Define clusters
  #cluster.on <- query(wordlist=namelist, desc="Variables:", 
  #                    ask="Define clusters using which variable(s)? (none):")
  cluster.on = NULL
	if(exists("xVars")) {
		if(xVars$info$clusterCnt > 0)
			cluster.on = xVars$Cluster
	}
  ### names for primary variables
  cnm <- var.name["conc"]
  tnm <- var.name["time"]

	### Process Strata
	istrata = NULL
	cstrata = NULL
	tstrata = NULL
	if(exists("xVars")) {
		if(xVars$info$interceptCnt > 0)
			istrata = xVars$Strata$intercept
		
		if (is.conc){	
		if(xVars$info$doseCnt > 0)
			cstrata = xVars$Strata$dose
		}
		
		if (is.time){	
		if(xVars$info$timeCnt > 0)
			tstrata = xVars$Strata$time
		}
	}


  ### select log or linear scale for quantitative variables

  #lgcnm <- paste("log10(", cnm, ")", sep="")
  #lgtnm <- paste("log10(", tnm, ")", sep="")

  xclog <- xtlog <- ""

  if (is.na(match(var.name["conc"], implied)))
	xclog = xVars$info$Dose
	
    #xclog <- c("", "C")[menu(c(cnm, lgcnm), graphics = FALSE,
    #         title = paste("\nLog or linear scale for ", cnm, "? \nChoices:", sep=""))]

  if (is.na(match(var.name["time"], implied)))
	xtlog = xVars$info$Time
	
    #xtlog <- c("", "T")[menu(c(tnm, lgtnm), graphics = FALSE,
    #         title = paste("\nLog or linear scale for ", tnm, "? \nChoices:", sep=""))]

  xlog <- paste(xclog, xtlog, sep="")


  ### check for missing response and zero exposure
	data <- cleandata(a, xlog = xlog)

        
  ### select link

  #links <- c("logit", "probit", "cloglog")
  #cat("\n")
  #plink <- links[menu(links, graphics = FALSE, title ="Link function?")]
  #plink <- ifelse(length(plink) > 0, plink, "logit")
  
  plink = xVars$info$LinkFunction

  ### function value

  list(data = data$data, excluded = exclude, cluster.var=cluster.on, 
       strata = list(intercept = istrata, conc=cstrata, time =tstrata), 
       xlog = xlog, link = plink, gpcorr=0, org_gpcorr=0, 
       missing.row = data$missing.row, zero.row=data$zero.row, infile = infile)

}