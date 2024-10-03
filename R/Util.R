##############################################################
# util.R 11/11/97
# R version. 02/12/02
# last updated: 12/18/02, by CLiu

#
# Functions for converting severity codes to severity intervals.
# Also combines individual and group response fields, adding
# fields for group size (individual = gp of size 1) and
# incidence.
####################### response ############################
##############################################################
# Expand records containing individual incidences into       #
# multiple records so that each record contains only one     #
# response category. Appends fields for severity code,       #
# incidence (=1 for groups), and group size (=1 for          #
# individuals).                                              #
# The user can specify an arbitrary number of conditional    #
# code assignements to modify codes based on other variables #
##############################################################
#################### rename() ################################
#  Modified by GyoungJin Ahn : 1/20/2006
##############################################################

response <-
function(achar,
         gpsev="GrpSev",
         gpsize="Nsub",
         indsev=c("N","U","A","F"),
         thresh="Thresh",
         if.code=c("A", "A", "A", "a", "a", "a"),
         if.var=c("Thresh", "Thresh", "Censored",
                    "Thresh", "Thresh", "Censored"),
         if.value=c("GrNT", "InNT", "y", "GrNT", "InNT", "y"),
         then.code=rep("U", 6),
         var.name=required)
{
  ncats <- length(indsev)
  anames <- dimnames(achar)[[2]]
  anew <- NULL
  for (irow in seq(dim(achar)[1])) {
    arow <- achar[irow,]
    if (arow[thresh] == "GrTh" | arow[thresh] == "GrNT")
      anew <- rbind(anew, c(arow, arow[gpsev], arow[gpsize], 1))
    if (arow[thresh] == "InTh" | arow[thresh] == "InNT") {
      incidence <- arow[indsev]
      arow <- cbind(
                matrix(rep(arow, ncats), nrow=ncats, byrow=TRUE),
                indsev,
                rep(1,ncats),
                incidence)
      anew <- rbind(anew, arow)
     }
   }
   dimnames(anew) <-
     list(rep("",dim(anew)[1]),
          c(anames, "Sev",
            var.name["gpsize"], var.name["weight"]))
cat(is.incid,"\n");
   anew <- anew[!is.na(as.numeric(anew[,var.name["weight"]])),]
   if (length(if.code) > 0) {
     sev <- anew[,"Sev"]
     for (i in seq(length(if.code)))
       sev[(sev == if.code[i]) &
           (anew[, if.var[i]] == if.value[i])] <- then.code[i]
     anew[, "Sev"] <- sev
   } 
   anew
}
                 
############################ sevcode #########################
##############################################################
# Convert severity codes to ylo/yhi severity intervals       #
# Specify codes in any order - corresponding score intervals #
# must be in the same order.                                 #
# Variables:                                                 #
#      data = character matrix                               #
#      sev = name of data column containing severity codes   #
#      code = vector of severity codes                       #
#      score = list of (loscore, hiscore) pairs to assign    #
#         to the severity codes                              #
##############################################################
sevcode <-
function(data, sev="Sev", 
         code=c("N","U","A","F","n","u","a","f"),
         score=list(N=c(0,0), U=c(0,1), A=c(1,1), F=c(2,2),
                    n=c(0,0), u=c(0,1), a=c(1,1), f=c(2,2))
) {
  sevcat <- data[,sev]
#
# -9 indicates a missing response
#
  ylo <- rep(-9, dim(data)[1])
  yhi <- rep(-9, dim(data)[1])
#
  for (i in seq(length(code))) {
    ylo[sevcat==code[i]] <- score[[i]][1]
    yhi[sevcat==code[i]] <- score[[i]][2]
  }
#
  list(ylo=ylo, yhi=yhi)
}
                 
######################## cross ###############################
##############################################################
# Cross character valued factors:                            #
# if cfactors is a vector it is returned as the value;       #
# if cfactor is a matrix its columns are crossed to          #
# create a new factor with as many categories as the         #
# number of combinations of factor categories across factors.#
# -DGS 3-31-95                                               #
##############################################################
cross <-
function(cfactors,sep=":") {
  ff <- as.matrix(cfactors)
  newfactor <- c(ff[,1])
  for(i in seq(length(newfactor)))
    newfactor[i] <- paste(ff[i,],collapse=sep)
  newfactor
}
######################### ascii ##############################
##############################################################
# Write a data matrix to an ascii file with user supplied    #
# separator. Writes one row per record. Default is comma     #
# separated. Calls the cross function given above.           #
#                             DGS  6-27-95                   #
##############################################################
ascii <- function(a, outfile="data.asc", sep=",") {
  if(is.matrix(a)) {
     asc <- matrix(as.character(a), ncol=ncol(a))
     asc <- cross(rbind(dimnames(a)[[2]], asc),sep=sep)
  } else asc <- as.character(a)
  write(asc, outfile, ncolumns=1)
}
######################## convert##############################
##############################################################
# Convert ascii input file from NUAF severity coding to      #
# "ylo/yhi" interval scoring.     DGS 6-27-95                #
##############################################################
convert <-
function(infile, outfile="conv.asc",
         sep=",",
         gpsev="GrpSev", gpsize="Nsub",
         indsev=c("N","U","A","F"),
         thresh="Thresh",
         if.code=c("A", "A", "A", "a", "a", "a"),
         if.var=c("Thresh", "Thresh", "Censored",
                    "Thresh", "Thresh", "Censored"),
         if.value=c("GrNT", "InNT", "y", "GrNT", "InNT", "y"),
         then.code=rep("U", 6),
         sev="Sev", 
         code=c("N","U","A","F","n","u","a","f"),
         score=list(N=c(0,0), U=c(0,1), A=c(1,1), F=c(2,2),
                    n=c(0,0), u=c(0,1), a=c(1,1), f=c(2,2)),
         var.name=required) {
#
# Find out how many fields per record and check for equal length
#
  reclength <- count.fields(infile, sep = sep)
  fncol <- reclength[1]
  recflag <- seq(length(reclength))[reclength!=fncol]
  if (length(recflag) > 0)
    stop(message=
    paste(c(">>>> Records", paste(as.character(recflag), collapse=", "),
          "have missing or extra fields!"), collapse=" "))
#
# First row assumed to be header of names...
#
  a <- matrix(scan(infile, what = character(), sep = sep),
              ncol = fncol, byrow = TRUE)
  dimnames(a) <- list(NULL, c(a[1,  ]))
  a <- a[-1,]
# remove redundant records
  a <- a[a[,gpsev] != "",]
#
# Expand records containing individual incidences,
# combine group and individual level responses to
# (Severity score, Group size, Incidence) fields.
#
  a <- response(a, gpsev, gpsize, indsev, thresh,
         if.code, if.var, if.value, then.code,
         var.name)

#
# Convert three-category severity codes to intervals...
#  
  y <- sevcode(a, sev, code, score)
  a <- cbind(a, as.character(y$ylo), as.character(y$yhi))
  dimnames(a)[[2]][dim(a)[2]-2+seq(2)] <-
    c(var.name["loscore"], var.name["hiscore"])
#
# Output to ascii file...
#
  ascii(a, outfile=outfile, sep=sep)
  cat("\n>>>> Output file is", outfile, "\n\n")
}
  
############################################################
# Function for pooling severity categories.                #
############################################################
join <-
function(infile, outfile="joined.csv", var.name=required, sep=",") {

#
# Avoid overwriting the input file..
#
  if (outfile == infile)
    stop(message=">>>> Output filename same as input filename!")

#
# Find out how many fields per record, and check that all records
# have the same length.
#
  
  reclength <- count.fields(infile, sep = sep)
  fncol <- reclength[1]
  recflag <- seq(length(reclength))[reclength!=fncol]
  if (length(recflag) > 0)
    stop(message=
    paste(c(">>>> Records", paste(as.character(recflag), collapse=", "),
          "have missing or extra fields!"), collapse=" "))
#
# First row assumed to be header of names...
#
  a <- matrix(scan(infile, what = character(), sep = sep),
              ncol = fncol, byrow = TRUE)
  dimnames(a) <- list(NULL, c(a[1,  ]))
  a <- a[-1,]
#
# check for required fields
#
  implied <- var.name[is.na(match(var.name, dimnames(a)[[2]]))]
  is.loscore <- is.na(match(var.name["loscore"], implied))
  is.hiscore <- is.na(match(var.name["hiscore"], implied))
#
# Extract severity categories
#
  allscores <- sevlo <- sevhi <- NULL
  if (is.loscore) {
     sevlo <- as.numeric(a[,var.name["loscore"]])
     allscores <- c(allscores, sevlo)
  }
  if (is.hiscore) {
     sevhi <- as.numeric(a[,var.name["hiscore"]])
     allscores <- c(allscores, sevhi)
  }
  if (!is.null(allscores)) {
#    maxscore <- max(allscores)
#    obscat <- sort(as.numeric(unique(sevhi[sevlo==sevhi])))
    obscat <- sort(as.numeric(unique(allscores)))
  }
#
# Display and ask which categories to combine
#
  cat(paste(c("\n\nCurrent observed categories are ",
      paste(obscat, collapse=", "))))
  cat("\nEnter lower and upper scores for combined category")
  cat("\nLower:")
  lower <- as.numeric(readline())
  cat("\nUpper:")
  upper <- as.numeric(readline())
#
# Join categories
#
  if(lower < upper) {
    shift <- upper - lower
    if (is.loscore) {
      sevlo[sevlo >= lower & sevlo <= upper] <- lower
      sevlo[sevlo > upper] <- sevlo[sevlo > upper] - shift
      a[,var.name["loscore"]] <- as.character(sevlo)
    }
    if (is.hiscore) {
      sevhi[sevhi >= lower & sevhi <= upper] <- lower
      sevhi[sevhi > upper] <- sevhi[sevhi > upper] - shift
      a[,var.name["hiscore"]] <- as.character(sevhi)
    }
  } else stop(message=">>>> No change.")
#
# Output to ascii file...
#
  ascii(a, outfile=outfile, sep=sep)
  cat("\n>>>> Output file is", outfile, "\n\n")
  invisible()
}

#######################################
# Function for viewing and extracting #
# subsets of the data                 #
#######################################

view <-
function(infile, row=NULL, col=NULL, sep=",", all=FALSE)
{
# check record lengths
   
  reclength <- count.fields(infile, sep = sep)
  fncol <- reclength[1]
  recflag <- seq(length(reclength))[reclength != fncol]
  if(length(recflag) > 0) stop(message = paste(c(
    ">>>> Records", paste(as.character(recflag
    ), collapse = ", "), 
    "have missing or extra fields"), collapse
     = " "))

# read the header and data into a character matrix

  a <- matrix(scan(infile, what = character(), sep = sep), 
    ncol = fncol, byrow = TRUE)	

# extract column names from first row

  dimnames(a) <- list(NULL, c(a[1,  ]))
  a <- a[-1,  ]

# variable selection option

  if(all) {
    varlist <- seq(dim(a)[2])
    reclist <- seq(dim(a)[1])
  } else {
    varlist <- col
    reclist <- row
  }
  if(is.null(reclist)) reclist <- seq(dim(a)[1])
  if(is.null(varlist)) {
    varlist <- dimnames(a)[[2]]
    select.on <- query(
       wordlist=varlist,
       desc="Variables:",
       ask="View which variables? (all):",
       addlist=c("all","none"))
#    if(length(select.on)== 0 | select.on[1]=="all" ) { # SPlus
    if(length(select.on)== 0 || select.on[1]=="all" ) { 
      select.on <- varlist
    } else varlist <- select.on
    if(select.on[1]=="none") varlist <- NULL
   }	

# selected data

  a[reclist,varlist]
}

##############################################################
# Convert user specified code for missing values into blanks #
##############################################################                    
missval <-
function(infile=NULL, outfile=NULL, value=NULL, change="", sep=",") {
  cat("\n\n**** Recode missing values ****\n")
  if(is.null(infile)) {
    cat("\n\nEnter the name of the input data file: ")
    infile <- readline()
  }
  if(is.null(outfile)) {
    cat("\n\nEnter the name of the output data file: ")
    outfile <- readline()
  }
  if (infile == outfile) {
    cat("\n\nOverwrite ", infile, "? (n):")
    ans <- readline()
    if (ans != "y") stop()
  }
  if(is.null(value)) {
    cat("\n\nEnter the current code for missing data: ")
    value <- readline()
  }
  dat <- view(infile, all=TRUE, sep=sep)
  dims <- dim(dat)
  dimnms <- dimnames(dat)
  dat <- c(dat)
  dat[dat==value] <- change
  dat <- matrix(dat, byrow=FALSE, nrow=dims[1], ncol=dims[2])
  dimnames(dat) <- dimnms 
  ascii(dat, outfile)
  invisible()
}

#########################################################
# recode a variable and write to same or new ascii file #
#########################################################

recode <-
function(infile=NULL, outfile=NULL)
{
  cat("\n\n**** Recode specified variables ****\n")  
  if(is.null(infile)) {
    cat("\n\nEnter the name of the input data file: ")
    infile <- readline()
  }
  if(is.null(outfile)) {
    cat("\n\nEnter the name of the output data file: ")
    outfile <- readline()
  }
  if (infile == outfile) {
    cat("\n\nOverwrite ", infile, "? (n):")
    ans <- readline()
    if (ans != "y") stop()
  }

# check record lengths

  reclength <- count.fields(infile, sep = ",")
  fncol <- reclength[1]
  recflag <- seq(length(reclength))[reclength != fncol]
  if(length(recflag) > 0) stop(message = paste(c(
    ">>>> Records", paste(as.character(recflag
    ), collapse = ", "), 
    "have missing or extra fields!"), collapse
     = " "))	

# read the header and data into a character matrix

  a <- matrix(scan(infile, what = character(), sep = ","), 
    ncol = fncol, byrow = TRUE)	

# extract column names from first row

  dimnames(a) <- list(NULL, c(a[1,  ]))
  a <- a[-1,  ]

# select fields and recode them..

  field <- query(
       wordlist=dimnames(a)[[2]],
       desc="Variables:",
       ask="Recode which variables? (none):")
  if(length(field) > 0) {
    for(i in seq(length(field))) {
      oldval <- sort(unique(a[, field[i]]))
      newval <-
        nquery(
          wordlist=oldval,
          desc=paste("Values of ", field[i], ":",
                     sep=""),
          )
        if(length(newval) == length(oldval)) {
           tmp <- a[,field[i]]
           for (j in seq(length(oldval)))
              tmp[a[,field[i]]==oldval[j]] <- newval[j]
           cat("\nName of new variable? (same):")
           newfield <- readline()

# SPLus codes	   if (newfield)=="") a[,field[i]] <- tmp
# R code
           if (substr(newfield, 1, 1)=="") a[,field[i]] <- tmp 
           else {
             a <- cbind(a, tmp)
             dimnames(a)[[2]][dim(a)[2]] <- newfield
           }
         } else
 stop(message="Number of new values must equal number of old values.")
      }
    }
  ascii(a, outfile)
  invisible()
}

nquery <- function(wordlist,
                  desc="Values:",
                  ask="Enter replacement values:",
                  default=NULL, addlist=NULL, n=length(wordlist))
{
    cat("\n\n", desc, "\n", wordlist, "\n")
    cat("\n\n", ask, "\n")
    scan(what = "", n=n)
}

######################################
# Change names of required variables #
######################################

# Begin Changed by C. Ahn on Apr 26

rename <-
function(var.name=NULL) {
  if (is.null(var.name)) {
# Begin. Ken Brown. 6/19/06 Remove "GpSize" and "Weight" from required variables
 #  vnms <- c(conc="mg/m3", time="Hours", loscore="SevLo",
 #     hiscore="SevHi", gpsize="GpSize", incid="Incid", weight="Weight", nsub="Nsub")
 vnms <- c(conc="mg/m3", time="Hours", loscore="SevLo",
      hiscore="SevHi", incid="Incid", nsub="Nsub")
# End. Ken Brown. 6/19/06

##### Added by GyoungJin Ahn : 1/20/2006 
   required  <- vnms
##### End Addition by GyoungJin Ahn : 1/20/2006 
   
       
# End Chage by C. Ahn on Apr 26

} else {
    cat("\n\n**** Change required variable names ****\n")  
    vnms <- var.name
    newnm<- NULL
    oldnm <- query(
         wordlist=vnms,
         desc="Current required variables:",
         ask="Rename which variables? (none):")

    if (length(oldnm) >0)
      newnm <- nquery(oldnm,
                    desc="Current names:",
                    ask="Enter new names:")

    if (!is.null(newnm))
      for (j in seq(length(oldnm)))
      vnms[var.name==oldnm[j]] <- newnm[j]
    vnms


##### Added by GyoungJin Ahn : 1/20/2006 
   
  required <- vnms
##### End Addition by GyoungJin Ahn : 1/20/2006 
  
    
  }
}

