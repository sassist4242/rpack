################################################################
# winplot.R   01/02/02					       # 	
# modified the legend() function in R. 12/27/01. CLiu          #
# modified the title display for ERD.CLiu. 02/26/02	       #
# modified the graph display - add colors. chliu, 08/16/02     #
# modified the label indication for log scale.                 #
# last updated 08/29/02					       #
# Modified : linetype part in ec2plot : by GyoungJin Ahn 11/10/2004 #
# Modified : color and shape for datapoints are changed to be matched 
# between graph and legend in all plots:  by GyoungJin Ahn 11/10/2005
# Modified :  ec2plot ( stratplot, dataplot): by GyoungJin Ahn 11/10/2005
#             1) Title in dataplot
#             2) lenged content and printout in the terminal
#             3) range of y values in graph
# Modified : ec3plot (confplot) : by GyoungJin Ahn 11/10/2005
#             => graph size and position are changed
# Modified : ecplot (catplot) : by GyoungJin Ahn 11/14/2005
#             1) range of y values in graph
#               <- after calculating all necessary y values and then
#                  range of y will be set up for graph
# Added graph : allsevsplot()    : by GyoungJin Ahn 11/14/2005
#                <- all severities in one plot
# Modified : In ecplot(catplot), ec2plot(stratplot, dataplot), allsevsplot()
#            If there are zeros or negative values in the dataset for graph,
#            those values are excluded from dataset for graph.
#            -> ex) zero concentration, negative ec or erd values don't show up
#                   in the graph.
#               :  by GyoungJin Ahn 11/14/2005
# Modified : In ecplot() ( for catplot()), since the format of ECline is changed,
#            calculation of CI is chnaged.
################################################################
#
#  Function to plot EC line with confidence bounds & severity levels.
#  modified 8-14-95
################################################################
# Modified by GyoungJin Ahn L 12/15/2005
#  ECline table layout is changed by Cahn : 12/09/2005
#  Whether log transformation with gamma is used or not(linear), ECline table layouts are the same.
#  New layout :    Time, EC** ,sd ,LB95,UB95
#  All main executive plot function are changed 
#  pr2plot(for prplot()), ecplot(for catplot()), ec2plot(for stratplot() and dataplot())
#  ec3plot(for confplot()), devplot(for devplot())
#  : plot function modified by GyoungJin Ahn : 12/15/2005
################################################################
# Modified by GyoungJin Ahn : 12/15/2005
# In confplot(), when log transformation with gamma is used(especially to concentration)
# instead of go() for data with no strata, different estimate process(for gamma)
# will be used ( But,there can be no gamma even in log transformed model)
################################################################
# Modified by GyoungJin Ahn : 12/16/2005
# In devplot(), when log transformation with gamma is used(especially to concentration)
#  gamma needs to be considered.
# ( But,there can be no gamma even in log transformed model)
################################################################
# Modified by GyoungJin Ahn : 12/21/2005
# catplot(), stratplot(), confplot(), allsevsplot()
# Instead of "erdans" variable, "risks" variable is used.
# Risk type recognition setting to erdans using "risks" variable
# if risk type is "extra risk" then "ERD"
# else all "EC"
################################################################
#  Modified by GyoungJin Ahn : 12/21/2005
#  ECline table layout is changed by Cahn : 12/20/2005
#  Whether log transformation with "no" gamma is used ,
#  New layout :    Time, EC** ,sd ,LB95,UB95
#  sd = sd of log10(EC)*log(10)*ecq
#  but, in order to calculate UB or LB, sd for log10(EC) is necessary.
#  : plot function modified by GyoungJin Ahn : 12/21/2005
################################################################
# Modified by GyoungJin Ahn : 12/21/2005
# output of ec3table() function is changed.
# if this output is used for input for other functions, 
# then keep the format
# if not, then change the output format like benchmark dose
################################################################
# Modified by GyoungJin Ahn :  1/27/2006
#  Instead of ERD, "ERC" term will be used  everywhere in all CatReg program
# if this output is used for input for other functions, 
################################################################
# Modified by GyoungJin Ahn :  2/2/2006
#  Explanation for Confidence interval is added.
#  ( One-sided, two-sided ...)
################################################################
# Modified by GyoungJin Ahn : 02/10/2006              
# output format of ec3table function depends on time variable's name.
# ( whether time variable name includes hour or not )
################################################################
# Modified by GyoungJin Ahn : 02/22/2006              
#  When making graphs(catplot, stratplot, allsevsplot,  ),
#  Infinite, NaN("Not a Number" (R value)), NA("Missing value") 
# are excluded from the graphs.
################################################################
# Modified by GyoungJin Ahn : 03/02/2006              
#  When making graphs(catplot, stratplot, allsevsplot, confplot ),
#  Show some footnotes for Infinite, NaN("Not a Number" (R value)), NA("Missing value") 
# on the screen. 
# When ec3table is used, the same footnotes will be shown in the screen
################################################################
# Modified by GyoungJin Ahn : 04/13/2006
# Subtitle is added when Inf is in ERC table.
# Order of graph explanations is changed in all plots but devplot.
################################################################
# Modified by GyoungJin Ahn : 04/13/2006
# prplot is changed in order to express the proper severity level which a user picks.
# Modified by Ken Brown : 6/13/2006 Change to provide severity level as
# an option did not work correctly and was removed
################################################################
# Modified by GyoungJin Ahn : 04/21/2006
# When "Hidden" is shown in the terminal, 
# in ecplot(catplot) and allsevsplot, severity level needs to be considered.
# in ec2plot(dataplot and stratplot), stratification needs to be considered.
################################################################

ecplot <- function (data=NULL,
		    ecline=NULL,
		    stratum=NULL,
                    strata=NULL,
                    cl=.90,
                    log="xy",
                    xlog="CT",
                    title=NULL,
                    bbox=TRUE,
                    alegend=NULL,
                    zlegend="top",
                    xlim=NULL,
                    ylim=NULL,
                    plotsym=NULL,
		    color = NULL,
                    slabels=NULL,
                    var.name=required,
                    lwd=1,
                    jitter=FALSE,
                    erdans=erdans,
                   ecplotEmf=NULL,
				   global_data = NULL)
{
# Do we use user supplied data or do we assume the standard data structures?

  if (!is.null(data))
    data.char <- data

  if (!is.null(ecline))
    ECline <- ecline

  if (is.null(stratum))
    stratum <- strata
  strata <- stratum

# Do the strata... user supplied value or default?

  if(exists("ECline")){
    allStrata <- attributes(ECline$tables)$names
  } else {
	allStrata <- paste("SEV", xVars$info$sevs, ":", unique(global_data$strata.strata), sep="")
  }
  
  if (is.null(strata))
    strata <- allStrata
  else {
    valid <- match (strata, allStrata, nomatch=0) == 0
    if (sum(valid) > 0) {
      cat (" These strata are invalid:", strata[valid], "\n")
      invisible()
      return(NULL)
    } 
  }

  if (length(strata) > 1 ) {
    strata <- strata[1]
    cat ("WARNING: Multiple strata - plotting first one:", strata, "\n")
  }

  
# We pull the data out of the data structure.
  
  conc   <- as.double(data.char$data[,var.name["conc"]])
  dura   <- as.double(data.char$data[,var.name["time"]])
  y <- list(ylo=as.numeric(data.char$data[,var.name["loscore"]]),
            yhi=as.numeric(data.char$data[,var.name["hiscore"]]))
  nsev   <- max(max(y$ylo),max(y$yhi))

# Create the vectors & stuff needed for the legend.
  # if (is.null (slabels)) {
    # if (nsev == 1)
      # slabels  <- c("No effect", "Severe", "Censored")
    # else if (nsev == 2)
      # slabels  <- c("No effect", "Adverse", "Severe", "Censored")
    # else
      slabels  <- c("No effect", paste(rep("Severity",nsev),1:nsev), 
	            "Censored")
  # }

    if (is.null(plotsym)) { 
    if (nsev == 1) 
      plotsym <- c(1, 17, 4)
    else if (nsev == 2)
      plotsym <- c(1, 2, 15, 4)
    else if (nsev == 3)
      plotsym <- c(1, 2, 15, 18, 4)
    else if (nsev == 4)
      plotsym <- c(1, 2, 5, 15, 18, 4)
    else if (nsev == 5)
      plotsym <- c(1, 2, 5, 9, 18, 15, 4)
    else if (nsev == 6)
      plotsym <- c(1, 2, 5, 0, 9, 18, 15, 4)
    else if (nsev >= 7) # breaks down if you have more then 15 categories...
# 11/02/2005  Modified by GyoungJin Ahn 
# original :     6:6+nsev-7   => doesn't work
      plotsym <- c(1, 2, 5, 0, seq(6,6+nsev-7), 17, 18, 15, 4)
  } 

  # add for colors
  # cliu. 05/16/02
  if (is.null(color)) { 
    if (nsev == 1) 
      color <- c(3, 4, 6)
    else if (nsev == 2)
      color <- c(3, 4, 6, 17)
    else if (nsev == 3)
      color <- c(3, 4, 6, 17, 21)
    else if (nsev == 4)
      color <- c(3, 4, 6, 17, 21, 23)
    else if (nsev == 5)
      color <- c(3, 4, 6, 17, 21, 23, 25)
    else if (nsev == 6)
      color <- c(1, 2, 5, 3, 17, 18, 15, 4)
    else if (nsev >= 7) # breaks down if you have more then 15 categories...
# 11/02/2005  Modified by GyoungJin Ahn 
# original :     6:6+nsev-7   => doesn't work
      color <- c(1, 2, 5, 3, seq(6,6+nsev-7), 17, 18, 15, 4)
  }  

  if (bbox == TRUE) # bounding box?
    bty <- "y"
  else
    bty <- "n"

# We create the main title being as informative as we can.

  if (is.null(title)) {
  # add title for ERD-T
  # CLiu. 02/26/02

  #### Modified by GyoungJin Ahn : 1/27/2006 : 
  #### ERD -> ERC and EC just stays.        
    if(exists("ECline")){
      if (erdans=="y" || erdans=="Y")
        title <- paste ("ERC", ECline$pct, " Line (", strata, ")", sep="")
      else
        title <- paste ("EC", ECline$pct, " Line (", strata, ")", sep="")
	
       
    if (cl > 0.0 && cl < 1.0)
      title <- paste (title, " with ", format(cl*100), 
                      "% two-sided confidence bounds", sep="")
      title <- paste (title, ", Link = ", ECline$link, sep="")
    } else {
	  title <- paste("Categorical Data Plot", strata, sep=" - ")
	}
	
  }
      
# We plot the data on a logarithmic axis without any points.

  # If the log scale is used, indicate it in the y labels.
  # CLiu. 09/17/02. Reverted back to original, 04/15/03
#  if (length(grep("C", xlog)) > 0)    
#    ylab <- paste("log10(Dose (", var.name["conc"], "))", collapse="")   
#  else     
    ylab <- paste("Dose (", var.name["conc"], ")", collapse="")
    xlab <- paste("Time (", var.name["time"], ")", collapse="")

  
# 11/14/2005 : GyoungJin Ahn following  5 lines for range setup and plot will be put
#  in the other part :  
 # if (is.null(xlim)) xlim <- c(min(dura), max(dura))
 # if (is.null(ylim)) ylim <- c(min(conc), max(conc))

  #par(err=-1)
  #plot (dura, conc, log=log, type="n", xlim=xlim, ylim=ylim,
  #xlab=xlab, ylab=ylab,main=title)



# We draw the ECline with confidence bands. We ignore any "Lines out
# of bounds" error messages.

# in S Plus
#  if (length(grep("*C*", xlog)) > 0)
# in R, use the following line

  if(exists("ECline")){
# Modified by GyoungJin Ahn : 12/15/2005
# : ECline table layout is chnaged : (in all cases) Time, EC** ,sd ,LB95,UB95
# Begin deleting : 12/15/2005
#  if (length(grep("C", xlog)) > 0)
#    yec <- 10^ECline$tables[[strata]][,2]
#  else
# End deleting : 12/15/2005
    yec <- ECline$tables[[strata]][,2]
    
# Modified by GyoungJin Ahn : 12/15/2005
# : ECline table layout is chnaged : (in all cases) Time, EC** ,sd ,LB95,UB95
# Begin deleting : 12/15/2005
#  if (length(grep("T", xlog)) > 0)
#    xec <- 10^ECline$tables[[strata]][,1]
#  else
# End deleting : 12/15/2005
    xec <- ECline$tables[[strata]][,1]
#  xec <- 10^ECline$tables[[strata]][,1]
#  yec <- 10^ECline$tables[[strata]][,2]
#  lines (xec, yec, type="l", lty=1, lwd=lwd, col=3)

  }

 # 11/14/2005 : GyoungJin Ahn : plotting order is changed
# lines (xec, yec, type="l", lty="solid", lwd=1.5)

  if (cl > 0.0 && cl < 1.0 && exists("ECline")) {
    Z <- abs(qnorm((1-cl)/2))


# Modified by GyoungJin Ahn : 12/15/2005
# : ECline table layout is chnaged : (in all cases) Time, EC** ,sd ,LB95,UB95
# Modified by GyoungJin Ahn : 12/21/2005
# different calculation is necessary for UB and LB in the model
# with log transformation and no gamma
# Begin deleting : 12/15/2005
    ### Begin Changed by C. Ahn on 07/18/05
      #   yeclb <- ECline$tables[[strata]][,6];
      # yecub <- ECline$tables[[strata]][,7];
# End deleting : 12/15/2005
# Begin Added part :  12/15/2005 and 12/21/2005

 if((length(grep("Gamma",names(fits$coefficients)))==0) && 
    (ECline$xlog=="C" || ECline$xlog=="CT" ) )  {
   tmp.sd <-  ECline$tables[[strata]][,3]/(ECline$tables[[strata]][,2]*log(10))
   yeclb <- 10**(log10(ECline$tables[[strata]][,2]) - Z*tmp.sd)
   yecub <- 10**(log10(ECline$tables[[strata]][,2]) + Z*tmp.sd)
  } else {   
   yeclb <- (ECline$tables[[strata]][,2] - Z*ECline$tables[[strata]][,3])
   yecub <- (ECline$tables[[strata]][,2] + Z*ECline$tables[[strata]][,3])
  }   
  
  
# End Added part :  12/15/2005 and 12/21/2005
  
  
#  if (length(grep("C", xlog, value=TRUE)) > 0) {
#    yeclb <- (ECline$tables[[strata]][,2] - Z*ECline$tables[[strata]][,3])
#    yecub <- (ECline$tables[[strata]][,2] + Z*ECline$tables[[strata]][,3])
#  } else {
#    yeclb <- (ECline$tables[[strata]][,2] - Z*ECline$tables[[strata]][,3])
#    yecub <- (ECline$tables[[strata]][,2] + Z*ECline$tables[[strata]][,3])
#  }
### End Changed by C. Ahn on 07/18/05


# output the value and the 95% confidence bounds of EC/ERD
# CLiu. 03/14/02

cat ('\n')
#### Modified by GyoungJin Ahn : 1/27/2006 : 
  #### ERD -> ERC and EC just stays.    
 if (erdans=="y" || erdans=="Y")
   tmp <- "ERC"
 else
   tmp <- "EC"
      
cat (paste(tmp, ECline$pct, " value (", strata, ")", " with ", format(cl*100),"% two-sided confidence bounds", sep=""), '\n\n')
   
cat (paste("Duration (", var.name["time"], ") = ",sep=""), round(xec,3), '\n')
 

cat (paste(tmp, ECline$pct," = ", sep=""), round(yec,4), '\n')
cat (paste("Lower Bound of ",tmp,ECline$pct,sep="")," = ", round(yeclb,4), '\n')
cat (paste("Upper Bound of ",tmp,ECline$pct,sep="")," = ", round(yecub,4), '\n')

# Begin : Added by GyoungJin Ahn : 2/2/2006
# confidence interval's information is added in the terminal
cat("\n")
cat("Note:\n")
tmp1 <- paste("These ", cl*100,"% two-sided confidence bounds are equivalent to ", sep="")
cat (tmp1, "\n")
tmp2 <- paste(" ",(1-(1-cl)/2)*100,"% one-sided confidence bounds in each direction.", sep="")
cat (tmp2, "\n")
# End : Added by GyoungJin Ahn : 2/2/2006

################################################################
# Modified by GyoungJin Ahn : 03/02/2006              
#  When making graphs(catplot, stratplot, allsevsplot, confplot ),
#  Show some footnotes for Infinite, NaN("Not a Number" (R value)), NA("Missing value") 
# on the screen. 
# When ec3table is used, the same footnotes will be shown in the screen
################################################################
# begin : Added by GyoungJin Ahn : 03/02/2006
if(!is.finite(max(yec))) {
   if(length(yeclb[!is.na(yeclb)]) && min(yeclb, na.rm=TRUE)<0){
     cat("\n2. Infinite",tmp,"values, negative lower bound values of ERC and zero concentrations\n are not shown in the graph.", "\n" )
   } else {
     cat("\n2. Infinite",tmp,"values and zero concentrations are not shown in the graph.", "\n" )
   }
    tmp6 <- paste(": \"Inf\", \"NA\" and \"NaN\" indicates that ",tmp," was not calculated "
                  ,"\nbecause estimated background risk "
                  ,"( i.e., probability of response at zero concentration) \nexceeds an inordinately high value of 0.9."
                  , sep="")
cat(tmp6, "\n\n")
} else {
   if(min(yeclb, na.rm=TRUE)<0){
      cat("\n2. Negative lower bound values of ERC and zero concentrations are not shown in the graph.", "\n\n")
   } else {
      cat("\n2. Zero concentrations are not shown in the graph.", "\n\n")   
   }      
}
# end : Added by GyoungJin Ahn : 03/02/2006




# modified by GyoungJin Ahn : 04/13/2006
#cat ("\n** To obtain ERC estimates for user-specified durations without reading \n")
#cat ("them from a graph, use ec3table(duration=c(time1, time2, ... ) ).\n")
#cat ("(See Section 9.2 of CatReg Software User Manual) ** \n\n")

#    lines (xec, yeclb, type="l", lty=7, col=4)
#    lines (xec, yecub, type="l", lty=7, col=6)

# 11/14/2005 : GyoungJin Ahn : plotting order is changed
    #lines (xec, yeclb, type="l", lty="dashed", lwd=1.5)
    #lines (xec, yecub, type="l", lty="dashed", lwd=1.5)
  }
  
# 11/14/2005 : GyoungJin Ahn  : range setup and plotting part are rearranged into this place.
if (is.null(xlim)) xlim <- c(min(dura), max(dura))
  
#  11/14/2005 : GyoungJin Ahn   
#  if (is.null(ylim)) ylim <- c(min(c(conc,yeclb) ), max(conc))
#####  11/21/2005 : GyoungJin Ahn : although there are negative or (/and) zero values in plot data,
##### make the plot   
# original before 11/21/2005:    if (is.null(ylim)) ylim <- c(min(c(conc,yeclb) ), max(c(conc,yecub)))



# Modified by GyoungJin Ahn : 02/22/2006              
#  When making catplot
#  Infinite, NaN("Not a Number" (R value)), NA("Missing value") 
# are excluded from the graph.

# Begin : deleted by GyoungJin Ahn : 02/22/2006
# if (is.null(ylim)) ylim <- c(min(c(conc[conc>0],yeclb[yeclb>0], yec[yec>0],yecub[yecub>0] ) )
#              , max(c(conc,yecub)))
# End: deleted by GyoungJin Ahn : 02/22/2006

# Begin : Added by GyoungJin Ahn : 02/22/2006

  if(exists("ECline")){
    if (is.null(ylim)) ylim <- c(min( c(  conc[conc>0]
                                      , yeclb[ (is.finite(yeclb))][yeclb[ (is.finite(yeclb))] >0]
                                      , yec[ (is.finite(yec))][yec[ (is.finite(yec))] >0]
                                      , yecub[ (is.finite(yecub))][yecub[ (is.finite(yecub))] >0]))
                             , max( c(  conc
                                      , yecub[ (is.finite(yecub))][yecub[ (is.finite(yecub))] >0] )))
# End: Added by GyoungJin Ahn : 02/22/2006       
  }                               
 
 if(exists("xVars")) {
   #win.graph()
  windows(rescale = "fit", restoreConsole = FALSE)
  } else {
  par(err=-1)
  op <- par(mgp=c(2,1,0))  }

  # Expand the top margin to place the legend
  par(mar=par("mar")+c(1,0,1.4,0), mgp=c(2.5,1,0))

  
  plot (dura, conc, log=log, type="n", xlim=xlim, ylim=ylim,
  xlab=xlab, ylab=ylab)
  # Includes logic to avoid truncation of long titles
  title(main=paste(strwrap(title,width=60),collapse="\n"), line=3.2)

  if(!exists("xVars")) {
  par(op)
  }
 # Begin : Added by GyoungJin Ahn : 2/2/2006
 # confidence interval's information is added in the terminal
 # Modified by GyoungJin Ahn : 04/13/2006
 
 if (exists("ECline")){
   note1 <- paste(" - ", tmp1, tmp2, sep="")
   if(!is.finite(max(yec))) {	 
     note1 <- paste(note1,"\n",sep="")
     title(sub=paste(strwrap(note1, width=90, exdent=10), collapse="\n"), cex.sub=0.8, col.sub="green", line=4, adj=0)
	 title(sub=paste(" - ", tmp, " is not calculated when background risk exceeds an inordinately high value of 0.9.", sep=""), cex.sub=0.8, col.sub="red", line=4.8, adj=0)
   } else {
     title(sub=paste(strwrap(note1, width=90, exdent=10), collapse="\n"), cex.sub=0.8, col.sub="green", line=4.5, adj=0)  
   }
 # End : Added by GyoungJin Ahn : 2/2/2006
  
    lines (xec, yec, type="l", lty="solid", lwd=1.5)

    lines (xec, yeclb, type="l", lty="dashed", lwd=1.5)
    lines (xec, yecub, type="l", lty="dashed", lwd=1.5)
  }
  
# We draw the alegend...

  if (!is.null(alegend)) {
#    legend.notice ()
# S Plus    legend(alegend, legend=slabels, marks=plotsym, bty=bty)
# marks is unused arguments in R, use pch instead. 12/27/01. chliu
# add colors in the legend. cliu, 08/22/02
     legend(alegend, legend=slabels, pch=plotsys, col=color, bty=bty)
  }  
  
  

# Now we plot the strata lines & points. We pull the data for this
# strata out of the data set. There has got to be a better way...

  index <- match (strata, allStrata, nomatch=0)
  get <- rep(TRUE, length=length(dura)) # Assume all data values are included...
  
  if(exists("ECline")){  
    if (!is.null(ECline$factors)) {
      if (length(ECline$factors) == 1) {
        get <- data.char$data[,ECline$factors] == 
                                  ECline$categories[index]
      }
      else {
        for (j in 1:length(ECline$factors)) {
          get1 <- data.char$data[,ECline$factors[j]] == 
                                  ECline$categories[index,][j]
          get <- get & get1
        }
      }
    }
  } else {
    if (length(global_data$strata.factors) == 1) {
      get <- data.char$data[,global_data$strata.factors] == 
                                  global_data$strata.categories[index]
    } else {
      for (j in 1:length(global_data$strata.factors)) {
        get1 <- data.char$data[,global_data$strata.factors[j]] == 
                                  global_data$strata.categories[index,][j]
        get <- get & get1
      }
    }
  }
  get1 <- get

# We draw the points (currently we have 4 possible effects).

#  y <- designy (data.char$data)
# modified 6-27-95 DGS
# y <- list(ylo=as.numeric(data.char$data[,var.name["loscore"]]),
#            yhi=as.numeric(data.char$data[,var.name["hiscore"]]))

  ### jitter the data

  dups <- duplicated(paste(dura[get],conc[get]))
#  logaxis <- ifelse (length(grep("*x*", log, value=TRUE)), TRUE, FALSE)
  logaxis <- ifelse (length(grep("x", log, value=TRUE)), TRUE, FALSE)
  dura[get] <- jitterdata (dura[get], dups, jitter, logaxis, xlim)
#  logaxis <- ifelse (length(grep("*y*", log)), TRUE, FALSE)
  logaxis <- ifelse (length(grep("y", log)), TRUE, FALSE)
  conc[get] <- jitterdata (conc[get], dups, jitter, logaxis, ylim)

  ### plot the points (case by case)

  cat ("\nPoints plotted:\n\n")

  get <- (y$ylo==0 & y$yhi==0) & get1 # no effect
  
# 11/05/2005 GyoungJin Ahn : col part is modified   
  points (dura[get],conc[get],pch=plotsym[1], col=color[1])
  cat ("  ", slabels[1], ":\t", sum(get), "\n", sep="")
  cx <- dura[get]
  cy <- conc[get]
# Added by GyoungJin Ahn : 04/21/2006 
  cz <- y$ylo[get]

  for (i in 1:nsev) {
    get <- (y$ylo==i & y$yhi==i) & get1 # severe effects
# 11/05/2005 GyoungJin Ahn : col part is modified   
    points (dura[get],conc[get],pch=plotsym[i+1], col=color[i+1])
    cat ("  ", slabels[i+1], ":\t", sum(get), "\n", sep="")
    cx <- c(cx, dura[get])
    cy <- c(cy, conc[get])
# Added by GyoungJin Ahn : 04/21/2006 
     cz <- c(cz, y$ylo[get])

  }
 
  get <- (y$ylo<y$yhi) & get1 # censored

  # 11/05/2005 GyoungJin Ahn : col part is modified   
  points (dura[get],conc[get],pch=plotsym[nsev+2], col=color[nsev+2])
  cat ("  ", slabels[nsev+2], ":\t", sum(get), "\n", sep="")
  cx <- c(cx,dura[get])
  cy <- c(cy,conc[get])
# Added by GyoungJin Ahn : 04/21/2006 
     cz <- c(cz, rep("C",sum(get)))
 
  
# Figure out how many points are "hidden"
# Modified by GyoungJin Ahn : 04/21/2006 
# original :  xy  <- paste(cx,cy)
  xy  <- paste(cx,cy,cz)
  
  xyn <- length(xy)
  xyu <- length(unique(xy))


  
  
  cat ("\n")
  cat ("  Total:\t", xyn, "\n", sep="")
  cat ("  Hidden:\t", xyn-xyu, "\n", sep="")
  cat ("\n")

# We draw the zlegend...

  if (!is.null(zlegend)) {
#    legend.notice ()
# S Plus    legend(zlegend, legend=slabels, marks=plotsym, bty=bty)
# marks is unused arguments in R, use pch instead. 12/27/01 - cliu
    legend(zlegend, xpd=TRUE, horiz=TRUE, inset=c(0,-0.1), legend=slabels, pch=plotsym, col=color, bty=bty)
 }
 
 if(!is.null(ecplotEmf))
 {
	savePlot(ecplotEmf, device = dev.cur(), type="emf")
	sink(file="Data/plot.rdat", append=TRUE)
    cat(ecplotEmf)
    cat(".emf\n")
    sink()
 } else {
	if(exists("xVars")) {
		savePlot(paste(xVars$info$catplotFName,gsub(":","-",strata),sep="-"), device = dev.cur(), type="emf")
		sink(file="Data/plot.rdat", append=TRUE)
        cat(paste(xVars$info$catplotFName,gsub(":","-",strata),sep="-"))
        cat(".emf\n")
        sink()
	} 
}
  invisible()
  return(NULL)
}


#
#  Function to plot EC lines against species (max 8)
#  modified 8-14-95

ec2plot <- function (data=NULL,
                     ecline=NULL,
                     log="xy",
                     xlog="CT",
                     title=NULL,
                     bbox=TRUE,
                     alegend=NULL,
                     zlegend="top",
                     plotsym=NULL,
                     color=NULL,
                     linetype=NULL,
                     plottype=NULL,
                     strata=NULL,
                     stratum=NULL,
                     snames=NULL,
                     xlim=NULL,
                     ylim=NULL,
                     var.name=required,
                     jitter=FALSE,
                     lwd=1,
                     ect=TRUE, #  ect = FALSE  : Dataplot()   , ect =  TRUE: Stratplot()
                     erdans=erdans,
					 ec2plotEmf=NULL
                    )
{

# Do we use user supplied data or do we assume the standard data structures?
windows(rescale = "fit", restoreConsole = FALSE)

  if (!is.null(data))
    data.char$data <- data

  if (!is.null(ecline))
    ECline <- ecline

  if (is.null(strata))
    strata <- stratum

# We pull the data out of the data structure.

  conc <- as.double(data.char$data[,var.name["conc"]])
  dura <- as.double(data.char$data[,var.name["time"]])

# Do the strata... are user supplied valied?

  allStrata <- attributes(ECline$tables)$names
  if (is.null(strata)) {
    strata <- allStrata
  } else {
    valid <- match (strata, allStrata,nomatch=0) == 0
    if (sum(valid) > 0) {
      cat ("These strata(s) are invalid:", strata[valid], "\n")
      invisible()
      return(NULL)
    }
  }
  nstrat <- length(strata)

  if (is.null(snames))
    snames <- strata

# Define defaults & stuff (strata, line types, etc.)

  if (is.null(linetype))
#    linetype <- 1:nstrat  # Will cycle after 8 line types...
#    linetype <- c(2,1,2:nstrat)  # Will cycle after 8 line types...
    ##### GyoungJin Ahn 11/10/2004 ####
    if(nstrat==1){
        linetype <- 1 
    } else {
        linetype <- c(1,2:nstrat)  # Will cycle after 8 line types...
    }
    ###### GyoungJin Ahn end 

    
  if (is.null(plotsym))   # All of these may not appear on device...
# 11/02/2005 GyoungJin Ahn    : seq parts are changed ( little bit)  
    plotsym <- c(12,17,25,16,18,0,15,3,4,seq(6,14),seq(48,57),seq(65,90),seq(97,121))

  if (is.null(color))   # All of these may not appear on device...
    color <- c(3,4,6,17,21,18,150,15,3,4,seq(6,14),seq(48,57),seq(65,90),seq(97,121))

  if (is.null(plottype))
    if (nstrat <= 8) {
      plottype <- "l"  # use lines only if we can...
    } else
      plottype <- "b"  # otherwise use lines & points

  if (bbox == TRUE) { # bounding box?
    bty <- "y"
  } else
    bty <- "n"

# We create the main title being as informative as we can.
# ect = FALSE  : Dataplot()
# ect =  TRUE: Stratplot()

  if (is.null(title)) {
   if(!ect)  {
    title <- "Data plot(all SEV points)"
    title <- paste(title, ifelse(nstrat ==1 , " without stratum", " with strata"), sep="")
  } else {
    if (nstrat == 1) {
      title <- paste ("No Strata (", strata[1], ") ", sep="")
      line <- " line"
    } else { 
      title <- "All Strata: "
      line <- " lines"
    }
  # add title for RED-T
  # CLiu. 02/26/02
  
  #### Modified by GyoungJin Ahn : 1/27/2006 : 
  #### ERD -> ERC and EC just stays.        
    if (erdans=="y" || erdans=="Y")
       title <- paste (title, "ERC", ECline$pct, line, " at SEV=",unique(ECline$sev)
         ,", Link = ", ECline$link, sep="")
    else 
       title <- paste (title, "EC", ECline$pct, line, " at SEV=",unique(ECline$sev)
       , ", Link = ", ECline$link, sep="")
  }       
 }

# We now find X & Y vectors that contain all points of the data 
# and the EClines. This way, plot() knows the correct dimensions
# of the plotting area.

  ly1 <- length(dura)
  ly2 <- length(ECline$tables[[1]][,1])
  ly <- ly1+nstrat*ly2

  plotx <- 1:ly
  ploty <- 1:ly

  plotx[1:ly1] <- dura    # Should we pull out only data we will plot?
  ploty[1:ly1] <- conc

# modified 8-14-95 
  for (i in 1:nstrat) {
    from <- (ly1+(i-1)*ly2)
#    if (length(grep("*T*", xlog)) > 0) {}

# Modified by GyoungJin Ahn : 12/15/2005
# : ECline table layout is chnaged : (in all cases) Time, EC** ,sd ,LB95,UB95
# Begin deleting : 12/15/2005
#    if (length(grep("T", xlog)) > 0) {
#      plotx[(from+1):(from+ly2)] <-
#        10^ECline$tables[[strata[i]]][,1]
#    } else
# End deleting : 12/15/2005
        plotx[(from+1):(from+ly2)] <-
          ECline$tables[[strata[i]]][,1]
#   if (length(grep("*C", xlog)) > 0) {}

# Modified by GyoungJin Ahn : 12/15/2005
# : ECline table layout is chnaged : (in all cases) Time, EC** ,sd ,LB95,UB95
# Begin deleting : 12/15/2005
#    if (length(grep("C", xlog)) > 0) {
#      ploty[(from+1):(from+ly2)] <-
#        10^ECline$tables[[strata[i]]][,2]
#    } else
# End deleting : 12/15/2005
      ploty[(from+1):(from+ly2)] <-
        ECline$tables[[strata[i]]][,2]
  }


  
# We plot the data on a logarithmic axis without any points.

  # If the log scale is used, indicate it in the x and y labels.
  # CLiu. 09/17/02. Reverted back to original, 04/14/03
#  if (length(grep("C", xlog)) > 0)    
#    ylab <- paste("log10(Dose (", var.name["conc"], "))", collapse="")   
#  else     
    ylab <- paste("Dose (", var.name["conc"], ")", collapse="")
  xlab <- paste("Time (", var.name["time"], ")", collapse="")  
  

### GyoungJin Ahn : 11/12/2005 : for the correct plot, xlim and ylim are changed. 
# original 
#  if (is.null(xlim)) xlim <- c(min(dura), max(dura))
#  if (is.null(ylim)) ylim <- c(min(conc), max(conc))
# ect = TRUE : stratplot() function
# ect = FALSE : dataplot() function

# ranage definition before 11/21/2005 : GyoungJin ahn
# if (!ect) {
#     if (is.null(xlim)) xlim <- c(min(dura), max(dura))
#    if (is.null(ylim)) ylim <- c(min(conc), max(conc))
#  } else {
#    if (is.null(xlim)) xlim <- c(min(plotx), max(plotx))
#    if (is.null(ylim)) ylim <- c(min(ploty), max(ploty))
#  }

#####  11/21/2005 : GyoungJin Ahn : although there are negative or (/and) zero values in plot data,
##### make the plot   
   if (!ect) {
     if (is.null(xlim)) xlim <- c(min(dura), max(dura))
     if (is.null(ylim)) ylim <- c(min(conc[conc>0]), max(conc))
   } else {

      
     if (is.null(xlim)) xlim <- c(min(plotx), max(plotx))


# Modified by GyoungJin Ahn : 02/22/2006              
#  When making stratplot
#  Infinite, NaN("Not a Number" (R value)), NA("Missing value") 
# are excluded from the graph.
         
# Begin : deleted by GyoungJin Ahn : 02/22/2006
#    if (is.null(ylim)) ylim <- c(min(ploty[ploty>0]), max(ploty))
# End: deleted by GyoungJin Ahn : 02/22/2006

# Begin : Added by GyoungJin Ahn : 02/22/2006
  if (is.null(ylim)) ylim <- c( min(ploty[ (is.finite(ploty))][ploty[ (is.finite(ploty))] >0])
                              , max(ploty[ (is.finite(ploty))][ploty[ (is.finite(ploty))] >0]))
# End: Added by GyoungJin Ahn : 02/22/2006                                      
     
   }
  


  par(err=-1)
  # Expand the top margin to place the legend
  par(mar=par("mar")+c(0,0,1.4,0), mgp=c(2.5,1,0))
    
  plot (plotx, ploty, log=log, type="n", xlim=xlim, ylim=ylim, col=color,
        xlab=xlab, ylab=ylab)
  # Includes logic to avoid truncation of long titles
  title(main=paste(strwrap(title,width=60),collapse="\n"), line=3.5)

# We draw the alegend...

  if (!is.null(alegend)) {
#    legend.notice ()
# S Plus    legend(alegend, legend=snames, marks=plotsym, lty=linetype, bty=bty)
# marks is unused arguments in R, use pch instead. 12/27/01 - cliu
     legend(alegend, legend=snames, pch=plotsym, col=color, lty=linetype, bty=bty)
  }


# Now we plot the strata lines & points. We pull the data for this
# strata out of the data set. There has got to be a better way...

  cat ("\nPoints plotted:\n\n")

  dups <- duplicated(paste(dura,conc)) # jitter duplicated points only..
    
####  GyoungJin Ahn : 11/12/2005 : 
#### wrongfully Printed results -> corrected
#### Stratplot() and dataplot() don't include seveirty level in datapoints.
###  EC or ERD lines in stratplot() , however, change by severity levels.
# ltitle term is used in legend and printed results in  the terminal.

    if(nstrat >1 ) {  
    ltitle <- NULL  
    startpoint <- which(unlist(strsplit(snames[1],"")) == ":")[1]
    for(i in 1:nstrat) {  
    ltitle <- c(ltitle, substr(snames[i], (startpoint+1), (nchar(snames[i]) ) ) )
    }
    ltitle <- ltitle
    }
    
    
    tmp.max <- NULL
    
  for (i in 1:nstrat) {
    index <- match(strata[i], allStrata, nomatch=0)
    get <- rep(TRUE,length=ly1) # Assume all data values are included...
    if (!is.null(ECline$factors)) {
      if (length(ECline$factors) == 1) {
        get <- data.char$data[,ECline$factors] == 
                                    ECline$categories[index]
      } else {
        for (j in 1:length(ECline$factors)) {
          get1 <- data.char$data[,ECline$factors[j]] == 
                                    ECline$categories[index,][j]
          get <- get & get1
        }
      }
    }


  
  
  
    ### jitter the data

#    logaxis <- ifelse (length(grep("*x*", log)), TRUE, FALSE)
    logaxis <- ifelse (length(grep("x", log)), TRUE, FALSE)
    dura[get] <- jitterdata (dura[get], dups[get], jitter, logaxis, xlim)
#    logaxis <- ifelse (length(grep("*y*", log)), TRUE, FALSE)
    logaxis <- ifelse (length(grep("y", log)), TRUE, FALSE)
    conc[get] <- jitterdata (conc[get], dups[get], jitter, logaxis, ylim)

    ### plot the points

    
   points (dura[get],conc[get],pch=plotsym[i], col=color[i])
   
   
####  GyoungJin Ahn : 11/12/2005 : 
#### wrongfully Printed results -> corrected
#### Stratplot() and dataplot() don't include seveirty level in datapoints.
###  EC or ERD lines in stratplot() , however, change by severity levels.
# ltitle term is used in legend and printed results in  the terminal.
   
#original :    cat ("  ", snames[i], "\t", sum(get), "\n", sep="")
   
    cat ("  ", ifelse(nstrat==1,"All data",ltitle[i]), "\t", sum(get), "\n", sep="")
    if (i == 1) {
      cx <- c(dura[get])
      cy <- c(conc[get])  
# Added by GyoungJin Ahn : 04/21/2006 
      cz <- rep(i, sum(get))
    }  else {
      cx <- c(cx, dura[get])
      cy <- c(cy, conc[get])  
# Added by GyoungJin Ahn : 04/21/2006 
      cz <- c(cz, rep(i, sum(get)))
    }

    if (ect == TRUE) {
#      xec <- 10^ECline$tables[[strata[i]]][,1]
#      yec <- 10^ECline$tables[[strata[i]]][,2]
# modified 8-14-95
#       if (length(grep("*C", xlog)) > 0) {}


# Modified by GyoungJin Ahn : 12/15/2005
# : ECline table layout is chnaged : (in all cases) Time, EC** ,sd ,LB95,UB95
# Begin deleting : 12/15/2005
#       if (length(grep("C", xlog)) > 0) {
#         yec <- 10^ECline$tables[[strata[i]]][,2] 
#       } else
# End deleting : 12/15/2005
       yec <- ECline$tables[[strata[i]]][,2]
       tmp.max <- c(tmp.max, max(yec))
#       if (length(grep("*T*", xlog)) > 0) {}

# Modified by GyoungJin Ahn : 12/15/2005
# : ECline table layout is chnaged : (in all cases) Time, EC** ,sd ,LB95,UB95
# Begin deleting : 12/15/2005
#       if (length(grep("T", xlog)) > 0) {
#         xec <- 10^ECline$tables[[strata[i]]][,1] 
#       } else
# End deleting : 12/15/2005

       xec <- ECline$tables[[strata[i]]][,1]
# debugging purposes 03/11/02
# cat ("xec = ", xec, '\n')
# cat ("yec = ", yec, '\n')

     lines (xec, yec, type=plottype, lty=linetype[i],lwd=1.5, pch=plotsym[i])
    }
  }

# Figure out how many points are "hidden"
# Modified by GyoungJin Ahn : 04/21/2006 
# original :  xy  <- paste(cx,cy)
  xy  <- paste(cx,cy, cz)
  xyn <- length(xy)
  xyu <- length(unique(xy))

  cat ("\n")
  cat ("  Total:\t", xyn, "\n", sep="")
  cat ("  Hidden:\t", xyn-xyu, "\n", sep="")
  cat ("\n")

# We draw the zlegend...

  if (!is.null(zlegend)) {
#    legend.notice ()
# S Plus    legend(zlegend, legend=snames, marks=plotsym, lty=linetype, bty=bty)
# in R, marks is the unused argument, use pch instead. cliu - 12/27/01

# 11/02/2005 GyoungJin Ahn : Modified for expressing difference between dataplot() and stratplot()
# if there is no strata, then  legend is just "AllDataPoints:No Strata".
####  GyoungJin Ahn : 11/12/2005 : 
#### wrongfully Printed results -> corrected
#### Stratplot() and dataplot() don't include seveirty level in datapoints.
###  EC or ERD lines in stratplot() , however, change by severity levels.
# ltitle term is used in legend and printed results in  the terminal.


# ect = FALSE  : Dataplot()
# ect =  TRUE: Stratplot()


   if(ect){
    if(nstrat==1) {
      legend(zlegend, xpd=TRUE, horiz=TRUE, inset=c(0,-0.1), legend="AllData:No Strata"
             , pch=plotsym, col=color,  bty=bty, lty=linetype)
  } else {
      legend(zlegend, xpd=TRUE, horiz=TRUE, inset=c(0,-0.1), legend=ltitle, pch=plotsym, col=color,  bty=bty, lty=linetype) 
  }
 #####  11/21/2005 : GyoungJin Ahn : although there are negative or (/and) zero values in plot data,
##### make the plot  
  #### Modified by GyoungJin Ahn : 1/27/2006 : 
  #### ERD -> ERC and EC just stays.  
  
################################################################
# Modified by GyoungJin Ahn : 03/02/2006              
#  When making graphs(catplot, stratplot, allsevsplot, confplot ),
#  Show some footnotes for Infinite, NaN("Not a Number" (R value)), NA("Missing value") 
# on the screen. 
# When ec3table is used, the same footnotes will be shown in the screen
################################################################

    if (erdans=="y" || erdans=="Y") {
     if(!is.finite(max(tmp.max))) {
     cat("\n", "Note:\n\n","1. Infinite ERC values and zero concentrations are not shown in the graph.", "\n")  
     tmp6 <- paste(c(" : \"Inf\", \"NA\" and \"NaN\" indicates that ERC was not calculated ")
                  ,"\nbecause estimated background risk "
                  ,"( i.e., probability of response at zero concentration) \nexceeds an inordinately high value of 0.9."
                  , sep="")
     cat(tmp6, "\n\n")
     
     title(sub=paste(" - ERC is not calculated when background risk exceeds an inordinately high value of 0.9.",
            sep=""), cex.sub=0.8, col.sub="red", adj=0)

     
     } else {
     cat("\n", "* Zero concentrations are not shown in the graph.", "\n", "\n")  
     } 
    } else {
     if(!is.finite(max(tmp.max))) {
     cat("\n", "Note:\n\n","1. Infinite EC values and zero concentrations are not shown in the graph.", "\n")  
    tmp6 <- paste(c(" : \"Inf\", \"NA\" and \"NaN\" indicates that EC was not calculated ")
                  ,"\nbecause estimated background risk "
                  ,"( i.e., probability of response at zero concentration) \nexceeds an inordinately high value of 0.9."
                  , sep="")
     cat(tmp6, "\n\n")
     title(sub=paste(" - EC is not calculated when background risk exceeds an inordinately high value of 0.9.",
          sep=""), cex.sub=0.8, col.sub="red", adj=0)

     
     } else {
     cat("\n", "* Zero concentrations are not shown in the graph.", "\n", "\n")  
     } 
    }
     
  }  else {
     if(nstrat==1) {
      legend(zlegend, xpd=TRUE, horiz=TRUE, inset=c(0,-0.1), legend="AllData:No Strata"
             , pch=plotsym, col=color,  bty=bty)
  } else {
      legend(zlegend, xpd=TRUE, horiz=TRUE, inset=c(0,-0.1), legend=ltitle, pch=plotsym, col=color,  bty=bty) 
  }
     cat("\n", "Note:\n\n","1. Zero concentrations are not shown in the graph.", "\n", "\n")  

  }
  
  }

	#if(exists("xVars")) {
	#	savePlot(xVars$info$stratplotFName,type="emf")
	#}

  if(!is.null(ec2plotEmf))
  {
  savePlot(ec2plotEmf,type="emf")
  sink(file="Data/plot.rdat", append=TRUE)
  cat(paste(ec2plotEmf,".emf\n",sep=""))
  sink()
  } else {
	  if(exists("xVars")) {
		savePlot(xVars$info$stratplotFName,type="emf")
		  sink(file="Data/plot.rdat", append=TRUE)
		  cat(paste(xVars$info$stratplotFName,".emf\n",sep=""))
          sink()
	 }
  }
  
  invisible()
}


#
#  Function to plot EC lines against species (max 8)
#  modified 8-14-95

ec2plotD <- function (data=NULL,
                     ecline=NULL,
                     log="xy",
                     xlog="CT",
                     title=NULL,
                     bbox=TRUE,
                     alegend=NULL,
                     zlegend="top",
                     plotsym=NULL,
                     color=NULL,
                     linetype=NULL,
                     plottype=NULL,
                     strata=NULL,
                     stratum=NULL,
                     snames=NULL,
                     xlim=NULL,
                     ylim=NULL,
                     var.name=required,
                     jitter=FALSE,
                     lwd=1,
                     ect=TRUE, #  ect = FALSE  : Dataplot()   , ect =  TRUE: Stratplot()
                     erdans=erdans,
					 ec2plotDEmf=NULL
                    )
{

# Do we use user supplied data or do we assume the standard data structures?
windows(rescale = "fit", restoreConsole = FALSE)

  if (!is.null(data))
    data.char$data <- data

  if (!is.null(ecline))
    ECline <- ecline

  if (is.null(strata))
    strata <- stratum

# We pull the data out of the data structure.

  conc <- as.double(data.char$data[,var.name["conc"]])
  dura <- as.double(data.char$data[,var.name["time"]])

# Do the strata... are user supplied valied?

  allStrata <- attributes(ECline$tables)$names
  if (is.null(strata)) {
    strata <- allStrata
  } else {
    valid <- match (strata, allStrata,nomatch=0) == 0
    if (sum(valid) > 0) {
      cat ("These strata(s) are invalid:", strata[valid], "\n")
      invisible()
      return(NULL)
    }
  }
  nstrat <- length(strata)

  if (is.null(snames))
    snames <- strata

# Define defaults & stuff (strata, line types, etc.)

  if (is.null(linetype))
#    linetype <- 1:nstrat  # Will cycle after 8 line types...
#    linetype <- c(2,1,2:nstrat)  # Will cycle after 8 line types...
    ##### GyoungJin Ahn 11/10/2004 ####
    if(nstrat==1){
        linetype <- 1 
    } else {
        linetype <- c(1,2:nstrat)  # Will cycle after 8 line types...
    }
    ###### GyoungJin Ahn end 

    
  if (is.null(plotsym))   # All of these may not appear on device...
# 11/02/2005 GyoungJin Ahn    : seq parts are changed ( little bit)  
    plotsym <- c(12,17,25,16,18,0,15,3,4,seq(6,14),seq(48,57),seq(65,90),seq(97,121))

  if (is.null(color))   # All of these may not appear on device...
    color <- c(3,4,6,17,21,18,150,15,3,4,seq(6,14),seq(48,57),seq(65,90),seq(97,121))

  if (is.null(plottype))
    if (nstrat <= 8) {
      plottype <- "l"  # use lines only if we can...
    } else
      plottype <- "b"  # otherwise use lines & points

  if (bbox == TRUE) { # bounding box?
    bty <- "y"
  } else
    bty <- "n"

# We create the main title being as informative as we can.
# ect = FALSE  : Dataplot()
# ect =  TRUE: Stratplot()

  if (is.null(title)) {
   if(!ect)  {
    title <- "Data plot(all SEV points)"
    title <- paste(title, ifelse(nstrat ==1 , " without stratum", " with strata"), sep="")
  } else {
    if (nstrat == 1) {
      title <- paste ("No Strata (", strata[1], ") ", sep="")
      line <- " line"
    } else { 
      title <- "All Strata: "
      line <- " lines"
    }
  # add title for RED-T
  # CLiu. 02/26/02
  
  #### Modified by GyoungJin Ahn : 1/27/2006 : 
  #### ERD -> ERC and EC just stays.        
    if (erdans=="y" || erdans=="Y")
       title <- paste (title, "ERC", ECline$pct, line, " at SEV=",unique(ECline$sev)
         ,", Link = ", ECline$link, sep="")
    else 
       title <- paste (title, "EC", ECline$pct, line, " at SEV=",unique(ECline$sev)
       , ", Link = ", ECline$link, sep="")
  }       
 }

# We now find X & Y vectors that contain all points of the data 
# and the EClines. This way, plot() knows the correct dimensions
# of the plotting area.

  ly1 <- length(dura)
  ly2 <- length(ECline$tables[[1]][,1])
  ly <- ly1+nstrat*ly2

  plotx <- 1:ly
  ploty <- 1:ly

  plotx[1:ly1] <- dura    # Should we pull out only data we will plot?
  ploty[1:ly1] <- conc

# modified 8-14-95 
  for (i in 1:nstrat) {
    from <- (ly1+(i-1)*ly2)
#    if (length(grep("*T*", xlog)) > 0) {}

# Modified by GyoungJin Ahn : 12/15/2005
# : ECline table layout is chnaged : (in all cases) Time, EC** ,sd ,LB95,UB95
# Begin deleting : 12/15/2005
#    if (length(grep("T", xlog)) > 0) {
#      plotx[(from+1):(from+ly2)] <-
#        10^ECline$tables[[strata[i]]][,1]
#    } else
# End deleting : 12/15/2005
        plotx[(from+1):(from+ly2)] <-
          ECline$tables[[strata[i]]][,1]
#   if (length(grep("*C", xlog)) > 0) {}

# Modified by GyoungJin Ahn : 12/15/2005
# : ECline table layout is chnaged : (in all cases) Time, EC** ,sd ,LB95,UB95
# Begin deleting : 12/15/2005
#    if (length(grep("C", xlog)) > 0) {
#      ploty[(from+1):(from+ly2)] <-
#        10^ECline$tables[[strata[i]]][,2]
#    } else
# End deleting : 12/15/2005
      ploty[(from+1):(from+ly2)] <-
        ECline$tables[[strata[i]]][,2]
  }


  
# We plot the data on a logarithmic axis without any points.

  # If the log scale is used, indicate it in the x and y labels.
  # CLiu. 09/17/02. Reverted back to original, 04/14/03
#  if (length(grep("C", xlog)) > 0)    
#    ylab <- paste("log10(Dose (", var.name["conc"], "))", collapse="")   
#  else     
    ylab <- paste("Dose (", var.name["conc"], ")", collapse="")
  xlab <- paste("Time (", var.name["time"], ")", collapse="")  
  

### GyoungJin Ahn : 11/12/2005 : for the correct plot, xlim and ylim are changed. 
# original 
#  if (is.null(xlim)) xlim <- c(min(dura), max(dura))
#  if (is.null(ylim)) ylim <- c(min(conc), max(conc))
# ect = TRUE : stratplot() function
# ect = FALSE : dataplot() function

# ranage definition before 11/21/2005 : GyoungJin ahn
# if (!ect) {
#     if (is.null(xlim)) xlim <- c(min(dura), max(dura))
#    if (is.null(ylim)) ylim <- c(min(conc), max(conc))
#  } else {
#    if (is.null(xlim)) xlim <- c(min(plotx), max(plotx))
#    if (is.null(ylim)) ylim <- c(min(ploty), max(ploty))
#  }

#####  11/21/2005 : GyoungJin Ahn : although there are negative or (/and) zero values in plot data,
##### make the plot   
   if (!ect) {
     if (is.null(xlim)) xlim <- c(min(dura), max(dura))
     if (is.null(ylim)) ylim <- c(min(conc[conc>0]), max(conc))
   } else {

      
     if (is.null(xlim)) xlim <- c(min(plotx), max(plotx))


# Modified by GyoungJin Ahn : 02/22/2006              
#  When making stratplot
#  Infinite, NaN("Not a Number" (R value)), NA("Missing value") 
# are excluded from the graph.
         
# Begin : deleted by GyoungJin Ahn : 02/22/2006
#    if (is.null(ylim)) ylim <- c(min(ploty[ploty>0]), max(ploty))
# End: deleted by GyoungJin Ahn : 02/22/2006

# Begin : Added by GyoungJin Ahn : 02/22/2006
  if (is.null(ylim)) ylim <- c( min(ploty[ (is.finite(ploty))][ploty[ (is.finite(ploty))] >0])
                              , max(ploty[ (is.finite(ploty))][ploty[ (is.finite(ploty))] >0]))
# End: Added by GyoungJin Ahn : 02/22/2006                                      
     
   }
  


  par(err=-1)
  # Expand the top margin to place the legend
  par(mar=par("mar")+c(0,0,1.4,0), mgp=c(2.5,1,0))
  plot (plotx, ploty, log=log, type="n", xlim=xlim, ylim=ylim, col=color,
        xlab=xlab, ylab=ylab)
  # Includes logic to avoid truncation of long titles
  title(main=paste(strwrap(title,width=60),collapse="\n"), line=3.5)
# We draw the alegend...

  if (!is.null(alegend)) {
#    legend.notice ()
# S Plus    legend(alegend, legend=snames, marks=plotsym, lty=linetype, bty=bty)
# marks is unused arguments in R, use pch instead. 12/27/01 - cliu
     legend(alegend, legend=snames, pch=plotsym, col=color, lty=linetype, bty=bty)
  }


# Now we plot the strata lines & points. We pull the data for this
# strata out of the data set. There has got to be a better way...

  cat ("\nPoints plotted:\n\n")

  dups <- duplicated(paste(dura,conc)) # jitter duplicated points only..
    
####  GyoungJin Ahn : 11/12/2005 : 
#### wrongfully Printed results -> corrected
#### Stratplot() and dataplot() don't include seveirty level in datapoints.
###  EC or ERD lines in stratplot() , however, change by severity levels.
# ltitle term is used in legend and printed results in  the terminal.

    if(nstrat >1 ) {  
    ltitle <- NULL  
    startpoint <- which(unlist(strsplit(snames[1],"")) == ":")[1]
    for(i in 1:nstrat) {  
    ltitle <- c(ltitle, substr(snames[i], (startpoint+1), (nchar(snames[i]) ) ) )
    }
    ltitle <- ltitle
    }
    
    
    tmp.max <- NULL
    
  for (i in 1:nstrat) {
    index <- match(strata[i], allStrata, nomatch=0)
    get <- rep(TRUE,length=ly1) # Assume all data values are included...
    if (!is.null(ECline$factors)) {
      if (length(ECline$factors) == 1) {
        get <- data.char$data[,ECline$factors] == 
                                    ECline$categories[index]
      } else {
        for (j in 1:length(ECline$factors)) {
          get1 <- data.char$data[,ECline$factors[j]] == 
                                    ECline$categories[index,][j]
          get <- get & get1
        }
      }
    }

    ### jitter the data

#    logaxis <- ifelse (length(grep("*x*", log)), TRUE, FALSE)
    logaxis <- ifelse (length(grep("x", log)), TRUE, FALSE)
    dura[get] <- jitterdata (dura[get], dups[get], jitter, logaxis, xlim)
#    logaxis <- ifelse (length(grep("*y*", log)), TRUE, FALSE)
    logaxis <- ifelse (length(grep("y", log)), TRUE, FALSE)
    conc[get] <- jitterdata (conc[get], dups[get], jitter, logaxis, ylim)

    ### plot the points

    
   points (dura[get],conc[get],pch=plotsym[i], col=color[i])
   
   
####  GyoungJin Ahn : 11/12/2005 : 
#### wrongfully Printed results -> corrected
#### Stratplot() and dataplot() don't include seveirty level in datapoints.
###  EC or ERD lines in stratplot() , however, change by severity levels.
# ltitle term is used in legend and printed results in  the terminal.
   
#original :    cat ("  ", snames[i], "\t", sum(get), "\n", sep="")
   
    cat ("  ", ifelse(nstrat==1,"All data",ltitle[i]), "\t", sum(get), "\n", sep="")
    if (i == 1) {
      cx <- c(dura[get])
      cy <- c(conc[get])  
# Added by GyoungJin Ahn : 04/21/2006 
      cz <- rep(i, sum(get))
    }  else {
      cx <- c(cx, dura[get])
      cy <- c(cy, conc[get])  
# Added by GyoungJin Ahn : 04/21/2006 
      cz <- c(cz, rep(i, sum(get)))
    }

    if (ect == TRUE) {
#      xec <- 10^ECline$tables[[strata[i]]][,1]
#      yec <- 10^ECline$tables[[strata[i]]][,2]
# modified 8-14-95
#       if (length(grep("*C", xlog)) > 0) {}


# Modified by GyoungJin Ahn : 12/15/2005
# : ECline table layout is chnaged : (in all cases) Time, EC** ,sd ,LB95,UB95
# Begin deleting : 12/15/2005
#       if (length(grep("C", xlog)) > 0) {
#         yec <- 10^ECline$tables[[strata[i]]][,2] 
#       } else
# End deleting : 12/15/2005
       yec <- ECline$tables[[strata[i]]][,2]
       tmp.max <- c(tmp.max, max(yec))
#       if (length(grep("*T*", xlog)) > 0) {}

# Modified by GyoungJin Ahn : 12/15/2005
# : ECline table layout is chnaged : (in all cases) Time, EC** ,sd ,LB95,UB95
# Begin deleting : 12/15/2005
#       if (length(grep("T", xlog)) > 0) {
#         xec <- 10^ECline$tables[[strata[i]]][,1] 
#       } else
# End deleting : 12/15/2005

       xec <- ECline$tables[[strata[i]]][,1]
# debugging purposes 03/11/02
# cat ("xec = ", xec, '\n')
# cat ("yec = ", yec, '\n')

     lines (xec, yec, type=plottype, lty=linetype[i],lwd=1.5, pch=plotsym[i])
    }
  }

# Figure out how many points are "hidden"
# Modified by GyoungJin Ahn : 04/21/2006 
# original :  xy  <- paste(cx,cy)
  xy  <- paste(cx,cy, cz)
  xyn <- length(xy)
  xyu <- length(unique(xy))

  cat ("\n")
  cat ("  Total:\t", xyn, "\n", sep="")
  cat ("  Hidden:\t", xyn-xyu, "\n", sep="")
  cat ("\n")

# We draw the zlegend...

  if (!is.null(zlegend)) {
#    legend.notice ()
# S Plus    legend(zlegend, legend=snames, marks=plotsym, lty=linetype, bty=bty)
# in R, marks is the unused argument, use pch instead. cliu - 12/27/01

# 11/02/2005 GyoungJin Ahn : Modified for expressing difference between dataplot() and stratplot()
# if there is no strata, then  legend is just "AllDataPoints:No Strata".
####  GyoungJin Ahn : 11/12/2005 : 
#### wrongfully Printed results -> corrected
#### Stratplot() and dataplot() don't include seveirty level in datapoints.
###  EC or ERD lines in stratplot() , however, change by severity levels.
# ltitle term is used in legend and printed results in  the terminal.


# ect = FALSE  : Dataplot()
# ect =  TRUE: Stratplot()


   if(ect){
    if(nstrat==1) {
      legend(zlegend, xpd=TRUE, horiz=TRUE, inset=c(0,-0.1), legend="AllData:No Strata"
             , pch=plotsym, col=color,  bty=bty, lty=linetype)
  } else {
      legend(zlegend, xpd=TRUE, horiz=TRUE, inset=c(0, -0.1), legend=ltitle, pch=plotsym, col=color,  bty=bty, lty=linetype) 
  }
 #####  11/21/2005 : GyoungJin Ahn : although there are negative or (/and) zero values in plot data,
##### make the plot  
  #### Modified by GyoungJin Ahn : 1/27/2006 : 
  #### ERD -> ERC and EC just stays.  
  
################################################################
# Modified by GyoungJin Ahn : 03/02/2006              
#  When making graphs(catplot, stratplot, allsevsplot, confplot ),
#  Show some footnotes for Infinite, NaN("Not a Number" (R value)), NA("Missing value") 
# on the screen. 
# When ec3table is used, the same footnotes will be shown in the screen
################################################################

    if (erdans=="y" || erdans=="Y") {
     if(!is.finite(max(tmp.max))) {
     cat("\n", "Note:\n\n","1. Infinite ERC values and zero concentrations are not shown in the graph.", "\n")  
     tmp6 <- paste(c(" : \"Inf\", \"NA\" and \"NaN\" indicates that ERC was not calculated ")
                  ,"\nbecause estimated background risk "
                  ,"( i.e., probability of response at zero concentration) \nexceeds an inordinately high value of 0.9."
                  , sep="")
     cat(tmp6, "\n\n")
     
     title(sub=paste(" - ERC is not calculated when background risk exceeds an inordinately high value of 0.9.",
            sep=""), cex.sub=0.8, col.sub="red", adj=0)

     
     } else {
     cat("\n", "* Zero concentrations are not shown in the graph.", "\n", "\n")  
     } 
    } else {
     if(!is.finite(max(tmp.max))) {
     cat("\n", "Note:\n\n","1. Infinite EC values and zero concentrations are not shown in the graph.", "\n")  
    tmp6 <- paste(c(" : \"Inf\", \"NA\" and \"NaN\" indicates that EC was not calculated ")
                  ,"\nbecause estimated background risk "
                  ,"( i.e., probability of response at zero concentration) \nexceeds an inordinately high value of 0.9."
                  , sep="")
     cat(tmp6, "\n\n")
     title(sub=paste(" - EC is not calculated when background risk exceeds an inordinately high value of 0.9.",
          sep=""), cex.sub=0.8, col.sub="red", adj=0)

     
     } else {
     cat("\n", "* Zero concentrations are not shown in the graph.", "\n", "\n")  
     } 
    }
     
  }  else {
     if(nstrat==1) {
      legend(zlegend, xpd=TRUE, horiz=TRUE, inset=c(0, -0.1), legend="AllData:No Strata"
             , pch=plotsym, col=color,  bty=bty)
  } else {
      legend(zlegend, xpd=TRUE, horiz=TRUE, inset=c(0, -0.1), legend=ltitle, pch=plotsym, col=color,  bty=bty) 
  }
     cat("\n", "Note:\n\n","1. Zero concentrations are not shown in the graph.", "\n", "\n")  

  }
  
  }

#  if(exists("xVars")) {
#	savePlot(xVars$info$dataplotFName,type="emf")
#  }

  if(!is.null(ec2plotDEmf))
  {
	savePlot(ec2plotDEmf,type="emf")
	sink(file="Data/plot.rdat", append=TRUE)
    cat(paste(ec2plotDEmf,".emf\n",sep=""))
    sink()
  } else {
	  if(exists("xVars")) {
		savePlot(xVars$info$dataplotFName,type="emf")
		sink(file="Data/plot.rdat", append=TRUE)
        cat(paste(xVars$info$dataplotFName,".emf\n",sep=""))
        sink()
	  }
  }
  
  invisible()
} # End of ec2plotD()


### Give notice to user that he has to click on plot for legend...

legend.notice <- function ()
{
  cat ("\n")
  cat (">>> Please click on the graph to locate the <<<\n")
  cat (">>> upper left hand corner of the legend.   <<<\n")
  cat ("\n")

  invisible ()
}



#
# Fixed duration, multiple confidence intervals intervals...
# modified at 4/6/1999 - lmfu
#

ec3table <- function (Data=data, ecline=ECline, ecfits=fits, duration, output=TRUE 
                      , printout=FALSE ) {
 if(!output) {
if (length(grep("T", ecline$xlog))>0) duration <- log10(duration)

if(ecfits$model == "unrestricted conditional model")
	ect <- ectable.cp(severities = sort(unique(ecline$sev)), times
		 = duration, Data$strata.coefnames, Data$strata.factors,
		Data$strata.categories, coefs0 = ecfits$coefficients, 
		sand.var0 = ecfits$sand.var, link = ecfits$link, model= ecfits$model
		, nsevcat = ecfits$nsevcat, q = ecline$ct/100, xlog = ecline$xlog)
else ect <- ectable(severities = sort(unique(ecline$sev)), times = 
		duration, Data$strata.coefnames, Data$strata.factors, 
		Data$strata.categories, coefs0 = ecfits$coefficients, 
		sand.var0 = ecfits$sand.var, link = ecfits$link, model= ecfits$model,
		risks=ecline$risks, q = ecline$pct/100 , q_ci = ecline$qci/100
        , xlog = ecline$xlog)

  return (ect) 
  
} else {
   if (length(grep("T", ecline$xlog))>0) duration <- log10(duration)
   
    if(length(grep("total", ecline$risks, ignore.case=TRUE))>0  && 
        length(grep("gamma", names(fits$coefficients), ignore.case=TRUE)) ==0 ) {
           risk.case <- "extra risk"
    } else   {
       risk.case <- ecline$risks
    }          
     
  #### Modified by GyoungJin Ahn : 1/27/2006 : 
  #### ERD -> ERC and EC just stays.        
    erdans<-ifelse(length(grep("extra", risk.case, ignore.case=TRUE))>0,"Y", "N")
    if (erdans=="y" || erdans=="Y")
       tmp_ec <- "ERC"
    else
       tmp_ec <- "EC"
       
if(ecfits$model == "unrestricted conditional model")
	ect <- ectable.cp(severities = sort(unique(ecline$sev))
	      , times= duration
		 , Data$strata.coefnames
		 , Data$strata.factors
		 , Data$strata.categories
		 , coefs0 = ecfits$coefficients
		 , sand.var0 = ecfits$sand.var
		 , link = ecfits$link
		 , model = ecfits$model
		 , nsevcat = ecfits$nsevcat
		 , q = ecline$pct/100
		 , xlog = ecline$xlog)
else ect <- ectable(severities = sort(unique(ecline$sev))
	                , times= duration
                    , Data$strata.coefnames
                    , Data$strata.factors
                    , Data$strata.categories
                    , coefs0 = ecfits$coefficients
                    , sand.var0 = ecfits$sand.var
                    , link = ecfits$link
                    , model= ecfits$model
                    , risks=ecline$risks
                    , q = ecline$pct/100
                    , q_ci = ecline$qci/100
                    , xlog = ecline$xlog)

if(printout)  sink(file=outfile, append=printout)

 

if(printout) cat("\n\n*****************************",tmp_ec,"summary ***************************** \n \n \n")


   title1 <- paste (tmp_ec, ect$pct, sep="")
          
   cat(paste("\n< ",title1, " at specific time point(s) >",sep=""), "\n")
   tmp_tt <- paste("One-sided ", ect$qci,"% lower bound and one-sided "
                 ,ect$qci,"% upper bound ", sep="")
   cat(" :",tmp_tt, "\n\n")
   
   cat("* Risk Type :", risk.case, "\n \n")
   
   tmp.max <- NULL  
                       
   
   
  if(length(grep("hour", required['time'], ignore.case=TRUE))>0) {
    for(i in 1:length(names(ect$tables))) {
     title <- paste(title1, " at ", names(ect$tables)[i], sep="")
     cat("* ",title, "\n")
  
 #    tmp <- data.frame (  Time=round(ect$tables[[i]][,1] , 3) 
 #                       , round(ect$tables[[i]][,2], 4)
 #                       , LB= round(ect$tables[[i]][,4], 4)
 #                       , UB= round(ect$tables[[i]][,5],4)
 #                       , row.names = NULL)
 #    names(tmp)[2] <- title1 
 #    names(tmp)[3] <- paste("Lower Bound of ",tmp_ec,ect$pct,sep="")
 #    names(tmp)[4] <- paste("Upper Bound of ",tmp_ec,ect$pct,sep="")

      
      tmp <- data.frame ( round(ect$tables[[i]][,2], 4)
                        , LB= round(ect$tables[[i]][,4], 4)
                        , UB= round(ect$tables[[i]][,5],4) 
                        , SD= round(ect$tables[[i]][,3],4)						
                        , row.names = NULL)
      names(tmp)[1] <- title1 
      names(tmp)[2] <- paste("Lower Bound of ",tmp_ec,ect$pct,sep="")
      names(tmp)[3] <- paste("Upper Bound of ",tmp_ec,ect$pct,sep="")                        
	  names(tmp)[4] <- "Standard Error"
      Time<-paste(format(round(ect$tables[[i]][,1] , 3) )," Hours",sep="")
      dimnames(tmp)[[1]] <- Time

     #write.table(tmp,file = "", sep="\t",  row.names = FALSE) 
 
     
################################################################
# Modified by GyoungJin Ahn : 03/02/2006              
# When ec3table is used, show some footnotes for Infinite, 
# NaN("Not a Number" (R value)), NA("Missing value") on the screen. 
################################################################
# Begin : Added by GyoungJin Ahn
            tmp.max <- c(tmp.max , max(tmp[,1]))
# End : Added by GyoungJin Ahn
     
     
     print(tmp)
     cat("\n \n")
     }

          
          
     cat("\n")  
  } else {
    cat("* Time :", required['time'], "\n \n")
    for(i in 1:length(names(ect$tables))) {
     title <- paste(title1, " at ", names(ect$tables)[i], sep="")
     cat("* ",title, "\n")
  
     tmp <- data.frame (  Time=round(ect$tables[[i]][,1] , 3) 
                        , round(ect$tables[[i]][,2], 4)
                        , LB= round(ect$tables[[i]][,4], 4)
                        , UB= round(ect$tables[[i]][,5],4)  
						, SD= round(ect$tables[[i]][,3],4)
                        , row.names = NULL)
     names(tmp)[2] <- title1 
     names(tmp)[3] <- paste("LB",ect$qci,sep="")
     names(tmp)[4] <- paste("UB",ect$qci,sep="")
	 names(tmp)[5] <- "Standard Error"
     
     
################################################################
# Modified by GyoungJin Ahn : 03/02/2006              
# When ec3table is used, show some footnotes for Infinite, 
# NaN("Not a Number" (R value)), NA("Missing value") on the screen. 
################################################################
# Begin : Added by GyoungJin Ahn
            tmp.max <- c(tmp.max , max(tmp[,2]))
# End : Added by GyoungJin Ahn

     write.table(tmp,file = "", sep="\t",  row.names = FALSE) 
     
     cat("\n \n")
  
  }  
     cat( paste("\n ** Above UB",ect$qci," and LB",ect$qci,":",sep=""), "\n")
  
  
}
  cat("Note:\n")
  cat(paste("1. These ", tmp_tt, sep="") ,"\n")
  cat("confidence intervals are equivalent to \n")
  tmp_tt <- paste("the lower bound and upper bound of two-sided ",(1-(1-ect$qci/100)*2)*100
         ,"% confidence intervals.", sep="")
  cat(tmp_tt,"\n")
  cat("\n")
  
################################################################
# Modified by GyoungJin Ahn : 03/02/2006              
# When ec3table is used, show some footnotes for Infinite, 
# NaN("Not a Number" (R value)), NA("Missing value") on the screen. 
################################################################
# Begin : Added by GyoungJin Ahn
  if(!is.finite(max(tmp.max))) {
  cat(paste("2. \"Inf\", \"NA\" and \"NaN\" indicates that ERC was not calculated "
                  ,"\nbecause estimated background risk "
                  ,"( i.e., probability of response at zero concentration) \nexceeds an inordinately high value of 0.9."
                  , sep="")
                  )
    cat("\n \n")
  }
# Begin : Added by GyoungJin Ahn      
  if(printout) cat("***********************************************************************  \n\n")
  if(printout) sink()
  
  
  if(printout && outputCSV) {
    sink(file=gsub(".otx","_summary.csv",outfile), append=printout)
	
	cat("\n\n*****************************",tmp_ec,"summary ***************************** \n \n")
	
	title1 <- paste (tmp_ec, ect$pct, sep="")
          
    cat(paste("< ",title1, " at specific time point(s) >:",sep=""))
    tmp_tt <- paste("One-sided ", ect$qci,"% lower bound and one-sided "
                 ,ect$qci,"% upper bound", sep="")
    cat(",",tmp_tt, "\n")
   
    cat("* Risk Type:,", risk.case, "\n \n")
   
    tmp.max <- NULL  
	
	
	if(length(grep("hour", required['time'], ignore.case=TRUE))>0) {
    for(i in 1:length(names(ect$tables))) {
     title <- paste(title1, " at ", names(ect$tables)[i], sep="")
     cat("* ",title, "\n")

      tmp <- data.frame ( round(ect$tables[[i]][,2], 4)
                        , LB= round(ect$tables[[i]][,4], 4)
                        , UB= round(ect$tables[[i]][,5],4) 
                        , SD= round(ect$tables[[i]][,3],4)						
                        , row.names = NULL)
      names(tmp)[1] <- title1 
      names(tmp)[2] <- paste("Lower Bound of ",tmp_ec,ect$pct,sep="")
      names(tmp)[3] <- paste("Upper Bound of ",tmp_ec,ect$pct,sep="")                        
	  names(tmp)[4] <- "Standard Error"
      Time<-paste(format(round(ect$tables[[i]][,1] , 3) )," Hours",sep="")
      dimnames(tmp)[[1]] <- Time
 
     
################################################################
# Modified by GyoungJin Ahn : 03/02/2006              
# When ec3table is used, show some footnotes for Infinite, 
# NaN("Not a Number" (R value)), NA("Missing value") on the screen. 
################################################################
# Begin : Added by GyoungJin Ahn
            tmp.max <- c(tmp.max , max(tmp[,1]))
# End : Added by GyoungJin Ahn
     
	 cat(",,",paste(names(tmp),collapse=","),"\n")
     for (i in 1:dim(tmp)[1]){
	    cat(paste(",",rownames(tmp)[i],",",paste(tmp[i,],collapse=","),"\n",sep=""))
	 }
     }

          
          
     cat("\n")  
  } else {

    cat("* Time:,", required['time'], "\n \n")
    for(i in 1:length(names(ect$tables))) {
     title <- paste(title1, " at ", names(ect$tables)[i], sep="")
     cat("* ",title, "\n")
  
     tmp <- data.frame (  Time=round(ect$tables[[i]][,1] , 3) 
                        , round(ect$tables[[i]][,2], 4)
                        , LB= round(ect$tables[[i]][,4], 4)
                        , UB= round(ect$tables[[i]][,5],4)  
						, SD= round(ect$tables[[i]][,3],4)
                        , row.names = NULL)
     names(tmp)[2] <- title1 
     names(tmp)[3] <- paste("LB",ect$qci,sep="")
     names(tmp)[4] <- paste("UB",ect$qci,sep="")
	 names(tmp)[5] <- "Standard Error"
     
     
################################################################
# Modified by GyoungJin Ahn : 03/02/2006              
# When ec3table is used, show some footnotes for Infinite, 
# NaN("Not a Number" (R value)), NA("Missing value") on the screen. 
################################################################
# Begin : Added by GyoungJin Ahn
            tmp.max <- c(tmp.max , max(tmp[,2]))
# End : Added by GyoungJin Ahn

     cat(",",paste(names(tmp),collapse=","),"\n")
     for (i in 1:dim(tmp)[1]){
	    cat(paste(",",paste(tmp[i,],collapse=","),"\n",sep=""))
	 }
  
  }  
     cat( paste("\n ** Above UB",ect$qci," and LB",ect$qci,":",sep=""), "\n")
} 

  cat("Note:\n")
  cat(paste("1. These ", tmp_tt, sep="") ,"confidence intervals are equivalent to ")
  tmp_tt <- paste("the lower bound and upper bound of two-sided ",(1-(1-ect$qci/100)*2)*100
         ,"% confidence intervals.", sep="")
  cat(tmp_tt,"\n")
  
################################################################
# Modified by GyoungJin Ahn : 03/02/2006              
# When ec3table is used, show some footnotes for Infinite, 
# NaN("Not a Number" (R value)), NA("Missing value") on the screen. 
################################################################
# Begin : Added by GyoungJin Ahn
  if(!is.finite(max(tmp.max))) {
  cat(paste("2. \"Inf\", \"NA\" and \"NaN\" indicates that ERC was not calculated "
                  ,"\nbecause estimated background risk "
                  ,"( i.e., probability of response at zero concentration) \nexceeds an inordinately high value of 0.9."
                  , sep="")
                  )
    cat("\n")
  }
# Begin : Added by GyoungJin Ahn      
  cat("***********************************************************************  \n\n")
  


    sink()
  }
  
}
}


# added argument pecdata

ec3plot <- function (ecdata.obj=NULL,

                     ecline=NULL,
                     ecfits=NULL,
                     stratum=NULL,
                     strata=NULL,
                     snames=NULL,

                     pecdata=NULL,
                     pecline=NULL,
                     pecfits=NULL,
                     pstratum=NULL,
                     pstrata=NULL,

                     duration=NULL,
                     cl=.90,
                     title=NULL,
                     legend=NULL,
                     xlim=NULL,
                     xspace=NULL,
                     xlog=NULL,
                     lwd=1,
                     offend=0,
                     erdans=erdans,
                     var.name=required,
                     replace =FALSE,
					 ec3plotEmf=NULL
                    )
{

    if (!is.null(ecdata.obj))
    data.obj <- ecdata.obj
# added
  if (!is.null(pecdata))
    pdata <- pecdata
  
  if (is.null(pecline) || is.null(pecfits) ||
      is.null(ecline)  || is.null(ecfits) || is.null(duration)) {
    cat ("Error: the ecline, ecfits, pecline, pecfits & duration arguments are required.\n")
    invisible()
    return(NULL)
  }    

  if (length(duration) > 1) {
    cat ("Error: only one duration may be specified in ec3plot()\n")
    invisible()
    return(NULL)
  }
#
# log transform the duration if appropriate  # 12/20/95
#


#   Begin : Modified by GyoungJin Ahn : 2/2/06 :
# ec3table input is changed to put just time, not log10(time), 
# so, just duration needs to be used.

  # xduration <-  ifelse(length(grep("T", ecline$xlog)) > 0, log10(duration),duration)
   xduration <-  duration

#   End : Modified by GyoungJin Ahn : 2/2/06 :       
         
           
         
#
# First we deal with the pooled values...
#

  if (is.null(pstratum))
    pstratum <- pstrata
  pstrata <- pstratum

# Only one valid strata in ECline and/or in the strata argument.

  allStrata <- attributes(pecline$tables)$names
  
  if (is.null(pstrata))
    pstrata <- allStrata
  else {
    valid <- match (pstrata, allStrata, nomatch=0) == 0
    if (sum(valid) > 0) {
      cat ("These pecline strata(s) are invalid:", pstrata[valid], "\n")
      invisible()
      return(NULL)
    }
  }
  nstrat <- length(pstrata)
  
    if (nstrat > 1) {
    cat ("Only one (pooled) strata can be specified in 'pecline='.\n")
    invisible()
    return(NULL)
  }

#
# ... now we deal with the stratified values.
#

  if (is.null(stratum))
    stratum <- strata
  strata <- stratum

# Check the ecline strata's
 
  allStrata <- attributes(ecline$tables)$names
  if (is.null(strata))
    strata <- allStrata
  else {
    valid <- match (strata, allStrata, nomatch=0) == 0
    if (sum(valid) > 0) {
      cat ("These ecline strata are invalid:", strata[valid], "\n")
      invisible()
      return(NULL)
    }
  }
  nstrat <- length(strata) 
  
  

#
# The strata names...
#

  if (is.null(snames))
    snames <- strata

# We create the main title & legend being as informative as we can.

  if (is.null(title)) {
# add title for RED-T
# CLiu. 02/26/02

  #### Modified by GyoungJin Ahn : 1/27/2006 : 
  #### ERD -> ERC and EC just stays.   
    if (erdans == "y" || erdans == "Y") 
       title <- paste ("ERC", ecline$pct, " (", pstrata, ")", sep="")
    else 
       title <- paste ("EC", ecline$pct, " (", pstrata, ")", sep="")
     
       
    if (cl > 0.0 && cl < 1.0)
      title <- paste (title, " with ", format(cl*100), 
                      "% two-sided confidence bounds\n", sep="")
  }

  if (is.null(legend)) {
#    if (duration >= 0)
      legend <- paste (legend, paste("Time (", var.name["time"], ") = ", sep=""), duration, sep="")
#    else {
#      if (duration == -50)
#        legend <- paste (legend, "Time = Median", sep="")
#      else
#        legend <- paste (legend, "Percentile Time = ",
#                         abs(duration), sep="")
#    }
    legend <- paste (legend, ", Link = ", ECline$link, ".", sep="")
  }

#
# Find the values to plot...
#
# cat("\nduration",duration)
# cat("\nxduration",xduration)
# cat("\n")
#  ECpval <- ec3table (data.obj, pecline, pecfits, duration)
  ECpval <- ec3table (pdata, pecline, pecfits, xduration, output=FALSE)
  ECsval <- ec3table (data.obj, ecline,  ecfits,  xduration, output=FALSE)
  
 
  
  
# print(ECpval)
### pooled (ecline)

#  not used?
#  ECn <- length(ecline$tables[[pstrata[1]]][,2])
#  ECx <- duration

# ECpooled <- ECpval$tables[pstrata[1]][[1]][2]
# ECpstddv <- ECpval$tables[pstrata[1]][[1]][3]

# Ad hoc method, assuming "pooled" always has one "stratum"
  ECpooled <- ECpval$tables[[1]][2]
  ECpstddv <- ECpval$tables[[1]][3]

#  cat ("pooled:\n");
#  print (ECpooled)
#  print (ECpstddv)

### strata (ecline2)

  ECstrata <- vector ("double", nstrat)
  ECsstddv <- vector ("double", nstrat)

  for (i in 1:nstrat) {
    ECn <- length(ecline$tables[[strata[i]]][,2])
    ECstrata[i] <- ECsval$tables[strata[i]][[1]][2]
    ECsstddv[i] <- ECsval$tables[strata[i]][[1]][3]
  } 

#  cat ("strata:\n");
#  print (ECstrata)
#  print (ECsstddv)

#
# Plot the shell
#
  Z <- abs(qnorm((1-cl)/2))  

  
  
  
  
  
# Modified by GyoungJin Ahn : 12/15/2005
# : ECline table layout is chnaged : (in all cases) Time, EC** ,sd ,LB95,UB95
# Modified by GyoungJin Ahn : 12/21/2005
# different calculation is necessary for UB and LB in the model
# with log transformation and no gamma
# Begin Added part :  12/15/2005 and 12/21/2005

 if((length(grep("Gamma",names(fits$coefficients)))==0) && 
    (ECline$xlog=="C" || ECline$xlog=="CT" ) )  {
   tmp.ECpstddv <-  ECpstddv/(ECpooled*log(10))
   tmp.ECsstddv <-  ECsstddv/(ECstrata*log(10))
   ubp <- 10**(log10(ECpooled) + Z*tmp.ECpstddv)
   lbp <- 10**(log10(ECpooled) - Z*tmp.ECpstddv)
   ubs <- 10**(log10(ECstrata) + Z*tmp.ECsstddv)
   lbs <- 10**(log10(ECstrata) - Z*tmp.ECsstddv)
   
  }  else {
  ubp <- ECpooled + Z*ECpstddv
  lbp <- ECpooled - Z*ECpstddv
  ubs <- ECstrata + Z*ECsstddv
  lbs <- ECstrata - Z*ECsstddv
  }

# Modified by GyoungJin Ahn : 02/22/2006              
#  When making confplot
#  Infinite, NaN("Not a Number" (R value)), NA("Missing value") 
# are excluded from the graph.

# Begin : Added by GyoungJin Ahn : 02/22/2006
tmp.ubp=ubp[ (is.finite(ubp))]
tmp.lbp=lbp[ (is.finite(lbp))]
tmp.ubs=ubs[ (is.finite(ubs))]
tmp.lbs=lbs[ (is.finite(lbs))]       

 tmp <- c(tmp.ubp,tmp.lbp,tmp.ubs,tmp.lbs)

  if(length(tmp) >0 ) { 
#  if(dev.cur()==1 | !replace) {
#     motif()
#    win.graph()
#  }
  
   min.tmp <- min(tmp)
   max.tmp <- max(tmp)
# End: Added by GyoungJin Ahn : 02/22/2006                                      
  
  if (is.numeric(xspace)) {
# Begin : deleted by GyoungJin Ahn : 02/22/2006
#    xrange <- c(min(ubp,lbp,ubs,lbs)-xspace,max(ubp,lbp,ubs,lbs)+xspace)
# End: deleted by GyoungJin Ahn : 02/22/2006

# Begin: Added by GyoungJin Ahn : 02/22/2006                                      
     xrange <- c(min.tmp-xspace,max.tmp+xspace)
# End: Added by GyoungJin Ahn : 02/22/2006                                      
     
     
  } else {
    if (is.numeric(xlim)) {
      xrange <- xlim
    } else  {
# Begin : deleted by GyoungJin Ahn : 02/22/2006
    #  11/12/2005 GyoungJin Ahn : make the proper x- range
    #  Original :    xrange <- c(min(ubp,lbp,ubs,lbs)-200,max(ubp,lbp,ubs,lbs)+200)   
         # len <-  max(ubp,lbp,ubs,lbs)-min(ubp,lbp,ubs,lbs)   
         # xrange <- c(min(ubp,lbp,ubs,lbs)-len*0.5,max(ubp,lbp,ubs,lbs)+len*0.5 )
# End: deleted by GyoungJin Ahn : 02/22/2006
          
# Begin: Added by GyoungJin Ahn : 02/22/2006                                      
     len <-  max.tmp-min.tmp  
     xrange <- c(min.tmp-len*0.5,max.tmp+len*0.5 )  
# End: Added by GyoungJin Ahn : 02/22/2006                                      
     }                  
   }               
windows(rescale = "fit", restoreConsole = FALSE)
                  
   #### Modified by GyoungJin Ahn : 4/13/2006 : 
  #### The explanation for blue vertical line is added.  
  op <- par(mgp=c(2,1,0))
  # Expand the top margin to place the legend
  par(mar=par("mar")+c(1,0,1.4,0), mgp=c(2.5,1,0))

  plot (xrange, c(.25,nstrat+.75),
    axes=FALSE, type="n", xlab=paste("Dose ( ",legend," )", sep=""), ylab="")
  # Includes logic to avoid truncation of long titles
  title(main=paste(strwrap(title,width=60),collapse="\n"), line=3.5)

    #### Modified by GyoungJin Ahn : 4/13/2006 : 
  #### The explanation for blue vertical line is added.  
     title(main=list("\n Vertical lines are without stratification", cex=1.1, col="blue"))
   par(op)

  axis(1)

  # modified to show labels horizontally, and move the position to right
  # Chunhua Liu. 01/14/03
 #axis(2,at=seq(nstrat),labels=snames, pos=0.15, las=2)
 
#####    11/14/2005 : GyoungJin Ahn : value for "pos" in axix() function is changed
#####  original :axis(2,at=seq(nstrat),labels=snames, pos=min(xlim)+0.15, las=2)
# las = 2 : parallel, las =NULL: perpendicular
 axis(2,at=seq(nstrat),labels=snames, pos=min(xrange))

#  axis(2,seq(nstrat),labels=snames,ticks=TRUE)

# Draw the confidence lines for pooled ecline at duration=

  abline(v=ECpooled, col=3)
  abline(v=ubp,lty=2, col=4)
  abline(v=lbp,lty=2, col=4)

# Draw the confidence bounds for the strata of ecline=

# add colors to the lines in R
  points(ECstrata, seq(nstrat), pch=16, col="red")
  points(ubs, seq(nstrat)+offend, pch="]", col="blue")
  points(lbs, seq(nstrat)+offend, pch="[", col="blue")

  for (i in 1:nstrat) {
    lines(c(lbs[i], ubs[i]), c(i, i), lty=1, lwd=lwd)
  }
}
# output the EC/ERD and their 95% confidence intervals
# CLiu. 03/14/02. Last updated 03/19/02
cat ('\n')

  #### Modified by GyoungJin Ahn : 1/27/2006 : 
  #### ERD -> ERC and EC just stays.        

if (erdans=="y" || erdans == "Y")
   tmp_ec <- "ERC"
else 
   tmp_ec <- "EC"
   
   
cat (paste(tmp_ec,ECline$pct, " and ",format(cl*100),"% two-sided confidence bounds:", sep=""), '\n\n' )

if (nstrat > 1 ){
   cat(" \n* Stratified model case : \n")
}

for (i in 1:nstrat) {
   cat(paste(" ",snames[i], " ->",sep=""), round(ECstrata[i],4), '\t'
   , paste("Lower Bound of ",tmp_ec,ECline$pct," =",sep=""), round(lbs[i],4), '\t'
   , paste("Upper Bound of ",tmp_ec,ECline$pct," =",sep=""), round(ubs[i],4), '\n')
   #  cat (ECstrata[i], '\t'
   #, "LB","=", lbs[i], '\t',"UB","=", ubs[i], '\n')

   }
cat ('\n')
# Begin : Added by GyoungJin Ahn : 03/02/2006
if (nstrat > 1 ){
   cat("* Unstratified model case(Vertical lines in plot) : \n")
   cat(paste(" ",unlist(strsplit(snames[1], ":"))[1]," ->",sep=""), round(ECpooled,4), '\t'
   , paste("Lower Bound of ",tmp_ec,ECline$pct," =",sep=""), round(lbp,4), '\t'
   , paste("Upper Bound of ",tmp_ec,ECline$pct," =",sep=""), round(ubp,4), '\n')
}
# End : Added by GyoungJin Ahn : 03/02/2006

cat ('\n')

cat (legend, '\n\n\n')

# Begin : Added by GyoungJin Ahn : 2/2/2006
# confidence interval's information is added in the terminal
cat("Note:\n")
tmp1 <- paste("These ", cl*100,"% two-sided confidence bounds are equivalent to ", sep="")
cat (tmp1, "\n")
tmp2 <- paste(" ",(1-(1-cl)/2)*100,"% one-sided confidence bounds in each direction.", sep="")
cat (tmp2, "\n\n")
# End : Added by GyoungJin Ahn : 2/2/2006

################################################################
# Modified by GyoungJin Ahn : 03/02/2006              
#  When making graphs(catplot, stratplot, allsevsplot,confplot  ),
#  Show some footnotes for Infinite, NaN("Not a Number" (R value)), NA("Missing value") 
# on the screen. 
# When ec3table is used, the same footnotes will be shown in the screen
################################################################
# begin : Added by GyoungJin Ahn : 03/02/2006
    if(!is.finite(max(ECpooled, ECstrata))){
     cat(paste("2. Infinite",tmp_ec," values are not shown in the graph. \n" ,sep=""))
    
    cat(paste(": \"Inf\", \"NA\" and \"NaN\" indicates that ",tmp_ec," was not calculated "
                  ,"\nbecause estimated background risk "
                  ,"( i.e., probability of response at zero concentration) \nexceeds an inordinately high value of 0.9."
                  , sep=""), "\n\n")
}
tmp.max <- 0 
# begin : Added by GyoungJin Ahn : 03/02/2006
    if(!is.finite(ECpooled)) {
       for(i in 1:length(ECstrata)) {
          tmp.max <- tmp.max+(is.finite(ECstrata[i]))
       }     
       if(tmp.max ==0) { 
       tmp6 <- paste(c("3. Since all ",tmp_ec," values are infinite, confplot is not shown.")
                  , sep="")
cat(tmp6,  "\n\n\n")
}
}

# end : Added by GyoungJin Ahn : 03/02/2006



# Modified by GyoungJin Ahn : 02/22/2006              
#  When making confplot
#  Infinite, NaN("Not a Number" (R value)), NA("Missing value") 
# are excluded from the graph.

# Begin : Added by GyoungJin Ahn : 02/22/2006
  if(length(tmp) >0 ) { 
# End : Added by GyoungJin Ahn : 02/22/2006

    # Begin : Added by GyoungJin Ahn : 2/2/2006
     # confidence interval's information is added in the terminal
	 note1<- paste(" - ", tmp1, tmp2, sep="")
    if(!is.finite(max(ECpooled, ECstrata))){
	 note1 <- paste(note1, "\n", sep="")
	 title(sub=paste(strwrap(note1, width=90, exdent=10), collapse="\n"), cex.sub=0.8, col.sub="green", line=4, adj=0)
     title(sub=paste(" - ",tmp_ec," is not calculated when background risk exceeds an inordinately high value of 0.9."
               , sep=""), cex.sub=0.8, col.sub="red", line=4.8, adj=0)
    } else {
	  title(sub=paste(strwrap(note1, width=90, exdent=10), collapse="\n"), cex.sub=0.8, col.sub="green", line=4.5, adj=0)
	}
          
    # End : Added by GyoungJin Ahn : 2/2/2006
}

# Modified by GyoungJin Ahn : 03/02/2006
# the following 2 lines are blocked.

#cat ("Note: If there is not enough space for low-bound and up-bound data display,", "\n")
#cat ("you might need to extend your x-axis range by resetting \"xlim=c()\" in confplot().", "\n\n")

# show the actual figures in the graph
# CLiu. 03/28/02


# Modified by GyoungJin Ahn : 02/22/2006              
#  When making confplot
#  Infinite, NaN("Not a Number" (R value)), NA("Missing value") 
# are excluded from the graph.

# Begin : Added by GyoungJin Ahn : 02/22/2006
  if(length(tmp) >0 ) { 
# End : Added by GyoungJin Ahn : 02/22/2006

  ECstrata <- round(ECstrata, 2)
  ubs <- round(ubs, 2)
  lbs <- round(lbs, 2) 
  text (ECstrata, seq(nstrat), ECstrata, pos=1)
  text (ubs, seq(nstrat)+offend, ubs, pos=4)
  text (lbs, seq(nstrat)+offend, lbs, pos=2)
}

# checks if plot exists
if (length(tmp) > 0){
  if(!is.null(ec3plotEmf))
  {
	savePlot(ec3plotEmf,type="emf")
	sink(file="Data/plot.rdat", append=TRUE)
	cat(paste(ec3plotEmf, ".emf\n", sep=""))
	sink()
  } else {
	  if(exists("xVars")) {             
		savePlot(xVars$info$confplotFName,type="emf")
		sink(file="Data/plot.rdat", append=TRUE)
        cat(paste(xVars$info$confplotFName, ".emf\n", sep=""))
        sink()
	  }
  }
  } else {
    stop("Since all ",tmp_ec," values are infinite, confplot is not shown.", call. = FALSE)
  }

#if(exists("xVars")) {
#	savePlot(xVars$info$confplotFName,type="emf")
#}  
return(NULL)
}


######################################################################
################## Drivers for plotting commands -DGS ################

# Simple front end for plotting effective concentration lines,
# confidence intervals, and severity categories
# for individual strata.

 catplot_all <- function(ecline=ECline, jitter=FALSE) {
 
   if(exists("ECline")){
        cat("ECline EXISTS!!!!\n")
		items <- sort(attributes(ecline$tables)$names)
		lapply(items, catplot, jitter=jitter)
   } else {
        cat("ECline DOES NOT EXISTS!!!!\n")
		items <- paste("SEV", xVars$info$sevs, ":", unique(data$strata.strata), sep="")
		lapply(items, catplot, jitter=jitter, global_data=data)
   }
   
 }

catplot <- function(stratum,ecline=ECline,
                    replace=FALSE,
                    log="y",
                    xlog=data.char$xlog,
                    cl=.90,
                    title=NULL,
                    xlim=NULL,
                    ylim=NULL,
#                    stratum=NULL,
                    zlegend="top",
                    slabels=NULL,
                    plotsym=NULL,
                    popup=TRUE,
                    jitter=FALSE,
                    lwd=1,
					catplotEmf=NULL,
					global_data=NULL) {
#  if(dev.cur()==1 | !replace) {
#     motif()
#    win.graph()
# }

  if (exists("ECline")){
		items <- sort(attributes(ecline$tables)$names)
  } else {
		items <- NULL
  }
  
  pick <- stratum
  if (length(items) > 1 & is.null(pick))
    pick <- items[menu(items,graphics=popup,
                       title="Stratum?")]
  if (is.character(pick) & length(pick) ==0) stop()
  if(exists("xVars")) {
	lgnd <- FALSE
	} else {
	  if (!is.null(zlegend)) {
		lgnd <- TRUE
	  } else {
		lgnd <- menu(c("Yes","No"), graphics=popup,
		title=
		"Legend? If yes, click mouse to locate.")
		lgnd <- ifelse(lgnd==1, TRUE, FALSE)
	  }
  }

  if (exists("ECline")){ 
### Begin : added by GyoungJin Ahn : 1/27/2006
# When zero background in log transformed model, totla risk== extra risk
      if(length(grep("total", ecline$risks, ignore.case=TRUE))>0  && 
          length(grep("gamma", names(fits$coefficients), ignore.case=TRUE)) ==0 ) {
             risk.case <- "extra risk"
      } else   {
         risk.case <- ecline$risks
      }      
# End : addded by GyoungJin Ahn : 1/27/2006
    
# Begin : addded by GyoungJin Ahn : 12/21/2005
# Risk type recognition

     erdans<-ifelse(length(grep("extra", risk.case, ignore.case=TRUE))>0,"Y", "N")
# End : addded by GyoungJin Ahn : 12/21/2005
  }   
  
  tmp <-
  if (lgnd) {
    if (!is.null(zlegend)) {
      ecplot(stratum=pick, zlegend=zlegend,
             log=log, xlog=xlog, cl=cl, title=title,
             xlim=xlim, ylim=ylim, plotsym=plotsym,
             slabels=slabels, jitter=jitter, lwd=lwd, erdans=erdans, ecplotEmf=catplotEmf, global_data=global_data)
      } else
        ecplot(stratum=pick, zlegend=locator(1),
               log=log, xlog=xlog, cl=cl, title=title,
               xlim=xlim, ylim=ylim, plotsym=plotsym,
               slabels=slabels, jitter=jitter, lwd=lwd, erdans=erdans, ecplotEmf=catplotEmf, global_data=global_data)
  } else
    ecplot(stratum=pick, log=log, xlog=xlog, cl=cl,
           title=title, xlim=xlim, ylim=ylim,
           plotsym=plotsym,slabels=slabels, jitter=jitter, lwd=lwd, erdans=erdans, ecplotEmf=catplotEmf, global_data=global_data)
  cat("\n")
  
  #if(exists("xVars")) {
  #savePlot(xVars$info$catplotFName,type="emf")
  #}  
}


# Front end for plotting EC-T lines for different strata
# in the same graph. Data are labeled by stratum.

stratplot <- function(replace=FALSE,
                      log="y",
                      xlog=data.char$xlog,
                      title=NULL,
                      xlim=NULL,
                      ylim=NULL,
                      zlegend="top",
                      snames=NULL,
                      plotsym=NULL,
		      color=NULL,
                      popup=TRUE,
                      lwd=1,
                      jitter=FALSE,
                      linetype=NULL,
					  stratplotEmf=NULL) {
# if(dev.cur()==1 | !replace) {
#     motif()
#    win.graph()
# }
if(exists("xVars")) {
	lgnd <- FALSE
} else {
	if (!is.null(zlegend)) {
		lgnd <- TRUE
	} else {
		lgnd <- menu(c("Yes","No"), graphics=popup,
		title=
		"Legend? If yes, click mouse to locate.")
		lgnd <- ifelse(lgnd==1, TRUE, FALSE)
	}
}
  
### Begin : added by GyoungJin Ahn : 1/27/2006
# When zero background in log transformed model, totla risk== extra risk
    if(length(grep("total", ECline$risks, ignore.case=TRUE))>0  && 
        length(grep("gamma", names(fits$coefficients), ignore.case=TRUE)) ==0 ) {
           risk.case <- "extra risk"
    } else   {
       risk.case <- ECline$risks
    }      
# End : addded by GyoungJin Ahn : 1/27/2006
    
  
  
# Begin : addded by GyoungJin Ahn : 12/21/2005
# Risk type recognition

   erdans<-ifelse(length(grep("extra", risk.case, ignore.case=TRUE))>0,"Y", "N")
# End : addded by GyoungJin Ahn : 12/21/2005
  
  tmp <-
  if (lgnd) {
    if (!is.null(zlegend)) {
      ec2plot(zlegend=zlegend, log=log, xlog=xlog,
              title=title, xlim=xlim, ylim=ylim,
              plotsym=plotsym, snames=snames, color=color,
              lwd=lwd, linetype=linetype, jitter=jitter, erdans=erdans, ect=TRUE, ec2plotEmf=stratplotEmf)
    } else
      ec2plot(zlegend=locator(1), log=log, xlog=xlog,
              title=title, xlim=xlim, ylim=ylim,
              plotsym=plotsym, snames=snames, color=color,
              lwd=lwd, linetype=linetype, jitter=jitter, erdans=erdans, ect=TRUE, ec2plotEmf=stratplotEmf)
  }
  else ec2plot(log=log, xlog=xlog, title=title, xlim=xlim,
               ylim=ylim, plotsym=plotsym, 
               snames=snames, lwd=lwd,color=color,
               linetype=linetype, jitter=jitter, erdans=erdans, ect=TRUE, ec2plotEmf=stratplotEmf)
                
  cat("\n")
  
}

# Front end for ec3plot - multiple
# confidence intervals - it superimposes
# the confidence interval for an unstratified
# analysis of the same data. 

confplot <-
function(time=NULL,
         chdata.obj=data.char,
         data.obj=data,
         ecline=ECline,
         ecfits=fits,
         strata=names(ecline$tables),
         replace=FALSE,
         cl=.90,
         title=NULL,
         legend=NULL,
         xlim=NULL,
         xspace=NULL,
         xlog=ECline$xlog,
         fits0=NULL,
         lwd=1,
         offend=0,
		 confplotEmf=NULL
) {
# get results for unstratified model
  if(is.null(time)) stop("should specify time\n") 
  nchdata.obj <- chdata.obj
  if(exists("xVars")){
	#nchdata.obj$intestrata <- NULL
	#nchdata.obj$concstrata <- NULL
	#nchdata.obj$timestrata <- NULL
	nchdata.obj$strata <- list(intercept=NULL, conc=NULL, time=NULL)
  }
  else { nchdata.obj$strata <- list(intercept=NULL, conc=NULL, time=NULL)}
  pdata.obj <- makestrata(nchdata.obj)
  pdata.obj <- c(pdata.obj, list(worstcase=data.obj$worstcase))
  pecfits <- fits0
# 4/6/1999 - lmfu

#### Modified by GyoungJin Ahn : 12/15/2005
##  When log transformation with gamma is used(especially to concentration)
## there is gamma parameter( there can be no gamma even in log transformed model)
##  So, we need to run fit process for this.


#  Begin added part : 12/15/2005
 if((is.null(pecfits)) && (chdata.obj$xlog=="C" || chdata.obj$xlog=="CT") && 
     length(grep("Gamma",names(ecfits$coefficients)))>0  ){
   
        tmp.gammam <- est_gamma(pdata.obj)

#   cat("!!!!!!!!! Gamma=", gammam, "\n");

   ldose  <- pdata.obj$x[,grep("LG10CONC", colnames(pdata.obj$x))]
   ldoseg <- log10(exp(ldose*log(10))+tmp.gammam-10^(-10)*(ldose==-10))

   pdata.obj$x[,grep("LG10CONC", colnames(pdata.obj$x))] <- ldoseg; 

#  print(data$x);

   pecfits <- fgo(pdata.obj, pmodel, total_entry);
  } else 
#  End added part : 12/15/2005
     if(is.null(pecfits)) { 
        pecfits <- go(pdata.obj,pmodel,total_entry)
     }
  pecline <- ec3table(pdata.obj, ecline, pecfits, time, output=FALSE)
  
  
  
  
### Begin : added by GyoungJin Ahn : 1/27/2006
# When zero background in log transformed model, totla risk== extra risk
    if(length(grep("total", ecline$risks, ignore.case=TRUE))>0  && 
        length(grep("gamma", names(fits$coefficients), ignore.case=TRUE)) ==0 ) {
           risk.case <- "extra risk"
    } else   {
       risk.case <- ecline$risks
    }      
# End : addded by GyoungJin Ahn : 1/27/2006
  
  
# Begin : addded by GyoungJin Ahn : 12/21/2005
# Risk type recognition
   erdans<-ifelse(length(grep("extra", risk.case, ignore.case=TRUE))>0,"Y", "N")
# End : addded by GyoungJin Ahn : 12/21/2005
  
  ec3plot(ecdata.obj=data.obj,
          ecline=ecline,
          ecfits=ecfits,
          stratum=NULL,
          strata=attributes(ecline$tables)$names,
          pecdata=pdata.obj,
          pecline=pecline,
          pecfits=pecfits,
          duration=time,
          cl=cl,
          title=title,
          legend=legend,
          xlim=xlim,
          xspace=xspace,
          xlog=xlog,
          lwd=lwd,
          offend=offend,
          erdans=erdans,
          replace=replace,
		  ec3plotEmf=confplotEmf
   )
   
}

# function for removing existing plots from the screen

rmplots <- function() {
  if(dev.cur() != 1) {
    for (idev in 1:length(dev.list()))
      dev.off()
  }
}


# Front end for dataplot function

dataplot  <- function(replace=FALSE,
                      log="y",
                      xlog=data.char$xlog,
                      title=NULL,
                      xlim=NULL,
                      ylim=NULL,
                      zlegend="top",
                      snames=NULL,
                      plotsym=NULL,
		      color=NULL,
                      popup=TRUE,
                      lwd=1,
                      jitter=FALSE,
                      linetype=NULL,
					  dataplotEmf=NULL) {
#  if(dev.cur()==1 | !replace) {
#     motif()
#   win.graph()
# }

  if (!is.null(zlegend)) {
  # Currently, this code will never execute because zlegend is defaulted
    lgnd <- TRUE
  } else {
    lgnd <- menu(c("Yes","No"), graphics=popup,
                 title="Legend? If yes, click mouse to locate.")
    lgnd <- ifelse(lgnd==1, TRUE, FALSE)
  }

  tmp <-
  if (lgnd) {
    if (!is.null(zlegend)) {
      ec2plotD(zlegend=zlegend, log=log, xlog=xlog,
              title=title, xlim=xlim, ylim=ylim,
              plotsym=plotsym, color=color, snames=snames,
              lwd=lwd, linetype=linetype, jitter=jitter, ect=FALSE, ec2plotDEmf=dataplotEmf)
    } else
      ec2plotD(   zlegend=locator(1)
               , log=log
               , xlog=xlog
               , title=title
               , xlim=xlim
               , ylim=ylim
               , plotsym=plotsym
               , snames=snames
               , color=color
               , lwd=lwd
               , linetype=linetype
               , jitter=jitter
               , ect=FALSE
			   , ec2plotDEmf=dataplotEmf)
  }
  else ec2plotD(log=log, xlog=xlog, title=title, xlim=xlim,
               ylim=ylim, plotsym=plotsym, color=color,
               snames=snames, lwd=lwd,
               linetype=linetype, jitter=jitter, ect=FALSE, ec2plotDEmf=dataplotEmf)
  cat("\n")
}

#
### added 8/22/97 - HS prplot() & jitter().
### added colors for the R version. 08/16/02, cliu

pr2plot <- function (data=NULL,
                     ecline=NULL,
                     prX=NULL,
                     stratum=NULL,
                     strata=NULL,
                     severity=NULL,
                     log="x",
                     xlog="CT",
                     time=NULL,
                     conc=NULL,
                     title=NULL,
                     bbox=TRUE,
                     alegend=NULL,
                     zlegend="top",
                     xlim=NULL,
                     ylim=NULL,
                     plotsym=NULL,
		     color=NULL,
                     slabels=NULL,
                     jitter=FALSE,
                     prcurve=TRUE,
                     var.name=required,
                     snames=NULL,
                     xnames=NULL,
                     xlen=100,
					 pr2plotEmf=NULL)
{

# Do we use user supplied data or do we assume the standard data structures?
  
  if (!is.null(data))
    data.char <- data


  if (!is.null(ecline))
    ECline <- ecline

  if (!is.null(prX))
    prplotX <- prX
  else {
   if (length(grep("prplotX",objects())) == 0) 
#   if (length(grep("prplotX",objects())) == 0) 
     prplotX <- NULL
  }

  if (is.null(stratum))
    stratum <- strata
  strata <- stratum

# Do the strata... user supplied value or default?

### Begin Ken Brown 6/13/06 Restore prior code so sev=value is not an 
### option with prplot and remove code of G. Ahn 04/13/06 that calculates 
### allStrata incorrectly

  allStrata <- attributes(ECline$tables)$names
  if (is.null(strata))
    strata <- allStrata
  else {

### Modified by GyoungJin Ahn : 04/13/2006
### User can choose severity level.
###  if (is.null(strata))
###    strata <- attributes(ECline$tables)$names
###  else {
###     y.low  <- as.numeric(data.char$data[,var.name["loscore"]])
###     y.high <- as.numeric(data.char$data[,var.name["hiscore"]])
###    tmp.sev <- seq(min(y.low, y.high), max(y.low, y.high))
###    tmp.sev <- tmp.sev[-1]
###    allStrata <- NULL
###    if(is.null(ECline$categories)) {
###       allStrata <- paste(paste("SEV",tmp.sev,sep=""))
###    } else {
###       for( i in 1:length(tmp.sev)){
###       allStrata <- c(allStrata,paste(paste("SEV",tmp.sev[i],sep=""), ":",ECline$categories, sep=""))
###       }
###    }
### End Ken Brown6/13/06  
    
    valid <- match (strata, allStrata, nomatch=0) == 0

    if (sum(valid) > 0) {
      cat (" These strata are invalid:", strata[valid], "\n")
	invisible()
      return(NULL)
    } 
  }

  if (length(strata) > 1 ) {
    strata <- strata[1]
    cat ("WARNING: Multiple strata - plotting first one:", strata, "\n")
  }

  
# we pull the relevant data out of the data structure (all strata)

  vconc   <- as.double(data.char$data[,var.name["conc"]])
  vdura   <- as.double(data.char$data[,var.name["time"]])
  y <- list(ylo=as.numeric(data.char$data[,var.name["loscore"]]),
            yhi=as.numeric(data.char$data[,var.name["hiscore"]]))
  nsev   <- max(max(y$ylo),max(y$yhi))

# print(vconc)
# print(vdura)
# print(y)
# print(nsev)

  if (is.null(severity)) {
    severity <- as.integer(substring(strata,4,4))
    
  }


# Create the vectors & stuff needed for the legend.

  # if (is.null (slabels)) {
    # if (nsev == 1)
      # slabels  <- c("No effect", "Severe", "Censored")
    # else if (nsev == 2)
      # slabels  <- c("No effect", "Adverse", "Severe", "Censored")
    # else
      slabels  <- c("No effect", paste(rep("Severity",nsev),1:nsev),"Censored")
  # }

    if (is.null(plotsym)) { 
    if (nsev == 1) 
      plotsym <- c(1, 17, 4)
    else if (nsev == 2)
      plotsym <- c(1, 2, 15, 4)
    else if (nsev == 3)
      plotsym <- c(1, 2, 15, 18, 4)
    else if (nsev == 4)
      plotsym <- c(1, 2, 5, 15, 18, 4)
    else if (nsev == 5)
      plotsym <- c(1, 2, 5, 9, 18, 15, 4)
    else if (nsev == 6)
      plotsym <- c(1, 2, 5, 0, 9, 18, 15, 4)
    else if (nsev >= 7) # breaks down if you have more then 15 categories...
# 11/02/2005  Modified by GyoungJin Ahn 
# original :     6:6+nsev-7   => doesn't work
      plotsym <- c(1, 2, 5, 0, seq(6,6+nsev-7), 17, 18, 15, 4)
  } 

  # add for colors
  # cliu. 05/16/02
  if (is.null(color)) { 
    if (nsev == 1) 
      color <- c(3, 4, 6)
    else if (nsev == 2)
      color <- c(3, 4, 6, 17)
    else if (nsev == 3)
      color <- c(3, 4, 6, 17, 21)
    else if (nsev == 4)
      color <- c(3, 4, 6, 17, 21, 23)
    else if (nsev == 5)
      color <- c(3, 4, 6, 17, 21, 23, 25)
    else if (nsev == 6)
      color <- c(1, 2, 5, 3, 17, 18, 15, 4)
    else if (nsev >= 7) # breaks down if you have more then 15 categories...
# 11/02/2005  Modified by GyoungJin Ahn 
# original :     6:6+nsev-7   => doesn't work
      color <- c(1, 2, 5, 3, seq(6,6+nsev-7), 17, 18, 15, 4)
  }  


  if (bbox == TRUE) # bounding box?
    bty <- "y"
  else
    bty <- "n"

# We pull the specific Stratum data out of the data structure.
# Modified by  GyoungJin Ahn : 04/13/2006
# in order to put the proper data points in the graph
 # if(is.null(ECline$categories))

 
 # Modified by GyoungJin Ahn : 04/13/2006
#  if(!is.null(ECline$factors) ){
  if(!is.null(ECline$factors) && length(ECline$factors) == 1){
    ttt <- unlist(strsplit(strata,":"))[2]
    index <- match (ttt, ECline$categories, nomatch=0)
  } else 
  index <- match (strata, allStrata, nomatch=0)

  get <- rep(TRUE, length=length(vdura)) # Assume all data values are included...
  if (!is.null(ECline$factors)) {
    if (length(ECline$factors) == 1) {
      get <- data.char$data[,ECline$factors] == 
                                  ECline$categories[index]
    }
    else {
      for (j in 1:length(ECline$factors)) {
        get1 <- data.char$data[,ECline$factors[j]] == 
                                  ECline$categories[index,][j]
        get <- get & get1
      }
    }
  }
  get1 <- get

# print(sort(unique(vdura[get])))
# print(sort(unique(vconc[get])))
# print(get)
# print(sum(get))

# Now we pull the specific time or dose out of the data...

#  if (is.null(time) && is.null(conc)) {
#    cat ("ERROR: No time or dose was specified. Please specify fixed value.\n")
#    invisible()
#    return(NULL)
#  }

  if (!is.null(time) && !is.null(conc)) {
    cat ("ERROR: Both time & dose was specified. Specify one or the other.\n")
    invisible()
    return(NULL)
  }

  # we set up our variables to be either time or by default dose...
 
  if (!is.null(time)) {  # time= specified
    type  <- TRUE
    get2  <- vdura==time
    stype <- "time"
    value <- time
    vplot <- vconc  # our x-axis vector is: vplot = concentration
    yplot <- vdura
#      whichcol <- snames[grep("*CONC*", snames)]
      whichcol <- snames[grep("CONC", snames)]
#      fixcol <- snames[grep("*TIME*", snames)]
      fixcol <- snames[grep("TIME", snames)]
#    islog <- ifelse (length(grep("*C", xlog)) > 0, TRUE, FALSE)  # switched-dgs
    islog <- ifelse (length(grep("C", xlog)) > 0, TRUE, FALSE)  # switched-dgs
#      isflog <- ifelse (length(grep("*T*", xlog)) > 0 , TRUE, FALSE)    
      isflog <- ifelse (length(grep("T", xlog)) > 0 , TRUE, FALSE)    
      
  }   else if (!is.null(conc)) {  # conc= specified
    type  <- FALSE
    get2  <- vconc==conc
    stype <- "concentration"
    value <- conc
    vplot <- vdura  # our x-axis vector is: vplot = duration
    yplot <- vconc
#      whichcol <- snames[grep("*TIME*", snames)]
      whichcol <- snames[grep("TIME", snames)]
#      fixcol <- snames[grep("*CONC*",snames)]    
      fixcol <- snames[grep("CONC",snames)]    
#    islog <- ifelse (length(grep("*T*", xlog)) > 0, TRUE, FALSE)  # switched-dgs
    islog <- ifelse (length(grep("T", xlog)) > 0, TRUE, FALSE)  # switched-dgs
#      isflog <- ifelse (length(grep("*C", xlog)) > 0 , TRUE, FALSE)
      isflog <- ifelse (length(grep("C", xlog)) > 0 , TRUE, FALSE)
  }
  else {  # conc & time are null - concentration is on the x-axis
    type  <- TRUE
    get2  <- rep(1,length(vconc))
    stype <- "concentration"
    value <- conc
    vplot <- vconc  # our x-axis vector is: vplot = concentration
    yplot <- vconc
#      whichcol <- snames[grep("*CONC*", snames)]
      whichcol <- snames[grep("CONC", snames)]
      fixcol <- NULL
#    islog <- ifelse (length(grep("*C", xlog)) > 0, TRUE, FALSE)  # switched-dgs
    islog <- ifelse (length(grep("C", xlog)) > 0, TRUE, FALSE)  # switched-dgs
#      isflog <- ifelse (length(grep("*T*", xlog)) > 0 , TRUE, FALSE)  
      isflog <- ifelse (length(grep("T", xlog)) > 0 , TRUE, FALSE)  
  }

  #print(sort(unique(yplot[get1])))   # all possible values for user to choose

  s1 <- sum(get2) # matches for ALL strata
  get <- get & get2
  s2 <- sum(get)  # matches for selected strata

  cat ("\n")
  ## Modified by GyoungJin Ahn : 04/13/2006
  if(!is.null(conc)) 
    cat("The number of datapoints:\n","( at Concentration = ",conc,")\n")
  else if(!is.null(time))  
    cat("The number of datapoints:\n","( at Time = ",time,")\n")
  else  
    cat("The number of datapoints:\n","(No specific time or concentration) \n")  
# Begin Ken Brown 6/13/06 Modify message below
  cat ("1. data points in ALL strata: ", s1, "\n")
  
  if (!is.null(ECline$factors)) {
  cat ("2. data points for stratum plotted:", s2, "\n")
  }
  cat ("\n")
# End Ken Brown 6/13/06
# error messages...

  if (s1 == 0) {
    cat ("Note: No data value for '", stype, " = ", value, 
         "' exists in ANY stratum.\n\n", sep="");
    cat ("Possible values are:\n\n")
    print(sort(unique(yplot[get1])))
    cat ("\n")
#    invisible()
#    return(NULL)
  } 

  # Modified by GyoungJin Ahn : 04/13/2006
  if (s2 == 0 && !is.null(ECline$factors) ) {
    cat ("Note: No data value for '", stype, " = ", value, 
       # Original : 
       "' exists in stratum: ", strata, ".\n\n", sep="");
       #"' exists in stratum: ","'", ttt,"'", ".\n\n", sep="");
    cat ("Possible values are:\n\n")
    print(sort(unique(yplot[get1])))
    cat ("\n")
#    invisible()
#    return(NULL)
  } 
  
# Modified by GyoungJin Ahn : 02/22/2006              
#  Zero concentration values are not shown in the graph.
# Message added

# Begin : Added by GyoungJin Ahn : 02/22/2006 
     if(!is.null(conc)) {
     cat("\n")
     cat("Note:\n1. Zero times are not shown in the graph.", "\n", "\n")  
    } else {
     cat("\n")
     cat("Note:\n1. Zero doses are not shown in the graph.", "\n", "\n")  
    }    
# End: Added by GyoungJin Ahn : 02/22/2006                                      
    

# We create the main title being as informative as we can.

  if (is.null(title)) {
    if (type == TRUE) {# time fixed  or (both time and conc : not fixed)
      if (!is.null(time)) 
         title <- paste("Time (", var.name["time"], ") = ", time, 
                           "   Stratum = ", strata, collapse="")
      else  
         title <- paste("Time (", var.name["time"], ") = ", 
                          ifelse(length(grep("T",ECline$xlog))>0 ,1,0),
                           "   Stratum = ", strata, "\n", sep="")
                   
      }
    else 
        title <- paste("Dose (", var.name["conc"], ") = ", conc,
                       "   Stratum = ", strata, collapse="")
  }

# We plot the data on a logarithmic axis without any points.

  # x-axis setup 
  # time fixed / concentration on x-axis
  if (type == TRUE) {
#    if (length(grep("C", xlog)) >0) 
      # indicate the log scale in the lable. CLiu, 09/17/02. Reverted back to original, 04/14/03
#      xlab <- paste("log10(Dose (", var.name["conc"], "))", collapse="")
#    else 
      xlab <- paste("Dose (", var.name["conc"], ")", collapse="")
  } else {           # concentration fixed / duration on x-axis
#    if (length(grep("C", xlog)) >0) 
#      xlab <- paste("log10(Time (", var.name["time"], "))", collapse="")
#    else 
      xlab <- paste("Time (", var.name["time"], ")", collapse="")
  }

# Modified by GyoungJin Ahn : 02/22/2006              
#  Zero concentration values are not shown in the graph

# Begin : deleted by GyoungJin Ahn : 02/22/2006
#  if (is.null(xlim)) xlim <- c(min(vplot), max(vplot))
# End: deleted by GyoungJin Ahn : 02/22/2006

# Begin : Added by GyoungJin Ahn : 02/22/2006
  if (is.null(xlim)) xlim <- c(min(vplot[vplot>0]), max(vplot[vplot>0]))
# End: Added by GyoungJin Ahn : 02/22/2006                                      



  # y-axis setup
  ylab <- paste("Pr (Severity >=", severity, ")", sep=" ")
  if (is.null(ylim)) ylim <- c(-.2, 1.2)

  # the plot (ignore errors)
  par(err=-1)

  # Expand the top margin to place the legend
  par(mar=par("mar")+c(0,0,1.4,0), mgp=c(2.5,1,0))

# Modified by GyoungJin Ahn : 02/22/2006              
#  Zero concentration values are not shown in the graph

# Begin : deleted by GyoungJin Ahn : 02/22/2006
#  plot (vdura, vconc, log=log, type="n", xlim=xlim, ylim=ylim,
#           xlab=xlab, ylab=ylab, main=title, col=3, lab=c(5,3,7))
# End: deleted by GyoungJin Ahn : 02/22/2006

# Begin : Added by GyoungJin Ahn : 02/22/2006
tmp <- 1
 plot (tmp, tmp, log=log, type="n", xlim=xlim, ylim=ylim,
         xlab=xlab, ylab=ylab, col=3, lab=c(5,3,7))
# Includes logic to avoid truncation of long titles
  title(main=paste(strwrap(title,width=60),collapse="\n"), line=3.5)

  if(is.null(time) && is.null(conc))         
  title(main=list("\n: When both concentration and time are not specified, Time=1(when log(Time)), Time=0(when Time) for Probability", cex=0.8, col="blue"))

     
# End: Added by GyoungJin Ahn : 02/22/2006                                      
  
  
  

# We draw the alegend... 

  if (!is.null(alegend)) {
# S Plus    legend(alegend, legend=slabels, marks=plotsym, bty=bty)
# marks is the unused argument in R, use pch instead. 12/27/01 - chliu
    legend(alegend, legend=slabels, pch=plotsym, col=color, bty=bty)
  }

#
# Plot the points.
#

  if (s2 !=0 ) {

    logaxis <- ifelse(log=="x", TRUE, FALSE)

    # plot the non-censored points:
 
    q0 <- rep(0.0,length(vplot))
    q1 <- rep(1.0,length(vplot))
    q5 <- rep(0.5,length(vplot))

    for (i in 0:nsev) {
      get1 <- (y$ylo == i & y$yhi == i) & get 

      v1 <- jitterdata (vplot[get1], duplicated(vplot[get1]), 
        jitter, logaxis, xlim, xdiv=10)
      if (i < severity)
        points (v1, q0[get1], pch=plotsym[i+1], col=color[i+1])
      else
        points (v1, q1[get1], pch=plotsym[i+1], col=color[i+1])
    }


    
    # plot the censored data

    # low interval of censored data > severity --- plot at 1.0
    get1 <- (y$ylo < y$yhi) & (y$ylo > severity) & get
    v1 <- jitterdata (vplot[get1], duplicated(vplot[get1]), 
      jitter, logaxis, xlim, xdiv=12)
    points (v1, q1[get1], pch=plotsym[nsev+2], col=color[nsev+2])
  
    # high interval of censored data < severity --- plot at 0.0
    get1 <- (y$ylo < y$yhi) & (y$yhi < severity) & get
    v1 <- jitterdata (vplot[get1], duplicated(vplot[get1]), 
      jitter, logaxis, xlim, xdiv=10)
# 11/14/2005 : GyoungJin Ahn :
# color setup is changed : originally, censored cases have three differenct types of
# coloring to express different types of censoring type
# => it is unified.      
#   original: points (v1, q0[get1], pch=plotsym[nsev+2], col=30)
    points (v1, q0[get1], pch=plotsym[nsev+2], col=color[nsev+2])

    # severity level inside censored interval --- plot at 0.5.
    get1 <- (y$ylo < y$yhi) & ((y$ylo <= severity) & (y$yhi >= severity)) & get
    v1 <- jitterdata (vplot[get1], duplicated(vplot[get1]), 
      jitter, logaxis, xlim, xdiv=10)
      
# 11/14/2005 : GyoungJin Ahn :
# color setup is changed : originally, censored cases have three differenct types of
# coloring to express different types of censoring type
# => it is unified.      
#   original: points (v1, q5[get1], pch=plotsym[nsev+2], col=40)
      
    points (v1, q5[get1], pch=plotsym[nsev+2], col=color[nsev+2])
  }

# We draw the probability curve.

    # check if constant time value for all data points
	# if no time column is  mapped, the time values will be constant (=1)
	timeCol <- data.char$data[,xVars$required["time"]]
	constTime <- isTRUE(all.equal(timeCol, rep(timeCol[1], length(timeCol))))

  if (prcurve == TRUE) {
  
    if (constTime && is.null(time)){
	  warning("All time values are equal.  No probability curve will be drawn.\n")
	} else {

#   if (is.null(prplotX))
#      cat ("'prplotX' data matrix not found.
# Define it or use the prX= argument.\n")
#    else {
#
      # pull the SEV level out of the string.

#      nsev <- as.integer(substring(strata,4,4))
      sev  <- rep(severity, xlen)

      # generate X & Y

      if (is.null(prX)) {
        prplotX <- makeprX(xlim=xlim,
                           xlen=xlen,
                           whichcol=whichcol,
                           islog=islog,
                           value=value,
                           fixcol=fixcol,
                           isflog=isflog,
                           snames=snames,
                           xnames=xnames)
      }

              

### Modified by GyoungJin Ahn : 12/15/2005
### When Log transformation with gamma(especially, log transformation to Concentration)
###  is used, the gamma value needs to be concerned 
# Begin added part : GyoungJin Ahn : 12/15/2005
      if(length(grep("Gamma", names(fits$coefficients))) >0 )
       yX <- pr.exceed(sev, nsevcat=fits$nsevcat, prplotX$X,
                coefs=fits$coefficients[-grep("Gamma", names(fits$coefficients))]
                , link=fits$link, model=fits$model, 
                derivs=0, linkscale=1)
# End added part : GyoungJin Ahn : 12/15/2005
      else   yX <- pr.exceed(sev, nsevcat=fits$nsevcat, prplotX$X,
                coefs=fits$coefficients
                , link=fits$link, model=fits$model, 
                derivs=0, linkscale=1)
 
                
          # marple test
        #  cat("****", names(prplotX), "\n")
        #  print(prplotX)
        #   print(yX)
                
                                     
      if (islog){
### Modified by GyoungJin Ahn : 12/15/2005
### When Log transformation with gamma(especially, log transformation to Concentration)
###  is used, the gamma value needs to be concerned 
# Begin added part : GyoungJin Ahn : 12/15/2005
        if(length(grep("CONC", whichcol)) >0 && length(grep("Gamma", names(fits$coefficients))) >0 )   
          xX <- 10^prplotX$X[, prplotX$whichcol]-fits$coefficients["Gamma"]
        else 
          xX <- 10^prplotX$X[, prplotX$whichcol]
     }  else
        xX <- prplotX$X[, prplotX$whichcol]

      # plot the points/lines
      lines (xX, yX)
#    }
   }
  }

# We draw the zlegend...

  if (!is.null(zlegend)) {
# S Plus    legend(zlegend, legend=slabels, marks=plotsym, bty=bty)
# marks is the unused argument in R.
    legend(zlegend, xpd=TRUE, horiz=TRUE, inset=c(0,-0.1), legend=slabels, pch=plotsym, col=color, bty=bty)
  }

# exit

  if(!is.null(pr2plotEmf))
  {
	savePlot(pr2plotEmf,type="emf")
  } else {
	  if(exists("xVars")) {
		savePlot(xVars$info$prplotFName,type="emf")
	  }
  }

  invisible()
  #return(NULL)
}

# construct pseudo X matrix for probability function
makeprX <-
function(xlim,
         xlen,
         whichcol,
         islog,         
         value,
         fixcol,
         isflog,
         snames,
         xnames) {

#  icol <- snames[grep("*INTERCEPT*", snames)]
  icol <- snames[grep("INTERCEPT", snames)]
  
# 4/6/1999 - lmfu  X <- matrix(0, nrow=xlen, ncol=length(xnames))
# 4/6/1999  dimnames(X) <- list(paste("[",seq(nrow(X)),",]",sep=""), xnames)
  X <- matrix(0, nrow = xlen, ncol = length(dimnames(data$x)[[2]][-1]))
  dimnames(X) <- list(paste("[", seq(nrow(X)), ",]", sep = ""), dimnames(
                   data$x)[[2]][-1])       #
# 4/6/1999  if(length(xnames[xnames==icol])>0)



  if(length(xnames[dimnames(data$x)[[2]][-1] == icol]) > 0)
    X[, icol] <- rep(1, xlen)

  if (islog) {
### Modified by GyoungJin Ahn : 12/15/2005
### When Log transformation with gamma(especially, log transformation to Concentration)
###  is used, the gamma value needs to be concerned 
# Begin added part : GyoungJin Ahn : 12/15/2005
    if(length(grep("CONC", whichcol)) >0 && length(grep("Gamma", names(fits$coefficients))) >0)   
        X[, whichcol] <- seq(log10(xlim[1]+fits$coefficients["Gamma"])
                  , log10(xlim[2]+fits$coefficients["Gamma"]), length=xlen)
    else 
# End added part : GyoungJin Ahn : 12/15/2005
        X[, whichcol] <- seq(log10(xlim[1]), log10(xlim[2]), length=xlen)
   } else
    X[, whichcol] <- seq(xlim[1], xlim[2], length=xlen)

  if (!is.null(fixcol)) {
    if (isflog) {
### Modified by GyoungJin Ahn : 12/15/2005
### When Log transformation with gamma(especially, log transformation to Concentration)
###  is used, the gamma value needs to be concerned 
# Begin added part : GyoungJin Ahn : 12/15/2005
    if(length(grep("CONC", fixcol)) >0 && length(grep("Gamma", names(fits$coefficients))) >0)   
        X[, fixcol] <- rep(log10(value+fits$coefficients["Gamma"]), xlen)
    else 
# End added part : GyoungJin Ahn : 12/15/2005
      X[, fixcol] <- rep(log10(value), xlen)
    }  else
      X[, fixcol] <- rep(value, xlen)
  }

  #print(X)
  list(X=X, whichcol=whichcol)
}


### jitter the values to make duplicates visible
### d = vector of T/F for values to jitter. Single "T" == all values.

jitterdata <- function (x, d, jitter, log, limits,
  xdiv=15)
{ 
  if (jitter == TRUE) {
    # jitter for a log axis?
    if (log == TRUE) { 
      z <- x/xdiv   # the smaller the devisor the bigger the jitter
      x[d] <- x[d] + runif(length(x[d]), -z, z)
    }
    else { 
      z <- diff(range(limits))
      z <- z/100
      x[d] <- x[d] + runif(length(x[d]), -z, z)
    }
  }

  return(x)
}

# Front end for plotting probability curves
# for individual strata.

prplot_all <- function(ecline=ECline, time=NULL, conc=NULL, jitter=FALSE) {
  items <- sort(attributes(ecline$tables)$names)
  
  cat(items)
  
  lapply(items, prplot, time=time, conc=conc, jitter=jitter)
 
}

prplot <- function(stratum, ecline=ECline,
                    replace=FALSE,
                    time=NULL,
                    conc=NULL,
                    log="x",
                    xlog=data.char$xlog,
                    title=NULL,
                    xlim=NULL,
                    ylim=NULL,
#                    stratum=NULL,
                    zlegend="top",
                    slabels=NULL,
                    plotsym=NULL,
		    color=NULL,
                    popup=TRUE,
                    jitter=FALSE,
                    prX=NULL,
                    prcurve=TRUE,
                    severity=NULL,
                    xlen=100,
                    namematrix=data$strata.coefnames,
                    xnames=fits$xnames,
					prplotEmf=NULL)
{
  # make sure either time= or conc= is specified --- but not both.
windows(rescale = "fit", restoreConsole = FALSE)

#  if (is.null(time) && is.null(conc)) {
#    cat ("ERROR: No time or concentration was specified. Please specify fixed value.\n")
#    invisible()
#    return(NULL)
#  }

  if (!is.null(time) && !is.null(conc)) {
    cat ("ERROR: Both time & concentration was specified. Specify one or the other.\n")
    invisible()
    return(NULL)
  }

  # set up device

# if(dev.cur()==1 | !replace) {
#     motif()
#   win.graph()
# }
#
### Begin Ken Brown 6/12/06. Deleted text to make sev option work with prplot as it does not work
### correctly and restored prior instructions. To change severity level from what was used in run of CatReg, 
### run ecdata before prplot. Ken Brown 6/12/06 
### if(is.null(severity) && is.null(stratum))
###  items <- sort(attributes(ecline$tables)$names)
###  else if(!is.null(severity) && is.null(stratum)) {
###     if(is.null(ecline$categories))
###       items <- sort(paste("SEV",severity, sep=""))
###     else
###       items <- sort(paste("SEV",severity,":",ecline$categories, sep=""))
###                           
###   }
  
### pick <- stratum
###  if (length(items) > 1 & is.null(pick)){
###         pick <- items[menu(items,graphics=popup,title="Stratum?")]
###  } else if(length(items)==1 & is.null(pick)){
###         pick <- items
###  }
### End Ken Brown 6/12/06

### Begin. Put code back like it was before deletion above. Ken Brown 6/12/06
    items <- sort(attributes(ecline$tables)$names)
    pick <- stratum
    if (length(items) > 1 & is.null(pick))
      pick <- items[menu(items,graphics=popup,
                       title="Stratum?")]
### End. Ken Brown 6/12/06

  if (is.character(pick) & length(pick) ==0) stop()

# coefficient names corresponding to stratum

  if (length(items) > 1)
    snames <- namematrix[items==pick,]
  else snames <- c(namematrix)


  
# legend

if(exists("xVars")) {
	#severity = xVars$info$sevs
	prplotEmf=ifelse(is.null(prplotEmf),xVars$info$prplotFName,prplotEmf)
}
if (!is.null(zlegend)) {
    lgnd <- TRUE
} else {
  # Currently, this code will never execute because zlegend is defaulted
    lgnd <- menu(c("Yes","No"), graphics=popup,
    title=
    "Legend? If yes, click mouse to locate.")
    lgnd <- ifelse(lgnd==1, TRUE, FALSE)
}

# call plotting function

  tmp <-
  if (lgnd) {
    if (!is.null(zlegend)) {
      pr2plot(stratum=pick, zlegend=zlegend,
             log=log, xlog=xlog, title=title,
             xlim=xlim, ylim=ylim, plotsym=plotsym,
             slabels=slabels, jitter=jitter, time=time, color=color,
             conc=conc, prcurve=prcurve, prX=prX, severity=severity,
             snames=snames, xnames=xnames, xlen=xlen, pr2plotEmf=paste(prplotEmf,gsub(":","-",pick),sep="-"))
      } else
        pr2plot(stratum=pick, zlegend=locator(1),
               log=log, xlog=xlog, title=title,
               xlim=xlim, ylim=ylim, plotsym=plotsym,
               slabels=slabels, jitter=jitter, time=time,color=color,
               conc=conc, prcurve=prcurve, prX=prX, severity=severity,
               snames=snames, xnames=xnames, xlen=xlen, pr2plotEmf=paste(prplotEmf,gsub(":","-",pick),sep="-"))
  } else
    pr2plot(stratum=pick, log=log, xlog=xlog,
           title=title, xlim=xlim, ylim=ylim,
           plotsym=plotsym,slabels=slabels,
           jitter=jitter, time=time, conc=conc,color=color,
           prcurve=prcurve, prX=prX, severity=severity,
           snames=snames, xnames=xnames, xlen=xlen, pr2plotEmf=paste(prplotEmf,gsub(":","-",pick),sep="-"))
   
 
  cat("\n")
  
  sink(file="Data/plot.rdat", append=TRUE)
  cat(paste(prplotEmf,gsub(":","-",pick),sep="-"))
  cat(".emf\n")
  sink()
  
}

#### deviance residual plot -dgs 10/3/97
#### modified to add colors in the plot - cliu 08/16/02

devplot <-
function(
  data.obj = data,
  fit.obj = fits,
  xvar = NULL,
  xlab = NULL,
  ylab= "Gen. Deviance",
  plotsym=c(1,16,2,17,5,18,0,15,3,4,6:14,48:57,65:90,97:121),
  # add color for R version. 08/16/02, cliu
 # color = c(3,4,6,17,21,3,4,6,17,21),
 color = c(3,4,6,1,5,2,8,7,3,4,6,1,5,2,8,7),
  var.name=required,
  replace = FALSE,
  zlegend="top",
  bty="y",
  popup=TRUE,
  devplotEmf=NULL
  ) {
  
#### Modified by GyoungJin Ahn : 12/16/2005
##  When log transformation with gamma is used(especially to concentration)
## there is gamma parameter( there can be no gamma even in log transformed model)
##  So, we need to run fit process for this.

#  Begin added part : 12/16/2005
 if(length(grep("LG10CONC",unlist(dimnames(data.obj$x)) )) >0 &&
    length(grep("Gamma",names(fit.obj$coefficients)))>0  ){

   #ldose  <- data.obj$x[,grep("LG10CONC", colnames(data.obj$x))]
   #tmp.gammam <- grep("Gamma",names(fit.obj$coefficients), value=TRUE)
   #ldoseg <- log10(exp(ldose*log(10))+tmp.gammam-10^(-10)*(ldose==-10))

   #data.obj$x[,grep("LG10CONC", colnames(data.obj$x))] <- ldoseg
   fit.obj <- list( link=fit.obj$link, model=fit.obj$model, scale=fit.obj$scale
           , coefficients=fit.obj$coefficients[-grep("Gamma",names(fit.obj$coefficients))])
   }
#  End added part : 12/16/2005
   
        resid <- diagnose(data.obj, fit.obj)$resid.dev
        all.nm <- dimnames(data.obj$x)[[2]]
# extract concentration
#        xc.nm <- all.nm[grep("*CONC*", all.nm)]
        xc.nm <- all.nm[grep("CONC", all.nm)]
        if (length(xc.nm)==0) {
          hasconc <- FALSE
        } else {
          hasconc <- TRUE
          xc.log <- 
#            ifelse(length(grep("*LG10*", xc.nm))>0,TRUE,FALSE)
            ifelse(length(grep("LG10", xc.nm))>0,TRUE,FALSE)
          xc <- data.obj$x[, xc.nm]
          if(is.matrix(xc))
            xc <- apply(xc,1,"sum")
        }
# extract duration
#        xt.nm <- all.nm[grep("*TIME*", all.nm)]
        xt.nm <- all.nm[grep("TIME", all.nm)]
        if (length(xt.nm)==0) {
          hastime <- FALSE
        } else {
          hastime <- TRUE
          xt.log <- 
#            ifelse(length(grep("*LG10*", xt.nm))>0,TRUE,FALSE)
            ifelse(length(grep("LG10", xt.nm))>0,TRUE,FALSE)
          xt <- data.obj$x[, xt.nm]
          if(is.matrix(xt))
            xt <- apply(xt,1,"sum")
        }
      # data frame of choices for x
        xdat <- data.frame("Obs Number"=seq(length(resid)))
        if (hasconc) {
          xdat <- data.frame(xdat, conc=xc)
		  if (xc.log){
				    xlabelConc <- paste("Log-10", var.name["conc"])
		  } else {
					xlabelConc <- var.name["conc"]
		  }
        }        
        if (hastime) {
          xdat <- data.frame(xdat, time=xt)
          if (xt.log){
			 xlabelTime <- paste("Log-10", var.name["time"])
		  } else {
			 xlabelTime <- var.name["time"]
		  }
        }

      # identify strata
        if(is.null(data.obj$strata.categories))
          snames <- NULL
        else
          snames <- cross(data.obj$strata.categories)
          
windows(rescale = "fit", restoreConsole = FALSE)

  # Expand the top margin to place the legend
  par(mar=par("mar")+c(0,0,1.4,0), mgp=c(2.5,1,0))

#	if(dev.cur() == 1 | !replace) {
#         motif()
#		win.graph()
#	}
	if(is.null(xvar)) {
		items <- desc <- names(xdat)

		## this changes names in pop menu from conc/time to xlabel values. 
             if (length(items) > 1) {
				if (hasconc){
				  concInd <- which(names(xdat)=="conc")
				  desc[[concInd]] <- xlabelConc
				}
				if (hastime){
				   timeInd <- which(names(xdat)=="time")
				   desc[[timeInd]] <- xlabelTime
				}
				
		  pick <- items[menu(desc, graphics = popup, 
			title = "Plot GD residuals versus what?"
			)]
		  index = which(pick == items)[[1]]
		  pick <- names(xdat)[[index]]
		  }
                else pick <- items
		if(is.character(pick) & length(pick) == 0)
			stop(message = 
				"No variable selected.")
      }
      else pick <- xvar
	  
	  # set label based on pick value
	  if (pick == "conc") {
		xlab = xlabelConc
	  }
	  if (pick == "time") {
	    if(!hastime)  stop("Time parameter not specified.  Devplot will not be created.", call.=FALSE)
		xlab = xlabelTime
	  }
	    
      if (is.null(snames)) {
         plot(xdat[, pick], resid, xlab =
           ifelse(is.null(xlab),pick,xlab), ylab = ylab)
	 title(main="Deviance plot", line=3.5)
      }
      else {         
         plot(xdat[, pick], resid, type="n", xlab = 
           ifelse(is.null(xlab),pick,xlab), ylab = ylab)
         title(main="Deviance plot", line=3.5)
         for (i in seq(length(snames))) {
           get <- data.obj$strata.strata==snames[i]
	   # add color to the R version. 08/16/02. CLiu
           points(xdat[get,pick], resid[get], pch=plotsym[i], col= color[i])
         }
#if(exists("xVars")) {
#	lgnd <- FALSE
#} else {
         if(!is.null(zlegend))
# S Plus           legend(zlegend, legend=snames, marks=plotsym, bty=bty)
# use "pch" instead of "marks" in R. 12/27/01 - chliu
	   legend(zlegend, xpd=TRUE, horiz=TRUE, inset=c(0,-0.1), legend=snames, pch=plotsym, bty=bty, col= c(3,4,6,1,5,2,8,7))
         else {
           lgnd <- menu(c("Yes","NO"), graphics=popup,
             title="Legend? If yes, click mouse to locate.")
           lgnd <- ifelse(lgnd==1,TRUE,FALSE)
           if(lgnd) 
	     legend(locator(1), legend=snames, pch=plotsym, bty=bty, col= c(3,4,6,1,5,2,8,7))
         }
#        }
      }

  if(!is.null(devplotEmf))
  {
	savePlot(devplotEmf,type="emf")
	sink(file="Data/plot.rdat", append=TRUE)
    cat(paste(devplotEmf,".emf\n",sep=""))
    sink()
  } else {
	  if(exists("xVars")) {
		savePlot(xVars$info$devplotFName,type="emf")
		sink(file="Data/plot.rdat", append=TRUE)
        cat(paste(xVars$info$devplotFName,".emf\n",sep=""))
        sink()
	  }
  }

	invisible()
}

#########################################################################
# Function "prprob2" is the objective function for computing predicted
# probability as an user option
# 10/29/2003 - changku kang
#########################################################################
prprob <- function( ecline=ECline,
                    replace=FALSE,
                    time=NULL,
                    conc=NULL,
                    log="x",
                    xlog=data.char$xlog,
                    title=NULL,
                    xlim=NULL,
                    ylim=NULL,
                    stratum=NULL,
                    zlegend="top",
                    slabels=NULL,
                    plotsym=NULL,
		    color=NULL,
                    popup=TRUE,
                    jitter=FALSE,
                    prX=NULL,
                    prcurve=TRUE,
                    severity=NULL,
                    xlen=100,
                    namematrix=data$strata.coefnames,
                    xnames=fits$xnames)
{
  
  if (!is.null(time) && !is.null(conc)) {
    cat ("ERROR: Both time & concentration was specified. Specify one or the other.\n")
    invisible()
    return(NULL)
  }

  items <- sort(attributes(ecline$tables)$names)
  pick <- stratum
  if (length(items) > 1 & is.null(pick))
    pick <- items[menu(items,graphics=popup,
                       title="Stratum?")]
  if (is.character(pick) & length(pick) ==0) stop()

# coefficient names corresponding to stratum

  if (length(items) > 1)
    snames <- namematrix[items==pick,]
  else snames <- c(namematrix)

###############################################
# legend

##  if (!is.null(zlegend)) {
##    lgnd <- TRUE
##  } else {
##    lgnd <- menu(c("Yes","No"), graphics=popup,
##    title=
##    "Legend? If yes, click mouse to locate.")
##    lgnd <- ifelse(lgnd==1, TRUE, FALSE)
##  }

# call plotting function




  tmp <- pr2prob(stratum=pick, log=log, xlog=xlog,
           title=title, xlim=xlim, ylim=ylim,
           plotsym=plotsym,slabels=slabels,
           jitter=jitter, time=time, conc=conc,color=color,
           prcurve=prcurve, prX=prX, severity=severity,
           snames=snames, xnames=xnames, xlen=xlen)
  cat("\n")
  out <- data.frame(tmp$xX,tmp$yX)
  names(out) <- c("Concentration levels","Predicted Prob")
  return(out)
}

######################################################################

pr2prob <- function (data=NULL,
                     ecline=NULL,
                     prX=NULL,
                     stratum=NULL,
                     strata=NULL,
                     severity=NULL,
                     log="x",
                     xlog="CT",
                     time=NULL,
                     conc=NULL,
                     title=NULL,
                     bbox=TRUE,
                     alegend=NULL,
                     zlegend="top",
                     xlim=NULL,
                     ylim=NULL,
                     plotsym=NULL,
		     color=NULL,
                     slabels=NULL,
                     jitter=FALSE,
                     prcurve=TRUE,
                     var.name=required,
                     snames=NULL,
                     xnames=NULL,
                     xlen=100)
{

# Do we use user supplied data or do we assume the standard data structures?

  if (!is.null(data))
    data.char <- data

  if (!is.null(ecline))
    ECline <- ecline

  if (!is.null(prX))
    prplotX <- prX
  else {
   if (length(grep("prplotX",objects())) == 0) 
#   if (length(grep("prplotX",objects())) == 0) 
     prplotX <- NULL
  }

  if (is.null(stratum))
    stratum <- strata
  strata <- stratum

# Do the strata... user supplied value or default?

  allStrata <- attributes(ECline$tables)$names
  if (is.null(strata))
    strata <- allStrata
  else {
    valid <- match (strata, allStrata, nomatch=0) == 0

    if (sum(valid) > 0) {
      cat (" These strata are invalid:", strata[valid], "\n")
      invisible()
      return(NULL)
    } 
  }

  if (length(strata) > 1 ) {
    strata <- strata[1]
    cat ("WARNING: Multiple strata - plotting first one:", strata, "\n")
  }

# we pull the relevant data out of the data structure (all strata)

  vconc   <- as.double(data.char$data[,var.name["conc"]])
  vdura   <- as.double(data.char$data[,var.name["time"]])
  y <- list(ylo=as.numeric(data.char$data[,var.name["loscore"]]),
            yhi=as.numeric(data.char$data[,var.name["hiscore"]]))
  nsev   <- max(max(y$ylo),max(y$yhi))

  if (is.null(severity)) {
    severity <- as.integer(substring(strata,4,4))
  }

# Create the vectors & stuff needed for the legend.
  

# We pull the specific Stratum data out of the data structure.

  index <- match (strata, allStrata, nomatch=0)
  get <- rep(TRUE, length=length(vdura)) # Assume all data values are included...
  if (!is.null(ECline$factors)) {
    if (length(ECline$factors) == 1) {
      get <- data.char$data[,ECline$factors] == 
                                  ECline$categories[index]
    }
    else {
      for (j in 1:length(ECline$factors)) {
        get1 <- data.char$data[,ECline$factors[j]] == 
                                  ECline$categories[index,][j]
        get <- get & get1
      }
    }
  }
  get1 <- get

  if (!is.null(time) && !is.null(conc)) {
    cat ("ERROR: Both time & concentration was specified. Specify one or the other.\n")
    invisible()
    return(NULL)
  }

  # we set up our variables to be either time or by default concentration...
 
  if (!is.null(time)) {  # time= specified
    type  <- TRUE
    get2  <- vdura==time
    stype <- "time"
    value <- time
    vplot <- vconc  # our x-axis vector is: vplot = concentration
    yplot <- vdura
#      whichcol <- snames[grep("*CONC*", snames)]
      whichcol <- snames[grep("CONC", snames)]
#      fixcol <- snames[grep("*TIME*", snames)]
      fixcol <- snames[grep("TIME", snames)]
#    islog <- ifelse (length(grep("*C", xlog)) > 0, TRUE, FALSE)  # switched-dgs
    islog <- ifelse (length(grep("C", xlog)) > 0, TRUE, FALSE)  # switched-dgs
#      isflog <- ifelse (length(grep("*T*", xlog)) > 0 , TRUE, FALSE)    
      isflog <- ifelse (length(grep("T", xlog)) > 0 , TRUE, FALSE)    
  }
  else if (!is.null(conc)) {  # conc= specified
    type  <- FALSE
    get2  <- vconc==conc
    stype <- "concentration"
    value <- conc
    vplot <- vdura  # our x-axis vector is: vplot = duration
    yplot <- vconc
#      whichcol <- snames[grep("*TIME*", snames)]
      whichcol <- snames[grep("TIME", snames)]
#      fixcol <- snames[grep("*CONC*",snames)]    
      fixcol <- snames[grep("CONC",snames)]    
#    islog <- ifelse (length(grep("*T*", xlog)) > 0, TRUE, FALSE)  # switched-dgs
    islog <- ifelse (length(grep("T", xlog)) > 0, TRUE, FALSE)  # switched-dgs
#      isflog <- ifelse (length(grep("*C", xlog)) > 0 , TRUE, FALSE)
      isflog <- ifelse (length(grep("C", xlog)) > 0 , TRUE, FALSE)
  }
  else {  # conc & time are null - concentration is on the x-axis
    type  <- TRUE
    get2  <- rep(1,length(vconc))
    stype <- "concentration"
    value <- conc
    vplot <- vconc  # our x-axis vector is: vplot = concentration
    yplot <- vconc
#      whichcol <- snames[grep("*CONC*", snames)]
      whichcol <- snames[grep("CONC", snames)]
      fixcol <- NULL
#    islog <- ifelse (length(grep("*C", xlog)) > 0, TRUE, FALSE)  # switched-dgs
    islog <- ifelse (length(grep("C", xlog)) > 0, TRUE, FALSE)  # switched-dgs
#      isflog <- ifelse (length(grep("*T*", xlog)) > 0 , TRUE, FALSE)  
      isflog <- ifelse (length(grep("T", xlog)) > 0 , TRUE, FALSE)  
  }

  #print(sort(unique(yplot[get1])))   # all possible values for user to choose

  s1 <- sum(get2) # matches for ALL strata
  get <- get & get2
  s2 <- sum(get)  # matches for selected strata

  cat ("\n")
  cat ("Matching data points in ALL strata:", s1, "\n")
  cat ("Matching data points for '", strata, "': ", s2, "\n", sep="")
  cat ("\n")

# error messages...

  if (s1 == 0) {
    cat ("Note: No data value for '", stype, " = ", value, 
         "' exists in ANY stratum.\n\n", sep="");
    cat ("Possible values are:\n\n")
    print(sort(unique(yplot[get1])))
    cat ("\n")
  } 

  if (s2 == 0) {
    cat ("Note: No data value for '", stype, " = ", value, 
         "' exists in stratum: ", strata, ".\n\n", sep="");
    cat ("Possible values are:\n\n")
    print(sort(unique(yplot[get1])))
    cat ("\n")
  }
  

  
#
# Plot the points.
#

  if (s2 !=0 ) {

    logaxis <- ifelse(log=="x", TRUE, FALSE)

    # plot the non-censored points:
 
    q0 <- rep(0.0,length(vplot))
    q1 <- rep(1.0,length(vplot))
    q5 <- rep(0.5,length(vplot))

    for (i in 0:nsev) {
      get1 <- (y$ylo == i & y$yhi == i) & get 

      v1 <- jitterdata (vplot[get1], duplicated(vplot[get1]), 
        jitter, logaxis, xlim, xdiv=10)      
    }

    if (is.null(xlim)) xlim <- c(min(vplot), max(vplot))  
  	if (is.null(ylim)) ylim <- c(-.2, 1.2)
    
  	
    # low interval of censored data > severity --- plot at 1.0
    get1 <- (y$ylo < y$yhi) & (y$ylo > severity) & get
    v1 <- jitterdata (vplot[get1], duplicated(vplot[get1]), 
      jitter, logaxis, xlim, xdiv=12)    
  
    # high interval of censored data < severity --- plot at 0.0
    get1 <- (y$ylo < y$yhi) & (y$yhi < severity) & get
    v1 <- jitterdata (vplot[get1], duplicated(vplot[get1]), 
      jitter, logaxis, xlim, xdiv=10)
    

    # severity level inside censored interval --- plot at 0.5.
    get1 <- (y$ylo < y$yhi) & ((y$ylo <= severity) & (y$yhi >= severity)) & get
    v1 <- jitterdata (vplot[get1], duplicated(vplot[get1]), 
      jitter, logaxis, xlim, xdiv=10)
    
  }

# We calculate the predicted probability.

  if (prcurve == TRUE) {

      sev  <- rep(severity, xlen)

      # generate X & Y

      if (is.null(prX)) {
        prplotX <- makeprX(xlim=xlim,
                           xlen=xlen,
                           whichcol=whichcol,
                           islog=islog,
                           value=value,
                           fixcol=fixcol,
                           isflog=isflog,
                           snames=snames,
                           xnames=xnames)
      }

      yX <- pr.exceed(sev, nsevcat=fits$nsevcat, prplotX$X,
                coefs=fits$coefficients, link=fits$link, model=fits$model, 
                derivs=0, linkscale=1)
      if (islog)
        xX <- 10^prplotX$X[, prplotX$whichcol]
      else
        xX <- prplotX$X[, prplotX$whichcol]
  }  
  return(xX,yX)
}

#####################################################
# Added graph : allplots()
# all severity levels are in one graph ( based on chosen stratum)
# if there is no stratification, just ll severity levels are in one graph 
# 11/10/2005 : GyoungJin Ahn
#########################

allsevsplot <- function(  replace=FALSE
                      , popup=TRUE  , zlegend="top"
                      , var.name=required , title=NULL
                      , bbox=TRUE , alegend=NULL
                      , zzlegend=NULL
                      , plotsym=NULL, color = NULL 
                      , xlim=NULL, ylim=NULL
                      , slabels=NULL
                      , linetype=NULL
                      , Title=NULL
                      , log="y"
                      , plottype=NULL
                      , jitter=FALSE
                      ) {
  windows(rescale = "fit", restoreConsole = FALSE)

# if(dev.cur()==1 | !replace) {
#     motif()
#    win.graph()
#  }
  
  ### Begin : added by GyoungJin Ahn : 1/27/2006
# When zero background in log transformed model, totla risk== extra risk
    if(length(grep("total", ECline$risks, ignore.case=TRUE))>0  && 
        length(grep("gamma", names(fits$coefficients), ignore.case=TRUE)) ==0 ) {
           risk.case <- "extra risk"
    } else   {
       risk.case <- ECline$risks
    }      
# End : addded by GyoungJin Ahn : 1/27/2006
    
# Begin : addded by GyoungJin Ahn : 12/21/2005
# Risk type recognition

   erdans<-ifelse(length(grep("extra", risk.case, ignore.case=TRUE))>0,"Y", "N")
# End : addded by GyoungJin Ahn : 12/21/2005
    
  
   pick <- NULL
  ######  Menu  part   #########
  # ltitle term is used in legend and printed results in  the terminal.
    
    if(!is.null(ECline$factors)) {  
    items <- NULL  
    snames <- attributes(ECline$tables)$names
    nstrat <- length(snames)
    startpoint <- which(unlist(strsplit(snames[1],"")) == ":")[1]
    for(i in 1:nstrat) {  
    items <- c(items, substr(snames[i], (startpoint+1), (nchar(snames[i]) ) ) )
    }
    
  
  if (length(items) > 1 & is.null(pick))
    pick <- items[menu(items,graphics=popup,title="Stratum?")]
  if (is.character(pick) & length(pick) ==0) stop()
  } else {
      pick <- "No Strata"
      nstrat <- 1
  }  
  
if(exists("xVars")) {
	lgnd <- FALSE
} else {  
  if (!is.null(zzlegend)) {
    lgnd <- TRUE
  } else {
    lgnd <- menu(c("Yes","No"), graphics=popup,
    title=
    "Legend? If yes, click mouse to locate.")
    lgnd <- ifelse(lgnd==1, TRUE, FALSE)
  }  
 }
  
  
   y <- list(ylo=as.numeric(data.char$data[,var.name["loscore"]]),
            yhi=as.numeric(data.char$data[,var.name["hiscore"]]))

  nsev   <- max(max(y$ylo),max(y$yhi))

  
  
######  options for plotting   #########
# Define defaults & stuff (strata, line types, etc.)

  if (is.null(linetype))
#    linetype <- 1:nstrat  # Will cycle after 8 line types...
#    linetype <- c(2,1,2:nstrat)  # Will cycle after 8 line types...
    ##### GyoungJin Ahn 11/10/2004 ####
    linetype <- c(1,2:nsev)  # Will cycle after 8 line types...
    ###### GyoungJin Ahn end 
 
  
# Create the vectors & stuff needed for the legend.

  if (is.null (slabels)) {
    if (nsev == 1)
      slabels  <- c("No effect", "Severe", "Censored")
    else if (nsev == 2)
      slabels  <- c("No effect", "Adverse", "Severe", "Censored")
    else
      slabels  <- c("No effect", paste(rep("Severity",nsev),1:nsev), "Censored")
  }

    if (is.null(plotsym)) { 
    if (nsev == 1) 
      plotsym <- c(1, 17, 4)
    else if (nsev == 2)
      plotsym <- c(1, 2, 15, 4)
    else if (nsev == 3)
      plotsym <- c(1, 2, 15, 18, 4)
    else if (nsev == 4)
      plotsym <- c(1, 2, 5, 15, 18, 4)
    else if (nsev == 5)
      plotsym <- c(1, 2, 5, 9, 18, 15, 4)
    else if (nsev == 6)
      plotsym <- c(1, 2, 5, 0, 9, 18, 15, 4)
    else if (nsev >= 7) # breaks down if you have more then 15 categories...
# 11/02/2005  Modified by GyoungJin Ahn 
# original :     6:6+nsev-7   => doesn't work
      plotsym <- c(1, 2, 5, 0, seq(6,6+nsev-7), 17, 18, 15, 4)
  } 

  # add for colors
  # cliu. 05/16/02
  if (is.null(color)) { 
    if (nsev == 1) 
      color <- c(3, 4, 6)
    else if (nsev == 2)
      color <- c(3, 4, 6, 17)
    else if (nsev == 3)
      color <- c(3, 4, 6, 17, 21)
    else if (nsev == 4)
      color <- c(3, 4, 6, 17, 21, 23)
    else if (nsev == 5)
      color <- c(3, 4, 6, 17, 21, 23, 25)
    else if (nsev == 6)
      color <- c(1, 2, 5, 3, 17, 18, 15, 4)
    else if (nsev >= 7) # breaks down if you have more then 15 categories...
# 11/02/2005  Modified by GyoungJin Ahn 
# original :     6:6+nsev-7   => doesn't work
      color <- c(1, 2, 5, 3, seq(6,6+nsev-7), 17, 18, 15, 4)
  }  

  if (bbox == TRUE) # bounding box?
    bty <- "y"
  else
    bty <- "n"

   
  if (is.null(plottype))
    if (nsev <= 8) {
      plottype <- "l"  # use lines only if we can...
    } else
      plottype <- "b"  # otherwise use lines & points
    
    
   ylab <- paste("Dose (", var.name["conc"], ")", collapse="")
   xlab <- paste("Time (", var.name["time"], ")", collapse="")
   
   
# We create the main title being as informative as we can.

  if (is.null(Title)) {
  # add title for ERD-T
  # CLiu. 02/26/02
  
  #### Modified by GyoungJin Ahn : 1/27/2006 : 
  #### ERD -> ERC and EC just stays.        
    if (erdans=="y" || erdans=="Y")
       ttt <- "ERC"
    else
       ttt <- "EC"
  }    
            
       Title <- paste (ttt, ECline$pct, " Lines (", pick, ") in all SEV levels", sep="")
   
   
# We pull the data out of the data structure.   
  conc <- as.double(data.char$data[,var.name["conc"]])
  dura <- as.double(data.char$data[,var.name["time"]])
 
    

# Now we plot the strata lines & points. We pull the data for this
# strata out of the data set. There has got to be a better way...

  get <- rep(TRUE, length=length(dura)) # Assume all data values are included...
  index <- grep(pick,  attributes(ECline$tables)$names)
  if (!is.null(ECline$factors)) {
    if (length(ECline$factors) == 1) {
      get <- data.char$data[,ECline$factors] == 
                                  ECline$categories[index]
    }
    else {
      for (j in 1:length(ECline$factors)) {
        get1 <- data.char$data[,ECline$factors[j]] == 
                                  ECline$categories[index,][j]
        get <- get & get1
      }
    }
  }
  get1 <- get


    
  dataforplots <- NULL
  maxy <- NULL
  miny <- NULL
  for (i in 1:nsev) {
      tmp     <-  ectable(severities=i,
                  times=seq(data$time.range[1],data$time.range[2],
                    length=20),
                  data$strata.coefnames,
                  data$strata.factors,
                  data$strata.categories,
                  coefs0=fits$coefficients,
                  sand.var0=fits$sand.var,
                  link=fits$link,
                  model=fits$model,
                  q=as.numeric(ECline$pct)/100,
                  risks=ECline$risks,
                  xlog=data.char$xlog)$tables
      if(nstrat >1)         # no stratification
      tmp <- tmp[grep(pick, names(tmp))]
      maxy <- c(maxy, max(tmp[[1]][,2]))
#####  11/21/2005 : GyoungJin Ahn : although there are negative or (/and) zero values in plot data,
##### make the plot   
### original before 11/21/2005 :   miny <- c(miny, min(tmp[[1]][,2])

# Modified by GyoungJin Ahn : 02/22/2006              
#  When making allsevsplot
#  Infinite, NaN("Not a Number" (R value)), NA("Missing value") 
# are excluded from the graph.

# Begin : deleted by GyoungJin Ahn : 02/22/2006
#      miny <- c(miny, min(tmp[[1]][,2][tmp[[1]][,2]>0]))
# End: deleted by GyoungJin Ahn : 02/22/2006

# Begin : Added by GyoungJin Ahn : 02/22/2006
       tmp.tmp <- tmp[[1]][,2]
       if(length(tmp.tmp[ (is.finite(tmp.tmp))][tmp.tmp[ (is.finite(tmp.tmp))] >0]) >0) {
          miny <- c(miny, min(tmp.tmp[ (is.finite(tmp.tmp))][tmp.tmp[ (is.finite(tmp.tmp))] >0]))
       } else {
          miny <- c(miny, NULL)      
       }   
# End: Added by GyoungJin Ahn : 02/22/2006                                      

      dataforplots <- c(dataforplots, tmp)
   }     
   
   
  
   
######   plotting part : Line ######
#####  11/21/2005 : GyoungJin Ahn : although there are negative or (/and) zero values in plot data,
##### make the plot    
#### Original before 11/21/2005
#    if(is.null(ylim))  ylim <- c(min(min(conc), min(miny)), max(max(conc), max(maxy))) 

    
#####  11/21/2005 : GyoungJin Ahn : although there are negative or (/and) zero values in plot data,
##### make the plot   
    if(is.null(xlim)) xlim <- c(min(dura),max(dura))

# Modified by GyoungJin Ahn : 02/22/2006              
#  When making allsevsplot
#  Infinite, NaN("Not a Number" (R value)), NA("Missing value") 
# are excluded from the graph.
 
# Begin : deleted by GyoungJin Ahn : 02/22/2006
#    if(is.null(ylim)) ylim <- c(min(min(conc[conc>0]), miny), max(max(conc), max(maxy))) 
# End: deleted by GyoungJin Ahn : 02/22/2006

                                      
# Begin : Added by GyoungJin Ahn : 02/22/2006
    if(is.null(ylim)) ylim <- c(  min(  min(conc[conc>0])
                                      , miny[ (is.finite(miny))][miny[ (is.finite(miny))] >0])
                                , max(  max(conc)
                                      , maxy[ (is.finite(maxy))][maxy[ (is.finite(maxy))] >0])) 
# End: Added by GyoungJin Ahn : 02/22/2006                                      
                                        
    
    
    
   
    par(err=-1)
    plot (dura, conc, log=log, type="n", xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab,main=Title)
  
    tmp.max <- NULL
    
    for (i in 1:nsev) {
    lines (x=dataforplots[[i]][,1], y=dataforplots[[i]][,2], type=plottype
           , lty=linetype[i],lwd=1.5, pch=plotsym[i])

      tmp.max  <- c(tmp.max ,max(dataforplots[[i]][,2]))
    }
    
    
    
    

###### We draw the points (currently we have 4 possible effects).

#  y <- designy (data.char$data)
# modified 6-27-95 DGS
# y <- list(ylo=as.numeric(data.char$data[,var.name["loscore"]]),
#            yhi=as.numeric(data.char$data[,var.name["hiscore"]]))

  ### jitter the data

  dups <- duplicated(paste(dura[get],conc[get]))
#  logaxis <- ifelse (length(grep("*x*", log, value=TRUE)), TRUE, FALSE)
  logaxis <- ifelse (length(grep("x", log, value=TRUE)), TRUE, FALSE)
  dura[get] <- jitterdata (dura[get], dups, jitter, logaxis, xlim)
#  logaxis <- ifelse (length(grep("*y*", log)), TRUE, FALSE)
  logaxis <- ifelse (length(grep("y", log)), TRUE, FALSE)
  conc[get] <- jitterdata (conc[get], dups, jitter, logaxis, ylim)

  ### plot the points (case by case)

  cat ("\nPoints plotted:\n\n")

  get <- (y$ylo==0 & y$yhi==0) & get1 # no effect
  
# 11/05/2005 GyoungJin Ahn : col part is modified   
  points (dura[get],conc[get],pch=plotsym[1], col=color[1])
  cat ("  ", slabels[1], ":\t", sum(get), "\n", sep="")
  cx <- dura[get]
  cy <- conc[get]
# Added by GyoungJin Ahn : 04/21/2006 
  cz <- y$ylo[get]

  
  
  for (i in 1:nsev) {
    get <- (y$ylo==i & y$yhi==i) & get1 # severe effects
# 11/05/2005 GyoungJin Ahn : col part is modified   
    points (dura[get],conc[get],pch=plotsym[i+1], col=color[i+1])
    cat ("  ", slabels[i+1], ":\t", sum(get), "\n", sep="")
    cx <- c(cx, dura[get])
    cy <- c(cy, conc[get])
# Added by GyoungJin Ahn : 04/21/2006 
     cz <- c(cz, y$ylo[get])
    
  }
 
  get <- (y$ylo<y$yhi) & get1 # censored

  # 11/05/2005 GyoungJin Ahn : col part is modified   
  points (dura[get],conc[get],pch=plotsym[nsev+2], col=color[nsev+2])
  cat ("  ", slabels[nsev+2], ":\t", sum(get), "\n", sep="")
  cx <- c(cx,dura[get])
  cy <- c(cy,conc[get])
# Added by GyoungJin Ahn : 04/21/2006 
     cz <- c(cz, rep("C",sum(get)))
  
 
# Figure out how many points are "hidden"
# Modified by GyoungJin Ahn : 04/21/2006 
# original :  xy  <- paste(cx,cy)
 
  xy  <- paste(cx,cy,cz)
  xyn <- length(xy)
  xyu <- length(unique(xy))

  cat ("\n")
  cat ("  Total:\t", xyn, "\n", sep="")
  cat ("  Hidden:\t", xyn-xyu, "\n", sep="")
  cat ("\n")
    
 # We draw the zlegend...
 
  if (lgnd) {
#    legend.notice ()
# S Plus    legend(zlegend, legend=slabels, marks=plotsym, bty=bty)
# marks is unused arguments in R, use pch instead. 12/27/01 - cliu
     zzlegend <- locator(1)
     linetype <- c(0, linetype, 0)
     legend(zzlegend, legend=slabels, pch=plotsym, col=color, bty=bty, lty=linetype )
  }
################################################################
# Modified by GyoungJin Ahn : 03/02/2006              
#  When making graphs(catplot, stratplot, allsevsplot, confplot ),
#  Show some footnotes for Infinite, NaN("Not a Number" (R value)), NA("Missing value") 
# on the screen. 
# When ec3table is used, the same footnotes will be shown in the screen
################################################################
# begin : Added by GyoungJin Ahn : 03/02/2006
  if(!is.finite(max(tmp.max))) {
}                    
# end : Added by GyoungJin Ahn : 03/02/2006        

  
  
#####  11/21/2005 : GyoungJin Ahn : although there are negative or (/and) zero values in plot data,
##### make the plot   
  #### Modified by GyoungJin Ahn : 1/27/2006 : 
  #### ERD -> ERC and EC just stays.        
    if (erdans=="y" || erdans=="Y") {
      if(!is.finite(max(tmp.max))) {
         cat("\n", "Note:\n\n", "1. Infinite ERC values and zero concentrations are not shown in the graph.", "\n")
     tmp6 <- paste( ": \"Inf\", \"NA\" and \"NaN\" indicates that ERC was not calculated "
                  ,"\nbecause estimated background risk "
                  ,"( i.e., probability of response at zero concentration) \nexceeds an inordinately high value of 0.9."
                  , sep="")
     cat(tmp6, "\n\n")
     title(sub=paste("1. ERC is not calculated when background risk exceeds an inordinately high value of 0.9.",
            sep=""), cex.sub=0.8, col.sub="red")
     
      } else {
         cat("\n", "Note:\n\n", "1. Zero concentrations are not shown in the graph.", "\n", "\n")
      }
    } else {
      if(!is.finite(max(tmp.max))) {
         cat("\n", "Note:\n\n","1. Infinite EC values and zero concentrations are not shown in the graph.", "\n")
    tmp6 <- paste("\n ** \"Inf\", \"NA\" and \"NaN\" indicates that EC was not calculated "
                  ,"\nbecause estimated background risk "
                  ,"( i.e., probability of response at zero concentration) \nexceeds an inordinately high value of 0.9."
                  , sep="")
     cat(tmp6, "\n\n")
     title(sub=paste("1. EC is not calculated when background risk exceeds an inordinately high value of 0.9.",
            sep=""), cex.sub=0.8, col.sub="red")
         
      } else {
         cat("\n", "Note:\n\n", "1. Zero concentrations are not shown in the graph.", "\n", "\n")
      }
    }
  
  
  
  
  invisible()
   
   
}


# Front end for plotting probability curves
# for individual strata.

barPRPlot_all <- function(xaxis) {
  items <- sort(attributes(ECline$tables)$names)
  
  cat(items)
  
  lapply(items, barPRPlot, grayscale=xVars$info$grayscale, xaxis=xaxis)
 
}


# Objective: Generate 2D normalized barchart for response vs. dose*time
# Software project: CatReg
# Author: Ilya Balabin balabin.ilya@epa.gov
# Modified by: Cody Simmons

barPRPlot <- function(
  strata,
  grayscale = TRUE,
  barplotEmf=NULL,
  namematrix=data$strata.coefnames,
  zlegend = "top",
  xlog=data.char$xlog,
  var.name=required,
  severity=NULL,
  strataProbs = NULL,
  xnames=fits$xnames,
  xaxis="dose",
  DEBUG=FALSE
) {

items <- sort(attributes(ECline$tables)$names)
  if (length(items) > 1) {
    snames <- namematrix[items==strata,]
  }else{ 
    snames <- c(namematrix)
	}
  
  allStrata <- attributes(ECline$tables)$names
  
  valid <- match (strata, allStrata, nomatch=0) == 0

  if (sum(valid) > 0) {
    cat (" These strata are invalid:", strata[valid], "\n")
  invisible()
    return(NULL)
  } 
  
  if (length(strata) > 1 ) {
    strata <- strata[1]
    cat ("WARNING: Multiple strata - plotting first one:", strata, "\n")
  }
  
  # we pull the relevant data out of the data structure (all strata)
  vconc   <- as.double(data.char$data[,var.name["conc"]])
  vdura   <- as.double(data.char$data[,var.name["time"]])
  y <- list(ylo=as.numeric(data.char$data[,var.name["loscore"]]),
            yhi=as.numeric(data.char$data[,var.name["hiscore"]]))
  nsev   <- max(max(y$ylo),max(y$yhi))
  
  if (is.null(severity)) {
    severity <- as.integer(substring(strata,4,4))
    
  }
  
  # We pull the specific Stratum data out of the data structure.
  
  if(!is.null(ECline$factors) && length(ECline$factors) == 1){
    ttt <- unlist(strsplit(strata,":"))[2]
    index <- match (ttt, ECline$categories, nomatch=0)
  } else 
  index <- match (strata, allStrata, nomatch=0)
  
  # if(!is.null(ECline$factors)){
    # ttt <- unlist(strsplit(strata,":"))[2]
    # index <- match (ttt, ECline$categories, nomatch=0)
  # } else 
  # index <- match (strata, allStrata, nomatch=0)

  get <- rep(TRUE, length=length(vdura)) # Assume all data values are included...
  if (!is.null(ECline$factors)) {
    if (length(ECline$factors) == 1) {
      get <- data.char$data[,ECline$factors] == 
                                  ECline$categories[index]
    }
    else {
      for (j in 1:length(ECline$factors)) {
        get1 <- data.char$data[,ECline$factors[j]] == 
                                  ECline$categories[index,][j]
        get <- get & get1
      }
    }
  }
  get1 <- get
  
  #data for this strata is vdura[get1] and vconc[get1]
  
  #windows(rescale = "fit", restoreConsole = FALSE)

  #all data
  mydata <- data.char$data
  
  #just data for current strata (all severities)
  mydata <- mydata[get1,]
  
  if (DEBUG)
    message(paste0("Read ", nrow(mydata), " rows of ", ncol(mydata),
                  " columns from ", wd, "/", response_data_file))
  
  # find unique concentrations
  colnames <- colnames(mydata)      # dirnames generates too much output
  concname <- xVars$info$conc               # colnames quietly replaces "/" with "."
  try(concname %in% colnames)
  concs <- mydata[, concname]       # whymydata$concname does not do, why?!
  concs <- unique(concs[order(concs)])
  concs <- sort(as.numeric(concs))
  if (DEBUG) {
    message(paste("Found", length(concs), "unique concentrations: "),
            appendLF=FALSE)
    cat(concs, "\n")
  }
  
  # for each concentration, find unique exposure time
  timename <- xVars$info$time
  try(timename %in% colnames)
  hours <- c()
  for (conc in concs) {
    hrs <- (mydata[as.numeric(mydata[, concname])==conc, timename])
    hrs <- unique(hrs[order(hrs)])
	hrs <- sort(as.numeric(hrs))
    hours <- c(hours, list(hrs))
  }
  if (DEBUG) {
    message("Found unique hours for each concentration:")
    for (i in 1:length(concs)) {
      message(paste("  Concentration", concs[i], "\b: "), appendLF=FALSE)
      cat(hours[[i]], "\n")
    }
  }
  
  # sanity check
  stopifnot(length(hours)==length(concs))
  
  # find unique severity levels
  sevname <- xVars$info$loscore
  try(sevname %in% colnames)
  # sevlevels <- mydata[, sevname]
  sevlevels <- data.char$data[, sevname]
  sevlevels <- unique(sevlevels[order(sevlevels)])
  if (DEBUG)
    message(paste("Observed", length(sevlevels), "severity levels: "),
            appendLF=FALSE)
    cat(sevlevels, "\n")

    # for each sevlevel/concentration/time, find incidents
    incname <- xVars$info$incid
    incids <- c()
	

    for (sev in sevlevels) {
      for (j in 1:length(concs)) {
        conc <- concs[j]
        hrs <- hours[[j]]
        for (k in 1:length(hrs)) {
          h <- hrs[[k]]
          s <- sum(as.numeric(mydata[as.numeric(mydata[, concname]) == conc
                          & as.numeric(mydata[, timename]) == h
                          & as.numeric(mydata[, sevname]) == sev, incname]))
          incids <- c(incids, s)
        }
      }
    }
	
	# generate flattened values for sorting
	concList <- c()
	timeList <- c()
	for (j in 1:length(concs)) {
      hrs <- hours[[j]]
      for (k in 1:length(hrs)) {
	    concList <- c(concList, concs[j])
	    timeList <- c(timeList, hrs[[k]])
      }
    }
    
    # convert incids to matrix
    incids <- matrix(incids, nrow=length(sevlevels), byrow=TRUE)
	
	#determine corrected order
	if (xaxis == "time"){
	  #already ordered by dose, now just order by time to get correct indices
	  correctedOrder <- order(as.numeric(timeList))
	  concList<-concList[correctedOrder]
	  timeList<-timeList[correctedOrder]
	  incids <- incids[,correctedOrder]
	} else if (xaxis == "product") {
	  correctedOrder <- order(as.numeric(concList)*as.numeric(timeList))
	  concList<-concList[correctedOrder]
	  timeList<-timeList[correctedOrder]
	  incids <- incids[,correctedOrder]
	}
	
	if(DEBUG)
      message(paste("Converted", length(incids), "incident counts to",
                    nrow(incids), "x", ncol(incids), "matrix for plotting"))
	
	# save total number of incidents at each strata
	totIncids <- colSums(incids)
	
	#normalize incids
	incids<- scale(incids, center=FALSE, scale=colSums(incids))
	
	#reverse order of matrix
	incids <- incids[ nrow(incids):1, ]

    
    # legends
	nsl <- length(sevlevels)
	
	if (match("0", sevlevels, nomatch=0)==0) nsl <- nsl + 1
	
	
	if (nsl == 1){
	  legends <- c("No effect", "Severe")
	}else if (nsl == 2) {
	  legends <- c("No effect", "Adverse", "Severe")
	}else{ 
	  legends  <- c("No effect", paste(rep("Severity",nsl-1),2:nsl-1))
	  }
    
    # bar colors (from 0.2 to 0.9 of the rainbow scale)
    
    nbeg <- 0.10 ;# green, lowest severity level
    nend <- 0.90 ;# magenta, highest severity level
	
	if(grayscale){
	  nfrags <- 200 ;# grayscale gradations
	  allcolors <- gray.colors(nfrags)
	}else {
	  nfrags <- 100 ;# color gradations
	  allcolors <- rainbow(nfrags)
	}
	
	
	
    cidx <- seq(1, nsl) * nfrags / nsl * (nend - nbeg) + nbeg * nfrags
    colors <- c()
    for (i in cidx) colors <- c(colors, allcolors[i])
	#reverse order of colors for bars since matrix has been reversed
	legcolors <- colors  #leave colors for legend in original order
	colors <- colors[length(colors):1]
	if(DEBUG)
       message(paste("Generated", length(colors), "bar colors"))

	   
	# check if constant time value for all data points
	# if no time column is  mapped, the time values will be constant (=1)
	timeCol <- mydata[,xVars$required["time"]]
	constTime <- isTRUE(all.equal(timeCol, rep(timeCol[1], length(timeCol))))
	   
    # bar names
    barnames <- c()
    # for (i in 1:length(concs)) {
      # for (j in 1:length(hours[[i]])) {
	    # if (xaxis == 'dose'){
           # barnames <- c(barnames, 
                      # toString(paste(concs[i], "|", hours[[i]][[j]])))
		# } else if (xaxis == "time") {
		   # barnames <- c(barnames, 
                      # toString(paste(hours[[i]][[j]], "|", concs[i])))
		# } else {
		   # barnames <- c(barnames, 
                      # toString(as.numeric(concs[i]) * as.numeric(hours[[i]][[j]])))
		# }
      # }
    # }
	
	if (!constTime){
	   if (xaxis == "dose"){
	      for (i in 1:length(concList)){
	         barnames <- c(barnames, toString(paste(concList[i], "|", timeList[i])))
	      }
	   } else if (xaxis == "time"){
	      for (i in 1:length(concList)){
	         barnames <- c(barnames, toString(paste(timeList[i], "|", concList[i])))
	       }
	   } else {
	      # for (i in 1:length(concList)){
	      # barnames <- c(barnames, toString(as.numeric(concList[i])* as.numeric(timeList)))
	      # }
	      barnames <- as.numeric(concList) * as.numeric(timeList)
	   }
	} else {
	   for (i in 1:length(concList)){
	      barnames <- c(barnames, toString(concList[i]))
	   }
	}
	
	if(!is.null(barplotEmf))
	{
	    cat("barplotEmf is not null\n")
		barplotEmf=paste(barplotEmf,gsub(":","-",strata),sep="-")
		png(filename=barplotEmf,
		     type="cairo", width=1280, height=1280,units="px", pointsize=22,bg="white", res=NA)
		# savePlot(barplotEmf,type="emf")
		sink(file="Data/plot.rdat", append=TRUE)
		cat(paste(barplotEmf,".png\n",sep=""))
		sink()
	} else {
	  if(exists("xVars")) {
	    cat("barplotEmf is null\nUsing xVars\n")
		png(filename=paste(paste(xVars$info$barplotFName,gsub(":","-",strata),sep="-"),".png",sep=""),
		     type="cairo", width=1280, height=1280,units="px", pointsize=26,bg="white", res=96, "gray")
		# savePlot(paste(xVars$info$barplotFName,gsub(":","-",strata),sep="-"),type="emf")
		sink(file="Data/plot.rdat", append=TRUE)
        cat(paste(paste(xVars$info$barplotFName,gsub(":","-",strata),sep="-"),".png\n",sep=""))
        sink()
	  }
	}
    
    # bar plot
    par(xpd = TRUE, mar = c(5.5, 3.5, 4.1, 4.1))
    bp <- barplot(
      incids,
	  ylim = c(0,1),
      #cex.main = 1.25,
      # axisnames=FALSE,
      # names.arg = barnames,
      # xlab = "",
      # ylab = "",
      # las = 3,
      border = TRUE,
      # beside = FALSE,
      col = colors,
      space = 0.25,
      xpd = TRUE,
      bty = 'L'
    )
	
	#title
	mainTitle = "Observed vs. Predicted"
	if (length(sort(attributes(ECline$tables)$names)) > 1){
	   mainTitle = paste(mainTitle," for Strata = ", paste(unlist(strsplit(strata,":"))[-c(1)], collapse=':'))
	}
	# mainTitle = paste("Observed vs. Predicted for strata = ", paste(unlist(strsplit(strata,":"))[-c(1)], collapse=':'))
	title(main=paste(strwrap(mainTitle, width=60), collapse="\n"), line=3.2)
    
    # tilt bar labels
    text(bp, par("usr")[3], labels = barnames, srt = 45, adj = c(1.1,1.8),
         xpd = TRUE, cex=.9)
	axis(side = 1, at=bp, labels = FALSE)
	box()
	
	shapeChoices <- c(21, 22, 23, 24, 25, 4, 7, 8, 9, 10, 11, 12, 13, 14)
	# define shapes for legend, points
	nsevs <- length(xVars$info$sevsLevel)
	# if (nsevs < 4){
	  # legendShapes <- c (21 : (21 + nsevs)) 
	# } else {
	  # legendShapes <- rep(21, nsevs)
	# }
	if (nsevs < 15){
	  legendShapes <- shapeChoices[1 : (1 + nsevs)] 
	} else {
	  legendShapes <- rep(1, nsevs)
	}
    
    # axes and axis labels
	if (!constTime){
	   if (xaxis == 'dose'){
          title(xlab=paste("Dose (", xVars$required["conc"] ,") | Time (", xVars$required["time"], ")"), line=4, cex.lab=1.25)
	   } else if (xaxis == 'time'){
	      title(xlab=paste("Time (", xVars$required["time"] ,") | Dose (", xVars$required["conc"], ")"), line=4, cex.lab=1.25)
	   } else {
	      title(xlab=paste("Dose (", xVars$required["conc"] ,") * Time (", xVars$required["time"], ")"), line=4, cex.lab=1.25)
	   }
	} else {
	   title(xlab=paste("Dose (", xVars$required["conc"] ,")"), line=4, cex.lab=1.25)
	}
    title(ylab="Probability", line=2, cex.lab=1.25)
    # legends (control layout using inset, text,width, and y.intersp)
    # legend(zlegend, xpd=TRUE, horiz=TRUE, inset=c(0, -0.1), legend=legends, 
           # col=colors, pch=22, pt.bg=colors, text.width=5., y.intersp=0.5,  
            # bty='y', box.col="white", bg="white")
	legend(zlegend, xpd=TRUE, horiz=TRUE, inset=c(0, -0.1), legend=legends, 
           col=legcolors, pch=legendShapes, pt.bg=legcolors)
		   
		   
		   
    	#ADD LOOP OVER SEV LEVELS
    for (theSev in 1:length(xVars$info$sevsLevel))	{
	  strataProbs=NULL
	  xVars$info$sevs = xVars$info$sevsLevel[theSev]
      source("ecdata.R")
	
	
    #loop through all times/concs for this strata to get probability
	uniqConc <- unique(vconc[get1])   
	
	for (i in 1:length(uniqConc)){
	  value <- uniqConc[i]
	  get2  <- vconc==value
      stype <- "concentration" 
      vplot <- vdura  # our x-axis vector is: vplot = duration
      yplot <- vconc
	  whichcol <- snames[grep("TIME", snames)]
	  fixcol <- snames[grep("CONC",snames)]
	  islog <- ifelse (length(grep("T", xlog)) > 0, TRUE, FALSE)  # switched-dgs
	  isflog <- ifelse (length(grep("C", xlog)) > 0 , TRUE, FALSE)
	  s1 <- sum(get2) # matches for ALL strata
      get <- get1 & get2
      s2 <- sum(get)  # matches for selected strata
	  
	  
	  xlen<-length(unique(vdura[get]))
	  sev  <- rep(xVars$info$sevs, xlen)
      xlim <- c(min(vplot[vplot>0]), max(vplot[vplot>0]))
	  
	  #####################################################################
	    #From makeprX

		icol <- snames[grep("INTERCEPT", snames)]

		X <- matrix(0, nrow = xlen, ncol = length(dimnames(data$x)[[2]][-1]))
		dimnames(X) <- list(paste("[", seq(nrow(X)), ",]", sep = ""), dimnames(
                   data$x)[[2]][-1])
				   
		if(length(xnames[dimnames(data$x)[[2]][-1] == icol]) > 0)
         X[, icol] <- rep(1, xlen)
	
	    ## NEED TO ADD GAMMA ADJUSTMENTS IF NECESSARY
		if (islog) {
		   #if(length(grep("CONC", whichcol)) >0 && length(grep("Gamma", names(fits$coefficients))) >0)
		   #   X[, whichcol] <- log10(unique(vdura[get])
		   #else
		      X[, whichcol] <- log10(unique(vdura[get]))
		} else{
		   X[, whichcol] <- unique(vdura[get])
		   }
		
		
		## NEED TO ADD GAMMA ADJUSTMENTS IF NECESSARY
		if (!is.null(fixcol)) {
			if (isflog) {
			  #if(length(grep("CONC", fixcol)) >0 && length(grep("Gamma", names(fits$coefficients))) >0)
			  #  X[, fixcol] <- rep(log10(value), xlen)   
			  #else
			    X[, fixcol] <- rep(log10(value), xlen)   
			}  else {
			X[, fixcol] <- rep(value, xlen)
			}
		}
  
  
		prplotX <- list(X=X, whichcol=whichcol)
		
		
				    yX <- pr.exceed(sev, nsevcat=fits$nsevcat, prplotX$X,
                coefs=fits$coefficients
                , link=fits$link, model=fits$model, 
                derivs=0, linkscale=1)
		#		}
				
			if (islog){
		### Modified by GyoungJin Ahn : 12/15/2005
		### When Log transformation with gamma(especially, log transformation to Concentration)
		###  is used, the gamma value needs to be concerned 
		# Begin added part : GyoungJin Ahn : 12/15/2005
		#		if(length(grep("CONC", whichcol)) >0 && length(grep("Gamma", names(fits$coefficients))) >0 )   
		#		xX <- 10^prplotX$X[, prplotX$whichcol]-fits$coefficients["Gamma"]
		#		else 
				xX <- 10^prplotX$X[, prplotX$whichcol]
			}  else {
				xX <- prplotX$X[, prplotX$whichcol]
			}
			
		
	  #organize probability values
	  if (!is.null(strataProbs)){
	     tempFrame <- data.frame(rep(value, xlen), unique(vdura[get]), yX)
		 names(tempFrame) <- c("Conc", "Time", "Prob")
	     strataProbs <- rbind(strataProbs, tempFrame  )
	  } else {
		 strataProbs <- data.frame(rep(value, xlen), unique(vdura[get]), yX)
	     names(strataProbs) <- c("Conc", "Time", "Prob")
	  }
			
	  #############################################################
	}
	
	#reorder if necessary
	if (xaxis!="dose"){
	  strataProbs <- strataProbs[correctedOrder,]
	}
	
	#plot probabilities on graph
	points(x=bp, y=strataProbs$Prob, pch=legendShapes[theSev+1])
	
	
	# calculate standard error
	sevRow <- nsev + 1 - theSev
	# se <- sqrt(1/totIncids * incids[sevRow,]*(1-incids[sevRow,]))
	# if (theSev == max(xVars$info$sevsLevel)) {
	   # se <- sqrt(1/totIncids * incids[sevRow,]*(1-incids[sevRow,]))  #this is max severity so just use this row
	# } else {
	   # use sum of incidences for all rows with <= this severity
	   tmp <- incids[1:sevRow,]
	   if (!is.null(dim(tmp))){
	      tmpIncids <- colSums(tmp)
	   } else {  # only one row
		  tmpIncids <- tmp
	   }
	      se <- sqrt(1/totIncids * tmpIncids*(1-tmpIncids))
	# }
	seHi <- se
	seLo <- se
	
	#adjust if greater than 1 or less than 0 so that error bars don't extend past plot
	for (loc in 1:length(bp)){
	   if (strataProbs$Prob[loc] + seHi[loc] > 1.0){
	      seHi[loc] <- 1.0 - strataProbs$Prob[loc]
	   }
	   if (strataProbs$Prob[loc] - seLo[loc] < 0.0){
	      seLo[loc] <- strataProbs$Prob[loc]
	   }
	   
	   # set 0 values to NA to avoid warnings for error bars/arrows
	   if (seHi[loc] == 0.0){
	      seHi[loc] <- NA
	   }
	   if (seLo[loc] == 0.0){
	      seLo[loc] <- NA
	   }
	}
	
	#plot error bars
	segments(bp, strataProbs$Prob - seLo, bp, strataProbs$Prob + seHi, lwd=1.5)
	arrows(bp, strataProbs$Prob - seLo, bp, strataProbs$Prob + seHi, lwd=1.5, angle=90, code=3, length=0.05)
	
	} ## end loop over sev levels
	
	dev.off()
	
	# if(!is.null(barplotEmf))
	# {
	    # cat("barplotEmf is not null\n")
		# barplotEmf=paste(barplotEmf,gsub(":","-",strata),sep="-")
		# savePlot(barplotEmf,type="emf")
		# sink(file="Data/plot.rdat", append=TRUE)
		# cat(paste(barplotEmf,".emf\n",sep=""))
		# sink()
	# } else {
	  # if(exists("xVars")) {
	    # cat("barplotEmf is null\nUsing xVars\n")
		# savePlot(paste(xVars$info$barplotFName,gsub(":","-",strata),sep="-"),type="emf")
		# sink(file="Data/plot.rdat", append=TRUE)
        # cat(paste(paste(xVars$info$barplotFName,gsub(":","-",strata),sep="-"),".emf\n",sep=""))
        # sink()
	  # }
	# }
    
}   