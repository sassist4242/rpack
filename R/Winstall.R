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
#source("winplot.R")
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
### Install the data conversion utilities
#source("util.R")
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

