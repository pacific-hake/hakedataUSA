catchPlot.fn <- function(x,y,plotType=c("default","proportion","cumulative"),...) {
    #x is period 
    #y is catch
    if(plotType[1]=="proportion") {
        y <- cumsum(y)/sum(y)
    }
    if(plotType[1]=="cumulative") {
        y <- cumsum(y)
    }
    xx <- 1:12
    yy <- rep(NA,12)
    for(i in xx) {
        if(any(x==i)) { #is the month present in the data
            yy[i] <- y[which(x==i)]
        } else {
            if(i == 1) { 
                yy[i] <- 0 
            } else {
                if(plotType[1]=="default") {
                    yy[i] <- 0
                } else {
                    yy[i] <- yy[i-1]
                }
            }
        }
    }

    lines(xx,yy,...)
}


plotHakeCatchMonthYear.fn <- function(dat,Yrs=range(dat$year),quotas=NULL,lineWds,lineTypes,cols,leg.cex=1,divisor=1000,plotNum=1:4) {
#Take hake dataframe of Fleet, Month, Year, MT and plots year specific catches by month
#Does not discriminate by fleet
#Assumes that there is one observation of Month in each year

    dat.yr <- split(dat,dat$year)
    maxYlim <- max(unlist(lapply(dat.yr,function(x){max(x$catch)}))[Yrs],
        na.rm = TRUE)/divisor
    if(1 %in% plotNum) {
        plot(1,1,xlab="Month",ylab=paste("Catch\n(",divisor," MT)",sep=""),xlim=c(1,12),ylim=c(0,maxYlim),type="n", xaxt = "n")
        for(i in 1:length(Yrs)) {
            catchPlot.fn(dat.yr[[Yrs[i]]]$month,dat.yr[[Yrs[i]]]$catch/divisor,col=cols[i],lwd=lineWds[i],lty=lineTypes[i],type="b",pch=20)
        }   
        legend("topleft",legend=Yrs,col=cols,lty=lineTypes,lwd=lineWds,cex=leg.cex,bty = "n")
        axis(side = 1, mgp = c(1.0, 0, 0), labels = NA)
    }

    if(2 %in% plotNum) {
        maxYlim <- max(unlist(lapply(dat.yr,function(x){sum(x$catch)}))[Yrs],
            na.rm = TRUE)/divisor
        plot(1,1,xlab="Month",ylab=paste("Cumulative Catch\n(",divisor," MT)",sep=""),xlim=c(1,12),ylim=c(0,maxYlim),type="n", xaxt = "n")
        for(i in 1:length(Yrs)) {
            catchPlot.fn(dat.yr[[Yrs[i]]]$month,dat.yr[[Yrs[i]]]$catch/divisor,plotType="cumulative",col=cols[i],lwd=lineWds[i],lty=lineTypes[i],type="b",pch=20)
        }
        legend("topleft",legend=Yrs,col=cols,lty=lineTypes,lwd=lineWds,cex=leg.cex,bty = "n")
        axis(side = 1, mgp = c(1.0, 0, 0), labels = NA)
    }

    if(1 %in% plotNum) {
        plot(1,1,xlab="Month",ylab=paste("Proportion of Total Catch"),xlim=c(1,12),ylim=c(0,1),type="n",yaxs="i")
        for(i in 1:length(Yrs)) {
            catchPlot.fn(dat.yr[[Yrs[i]]]$month,dat.yr[[Yrs[i]]]$catch/divisor,plotType="proportion",col=cols[i],lwd=lineWds[i],lty=lineTypes[i],type="b",pch=20)
        }
        legend("bottomright",legend=Yrs,col=cols,lty=lineTypes,lwd=lineWds,cex=leg.cex,bty = "n")
    }

    if(!is.null(quotas) & 1 %in% plotNum) {
        plot(1,1,xlab="Month",ylab=paste("Proportion of Sector Quota"),xlim=c(1,12),ylim=c(0,1.1),type="n",yaxs="i")
        abline(h=1,col=gray(0.5))
        for(i in 1:length(Yrs)) {
            if (is.null(dim(dat.yr[[Yrs[i]]])[1])) next
            dat.yr[[Yrs[i]]] <- rbind(dat.yr[[Yrs[i]]],c(NA,13,as.numeric(Yrs[i]),quotas[[Yrs[i]]]-sum(dat.yr[[Yrs[i]]][,"catch"])))
            catchPlot.fn(dat.yr[[Yrs[i]]]$month,dat.yr[[Yrs[i]]]$catch/divisor,plotType="proportion",col=cols[i],lwd=lineWds[i],lty=lineTypes[i],type="b",pch=20)
        }
        legend("bottomright",legend=Yrs,col=cols,lty=lineTypes,lwd=lineWds,cex=leg.cex,bty = "n")
    
    }
}

#Old line version
# plotHakeCatchMonthYear.fn <- function(dat,Yrs=range(dat$Year),quotas=NULL,lineWds,lineTypes,cols,leg.cex=1,divisor=1000) {
# #Take hake dataframe of Fleet, Month, Year, MT and plots year specific catches by month
# #Does not discriminate by fleet
# #Assumes that there is one observation of Month in each year

#     dat.yr <- split(dat,dat$Year)
#     maxYlim <- max(unlist(lapply(dat.yr,function(x){max(x$Catch.MT)}))[Yrs])/divisor
    
#     plot(1,1,xlab="Month",ylab=paste("Catch\n(",divisor," MT)",sep=""),xlim=c(1,12),ylim=c(0,maxYlim),type="n")
#     for(i in 1:length(Yrs)) {
#         catchPlot.fn(dat.yr[[Yrs[i]]]$Month,dat.yr[[Yrs[i]]]$Catch.MT/divisor,col=cols[i],lwd=lineWds[i],lty=lineTypes[i])
#     }
#     legend("topleft",legend=Yrs,col=cols,lty=lineTypes,lwd=lineWds,cex=leg.cex)

#     maxYlim <- max(unlist(lapply(dat.yr,function(x){sum(x$Catch.MT)}))[Yrs])/divisor
#     plot(1,1,xlab="Month",ylab=paste("Cumulative Catch\n(",divisor," MT)",sep=""),xlim=c(1,12),ylim=c(0,maxYlim),type="n")
#     for(i in 1:length(Yrs)) {
#         catchPlot.fn(dat.yr[[Yrs[i]]]$Month,dat.yr[[Yrs[i]]]$Catch.MT/divisor,type="cumulative",col=cols[i],lwd=lineWds[i],lty=lineTypes[i])
#     }
#     legend("topleft",legend=Yrs,col=cols,lty=lineTypes,lwd=lineWds,cex=leg.cex)

#     plot(1,1,xlab="Month",ylab=paste("Proportion of Total Catch"),xlim=c(1,12),ylim=c(0,1),type="n",yaxs="i")
#     for(i in 1:length(Yrs)) {
#         catchPlot.fn(dat.yr[[Yrs[i]]]$Month,dat.yr[[Yrs[i]]]$Catch.MT/divisor,type="proportion",col=cols[i],lwd=lineWds[i],lty=lineTypes[i])
#     }
#     legend("topleft",legend=Yrs,col=cols,lty=lineTypes,lwd=lineWds,cex=leg.cex)
    
#     if(!is.null(quotas)) {
#         plot(1,1,xlab="Month",ylab=paste("Proportion of Sector Quota"),xlim=c(1,12),ylim=c(0,1.1),type="n",yaxs="i")
#         abline(h=1,col=gray(0.5))
#         for(i in 1:length(Yrs)) {
#             dat.yr[[Yrs[i]]] <- rbind(dat.yr[[Yrs[i]]],c(NA,13,as.numeric(Yrs[i]),quotas[[Yrs[i]]]-sum(dat.yr[[Yrs[i]]][,"Catch.MT"])))
#             catchPlot.fn(dat.yr[[Yrs[i]]]$Month,dat.yr[[Yrs[i]]]$Catch.MT/divisor,type="proportion",col=cols[i],lwd=lineWds[i],lty=lineTypes[i])
#         }
#         legend("bottomright",legend=Yrs,col=cols,lty=lineTypes,lwd=lineWds,cex=leg.cex)
    
#     }
# }
