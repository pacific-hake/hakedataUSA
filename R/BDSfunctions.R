SetUpHakeBDS.fn <- function(BDS,verbose=T,max.mmLength=1000,dataTypes=c("C"),
                                sampleMethods=c("R"),sampleTypes=c("","C","M"),
                                states=c("CA","OR","WA")) {
###############################################################################
#Reads in the BDS data and creates additional columns as well as filters the data
#Columns added are:
    #length.cm
# NOTE: There is an MPT gear type which is midwater trawl catcher/processor (why are the landing it?)
#           There are MPT samples in summer when no CP's were fishing
###############################################################################


    BDS$length.cm <- BDS$FISH_LENGTH/10                                                 #lengths in cm

    BDS$inpfc <- rep(NA,nrow(BDS))                                  #assign single ipnfc codes to areas
    BDS$inpfc[BDS$INPFC_AREA %in% c("CP")] <- "CP"
    BDS$inpfc[BDS$INPFC_AREA %in% c("MT")] <- "MT"
    BDS$inpfc[BDS$INPFC_AREA %in% c("EK","EU","EUR")] <- "EK"
    BDS$inpfc[BDS$INPFC_AREA %in% c("CL","COL","NC","SC")] <- "CL"
    BDS$inpfc[BDS$INPFC_AREA %in% c("VN","VUS")] <- "VN"
    BDS$inpfc[is.na(BDS$INPFC_AREA)] <- "NA"

    BDS$area <- BDS$inpfc  #can use this to define specific areas

    BDS$gear <- NA
    BDS$gear[BDS$GRID%in%c("SST","SHT","PWT","DST","DSG")] <- NA #"ShrimpTrawl"
    BDS$gear[BDS$GRID%in%c("RLT","GFT","GFS","GFL","FTS","FFT","BTT","BMT","56")] <- "BottomTrawl"
    BDS$gear[BDS$GRID%in%c("OTW","MDT","54","MPT")] <- "MidwaterTrawl"
    BDS$gear[BDS$GRID%in%c("PRT","DNT")] <- NA #"MiscTrawl"
    BDS$gear[BDS$GRID%in%c("BTR","CLP","CPT","FPT","OPT","PRW")] <- NA# "Pot"
    BDS$gear[BDS$GRID%in%c("HKL","JIG","LGL","OHL","POL","TRL","VHL")] <- "HnL"
    BDS$gear[BDS$GRID%in%c("DPN","DGN","GLN","ONT","SEN","STN")] <- "Net"
    BDS$gear[BDS$GRID%in%c("DVG","USP","UNK","XXX")] <- NA #"Other"   #MPT is CP midwater

    if(verbose) {
        cat("There are",sum(is.na(BDS$gear)),"rows out of",nrow(BDS),"where gear is not classified as",unique(BDS$gear),".\n\n")
        flush.console()
    }

    BDS$state <- rep(NA,nrow(BDS))
    BDS$state[BDS$SAMPLE_AGENCY == "CA"] <- "CA"
    BDS$state[BDS$SAMPLE_AGENCY == "OR"] <- "OR"
    BDS$state[BDS$SAMPLE_AGENCY == "W"] <- "WA"
    BDS$state[BDS$SAMPLE_AGENCY == "PW"] <- "PW"


    #drop columns that have all NA's
    ind <- apply(BDS,2,function(x){all(is.na(x))})  #goes column by column and returns TRUE for columns with all NA's
    if(verbose) {
        cat("These columns were dropped:\n")
        print(names(BDS)[ind])
        cat("\n")
        flush.console()
    }
    BDS <- BDS[,!ind]

    if(verbose) {
        cat("\nThe dimensions of the unfiltered dataframe are:\n")
        print(dim(BDS))
        cat("\n")
        flush.console()
    }

    #filter out the rows that will not be used
    keep <- rep(T,nrow(BDS))                                             #TRUE means to omit that row
    totKeep <- sum(keep)
    keep <- keep & !is.na(BDS$area)
     if(verbose){cat(totKeep-sum(keep),"rows omitted becasue of NA in area (Canadian catch)\n")}; totKeep <- sum(keep)
    keep <- keep & (BDS$state %in% states)
     if(verbose){cat(totKeep-sum(keep),"rows omitted becasue not a specified state:",states,"\n")}; totKeep <- sum(keep)
    keep <- keep & (!is.na(BDS$gear))
     if(verbose){cat(totKeep-sum(keep),"rows omitted becasue not a specified gear:",unique(BDS$gear),"\n")}; totKeep <- sum(keep)
    keep <- keep & !is.na(BDS$FISH_LENGTH)                               #omit missing observations of length
     if(verbose){cat(totKeep-sum(keep),"rows omitted becasue of NA in Fish Length\n")}; totKeep <- sum(keep)
    keep <- keep & BDS$FISH_LENGTH > 0                                   #omit lengths 0 or less
     if(verbose){cat(totKeep-sum(keep),"rows omitted becasue of Fish Length <= 0\n")}; totKeep <- sum(keep)
    keep <- keep & BDS$FISH_LENGTH <= max.mmLength                       #omit fish greater than maxLength
     if(verbose){cat(totKeep-sum(keep),"rows omitted becasue of Fish Length >",max.mmLength,"mm\n")}; totKeep <- sum(keep)
    keep <- keep & BDS$DATA_TYPE %in% dataTypes                          #types of data such as commercial or survey
     if(verbose){cat(totKeep-sum(keep),"rows omitted becasue dataTypes were not of type",dataTypes,"\n")}; totKeep <- sum(keep)

    midFilterBDS <- BDS[keep,]                                                  #save this to use for calcualting the median landing weights

    keep <- keep & BDS$SAMPLE_METHOD %in% sampleMethods                  #sampling method such as random, stratified, special,...
     if(verbose){cat(totKeep-sum(keep),"rows omitted becasue sampleMethods were not of type",sampleMethods,"\n")}; totKeep <- sum(keep)
    keep <- keep & BDS$SAMPLE_TYPE %in% sampleTypes                      #sampling type such as market, research, special
     if(verbose){cat(totKeep-sum(keep),"rows omitted becasue sampleTypes were not of type",sampleTypes,"\n")}; totKeep <- sum(keep)
     if(verbose) {
        cat(sum(keep),"rows retained out of",nrow(BDS),"based on filtering.\n")
     }
    BDS <- BDS[keep,]

    #BDS$length.cm <- round(BDS$length.cm,0)                         #round all lengths to nearest cm
    #if(verbose) cat("Lengths were rounded to the nearest cm in length.cm\n")
    BDS$length.cm <- floor(BDS$length.cm)                         #floor all lengths to nearest cm
    if(verbose) cat("Lengths were floored to the lower cm in length.cm\n")

    #set up total weights for expansion
    #use EXP_WT if provided (for OR only)
    #use TOTAL_WGT otherwise
    BDS$totalWt <- rep(NA,nrow(BDS))
    ind <- !is.na(BDS$TOTAL_WGT) & BDS$TOTAL_WGT>0                                      #use total weight  where available
    BDS$totalWt[ind] <- BDS$TOTAL_WGT[ind]
    ind <- !is.na(BDS$EXP_WT) & BDS$EXP_WT>0                                           #OR samples with an expanded total weight
    BDS$totalWt[ind] <- BDS$EXP_WT[ind]                                          # replace any total weights with these expanded (IAN used exp_wts for Ebglish Sole, but did not use TOTAL_WGT from OR)
    if(verbose) cat("A column call totalWt created using TOTAL_WGT and EXP_WT\n")

    ############
    # Some agencies do not have a total weight or cluster weight (WA)
    # sum up FISH_WEIGHTS to get a total and cluster weight
    # coded real quick and sloppy for now
    # Fills in MALES_WGT, FEMALES_WGT, and CLUSTER_WGT with sum of individual fish weights
    for(iii in unique(BDS$SAMPLE_NO)) {
        tmp <- BDS[BDS$SAMPLE_NO == iii,]
        if(is.na(sum(tmp$MALES_WGT))) {
            BDS[BDS$SAMPLE_NO == iii,"MALES_WGT"] <- sum(tmp$FISH_WEIGHT[tmp$SEX=="M"])
        }
        if(is.na(sum(tmp$MALES_NUM))) {
            BDS[BDS$SAMPLE_NO == iii,"MALES_NUM"] <- sum(tmp$SEX=="M")
        }
        if(is.na(sum(tmp$FEMALES_WGT))) {
            BDS[BDS$SAMPLE_NO == iii,"FEMALES_WGT"] <- sum(tmp$FISH_WEIGHT[tmp$SEX=="F"])
        }
        if(is.na(sum(tmp$FEMALES_NUM))) {
            BDS[BDS$SAMPLE_NO == iii,"FEMALES_NUM"] <- sum(tmp$SEX=="F")
        }
        if(is.na(sum(tmp$CLUSTER_WGT))) {
            BDS[BDS$SAMPLE_NO == iii,"CLUSTER_WGT"] <- sum(tmp$FISH_WEIGHT)
        }
    }
 

    if(verbose) {
        cat("\nThe final dimensions of the filtered dataframe are:\n")
        print(dim(BDS))
        cat("\n")
        flush.console()
    }

    BDS$SAMPLE_NO <- as.character(BDS$SAMPLE_NO)

    return(BDS)
}


commLFs.fn <- function(bds,lw,gear="TWL",state=NULL,catchFile=NULL,
                  maxExpansion=300,verbose=T,normalize=T,
                  doSexRatio=TRUE,sexRatioUnsexed=NA,maxSizeUnsexed=NA,loessSpan=0.3,
                  ageComp=FALSE)  {
    #####################################################################################
    # lw is a data.frame of length-weight params with columns as parameters for different sexes (and unsexed) and rows as WA, OR, CA (1, 2, 3)
    #      # create a predicted fish weight based on state and length
    #   Use unsexed because of uncertainty
    #   use cm and kg for length and weight for fitted regression coefficients!)
    #   lw=data.frame(WA=c(5.6992e-6,3.2523),OR=c(6.8960e-6,3.1873),CA=c(9.4427e-6,3.0855)),
    # sexRatioUnsexed is the ratio to assume for unsexed fish less than maxSizeUnsexed
    # maxExpansion of 300 is about where 20% ofthe samples would be capped, it is also where there is a kink in cumsum
    ######################################################################################

    if(!is.null(gear)) {
        bds <- bds[!is.na(bds$gear)&bds$gear==gear,]             #use only a specific gear code (see BDS_filterData.R for gear code assignments)
    }
    if(!is.null(state)) {
        bds <- bds[bds$state==state,]             #use only a specific state (see BDS_filterData.R for state assignments)
    }

    # set up the expansion based on landed weight to cluster weight
    #if that isn't available, check for species landed weight (RWT_LBS) to species weight
    #if that isn't available, guess
    #use totalWt that was created in BDS_filterData.R, unless missing
    #bds$TOTAL_WGT[is.na(bds$TOTAL_WGT)] <- 0
    bds$usetot_wgt <- NA
    bds$sampleWgt <- NA

    # create a predicted fish weight based on sex and length
    # uses cm and kg ans length and weight units
    # these will be summed to give the sample weight, however
    #don't need to use this because expand cluster weight to total weight because they are cluster sampling a mixture of species
    bds$predwt <- NA
    bds$predwt[bds$state %in% "WA"] <- lw$WA[1]*((bds$FISH_LENGTH[bds$state %in% "WA"]/10)^lw$WA[2]) * 2.20462  #convert kg to pounds
    bds$predwt[bds$state %in% "OR"] <- lw$OR[1]*((bds$FISH_LENGTH[bds$state %in% "OR"]/10)^lw$OR[2]) * 2.20462  #convert kg to pounds
    bds$predwt[bds$state %in% "PW"] <- lw$OR[1]*((bds$FISH_LENGTH[bds$state %in% "PW"]/10)^lw$OR[2]) * 2.20462  #convert kg to pounds
    bds$predwt[bds$state %in% "CA"] <- lw$CA[1]*((bds$FISH_LENGTH[bds$state %in% "CA"]/10)^lw$CA[2]) * 2.20462  #convert kg to pounds

    tmp.split <- split(bds$predwt,bds$SAMPLE_NO)
    predWtSum <- unlist(lapply(tmp.split,sum))
    bds$predWtSum <- predWtSum[match(bds$SAMPLE_NO,names(predWtSum))]
    if(any(is.na(bds$predWtSum))) {
        stop("There are some predicted weights that are NA. This means that there are NA's in lengths, the parameters, or somewhere else.")
    }

    #WA does not have a sample weight
    #I will calcualte sample weight from individual fish weights (filled in using setUp function) or l-w parameters
    ### This is likely incorrect, but may not cause too much bias. The totalWt is probably the landing of mixed species, and the sample was taken from the mixed species
    ### Theoretically, the best weighting would be totalWt:SampleWt, not totalWT:SpeciesWt.
    ### So, expansion factors are probably bigger than actual, although for hake the catches are predominately hake
    #missing fields will assume an expansion of 1
    ind <- bds$SOURCE_AGID %in% c("W")
    bds$usetot_wgt[ind] <- bds$totalWt[ind]

    bds$sampleWgt[ind] <- bds$all_cluster_sum[ind]
    ind <- ind & is.na(bds$sampleWgt)
    bds$sampleWgt[ind] <- bds$CLUSTER_WGT[ind]
    ind <- ind & is.na(bds$sampleWgt)
    bds$sampleWgt[ind] <- bds$predWtSum[ind]

    ind <- (bds$SOURCE_AGID %in% c("W")) & is.na(bds$usetot_wgt)
    bds$usetot_wgt[ind] <- bds$sampleWgt[ind]


    #OR uses expanded weight, lbs
    ind <- !is.na(bds$EXP_WT) & bds$SOURCE_AGID=="O"
    bds$usetot_wgt[ind] <- bds$EXP_WT[ind]

    ind <- bds$SOURCE_AGID=="O"
    bds$sampleWgt[ind] <- bds$all_cluster_sum[ind]
    ind <- ind & is.na(bds$sampleWgt)
    bds$sampleWgt[ind] <- bds$CLUSTER_WGT[ind]
    ind <- ind & is.na(bds$sampleWgt)
    bds$sampleWgt[ind] <- bds$predWtSum[ind]


    #CA uses total weight, lbs and all_cluster_wgt because multiple clusters
    #all cluster wt is the weight of the sample, which might not be all one species.
    #So, use specieswt
    #TOTAL_WGT is the weight of the landing for what was sampled in cluster
    #it looks like the ratio should be either RWT_LBS/allSPECIES_WGT or TOTAL_WGT/all_cluster_sum
    #the difference is when the cluster sample is not specifically widow, but a mix
    ind <- bds$SOURCE_AGID == "C" & !is.na(bds$TOTAL_WGT)
    #first try to fill in TOTAL_WGT/all_cluster_sum
    bds$usetot_wgt[ind] <- bds$TOTAL_WGT[ind]
    bds$sampleWgt[ind] <- bds$all_cluster_sum[ind]
    #then go for RWT_LBS/allSPECIES_WGT for ones that don't have both of above

    # ind2 <- ind & is.na(bds$usetot_wgt) | is.na(bds$sampleWgt) & !is.na(bds$RWT_LBS)
    # bds$usetot_wgt[ind2] <- bds$RWT_LBS[ind2]
    # ind2 <- ind & (is.na(bds$usetot_wgt) | is.na(bds$sampleWgt)) &!is.na(bds$allSpSum)
    # bds$sampleWgt[ind] <- bds$allSpSum[ind]

    ind <- is.na(bds$sampleWgt)
    bds$sampleWgt[ind] <- bds$predWtSum[ind]

    #using predWt here with total landing is no different than other states, even though expansion may be larger than it should
    #bds$usetot_wgt[ind & is.na(bds$usetot_wgt)] <- bds$all_cluster_sum[ind & is.na(bds$usetot_wgt)]*0.999*0.45359237


    # possibly use median from the right agency where total landed weight missing (if weights are missing)
    #fill in missing landings weights with predicted weights (could do medians or something also, before expansion) I will set expansion to 0.999 because
    #only 2 samples from OR and 63 from CA (mostly early years) has missing values (WA likely has a lot of missing species weights, but I used predWt)
    #so I'm just going to use 0.999 as the expansion (filled in later)
    # ind <- bds$SOURCE_AGID == "O"
    # bds$usetot_wgt[ind & is.na(bds$usetot_wgt)] <- bds$all_cluster_sum[ind & is.na(bds$usetot_wgt)]*0.999*0.45359237

    bds$usetot_wgt <- as.numeric(bds$usetot_wgt)
    bds$sampleWgt <-  as.numeric(bds$sampleWgt)

    #calculate expansion factors
    # compute an expansion factor for each length observation
    #Owen's recommended power function to account for non-homogeneity within a trip
    bds$expand <- (bds$usetot_wgt/bds$sampleWgt)^0.9
    bds$effN <- bds$sampleWgt^0.8

    bds$expand[is.na(bds$expand)] <- 0.999  #arbitrary, but I can see which ones

    samps <- bds[!duplicated(bds$SAMPLE_NO),]
    row.names(samps) <- samps$SAMPLE_NO
    if(verbose) {
        cat("Table of number of samples with missing weights to expand with by source agency ID. (TRUE or NA means missing)\n")
        print(table(is.na(samps$usetot_wgt),samps$SOURCE_AGID,useNA="ifany"))
        print(table(is.na(samps$usetot_wgt),samps$SAMPLE_YEAR,samps$SOURCE_AGID,useNA="ifany"))
        cat("If there are missing weights, you may want to decide how to fill them in.\n\n")
        print(samps[is.na(samps$usetot_wgt),])
    }

    if(verbose) {
        cat("There are",sum(bds$expand < 0.999),"expansion factors less than 0.999.\n")
        cat("There are",sum(bds$expand > maxExpansion),"expansion factors greater than",maxExpansion,"\n")
        par(mfrow=c(2,1))
        #####expansion factors by state
        boxplot(split(bds$expand,bds$state),xlab="State",ylab="expansion to landing factor")
        boxplot(split(bds$expand,bds$state),xlab="State",ylab="expansion to landing factor",ylim=c(0,maxExpansion))
        windows(height=5,width=6.5)
        plot(cumsum(table(round(samps$expand,0),useNA='ifany'))/nrow(samps))
    }

    bds$expand[bds$expand < 0.999] <- 0.998
    bds$expand[bds$expand > maxExpansion] <- maxExpansion

    #####expansion factors by state
    windows()
    boxplot(split(bds$expand,bds$state),xlab="State",ylab="expansion to landing factor")

    #USE sex ratio to assign unsexed fish to M or F
    if(doSexRatio) {
        tmp.split <- split(bds$SEX,bds$length.cm)
        x.fn <- function(x) {
            xx <- table(x)
            out <- xx["F"]/(xx["F"]+xx["M"])
            names(out) <- NULL
            return(out)
        }
        propF <- unlist(lapply(tmp.split,x.fn))
        nobs <- unlist(lapply(tmp.split,function(x){sum(x=="M" | x=="F")}))
        lens <- as.numeric(names(propF))
        if(verbose) {
            windows(height=5,width=6.5)
            plot(lens,propF,type="l",col="red",xlab="Length (cm)",ylab="Fraction female",ylim=c(0,1),main="Sex Ratio")
            symbols(lens,propF,circles=nobs,inches=0.1,fg="red",bg=rgb(1,0,0,alpha=0.5),add=T)
        }
        propF[lens<=28] <- 0.5
        propF[lens>55] <- 1
        nobs[lens<=28] <- max(nobs)
        lo <- loess(propF~lens,weights=nobs,span=loessSpan)  #determined by eye
        sexRatio <- predict(lo,newdata=data.frame(lens=min(bds$length.cm):max(bds$length.cm)))
        names(sexRatio) <- as.numeric(min(bds$length.cm):max(bds$length.cm))
        sexRatio[sexRatio>1] <- 1
        if(verbose) {
            lines(lens,propF,col="darkgreen")
            lines(as.numeric(names(sexRatio)),sexRatio,col="blue")
            legend("topleft",c("Data","Data w/ Assumed","LOESS"),col=c("red","darkgreen","blue"),lty=1)
        }
        bds$sexRatio <- sexRatio[match(bds$length.cm,as.numeric(names(sexRatio)))]
        bds$FREQ <- as.numeric(bds$FREQ)

        bdsSexRatio <- bds
        tmpF <- tmpM <- bdsSexRatio[bdsSexRatio$SEX=="U",]
        tmpF$SEX <- "F"
        tmpM$SEX <- "M"
        tmpF$FREQ <- tmpF$sexRatio
        tmpM$FREQ <- 1-tmpM$sexRatio
        bdsSexRatio <- rbind(bdsSexRatio,tmpM,tmpF)
        # for(i in 1:nrow(bdsSexRatio)) {
        #     if(bdsSexRatio$SEX[i] == "U") {
        #         tmpM <- tmpF <- bdsSexRatio[i,]
        #         tmpM$SEX <- "M"
        #         tmpF$SEX <- "F"
        #         tmpM$FREQ <- 1-tmpM$sexRatio
        #         tmpF$FREQ <- tmpF$sexRatio
        #         bdsSexRatio <- rbind(bdsSexRatio,tmpM,tmpF)
        #     }
        # }
    }else {
        bdsSexRatio <- bds
    }

    if(ageComp) {
        #switch age into length.cm to trick the code
        #Do this now so that sex ratio is determined using lengths
        origLength <- bds$length.cm
        bds$length.cm <- bds$FISH_AGE_YEARS_FINAL
        origLengthSexRatio <- bdsSexRatio$length.cm
        bdsSexRatio$length.cm <- bdsSexRatio$FISH_AGE_YEARS_FINAL
    }

    # get the number of observed lengths for each sample
    tmp <- table(bds$SAMPLE_NO,!is.na(bds$length.cm))[,"TRUE"]   #numer of lengths in each sample
    samps$obsSampNum <- NA
    samps[names(tmp),"obsSampNum"] <- tmp

    bdsSampsInd <- match(bds$SAMPLE_NO,samps$SAMPLE_NO)

    bds$numLens <- NA
    bds$numLens <- samps[bdsSampsInd,"obsSampNum"]

    samps <- bds[!duplicated(bds$SAMPLE_NO),]
    row.names(samps) <- samps$SAMPLE_NO


    #THINK ABOUT COMBINING SAMPLES WITHIN PORTS OR SIMILAR TO GET LRGER NUMBER OF FISH PER SAMPLE (LOTS OF 1'S)


    # the effective number of fish in each length is the expansion factor (b/c only one per length)
    # except if fish are put in from sex ratio. Then it is expand*FREQ
    # aggregate the effective number to build the composition data for each sex
    tmp <- bdsSexRatio[bdsSexRatio$SEX == "M",]
    if(nrow(tmp)>0) {
        maleLenComps <- aggregate(tmp$expand*tmp$FREQ,
                        list(state=tmp$state, year=tmp$SAMPLE_YEAR, sex=tmp$SEX, length=tmp$length.cm),
                        sum)
    }
    else {
        maleLenComps <- data.frame(state=NA,year=NA,sex=NA,length=NA,x=NA)
    }

    tmp <- bdsSexRatio[bdsSexRatio$SEX == "F",]
    if(nrow(tmp)>0) {
    femaleLenComps <- aggregate(tmp$expand*tmp$FREQ,
                        list(state=tmp$state, year=tmp$SAMPLE_YEAR, sex=tmp$SEX, length=tmp$length.cm),
                        sum)
    }
    else {
        maleLenComps <- data.frame(state=NA,year=NA,sex=NA,length=NA,x=NA)
    }

    tmp <- bds[bds$SEX == "U",]   #FREQ should be all be 1
    if(nrow(tmp)>0) {
    unsexedLenComps <- aggregate(tmp$expand,
                        list(state=tmp$state, year=tmp$SAMPLE_YEAR, sex=tmp$SEX, length=tmp$length.cm),
                        sum)
    }
    else {
        unsexedLenComps <- data.frame(state=NA,year=NA,sex=NA,length=NA,x=NA)
    }

    tmp <- bds
    if(nrow(tmp)>0) {  #use only the originally since M and F added on for sex ratio stuff
    allSexLenComps <- aggregate(tmp$expand,
                        list(state = tmp$state, year = tmp$SAMPLE_YEAR, length = tmp$length.cm),
                        sum)
    allSexLenComps$sex <- "FMU"
    }
    else {
        allSexLenComps <- data.frame(state=NA,year=NA,sex=NA,length=NA,x=NA)
    }

    tmpF <- femaleLenComps
    tmpF$sex <- "F"
    tmpM <- maleLenComps
    tmpM$sex <- "M"
    bothSexLenComps <- rbind(tmpF,tmpM)

    # summary of the number of fish of each sex sampled
    maleLens <- table(bds$SAMPLE_YEAR[bds$SEX == "M"],bds$state[bds$SEX == "M"])
    femaleLens <- table(bds$SAMPLE_YEAR[bds$SEX == "F"],bds$state[bds$SEX == "F"])
    unsexedLens <- table(bds$SAMPLE_YEAR[bds$SEX == "U"],bds$state[bds$SEX == "U"])
    allSexLens <- table(bds$SAMPLE_YEAR,bds$state)

    # calculate number of samples by year and state
    table(samps$SAMPLE_YEAR,samps$state)

    #the total weights are of the entire species mix, sometimes
    #so, instead of calcualting an expansion factor, I'll renormailize the LF and multiply by the catch
    #this is only important if combining states
    maleLenComps <- split(maleLenComps,maleLenComps$state)
    maleLenComps <- lapply(maleLenComps,function(x){split(x,x$year)})
    femaleLenComps <- split(femaleLenComps,femaleLenComps$state)
    femaleLenComps <- lapply(femaleLenComps,function(x){split(x,x$year)})
    unsexedLenComps <- split(unsexedLenComps,unsexedLenComps$state)
    unsexedLenComps <- lapply(unsexedLenComps,function(x){split(x,x$year)})
    allSexLenComps <- split(allSexLenComps,allSexLenComps$state)
    allSexLenComps <- lapply(allSexLenComps,function(x){split(x,x$year)})
    bothSexLenComps <- split(bothSexLenComps,bothSexLenComps$state)
    bothSexLenComps <- lapply(bothSexLenComps,function(x){split(x,x$year)})

    if(is.null(state)) { #only expand up to state if all states are included
        #You want to expand by the TotalCatch/TotalWtSampledFrom
        #But the catch is in the particular species, whereas the samples come from possibly a mix of species
        #So, instead, of calculating an expansion factor, renormailize the LF and multiply by the catch
        #Then add all states together and renormalize to get a weighted proportion by catch in each state
        #Remember to normalize males and females together to retain sex ratio

        #read in total catches by year, area, season for final expansion
        Catch <- read.csv(catchFile,header=T)
        tmp <- unlist(Catch[,-1])
        Catch <- data.frame(year=Catch[,1],state=substring(names(tmp),1,2),catch=tmp)

        normalizeLF.fn <- function(xx,catch) {
            lapply(xx,function(x){
              prop <- x$x/sum(x$x)
              prop <- catch[catch$state==x$state[1] & catch$year==x$year[1],"catch"]*prop
              ret <- data.frame(state=x$state,year=x$year,length=x$length,sex=x$sex,lf=prop)
              if(ageComp) names(ret)[3] <- "age"
              return(ret)
            })
        }
        #Only bothSexLenComps has both sexes normalized together.
        #maleLenComps and femaleLenComps set the sex ratio to 50:50 is used together
        maleLenComps <- lapply(maleLenComps,normalizeLF.fn,catch=Catch)
        femaleLenComps <- lapply(femaleLenComps,normalizeLF.fn,catch=Catch)
        unsexedLenComps <- lapply(unsexedLenComps,normalizeLF.fn,catch=Catch)
        allSexLenComps <- lapply(allSexLenComps,normalizeLF.fn,catch=Catch)
        bothSexLenComps <- lapply(bothSexLenComps,normalizeLF.fn,catch=Catch)

        cat("Comps weighted by state specific catches.\nThey can simply be added together for a coastwide comp\n")
    }

    if(!is.null(state)) { #only expand up to state if all states are included, but format all lists here for a single state
        if(normalize) {
            normalizeLF.fn <- function(xx) {
              lapply(xx,function(x){
                prop <- x$x/sum(x$x)
                ret <- data.frame(state=x$state,year=x$year,sex=x$sex,length=x$length,lf=prop)
                if(ageComp) names(ret)[4] <- "age"
                return(ret)
              })
            }
        }

        if(!normalize) {
            normalizeLF.fn <- function(xx) {
              lapply(xx,function(x){
                prop <- x$x
                ret <- data.frame(state=x$state,year=x$year,sex=x$sex,length=x$length,lf=prop)
                if(ageComp) names(ret)[4] <- "age"
                return(ret)
              })
            }
        }
        maleLenComps <- lapply(maleLenComps,normalizeLF.fn)
        femaleLenComps <- lapply(femaleLenComps,normalizeLF.fn)
        unsexedLenComps <- lapply(unsexedLenComps,normalizeLF.fn)
        allSexLenComps <- lapply(allSexLenComps,normalizeLF.fn)
        bothSexLenComps <- lapply(bothSexLenComps,normalizeLF.fn)
    }

    return(list(female=femaleLenComps,male=maleLenComps,both=bothSexLenComps,unsexed=unsexedLenComps,all=allSexLenComps,bds=bds[,c("SAMPLE_NO","SAMPLE_YEAR","state","expand","effN")]))
}



lfsForSS3_combinedsex.fn <- function(LFs,years,season=1,fleet,gender=0,partition=0,lens) {
    #LFs is a list of years and from above would be specific to a state
    #years <- sort(unique(c(names(LFs))))
    out <- matrix(0,nrow=length(years),ncol=2*length(lens)+6,dimnames=list(years,c("year","Season","Fleet","gender","partition","nSamps",paste("F",lens,sep=""),paste("M",lens,sep=""))))
    out[,"year"] <- as.numeric(years)
    out[,"Season"] <- season
    out[,"Fleet"] <- fleet
    out[,"gender"] <- gender
    out[,"partition"] <- partition
    out[,"nSamps"] <- NA
    for(i in names(LFs)) {
        tmp <- LFs[[i]]
        lfsplus <- sum(tmp$lf[tmp$length>=max(lens)])
        lfsminus <- sum(tmp$lf[tmp$length<=min(lens)])
        tmp[tmp$length==max(lens),"lf"] <- lfsplus
        tmp[tmp$length==min(lens),"lf"] <- lfsminus
        tmp <- tmp[tmp$length>=min(lens)&tmp$length<=max(lens),]
        lgths <- lens[findInterval(tmp$length,lens)]
        lgths <- paste("F",lgths,sep="")
        x <- tapply(tmp$lf,lgths,sum)
        lgths <- names(x)
        out[i,lgths] <- x
        lgths <- lens[findInterval(tmp$length,lens)]
        lgths <- paste("M",lgths,sep="")
        x <- tapply(tmp$lf,lgths,sum)
        lgths <- names(x)
        out[i,lgths] <- x
    }
    return(out)
}


effN <- function(nSample,nFish,source=c("fishery","survey")) {
    if(length(nSample)!=length(nFish)) {stop("Vectors must be same length")}
    if(source=="fishery") {
        nFishCoeff <- 0.138
        nSampleCoeff <- 7.06
        breakPt <- 44
    }
    if(source=="survey") {
        nFishCoeff <- 0.0707
        nSampleCoeff <- 4.89
        breakPt <- 55
    }
    nEff <- rep(NA,length(nSample))
    ind <- nFish/nSample < breakPt
    ind[is.na(ind)] <- TRUE
    nEff[ind] <- nSample[ind] + nFishCoeff*nFish[ind]
    ind <- !ind
    nEff[ind] <- nSampleCoeff * nSample[ind]
    names(nEff) <- names(nFish)
    return(nEff)
}
