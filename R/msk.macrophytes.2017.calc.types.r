msk.macrophytes.2017.sample <- function(dist,par,sampsize)
{
  samp <- rep(NA,sampsize)
  if ( dist == "Delta" | dist == "delta" )
  {
    # delta distribution; parameter is mean:
    if ( !is.na(par[1]) ) samp <- rep(par[1],sampsize)
  }
  if ( dist == "Normal" | dist == "normal" )
  {
    # normal distribution; parameters are mean and sd:
    if ( !anyNA(par[1:2]) ) samp <- rnorm(sampsize,mean=par[1],sd=par[2])
  }
  if ( dist == "Lognormal" | dist == "lognormal" )
  {
    # lognormal distribution; parameters are mean and sd:
    if ( !anyNA(par[1:2]) )
    {
      if ( par[2] == 0 )
      {
        samp <- rep(par[1],sampsize)
      }
      else
      {
        sdlog   <- sqrt(log(1+par[2]^2/par[1]^2))
        meanlog <- log(par[1]) - 0.5*sdlog^2
        samp    <- rlnorm(sampsize,meanlog=meanlog,sdlog=sdlog)
      }
    }
  }
  if ( dist == "NormalTrunc" | dist == "normaltrunc" )
  {
    # truncated normal distribution; parameters are mean, sd, min and max
    # of untruncated normal distribution
    if ( !anyNA(par[1:4]) )
    {
      cdf.min <- pnorm(par[3],mean=par[1],sd=par[2])
      cdf.max <- pnorm(par[4],mean=par[1],sd=par[2])
      samp <- qnorm(runif(sampsize,min=cdf.min,max=cdf.max),mean=par[1],sd=par[2])
    }
  }
  if ( dist == "LognormalTrunc" | dist == "lognormaltrunc" )
  {
    # truncated lognormal distribution; parameters are mean, sd, min and max
    # of untruncated lognormal distribution
    if ( !anyNA(par[1:4]) )
    {
      if ( par[2] == 0 )
      {
        samp <- rep(par[1],sampsize)
      }
      else
      {
        sdlog   <- sqrt(log(1+par[2]^2/par[1]^2))
        meanlog <- log(par[1]) - 0.5*sdlog^2
        cdf.min <- plnorm(par[3],meanlog=meanlog,sdlog=sdlog)
        cdf.max <- plnorm(par[4],meanlog=meanlog,sdlog=sdlog)
        samp <- qlnorm(runif(sampsize,min=cdf.min,max=cdf.max),meanlog=meanlog,sdlog=sdlog)
      }
    }
  }
  return(samp)
}


msk.macrophytes.2017.decode.typedef <- function(language="English",dictionaries=NA)
{
  DefStruct    <- ecoval::msk.macrophytes.2017_RiverTypes_DefStruct
  DefLimitsUnc <- ecoval::msk.macrophytes.2017_RiverTypes_DefLimitsUnc
  DefObsUnc    <- ecoval::msk.macrophytes.2017_RiverTypes_DefObsUnc
  
  n.var <- match(FALSE,nchar(DefStruct[1,])==3 & substring(DefStruct[1,],2,2)=="_") - 1  # strings for variables are "x_y"
  
  dict <- ecoval.dict(language,dictionaries)
  
  # translate river type definition files:
  
  for ( j in 1:ncol(DefStruct) ) colnames(DefStruct)[j] <- ecoval.translate(colnames(DefStruct)[j],dict)
  for ( j in (n.var+2):ncol(DefStruct) )
  {
    for ( i in 1:nrow(DefStruct) )
    {
      words <- strsplit(DefStruct[i,j],split=",")[[1]]
      for ( k in 1:length(words) ) words[k] <- ecoval.translate(words[k],dict)
      DefStruct[i,j] <- paste(words,collapse=",")
    }
  }
  for ( i in 1:nrow(DefLimitsUnc) ) DefLimitsUnc[i,1] <- ecoval.translate(DefLimitsUnc[i,1],dict)
  for ( i in 1:nrow(DefObsUnc) )    DefObsUnc[i,1]    <- ecoval.translate(DefObsUnc[i,1],dict)
  
  # decode threshold indices:
  
  var.names <- colnames(DefStruct)[1:n.var]
  thresholds.indices <- matrix(NA,nrow=nrow(DefStruct),ncol=2*n.var)
  colnames(thresholds.indices) <- paste(rep(var.names,each=2),c("_lb","_ub"),sep="")
  for ( j in 1:n.var )
  {
    thresholds.indices[,paste(var.names[j],c("_lb","_ub"),sep="")] <- 
      matrix(as.numeric(unlist(strsplit(DefStruct[,var.names[j]],split="_"))),ncol=2,byrow=TRUE)
  }
  
  thresholds <- matrix(NA,nrow=1,ncol=nrow(DefLimitsUnc))
  thresholds[1,] <- t(DefLimitsUnc[,3])
  colnames(thresholds) <- DefLimitsUnc[,1]
  
  thresholds.offsets <- match(var.names,colnames(thresholds)) - 1
  names(thresholds.offsets) <- var.names
  
  return(list(thresholds.indices = thresholds.indices,
              thresholds.offsets = thresholds.offsets,
              thresholds         = thresholds,
              types              = DefStruct[,c(ecoval.translate("A_macrophytes_rivertypescheme_class",dict),
                                                ecoval.translate("A_macrophytes_rivertypeval_class",dict),
                                                ecoval.translate("A_macrophytes_rivertypegrfo_class",dict))],
              fields             = DefStruct[,ecoval.translate("A_macrophytes_rivertypescheme_index",dict)],
              thresholds.unc     = DefLimitsUnc,
              observations.unc   = DefObsUnc))
}


msk.macrophytes.2017.get.comb <- function(thresholds.indices,
                                          thresholds.offsets,
                                          thresholds,
                                          attrib)
{
  which.unique <- function(x)
  {
    y <- which(x)
    if ( length(y) == 1 ) return(y)
    return(NA)
  }

  var.names <- names(thresholds.offsets)
  n <- nrow(thresholds)        # sample size of threshold and attrib
  m <- nrow(thresholds.indices) # number of (full) river types
  ind <- matrix(TRUE,nrow=n,ncol=m)
  for ( j in 1:length(var.names) )
  {
    ind.j <- thresholds[,thresholds.offsets[j]+thresholds.indices[,2*(j-1)+1]] <= attrib[,var.names[j]] &
             thresholds[,thresholds.offsets[j]+thresholds.indices[,2*(j-1)+2]] >  attrib[,var.names[j]]
    ind <- ind & ind.j
  }
  ind <- apply(ind,1,which.unique)
  return(ind)
}


msk.macrophytes.2017.calc.types <- function(attrib,
                                            sampsize     = 10000,
                                            language     = "English",
                                            dictionaries = NA)
{
  # get dictionary:
  
  dict <- ecoval.dict(language,dictionaries)
  
  # check presence of columns required in the attribute data frame:
  
  attrib.required <- c(ecoval.translate("A_macrophytes_site_discharge_lpers",dict),
                       ecoval.translate("A_macrophytes_site_waterdepth_m",dict),
                       ecoval.translate("A_macrophytes_site_slope_percent",dict),
                       ecoval.translate("A_macrophytes_site_coarsegravel_percent",dict),
                       ecoval.translate("A_macrophytes_site_shading_percent",dict))
  ind <- is.na(match(attrib.required,colnames(attrib)))
  if ( sum(ind) > 0 )
  {
    cat(paste("*** missing attribute(s):",paste(attrib.required[ind],collapse=",")))
    return(NA)
  }

  # get threshold definitions:
  
  typedef <- msk.macrophytes.2017.decode.typedef(language=language,dictionaries=dictionaries)
  thresholds.indices <- typedef$thresholds.indices
  thresholds.offsets <- typedef$thresholds.offsets
  thresholds         <- typedef$thresholds
  types              <- typedef$types
  fields             <- typedef$fields
  var.names          <- names(thresholds.offsets)

  # increase larges threshold of each variable to accept equality at that bound:
  
  for ( name in var.names )
  {
    ind <- which(colnames(thresholds) == name)
    ind <- ind[which.max(thresholds[1,ind])]
    thresholds[1,ind] <- 1.0000001 * thresholds[1,ind]
  }

  # evaluate interval combinations at observed values:
  
  comb.obs <- rep(NA,nrow(attrib))
  names(comb.obs) <- rownames(attrib)
  for ( i in 1:nrow(attrib) ) 
  {
    comb.obs[i] <- msk.macrophytes.2017.get.comb(thresholds.indices,
                                                 thresholds.offsets,
                                                 thresholds,
                                                 attrib[i,])
  }
  
  # evaluate types at observed values:
  
  types.obs <- types[comb.obs,1]
  names(types.obs) <- rownames(attrib)
  
  # evaluate type scheme field indices at observed values:
  
  fields.obs <- typedef$fields[comb.obs]
  
  # compile results at observations:
  
  res <- list(typedef          = typedef,
              attrib.types     = attrib[,attrib.required],
              types.comb.obs   = comb.obs,
              types.scheme.obs = types.obs,
              types.fields.obs = fields.obs)
  
  if ( sampsize > 0 )
  {
    # calculate probabilities for interval combinations:
    
    DefLimitsUnc <- ecoval::msk.macrophytes.2017_RiverTypes_DefLimitsUnc
    DefObsUnc    <- ecoval::msk.macrophytes.2017_RiverTypes_DefObsUnc
    thresholds.sample <- matrix(NA,nrow=sampsize,ncol=ncol(thresholds))
    colnames(thresholds.sample) <- colnames(thresholds)
    thresholds.sample[1,] <- thresholds
    for ( j in 1:ncol(thresholds) )
    {
      thresholds.sample[2:sampsize,j] <- 
        msk.macrophytes.2017.sample(DefLimitsUnc[j,2],as.numeric(DefLimitsUnc[j,3:ncol(DefLimitsUnc)]),sampsize-1)
    }
    comb <- rep(NA,nrow(attrib))
    comb.probs <- matrix(0,nrow=nrow(attrib),ncol=nrow(types))
    rownames(comb.probs) <- rownames(attrib)
    for ( i in 1:nrow(attrib) ) 
    {
      if ( anyNA(attrib[i,var.names]) )
      {
        comb.probs[i,] <- rep(NA,nrow(types))
      }
      else
      {
        attrib.i.sample <- matrix(NA,nrow=sampsize,ncol=length(var.names))
        colnames(attrib.i.sample) <- var.names
        attrib.i.sample[1,] <- as.numeric(attrib[i,var.names])
        for ( j in 1:length(var.names) )
        {
          par <- as.numeric(DefObsUnc[j,4:ncol(DefObsUnc)])
          par <- c(attrib.i.sample[1,j],par)
          if ( DefObsUnc[j,3] == "rel" ) par[2] <- par[2]*par[1]
          attrib.i.sample[2:sampsize,j] <- 
            msk.macrophytes.2017.sample(DefObsUnc[j,2],par,sampsize-1)
        }
        comb <- 
          msk.macrophytes.2017.get.comb(thresholds.indices,
                                        thresholds.offsets,
                                        thresholds.sample,
                                        attrib.i.sample)
        ind <- unique(comb)
        for ( j in 1:length(ind) ) comb.probs[i,ind[j]] <- comb.probs[i,ind[j]] + sum(comb==ind[j])
      }
    }
    comb.probs <- comb.probs/sampsize
    
    # calculate probabilities for (original) river types:
    
    types.names <- unique(types[,1])
    types.probs <- matrix(NA,ncol=length(types.names),nrow=nrow(attrib))
    colnames(types.probs) <- types.names
    rownames(types.probs) <- rownames(attrib)
    for ( j in 1:length(types.names) )
    {
      ind <- which(types[,1]==types.names[j])
      if ( length(ind) == 0 ) stop("ecoval::msk.macrophytes.2017.calc.types: unknown river type")
      if ( length(ind) == 1 )
      {
        types.probs[,j] <- comb.probs[,ind]
      }
      else
      {
        if ( nrow(types.probs) == 1 )
        {
          types.probs[1,j] <- sum(comb.probs[1,ind])
        }
        else
        {
          types.probs[,j] <- apply(comb.probs[,ind],1,sum)
        }
      }
    }
    
    # calculate probabilities for valuation river types:

    types.val.names <- unique(unlist(strsplit(types[,2],split=",")))
    types.val.probs <- matrix(0,ncol=length(types.val.names),nrow=nrow(attrib))
    colnames(types.val.probs) <- types.val.names
    rownames(types.val.probs) <- rownames(attrib)
    for ( j in 1:nrow(types) )
    {
      names <- strsplit(types[j,2],split=",")[[1]]
      l <- length(names)
      if ( l == 1 )
      {
        types.val.probs[,names] <- types.val.probs[,names] + comb.probs[,j]
      }
      else
      {
        for ( k in 1:l )
        {
          types.val.probs[,names[k]] <- types.val.probs[,names[k]] + comb.probs[,j]/l
        }
      }
    }
    
    # calculate probabilities for growthform river types:

    types.grfo.names <- unique(unlist(strsplit(types[,3],split=",")))
    types.grfo.probs <- matrix(0,ncol=length(types.grfo.names),nrow=nrow(attrib))
    colnames(types.grfo.probs) <- types.grfo.names
    rownames(types.grfo.probs) <- rownames(attrib)
    for ( j in 1:nrow(types) )
    {
      names <- strsplit(types[j,3],split=",")[[1]]
      l <- length(names)
      if ( l == 1 )
      {
        types.grfo.probs[,names] <- types.grfo.probs[,names] + comb.probs[,j]
      }
      else
      {
        for ( k in 1:l )
        {
          types.grfo.probs[,names[k]] <- types.grfo.probs[,names[k]] + comb.probs[,j]/l
        }
      }
    }
    
    # calculate probabilities for type scheme field indices:
    
    fields.names <- sort(unique(fields))
    fields.probs <- matrix(NA,ncol=length(fields.names),nrow=nrow(attrib))
    colnames(fields.probs) <- fields.names
    rownames(fields.probs) <- rownames(attrib)
    for ( j in 1:length(fields.names) )
    {
      ind <- which(fields==fields.names[j])
      if ( length(ind) == 0 ) stop("ecoval::msk.macrophytes.2017.calc.types: unknown river type field")
      if ( length(ind) == 1 )
      {
        fields.probs[,j] <- comb.probs[,ind]
      }
      else
      {
        if ( nrow(fields.probs) == 1 )
        {
          fields.probs[1,j] <- sum(comb.probs[1,ind])
        }
        else
        {
          fields.probs[,j] <- apply(comb.probs[,ind],1,sum)
        }
      }
    }
    
    # calculate types for valuation:
    
    types.val <- types[comb.obs,2]
    names(types.val) <- rownames(attrib)
    for ( i in 1:nrow(attrib) )
    {
      names <- strsplit(types.val[i],split=",")[[1]]
      if ( length(names) > 1 )
      {
        ind <- which.max(types.val.probs[i,names])
        types.val[i] <- names[ind]
      }
    }
    
    # calculate types of maximum probability:
    
    rows.available <- !apply(types.probs,1,anyNA)
    types.maxprob <- rep(NA,nrow(attrib))
    if ( sum(rows.available) > 0 ) 
    {
      if ( sum(rows.available) > 1 )
      {
        types.maxprob[rows.available] <- colnames(types.probs)[apply(types.probs[rows.available,],1,which.max)]
      }
      else
      {
        types.maxprob[rows.available] <- colnames(types.probs)[which.max(types.probs[rows.available,])]
      }
    }
    names(types.maxprob) <- rownames(attrib)
    
    # calculate valuation types of maximum probability:
    
    rows.available <- !apply(types.val.probs,1,anyNA)
    types.val.maxprob <- rep(NA,nrow(attrib))
    if ( sum(rows.available) > 0 )
    {
      if ( sum(rows.available) > 1 )
      {
        types.val.maxprob[rows.available] <- colnames(types.val.probs)[apply(types.val.probs[rows.available,],1,which.max)]
      }
      else
      {
        types.val.maxprob[rows.available] <- colnames(types.val.probs)[which.max(types.val.probs[rows.available,])]
      }
    }
    names(types.val.maxprob) <- rownames(attrib)
    
    # extend output:
    
    res$types.comb.probs     <- comb.probs
    res$types.scheme.probs   <- types.probs
    res$types.val.probs      <- types.val.probs
    res$types.grfo.probs     <- types.grfo.probs
    res$types.fields.probs   <- fields.probs
    res$types.val.obs        <- types.val
    res$types.scheme.maxprob <- types.maxprob
    res$types.val.maxprob    <- types.val.maxprob
  }

  res$types.table <- data.frame(res$types.scheme.obs)
  colnames(res$types.table) <- ecoval.translate("A_macrophytes_rivertypescheme_class",dict)
  if ( sampsize > 0 )
  {
    res$types.table <- data.frame(res$types.scheme.obs,
                                  res$types.scheme.maxprob,
                                  res$types.val.obs,
                                  res$types.val.maxprob,
                                  res$types.grfo.probs,
                                  res$types.val.probs,
                                  res$types.scheme.probs)
    colnames(res$types.table) <- c(paste(ecoval.translate("R_macrophytes_rivertype_scheme",dict),
                                         ecoval.translate("A_macrophytes_rivertypescheme_class",dict),
                                         sep="_"),
                                   paste(ecoval.translate("R_macrophytes_rivertype_schememaxprob",dict),
                                         ecoval.translate("A_macrophytes_rivertypescheme_class",dict),
                                         ecoval.translate("R_macrophytes_maxprobability",dict),
                                         sep="_"),
                                   paste(ecoval.translate("R_macrophytes_rivertype_val",dict),
                                         ecoval.translate("A_macrophytes_rivertypeval_class",dict),
                                         sep="_"),
                                   paste(ecoval.translate("R_macrophytes_rivertype_valmaxprob",dict),
                                         ecoval.translate("A_macrophytes_rivertypeval_class",dict),
                                         ecoval.translate("R_macrophytes_maxprobability",dict),
                                         sep="_"),
                                   paste(ecoval.translate("R_macrophytes_rivertype_probgrfo",dict),
                                         ecoval.translate("R_macrophytes_probability",dict),
                                         ecoval.translate("A_macrophytes_rivertypegrfo_class",dict),
                                         colnames(res$types.grfo.probs),
                                         sep="_"),
                                   paste(ecoval.translate("R_macrophytes_rivertype_probval",dict),
                                         ecoval.translate("R_macrophytes_probability",dict),
                                         ecoval.translate("A_macrophytes_rivertypeval_class",dict),
                                         colnames(res$types.val.probs),
                                         sep="_"),
                                   paste(ecoval.translate("R_macrophytes_rivertype_probscheme",dict),
                                         ecoval.translate("R_macrophytes_probability",dict),
                                         ecoval.translate("A_macrophytes_rivertypescheme_class",dict),
                                         colnames(res$types.scheme.probs),
                                         sep="_"))
  }
  
  return(res)
}


# msk.macrophytes.2017.plot.types.scheme <- function(res.calc.types,i,cex=1,...)
# {
#   row.no <- i
#   res    <- res.calc.types
#   
#   if ( is.na(match("language",names(res))) | is.na(match("dictionaries",names(res))) )
#     stop("ecoval::msk.macrophytes.2017.plot.types.scheme: list element \"language\" is missing")
#   if ( is.na(match("types.fields.probs",names(res))) )
#     stop("ecoval::msk.macrophytes.2017.plot.types.scheme: list element \"types.fields.probs\" is missing")
#   
#   site <- row.no
#   dict <- ecoval.dict(res$language,res$dictionaries)
#   
#   nrow = max(as.numeric(substring(colnames(res$types.fields.probs),1,1)))
#   ncol = max(as.numeric(substring(colnames(res$types.fields.probs),2,2)))
#   x <- (0:ncol)/ncol
#   y <- rev(0:nrow)/nrow
#   
#   plot(numeric(0),numeric(0),xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxs="i",yaxs="i",axes=FALSE,
#        ...)
#   
#   for ( i in 1:nrow )
#   {
#     for ( j in 1:ncol )
#     {
#       prob <- res$types.fields.probs[site,paste(i,j,sep="")]
#       polygon(x=c(x[j],x[j+1],x[j+1],x[j],x[j]),
#               y=c(y[i],y[i],y[i+1],y[i+1],y[i]),
#               col=ifelse(is.na(prob),"white",grey(1-0.5*prob)))  # grey between 0.5 and 1.0 to keep the text readable
#       types <- unique(res$typedef$types[,ecoval.translate("A_macrophytes_rivertypescheme_class",dict)][res$typedef$fields==paste(i,j,sep="")])
#       types <- c(types,paste(ifelse(is.na(prob),"",100*round(prob,3)),"%"))
#       text(x=0.5*(x[j]+x[j+1]),
#            y=0.5*(y[i]+y[i+1]) + 0.5*(y[i]-y[i+1])/length(types) * (((length(types):1)-1)-0.5*(length(types)-1)),
#            labels=types,adj=c(0.5,0.5),cex=cex)
#     }
#   }
# }
msk.macrophytes.2017.plot.types.scheme <- function(res.calc.types,i,cex=1,cex.labels=1,...)
{
  # local variables and checks:
  
  offset.x <- 2
  offset.y <- 1
  
  row.no <- i
  res    <- res.calc.types
  
  if ( is.na(match("language",names(res))) | is.na(match("dictionaries",names(res))) )
    stop("ecoval::msk.macrophytes.2017.plot.types.scheme: list element \"language\" is missing")
  if ( is.na(match("types.fields.probs",names(res))) )
    stop("ecoval::msk.macrophytes.2017.plot.types.scheme: list element \"types.fields.probs\" is missing")
  
  site <- row.no
  dict <- ecoval.dict(res$language,res$dictionaries)
  
  nrow = max(as.numeric(substring(colnames(res$types.fields.probs),1,1)))+offset.y
  ncol = max(as.numeric(substring(colnames(res$types.fields.probs),2,2)))+offset.x
  x <- (0:ncol)/ncol
  y <- rev(0:nrow)/nrow
  
  # plot area:
  
  plot(numeric(0),numeric(0),xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxs="i",yaxs="i",axes=FALSE,
       ...)
  
  # plot types and probabilities:
  
  for ( i in (offset.y+1):nrow )
  {
    for ( j in (offset.x+1):ncol )
    {
      prob <- res$types.fields.probs[site,paste(i-offset.y,j-offset.x,sep="")]
      polygon(x=c(x[j],x[j+1],x[j+1],x[j],x[j]),
              y=c(y[i],y[i],y[i+1],y[i+1],y[i]),
              col=ifelse(is.na(prob),"white",grey(1-0.5*prob)))  # grey between 0.5 and 1.0 to keep the text readable
      types <- unique(res$typedef$types[,ecoval.translate("A_macrophytes_rivertypescheme_class",dict)][res$typedef$fields==paste(i-offset.y,j-offset.x,sep="")])
      types <- c(types,paste(ifelse(is.na(prob),"",100*round(prob,3)),"%"))
      text(x=0.5*(x[j]+x[j+1]),
           y=0.5*(y[i]+y[i+1]) + 0.5*(y[i]-y[i+1])/length(types) * (((length(types):1)-1)-0.5*(length(types)-1)),
           labels=types,adj=c(0.5,0.5),cex=cex)
    }
  }
  
  # plot labels:
  
  col.labels <- ecoval.translate("R_macrophytes_rivertype_collabels",dict)
  row.labels <- ecoval.translate("R_macrophytes_rivertype_rowlabels",dict)
  col.labels <- strsplit(unlist(strsplit(col.labels,split=";")),split=",")
  row.labels <- strsplit(unlist(strsplit(row.labels,split=";")),split=",")
  
  h.cell <- offset.x/nrow
  n.rows <- length(col.labels)
  for ( i in 1:n.rows ) 
  {
    for ( j in 1:length(col.labels[[i]]) ) text(x=0.5*ifelse(j==1,x[j]+x[j+offset.x],x[j+offset.x-1]+x[j+offset.x]),y=y[1]-i*(y[1]-y[1+offset.y])/(n.rows+1),labels=col.labels[[i]][j],cex=cex.labels)
  }
  h.cell <- 1/nrow
  n.rows <- length(row.labels[[1]])
  for ( i in 1:length(row.labels) )
  {
    for ( j in 1:length(row.labels[[i]]) ) text(x=0.5*(x[1]+x[1+offset.x]),y=y[i+offset.y]-j*h.cell/(n.rows+1),labels=row.labels[[i]][j],cex=cex.labels)
  }
}

  
msk.macrophytes.2017.plot.types.grfo <- function(res.calc.types,i,...)
{
  row.no <- i
  res    <- res.calc.types

  if ( is.na(match("language",names(res))) | is.na(match("dictionaries",names(res))) )
    stop("ecoval::msk.macrophytes.2017.plot.types.grfo: list element \"language\" is missing")
  if ( is.na(match("types.grfo.probs",names(res))) )
    stop("ecoval::msk.macrophytes.2017.plot.types.grfo: list element \"types.grfo.probs\" is missing")
  
  dict <- ecoval.dict(res$language,res$dictionaries)
  
  barplot(100*res$types.grfo.probs[row.no,],ylim=c(0,100),
          ylab=paste(ecoval.translate("R_macrophytes_probability",dict),"[%]"),
          border    = FALSE,
          col       = c(rgb(148, 32,146, maxColorValue = 255),
                        rgb( 49,132,155, maxColorValue = 255),
                        rgb(123, 64, 55, maxColorValue = 255),
                        rgb(255,209,142, maxColorValue = 255)),
          ...)
  abline(h = 0)
}


msk.macrophytes.2017.plot.typedef <- function(res.calc.types,max.x=NA,max.y=NA,...)
{
  res <- res.calc.types
  
  calcpdf <- function(x,distpar,log=FALSE)
  {
    if ( distpar[1] == "Delta" | distpar[1] == "delta" )
    {
      # delta distribution; parameter is its location
      loc <- as.numeric(distpar[2])
      return(ifelse(x==loc,Inf,0))
    }
    if ( distpar[1] == "Uniform" | distpar[1] == "uniform" )
    {
      # uniform distribution; parameters are min and max
      min <- as.numeric(distpar[2])
      max <- as.numeric(distpar[3])
      return(dunif(x,min=min,max=max,log=log))
    }
    if ( distpar[1] == "Normal" | distpar[1] == "normal" )
    {
      # normal distribution; parameters are mean and sd:
      mean <- as.numeric(distpar[2])
      sd   <- as.numeric(distpar[3])
      return(dnorm(x,mean=mean,sd=sd,log=log))
    }
    if ( distpar[1] == "NormalTrunc" | distpar[1] == "normaltrunc" )
    {
      # truncated normal distribution; parameters are mean, sd, min and max
      mean <- as.numeric(distpar[2])
      sd   <- as.numeric(distpar[3])
      min  <- as.numeric(distpar[4])
      max  <- as.numeric(distpar[5])
      fact <- 1/(pnorm(q=max,mean=mean,sd=sd)-pnorm(q=min,mean=mean,sd=sd))
      if ( !log )
      {
        return(ifelse(x<min|x>max,0,fact*dnorm(x,mean=mean,sd=sd)))
      }
      else
      {
        return(ifelse(x<min|x>max,NA,
                      log(fact)+dnorm(x,mean=mean,sd=sd,log=TRUE)))
      }
    }
    if ( distpar[1] == "Lognormal" | distpar[1] == "lognormal")
    {
      # lognormal distribution; parameters are mean and sd;
      # R parameters are mean and sd of the log of the random variable
      mean    <- as.numeric(distpar[2])
      sd      <- as.numeric(distpar[3])
      sdlog   <- sqrt(log(1+sd^2/mean^2))
      meanlog <- log(mean) - 0.5*sdlog^2
      return(dlnorm(x,meanlog=meanlog,sdlog=sdlog,log=log))
    }
    if ( distpar[1] == "LognormalTrunc" | distpar[1] == "lognormaltrunc" )
    {
      # truncated lognormal distribution; parameters are mean, sd, min and max;
      # R parameters are mean and sd of the log of the random variable
      mean    <- as.numeric(distpar[2])
      sd      <- as.numeric(distpar[3])
      sdlog   <- sqrt(log(1+sd^2/mean^2))
      meanlog <- log(mean) - 0.5*sdlog^2
      min     <- as.numeric(distpar[4])
      max     <- as.numeric(distpar[5])
      fact <- 1/(plnorm(q=max,meanlog=meanlog,sdlog=sdlog)-plnorm(q=min,meanlog=meanlog,sdlog=sdlog))
      if ( !log )
      {
        return(ifelse(x<min|x>max,0,fact*dlnorm(x,meanlog=meanlog,sdlog=sdlog)))
      }
      else
      {
        return(ifelse(x<min|x>max,NA,
                      log(fact)+dlnorm(x,meanlog=meanlog,sdlog=sdlog,log=TRUE)))
      }
    }
    if ( distpar[1] == "Inv" | distpar[1] == "inv" )
    {
      # inverse distribution; parameters are min and max:
      min <- as.numeric(distpar[2])
      max <- as.numeric(distpar[3])
      if ( !log )
      {
        return(ifelse(x<min|x>max,0,1/(log(max/min)*x)))   
      }
      else
      {
        return(ifelse(x<min|x>max,NA,-log(log(max/min)) - log(x)))   
      }
    }
    if ( distpar[1] == "Exponential" | distpar[1] == "exponential" )
    {
      # exponential distribution; parameter is mean:
      mean <- as.numeric(distpar[2])
      if ( !log )
      {
        return(ifelse(x<0,0,1/mean*exp(-x/mean)))   
      }
      else
      {
        return(ifelse(x<0,NA,-log(mean)-x/mean))   
      }
    }
    stop(paste("ecoval::msk.macrophytes.2017.plot.typedef: distribution",distpar[1],"not yet implemented"))
  }
  
  fact.max.x <- 2.0
  fact.max.y <- 1.1
  n.plot     <- 501
  x.obs.rel  <- c(0.075,0.333,0.667)
  
  variables        <- names(res$typedef$thresholds.offsets)
  thresholds       <- res$typedef$thresholds
  thresholds.unc   <- res$typedef$thresholds.unc
  observations.unc <- res$typedef$observations.unc
  
  for ( i in 1:length(variables) )
  {
    v     <- variables[i]
    t     <- thresholds[1,colnames(thresholds)==v]
    t.unc <- thresholds.unc[thresholds.unc[,1]==v,-1]
    o.unc <- observations.unc[observations.unc[,1]==v,-1]
    n.lim <- length(t)
    mn.x <- 0
    mx.x <- max.x[v]
    if ( is.na(mx.x) )
    { 
      mx.x <- t[n.lim]
      if ( mx.x == Inf ) mx.x <- fact.max.x*t[n.lim-1]
    }
    x     <- mn.x +(0:(n.plot-1))/(n.plot-1)*(mx.x-mn.x)
    mn.y <- 0
    mx.y <- max.y[v]
    calc.max.y <- FALSE; if ( is.na(mx.y) ) calc.max.y <- TRUE
    LimitsDensities <- list()
    legend <- rep("",n.lim)
    for ( j in 1:n.lim )
    {
      LimitsDensities[[j]] <- calcpdf(x,t.unc[j,])
      if ( calc.max.y ) mx.y <- max(c(mx.y,fact.max.y*LimitsDensities[[j]][LimitsDensities[[j]]!=Inf]),na.rm=TRUE)
      #legend[j] <- paste("Lim",j,": ",paste(TypesDef$LimitsUnc[[paste(v,j,sep="_")]],collapse=","),sep="")
      legend[j] <- paste("Lim",j)
    }
    ObsDensities <- list()
    ObsMean      <- mn.x + x.obs.rel*(mx.x-mn.x)
    for ( j in 1:length(x.obs.rel) )
    {
      distpar <- as.character(o.unc)
      if ( distpar[2] == "rel" ) distpar[3] <- ObsMean[j]*as.numeric(distpar[3])
      distpar[2] <- ObsMean[j]
      ObsDensities[[j]] <- calcpdf(x,distpar)
      if ( calc.max.y ) mx.y <- max(c(mx.y,fact.max.y*ObsDensities[[j]]))
    }
    plot(numeric(0),numeric(0),main=variables[i],xaxs="i",yaxs="i",
         xlim=c(mn.x,mx.x),ylim=c(mn.y,mx.y),xlab=variables[i],ylab="f",...)
    for ( j in 1:n.lim )
    {
      lines(x,LimitsDensities[[j]],lty=j,lwd=2)
      abline(v=t[j],lty=j,lwd=2)
    }
    for ( j in 1:length(x.obs.rel) )
    {
      lines(x,ObsDensities[[j]],col="red")
      abline(v=ObsMean[j],col="red")
    }
    #legend <- c(legend,paste("Obs:  ",paste(TypesDef$ObsUnc[[v]],collapse=","),sep=""))
    legend <- c(legend,"Obs")
    legend("topright",
           legend=legend,
           lty=c(1:n.lim,1),
           lwd=c(rep(2,n.lim),1),
           col=c(rep("black",n.lim),"red"),
           cex=0.75)
  }
}


