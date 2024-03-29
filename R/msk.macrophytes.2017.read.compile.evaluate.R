msk.macrophytes.2017.read.compile.evaluate <- function(file.site,
                                                       pic.folder        = ".",
                                                       file.species      = NA,
                                                       file.typeplaus    = NA,
                                                       sampling.protocol = "v2018",
                                                       sampsize          = 10000,
                                                       file.res          = NA,
                                                       file.doc          = NA,
                                                       file.taxa.used    = NA,
                                                       file.taxa.removed = NA,
                                                       file.check.msg    = NA,
                                                       sep               = "\t",
                                                       sep.in            = NA,
                                                       sep.out           = NA,
                                                       language          = "English",
                                                       dictionaries      = NA)
{
  # functions
  # ---------
  
  # function to add prefix to file name:
  
  prepend.filename <- function(prefix, filename, sep="_")
  {
    file.path(dirname(filename), paste0(prefix, sep, basename(filename)))
  }

  # function to compress values table:
  
  val.compact.table <- function(val,
                                language     = "English",
                                dictionaries = NA)
  {
    colnames <- colnames(val)[-1]  # eliminate overarching goal
    code.mb <- paste(" ",ecoval.translate("N_macrophytes_bryophytesriver",dict),sep="")
    char.mb <- nchar(code.mb)
    colnames <- unique(ifelse(substring(colnames,nchar(colnames)-char.mb+1,nchar(colnames)) == code.mb,
                              substring(colnames,1,nchar(colnames)-char.mb),
                              colnames))
    val.compact <- matrix(NA,ncol=length(colnames),nrow=nrow(val))
    colnames(val.compact) <- colnames
    for ( j in 1:length(colnames) )
    {
      if ( !is.na(match(colnames[j],colnames(val))) )
      {
        val.compact[,j] <- val[,colnames[j]]
      }
      if ( !is.na(match(paste(colnames[j],code.mb,sep=""),colnames(val))) )
      {
        val.compact[,j] <- ifelse(!is.na(val.compact[,j]),val.compact[,j],val[,paste(colnames[j],code.mb,sep="")])
      }
    }
    return(val.compact)
  }

  # initialization
  # --------------
  
  if ( length(pic.folder) == 0 ) pic.folder <- "."
  if ( is.na(pic.folder) )       pic.folder <- "."
  
  if ( is.na(sep.in) )  sep.in  <- sep
  if ( is.na(sep.out) ) sep.out <- sep
  
  # initialize result list:
  
  res <- list()
  res$language     <- language
  res$dictionaries <- dictionaries
  res$valfun.macrophytes <- msk.macrophytes.2017.create(language=language,dictionaries=dictionaries)
  
  # get dictionary:
  
  dict <- ecoval.dict(language,dictionaries)

  # read and compile data
  # ---------------------

  # read and compile site data
  
  data.site <- read.csv(file.site,
                        header = TRUE,
                        na.strings = c("","NA","Na","na"),
                        stringsAsFactors = FALSE,
                        sep = sep.in)
  
  morphol <- msk.morphol.1998.create(language,dictionaries)
  val.morphol <- evaluate(morphol,data.site)
  colnames(val.morphol) <- gsub(" ","_",colnames(val.morphol))
  data.site <- data.frame(data.site,val.morphol,stringsAsFactors=FALSE)
  
  res.site  <- msk.macrophytes.2017.compile.sitedat(data.site,language,dictionaries)  
  res <- append(res,res.site)
  msg <- res.site$site.quality
  
  # read and compile species data (if available):
  
  if ( !is.na(file.species) )
  {
    data.species <- read.csv(file.species,
                             header = TRUE,
                             na.strings = c("","NA","Na","na","-999","-999.00","-999,00"),
                             stringsAsFactors = FALSE,
                             sep = sep.in)
  
    res.species <- msk.macrophytes.2017.compile.speciesdat(data.species,
                                                           sampling.protocol = sampling.protocol,
                                                           language          = language,
                                                           dictionaries      = dictionaries)
    res <- append(res,res.species)
    msg <- rbind(msg,res.species$species.quality)

    if ( !is.data.frame(res$data.site) | !is.data.frame(res$species.assess) )
    {
      if ( !is.na(file.check.msg) )
      {  
        write.table(msg,
                    file      = file.check.msg,
                    sep       = sep.out,
                    na        = "",
                    quote     = FALSE,
                    row.names = FALSE,
                    col.names = TRUE)
      }
      return(res)
    }
    
    if ( !is.na(file.taxa.used) )
    {
      write.table(res$species.assess,  # species used for assessment
                  file      = file.taxa.used,
                  sep       = sep.out,
                  na        = "",
                  quote     = FALSE,
                  row.names = FALSE,
                  col.names = TRUE)
    }
    if ( !is.na(file.taxa.removed) )
    {
      write.table(res$species.removed,       # taxa removed because of insuffcient determination or not on taxa list
                  file      = file.taxa.removed,  
                  sep       = sep.out,
                  na        = "",
                  quote     = FALSE,
                  row.names = FALSE,
                  col.names = TRUE)
    }
    
    # read plausibilization data (if available)
    
    if ( !is.na(file.typeplaus) )
    {
      data.typeplaus <- read.csv(file.typeplaus,
                                 header = TRUE,
                                 na.strings = c("","NA"),
                                 stringsAsFactors = FALSE,
                                 sep = sep.in)
      req.colnames <- c(ecoval.translate("A_macrophytes_site_siteid",dict),
                        ecoval.translate("A_macrophytes_site_samplingdate",dict),
                        ecoval.translate("A_macrophytes_rivertypescheme_plaus_class",dict),
                        ecoval.translate("A_macrophytes_rivertypeval_plaus_class",dict),
                        paste(ecoval.translate("A_macrophytes_rivertype_plaus_class",dict),ecoval.translate("R_macrophytes_comment",dict),sep="_"),
                        ecoval.translate("R_macrophytes_plaus_changetype_suggestions",dict),
                        ecoval.translate("R_macrophytes_plaus_changetype_highprob",dict),
                        ecoval.translate("R_macrophytes_plaus_changetype_poorvegsubstrate",dict),
                        ecoval.translate("R_macrophytes_plaus_changetype_mossartsubst",dict),
                        ecoval.translate("R_macrophytes_plaus_changetype_wrongsitedata",dict),
                        ecoval.translate("R_macrophytes_plaus_changetype_upstreaminfluence",dict),
                        ecoval.translate("R_macrophytes_plaus_changetype_intermittentriver",dict),
                        ecoval.translate("R_macrophytes_plaus_changetype_highdischargevariability",dict),
                        ecoval.translate("R_macrophytes_plaus_changetype_limitstypescheme",dict),
                        ecoval.translate("R_macrophytes_plaus_changetype_multiplesampling",dict),
                        ecoval.translate("R_macrophytes_plaus_changetype_justification",dict),
                        ecoval.translate("R_macrophytes_plaus_keeptype_mostplausible",dict),
                        ecoval.translate("R_macrophytes_plaus_keeptype_bettervalhumanimpact",dict),
                        ecoval.translate("R_macrophytes_plaus_keeptype_substrateuncertain",dict))
      ind.req <- match(req.colnames,colnames(data.typeplaus))
      if ( anyNA(ind.req) )
      {
        msg <- rbind(msg,
                     c("","","",
                       paste(paste(ecoval.translate("R_macrophytes_error_plauscolmissing",dict),":",sep=""),
                             paste(req.colnames[is.na(ind.req)],collapse=", ")),
                       ecoval.translate("R_macrophytes_error_error",dict),""))
      }
      else
      {
        # match sample ids:
        rownames(data.typeplaus) <- get.site.sampleid(data.typeplaus[,ecoval.translate("A_macrophytes_site_siteid",dict)],
                                                      data.typeplaus[,ecoval.translate("A_macrophytes_site_samplingdate",dict)])
        data.typeplaus <- data.typeplaus[res$data.site[,ecoval.translate("A_macrophytes_site_sampleid",dict)],]
        res$types.plaus=data.typeplaus[,req.colnames]
      }
    }
    
    # calculate attributes:
    
    res$attrib.species <- msk.macrophytes.2017.calc.attrib(res$data.site,
                                                           res$species.assess,
                                                           language     = language,
                                                           dictionaries = dictionaries)
  }  
  
  # write error messages
  # --------------------
  
  if ( !is.na(file.check.msg) )
  {  
    write.table(msg,
                file      = file.check.msg,
                sep       = sep.out,
                na        = "",
                quote     = FALSE,
                row.names = FALSE,
                col.names = TRUE)
  }
  if ( !is.data.frame(res$data.site) ) return(res)

  # return input and, potentially, attributes if sampsize == 0 (read data only)
  # ---------------------------------------------------------------------------
  
  if ( sampsize < 1 )
  {
    output <- res$data.site
    if ( !is.na(match("attrib.species",names(res))) )
    {
      output <- data.frame(output,res$attrib.species)
    }
    if ( !is.na(file.res) )
    {
      write.table(output,
                  file      = file.res,
                  sep       = sep.out,
                  na        = "",
                  row.names = FALSE,
                  col.names = TRUE)
    }
    if ( !is.na(file.doc) )
    {
      for ( i in 1:nrow(res$data.site) ) 
      {
        pdf(prepend.filename(get.site.sampleid(res$data.site[i,ecoval.translate("A_macrophytes_site_siteid",dict)],
                                               res$data.site[i,ecoval.translate("A_macrophytes_site_samplingdate",dict)]),
                             file.doc),
            height=11.69,width=8.27)
        msk.macrophytes.2017.doc.site(res=res,row.no=i,pic.folder=pic.folder)
        dev.off()
      }
    }
    return(res)
  }
  
  # calculate river types
  # ---------------------

  res.types <- msk.macrophytes.2017.calc.types(attrib       = res$data.site,
                                               sampsize     = sampsize,
                                               language     = language,
                                               dictionaries = dictionaries)
  res <- append(res,res.types)
  
  # return river types if no species data are available:
  
  if ( is.na(file.species) )
  {
    if ( !is.na(file.res) )
    {
      write.table(data.frame(res$data.site,res$types.table),
                  file      = file.res,
                  sep       = sep.out,
                  na        = "",
                  row.names = FALSE,
                  col.names = TRUE)
    }
    if ( !is.na(file.doc) )
    {
      for ( i in 1:nrow(res$data.site) ) 
      {
        pdf(prepend.filename(get.site.sampleid(res$data.site[i,ecoval.translate("A_macrophytes_site_siteid",dict)],
                                               res$data.site[i,ecoval.translate("A_macrophytes_site_samplingdate",dict)]),
                             file.doc),
            height=11.69,width=8.27)
        msk.macrophytes.2017.doc.site(res=res,row.no=i,pic.folder=pic.folder)
        msk.macrophytes.2017.doc.typology(res=res,row.no=i)
        dev.off()
      }
    }
    return(res)
  }
  
  # calculate valuation based on calculated river type
  # --------------------------------------------------
  
  input.eval <- data.frame(res$types.val.obs,res$attrib.species,stringsAsFactors=FALSE)
  colnames(input.eval)[1] <- ecoval.translate("A_macrophytes_rivertype_class",dict)
  val <- evaluate(res$valfun.macrophytes,input.eval)
  res$val <- val
  
  # calculate valuations for all river types:
  
  types <- unique(res$typedef$types[,ecoval.translate("A_macrophytes_rivertypeval_class",dict)])   # get type names for val.
  types <- types[types!=ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict) &   # remove VA and A
                 types!=ecoval.translate("L_macrophytes_rivertype_class_large",dict)]
  types <- types[-grep(",",types)] # remove elements with comma
  res$val.types.alllevels <- list()
  res$val.types <- matrix(NA,ncol=length(types),nrow=nrow(res$attrib.species))
  colnames(res$val.types) <- types
  for ( i in 1:length(types) )
  {
    input.eval <- data.frame(rep(types[i],nrow(res$attrib.species)),res$attrib.species,stringsAsFactors=FALSE)
    colnames(input.eval)[1] <- ecoval.translate("A_macrophytes_rivertype_class",dict)
    res$val.types.alllevels[[types[i]]] <- evaluate(res$valfun.macrophytes,input.eval)
    res$val.types[,i] <- res$val.types.alllevels[[types[i]]][,1]
  }
  
  # sort for decreasing probability:
  
  res$val.types.decreasingprob <- rep("",nrow(res$val.types))
  for ( i in 1:nrow(res$val.types) )
  {
    if ( !anyNA(res$types.val.probs[i,]) )
    {
      ind <- order(res$types.val.probs[i,],decreasing=TRUE)
      val.types <- cbind(res$val.types,rep(NA,nrow(res$val.types)),rep(NA,nrow(res$val.types)))
      colnames(val.types)[(ncol(val.types)-1):ncol(val.types)] <- c(ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict),
                                                                    ecoval.translate("L_macrophytes_rivertype_class_large",dict))
      res$val.types.decreasingprob[i] <- paste(paste(colnames(res$types.val.probs)[ind],
                                                     round(val.types[i,colnames(res$types.val.probs)[ind]],2),
                                                     100*round(res$types.val.probs[i,ind],2),sep="_"),
                                               collapse=" | ")
    }
  }
  
  # sort for decreasing valuation, for probability >= 0.1:
  
  res$val.types.decreasingval <- rep("",nrow(res$val.types))
  for ( i in 1:nrow(res$types.val.probs) )
  {
    if ( !anyNA(res$types.val.probs[i,]) )
    {
      ind <- order(res$val.types[i,],decreasing=TRUE)
      Pge10 <- res$types.val.probs[i,colnames(res$val.types)[ind]] >= 0.1
      if ( sum(Pge10) > 0 )
      {
        res$val.types.decreasingval[i] <- paste(paste(colnames(res$val.types)[ind[Pge10]],
                                                      round(res$val.types[i,ind[Pge10]],2),
                                                      100*round(res$types.val.probs[i,colnames(res$val.types)[ind[Pge10]]],2),sep="_"),
                                                collapse=" | ")
      }
      if ( res$types.val.probs[i,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] >= 0.1 )
      {
        if (  res$val.types.decreasingval[i] == "" )
        {
          res$val.types.decreasingval[i] <- paste(ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict),
                                                  100*round(res$types.val.probs[i,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)],2),
                                                  sep="__")
        }
        else
        {
          res$val.types.decreasingval[i] <- paste(res$val.types.decreasingval[i],
                                                  paste(ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict),
                                                        100*round(res$types.val.probs[i,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)],2),
                                                        sep="__"),
                                                  sep=" | ")
        }
      }
      if ( res$types.val.probs[i,ecoval.translate("L_macrophytes_rivertype_class_large",dict)] >= 0.1 )
      {
        if (  res$val.types.decreasingval[i] == "" )
        {
          res$val.types.decreasingval[i] <- paste(ecoval.translate("L_macrophytes_rivertype_class_large",dict),
                                                  100*round(res$types.val.probs[i,ecoval.translate("L_macrophytes_rivertype_class_large",dict)],2),
                                                  sep="__")
        }
        else
        {
          res$val.types.decreasingval[i] <- paste(res$val.types.decreasingval[i],
                                                  paste(ecoval.translate("L_macrophytes_rivertype_class_large",dict),
                                                        100*round(res$types.val.probs[i,ecoval.translate("L_macrophytes_rivertype_class_large",dict)],2),
                                                        sep="__"),
                                                  sep=" | ")
        }
      }
    }
  }
  
  # create output table
  # -------------------
  
  val <- val.compact.table(res$val,language=language,dictionaries=dictionaries)
  colnames(val) <- gsub(" ","_",colnames(val))
  colnames(val) <- paste(ecoval.translate("R_macrophytes_value_obs",dict),colnames(val),sep="_")
  val.types <- res$val.types
  colnames(val.types) <- paste(ecoval.translate("R_macrophytes_value_types",dict),colnames(val.types),sep="_")
  attrib.species <- res$attrib.species
  colnames(attrib.species) <- paste(ecoval.translate("R_macrophytes_attrib_calc",dict),colnames(attrib.species),sep="_")
  res.table <- data.frame(res$data.site,
                          res$types.table,
                          attrib.species,
                          val,
                          res$val.types.decreasingprob,
                          res$val.types.decreasingval,
                          val.types,
                          stringsAsFactors = FALSE)
  colnames(res.table)[(ncol(res.table)-1):ncol(res.table)-ncol(val.types)] <- 
    c(ecoval.translate("R_macrophytes_value_decreasingprob",dict),
      ecoval.translate("R_macrophytes_value_decreasingval",dict))
  
  # perform plausibility checks
  # ---------------------------
  
  res$plaus.crit <- msk.macrophytes.2017.plaus.crit(res,
                                                    language     = language,
                                                    dictionaries = dictionaries)
  
  # calculate valuation based on plausibilized river type
  # -----------------------------------------------------

  if ( !is.na(match("types.plaus",names(res))) )
  {
    res$plaus.crit[,req.colnames[-c(1,2)]] <- res$types.plaus[,req.colnames[-c(1,2)]]
    types.scheme.plaus <- res$types.plaus[,ecoval.translate("A_macrophytes_rivertypescheme_plaus_class",dict)]
    types.val.plaus <- res$types.plaus[,ecoval.translate("A_macrophytes_rivertypeval_plaus_class",dict)]
    res$types.scheme.final <- ifelse(is.na(types.scheme.plaus),res$types.scheme.obs,types.scheme.plaus)
    types.val.final <- res$types.scheme.final
    types.val <- colnames(res$types.val.probs)
    for ( i in 1:length(types.val.final) )
    {
      if ( ! types.val.final[i] %in% types.val )
      {
        ind <- which(res$typedef$types[,1]==types.val.final[i])
        if ( length(ind) == 0 )
        {
          types.val.final[i] <- NA
        }
        else
        {
          types <- unlist(strsplit(paste(res$typedef$types[ind,2],collapse=","),split=","))
          types.val.final[i] <- types[which.max(res$types.val.probs[i,types])]
        }
      }
    }
    res$types.val.final <- ifelse(is.na(types.val.plaus),types.val.final,types.val.plaus)
    input.eval <- data.frame(res$types.val.final,res$attrib.species,stringsAsFactors=FALSE)
    colnames(input.eval)[1] <- ecoval.translate("A_macrophytes_rivertype_class",dict)
    val <- evaluate(res$valfun.macrophytes,input.eval)
    res$val.final <- val
  }
  
  # main output table
  
  if ( !is.na(file.res) )
  {
    output <- data.frame(res.table,res$plaus.crit)
    if ( is.na(match("types.plaus",names(res))) )
    {
      attrib <- data.frame(res$types.val.obs,res$attrib.species)     # attributes for future evaluation; observed river type
    }
    else
    {
      val.final <- val.compact.table(res$val.final,language=language,dictionaries=dictionaries)
      colnames(val.final) <- gsub(" ","_",colnames(val.final))
      colnames(val.final) <- paste(ecoval.translate("R_macrophytes_value_final",dict),colnames(val.final),sep="_")
      output <- data.frame(output,res$types.scheme.final,res$types.val.final,val.final,stringsAsFactors=FALSE)
      ind.start <- ncol(output)-ncol(val.final)-1
      colnames(output)[ind.start+0:1] <- c(ecoval.translate("A_macrophytes_rivertypescheme_final_class",dict),
                                           ecoval.translate("A_macrophytes_rivertypeval_final_class",dict))
      attrib <- data.frame(res$types.val.final,res$attrib.species)   # attributes for future evaluation; final river type
    }
    colnames(attrib)[1] <- ecoval.translate("A_macrophytes_rivertype_class",dict)
    output <- data.frame(output,attrib,stringsAsFactors=FALSE)
    write.table(output,
                file      = file.res,
                sep       = sep.out,
                na        = "",
                quote     = FALSE,
                row.names = FALSE,
                col.names = TRUE)
  }
  if ( !is.na(file.doc) )
  {
    for ( i in 1:nrow(res$data.site) ) 
    {
      pdf(prepend.filename(get.site.sampleid(res$data.site[i,ecoval.translate("A_macrophytes_site_siteid",dict)],
                                             res$data.site[i,ecoval.translate("A_macrophytes_site_samplingdate",dict)]),
                           file.doc),
          height=11.69,width=8.27)
      msk.macrophytes.2017.doc.site(res=res,row.no=i,pic.folder=pic.folder)
      msk.macrophytes.2017.doc.vegetation(res=res,row.no=i)
      msk.macrophytes.2017.doc.typology(res=res,row.no=i)
      msk.macrophytes.2017.doc.valuation(res=res,row.no=i)
      dev.off()
    }
  }
  
  return(res)
}

msk.macrophytes.2017.plot.hierarchy <- function(res,i,final=TRUE,...)
{
  if ( is.na(match("language",names(res))) | is.na(match("dictionaries",names(res))) ) return()
  if ( !final & is.na(match("types.val.obs",names(res))) )   return()
  if ( final  & is.na(match("types.val.final",names(res))) ) return()
  
  dict <- ecoval.dict(res$language,res$dictionaries)
  
  moss      <- c(ecoval.translate("L_macrophytes_rivertype_class_smallbryophyte",dict),
                 ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict),
                 ecoval.translate("L_macrophytes_rivertype_class_largebryophyte",dict),
                 ecoval.translate("L_macrophytes_rivertype_class_verylargebryophyte",dict))
  
  top.node <- ecoval.translate("N_macrophytes_goodstate",dict)
  val      <- res$val[i,]
  if ( !final )
  {
    if ( res$types.val.obs[i] %in% moss ) top.node <- paste(ecoval.translate("N_macrophytes_goodstate",dict),
                                                            ecoval.translate("N_macrophytes_bryophytesriver",dict))
  }
  else
  {
    val <- res$val.final[i,]
    if ( res$types.val.final[i] %in% moss ) top.node <- paste(ecoval.translate("N_macrophytes_goodstate",dict),
                                                              ecoval.translate("N_macrophytes_bryophytesriver",dict))
  }
  plot(res$valfun.macrophytes,val,nodes=top.node,two.lines=TRUE,...)
}


