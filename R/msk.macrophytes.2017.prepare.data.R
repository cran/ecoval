# ========================================================================================
# This script contains two functions to validate and compile the site and species data
# provided in the templatesas well as a function to calculate the vegetation attributes
# needed for the assessment. The two functions for compiling the site and species data also
# provide initial quality checks for all parameters needed within the tool, and for
# clean-up of the species data axccording to the official taxalist and quality of
# determination.
# 
# Required Input as R data frames:
# data.site    = Abiotic data collected in sites, user provided in official template
# data.species = Biotic data of taxa collected in sites, user provided in official template
#
# Required Input provided as part of ECOVAL package:
# taxalist.dat = Oficial taxalist
#
# Functions:
# msk.macrophytes.2017.compile.sitedat
# msk.macrophytes.2017.compile.speciesdat
# msk.macrophytes.2017.calc.attrib
# 
# Authors:
# Christian Michel and Peter Reichert, EAWAG, Duebendorf, Switzerland
# =======================================================================================


# =======================================
# FUNCTION TO READ AND VALIDATE SITE DATA
# =======================================

msk.macrophytes.2017.compile.sitedat <- function(data.site,
                                                 language     = "English",
                                                 dictionaries = NA)
{
  # get dictionary:
  
  dict <- ecoval.dict(language,dictionaries)
  
  
  # Create data.frame to collect information on quality checks
  site.quality <- matrix(NA, nrow = 0, ncol = 5)
  colnames(site.quality) <- c(ecoval.translate("A_macrophytes_site_siteid",dict),
                              ecoval.translate("A_macrophytes_site_samplingdate",dict),
                              ecoval.translate("R_macrophytes_error_parameter",dict),
                              ecoval.translate("R_macrophytes_error_message",dict),
                              ecoval.translate("R_macrophytes_error_type",dict))
  
  # check if all required columns are contained in data.site
  req.colnames <- c(ecoval.translate("A_macrophytes_site_siteid",dict),
                    ecoval.translate("A_macrophytes_site_samplingdate",dict),
                    #ecoval.translate("A_macrophytes_site_canton",dict),
                    ecoval.translate("A_macrophytes_site_discharge_lpers",dict),
                    #ecoval.translate("A_macrophytes_site_discharge_source",dict),
                    ecoval.translate("A_macrophytes_site_slope_percent",dict),
                    #ecoval.translate("A_macrophytes_site_slope_source",dict),
                    ecoval.translate("A_macrophytes_site_waterdepth_m",dict),
                    ecoval.translate("A_macrophytes_site_shading_percent",dict),
                    ecoval.translate("A_macrophytes_site_substrate_above40_percent", dict),
                    ecoval.translate("A_macrophytes_site_substrate_20to40_percent", dict),
                    ecoval.translate("A_macrophytes_site_substrate_6to20_percent", dict),
                    ecoval.translate("A_macrophytes_site_substrate_2to6_percent", dict),
                    ecoval.translate("A_macrophytes_site_substrate_0to2_percent", dict),
                    ecoval.translate("A_macrophytes_site_substrate_sand_percent", dict),
                    ecoval.translate("A_macrophytes_site_substrate_siltclay_percent", dict),
                    ecoval.translate("A_macrophytes_site_substrate_mud_percent", dict),
                    ecoval.translate("A_macrophytes_site_substrate_humuspeat_percent", dict),
                    ecoval.translate("A_macrophytes_site_substrate_artificial_percent", dict))
  ind.req <- match(req.colnames,colnames(data.site))
  if ( anyNA(ind.req) )
  {
    site.quality <- rbind(site.quality,
                          c("","","",
                            paste(paste(ecoval.translate("R_macrophytes_error_sitescolmissing", dict),":",sep=""),
                                  paste(req.colnames[is.na(ind.req)],collapse=", ")),
                            ecoval.translate("R_macrophytes_error_error", dict)))
    
    # ERGAENZT: Complement "site.quality" data.frame with data-set information
    site.quality <- cbind(site.quality, rep(ecoval.translate("R_macrophytes_error_sitedate", dict), nrow(site.quality)))
    colnames(site.quality)[ncol(site.quality)] <- ecoval.translate("R_macrophytes_error_dataset", dict)
    
    # Return results
    return(list(data.site = NA,
                site.quality = site.quality))
    
  } else {
    # Eliminate rows without site and data entry:
    data.site <- data.site[!is.na(data.site[,ecoval.translate("A_macrophytes_site_siteid",dict)]) & !is.na(data.site[,ecoval.translate("A_macrophytes_site_samplingdate",dict)]),]
    if ( nrow(data.site) == 0 )
    {
      site.quality <- rbind(site.quality,
                            c("","","",
                              ecoval.translate("R_macrophytes_error_nositedata", dict),
                              ecoval.translate("R_macrophytes_error_error", dict)))
      
      # ERGAENZT: Complement "site.quality" data.frame with data-set information
      site.quality <- cbind(site.quality, rep(ecoval.translate("R_macrophytes_error_sitedate", dict), nrow(site.quality)))
      colnames(site.quality)[ncol(site.quality)] <- ecoval.translate("R_macrophytes_error_dataset", dict)
      
      # Return results
      return(list(data.site = NA,
                  site.quality = site.quality))
      
    }
            
    # Add row names and unique sample identifier column
    
    rnames <- paste(data.site[,ecoval.translate("A_macrophytes_site_siteid",dict)],
                    data.site[,ecoval.translate("A_macrophytes_site_samplingdate",dict)], sep = "_")
    dup <- duplicated(rnames)
    if ( sum(dup) > 0 )
    {
      site.quality <- rbind(site.quality,
                            c("","","",
                              paste(paste(ecoval.translate("R_macrophytes_error_dupsitedate", dict),":",sep=""),
                                    paste(rnames[dup],collapse=", ")),
                              ecoval.translate("R_macrophytes_error_error", dict)))
      
      # ERGAENZT: Complement "site.quality" data.frame with data-set information
      site.quality <- cbind(site.quality, rep(ecoval.translate("R_macrophytes_error_sitedate", dict), nrow(site.quality)))
      colnames(site.quality)[ncol(site.quality)] <- ecoval.translate("R_macrophytes_error_dataset", dict)
      
      # Return results
      return(list(data.site = NA,
                  site.quality = site.quality))
      
    }
    
    rownames(data.site) <- rnames
    data.site <- data.frame(rownames(data.site), data.site, stringsAsFactors = FALSE)
    colnames(data.site)[1] <- ecoval.translate("A_macrophytes_site_sampleid",dict)
    
    # CHECK IF ALL CORE DATA REQUIRED AS UNIQUE IDENTIFIER OF DATA POINTS IS PRESENT
    
    # SITE CODE
    if ( sum( is.na(data.site[,ecoval.translate("A_macrophytes_site_siteid",dict)]) ) > 0 ) {
      quality.out <- cbind(data.site[is.na(data.site[,ecoval.translate("A_macrophytes_site_siteid",dict)]),
                                          ecoval.translate("A_macrophytes_site_siteid",dict)],
                                data.site[is.na(data.site[,ecoval.translate("A_macrophytes_site_siteid",dict)]),
                                          ecoval.translate("A_macrophytes_site_samplingdate",dict)],
                                rep(ecoval.translate("A_macrophytes_site_siteid",dict),
                                    sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_siteid",dict)]))),
                                rep(ecoval.translate("R_macrophytes_error_incompleterecord",dict),
                                    sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_siteid",dict)]))),
                                rep(ecoval.translate("R_macrophytes_error_error", dict),
                                    sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_siteid",dict)]))))
      colnames(quality.out) <- colnames(site.quality)
      site.quality <- rbind(site.quality, quality.out)
    }
    
    # CANTON
    # if ( sum( is.na(data.site[,ecoval.translate("A_macrophytes_site_canton",dict)]) ) > 0 ) {
    #   quality.out <- cbind(data.site[is.na(data.site[,ecoval.translate("A_macrophytes_site_canton",dict)]),
    #                                       ecoval.translate("A_macrophytes_site_siteid",dict)],
    #                             data.site[is.na(data.site[,ecoval.translate("A_macrophytes_site_canton",dict)]),
    #                                       ecoval.translate("A_macrophytes_site_samplingdate",dict)],
    #                             rep(ecoval.translate("A_macrophytes_site_canton",dict),
    #                                 sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_canton",dict)]))),
    #                             rep(ecoval.translate("R_macrophytes_error_incompleterecord",dict),
    #                                 sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_canton",dict)]))),
    #                             rep(ecoval.translate("R_macrophytes_error_warning",dict),
    #                                 sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_canton",dict)]))))
    #   colnames(quality.out) <- colnames(site.quality)
    #   site.quality <- rbind(site.quality, quality.out)
    # }
    
    # DATE
    if ( sum( is.na(data.site[,ecoval.translate("A_macrophytes_site_samplingdate",dict)]) ) > 0 ) {
      quality.out <- cbind(data.site[is.na(data.site[,ecoval.translate("A_macrophytes_site_samplingdate",dict)]),
                                          ecoval.translate("A_macrophytes_site_siteid",dict)],
                                data.site[is.na(data.site[,ecoval.translate("A_macrophytes_site_samplingdate",dict)]),
                                          ecoval.translate("A_macrophytes_site_samplingdate",dict)],
                                rep(ecoval.translate("A_macrophytes_site_samplingdate",dict),
                                    sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_samplingdate",dict)]))),
                                rep(ecoval.translate("R_macrophytes_error_incompleterecord",dict),
                                    sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_samplingdate",dict)]))),
                                rep(ecoval.translate("R_macrophytes_error_error", dict),
                                    sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_samplingdate",dict)]))))
      colnames(quality.out) <- colnames(site.quality)
      site.quality <- rbind(site.quality, quality.out)
    }
    
    # VALIDATE SITE DATA
    
    # QUALITY CHECK OF OBLIGATORY TYPOLOGY PARAMETERS 
    
    # DISCHARGE
    if( sum( is.na(data.site[,ecoval.translate("A_macrophytes_site_discharge_lpers",dict)]) ) > 0 ) {
      quality.out <- cbind(data.site[is.na(data.site[,ecoval.translate("A_macrophytes_site_discharge_lpers",dict)]),
                                          ecoval.translate("A_macrophytes_site_siteid",dict)],
                                data.site[is.na(data.site[,ecoval.translate("A_macrophytes_site_discharge_lpers",dict)]),
                                          ecoval.translate("A_macrophytes_site_samplingdate",dict)],
                                rep(ecoval.translate("A_macrophytes_site_discharge_lpers",dict),
                                    sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_discharge_lpers",dict)]))),
                                rep(ecoval.translate("R_macrophytes_error_completevalue",dict),
                                    sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_discharge_lpers",dict)]))),
                                rep(ecoval.translate("R_macrophytes_error_error", dict),
                                    sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_discharge_lpers",dict)]))))
      colnames(quality.out) <- colnames(site.quality)
      site.quality <- rbind(site.quality, quality.out)
    }
    
    # if( sum( is.na(data.site[,ecoval.translate("A_macrophytes_site_discharge_source",dict)]) ) > 0 ) {
    #   quality.out <- cbind(data.site[is.na(data.site[,ecoval.translate("A_macrophytes_site_discharge_source",dict)]),
    #                                       ecoval.translate("A_macrophytes_site_siteid",dict)],
    #                             data.site[is.na(data.site[,ecoval.translate("A_macrophytes_site_discharge_source",dict)]),
    #                                       ecoval.translate("A_macrophytes_site_samplingdate",dict)],
    #                             rep(ecoval.translate("A_macrophytes_site_discharge_source",dict),
    #                                 sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_discharge_source",dict)]))),
    #                             rep(ecoval.translate("R_macrophytes_error_completesourceofvalue",dict),
    #                                 sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_discharge_source",dict)]))),
    #                             rep(ecoval.translate("R_macrophytes_error_warning",dict),
    #                                 sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_discharge_source",dict)]))))
    #   colnames(quality.out) <- colnames(site.quality)
    #   site.quality <- rbind(site.quality, quality.out)
    # }
    
    # SLOPE
    if( sum( is.na(data.site[,ecoval.translate("A_macrophytes_site_slope_percent",dict)]) ) > 0 ) {
      quality.out <- cbind(data.site[is.na(data.site[,ecoval.translate("A_macrophytes_site_slope_percent",dict)]),
                                          ecoval.translate("A_macrophytes_site_siteid",dict)],
                                data.site[is.na(data.site[,ecoval.translate("A_macrophytes_site_slope_percent",dict)]),
                                          ecoval.translate("A_macrophytes_site_samplingdate",dict)],
                                rep(ecoval.translate("A_macrophytes_site_slope_percent",dict),
                                    sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_slope_percent",dict)]))),
                                rep(ecoval.translate("R_macrophytes_error_completevalue",dict),
                                    sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_slope_percent",dict)]))),
                                rep(ecoval.translate("R_macrophytes_error_error", dict),
                                    sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_slope_percent",dict)]))))
      colnames(quality.out) <- colnames(site.quality)
      site.quality <- rbind(site.quality, quality.out)
    }
    
    # if( sum( is.na(data.site[,ecoval.translate("A_macrophytes_site_slope_source",dict)]) ) > 0 ) {
    #   quality.out <- cbind(data.site[is.na(data.site[,ecoval.translate("A_macrophytes_site_slope_source",dict)]),
    #                                       ecoval.translate("A_macrophytes_site_siteid",dict)],
    #                             data.site[is.na(data.site[,ecoval.translate("A_macrophytes_site_slope_source",dict)]),
    #                                       ecoval.translate("A_macrophytes_site_samplingdate",dict)],
    #                             rep(ecoval.translate("A_macrophytes_site_slope_source",dict),
    #                                 sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_slope_source",dict)]))),
    #                             rep(ecoval.translate("R_macrophytes_error_completesourceofvalue",dict),
    #                                 sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_slope_source",dict)]))),
    #                             rep(ecoval.translate("R_macrophytes_error_warning",dict),
    #                                 sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_slope_source",dict)]))))
    #   colnames(quality.out) <- colnames(site.quality)
    #   site.quality <- rbind(site.quality, quality.out)
    # }
    
    if( sum( data.site[,ecoval.translate("A_macrophytes_site_slope_percent",dict)] < 0 , na.rm=T) > 0 ) {
      quality.out <- cbind(data.site[data.site[,ecoval.translate("A_macrophytes_site_slope_percent",dict)] < 0,
                                          ecoval.translate("A_macrophytes_site_siteid",dict)],
                                data.site[data.site[,ecoval.translate("A_macrophytes_site_slope_percent",dict)] < 0,
                                          ecoval.translate("A_macrophytes_site_samplingdate",dict)],
                                rep(ecoval.translate("A_macrophytes_site_slope_percent",dict),
                                    sum(data.site[,ecoval.translate("A_macrophytes_site_slope_percent",dict)] < 0)),
                                rep(ecoval.translate("R_macrophytes_error_valuelt0percent", dict),
                                    sum(data.site[,ecoval.translate("A_macrophytes_site_slope_percent",dict)] < 0)),
                                rep(ecoval.translate("R_macrophytes_error_error", dict),
                                    sum(data.site[,ecoval.translate("A_macrophytes_site_slope_percent",dict)] < 0)))
      colnames(quality.out) <- colnames(site.quality)
      site.quality <- rbind(site.quality, quality.out)
    }
    
    # WATER DEPTH
    if( sum( is.na(data.site[,ecoval.translate("A_macrophytes_site_waterdepth_m",dict)]) ) > 0 ) {
      quality.out <- cbind(data.site[is.na(data.site[,ecoval.translate("A_macrophytes_site_waterdepth_m",dict)]),
                                          ecoval.translate("A_macrophytes_site_siteid",dict)],
                                data.site[is.na(data.site[,ecoval.translate("A_macrophytes_site_waterdepth_m",dict)]),
                                          ecoval.translate("A_macrophytes_site_samplingdate",dict)],
                                rep(ecoval.translate("A_macrophytes_site_waterdepth_m",dict),
                                    sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_waterdepth_m",dict)]))),
                                rep(ecoval.translate("R_macrophytes_error_completevalue", dict),
                                    sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_waterdepth_m",dict)]))),
                                rep(ecoval.translate("R_macrophytes_error_error", dict),
                                    sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_waterdepth_m",dict)]))))
      colnames(quality.out) <- colnames(site.quality)
      site.quality <- rbind(site.quality, quality.out)
    }
    
    if( sum( data.site[,ecoval.translate("A_macrophytes_site_waterdepth_m",dict)] < 0 , na.rm=T) > 0 ) {
      quality.out <- cbind(data.site[data.site[,ecoval.translate("A_macrophytes_site_waterdepth_m",dict)] < 0,
                                          ecoval.translate("A_macrophytes_site_siteid",dict)],
                                data.site[data.site[,ecoval.translate("A_macrophytes_site_waterdepth_m",dict)] < 0,
                                          ecoval.translate("A_macrophytes_site_samplingdate",dict)],
                                rep(ecoval.translate("A_macrophytes_site_waterdepth_m",dict),
                                    sum(data.site[,ecoval.translate("A_macrophytes_site_waterdepth_m",dict)] < 0)),
                                rep(ecoval.translate("R_macrophytes_error_valuelt0m", dict),
                                    sum(data.site[,ecoval.translate("A_macrophytes_site_waterdepth_m",dict)] < 0)),
                                rep(ecoval.translate("R_macrophytes_error_error", dict),
                                    sum(data.site[,ecoval.translate("A_macrophytes_site_waterdepth_m",dict)] < 0)))
      colnames(quality.out) <- colnames(site.quality)
      site.quality <- rbind(site.quality, quality.out)
    }
    
    if( sum( data.site[,ecoval.translate("A_macrophytes_site_waterdepth_m",dict)] > 2 , na.rm=T) > 0 ) {
      quality.out <- cbind(data.site[data.site[,ecoval.translate("A_macrophytes_site_waterdepth_m",dict)] > 2,
                                          ecoval.translate("A_macrophytes_site_siteid",dict)],
                                data.site[data.site[,ecoval.translate("A_macrophytes_site_waterdepth_m",dict)] > 2,
                                          ecoval.translate("A_macrophytes_site_samplingdate",dict)],
                                rep(ecoval.translate("A_macrophytes_site_waterdepth_m",dict),
                                    sum(data.site[,ecoval.translate("A_macrophytes_site_waterdepth_m",dict)] > 2)),
                                rep(ecoval.translate("R_macrophytes_error_valuegt2m",dict),
                                    sum(data.site[,ecoval.translate("A_macrophytes_site_waterdepth_m",dict)] > 2)),
                                rep(ecoval.translate("R_macrophytes_error_warning",dict),
                                    sum(data.site[,ecoval.translate("A_macrophytes_site_waterdepth_m",dict)] > 2)))
      colnames(quality.out) <- colnames(site.quality)
      site.quality <- rbind(site.quality, quality.out)
    }
    
    # SHADING
    if( sum( is.na(data.site[,ecoval.translate("A_macrophytes_site_shading_percent",dict)]) ) > 0 ) {
      quality.out <- cbind(data.site[is.na(data.site[,ecoval.translate("A_macrophytes_site_shading_percent",dict)]),
                                          ecoval.translate("A_macrophytes_site_siteid",dict)],
                                data.site[is.na(data.site[,ecoval.translate("A_macrophytes_site_shading_percent",dict)]),
                                          ecoval.translate("A_macrophytes_site_samplingdate",dict)],
                                rep(ecoval.translate("A_macrophytes_site_shading_percent",dict),
                                    sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_shading_percent",dict)]))),
                                rep(ecoval.translate("R_macrophytes_error_completevalue", dict),
                                    sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_shading_percent",dict)]))),
                                rep(ecoval.translate("R_macrophytes_error_error", dict),
                                    sum(is.na(data.site[,ecoval.translate("A_macrophytes_site_shading_percent",dict)]))))
      colnames(quality.out) <- colnames(site.quality)
      site.quality <- rbind(site.quality, quality.out)
    }
    
    if( sum( data.site[,ecoval.translate("A_macrophytes_site_shading_percent",dict)] < 0 , na.rm=T) > 0 ) {
      quality.out <- cbind(data.site[data.site[,ecoval.translate("A_macrophytes_site_shading_percent",dict)] < 0,
                                          ecoval.translate("A_macrophytes_site_siteid",dict)],
                                data.site[data.site[,ecoval.translate("A_macrophytes_site_shading_percent",dict)] < 0,
                                          ecoval.translate("A_macrophytes_site_samplingdate",dict)],
                                rep(ecoval.translate("A_macrophytes_site_shading_percent",dict),
                                    sum(data.site[,ecoval.translate("A_macrophytes_site_shading_percent",dict)] < 0)),
                                rep(ecoval.translate("R_macrophytes_error_valuelt0percent", dict),
                                    sum(data.site[,ecoval.translate("A_macrophytes_site_shading_percent",dict)] < 0)),
                                rep(ecoval.translate("R_macrophytes_error_error", dict),
                                    sum(data.site[,ecoval.translate("A_macrophytes_site_shading_percent",dict)] < 0)))
      colnames(quality.out) <- colnames(site.quality)
      site.quality <- rbind(site.quality, quality.out)
    }
    
    if( sum( data.site[,ecoval.translate("A_macrophytes_site_shading_percent",dict)] > 100 , na.rm=T) > 0 ) {
      quality.out <- cbind(data.site[data.site[,ecoval.translate("A_macrophytes_site_shading_percent",dict)] > 100,
                                          ecoval.translate("A_macrophytes_site_siteid",dict)],
                                data.site[data.site[,ecoval.translate("A_macrophytes_site_shading_percent",dict)] > 100,
                                          ecoval.translate("A_macrophytes_site_samplingdate",dict)],
                                rep(ecoval.translate("A_macrophytes_site_shading_percent",dict),
                                    sum(data.site[,ecoval.translate("A_macrophytes_site_shading_percent",dict)] > 100)),
                                rep(ecoval.translate("R_macrophytes_error_valuegt100percent", dict),
                                    sum(data.site[,ecoval.translate("A_macrophytes_site_shading_percent",dict)] > 100)),
                                rep(ecoval.translate("R_macrophytes_error_error", dict),
                                    sum(data.site[,ecoval.translate("A_macrophytes_site_shading_percent",dict)] > 100)))
      colnames(quality.out) <- colnames(site.quality)
      site.quality <- rbind(site.quality, quality.out)
    }
    
    # SUBSTRATE
    header.substrat  <- c(ecoval.translate("A_macrophytes_site_substrate_above40_percent", dict),
                          ecoval.translate("A_macrophytes_site_substrate_20to40_percent", dict),
                          ecoval.translate("A_macrophytes_site_substrate_6to20_percent", dict),
                          ecoval.translate("A_macrophytes_site_substrate_2to6_percent", dict),
                          ecoval.translate("A_macrophytes_site_substrate_0to2_percent", dict),
                          ecoval.translate("A_macrophytes_site_substrate_sand_percent", dict),
                          ecoval.translate("A_macrophytes_site_substrate_siltclay_percent", dict),
                          ecoval.translate("A_macrophytes_site_substrate_mud_percent", dict),
                          ecoval.translate("A_macrophytes_site_substrate_humuspeat_percent", dict),
                          ecoval.translate("A_macrophytes_site_substrate_artificial_percent", dict))
    
    # Check substrate size-class data
    sum.substrat <- rowSums(data.site[, colnames(data.site) %in% header.substrat], na.rm = T)
    
    if( sum(sum.substrat == 0, na.rm=T) > 0) {
      quality.out <- cbind(data.site[sum.substrat == 0,
                                          ecoval.translate("A_macrophytes_site_siteid",dict)],
                                data.site[sum.substrat == 0,
                                          ecoval.translate("A_macrophytes_site_samplingdate",dict)],
                                rep(ecoval.translate("R_macrophytes_error_substrate", dict), sum(sum.substrat == 0)),
                                rep(ecoval.translate("R_macrophytes_error_missingdata", dict), sum(sum.substrat == 0)),
                                rep(ecoval.translate("R_macrophytes_error_error", dict),sum(sum.substrat == 0)))
      colnames(quality.out) <- colnames(site.quality)
      site.quality <- rbind(site.quality, quality.out)
    }
    
    if( any(abs(sum.substrat - 100) > 0.9) ) {
      quality.out <- cbind(data.site[abs(sum.substrat - 100) > 0.9,
                                          ecoval.translate("A_macrophytes_site_siteid",dict)],
                                data.site[abs(sum.substrat - 100) > 0.9,
                                          ecoval.translate("A_macrophytes_site_samplingdate",dict)],
                                rep(ecoval.translate("R_macrophytes_error_substrate", dict), sum(abs(sum.substrat - 100) > 0.9)),
                                rep(ecoval.translate("R_macrophytes_error_sumnot100%", dict), sum(abs(sum.substrat - 100) > 0.9)),
                                rep(ecoval.translate("R_macrophytes_error_error", dict), sum(abs(sum.substrat - 100) > 0.9)))
      colnames(quality.out) <- colnames(site.quality)
      site.quality <- rbind(site.quality, quality.out)
    }
    
    # Add proportion of stones (sum substrate size-classes > 6.3 cm) and fine material (< 6.3cm) in data.site
    data.site.sub <- data.site[,1:match(ecoval.translate("A_macrophytes_site_substrate_above40_percent", dict),
                                        colnames(data.site)) - 1]
    data.site.sub <- data.frame(data.site.sub, rep(NA, nrow(data.site.sub)), rep(NA, nrow(data.site.sub)), stringsAsFactors = FALSE)
    colnames(data.site.sub)[ncol(data.site.sub)-1] <- ecoval.translate("A_macrophytes_site_coarsegravel_percent",dict)
    colnames(data.site.sub)[ncol(data.site.sub)] <- ecoval.translate("A_macrophytes_site_finesubstrate_percent",dict)
    data.site <- data.frame(data.site.sub, data.site[, match(ecoval.translate("A_macrophytes_site_substrate_above40_percent", dict),
                                                             colnames(data.site)):ncol(data.site)])
    
    data.site[,ecoval.translate("A_macrophytes_site_coarsegravel_percent",dict)]  <- rowSums(data.site[, header.substrat[1:3]], na.rm = T)
    data.site[,ecoval.translate("A_macrophytes_site_finesubstrate_percent",dict)] <- rowSums(data.site[, header.substrat[4:9]], na.rm = T)
    
    data.site[,ecoval.translate("A_macrophytes_site_sumsubstrate_comment",dict)] <- NA
    
    for ( i in 1:nrow(data.site) ) {
      sum.substrat <- data.site[i,ecoval.translate("A_macrophytes_site_coarsegravel_percent",dict)] + data.site[i,ecoval.translate("A_macrophytes_site_finesubstrate_percent",dict)]
      
      if ( abs(sum.substrat) == 0 ) {
        {
          if(data.site[i,ecoval.translate("A_macrophytes_site_discharge_lpers", dict)] < 200) {
            data.site[i,ecoval.translate("A_macrophytes_site_coarsegravel_percent",dict)] <- 39
            data.site[i,ecoval.translate("A_macrophytes_site_finesubstrate_percent",dict)]   <- 61
            data.site[i,ecoval.translate("A_macrophytes_site_sumsubstrate_comment",dict)] <- "Nur kuenstliches Substrat, Anteil Steine 39% ergaenzt"
          } else {
            if(data.site[i,ecoval.translate("A_macrophytes_site_discharge_lpers", dict)]  < 2000) {
              data.site[i,ecoval.translate("A_macrophytes_site_coarsegravel_percent",dict)] <- 49
              data.site[i,ecoval.translate("A_macrophytes_site_finesubstrate_percent",dict)]   <- 51
              data.site[i,ecoval.translate("A_macrophytes_site_sumsubstrate_comment",dict)] <- "Nur kuenstliches Substrat, Anteile Steine 49% ergaenzt"
            } else {
              data.site[i,ecoval.translate("A_macrophytes_site_coarsegravel_percent",dict)] <- 59
              data.site[i,ecoval.translate("A_macrophytes_site_coarsegravel_percent",dict)]   <- 41
              data.site[i,ecoval.translate("A_macrophytes_site_sumsubstrate_comment",dict)] <- "Nur kuenstliches Substrat, Anteile Steine 59% ergaenzt"
            }
          }
        }
      } else {
        data.site[i,ecoval.translate("A_macrophytes_site_coarsegravel_percent",dict)]  <- data.site[i,ecoval.translate("A_macrophytes_site_coarsegravel_percent",dict)] * (100/sum.substrat)
        data.site[i,ecoval.translate("A_macrophytes_site_finesubstrate_percent",dict)] <- data.site[i,ecoval.translate("A_macrophytes_site_finesubstrate_percent",dict)] * (100/sum.substrat)
      }
    }
    # ERGAENZT: Complement "site.quality" data.frame with data-set information
    site.quality <- cbind(site.quality, rep(ecoval.translate("R_macrophytes_error_sitedate", dict), nrow(site.quality)))
    colnames(site.quality)[ncol(site.quality)] <- ecoval.translate("R_macrophytes_error_dataset", dict)
    
    # Return results
    return(list(data.site = data.site, site.quality = site.quality))
  }
}



# ==========================================
# FUNCTION TO READ AND VALIDATE SPECIES DATA
# ==========================================

msk.macrophytes.2017.compile.speciesdat <- function (data.species,
                                                     sampling.protocol = "v2018",
                                                     language          = "English",
                                                     dictionaries      = NA)
{
  # Get dictionary:
  
  dict <- ecoval.dict(language,dictionaries)
  
  # Get taxa list and translate entries to the active language:
  
  taxalist.dat <- ecoval::msk.macrophytes.2017_ListTaxa
  
  for ( j in 1:ncol(taxalist.dat) ) colnames(taxalist.dat)[j] <- ecoval.translate(colnames(taxalist.dat)[j],dict)
  col.gf <- match(ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict),colnames(taxalist.dat))
  if ( !is.na(col.gf) ) for ( i in 1:nrow(taxalist.dat) ) taxalist.dat[i,col.gf] <- ecoval.translate(taxalist.dat[i,col.gf],dict)
  col.gfabbrev <- match(ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict),colnames(taxalist.dat))
  if ( !is.na(col.gfabbrev) ) for ( i in 1:nrow(taxalist.dat) ) taxalist.dat[i,col.gfabbrev] <- ecoval.translate(taxalist.dat[i,col.gfabbrev],dict)
  col.es <- match(ecoval.translate("A_macrophytes_taxalist_growthform_assess_twoforms_grfo",dict),colnames(taxalist.dat))
  if ( !is.na(col.es) ) for ( i in 1:nrow(taxalist.dat) ) taxalist.dat[i,col.es] <- ifelse(is.na(taxalist.dat[i,col.es]),NA,ecoval.translate(taxalist.dat[i,col.es],dict))
  col.trait <- match(ecoval.translate("A_macrophytes_taxalist_determination_trait",dict),colnames(taxalist.dat))
  if ( !is.na(col.trait) ) for ( i in 1:nrow(taxalist.dat) ) taxalist.dat[i,col.trait] <- ifelse(is.na(taxalist.dat[i,col.trait]),NA,ecoval.translate(taxalist.dat[i,col.trait],dict))
  col.cons <- match(ecoval.translate("A_macrophytes_taxalist_conservationinfo",dict),colnames(taxalist.dat))
  if ( !is.na(col.cons) ) for ( i in 1:nrow(taxalist.dat) ) taxalist.dat[i,col.cons] <- ifelse(is.na(taxalist.dat[i,col.cons]),NA,ecoval.translate(taxalist.dat[i,col.cons],dict))
  
  # Create data.frame to collect information on quality checks:
  
  species.quality <- matrix(NA, nrow = 0, ncol = 5)
  colnames(species.quality) <- c(ecoval.translate("A_macrophytes_site_siteid",dict),
                                 ecoval.translate("A_macrophytes_site_samplingdate",dict),
                                 ecoval.translate("R_macrophytes_error_parameter",dict),
                                 ecoval.translate("R_macrophytes_error_message",dict),
                                 ecoval.translate("R_macrophytes_error_type",dict))
  
  # Check if all required columns are contained in data.species:
  
  req.colnames <- c(ecoval.translate("A_macrophytes_site_siteid",dict),
                    ecoval.translate("A_macrophytes_site_samplingdate",dict),
                    ecoval.translate("A_macrophytes_species_number_msk",dict),
                    ecoval.translate("A_macrophytes_species_name_latin",dict),
                    ecoval.translate("A_macrophytes_species_absolutecover_percent",dict),
                    ecoval.translate("A_macrophytes_species_artificialsubstrate_relcover_percent",dict),
                    ecoval.translate("A_macrophytes_species_determinationuncertainty",dict),
                    ecoval.translate("A_macrophytes_species_phenology_blossom",dict),
                    ecoval.translate("A_macrophytes_species_phenology_fruits",dict))
  ind.req <- match(req.colnames,colnames(data.species))
  if ( anyNA(ind.req) )
  {
    species.quality <- rbind(species.quality,
                             c("","","",
                               paste(paste(ecoval.translate("R_macrophytes_error_speciescolmissing", dict),":",sep=""),
                                     paste(req.colnames[is.na(ind.req)],collapse=", ")),
                               ecoval.translate("R_macrophytes_error_error", dict)))
    # Complement "species.quality" data.frame with data-set information
    species.quality <- cbind(species.quality, rep(ecoval.translate("R_macrophytes_error_speciesdata", dict), nrow(species.quality)))
    colnames(species.quality)[ncol(species.quality)] <- ecoval.translate("R_macrophytes_error_dataset", dict)
    
    # Return list with generared outputs
    return(list(species.assess       = NA, 
                species.removed      = NA, 
                species.quality      = species.quality, 
                taxalist             = taxalist.dat))
  }
  
  # Eliminate rows without site and data entry:
  
  data.species <- data.species[!is.na(data.species[,ecoval.translate("A_macrophytes_site_siteid",dict)]) & !is.na(data.species[,ecoval.translate("A_macrophytes_site_samplingdate",dict)]),]
  if ( nrow(data.species) == 0 )
  {
    species.quality <- rbind(species.quality,
                             c("","","",
                               ecoval.translate("R_macrophytes_error_nospeciesdata", dict),
                               ecoval.translate("R_macrophytes_error_error", dict)))
    
    # Complement "species.quality" data.frame with data-set information
    species.quality <- cbind(species.quality, rep(ecoval.translate("R_macrophytes_error_speciesdata", dict), nrow(species.quality)))
    colnames(species.quality)[ncol(species.quality)] <- ecoval.translate("R_macrophytes_error_dataset", dict)
    
    # Return results
    return(list(species.assess       = NA, 
                species.removed      = NA, 
                species.quality      = species.quality, 
                taxalist             = taxalist.dat))
    
  }
  
  # Add unique identifier for individual assessments in extra column; and species within assessments as row names:
  
  data.species <- data.frame(paste(data.species[,ecoval.translate("A_macrophytes_site_siteid",dict)],
                                   data.species[,ecoval.translate("A_macrophytes_site_samplingdate",dict)], sep = "_"),
                             data.species, stringsAsFactors = FALSE)
  colnames(data.species)[1] <- ecoval.translate("A_macrophytes_site_sampleid",dict)

  # Function to generate output for quality checks
  split.last.underline <- function(x)
  {
    y <- strsplit(x,split="_")[[1]]
    n <- length(y)
    x2 <- y[n]
    x1 <- NA
    if ( n == 2 ) x1 <- y[1]
    if ( n > 2 )  x1 <- paste(y[1:(n-1)],collapse="_")
    return(c(x1,x2))
  }
  
  # Perform quality checks
  
  # CHECK IF ALL CORE DATA REQUIRED AS UNIQUE IDENTIFIER ARE PRESENT
  
  # # SITE CODE  # *** note: lines without site code are now eliminated above
  #
  # ind.na <- is.na(data.species[,ecoval.translate("A_macrophytes_site_siteid",dict)])  # site code NA
  # if ( sum(ind.na) > 0 ) {
  #   quality.out <- unique(data.species[ind.na,ecoval.translate("A_macrophytes_site_sampleid",dict)])
  #   quality.out <- cbind(t(sapply(quality.out , split.last.underline)))
  #   quality.out <- cbind(quality.out, rep(ecoval.translate("A_macrophytes_site_siteid",dict), nrow(quality.out)))
  #   quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_completevalue", dict), nrow(quality.out)))
  #   quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_error", dict), nrow(quality.out)))
  #   colnames(quality.out) <- colnames(species.quality)
  #   species.quality <- rbind(species.quality, quality.out)
  # }
  # 
  # # DATE  # *** note: lines without data are now eliminated above
  #
  # ind.na <- is.na(data.species[,ecoval.translate("A_macrophytes_site_samplingdate",dict)])  # sampling data NA
  # if ( sum(ind.na) > 0 ) {
  #   quality.out <- unique(data.species[ind.na,ecoval.translate("A_macrophytes_site_sampleid",dict)])
  #   quality.out <- cbind(t(sapply(quality.out , split.last.underline)))
  #   quality.out <- cbind(quality.out, rep(ecoval.translate("A_macrophytes_site_samplingdate",dict), nrow(quality.out)))
  #   quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_completevalue", dict), nrow(quality.out)))
  #   quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_error", dict), nrow(quality.out)))
  #   colnames(quality.out) <- colnames(species.quality)
  #   species.quality <- rbind(species.quality, quality.out)
  # }
  
  # SPECIES NUMBER
  
  ind.na <- is.na(data.species[,ecoval.translate("A_macrophytes_species_number_msk",dict)])  # species taxa list number NA
  if( sum(ind.na) > 0 ) {
    quality.out <- unique(data.species[ind.na,ecoval.translate("A_macrophytes_site_sampleid",dict)])
    quality.out <- cbind(t(sapply(quality.out , split.last.underline)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("A_macrophytes_species_number_msk",dict), nrow(quality.out)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_completevalue", dict), nrow(quality.out)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_error", dict), nrow(quality.out)))
    colnames(quality.out) <- colnames(species.quality)
    species.quality <- rbind(species.quality, quality.out)
  }
  
  # VALIDATE SPECIES DATA
  
  # QUALITY CHECK OF OBLIGATORY PARAMETERS 
  
  # ABSOLUTE COVER OF INDIVIDUAL SPECIES

  ind.taxalist <- data.species[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
                    taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)]  
  ind.na <- is.na(data.species[,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)])
  ind.bry <- data.species[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
    taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]==
                                                                               ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_bry",dict)]
  ind.brysum <- data.species[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
    taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]==
                                                                               ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_brysum",dict)]
  ind.cha <- data.species[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
    taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]==
                                                                               ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_cha",dict)]
  ind.chasum <- data.species[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
    taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]==
                                                                               ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_chasum",dict)]
  
  # Check if cover values are given for each species except bryophytes and characea
  
  ind.illegal.na <- ind.na & !ind.bry & !ind.cha & ind.taxalist
  if( sum( ind.illegal.na ) > 0 ) {
    #quality.out <- unique(data.species[ind.illegal.na,ecoval.translate("A_macrophytes_site_sampleid",dict)])
    site_sample_species <- aggregate(data.species[ind.illegal.na,ecoval.translate("A_macrophytes_species_number_msk",dict)],
                                     by=list(data.species[ind.illegal.na,ecoval.translate("A_macrophytes_site_sampleid",dict)]),FUN=paste,collapse=",")
    quality.out <- site_sample_species[,1]
    quality.out <- cbind(t(sapply(quality.out , split.last.underline)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("A_macrophytes_species_absolutecover_percent",dict), nrow(quality.out)))
    quality.out <- cbind(quality.out, paste(rep(ecoval.translate("R_macrophytes_error_completevalue", dict), nrow(quality.out)),":",site_sample_species[,2]))
    quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_error", dict), nrow(quality.out)))
    colnames(quality.out) <- colnames(species.quality)
    species.quality <- rbind(species.quality, quality.out)
  }
  
  # Check if 0% cover values are present, which is either not correct or species should be not contained in data
  
  ind.zero <- data.species[,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] == 0; ind.zero[is.na(ind.zero)] <- FALSE
  if( sum(ind.zero) > 0 ) {
    quality.out <- unique(data.species[ind.zero,ecoval.translate("A_macrophytes_site_sampleid",dict)])
    quality.out <- cbind(t(sapply(quality.out, split.last.underline)), row.names = NULL)
    quality.out <- cbind(quality.out, rep(ecoval.translate("A_macrophytes_species_absolutecover_percent",dict), nrow(quality.out)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_value0percent", dict), nrow(quality.out)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_error", dict), nrow(quality.out)))
    colnames(quality.out) <- colnames(species.quality)
    species.quality <- rbind(species.quality, quality.out)
  }
  
  # Check if <0% cover values are present
  
  ind.ltzero <- data.species[,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] < 0; ind.ltzero[is.na(ind.ltzero)] <- FALSE
  if( sum(ind.ltzero) > 0 ) {
    quality.out <- unique(data.species[ind.ltzero,ecoval.translate("A_macrophytes_site_sampleid",dict)])
    quality.out <- cbind(t(sapply(quality.out, split.last.underline)), row.names = NULL)
    quality.out <- cbind(quality.out, rep(ecoval.translate("A_macrophytes_species_absolutecover_percent",dict), nrow(quality.out)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_valuelt0percent", dict), nrow(quality.out)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_error", dict), nrow(quality.out)))
    colnames(quality.out) <- colnames(species.quality)
    species.quality <- rbind(species.quality, quality.out)
  }
  
  # Check if >100% cover values are present
  
  ind.gt100 <- data.species[,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] >= 100; ind.gt100[is.na(ind.gt100)] <- FALSE
  if( sum(ind.gt100) > 0 ) {
    quality.out <- unique(data.species[ind.gt100,ecoval.translate("A_macrophytes_site_sampleid",dict)])
    quality.out <- cbind(t(sapply(quality.out, split.last.underline)), row.names = NULL)
    quality.out <- cbind(quality.out, rep(ecoval.translate("A_macrophytes_species_absolutecover_percent",dict), nrow(quality.out)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_valuegt100percent", dict), nrow(quality.out)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_error", dict), nrow(quality.out)))
    colnames(quality.out) <- colnames(species.quality)
    species.quality <- rbind(species.quality, quality.out)
  }
  
  # Check consistency of sum bry and sum cha with brysum and chasum.
  
  Erhebung_Code <- unique(data.species[,ecoval.translate("A_macrophytes_site_sampleid",dict)])
  dg.abs.check <- rep("",length(Erhebung_Code))
  for ( i in 1:length(dg.abs.check) ) 
  {
    ind.sampling <- data.species[,ecoval.translate("A_macrophytes_site_sampleid",dict)] == Erhebung_Code[i]
    if ( sum(ind.brysum&ind.sampling) > 0 & sum(ind.bry&ind.sampling) > 0 ) 
    {
      dg.brysum <- sum(data.species[ind.brysum&ind.sampling,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)],na.rm=TRUE)
      dg.bry    <- sum(data.species[ind.bry   &ind.sampling,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)],na.rm=TRUE)
      dg.bryall <- sum(data.species[ind.bry   &ind.sampling,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)],na.rm=FALSE)
      if ( !is.na(dg.brysum) )
      {
        if ( !is.na(dg.bryall) )  # all species coverages available
        {
          if( abs(dg.brysum-dg.bryall) > 0.001 ) dg.abs.check[i] <- paste(dg.abs.check[i],"bry",sep="")
        }
        else
        {
          if ( !is.na(dg.bry) ) # some species coverages available
          {
            if( dg.bry > dg.brysum ) dg.abs.check[i] <- paste(dg.abs.check[i],"bry",sep="")
          }
        }
      }
    }
    if ( sum(ind.chasum&ind.sampling) > 0 & sum(ind.cha&ind.sampling) > 0 ) 
    {
      dg.chasum <- sum(data.species[ind.chasum&ind.sampling,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)],na.rm=TRUE)
      dg.cha    <- sum(data.species[ind.cha   &ind.sampling,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)],na.rm=TRUE)
      dg.chaall <- sum(data.species[ind.cha   &ind.sampling,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)],na.rm=FALSE)
      if ( !is.na(dg.chasum) )
      {
        if ( !is.na(dg.chaall) )  # all species coverages available
        {
          if( abs(dg.chasum-dg.chaall) > 0.001 ) dg.abs.check[i] <- paste(dg.abs.check[i],"cha",sep="")
        }
        else
        {
          if ( !is.na(dg.cha) ) # some species coverages available
          {
            if( dg.cha > dg.chasum ) dg.abs.check[i] <- paste(dg.abs.check[i],"cha",sep="")
          }
        }
      }
    }
  }
  if( sum(dg.abs.check == "bry", na.rm =T) > 0 ) {
    quality.out <- Erhebung_Code[substring(dg.abs.check,1,3) == "bry"]
    quality.out <- cbind(t(sapply(quality.out , split.last.underline)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("A_macrophytes_species_absolutecover_percent",dict), nrow(quality.out)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_sumbryincompatible", dict), nrow(quality.out)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_error", dict), nrow(quality.out)))
    colnames(quality.out) <- colnames(species.quality)
    species.quality <- rbind(species.quality, quality.out)
  }
  if( sum(dg.abs.check == "cha",na.rm=T) > 0 | sum(dg.abs.check == "brycha", na.rm =T) > 0 ) {
    quality.out <- Erhebung_Code[dg.abs.check == "cha" | dg.abs.check == "brycha"]
    quality.out <- cbind(t(sapply(quality.out , split.last.underline)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("A_macrophytes_species_absolutecover_percent",dict), nrow(quality.out)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_sumchaincompatible", dict), nrow(quality.out)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_error", dict), nrow(quality.out)))
    colnames(quality.out) <- colnames(species.quality)
    species.quality <- rbind(species.quality, quality.out)
  }
  
  # Check sum of cover values is > 100%.
  
  Erhebung_Code <- unique(data.species[,ecoval.translate("A_macrophytes_site_sampleid",dict)])
  dg.abs.check <- rep("ok",length(Erhebung_Code))
  for ( i in 1:length(dg.abs.check) ) {
    ind.sampling <- data.species[,ecoval.translate("A_macrophytes_site_sampleid",dict)] == Erhebung_Code[i]
    ind.test <- ind.sampling
    if ( sum(ind.brysum&ind.sampling) > 0 ) ind.test <- ind.test & !ind.bry  # if summary species present, exclude individual taxa
    if ( sum(ind.chasum&ind.sampling) > 0 ) ind.test <- ind.test & !ind.cha  # if summary species present, exclude individual taxa
    if ( sum(data.species[ind.test,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)],na.rm=TRUE) > 100 )
    {
      dg.abs.check[i] <- "error"
    }
  }
  if( sum(dg.abs.check == "error", na.rm =T) > 0 ) {
    quality.out <- Erhebung_Code[dg.abs.check == "error"]
    quality.out <- cbind(t(sapply(quality.out , split.last.underline)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("A_macrophytes_species_absolutecover_percent",dict), nrow(quality.out)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_sumabscoveragegt100%", dict), nrow(quality.out)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_error", dict), nrow(quality.out)))
    colnames(quality.out) <- colnames(species.quality)
    species.quality <- rbind(species.quality, quality.out)
  }
  
  # Check if uncertainty of determination is given for each species, only if sampling protocol 2018 was used
  
  if ( sampling.protocol == "v2018" ) {
    ind.na <- is.na(data.species[,ecoval.translate("A_macrophytes_species_determinationuncertainty",dict)])
    if( sum( ind.na ) > 0 ) {
      site_sample_species <- aggregate(data.species[ind.na,ecoval.translate("A_macrophytes_species_number_msk",dict)],
                                       by=list(data.species[ind.na,ecoval.translate("A_macrophytes_site_sampleid",dict)]),FUN=paste,collapse=",")
      quality.out <- site_sample_species[,1]
      quality.out <- cbind(t(sapply(quality.out , split.last.underline)))
      quality.out <- cbind(quality.out, rep(ecoval.translate("A_macrophytes_species_determinationuncertainty",dict), nrow(quality.out)))
      quality.out <- cbind(quality.out, paste(rep(ecoval.translate("R_macrophytes_error_completevalue", dict), nrow(quality.out)),":",site_sample_species[,2]))
      quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_error", dict), nrow(quality.out)))
      colnames(quality.out) <- colnames(species.quality)
      species.quality <- rbind(species.quality, quality.out)
    }
  }
  
  ## Reduce data-set to species on taxalist
  
  species.not.taxalist <- data.species[data.species[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
                                         taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)] == FALSE,]
  
  data.species <- data.species[data.species[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
                                 taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)],]
  
  # Add column for comments regarding species validation (determination quality, see below)
  
  data.species[, ecoval.translate("A_macrophytes_species_commentvalidation_taxa",dict)] <- NA
  
  # Add growthform information to data
  
  names <- colnames(data.species)
  data.species <- cbind(data.species,NA,NA)
  colnames(data.species) <- c(names,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict),ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict))

  for( i in 1:nrow(data.species) ) {
    row.taxalist <- match(data.species[i,ecoval.translate("A_macrophytes_species_number_msk",dict)],
                          taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)])
    if( !is.na(row.taxalist) ) {
      data.species[i, ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)] <- taxalist.dat[row.taxalist, ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]
      data.species[i, ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] <- taxalist.dat[row.taxalist, ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)]
    }
  }
  
  # Check for double entries (only for taxa in taxa list).
  
  Erhebung_Code <- unique(data.species[,ecoval.translate("A_macrophytes_site_sampleid",dict)])
  spec.num.check <- rep("ok",length(Erhebung_Code))
  for ( i in 1:length(Erhebung_Code) ) {
    ind.sampling <- data.species[,ecoval.translate("A_macrophytes_site_sampleid",dict)] == Erhebung_Code[i]
    spec.number <- data.species[ind.sampling,ecoval.translate("A_macrophytes_species_number_msk",dict)]
    if ( length(spec.number) != length(unique(spec.number))) spec.num.check[i] <- "error"
  }
  if( sum(spec.num.check == "error", na.rm =T) > 0 ) {
    quality.out <- Erhebung_Code[spec.num.check == "error"]
    quality.out <- cbind(t(sapply(quality.out , split.last.underline)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("A_macrophytes_species_number_msk",dict), nrow(quality.out)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_doubleentries", dict), nrow(quality.out)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_error", dict), nrow(quality.out)))
    colnames(quality.out) <- colnames(species.quality)
    species.quality <- rbind(species.quality, quality.out)
  }

  # Check species assignments and determination accuracy, and assign them to aggregates if this is adequate
  # This is only done for data collected with sampling protocol published 2017/2018
  # For old data species are taken as they are and determination uncertanity is not considered
  
  rows.delete <- rep(FALSE,nrow(data.species))
  if ( sampling.protocol == "v2018" ) {
    
    for ( i in 1:nrow(data.species) ) {
      row.taxalist <- match(data.species[i,ecoval.translate("A_macrophytes_species_number_msk",dict)],
                            taxalist.dat[ ,ecoval.translate("A_macrophytes_species_number_msk",dict)])
      
      detunc.data.species  <- data.species[i,ecoval.translate("A_macrophytes_species_determinationuncertainty",dict)]
      
      if( !is.na(detunc.data.species) ) {
        # Get corresponding determination value and taxa-group assigment from taxalist
        detval.taxalist.dat <- taxalist.dat[row.taxalist, ecoval.translate("A_macrophytes_taxalist_determination",dict)]
        grouptaxon.taxalist.dat <- taxalist.dat[row.taxalist, ecoval.translate("A_macrophytes_taxalist_taxagroups_assign",dict)]
        
        # Do quality check for bryophyte species
        if( data.species[i,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)] == ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_bry",dict) ) {
          if ( ( detval.taxalist.dat <= 2 & detunc.data.species == 4 ) |
               ( detval.taxalist.dat == 3 & detunc.data.species != 2 ) ) {
            if( !is.na(grouptaxon.taxalist.dat) ) {
              data.species[i,ecoval.translate("A_macrophytes_species_number_msk",dict)] <- grouptaxon.taxalist.dat
              data.species[i,ecoval.translate("A_macrophytes_species_name_latin",dict)] <- taxalist.dat[match(grouptaxon.taxalist.dat,taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk", dict)]),
                                                                                                        ecoval.translate("A_macrophytes_species_name_latin",dict)]
              data.species[i,ecoval.translate("A_macrophytes_species_determinationuncertainty",dict)] <- NA
              
              data.species[i,ecoval.translate("A_macrophytes_species_commentvalidation_taxa",dict)] <- ecoval.translate("R_macrophytes_doc_speciesreplaced",dict)
            } else {
              rows.delete[i] <- TRUE
            }
          }
        } else {   # end if bryophyte
          
          # Quality check for all higher macrophytes species
          if ( detval.taxalist.dat <= 2 ) {   # detval <= 2
            if( detunc.data.species == 4 ) {   # detunc = 4
              if( !is.na(grouptaxon.taxalist.dat) ) {   # aggregate tayon exists
                data.species[i,ecoval.translate("A_macrophytes_species_number_msk",dict)] <- grouptaxon.taxalist.dat
                data.species[i,ecoval.translate("A_macrophytes_species_name_latin",dict)] <- taxalist.dat[match(grouptaxon.taxalist.dat,taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk", dict)]),
                                                                                                          ecoval.translate("A_macrophytes_species_name_latin",dict)]
                data.species[i,ecoval.translate("A_macrophytes_species_determinationuncertainty",dict)] <- NA
                
                data.species[i,ecoval.translate("A_macrophytes_species_commentvalidation_taxa",dict)] <- ecoval.translate("R_macrophytes_doc_speciesreplaced",dict)
                
                data.species[i,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]  <- taxalist.dat[match(grouptaxon.taxalist.dat,taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk", dict)]),
                                                             ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]
                data.species[i,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] <- taxalist.dat[match(grouptaxon.taxalist.dat,taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk", dict)]),
                                                             ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)]
                
              } else {   # no aggregate taxon
                rows.delete[i] <- TRUE
              }
            }   # end detunc = 4
          } else {   # detval > 2
            if ( detval.taxalist.dat == 3 ) {
              # For species with a determination value of 3 in the taxalist check if phenology trait necessary for correct
              # determination of the taxon was present
              phen.check.code <- taxalist.dat[match(data.species[i,ecoval.translate("A_macrophytes_species_number_msk",dict)],
                                                    taxalist.dat[ ,ecoval.translate("A_macrophytes_species_number_msk",dict)]),
                                              ecoval.translate("A_macrophytes_taxalist_phenology_trait",dict)]
              phen.check.ok <- FALSE
              
              # Code 1 for species in taxalist: Blossoms need to be present
              if( phen.check.code == 1 ) {
                phen.check.ok <- !is.na(data.species[i,ecoval.translate("A_macrophytes_species_phenology_blossom",dict)])
              } else {
                # Code 2 for species in taxalist: Fruits need to be present
                if( phen.check.code == 2 ) {
                  phen.check.ok <- !is.na(data.species[i,ecoval.translate("A_macrophytes_species_phenology_fruits",dict)]) 
                } else {
                  # Code 3 for species in taxalist: Blossoms OR fruits need to be present
                  if( phen.check.code == 3 ) {
                    phen.check.ok <- !is.na(data.species[i,ecoval.translate("A_macrophytes_species_phenology_fruits",dict)]) | !is.na(data.species[i,ecoval.translate("A_macrophytes_species_phenology_blossom",dict)])
                  } else {
                    # Code 3 for species in taxalist: Blossoms AND fruits need to be present
                    if( phen.check.code == 4 ) {
                      phen.check.ok <- !is.na(data.species[i,ecoval.translate("A_macrophytes_species_phenology_fruits",dict)]) & !is.na(data.species[i,ecoval.translate("A_macrophytes_species_phenology_blossom",dict)])
                      
                    }
                  }
                }
              }

              # Species is corrected to group-taxon if required phenology trait was not present or if determination was insecure
              if ( !phen.check.ok | detunc.data.species > 3 ) {
                if ( !is.na(grouptaxon.taxalist.dat) ) {
                  data.species[i,ecoval.translate("A_macrophytes_species_number_msk",dict)] <- grouptaxon.taxalist.dat
                  data.species[i,ecoval.translate("A_macrophytes_species_name_latin",dict)] <- taxalist.dat[match(grouptaxon.taxalist.dat,taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk", dict)]),
                                                                                                            ecoval.translate("A_macrophytes_species_name_latin",dict)]
                  data.species[i,ecoval.translate("A_macrophytes_species_determinationuncertainty",dict)] <- NA
                  
                  data.species[i,ecoval.translate("A_macrophytes_species_commentvalidation_taxa",dict)] <- ecoval.translate("R_macrophytes_doc_speciesreplaced",dict)
                  
                  data.species[i,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]  <- taxalist.dat[match(grouptaxon.taxalist.dat,taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk", dict)]),
                                                               ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]
                  data.species[i,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] <- taxalist.dat[match(grouptaxon.taxalist.dat,taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk", dict)]),
                                                               ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)]
                } else {
                  rows.delete[i] <- TRUE
                }
              }
            } else {
              if ( detval.taxalist.dat == 4 ) {
                if ( !is.na(grouptaxon.taxalist.dat) ) {
                  data.species[i,ecoval.translate("A_macrophytes_species_number_msk",dict)] <- grouptaxon.taxalist.dat
                  data.species[i,ecoval.translate("A_macrophytes_species_name_latin",dict)] <- taxalist.dat[match(grouptaxon.taxalist.dat,taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk", dict)]),
                                                                                                            ecoval.translate("A_macrophytes_species_name_latin",dict)]
                  data.species[i,ecoval.translate("A_macrophytes_species_determinationuncertainty",dict)] <- NA
                  
                  data.species[i,ecoval.translate("A_macrophytes_species_commentvalidation_taxa",dict)] <- ecoval.translate("R_macrophytes_doc_speciesreplaced",dict)
                  
                  data.species[i,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]  <- taxalist.dat[match(grouptaxon.taxalist.dat,taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk", dict)]),
                                                               ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]
                  data.species[i,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] <- taxalist.dat[match(grouptaxon.taxalist.dat,taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk", dict)]),
                                                               ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)]
                } else {
                  rows.delete[i] <- TRUE
                }
              }
            }
          }
        }   # end not bryophyte 
      }   # end if( !is.na(detunc.data.species) )
    }   # end for ( i in 1:nrow(data.species) )
  }   # end if ( sampling.protocol == "v2018" )
  # Correct overall bryophyta and characea coverage:
  if ( sum(rows.delete) > 0 )
  {
    for ( i in which(rows.delete) )
    {
      ind.sampling <- data.species[,ecoval.translate("A_macrophytes_site_sampleid",dict)] == 
                        data.species[i,ecoval.translate("A_macrophytes_site_sampleid",dict)]
      
      # bryophytes:
      
      if ( data.species[i,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)] == 
             ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_bry",dict) )
      {
        if ( !is.na(data.species[i,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)]) )
        {
          ind.brysum <- which(ind.sampling & 
                                data.species[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)] == 
                                  ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_brysum",dict) )
          if ( length(ind.brysum) == 1 ) 
          {
            data.species[ind.brysum,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] =
              data.species[ind.brysum,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] -
              data.species[i,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)]
          }
        }
      }
      
      # characea:
      
      if ( data.species[i,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)] == 
           ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_cha",dict) )
      {
        if ( !is.na(data.species[i,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)]) )
        {
          ind.chasum <- which(ind.sampling & 
                                data.species[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)] == 
                                ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_chasum",dict) )
          if ( length(ind.chasum) == 1 ) 
          {
            data.species[ind.chasum,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] =
              data.species[ind.chasum,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] -
              data.species[i,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)]
          }
        }
      }
    }
  }
  # Remove species that did not conform to determination quality requirements
  species.insuff.determ. <- data.species[rows.delete,]
  data.species           <- data.species[!rows.delete,]
  
  # Aggregate douplicated species (can occur due to aggregation made above), aggregate algae, 
  # and add aggregate taxon for bryophytes and characea if needed:
  
  ind.bry <- data.species[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
    taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]==
                                                                               ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_bry",dict)]
  ind.brysum <- data.species[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
    taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]==
                                                                               ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_brysum",dict)]
  ind.cha <- data.species[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
    taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]==
                                                                               ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_cha",dict)]
  ind.chasum <- data.species[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
    taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]==
                                                                               ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_chasum",dict)]
  rows.delete <- rep(FALSE,nrow(data.species))
  rows.add <- data.species[0,]
  sampling.unique <- unique(data.species[,ecoval.translate("A_macrophytes_site_sampleid",dict)])
  dg.abs.check <- rep("",length(sampling.unique))
  for ( i in 1:length(sampling.unique) ) 
  {
    ind.sampling <- data.species[,ecoval.translate("A_macrophytes_site_sampleid",dict)] == sampling.unique[i]
    ind.samp <- which(ind.sampling)
    if ( length(ind.samp) > 0 )
    {
      # replace algae species by aggregate:
      
      ind <- ind.samp[data.species[ind.sampling,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] == 
                               ecoval.translate("L_macrophytes_taxalist_growthform_assess_algae",dict)]
      if ( length(ind) > 0 )
      {
        for ( j in ind )
        {
          row.taxalist <- which(taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)] == 
                                  data.species[j,ecoval.translate("A_macrophytes_species_number_msk",dict)])
          taxagroup.assign <- taxalist.dat[row.taxalist,ecoval.translate("A_macrophytes_taxalist_taxagroups_assign",dict)]
          if ( !is.na(taxagroup.assign) )
          {
            data.species[j,ecoval.translate("A_macrophytes_species_number_msk",dict)] <- 
              taxagroup.assign
            data.species[j,ecoval.translate("A_macrophytes_species_name_latin",dict)] <- 
              taxalist.dat[which(taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)] == taxagroup.assign),
                                 ecoval.translate("A_macrophytes_species_name_latin",dict)]
          }
        }
      }
      
      # add aggregate taxa for bryophytes and characea if needed:
      
      if ( sum(ind.bry&ind.sampling) > 0 & sum(ind.brysum&ind.sampling)==0 )
      {
        rows.add <- rbind(rows.add,rep(NA,ncol(rows.add)))
        colnames(rows.add) <- colnames(data.species)
        ind.add <- nrow(rows.add)
        ind.copy <- which(ind.bry&ind.sampling)[1]
        rows.add[ind.add,ecoval.translate("A_macrophytes_site_siteid",dict)] <- 
          data.species[ind.copy,ecoval.translate("A_macrophytes_site_siteid",dict)]
        rows.add[ind.add,ecoval.translate("A_macrophytes_site_samplingdate",dict)] <- 
          data.species[ind.copy,ecoval.translate("A_macrophytes_site_samplingdate",dict)]
        rows.add[ind.add,ecoval.translate("A_macrophytes_site_sampleid",dict)] <- 
          data.species[ind.copy,ecoval.translate("A_macrophytes_site_sampleid",dict)]
        rows.add[ind.add,ecoval.translate("A_macrophytes_species_number_msk",dict)] <- 
          taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]==
                                                                                   ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_brysum",dict)]        
        rows.add[ind.add,ecoval.translate("A_macrophytes_species_name_latin",dict)] <- 
          taxalist.dat[,ecoval.translate("A_macrophytes_species_name_latin",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]==
                                                                                     ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_brysum",dict)]        
        rows.add[ind.add,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] <- 
          sum(data.species[ind.bry&ind.sampling,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)])
        rows.add[ind.add,ecoval.translate("A_macrophytes_species_artificialsubstrate_relcover_percent",dict)] <- 
          sum(data.species[ind.bry&ind.sampling,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)]*
                data.species[ind.bry&ind.sampling,ecoval.translate("A_macrophytes_species_artificialsubstrate_relcover_percent",dict)])/
          sum(data.species[ind.bry&ind.sampling,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)])
        rows.add[ind.add,ecoval.translate("A_macrophytes_species_determinationuncertainty",dict)] <- 1
        rows.add[ind.add,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)] <- 
          ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_brysum",dict)
        rows.add[ind.add,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] <- 
          ecoval.translate("L_macrophytes_taxalist_growthform_assess_bryophyte",dict)
        if ( is.na(rows.add[ind.add,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)]) ) dg.abs.check[i] <- paste(dg.abs.check[i],"bry")
      }

      if ( sum(ind.cha&ind.sampling) > 0 & sum(ind.chasum&ind.sampling)==0 )
      {
        rows.add <- rbind(rows.add,rep(NA,ncol(rows.add)))
        colnames(rows.add) <- colnames(data.species)
        ind.add <- nrow(rows.add)
        ind.copy <- which(ind.cha&ind.sampling)[1]
        rows.add[ind.add,ecoval.translate("A_macrophytes_site_siteid",dict)] <- 
          data.species[ind.copy,ecoval.translate("A_macrophytes_site_siteid",dict)]
        rows.add[ind.add,ecoval.translate("A_macrophytes_site_samplingdate",dict)] <- 
          data.species[ind.copy,ecoval.translate("A_macrophytes_site_samplingdate",dict)]
        rows.add[ind.add,ecoval.translate("A_macrophytes_site_sampleid",dict)] <- 
          data.species[ind.copy,ecoval.translate("A_macrophytes_site_sampleid",dict)]
        rows.add[ind.add,ecoval.translate("A_macrophytes_species_number_msk",dict)] <- 
          taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]==
                                                                                     ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_chasum",dict)]        
        rows.add[ind.add,ecoval.translate("A_macrophytes_species_name_latin",dict)] <- 
          taxalist.dat[,ecoval.translate("A_macrophytes_species_name_latin",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]==
                                                                                     ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_chasum",dict)]        
        rows.add[ind.add,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] <- 
          sum(data.species[ind.cha&ind.sampling,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)])
        rows.add[ind.add,ecoval.translate("A_macrophytes_species_artificialsubstrate_relcover_percent",dict)] <- 
          sum(data.species[ind.cha&ind.sampling,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)]*
                data.species[ind.cha&ind.sampling,ecoval.translate("A_macrophytes_species_artificialsubstrate_relcover_percent",dict)])/
          sum(data.species[ind.cha&ind.sampling,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)])
        rows.add[ind.add,ecoval.translate("A_macrophytes_species_determinationuncertainty",dict)] <- 1
        rows.add[ind.add,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)] <- 
          ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_chasum",dict)
        rows.add[ind.add,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] <- 
          ecoval.translate("L_macrophytes_taxalist_growthform_assess_aquatic",dict)
        if ( is.na(rows.add[ind.add,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)]) ) dg.abs.check[i] <- paste(dg.abs.check[i],"cha")
      }
      
      # aggregate identical taxa:
      
      species_no <- unique(data.species[ind.sampling,ecoval.translate("A_macrophytes_species_number_msk",dict)])
      for ( j in 1:length(species_no) )
      {
        ind <- ind.samp[data.species[ind.sampling,ecoval.translate("A_macrophytes_species_number_msk",dict)] == species_no[j]]
        if ( length(ind) > 1 )
        {
          data.species[ind[1],ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] <- 
            sum(data.species[ind,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)])
          rows.delete[ind[2:length(ind)]] <- TRUE
        }
      }
    }
  }
  data.species <- data.species[!rows.delete,]
  if ( nrow(rows.add) > 0 ) data.species <- rbind(data.species,rows.add)
  if( sum(dg.abs.check == "bry",na.rm=T) > 0 ) {
    quality.out <- Erhebung_Code[substring(dg.abs.check,1,3) == "bry"]
    quality.out <- cbind(t(sapply(quality.out , split.last.underline)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("A_macrophytes_species_absolutecover_percent",dict), nrow(quality.out)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_abscoveragebrymissing", dict), nrow(quality.out)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_error", dict), nrow(quality.out)))
    colnames(quality.out) <- colnames(species.quality)
    species.quality <- rbind(species.quality, quality.out)
  }
  if( sum(dg.abs.check == "cha",na.rm=T)>0 | sum(dg.abs.check == "brycha",na.rm =T) > 0 ) {
    quality.out <- Erhebung_Code[dg.abs.check == "cha" | dg.abs.check == "brycha"]
    quality.out <- cbind(t(sapply(quality.out , split.last.underline)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("A_macrophytes_species_absolutecover_percent",dict), nrow(quality.out)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_abscoveragechamissing", dict), nrow(quality.out)))
    quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_error", dict), nrow(quality.out)))
    colnames(quality.out) <- colnames(species.quality)
    species.quality <- rbind(species.quality, quality.out)
  }
  
  # Combine all data containing removed species, either due to taxalist or insufficient determination, in one data.frame
  species.insuff.determ.[,ecoval.translate("A_macrophytes_species_reason_elimination",dict)] <- rep(ecoval.translate("A_macrophytes_species_reason_insufficientquality",dict), nrow(species.insuff.determ.))
  species.not.taxalist[,ecoval.translate("A_macrophytes_species_reason_elimination",dict)] <- rep(ecoval.translate("A_macrophytes_species_reason_notontaxalist",dict), nrow(species.not.taxalist))
  
  # Remove additional columns in species.insuff.determ. introduced during data perparation after reductioh to species not on taxalist
  species.insuff.determ. <- species.insuff.determ.[,colnames(species.insuff.determ.) %in% colnames(species.not.taxalist)]
  
  # Create output data.frame with all removed species
  species.removed <- rbind(species.insuff.determ., species.not.taxalist, stringsAsFactors = FALSE)
  
  # Complement "species.quality" data.frame with data-set information
  species.quality <- cbind(species.quality, rep(ecoval.translate("R_macrophytes_error_speciesdata", dict), nrow(species.quality)))
  colnames(species.quality)[ncol(species.quality)] <- ecoval.translate("R_macrophytes_error_dataset", dict)
  
  # Return list with generated outputs
  return(list(species.assess       = data.species, 
              species.removed      = species.removed, 
              species.quality      = species.quality, 
              taxalist             = taxalist.dat))
}



# ================================
# FUNCTION TO CALCULATE ATTRIBUTES
# ================================

msk.macrophytes.2017.calc.attrib <- function(data.site,
                                             data.species,
                                             language     = "English",
                                             dictionaries = NA)
{
  # get dictionary:
  
  dict <- ecoval.dict(language,dictionaries)
  
  # get taxa list:
  
  taxalist.dat <- ecoval::msk.macrophytes.2017_ListTaxa
  for ( j in 1:ncol(taxalist.dat) ) colnames(taxalist.dat)[j] <- ecoval.translate(colnames(taxalist.dat)[j],dict)
  col.gf <- match(ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict),colnames(taxalist.dat))
  if ( !is.na(col.gf) ) for ( i in 1:nrow(taxalist.dat) ) taxalist.dat[i,col.gf] <- ecoval.translate(taxalist.dat[i,col.gf],dict)
  col.gfabbrev <- match(ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict),colnames(taxalist.dat))
  if ( !is.na(col.gfabbrev) ) for ( i in 1:nrow(taxalist.dat) ) taxalist.dat[i,col.gfabbrev] <- ecoval.translate(taxalist.dat[i,col.gfabbrev],dict)
  col.es <- match(ecoval.translate("A_macrophytes_taxalist_growthform_assess_twoforms_grfo",dict),colnames(taxalist.dat))
  if ( !is.na(col.es) ) for ( i in 1:nrow(taxalist.dat) ) taxalist.dat[i,col.es] <- ifelse(is.na(taxalist.dat[i,col.es]),NA,ecoval.translate(taxalist.dat[i,col.es],dict))
  col.trait <- match(ecoval.translate("A_macrophytes_taxalist_determination_trait",dict),colnames(taxalist.dat))
  if ( !is.na(col.trait) ) for ( i in 1:nrow(taxalist.dat) ) taxalist.dat[i,col.trait] <- ifelse(is.na(taxalist.dat[i,col.trait]),NA,ecoval.translate(taxalist.dat[i,col.trait],dict))
  col.cons <- match(ecoval.translate("A_macrophytes_taxalist_conservationinfo",dict),colnames(taxalist.dat))
  if ( !is.na(col.cons) ) for ( i in 1:nrow(taxalist.dat) ) taxalist.dat[i,col.cons] <- ifelse(is.na(taxalist.dat[i,col.cons]),NA,ecoval.translate(taxalist.dat[i,col.cons],dict))
  
  # Create data.frame to collect calculated attributes
  
  attrib.names <- c(ecoval.translate("A_macrophytes_taxa_all_richness_count",dict),
                    ecoval.translate("A_macrophytes_taxa_helophytes_richness_count",dict),
                    ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict),
                    ecoval.translate("A_macrophytes_taxa_bryophytes_richness_count",dict),
                    ecoval.translate("A_macrophytes_growthform_all_richness_count",dict),
                    ecoval.translate("A_macrophytes_growthform_helophytes_richness_count",dict),
                    ecoval.translate("A_macrophytes_growthform_aquatic_richness_count",dict),
                    ecoval.translate("A_macrophytes_taxa_helophytes_relcover_percent",dict),
                    ecoval.translate("A_macrophytes_taxa_aquatic_relcover_percent",dict),
                    ecoval.translate("A_macrophytes_taxa_bryophytes_relcover_percent",dict),
                    ecoval.translate("A_macrophytes_taxa_bryophytes_adjusted_relcover_percent",dict),
                    ecoval.translate("A_macrophytes_taxa_bryophytes_artificialsubstrate_relcover_percent",dict),
                    ecoval.translate("A_macrophytes_filamentousgreenalgae_relcover_percent",dict),
                    ecoval.translate("A_macrophytes_allvegetation_abscover_percent",dict),
                    ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict),
                    ecoval.translate("A_macrophytes_taxa_helophytes_abscover_percent",dict),
                    ecoval.translate("A_macrophytes_taxa_aquatic_abscover_percent",dict),
                    ecoval.translate("A_macrophytes_taxa_bryophytes_abscover_percent",dict),
                    ecoval.translate("A_macrophytes_filamentousgreenalgae_abscover_percent",dict),
                    ecoval.translate("A_macrophytes_heipindex_bryophyteonespecies",dict),
                    ecoval.translate("A_macrophytes_neophytes_relcover_percent",dict),
                    ecoval.translate("A_macrophytes_taxa_neophytes_richness_count",dict),
                    ecoval.translate("A_macrophytes_indexspeciesmac1_count",dict),
                    ecoval.translate("A_macrophytes_indexspeciesmac2_count",dict),
                    ecoval.translate("A_macrophytes_indexspeciesmac3_count",dict),
                    ecoval.translate("A_macrophytes_indexspeciesbry1_count",dict),
                    ecoval.translate("A_macrophytes_indexspeciesbry2_count",dict),
                    ecoval.translate("A_macrophytes_indexspeciesbry3_count",dict),
                    ecoval.translate("A_macrophytes_priorityspecies1_count",dict),
                    ecoval.translate("A_macrophytes_priorityspecies2_count",dict),
                    ecoval.translate("A_macrophytes_priorityspecies3_count",dict),
                    ecoval.translate("A_macrophytes_priorityspecies4_count",dict),
                    ecoval.translate("A_macrophytes_taxa_aquatic",dict),
                    ecoval.translate("A_macrophytes_taxa_helophytes",dict),
                    ecoval.translate("A_macrophytes_taxa_bryophytes",dict))
  
  attrib.dat <- data.frame(matrix(NA, nrow = nrow(data.site), ncol = length(attrib.names)), stringsAsFactors = FALSE)
  colnames(attrib.dat) <- attrib.names
  rownames(attrib.dat) <- rownames(data.site)
  
  # Fill data.frame with entries as if no vegetation is present at sites. For sites with vegetation cover these values will be
  # later replaced by calculated attribute values
  
  attrib.dat[, c(ecoval.translate("A_macrophytes_taxa_all_richness_count",dict),
                 ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict),
                 ecoval.translate("A_macrophytes_taxa_helophytes_richness_count",dict),
                 ecoval.translate("A_macrophytes_taxa_bryophytes_richness_count",dict),
                 ecoval.translate("A_macrophytes_growthform_all_richness_count",dict),
                 ecoval.translate("A_macrophytes_growthform_helophytes_richness_count",dict),
                 ecoval.translate("A_macrophytes_growthform_aquatic_richness_count",dict),
                 ecoval.translate("A_macrophytes_neophytes_relcover_percent",dict),
                 ecoval.translate("A_macrophytes_taxa_neophytes_richness_count",dict),
                 ecoval.translate("A_macrophytes_taxa_aquatic_relcover_percent",dict),
                 ecoval.translate("A_macrophytes_taxa_helophytes_relcover_percent",dict),
                 ecoval.translate("A_macrophytes_taxa_bryophytes_relcover_percent",dict),
                 ecoval.translate("A_macrophytes_taxa_bryophytes_adjusted_relcover_percent",dict),
                 ecoval.translate("A_macrophytes_filamentousgreenalgae_relcover_percent",dict),
                 ecoval.translate("A_macrophytes_allvegetation_abscover_percent",dict),
                 ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict),
                 ecoval.translate("A_macrophytes_taxa_aquatic_abscover_percent",dict),
                 ecoval.translate("A_macrophytes_taxa_helophytes_abscover_percent",dict),
                 ecoval.translate("A_macrophytes_taxa_bryophytes_abscover_percent",dict),
                 ecoval.translate("A_macrophytes_filamentousgreenalgae_abscover_percent",dict))] <- 0
  
  # Calculate attributes for all data contained in data.site
  
  for ( i in 1:nrow(attrib.dat) ) {
    
    # Create data.frame with vegetation data of sampling i:
    
    data.select <- data.species[data.species[,ecoval.translate("A_macrophytes_site_sampleid",dict)] == rownames(attrib.dat)[i],
                                c(ecoval.translate("A_macrophytes_species_name_latin",dict),
                                  ecoval.translate("A_macrophytes_species_number_msk",dict),
                                  ecoval.translate("A_macrophytes_species_absolutecover_percent", dict),
                                  ecoval.translate("A_macrophytes_species_artificialsubstrate_relcover_percent",dict),
                                  ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict),
                                  ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict))]
    
    # get indices of bryophytes and characea:
    
    ind.bry <- data.select[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
      taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]==
                                                                                 ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_bry",dict)]
    ind.brysum <- data.select[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
      taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]==
                                                                                 ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_brysum",dict)]
    ind.cha <- data.select[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
      taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]==
                                                                                 ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_cha",dict)]
    ind.chasum <- data.select[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
      taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]==
                                                                                 ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_chasum",dict)]
    ind.alg <- data.select[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
      taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)]==
                                                                                 ecoval.translate("L_macrophytes_taxalist_growthform_assess_algae",dict)]
    ind.helo <- data.select[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
      taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)]==
                                                                                 ecoval.translate("L_macrophytes_taxalist_growthform_assess_helophyte",dict)]
    ind.aquatic <- data.select[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
      taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)]==
                                                                                 ecoval.translate("L_macrophytes_taxalist_growthform_assess_aquatic",dict)]

    if( nrow(data.select) > 0 ) {
      
      ## ABSOLUTE COVER / BIOMASS ATTRIBUTES
      
      # Absolute cover of all vegetation at site
      
      attrib.dat[i, ecoval.translate("A_macrophytes_allvegetation_abscover_percent",dict)] <- 
        sum(data.select[!(ind.bry|ind.cha),ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)])

      # Calculate and add relative cover per species (Colname: "Rel_Deckung") in selected vegetation data
      
      data.select$Rel_Deckung <- data.select[, ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] * 100 /
        attrib.dat[i, ecoval.translate("A_macrophytes_allvegetation_abscover_percent",dict)] 

      # Absolute cover of only higher macrophytes
      
      attrib.dat[i, ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict)] <- 
        sum(data.select[!(ind.bry|ind.cha|ind.alg),ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)])

      # Add relevant information from taxalist to data-set
      data.select$neophyte_info <- rep(NA, nrow(data.select))
      data.select$priority      <- rep(NA, nrow(data.select))
      data.select$guide_value   <- rep(NA, nrow(data.select))
      
      for (j in 1:nrow(data.select)) {
        row.taxalist <- match(data.select[j,ecoval.translate("A_macrophytes_species_number_msk",dict)],
                              taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)])
        
        data.select[j, "neophyte_info"] <- taxalist.dat[row.taxalist, ecoval.translate("A_macrophytes_taxalist_conservationinfo",dict)]
        data.select[j, "priority"]      <- taxalist.dat[row.taxalist, ecoval.translate("A_macrophytes_taxalist_priorityspecies",dict)]
        data.select[j, "guide_value"]   <- taxalist.dat[row.taxalist, ecoval.translate("A_macrophytes_taxalist_indexspecies",dict)]
      }
      
      # Generate data.frame for calculation of richness and dominance by bundling species that can be emerged or submerged
      
      data.select.richness <- data.select
      rows.delete <- rep(FALSE,nrow(data.select.richness))
      for ( j in 1:nrow(data.select.richness) )
      {
        row.taxalist <- match(data.select.richness[j,ecoval.translate("A_macrophytes_species_number_msk",dict)],
                              taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)])
        number.msk.grfoval <- taxalist.dat[row.taxalist,ecoval.translate("A_macrophytes_taxalist_growthform_assess_twoforms_grfoval",dict)]
        if ( !is.na(number.msk.grfoval) )
        {
          row.data.select <- match(number.msk.grfoval,data.select.richness[,ecoval.translate("A_macrophytes_species_number_msk",dict)])
          if ( !is.na(row.data.select) )
          {
            # add absolute coverage from row that should be eliminated:
            data.select.richness[row.data.select,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] <-
              data.select.richness[row.data.select,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] +
                data.select.richness[j,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)]
            # add absolute coverage from row that should be eliminated:
            data.select.richness[row.data.select,"Rel_Deckung"] <-
              data.select.richness[row.data.select,"Rel_Deckung"] +
                data.select.richness[j,"Rel_Deckung"]
            # mark row for deletion:
            rows.delete[j] <- TRUE
          }
        }
      }
      if ( sum(rows.delete) > 0 ) data.select.richness <- data.select.richness[!rows.delete,]

      # get indices of bryophytes and characea:
      
      ind.richness.bry <- data.select.richness[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
        taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]==
                                                                                   ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_bry",dict)]
      ind.richness.brysum <- data.select.richness[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
        taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]==
                                                                                   ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_brysum",dict)]
      ind.richness.cha <- data.select.richness[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
        taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]==
                                                                                   ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_cha",dict)]
      ind.richness.chasum <- data.select.richness[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
        taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]==
                                                                                   ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_chasum",dict)]
      ind.richness.alg <- data.select.richness[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
        taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)]==
                                                                                   ecoval.translate("L_macrophytes_taxalist_growthform_assess_algae",dict)]
      ind.richness.helo <- data.select.richness[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
        taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)]==
                                                                                   ecoval.translate("L_macrophytes_taxalist_growthform_assess_helophyte",dict)]
      ind.richness.aquatic <- data.select.richness[,ecoval.translate("A_macrophytes_species_number_msk",dict)] %in%
        taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)][taxalist.dat[,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)]==
                                                                                   ecoval.translate("L_macrophytes_taxalist_growthform_assess_aquatic",dict)]
      
      # # Generate data.frames with only data on higher macrophytes (i.e. no algae)
      # dat.makro <- data.select[data.select[,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] != ecoval.translate("L_macrophytes_taxalist_growthform_assess_algae",dict),]
      # dat.makro.richness <- data.select.richness[data.select.richness[,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] != ecoval.translate("L_macrophytes_taxalist_growthform_assess_algae",dict),]

      ## DIVERSITY ATTRIBUTES
      
      # SPECIES RICHNESS
      
      # number of summary taxa and algae:
      
      num.bry     <- sum(ind.richness.bry); if( num.bry==0 & sum(ind.richness.brysum)>0 ) num.bry <- 1
      num.aquatic <- sum(ind.richness.aquatic) - sum(ind.richness.chasum); if ( sum(ind.richness.cha)==0 & sum(ind.richness.chasum)>0 ) num.aquatic <- num.aquatic+1
      num.helo    <- sum(ind.richness.helo)
      num.alg     <- sum(ind.richness.alg)
      
      # Species richness of all growthforms, without algae:
      
      attrib.dat[i,ecoval.translate("A_macrophytes_taxa_all_richness_count",dict)] <- num.bry + num.aquatic + num.helo

      # Species richness per growthform group (i.e. aquatic, helophyte, bryophyte)

      attrib.dat[i,ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict)] <- num.aquatic
      
      attrib.dat[i,ecoval.translate("A_macrophytes_taxa_helophytes_richness_count",dict)] <- num.helo

      attrib.dat[i,ecoval.translate("A_macrophytes_taxa_bryophytes_richness_count",dict)]  <- num.bry
      
      attrib.dat[i,ecoval.translate("A_macrophytes_taxa_neophytes_richness_count",dict)] <-
        sum(data.select.richness[!(ind.richness.brysum|ind.richness.chasum|ind.richness.alg),"neophyte_info"] == "N",na.rm=T)
      
      # GROWTHFORM RICHNESS
      
      attrib.dat[i,ecoval.translate("A_macrophytes_growthform_all_richness_count",dict)] <- 
        length(na.omit(unique(data.select[!(ind.brysum|ind.chasum|ind.alg),ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)])))

      attrib.dat[i,ecoval.translate("A_macrophytes_growthform_helophytes_richness_count",dict)] <- 
        length(na.omit(unique(data.select[ind.helo,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)])))

      attrib.dat[i,ecoval.translate("A_macrophytes_growthform_aquatic_richness_count",dict)] <- 
        length(na.omit(unique(data.select[ind.aquatic&(!ind.chasum),ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)])))

      ## COMMUNITY COMPOSITION
      
      # HEIPs DOMINANCE INDEX (calculated with data.select.richness)
      #dat.rel.sw <- data.select.richness[!(ind.richness.bry|ind.richness.cha|ind.richness.alg), "Rel_Deckung"]
      dat.rel.sw <- data.select.richness[!(ind.richness.bry|ind.richness.cha), "Rel_Deckung"]  # previous implementation was with algae
      n.taxa.sw  <- length(na.omit(dat.rel.sw))
      sw.index.bry <- -(sum((dat.rel.sw/100) * log((dat.rel.sw/100))))
      if ( n.taxa.sw == 1 ) sw.index.bry <- 0
      if ( n.taxa.sw == 0 ) sw.index.bry <- NA
      
      attrib.dat[i, ecoval.translate("A_macrophytes_heipindex_bryophyteonespecies",dict)] <- (exp(sw.index.bry) - 1) / (n.taxa.sw - 1) #ATTRIBUT
      if ( n.taxa.sw == 1 ) attrib.dat[i, ecoval.translate("A_macrophytes_heipindex_bryophyteonespecies",dict)] <- 0 # Minimaler Wert fuer Heip Index ist 0.006 (cf. Maggurran A., Measuring biological diversity )
      if ( n.taxa.sw == 0 ) attrib.dat[i, ecoval.translate("A_macrophytes_heipindex_bryophyteonespecies",dict)] <- NA
      
      rm(list = c("dat.rel.sw", "n.taxa.sw", "sw.index.bry"))
      
      # NEOPHYTES RELATIVE COVER
      ind.neo <- data.select[, "neophyte_info"] == "N"; ind.neo <- ifelse(is.na(ind.neo),FALSE,ind.neo)
      attrib.dat[i,ecoval.translate("A_macrophytes_neophytes_relcover_percent",dict)] <-
        sum(data.select[ind.neo , "Rel_Deckung"]) #ATTRIBUT

      # RELATIVE COVER OF GROWTHFORMS (i.e. AQUATIC, HELOPHYTES, and BRYOPHYTES)
      attrib.dat[i,ecoval.translate("A_macrophytes_taxa_aquatic_relcover_percent",dict)] <- 
        sum(data.select[ind.aquatic&!ind.cha,"Rel_Deckung"])
      if ( !is.na(attrib.dat[i,ecoval.translate("A_macrophytes_taxa_aquatic_relcover_percent",dict)]) )
      {
        if ( attrib.dat[i, ecoval.translate("A_macrophytes_taxa_aquatic_relcover_percent",dict)] - 100 > 0 & 
             attrib.dat[i, ecoval.translate("A_macrophytes_taxa_aquatic_relcover_percent",dict)] - 100 < 0.01 )
        attrib.dat[i, ecoval.translate("A_macrophytes_taxa_aquatic_relcover_percent",dict)] <- 100
      }
      
      attrib.dat[i,ecoval.translate("A_macrophytes_taxa_helophytes_relcover_percent",dict)] <- 
        sum(data.select[ind.helo,"Rel_Deckung"])
      if ( !is.na(attrib.dat[i,ecoval.translate("A_macrophytes_taxa_helophytes_relcover_percent",dict)]) )
      {
        if ( attrib.dat[i, ecoval.translate("A_macrophytes_taxa_helophytes_relcover_percent",dict)] - 100 > 0 & 
             attrib.dat[i, ecoval.translate("A_macrophytes_taxa_helophytes_relcover_percent",dict)] - 100 < 0.01 )
          attrib.dat[i,ecoval.translate("A_macrophytes_taxa_helophytes_relcover_percent",dict)] <- 100
      }
      
      attrib.dat[i,ecoval.translate("A_macrophytes_taxa_bryophytes_relcover_percent",dict)] <- 
        sum(data.select[ind.brysum,"Rel_Deckung"])
      if ( !is.na(attrib.dat[i,ecoval.translate("A_macrophytes_taxa_bryophytes_relcover_percent",dict)]) )
      {
        if ( attrib.dat[i,ecoval.translate("A_macrophytes_taxa_bryophytes_relcover_percent",dict)] - 100 > 0 & 
             attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_relcover_percent",dict)] - 100 < 0.01 )
          attrib.dat[i,ecoval.translate("A_macrophytes_taxa_bryophytes_relcover_percent",dict)] <- 100
      }
      
      attrib.dat[i,ecoval.translate("A_macrophytes_filamentousgreenalgae_relcover_percent",dict)] <- 
        sum(data.select[ind.alg,"Rel_Deckung"])
      if ( !is.na(attrib.dat[i,ecoval.translate("A_macrophytes_filamentousgreenalgae_relcover_percent",dict)]) )
      {
        if ( attrib.dat[i,ecoval.translate("A_macrophytes_filamentousgreenalgae_relcover_percent",dict)] - 100 > 0 & 
             attrib.dat[i, ecoval.translate("A_macrophytes_filamentousgreenalgae_relcover_percent",dict)] - 100 < 0.01 )
          attrib.dat[i,ecoval.translate("A_macrophytes_filamentousgreenalgae_relcover_percent",dict)] <- 100
      }
      
      # ADJUST RELATIVE COVER BRYOPHYTES TO PROPORTION ON ARTIFICIAL SUBSTRATE FOR ASSESSMENT
      attrib.dat[i,ecoval.translate("A_macrophytes_taxa_bryophytes_adjusted_relcover_percent",dict)] <- NA
      if ( !is.na(attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_relcover_percent",dict)]) )
      {
        if( attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_relcover_percent",dict)] > 0 ) {
          attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_artificialsubstrate_relcover_percent",dict)] <- 
            data.select[ind.brysum,ecoval.translate("A_macrophytes_species_artificialsubstrate_relcover_percent", dict)]  # Attribute for illustrative purposes in value function
        
          if( is.na(attrib.dat[i,ecoval.translate("A_macrophytes_taxa_bryophytes_artificialsubstrate_relcover_percent",dict)]) ) {
            attrib.dat[i,ecoval.translate("A_macrophytes_taxa_bryophytes_adjusted_relcover_percent",dict)] <- 
              attrib.dat[i,ecoval.translate("A_macrophytes_taxa_bryophytes_relcover_percent",dict)] #ATTRIBUT
          } else {
            attrib.dat[i,ecoval.translate("A_macrophytes_taxa_bryophytes_adjusted_relcover_percent",dict)] <- 
              attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_relcover_percent",dict)] * 
                 (1 - attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_artificialsubstrate_relcover_percent",dict)]/100) +
              0.33 * attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_relcover_percent",dict)] * 
                         attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_artificialsubstrate_relcover_percent",dict)]/100 #ATTRIBUT
          }
        } else {
          attrib.dat[i,ecoval.translate("A_macrophytes_taxa_bryophytes_adjusted_relcover_percent",dict)] <- 0
        }
      }
      
      # ABSOLUTE COVER BY GROWTHFORMS, i.e. AQUATIC, HELOPHYTES, and BRYOPHYTES
      attrib.dat[i,ecoval.translate("A_macrophytes_taxa_aquatic_abscover_percent",dict)] <- 
        sum(data.select[ind.aquatic&!ind.cha,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)])
      if ( !is.na(attrib.dat[i,ecoval.translate("A_macrophytes_taxa_aquatic_abscover_percent",dict)]) )
      {
        if ( attrib.dat[i,ecoval.translate("A_macrophytes_taxa_aquatic_abscover_percent",dict)] - 100 > 0 & 
             attrib.dat[i, ecoval.translate("A_macrophytes_taxa_aquatic_abscover_percent",dict)] - 100 < 0.01 )
          attrib.dat[i,ecoval.translate("A_macrophytes_taxa_aquatic_abscover_percent",dict)] <- 100
      }
      
      attrib.dat[i,ecoval.translate("A_macrophytes_taxa_helophytes_abscover_percent",dict)] <- 
        sum(data.select[ind.helo,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)])
      if ( !is.na(attrib.dat[i,ecoval.translate("A_macrophytes_taxa_helophytes_abscover_percent",dict)]) )
      {
        if ( attrib.dat[i, ecoval.translate("A_macrophytes_taxa_helophytes_abscover_percent",dict)] - 100 > 0 & 
             attrib.dat[i, ecoval.translate("A_macrophytes_taxa_helophytes_abscover_percent",dict)] - 100 < 0.01 )
          attrib.dat[i,ecoval.translate("A_macrophytes_taxa_helophytes_abscover_percent",dict)] <- 100
      }
      
      attrib.dat[i,ecoval.translate("A_macrophytes_taxa_bryophytes_abscover_percent",dict)] <- 
        sum(data.select[ind.brysum,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)])
      if ( !is.na(attrib.dat[i,ecoval.translate("A_macrophytes_taxa_bryophytes_abscover_percent",dict)]) )
      {
        if ( attrib.dat[i,ecoval.translate("A_macrophytes_taxa_bryophytes_abscover_percent",dict)] - 100 > 0 & 
             attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_abscover_percent",dict)] - 100 < 0.01 )
          attrib.dat[i,ecoval.translate("A_macrophytes_taxa_bryophytes_abscover_percent",dict)] <- 100
      }
      
      attrib.dat[i,ecoval.translate("A_macrophytes_filamentousgreenalgae_abscover_percent",dict)] <- 
        sum(data.select[ind.alg,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)]) #ATTRIBUT
      if ( !is.na(attrib.dat[i,ecoval.translate("A_macrophytes_filamentousgreenalgae_abscover_percent",dict)]) )
      {
        if ( attrib.dat[i,ecoval.translate("A_macrophytes_filamentousgreenalgae_abscover_percent",dict)] - 100 > 0 & 
             attrib.dat[i, ecoval.translate("A_macrophytes_filamentousgreenalgae_abscover_percent",dict)] - 100 < 0.01 )
          attrib.dat[i,ecoval.translate("A_macrophytes_filamentousgreenalgae_abscover_percent",dict)] <- 100
      }
      
      ## SPECIES QUALITY / CONSERVAT
      
      # PRIORITY SPECIES
      
      # NUMBER OF SPECIES FOR EACH PRIORITY CATEGORY
      attrib.dat[i,ecoval.translate("A_macrophytes_priorityspecies1_count",dict)] <- sum(data.select[!ind.alg,"priority"] == 1, na.rm = T) #ATTRIBUT
      if( abs(attrib.dat[i,ecoval.translate("A_macrophytes_priorityspecies1_count",dict)]) == Inf ) {
        attrib.dat[i,ecoval.translate("A_macrophytes_priorityspecies1_count",dict)] <- 0
      }
      
      attrib.dat[i,ecoval.translate("A_macrophytes_priorityspecies2_count",dict)] <- sum(data.select[!ind.alg,"priority"] == 2, na.rm = T) #ATTRIBUT
      if( abs(attrib.dat[i,ecoval.translate("A_macrophytes_priorityspecies2_count",dict)]) == Inf ) {
        attrib.dat[i,ecoval.translate("A_macrophytes_priorityspecies2_count",dict)] <- 0
      }
      
      attrib.dat[i,ecoval.translate("A_macrophytes_priorityspecies3_count",dict)] <- sum(data.select[!ind.alg,"priority"] == 3, na.rm = T) #ATTRIBUT
      if( abs(attrib.dat[i,ecoval.translate("A_macrophytes_priorityspecies3_count",dict)]) == Inf ) {
        attrib.dat[i,ecoval.translate("A_macrophytes_priorityspecies3_count",dict)] <- 0
      }
      
      attrib.dat[i,ecoval.translate("A_macrophytes_priorityspecies4_count",dict)] <- sum(data.select[!ind.alg,"priority"] == 4, na.rm = T) #ATTRIBUT
      if( abs(attrib.dat[i,ecoval.translate("A_macrophytes_priorityspecies4_count",dict)]) == Inf ) {
        attrib.dat[i,ecoval.translate("A_macrophytes_priorityspecies4_count",dict)] <- 0
      }
      
      # GUIDE VALUES, NUMBER OF SPECIES ("Leitarten")
      
      # Anzahl Arten mit spezifischem Leitwert
      attrib.dat[i,ecoval.translate("A_macrophytes_indexspeciesmac1_count",dict)]  <- 
        sum(data.select[!(ind.alg|ind.bry|ind.brysum),"guide_value"] == 1, na.rm = T)
      if( abs(attrib.dat[i,ecoval.translate("A_macrophytes_indexspeciesmac1_count",dict)]) == Inf ) {
        attrib.dat[i,ecoval.translate("A_macrophytes_indexspeciesmac1_count",dict)] <- 0
      }
      
      attrib.dat[i,ecoval.translate("A_macrophytes_indexspeciesmac2_count",dict)]  <- 
        sum(data.select[!(ind.alg|ind.bry|ind.brysum),"guide_value"] == 2, na.rm = T)
      if( abs(attrib.dat[i,ecoval.translate("A_macrophytes_indexspeciesmac2_count",dict)]) == Inf ) {
        attrib.dat[i,ecoval.translate("A_macrophytes_indexspeciesmac2_count",dict)] <- 0
      }
      
      attrib.dat[i,ecoval.translate("A_macrophytes_indexspeciesmac3_count",dict)]  <- 
        sum(data.select[!(ind.alg|ind.bry|ind.brysum),"guide_value"] == 3, na.rm = T)
      if( abs(attrib.dat[i,ecoval.translate("A_macrophytes_indexspeciesmac3_count",dict)]) == Inf ) {
        attrib.dat[i,ecoval.translate("A_macrophytes_indexspeciesmac3_count",dict)] <- 0
      }

      # BRYOPHYTES

      attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesbry1_count",dict)]  <- sum(data.select[ind.bry,"guide_value"] == 1, na.rm = T)
      if( abs(attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesbry1_count",dict)]) == Inf ) {
        attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesbry1_count",dict)] <- 0
      }
      
      attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesbry2_count",dict)]  <- sum(data.select[ind.bry,"guide_value"] == 2, na.rm = T)
      if( abs(attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesbry2_count",dict)]) == Inf ) {
        attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesbry2_count",dict)] <- 0
      }
      
      attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesbry3_count",dict)]  <- sum(data.select[ind.bry,"guide_value"] == 3, na.rm = T)
      if( abs(attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesbry3_count",dict)]) == Inf ) {
        attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesbry3_count",dict)] <- 0
      }

      ## Collect taxon names grouped by growthform ZH
      
      attrib.dat[i,ecoval.translate("A_macrophytes_taxa_aquatic",dict)]    <- 
        paste(data.select[ind.aquatic&!ind.chasum,ecoval.translate("A_macrophytes_species_name_latin",dict)], collapse = "; ")
      attrib.dat[i,ecoval.translate("A_macrophytes_taxa_helophytes",dict)] <- 
        paste(data.select[ind.helo,ecoval.translate("A_macrophytes_species_name_latin",dict)], collapse = "; ")
      attrib.dat[i,ecoval.translate("A_macrophytes_taxa_bryophytes",dict)] <- 
        paste(data.select[ind.bry,ecoval.translate("A_macrophytes_species_name_latin",dict)], collapse = "; ")
      
      rm(list = c("data.select"))
    }
    
  }
  
  # Return results
  
  # Data tables as tab-delimited text-files to output directiory
  # For this output the sampling event code is written in the first column
  # attrib.out <- data.frame(rownames(attrib.dat), attrib.dat, stringsAsFactors = FALSE)
  # colnames(attrib.out)[1] <- ecoval.translate("A_macrophytes_site_sampleid",dict)
  # write.table(attrib.out, paste(OutputDir,"AttributeData_Final.dat", sep = ""), quote = FALSE, row.names = FALSE, na = "", sep = "\t")
  
  # Generated data.frame to environment
  return(attrib.dat)
}

