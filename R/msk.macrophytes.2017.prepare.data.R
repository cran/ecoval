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
    # Add row names and unique sample identifier column
    
    rownames(data.site) <- paste(data.site[,ecoval.translate("A_macrophytes_site_siteid",dict)],
                                 data.site[,ecoval.translate("A_macrophytes_site_samplingdate",dict)], sep = "_")
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
  # get dictionary:
  
  dict <- ecoval.dict(language,dictionaries)
  
  # get taxa list:
  
  taxalist.dat <- ecoval::msk.macrophytes.2017_ListTaxa
  for ( j in 1:ncol(taxalist.dat) ) colnames(taxalist.dat)[j] <- ecoval.translate(colnames(taxalist.dat)[j],dict)
  col.gf <- match(ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict),colnames(taxalist.dat))
  if ( !is.na(col.gf) ) for ( i in 1:nrow(taxalist.dat) ) taxalist.dat[i,col.gf] <- ecoval.translate(taxalist.dat[i,col.gf],dict)
  col.es <- match(ecoval.translate("A_macrophytes_taxalist_growthform_assess_twoforms_grfo",dict),colnames(taxalist.dat))
  if ( !is.na(col.es) ) for ( i in 1:nrow(taxalist.dat) ) taxalist.dat[i,col.es] <- ifelse(is.na(taxalist.dat[i,col.es]),NA,ecoval.translate(taxalist.dat[i,col.es],dict))
  col.trait <- match(ecoval.translate("A_macrophytes_taxalist_determination_trait",dict),colnames(taxalist.dat))
  if ( !is.na(col.trait) ) for ( i in 1:nrow(taxalist.dat) ) taxalist.dat[i,col.trait] <- ifelse(is.na(taxalist.dat[i,col.trait]),NA,ecoval.translate(taxalist.dat[i,col.trait],dict))
  col.cons <- match(ecoval.translate("A_macrophytes_taxalist_conservationinfo",dict),colnames(taxalist.dat))
  if ( !is.na(col.cons) ) for ( i in 1:nrow(taxalist.dat) ) taxalist.dat[i,col.cons] <- ifelse(is.na(taxalist.dat[i,col.cons]),NA,ecoval.translate(taxalist.dat[i,col.cons],dict))
  
  # Create data.frame to collect information on quality checks
  species.quality <- matrix(NA, nrow = 0, ncol = 5)
  colnames(species.quality) <- c(ecoval.translate("A_macrophytes_site_siteid",dict),
                                 ecoval.translate("A_macrophytes_site_samplingdate",dict),
                                 ecoval.translate("R_macrophytes_error_parameter",dict),
                                 ecoval.translate("R_macrophytes_error_message",dict),
                                 ecoval.translate("R_macrophytes_error_type",dict))
  
  # check if all required columns are contained in data.species
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
    
  } else {
    
    # ADD UNIQUE IDENTIFIER FOR INDIVIDUAL ASSESSMENTS IN EXTRA COLUMN; AND SPECIES WITHIN ASSESSMENTS AS ROWNAMES
    data.species <- data.frame(paste(data.species[,ecoval.translate("A_macrophytes_site_siteid",dict)],
                                     data.species[,ecoval.translate("A_macrophytes_site_samplingdate",dict)], sep = "_"),
                               data.species, stringsAsFactors = FALSE)
    colnames(data.species)[1] <- ecoval.translate("A_macrophytes_site_sampleid",dict)
    rownames(data.species) <- paste(data.species[,colnames(data.species)[1]],rownames(data.species), sep = "_")
    
    # Perform quality checks
    
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
    
    # Quality checks
    
    # CHECK IF ALL CORE DATA REQUIRED AS UNIQUE IDENTIFIER ARE PRESENT
    
    # SITE CODE
    if ( sum(is.na(data.species[,ecoval.translate("A_macrophytes_site_siteid",dict)])) > 0 ) {
      quality.out <- unique(data.species[is.na(data.species[,ecoval.translate("A_macrophytes_site_siteid",dict)]),
                                         ecoval.translate("A_macrophytes_site_sampleid",dict)])
      
      quality.out <- cbind(t(sapply(quality.out , split.last.underline)))
      quality.out <- cbind(quality.out, rep(ecoval.translate("A_macrophytes_site_siteid",dict), nrow(quality.out)))
      quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_completevalue", dict), nrow(quality.out)))
      quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_error", dict), nrow(quality.out)))
      #quality.out[quality.out == "NA"] <- NA
      colnames(quality.out) <- colnames(species.quality)
      species.quality <- rbind(species.quality, quality.out)
    }
    
    # DATE
    if ( sum( is.na(data.species[,ecoval.translate("A_macrophytes_site_samplingdate",dict)]) ) > 0 ) {
      quality.out <- unique(data.species[is.na(data.species[,ecoval.translate("A_macrophytes_site_samplingdate",dict)]),
                                         ecoval.translate("A_macrophytes_site_sampleid",dict)])
      quality.out <- cbind(t(sapply(quality.out , split.last.underline)))
      
      quality.out <- cbind(quality.out, rep(ecoval.translate("A_macrophytes_site_samplingdate",dict), nrow(quality.out)))
      quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_completevalue", dict), nrow(quality.out)))
      quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_error", dict), nrow(quality.out)))
      #quality.out[quality.out == "NA"] <- NA
      colnames(quality.out) <- colnames(species.quality)
      species.quality <- rbind(species.quality, quality.out)
    }
    
    # SPECIES NUMBER
    if( sum( is.na(data.species[,ecoval.translate("A_macrophytes_species_number_msk",dict)]) ) > 0 ) {
      quality.out <- unique(data.species[is.na(data.species[,ecoval.translate("A_macrophytes_species_number_msk",dict)]),
                                         ecoval.translate("A_macrophytes_site_sampleid",dict)])
      quality.out <- cbind(t(sapply(quality.out , split.last.underline)))
      
      quality.out <- cbind(quality.out, rep(ecoval.translate("A_macrophytes_species_number_msk",dict), nrow(quality.out)))
      quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_completevalue", dict), nrow(quality.out)))
      quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_error", dict), nrow(quality.out)))
      #quality.out[quality.out == "NA"] <- NA
      colnames(quality.out) <- colnames(species.quality)
      species.quality <- rbind(species.quality, quality.out)
    }
    
    # VALIDATE SPECIES DATA
    
    # QUALITY CHECK OF OBLIGATORY PARAMETERS 
    
    # ABSOLUTE COVER OF INDIVIDUAL SPECIES
    
    # Check if cover values are given for each species
    if( sum( is.na(data.species[,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)]) ) > 0 ) {
      quality.out <- unique(data.species[is.na(data.species[,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)]),
                                         ecoval.translate("A_macrophytes_site_sampleid",dict)])
      quality.out <- cbind(t(sapply(quality.out , split.last.underline)))
      
      quality.out <- cbind(quality.out, rep(ecoval.translate("A_macrophytes_species_absolutecover_percent",dict), nrow(quality.out)))
      quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_completevalue", dict), nrow(quality.out)))
      quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_error", dict), nrow(quality.out)))
      #quality.out[quality.out == "NA"] <- NA
      colnames(quality.out) <- colnames(species.quality)
      species.quality <- rbind(species.quality, quality.out)
    }
    
    # Check if 0% cover values are present, which is either not correct or species should be not contained in data
    if( sum( data.species[,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] == 0, na.rm = T ) > 0 ) {
      quality.out <- unique(data.species[data.species[,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] == 0,
                                         ecoval.translate("A_macrophytes_site_sampleid",dict)])
      quality.out <- cbind(t(sapply(quality.out, split.last.underline)), row.names = NULL)
      
      quality.out <- cbind(quality.out, rep(ecoval.translate("A_macrophytes_species_absolutecover_percent",dict), nrow(quality.out)))
      quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_value0percent", dict), nrow(quality.out)))
      quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_error", dict), nrow(quality.out)))
      #quality.out[quality.out == "NA"] <- NA
      colnames(quality.out) <- colnames(species.quality)
      species.quality <- rbind(species.quality, quality.out)
    }
    
    # Check if values < 0 are only -999, i.e.code for presence of bryophytes
    if( sum( data.species[,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] < 0 &
             data.species[,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] != -999, na.rm = T ) > 0 ) {
      quality.out <- unique(data.species[data.species[,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] < 0 &
                                           data.species[,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] != -999,
                                         ecoval.translate("A_macrophytes_site_sampleid",dict)])
      quality.out <- cbind(t(sapply(quality.out , split.last.underline)))
      
      quality.out <- cbind(quality.out, rep(ecoval.translate("A_macrophytes_species_absolutecover_percent",dict), nrow(quality.out)))
      quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_valuelt0percent", dict), nrow(quality.out)))
      quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_error", dict), nrow(quality.out)))
      #quality.out[quality.out == "NA"] <- NA
      colnames(quality.out) <- colnames(species.quality)
      species.quality <- rbind(species.quality, quality.out)
    }
    
    # Check if remaining cover values contain data out of range > 100%
    if( sum( data.species[,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] > 100, na.rm = T ) > 0 ) {
      quality.out <- unique(data.species[data.species[,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] > 100,
                                         ecoval.translate("A_macrophytes_site_sampleid",dict)])
      quality.out <- cbind(t(sapply(quality.out , split.last.underline)))
      
      quality.out <- cbind(quality.out, rep(ecoval.translate("A_macrophytes_species_absolutecover_percent",dict), nrow(quality.out)))
      quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_valuegt100percent", dict), nrow(quality.out)))
      quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_error", dict), nrow(quality.out)))
      quality.out[quality.out == "NA"] <- NA
      colnames(quality.out) <- colnames(species.quality)
      species.quality <- rbind(species.quality, quality.out)
    }
    
    # Check sum of cover values is > 100%. Since some people record bryophyte cover for each species this can occur,
    # but needs checking. The chec
    Erhebung_Code <- unique(data.species[,ecoval.translate("A_macrophytes_site_sampleid",dict)])
    dg.abs.check <- rep("ok",length(Erhebung_Code))

    for ( i in 1:length(dg.abs.check) ) {
      dg.abs.select <- data.species[data.species[,ecoval.translate("A_macrophytes_site_sampleid",dict)] == Erhebung_Code[i],]
      
      if( sum(dg.abs.select[,ecoval.translate("A_macrophytes_species_number_msk",dict)] == 50000001,na.rm=T) > 0 ) {
        if( sum(dg.abs.select[,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] == -999) > 0 ) {
          if( sum(dg.abs.select[dg.abs.select[,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] != -999,
                                ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)]) > 100 ) {
            dg.abs.check[i] <- "error"
          }
        } else {
          if( sum(dg.abs.select[dg.abs.select[,ecoval.translate("A_macrophytes_species_number_msk",dict)] != 50000001,
                                ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)]) > 100 ) {
            dg.abs.check[i] <- "error"
          } 
        }
      } else {
        if( sum(dg.abs.select[dg.abs.select[,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] != -999,
                              ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)]) > 100 ) {
          dg.abs.check[i] <- "error"
        }
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
    
    
    # Check if uncertanity of determination is given for each species, only if sampling protocol 2018 was used
    if ( sampling.protocol == "v2018" ) {
      if( sum( is.na(data.species[,ecoval.translate("A_macrophytes_species_determinationuncertainty",dict)]), na.rm = T) > 0 ) {
        quality.out <- unique(data.species[is.na(data.species[,ecoval.translate("A_macrophytes_species_determinationuncertainty",dict)]),
                                           ecoval.translate("A_macrophytes_site_sampleid",dict)])
        quality.out <- cbind(t(sapply(quality.out , split.last.underline)))
        
        quality.out <- cbind(quality.out, rep(ecoval.translate("A_macrophytes_species_determinationuncertainty",dict), nrow(quality.out)))
        quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_completevalue", dict), nrow(quality.out)))
        quality.out <- cbind(quality.out, rep(ecoval.translate("R_macrophytes_error_error", dict), nrow(quality.out)))
        #quality.out[quality.out == "NA"] <- NA
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
    data.species$WF_short <- NA
    data.species$WF_assess <- NA
    
    for( i in 1:nrow(data.species) ) {
      row.taxalist <- match(data.species[i,ecoval.translate("A_macrophytes_species_number_msk",dict)],
                            taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)])
      if( !is.na(row.taxalist) ) {
        data.species[i, "WF_short"]  <- taxalist.dat[row.taxalist, ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]
        data.species[i, "WF_assess"] <- taxalist.dat[row.taxalist, ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)]
      }
    }
    
    # Check species assignments and determination accuracy, but only for data collected with sampling protocol published 2017/2018
    # For old data species are taken as they are and determination uncertanity is not considered
    
    # Create vector that collects data entries to be deleted
    rows.delete <- c()
    
    if ( sampling.protocol == "v2018" ) {
      
      for ( i in 1:nrow(data.species) ) {
        row.taxalist <- match(data.species[i,ecoval.translate("A_macrophytes_species_number_msk",dict)],
                              taxalist.dat[ ,ecoval.translate("A_macrophytes_species_number_msk",dict)])
        
        detunc.data.species  <- data.species[i,ecoval.translate("A_macrophytes_species_determinationuncertainty",dict)]
        
        if( !is.na(detunc.data.species) ) {
          # Get corresponding determination value and taxa-group assigment from taxalist
          detval.taxalist.dat <- taxalist.dat[row.taxalist, ecoval.translate("A_macrophytes_taxalist_determination",dict)]
          grouptaxa.taxalist.dat <- taxalist.dat[row.taxalist, ecoval.translate("A_macrophytes_taxalist_taxagroups_assign",dict)]
          
          # Do quality check for bryophyte species
          if( data.species[i,"WF_short"] == "Bry" ) {
            if ( detval.taxalist.dat <= 2 ) {
              if( detunc.data.species == 4 ) {
                if( !is.na(grouptaxa.taxalist.dat) ) {
                  data.species[i,ecoval.translate("A_macrophytes_species_number_msk",dict)] <- grouptaxa.taxalist.dat
                  data.species[i,ecoval.translate("A_macrophytes_species_name_latin",dict)] <- taxalist.dat[match(grouptaxa.taxalist.dat,taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk", dict)]),
                                                                                                            ecoval.translate("A_macrophytes_species_name_latin",dict)]
                  data.species[i,ecoval.translate("A_macrophytes_species_determinationuncertainty",dict)] <- NA
                  
                  data.species[i,ecoval.translate("A_macrophytes_species_commentvalidation_taxa",dict)] <- "Ersetzt durch Sammeltaxon"
                } else {
                  rows.delete <- c(rows.delete, rownames(data.species)[i])
                }
              }
            } else {
              if ( detval.taxalist.dat == 3 ) {
                if( !is.na(grouptaxa.taxalist.dat) ) {
                  data.species[i,ecoval.translate("A_macrophytes_species_number_msk",dict)] <- grouptaxa.taxalist.dat
                  data.species[i,ecoval.translate("A_macrophytes_species_name_latin",dict)] <- taxalist.dat[match(grouptaxa.taxalist.dat,taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk", dict)]),
                                                                                                            ecoval.translate("A_macrophytes_species_name_latin",dict)]
                  data.species[i,ecoval.translate("A_macrophytes_species_determinationuncertainty",dict)] <- NA
                  
                  data.species[i,ecoval.translate("A_macrophytes_species_commentvalidation_taxa",dict)] <- "Ersetzt durch Sammeltaxon"
                } else {
                  rows.delete <- c(rows.delete, rownames(data.species)[i])
                }
              }
            }
          } else {
            
            # Quality check for all higher macrophytes species
            if ( detval.taxalist.dat <= 2 ) {
              if( detunc.data.species == 4 ) {
                if( !is.na(grouptaxa.taxalist.dat) ) {
                  data.species[i,ecoval.translate("A_macrophytes_species_number_msk",dict)] <- grouptaxa.taxalist.dat
                  data.species[i,ecoval.translate("A_macrophytes_species_name_latin",dict)] <- taxalist.dat[match(grouptaxa.taxalist.dat,taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk", dict)]),
                                                                                                            ecoval.translate("A_macrophytes_species_name_latin",dict)]
                  data.species[i,ecoval.translate("A_macrophytes_species_determinationuncertainty",dict)] <- NA
                  
                  data.species[i,ecoval.translate("A_macrophytes_species_commentvalidation_taxa",dict)] <- "Ersetzt durch Sammeltaxon"
                  
                  data.species[i, "WF_short"]  <- taxalist.dat[match(grouptaxa.taxalist.dat,taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk", dict)]),
                                                               ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]
                  data.species[i, "WF_assess"] <- taxalist.dat[match(grouptaxa.taxalist.dat,taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk", dict)]),
                                                               ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)]
                  
                } else {
                  rows.delete <- c(rows.delete, rownames(data.species)[i])
                }
              }
            } else {
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
                
                
                # Species is corrected to group-taxa if required phenology trait was not present or if determination was insecure
                if ( !phen.check.ok | detunc.data.species > 3 ) {
                  if ( !is.na(grouptaxa.taxalist.dat) ) {
                    data.species[i,ecoval.translate("A_macrophytes_species_number_msk",dict)] <- grouptaxa.taxalist.dat
                    data.species[i,ecoval.translate("A_macrophytes_species_name_latin",dict)] <- taxalist.dat[match(grouptaxa.taxalist.dat,taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk", dict)]),
                                                                                                              ecoval.translate("A_macrophytes_species_name_latin",dict)]
                    data.species[i,ecoval.translate("A_macrophytes_species_determinationuncertainty",dict)] <- NA
                    
                    data.species[i,ecoval.translate("A_macrophytes_species_commentvalidation_taxa",dict)] <- "Ersetzt durch Sammeltaxon"
                    
                    data.species[i, "WF_short"]  <- taxalist.dat[match(grouptaxa.taxalist.dat,taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk", dict)]),
                                                                 ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]
                    data.species[i, "WF_assess"] <- taxalist.dat[match(grouptaxa.taxalist.dat,taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk", dict)]),
                                                                 ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)]
                  } else {
                    rows.delete <- c(rows.delete, rownames(data.species)[i])
                  }
                }
              } else {
                if ( detval.taxalist.dat == 4 ) {
                  if ( !is.na(grouptaxa.taxalist.dat) ) {
                    data.species[i,ecoval.translate("A_macrophytes_species_number_msk",dict)] <- grouptaxa.taxalist.dat
                    data.species[i,ecoval.translate("A_macrophytes_species_name_latin",dict)] <- taxalist.dat[match(grouptaxa.taxalist.dat,taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk", dict)]),
                                                                                                              ecoval.translate("A_macrophytes_species_name_latin",dict)]
                    data.species[i,ecoval.translate("A_macrophytes_species_determinationuncertainty",dict)] <- NA
                    
                    data.species[i,ecoval.translate("A_macrophytes_species_commentvalidation_taxa",dict)] <- "Ersetzt durch Sammeltaxon"
                    
                    data.species[i, "WF_short"]  <- taxalist.dat[match(grouptaxa.taxalist.dat,taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk", dict)]),
                                                                 ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]
                    data.species[i, "WF_assess"] <- taxalist.dat[match(grouptaxa.taxalist.dat,taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk", dict)]),
                                                                 ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)]
                  } else {
                    rows.delete <- c(rows.delete, rownames(data.species)[i])
                  }
                }
              }
            }
          }
        }
      }
    }
    
    
    # Remove species that did not conform to determination quality requirements
    data.species.removed <- data.species[rownames(data.species) %in% rows.delete,]
    data.species         <- data.species[rownames(data.species) %in% rows.delete == FALSE,]
    
    # Create final version of species data for calculation of attributes
    # - Bryophyte data are standardized so that absolute cover of all moss species in site is contained in the taxon Bryophyta
    #   and presence of individual moss taxa is marked by -999
    # - Filamentous algae data are summarized in the Taxon "faedige Gruenalge", and only this
    #   taxon remains in the data
    data.species.assess <- data.species[0,]
    assessment.unique <- unique(data.species[,ecoval.translate("A_macrophytes_site_sampleid",dict)])
    
    collect.bryophyta.covabs.missing <- c()
    collect.bryophyta.missing <- c()
    
    for ( i in 1:length(assessment.unique) ) {
      data.species.select <- data.species[data.species[,ecoval.translate("A_macrophytes_site_sampleid",dict)] == assessment.unique[i],]
      
      # Standardize bryophyte data
      # The taxon "Bryophyta" (Taxa Number: 50000001) contains absolute bryophyte cover of all species, and presence
      # of individual bryophyte taxa is marked by -999
      data.species.bryo <- data.species.select[data.species.select[,"WF_assess"] ==
                                                 ecoval.translate("L_macrophytes_taxalist_growthform_assess_bryophyte",dict),]
      
      if( nrow(data.species.bryo) > 1 ) {
        
        if( sum( data.species.bryo[,ecoval.translate("A_macrophytes_species_number_msk",dict)] == 50000001, na.rm = T) == 1 ) {
          
          if( !is.na(data.species.bryo[data.species.bryo[,ecoval.translate("A_macrophytes_species_number_msk",dict)] == 50000001,
                                       ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)]) |
              data.species.bryo[data.species.bryo[,ecoval.translate("A_macrophytes_species_number_msk",dict)] == 50000001,
                                ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] != -999 ) {
            
            data.species.bryo[data.species.bryo[,ecoval.translate("A_macrophytes_species_number_msk",dict)] != 50000001,
                              ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] <- -999
            
            
            data.species.select <- rbind(data.species.select[data.species.select[,"WF_assess"] != ecoval.translate("L_macrophytes_taxalist_growthform_assess_bryophyte",dict),],
                                         data.species.bryo, stringsAsFactors = FALSE)
            
          } else {
            collect.bryophyta.covabs.missing <- c(collect.bryophyta.covabs.missing, assessment.unique[i])
          }
          
        } else {
          collect.bryophyta.missing <- c(collect.bryophyta.missing, assessment.unique[i])
        }
      } else {
        # Check if site only contains taxon bryophyta after limiting taxa to taxalist, if yes bryophyta is removed
        if( nrow(data.species.bryo) == 1 ) {
          if ( data.species.bryo[1,ecoval.translate("A_macrophytes_species_number_msk",dict)] == 50000001 ) {
            data.species.select <- data.species.select[data.species.select[,"WF_assess"] != ecoval.translate("L_macrophytes_taxalist_growthform_assess_bryophyte",dict),]
          } else {
            collect.bryophyta.missing <- c(collect.bryophyta.missing, assessment.unique[i])
          }
        }
      }
      
      
      # Combine all taxa representing filamentous green algae in one row
      data.species.algae <- data.species.select[data.species.select[,"WF_assess"] == ecoval.translate("L_macrophytes_taxalist_growthform_assess_algae",dict),]
      
      if( nrow(data.species.algae) > 0 ) {
        sum.algae <- sum(data.species.algae[,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)])
        
        data.species.algae <- data.species.algae[1,]
        data.species.algae[1,] <- NA
        
        data.species.algae[1,ecoval.translate("A_macrophytes_site_sampleid",dict)] <- data.species.select[1, ecoval.translate("A_macrophytes_site_sampleid",dict)] 
        data.species.algae[1,ecoval.translate("A_macrophytes_site_siteid",dict)] <- data.species.select[1,ecoval.translate("A_macrophytes_site_siteid",dict)]
        data.species.algae[1,ecoval.translate("A_macrophytes_site_samplingdate",dict)] <- data.species.select[1,ecoval.translate("A_macrophytes_site_samplingdate",dict)]
        data.species.algae[1,ecoval.translate("A_macrophytes_species_number_msk",dict)] <- 50000004
        data.species.algae[1,ecoval.translate("A_macrophytes_species_name_latin",dict)] <- "faedige Gruenalge"
        data.species.algae[1,ecoval.translate("A_macrophytes_species_absolutecover_percent", dict)] <- sum.algae
        data.species.algae[1,"WF_short"]  <- "FaedAlg"
        data.species.algae[1,"WF_assess"] <- ecoval.translate("L_macrophytes_taxalist_growthform_assess_algae",dict)
        
        data.species.select <- rbind(data.species.select[data.species.select[,"WF_assess"] != ecoval.translate("L_macrophytes_taxalist_growthform_assess_algae",dict),],
                                     data.species.algae, stringsAsFactors = FALSE)
      }
      
      data.species.assess <- rbind(data.species.assess, data.species.select, stringsAsFactors = FALSE)
    }
    
    if( length(collect.bryophyta.covabs.missing) > 0 ) {
      quality.out <- cbind(t(sapply(collect.bryophyta.covabs.missing , split.last.underline)),
                                rep(ecoval.translate("A_macrophytes_species_number_msk",dict),
                                    length(collect.bryophyta.covabs.missing)),
                                rep(ecoval.translate("R_macrophytes_error_abscoveragebrymissing", dict),
                                    length(collect.bryophyta.covabs.missing)),
                                rep(ecoval.translate("R_macrophytes_error_error", dict),
                                    length(collect.bryophyta.covabs.missing)))
      colnames(quality.out) <- colnames(species.quality)
      species.quality <- rbind(species.quality, quality.out)
    }
    
    if( length(collect.bryophyta.missing) > 0 ) {
      quality.out <- cbind(t(sapply(collect.bryophyta.missing , split.last.underline)),
                                rep(ecoval.translate("A_macrophytes_species_number_msk",dict),
                                    length(collect.bryophyta.missing)),
                                rep(ecoval.translate("R_macrophytes_error_taxonbrymissing", dict),
                                    length(collect.bryophyta.missing)),
                                rep(ecoval.translate("R_macrophytes_error_error", dict),
                                    length(collect.bryophyta.missing)))
      colnames(quality.out) <- colnames(species.quality)
      species.quality <- rbind(species.quality, quality.out)
    }
    
    
    # Combine species data in single row if two species in original data where assigned to the same "Sammeltaxon"
    data.species.collect <- data.species.assess[0,]
    assessment.unique    <- unique(data.species.assess[,ecoval.translate("A_macrophytes_site_sampleid",dict)])
    
    for ( i in 1:length(assessment.unique) ) {
      data.species.select <- data.species.assess[data.species.assess[,ecoval.translate("A_macrophytes_site_sampleid",dict)] == assessment.unique[i],]
      spec.id <- unique(data.species.select[,ecoval.translate("A_macrophytes_species_number_msk",dict)])
      
      if( length(spec.id) < nrow(data.species.select) ) {
        # Extract species numbers of species that are present multiple times
        spec.no.collect <- c()
        for( k in 1:length(spec.id) ) {
          if( sum(data.species.select[,ecoval.translate("A_macrophytes_species_number_msk",dict)] == spec.id[k]) > 1 ) {
            spec.no.collect <- c(spec.no.collect,spec.id[k]) 
          }
        }
        
        # Adjust species data and combine with entire data.set
        for( k in 1:length(spec.no.collect) ) {
          data.species.sub <- data.species.select[data.species.select[,ecoval.translate("A_macrophytes_species_number_msk",dict)] == spec.no.collect[k],]
          data.species.sub[1,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] <- sum(data.species.sub[,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)])
          data.species.sub[1,ecoval.translate("A_macrophytes_species_artificialsubstrate_relcover_percent",dict)] <- sum(data.species.sub[,ecoval.translate("A_macrophytes_species_artificialsubstrate_relcover_percent",dict)])
          data.species.sub[1,ecoval.translate("A_macrophytes_species_determinationuncertainty",dict)] <- NA
          
          data.species.select <- data.species.select[data.species.select[,ecoval.translate("A_macrophytes_species_number_msk",dict)] != spec.no.collect[k],]
          data.species.select <- rbind(data.species.select,data.species.sub[1,], stringsAsFactors = FALSE)
        }
      }
      
      data.species.collect <- rbind(data.species.collect,data.species.select, stringsAsFactors = FALSE)
    }
    
    data.species.assess <- data.species.collect
    
    # Replace headers for growthforms with names defined in dictionary
    colnames(data.species.assess)[match("WF_short", colnames(data.species.assess))]  <- ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)
    colnames(data.species.assess)[match("WF_assess", colnames(data.species.assess))]  <- ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)
    
    # Combine all data containing removed species, either due to taxalist or insufficient determination, in one data.frame
    data.species.removed[,ecoval.translate("A_macrophytes_species_reason_elimination",dict)] <- rep(ecoval.translate("A_macrophytes_species_reason_insufficientquality",dict), nrow(data.species.removed))
    species.not.taxalist[,ecoval.translate("A_macrophytes_species_reason_elimination",dict)] <- rep(ecoval.translate("A_macrophytes_species_reason_notontaxalist",dict), nrow(species.not.taxalist))
    
    # Remove additional columns in data.species.removed introduced during data perparation after reductioh to species not on taxalist
    data.species.removed <- data.species.removed[,colnames(data.species.removed) %in% colnames(species.not.taxalist)]
    
    # Create output data.frame with alle removed species
    data.species.removed <- rbind(data.species.removed, species.not.taxalist, stringsAsFactors = FALSE)
    
    # Complement "species.quality" data.frame with data-set information
    species.quality <- cbind(species.quality, rep(ecoval.translate("R_macrophytes_error_speciesdata", dict), nrow(species.quality)))
    colnames(species.quality)[ncol(species.quality)] <- ecoval.translate("R_macrophytes_error_dataset", dict)
    
    # Return list with generared outputs
    return(list(species.assess       = data.species.assess, 
                species.removed      = data.species.removed, 
                species.quality      = species.quality, 
                taxalist             = taxalist.dat))
    
    
  }
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
    
    # Create data-frame with vegetation data sample event
    data.select <- data.species[data.species[,ecoval.translate("A_macrophytes_site_sampleid",dict)] == rownames(attrib.dat)[i],
                                c(ecoval.translate("A_macrophytes_species_name_latin",dict),
                                  ecoval.translate("A_macrophytes_species_number_msk",dict),
                                  ecoval.translate("A_macrophytes_species_absolutecover_percent", dict),
                                  ecoval.translate("A_macrophytes_species_artificialsubstrate_relcover_percent",dict),
                                  ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict),
                                  ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict))]
    
    if( nrow(data.select) > 0 ) {
      
      ## ABSOLUTE COVER / BIOMASS ATTRIBUTES
      
      # Absolute cover of all vegetation in site
      attrib.dat[i, ecoval.translate("A_macrophytes_allvegetation_abscover_percent",dict)] <- sum(data.select[data.select[,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] != -999,
                                                                                                              ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)], na.rm = TRUE)
      # Calculate and add relative cover per species (Colname: "Rel_Deckung") in selected vegetation data
      data.select$Rel_Deckung <- data.select[, ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] * 100/attrib.dat[i, ecoval.translate("A_macrophytes_allvegetation_abscover_percent",dict)] 
      data.select[data.select[, ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] == -999, "Rel_Deckung"] <- -999
      
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
      
      # Generate data.frame for calculation of richness and dominance by removing species potentially occuring both emergent and submerged
      data.select.richness <- data.select
      
      data.select.richness$spec_select   <- rep(NA, nrow(data.select))
      
      for (j in 1:nrow(data.select.richness)) {
        row.taxalist <- match(data.select.richness[j,ecoval.translate("A_macrophytes_species_number_msk",dict)],
                              taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)])
        data.select.richness[j,"spec_select"]    <- taxalist.dat[row.taxalist,
                                                                 ecoval.translate("A_macrophytes_taxalist_growthform_assess_twoforms_grfoval",dict)]
      }
      
      if( sum(!is.na(data.select.richness[,"spec_select"])) > 0 ) {
        spec.assign <- unique(na.omit(data.select.richness[,"spec_select"]))
        
        if( length(spec.assign) > 0 ) {
          
          for ( k in 1:length(spec.assign) ) {
            spec.assign.1 <- match(spec.assign[k], data.select.richness[,ecoval.translate("A_macrophytes_species_number_msk",dict)])
            
            if( !is.na(spec.assign.1) ) {
              dg.abs <- sum(data.select.richness[spec.assign.1,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)],
                            data.select.richness[match(spec.assign[k],data.select.richness[,"spec_select"]),ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)])
              dg.rel <- sum(data.select.richness[spec.assign.1,"Rel_Deckung"],
                            data.select.richness[match(spec.assign[k],data.select.richness[,"spec_select"]),"Rel_Deckung"])
              
              data.select.richness[spec.assign.1,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] <- dg.abs
              data.select.richness[spec.assign.1,"Rel_Deckung"] <- dg.rel
              
              data.select.richness <- data.select.richness[-match(spec.assign[k],data.select.richness[,"spec_select"]),]
            }
          }
        }
      }
      
      # Generate data.frames with only data on higher macrophytes (i.e. no algae)
      dat.makro <- data.select[data.select[,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] != ecoval.translate("L_macrophytes_taxalist_growthform_assess_algae",dict),]
      dat.makro.richness <- data.select.richness[data.select.richness[,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] != ecoval.translate("L_macrophytes_taxalist_growthform_assess_algae",dict),]
      
      # Absolute cover of only higher macrophytes
      attrib.dat[i, ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict)] <- sum(dat.makro[dat.makro[,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] != -999,
                                                                                                             ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)])
      ## DIVERSITY ATTRIBUTES
      
      # SPECIES RICHNESS
      
      # Species richness of all growthforms, without algae (calculated with dat.makro.richness)
      attrib.dat[i, ecoval.translate("A_macrophytes_taxa_all_richness_count",dict)] <- nrow(dat.makro.richness[dat.makro.richness[,ecoval.translate("A_macrophytes_species_number_msk",dict)] != 50000001,]) #ATTRIBUT
      
      # Species richness per growthform group (i.e. aquatic, helophyte, bryophyte)
      attrib.dat[i, ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict)]     <- sum(dat.makro.richness[dat.makro.richness[,ecoval.translate("A_macrophytes_species_number_msk",dict)] != 50000001, 
                                                                                                                      ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] == ecoval.translate("L_macrophytes_taxalist_growthform_assess_aquatic",dict),
                                                                                                   na.rm = T) #ATTRIBUT
      attrib.dat[i, ecoval.translate("A_macrophytes_taxa_helophytes_richness_count",dict)]  <- sum(dat.makro.richness[dat.makro.richness[,ecoval.translate("A_macrophytes_species_number_msk",dict)] != 50000001,
                                                                                                                      ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] == ecoval.translate("L_macrophytes_taxalist_growthform_assess_helophyte",dict),
                                                                                                   na.rm = T) #ATTRIBUT
      attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_richness_count",dict)]  <- sum(dat.makro.richness[dat.makro.richness[,ecoval.translate("A_macrophytes_species_number_msk",dict)] != 50000001,
                                                                                                                      ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] == ecoval.translate("L_macrophytes_taxalist_growthform_assess_bryophyte",dict),
                                                                                                   na.rm = T) #ATTRIBUT
      attrib.dat[i, ecoval.translate("A_macrophytes_taxa_neophytes_richness_count",dict)]  <- sum(dat.makro.richness[, "neophyte_info"] == "N", na.rm = T) #ATTRIBUT
      
      
      # GROWTHFORM RICHNESS (Calculated with dat.makro)
      attrib.dat[i, ecoval.translate("A_macrophytes_growthform_all_richness_count",dict)]         <- length(na.omit(unique(dat.makro[dat.makro[,ecoval.translate("A_macrophytes_species_number_msk",dict)] != 50000001,
                                                                                                                                     ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]))) #ATTRIBUT
      
      attrib.dat[i, ecoval.translate("A_macrophytes_growthform_helophytes_richness_count",dict)]  <- length(na.omit(unique(dat.makro[dat.makro[,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] == ecoval.translate("L_macrophytes_taxalist_growthform_assess_helophyte",dict),
                                                                                                                                     ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]))) #ATTRIBUT
      
      attrib.dat[i, ecoval.translate("A_macrophytes_growthform_aquatic_richness_count",dict)]     <- length(na.omit(unique(dat.makro[dat.makro[,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] == ecoval.translate("L_macrophytes_taxalist_growthform_assess_aquatic",dict),
                                                                                                                                     ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)]))) #ATTRIBUT
      ## COMMUNITY COMPOSITION
      
      # HEIPs DOMINANCE INDEX (calculated with data.select.richness)
      dat.rel.sw <- data.select.richness[data.select.richness[,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] != -999, "Rel_Deckung"]
      n.taxa.sw  <- length(na.omit(dat.rel.sw))
      sw.index.bry <- -(sum((dat.rel.sw/100) * log((dat.rel.sw/100))))
      if ( n.taxa.sw == 1 ) sw.index.bry <- 0
      if ( n.taxa.sw == 0 ) sw.index.bry <- NA
      
      attrib.dat[i, ecoval.translate("A_macrophytes_heipindex_bryophyteonespecies",dict)] <- (exp(sw.index.bry) - 1) / (n.taxa.sw - 1) #ATTRIBUT
      if ( n.taxa.sw == 1 ) attrib.dat[i, ecoval.translate("A_macrophytes_heipindex_bryophyteonespecies",dict)] <- 0 # Minimaler Wert fuer Heip Index ist 0.006 (cf. Maggurran A., Measuring biological diversity )
      if ( n.taxa.sw == 0 ) attrib.dat[i, ecoval.translate("A_macrophytes_heipindex_bryophyteonespecies",dict)] <- NA
      
      rm(list = c("dat.rel.sw", "n.taxa.sw", "sw.index.bry"))
      
      # NEOPHYTES RELATIVE COVER
      attrib.dat[i, ecoval.translate("A_macrophytes_neophytes_relcover_percent",dict)] <- sum(dat.makro[dat.makro[, "neophyte_info"] == "N", "Rel_Deckung"], na.rm = T) #ATTRIBUT
      
      # RELATIVE COVER OF GROWTHFORMS (i.e. AQUATIC, HELOPHYTES, and BRYOPHYTES)
      attrib.dat[i, ecoval.translate("A_macrophytes_taxa_aquatic_relcover_percent",dict)] <- sum(data.select[data.select[,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] == ecoval.translate("L_macrophytes_taxalist_growthform_assess_aquatic",dict),
                                                                                                             "Rel_Deckung"])  #ATTRIBUT
      if ( I(attrib.dat[i, ecoval.translate("A_macrophytes_taxa_aquatic_relcover_percent",dict)] - 100) > 0 & I(attrib.dat[i, ecoval.translate("A_macrophytes_taxa_aquatic_relcover_percent",dict)] - 100) < 0.01 ) {
        attrib.dat[i, ecoval.translate("A_macrophytes_taxa_aquatic_relcover_percent",dict)] <- 100
      }
      
      attrib.dat[i, ecoval.translate("A_macrophytes_taxa_helophytes_relcover_percent",dict)] <- sum(data.select[data.select[,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] == ecoval.translate("L_macrophytes_taxalist_growthform_assess_helophyte",dict),
                                                                                                                "Rel_Deckung"])  #ATTRIBUT
      if ( I(attrib.dat[i, ecoval.translate("A_macrophytes_taxa_helophytes_relcover_percent",dict)] - 100) > 0 & I(attrib.dat[i, ecoval.translate("A_macrophytes_taxa_helophytes_relcover_percent",dict)] - 100) < 0.01 ) {
        attrib.dat[i, ecoval.translate("A_macrophytes_taxa_helophytes_relcover_percent",dict)] <- 100
      }
      
      attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_relcover_percent",dict)] <- sum(data.select[data.select[,ecoval.translate("A_macrophytes_species_number_msk",dict)] == 50000001,
                                                                                                                "Rel_Deckung"])
      if ( I(attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_relcover_percent",dict)] - 100) > 0 & I(attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_relcover_percent",dict)] - 100) < 0.01 ) {
        attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_relcover_percent",dict)] <- 100
      }
      
      attrib.dat[i, ecoval.translate("A_macrophytes_filamentousgreenalgae_relcover_percent",dict)] <- sum(data.select[data.select[,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] == ecoval.translate("L_macrophytes_taxalist_growthform_assess_algae",dict),
                                                                                                                      "Rel_Deckung"]) #ATTRIBUT
      if ( I(attrib.dat[i, ecoval.translate("A_macrophytes_filamentousgreenalgae_relcover_percent",dict)] - 100) > 0 & I(attrib.dat[i, ecoval.translate("A_macrophytes_filamentousgreenalgae_relcover_percent",dict)] - 100) < 0.01 ) {
        attrib.dat[i, ecoval.translate("A_macrophytes_filamentousgreenalgae_relcover_percent",dict)] <- 100
      }
      
      # ADJUST RELATIVE COVER BRYOPHYTES TO PROPORTION ON ARTIFICIAL SUBSTRATE FOR ASSESSMENT
      if( attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_relcover_percent",dict)] > 0 ) {
        attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_artificialsubstrate_relcover_percent",dict)] <- data.select[data.select[, ecoval.translate("A_macrophytes_species_number_msk",dict)] == 50000001,
                                                                                                                                  ecoval.translate("A_macrophytes_species_artificialsubstrate_relcover_percent", dict)]  # Attribute for illustrative purposes in value function
        
        if( is.na(attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_artificialsubstrate_relcover_percent",dict)]) | attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_artificialsubstrate_relcover_percent",dict)] == -888 ) {
          attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_artificialsubstrate_relcover_percent",dict)] <- NA
          attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_adjusted_relcover_percent",dict)] <- attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_relcover_percent",dict)] #ATTRIBUT
        } else {
          attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_adjusted_relcover_percent",dict)] <- (attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_relcover_percent",dict)] * (1 - (attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_artificialsubstrate_relcover_percent",dict)]/100))) + (0.33 * (attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_relcover_percent",dict)] * (attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_artificialsubstrate_relcover_percent",dict)]/100))) #ATTRIBUT
        }
      } else {
        attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_adjusted_relcover_percent",dict)] <- 0
      }
      
      # ABSOLUTE COVER BY GROWTHFORMS, i.e. AQUATIC, HELOPHYTES, and BRYOPHYTES
      attrib.dat[i, ecoval.translate("A_macrophytes_taxa_aquatic_abscover_percent",dict)] <- sum(data.select[data.select[, ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] == ecoval.translate("L_macrophytes_taxalist_growthform_assess_aquatic",dict),
                                                                                                             ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)], na.rm = T)
      if ( I(attrib.dat[i, ecoval.translate("A_macrophytes_taxa_aquatic_abscover_percent",dict)] - 100) > 0 & I(attrib.dat[i, ecoval.translate("A_macrophytes_taxa_aquatic_abscover_percent",dict)] - 100) < 0.01 ) {
        attrib.dat[i, ecoval.translate("A_macrophytes_taxa_aquatic_abscover_percent",dict)] <- 100
      }
      
      attrib.dat[i, ecoval.translate("A_macrophytes_taxa_helophytes_abscover_percent",dict)] <- sum(data.select[data.select[, ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] == ecoval.translate("L_macrophytes_taxalist_growthform_assess_helophyte",dict),
                                                                                                                ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)], na.rm = T)
      if ( I(attrib.dat[i, ecoval.translate("A_macrophytes_taxa_helophytes_abscover_percent",dict)] - 100) > 0 & I(attrib.dat[i, ecoval.translate("A_macrophytes_taxa_helophytes_abscover_percent",dict)] - 100) < 0.01 ) {
        attrib.dat[i, ecoval.translate("A_macrophytes_taxa_helophytes_abscover_percent",dict)] <- 100
      }
      
      attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_abscover_percent",dict)] <- sum(data.select[data.select[, ecoval.translate("A_macrophytes_species_number_msk",dict)] == 50000001,
                                                                                                                ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)], na.rm = T)
      if ( I(attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_abscover_percent",dict)] - 100) > 0 & I(attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_abscover_percent",dict)] - 100) < 0.01 ) {
        attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes_abscover_percent",dict)] <- 100
      }
      
      attrib.dat[i, ecoval.translate("A_macrophytes_filamentousgreenalgae_abscover_percent",dict)] <- sum(data.select[data.select[, ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] == ecoval.translate("L_macrophytes_taxalist_growthform_assess_algae",dict),
                                                                                                                      ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)]) #ATTRIBUT
      if ( I(attrib.dat[i, ecoval.translate("A_macrophytes_filamentousgreenalgae_abscover_percent",dict)] - 100) > 0 & I(attrib.dat[i, ecoval.translate("A_macrophytes_filamentousgreenalgae_abscover_percent",dict)] - 100) < 0.01 ) {
        attrib.dat[i, ecoval.translate("A_macrophytes_filamentousgreenalgae_abscover_percent",dict)] <- 100
      }
      
      ## SPECIES QUALITY / CONSERVAT
      
      # PRIORITY SPECIES
      
      # NUMBER OF SPECIES FOR EACH PRIORITY CATEGORY
      attrib.dat[i, ecoval.translate("A_macrophytes_priorityspecies1_count",dict)] <- sum(dat.makro$priority == 1, na.rm = T) #ATTRIBUT
      if( abs(attrib.dat[i, ecoval.translate("A_macrophytes_priorityspecies1_count",dict)]) == Inf ) {
        attrib.dat[i, ecoval.translate("A_macrophytes_priorityspecies1_count",dict)] <- 0
      }
      
      attrib.dat[i, ecoval.translate("A_macrophytes_priorityspecies2_count",dict)] <- sum(dat.makro$priority == 2, na.rm = T) #ATTRIBUT
      if( abs(attrib.dat[i, ecoval.translate("A_macrophytes_priorityspecies2_count",dict)]) == Inf ) {
        attrib.dat[i, ecoval.translate("A_macrophytes_priorityspecies2_count",dict)] <- 0
      }
      
      attrib.dat[i, ecoval.translate("A_macrophytes_priorityspecies3_count",dict)] <- sum(dat.makro$priority == 3, na.rm = T) #ATTRIBUT
      if( abs(attrib.dat[i, ecoval.translate("A_macrophytes_priorityspecies3_count",dict)]) == Inf ) {
        attrib.dat[i, ecoval.translate("A_macrophytes_priorityspecies3_count",dict)] <- 0
      }
      
      attrib.dat[i, ecoval.translate("A_macrophytes_priorityspecies4_count",dict)] <- sum(dat.makro$priority == 4, na.rm = T) #ATTRIBUT
      if( abs(attrib.dat[i, ecoval.translate("A_macrophytes_priorityspecies4_count",dict)]) == Inf ) {
        attrib.dat[i, ecoval.translate("A_macrophytes_priorityspecies4_count",dict)] <- 0
      }
      
      # GUIDE VALUES, NUMBER OF SPECIES ("Leitarten")
      
      # HELOPHYTES AND AQUATIC MACROPHYTES
      dat.makro.select <- dat.makro[dat.makro[,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] != ecoval.translate("L_macrophytes_taxalist_growthform_assess_bryophyte",dict),]
      
      # Anzahl Arten mit spezifischem Leitwert
      attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesmac1_count",dict)]  <- sum(dat.makro.select$guide_value == 1, na.rm = T)
      if( abs(attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesmac1_count",dict)]) == Inf ) {
        attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesmac1_count",dict)] <- 0
      }
      
      attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesmac2_count",dict)]  <- sum(dat.makro.select$guide_value == 2, na.rm = T)
      if( abs(attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesmac2_count",dict)]) == Inf ) {
        attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesmac2_count",dict)] <- 0
      }
      
      attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesmac3_count",dict)]  <- sum(dat.makro.select$guide_value == 3, na.rm = T)
      if( abs(attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesmac3_count",dict)]) == Inf ) {
        attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesmac3_count",dict)] <- 0
      }
      
      rm(list = c("dat.makro.select"))
      
      
      # BRYOPHYTES
      dat.makro.select <- dat.makro[dat.makro[,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] == ecoval.translate("L_macrophytes_taxalist_growthform_assess_bryophyte",dict),]
      
      attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesbry1_count",dict)]  <- sum(dat.makro.select$guide_value == 1, na.rm = T)
      if( abs(attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesbry1_count",dict)]) == Inf ) {
        attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesbry1_count",dict)] <- 0
      }
      
      attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesbry2_count",dict)]  <- sum(dat.makro.select$guide_value == 2, na.rm = T)
      if( abs(attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesbry2_count",dict)]) == Inf ) {
        attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesbry2_count",dict)] <- 0
      }
      
      attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesbry3_count",dict)]  <- sum(dat.makro.select$guide_value == 3, na.rm = T)
      if( abs(attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesbry3_count",dict)]) == Inf ) {
        attrib.dat[i, ecoval.translate("A_macrophytes_indexspeciesbry3_count",dict)] <- 0
      }
      
      rm( list = c("dat.makro.select") )
      
      
      ## Collect taxon names grouped by growthform ZH
      attrib.dat[i, ecoval.translate("A_macrophytes_taxa_aquatic",dict)]    <- paste(dat.makro[dat.makro[, ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] == ecoval.translate("L_macrophytes_taxalist_growthform_assess_aquatic",dict),
                                                                                               ecoval.translate("A_macrophytes_species_name_latin",dict)], collapse = "; ")
      attrib.dat[i, ecoval.translate("A_macrophytes_taxa_helophytes",dict)] <- paste(dat.makro[dat.makro[, ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] == ecoval.translate("L_macrophytes_taxalist_growthform_assess_helophyte",dict),
                                                                                               ecoval.translate("A_macrophytes_species_name_latin",dict)], collapse = "; ")
      attrib.dat[i, ecoval.translate("A_macrophytes_taxa_bryophytes",dict)] <- paste(dat.makro[dat.makro[, ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] == ecoval.translate("L_macrophytes_taxalist_growthform_assess_bryophyte",dict) & dat.makro[, ecoval.translate("A_macrophytes_species_number_msk",dict)] != 50000001,
                                                                                               ecoval.translate("A_macrophytes_species_name_latin",dict)], collapse = "; ")
      
      rm(list = c("data.select", "dat.makro"))
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

