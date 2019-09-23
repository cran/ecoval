# =======================================
# FUNCTION TO EVALUATE PLAUSIBILITY CRITERIA
# =======================================

msk.macrophytes.2017.plaus.crit <- function(res,
                                            language     = "English",
                                            dictionaries = NA)
{
  # initialization:
  # ===============
  
  dict <- ecoval.dict(language,dictionaries)
  
  n <- length(res$types.val.obs)
  
  rows.available <- !apply(cbind(res$types.probs,res$types.val.obs),1,anyNA)
  
  subm      <- c(ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict),
                 ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict),
                 ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict),
                 ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict))
  helo      <- c(ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict),
                 ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict))
  moss      <- c(ecoval.translate("L_macrophytes_rivertype_class_smallbryophyte",dict),
                 ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict),
                 ecoval.translate("L_macrophytes_rivertype_class_largebryophyte",dict),
                 ecoval.translate("L_macrophytes_rivertype_class_verylargebryophyte",dict))
  helo.moss <- c(ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict),
                 ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict))

  # set up plausibilization criteria:
  # =================================
  
  char.grfo.missing               <- rep(NA,n)
  type.scheme.not.max.prob        <- rep(NA,n)
  type.max.prob.not.best          <- rep(NA,n)
  abs.cover.low                   <- rep(NA,n)
  prob.poorveg.high               <- rep(NA,n)
  low.cover.and.prob.poorveg.high <- rep(NA,n)
  high.helo.aquat.moss.river      <- rep(NA,n)
  moos.mainly.on.art.subst        <- rep(NA,n)
  subs.poor.for.moss              <- rep(NA,n)
  high.cover.moss.poorveg         <- rep(NA,n)
  better.val.other.type           <- rep(NA,n)
  
  # R1: characteristic growth form missing:
  # ---------------------------------------
  
  char.grfo.missing[res$types.val.obs %in% subm              & res$attrib.species[,ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict)]    == 0] <- 1
  char.grfo.missing[res$types.val.obs %in% c(helo,helo.moss) & res$attrib.species[,ecoval.translate("A_macrophytes_taxa_helophytes_richness_count",dict)] == 0] <- 1
  char.grfo.missing[res$types.val.obs %in% c(moss,helo.moss) & res$attrib.species[,ecoval.translate("A_macrophytes_taxa_bryophytes_richness_count",dict)] == 0] <- 1
  
  # R2: type not highest probability:
  # ---------------------------------
  
  type.scheme.not.max.prob[rows.available & res$types.scheme.maxprob != res$types.scheme.obs] <- 1
  
  # R3: type with highest probability not best:
  # -------------------------------------------
  
  types <- colnames(res$val.types)
  for ( i in 1:n )
  {
    if ( rows.available[i] )
    {
      if ( res$types.val.obs[i] != ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict) &
           res$types.val.obs[i] != ecoval.translate("L_macrophytes_rivertype_class_large",dict) )
      {
        type.prob.max    <- types[which.max(res$types.val.probs[i,types])]
        if ( !is.na(res$val.types[i,type.prob.max]) )
        {
          types.prob.ge.10 <- types[which(res$types.val.probs[i,types] >= 0.1)]
          if ( length(types.prob.ge.10) > 0 )
          {
            if ( max(res$val.types[i,types.prob.ge.10]) > res$val.types[i,type.prob.max] ) type.max.prob.not.best[i] <- 1
          }
        }
      }
    }
  }
  
  # R4: absolute coverage low:
  # --------------------------
  
  abs.cover.low[res$types.val.obs %in% c(subm,helo,helo.moss) &
                res$data.site[,ecoval.translate("A_macrophytes_site_shading_percent",dict)] <= 50  &
                res$attrib.species[,ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict)] < 10] <- 1
  
  # R5: prob poorveg high:
  # ----------------------
  
  prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict) &
                    res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.1] <- 1
  prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict) &
                    res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.1] <- 1
  prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict) &
                    res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.2] <- 1
  prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict) &
                    res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.3] <- 1
  
  prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_smallsubmergedhelophyte",dict) &
                    res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.1] <- 1
  prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_mediumsubmergedhelophyte",dict) &
                    res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.2] <- 1

  prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict) &
                    res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.1] <- 1
  prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict) &
                    res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.2] <- 1
  
  prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict) &
                    res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.1] <- 1
  prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict) &
                    res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.2] <- 1
  
  prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_smallbryophyte",dict) &
                    res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.1] <- 1
  prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict) &
                    res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.2] <- 1
  prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_largebryophyte",dict) &
                    res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.2] <- 1
  prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_verylargebryophyte",dict) &
                    res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.3] <- 1
  
  # R6: absolute cover low and prob poorveg high:
  # ---------------------------------------------
  
  low.cover.and.prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict) &
                                  res$attrib.species[,ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict)] < 10 &
                                  res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.1] <- 1
  low.cover.and.prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict) &
                                  res$attrib.species[,ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict)] < 10 &
                                  res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.1] <- 1
  low.cover.and.prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict) &
                                  res$attrib.species[,ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict)] < 10 &
                                  res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.2] <- 1
  low.cover.and.prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict) &
                                  res$attrib.species[,ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict)] < 10 &
                                  res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.3] <- 1
  
  low.cover.and.prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_smallsubmergedhelophyte",dict) &
                                  res$attrib.species[,ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict)] < 10 &
                                  res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.1] <- 1
  low.cover.and.prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_mediumsubmergedhelophyte",dict) &
                                    res$attrib.species[,ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict)] < 10 &
                                    res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.2] <- 1

  low.cover.and.prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict) &
                                  res$attrib.species[,ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict)] < 10 &
                                  res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.1] <- 1
  low.cover.and.prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict) &
                                  res$attrib.species[,ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict)] < 10 &
                                  res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.2] <- 1
  
  low.cover.and.prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict) &
                                  res$attrib.species[,ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict)] < 10 &
                                  res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.1] <- 1
  low.cover.and.prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict) &
                                  res$attrib.species[,ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict)] < 10 &
                                  res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.2] <- 1

  # low.cover.and.prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_smallbryophyte",dict) &
  #                                 res$attrib.species[,ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict)] < 10 &
  #                                 res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.1] <- 1
  # low.cover.and.prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict) &
  #                                 res$attrib.species[,ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict)] < 10 &
  #                                 res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.2] <- 1
  # low.cover.and.prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_largebryophyte",dict) &
  #                                 res$attrib.species[,ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict)] < 10 &
  #                                 res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.2] <- 1
  # low.cover.and.prob.poorveg.high[res$types.scheme.obs==ecoval.translate("L_macrophytes_rivertype_class_verylargebryophyte",dict) &
  #                                 res$attrib.species[,ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict)] < 10 &
  #                                 res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.3] <- 1
  
  # R7: absolute cover of helophytes and aquatic forms high for moss or poorveg river:
  # ----------------------------------------------------------------------------------
  
  high.helo.aquat.moss.river[res$types.val.obs %in% moss &
                             res$attrib.species[,ecoval.translate("A_macrophytes_taxa_aquatic_abscover_percent",dict)] +
                             res$attrib.species[,ecoval.translate("A_macrophytes_taxa_helophytes_abscover_percent",dict)] > 10] <- 1
  
  high.helo.aquat.moss.river[res$types.val.obs == ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict) &
                               res$attrib.species[,ecoval.translate("A_macrophytes_taxa_aquatic_abscover_percent",dict)] +
                               res$attrib.species[,ecoval.translate("A_macrophytes_taxa_helophytes_abscover_percent",dict)] > 10] <- 1
  
  # R8: moss mainly on artificial substrate:
  # ----------------------------------------
                             
  moos.mainly.on.art.subst[res$types.val.obs %in% c(helo.moss,moss) &
                           res$attrib.species[,ecoval.translate("A_macrophytes_taxa_bryophytes_artificialsubstrate_relcover_percent",dict)] >= 80] <- 1
  
  # R9: substrate poor for moss:
  # ----------------------------
  
  if ( !is.na(match(ecoval.translate("A_macrophytes_site_substratestability_class",dict),colnames(res$data.site))) )
  {
    subs.poor.for.moss[res$types.val.obs == ecoval.translate("L_macrophytes_rivertype_class_smallbryophyte",dict) &
                       res$attrib.species[,ecoval.translate("A_macrophytes_taxa_bryophytes_richness_count",dict)] < 3 &
                       res$attrib.species[,ecoval.translate("A_macrophytes_taxa_bryophytes_abscover_percent",dict)] < 5 &
                       !is.na(res$data.site[,ecoval.translate("A_macrophytes_site_substratestability_class",dict)]) &
                       res$data.site[,ecoval.translate("A_macrophytes_site_substratestability_class",dict)] > 1] <- 1
    
    subs.poor.for.moss[res$types.val.obs %in% c(ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict),
                                                ecoval.translate("L_macrophytes_rivertype_class_largebryophyte",dict),
                                                ecoval.translate("L_macrophytes_rivertype_class_verylargebryophyte",dict)) &
                       res$attrib.species[,ecoval.translate("A_macrophytes_taxa_bryophytes_richness_count",dict)] < 4 &
                       res$attrib.species[,ecoval.translate("A_macrophytes_taxa_bryophytes_abscover_percent",dict)] < 5 &
                       !is.na(res$data.site[,ecoval.translate("A_macrophytes_site_substratestability_class",dict)]) &
                       res$data.site[,ecoval.translate("A_macrophytes_site_substratestability_class",dict)] > 1] <- 1
  }
  
  # R10: high cover moss in poor vegetation river:
  # ----------------------------------------------
                     
  high.cover.moss.poorveg[res$types.val.obs == ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict) &
                          res$attrib.species[,ecoval.translate("A_macrophytes_taxa_bryophytes_abscover_percent",dict)] > 10] <- 1 
  
  # R11: better valuation in other type of same discharge class:
  # ------------------------------------------------------------
  
  better.val.other.type[res$types.val.obs %in% c(ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict),
                                                 ecoval.translate("L_macrophytes_rivertype_class_smallbryophyte",dict)) &
                        apply(res$val.types[,c(ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict),
                                               ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)),drop=FALSE],1,max,na.rm=TRUE) > res$val[,1]] <- 1
  
  better.val.other.type[res$types.val.obs %in% c(ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict),
                                                 ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict)) &
                        apply(res$val.types[,c(ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict),
                                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict)),drop=FALSE],1,max,na.rm=TRUE) > res$val[,1]] <- 1
  
  better.val.other.type[res$types.val.obs == ecoval.translate("L_macrophytes_rivertype_class_largebryophyte",dict) &
                        res$val.types[,ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict)] > res$val[,1]] <- 1
  
  better.val.other.type[res$types.val.obs == ecoval.translate("L_macrophytes_rivertype_class_verylargebryophyte",dict) &
                        res$val.types[,ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict)] > res$val[,1]] <- 1
  
  better.val.other.type[res$types.val.obs == ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict) &
                        apply(res$val.types,1,max,na.rm=T) >= 0.6] <- 1
  
  # R: collect plausibilization criteria:
  # -------------------------------------
  
  plaus.warning <- cbind(char.grfo.missing,
                         type.scheme.not.max.prob,
                         type.max.prob.not.best,
                         abs.cover.low,
                         prob.poorveg.high,
                         low.cover.and.prob.poorveg.high,
                         high.helo.aquat.moss.river,
                         moos.mainly.on.art.subst,
                         subs.poor.for.moss,
                         high.cover.moss.poorveg,
                         better.val.other.type)
  colnames(plaus.warning) <- c(ecoval.translate("R_macrophytes_plaus_warning_chargrfomissing",dict),
                               ecoval.translate("R_macrophytes_plaus_warning_typevalnotmaxprob",dict),
                               ecoval.translate("R_macrophytes_plaus_warning_typemaxprobnotbest",dict),
                               ecoval.translate("R_macrophytes_plaus_warning_abscoverlow",dict),
                               ecoval.translate("R_macrophytes_plaus_warning_probpoorveghigh",dict),
                               ecoval.translate("R_macrophytes_plaus_warning_abscoverlowandprobpoorveghigh",dict),
                               ecoval.translate("R_macrophytes_plaus_warning_highheloaquatmossriver",dict),
                               ecoval.translate("R_macrophytes_plaus_warning_mossmainlyonartsubst",dict),
                               ecoval.translate("R_macrophytes_plaus_warning_subspoorformoss",dict),
                               ecoval.translate("R_macrophytes_plaus_warning_highcovermosspoorveg",dict),
                               ecoval.translate("R_macrophytes_plaus_warning_bettervaltypesamedischarge",dict))
  plaus.warning <- cbind(plaus.warning,apply(plaus.warning,1,sum,na.rm=TRUE))
  colnames(plaus.warning)[ncol(plaus.warning)] <- ecoval.translate("R_macrophytes_plaus_warning_sum",dict)

  # set up type change suggestions:
  # ===============================
  
  small.submerged.without.aquatic.grfo <- rep("",n)
  change.macrophyte.high.cover         <- rep("",n)
  change.macrophyte.medium.cover       <- rep("",n)
  change.moss.high.cover               <- rep("",n)
  other.base.type                      <- rep("",n)
  other.discharge.class                <- rep("",n)
  change.poorveg                       <- rep("",n)
  change.better.valuation              <- rep("",n)
  
  # U1: change KS to KH due to missing aquatic forms:
  # -------------------------------------------------
  
  ind <- res$types.val.obs == ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict) &
         res$attrib.species[,ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict)] == 0
  small.submerged.without.aquatic.grfo[ind] <- ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)

  # U2: change to macrophyte river due to high coverage:
  # ----------------------------------------------------
  
  ind <- which(res$types.val.obs == ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict) &
                 res$attrib.species[,ecoval.translate("A_macrophytes_taxa_aquatic_abscover_percent",dict)] +
                 res$attrib.species[,ecoval.translate("A_macrophytes_taxa_helophytes_abscover_percent",dict)] > 20)
  if ( length(ind) > 0 )
  {
    for ( i in ind )
    {
      n.subm <- res$attrib.species[i,ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict)]
      n.helo <- res$attrib.species[i,ecoval.translate("A_macrophytes_taxa_helophytes_richness_count",dict)]
      if ( n.subm > 0 & n.helo > 0 )
      {
        probs <- res$types.val.probs[i,c(subm,helo)]
      }
      else
      {
        if ( n.subm > 0 ) probs <- res$types.val.probs[i,subm]
        else              probs <- res$types.val.probs[i,helo]
      }
      change.macrophyte.high.cover[i] <- paste(names(probs)[max(probs)==probs],collapse=",")
    }
  }
  
  # U3: change to macrophyte river due to medium coverage:
  # ------------------------------------------------------
  
  ind <- which(res$types.val.obs == ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict) &
                 res$attrib.species[,ecoval.translate("A_macrophytes_taxa_aquatic_abscover_percent",dict)] +
                 res$attrib.species[,ecoval.translate("A_macrophytes_taxa_helophytes_abscover_percent",dict)] > 10 &
                 res$attrib.species[,ecoval.translate("A_macrophytes_taxa_aquatic_abscover_percent",dict)] +
                 res$attrib.species[,ecoval.translate("A_macrophytes_taxa_helophytes_abscover_percent",dict)] <= 20 )
  if ( length(ind) > 0 )
  {
    for ( i in ind )
    {
      n.subm <- res$attrib.species[i,ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict)]
      n.helo <- res$attrib.species[i,ecoval.translate("A_macrophytes_taxa_helophytes_richness_count",dict)]
      if ( n.subm > 0 & n.helo > 0 )
      {
        probs <- res$types.val.probs[i,c(subm,helo)]
      }
      else
      {
        if ( n.subm > 0 ) probs <- res$types.val.probs[i,subm]
        else              probs <- res$types.val.probs[i,helo]
      }
      if ( max(probs) > 0.1 )
      {
        change.macrophyte.medium.cover[i] <- paste(names(probs)[max(probs)==probs],collapse=",")
      }
    }
  }
  
  # U4: change to moss river due to high coverage:
  # ----------------------------------------------
  
  ind <- which(res$types.val.obs == ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict) &
                 res$attrib.species[,ecoval.translate("A_macrophytes_taxa_bryophytes_abscover_percent",dict)] > 10 &
                 res$attrib.species[,ecoval.translate("A_macrophytes_taxa_bryophytes_artificialsubstrate_relcover_percent",dict)] < 80)
  if ( length(ind) > 0 )
  {
    for ( i in ind )
    {
      n.helo <- res$attrib.species[i,ecoval.translate("A_macrophytes_taxa_helophytes_richness_count",dict)]
      if ( n.helo > 0 )
      {
        probs <- res$types.val.probs[i,c(moss,helo.moss)]
      }
      else
      {
        probs <- res$types.val.probs[i,moss]
      }
      change.moss.high.cover[i] <- paste(names(probs)[max(probs)==probs],collapse=",")
    }
  }
  
  # V1: change to other base type for transition type:
  # --------------------------------------------------
  
  ind.trans <- which(res$types.scheme.obs == ecoval.translate("L_macrophytes_rivertype_class_smallsubmergedhelophyte",dict))
  if ( length(ind.trans) > 0 )
  {
    for ( i in ind.trans )
    {
      if ( abs(res$types.scheme.probs[i,ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict)]-
               res$types.scheme.probs[i,ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)]) < 0.2 )
      {
        if ( res$types.val.obs[i] == ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict) &
             !is.na(res$val.types[i,ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)]) &
             !is.na(res$val.types[i,ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict)]) &
             res$val.types[i,ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)] >
             res$val.types[i,ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict)] &
             !is.na(res$attrib.species[i,ecoval.translate("A_macrophytes_taxa_helophytes_richness_count",dict)]) &
             res$attrib.species[i,ecoval.translate("A_macrophytes_taxa_helophytes_richness_count",dict)] > 0 )
        {
          other.base.type[i] <- ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)
        }
        else
        {
          if ( res$types.val.obs[i] == ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict) &
               !is.na(res$val.types[i,ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict)]) &
               !is.na(res$val.types[i,ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)]) &
               res$val.types[i,ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict)] >
                 res$val.types[i,ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)] &
               !is.na(res$attrib.species[i,ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict)]) &
               res$attrib.species[i,ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict)] > 0 )
          {
            other.base.type[i] <- ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict)
          }
        }
      }
    }
  }

  ind.trans <- which(res$types.scheme.obs == ecoval.translate("L_macrophytes_rivertype_class_mediumsubmergedhelophyte",dict))
  if ( length(ind.trans) > 0 )
  {
    for ( i in ind.trans )
    {
      if ( abs(res$types.scheme.probs[i,ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict)]-
               res$types.scheme.probs[i,ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict)]) < 0.2 )
      {
        if ( res$types.val.obs[i] == ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict) &
             !is.na(res$val.types[i,ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict)]) &
             !is.na(res$val.types[i,ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict)]) &
             res$val.types[i,ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict)] >
               res$val.types[i,ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict)] &
             !is.na(res$attrib.species[i,ecoval.translate("A_macrophytes_taxa_helophytes_richness_count",dict)]) &
             res$attrib.species[i,ecoval.translate("A_macrophytes_taxa_helophytes_richness_count",dict)] > 0 )
        {
          other.base.type[i] <- ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict)
        }
        else
        {
          if ( res$types.val.obs[i] == ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict) &
               !is.na(res$val.types[i,ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict)]) &
               !is.na(res$val.types[i,ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict)]) &
               res$val.types[i,ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict)] >
                 res$val.types[i,ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict)] &
               !is.na(res$attrib.species[i,ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict)]) &
               res$attrib.species[i,ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict)] > 0 )
          {
            other.base.type[i] <- ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict)
          }
        }
      }
    }
  }
  
  # V2: change to other discharge class:
  # ------------------------------------
  
  ind.subm <- which(res$types.val.obs %in% subm)
  if ( length(ind.subm) > 0 )
  {
    for ( i in ind.subm )
    {
      ind.types <- which(res$types.val.prob[i,subm] > 0.1)
      if ( length(ind.types) > 0 )
      {
        pot.types <- subm[ind.types]
        ind <- which.max(res$val.types[i,pot.types])
        if ( length(ind) > 0 )
        {
          if ( res$val.types[i,pot.types[ind]] > res$val[i,1] ) other.discharge.class[i] <- pot.types[ind]
          # accept type "KS" only if there are aquatic species:
          if ( pot.types[ind] == ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict) )
          {
            if ( !is.na(res$attrib.species[i,ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict)]) &
                 res$attrib.species[i,ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict)] == 0 )
              other.discharge.class[i] <- ""
          }
        }
      }
    }
  }
  
  ind.helo <- which(res$types.val.obs %in% helo)
  if ( length(ind.helo) > 0 )
  {
    for ( i in ind.helo )
    {
      ind.types <- which(res$types.val.prob[i,helo] > 0.1)
      if ( length(ind.types) > 0 )
      {
        pot.types <- helo[ind.types]
        ind <- which.max(res$val.types[i,pot.types])
        if ( length(ind) > 0 )
        {
          if ( res$val.types[i,pot.types[ind]] > res$val[i,1] ) other.discharge.class[i] <- pot.types[ind]
        }
      }
    }
  }
  
  ind.helo.moss <- which(res$types.val.obs %in% helo.moss)
  if ( length(ind.helo.moss) > 0 )
  {
    for ( i in ind.helo.moss )
    {
      ind.types <- which(res$types.val.prob[i,helo.moss] > 0.1)
      if ( length(ind.types) > 0 )
      {
        pot.types <- helo.moss[ind.types]
        ind <- which.max(res$val.types[i,pot.types])
        if ( length(ind) > 0 )
        {
          if ( res$val.types[i,pot.types[ind]] > res$val[i,1] ) other.discharge.class[i] <- pot.types[ind]
        }
      }
    }
  }
  
  ind.moss <- which(res$types.val.obs %in% moss)
  if ( length(ind.moss) > 0 )
  {
    for ( i in ind.moss )
    {
      ind.types <- which(res$types.val.prob[i,moss] > 0.1)
      if ( length(ind.types) > 0 )
      {
        pot.types <- moss[ind.types]
        ind <- which.max(res$val.types[i,pot.types])
        if ( length(ind) > 0 )
        {
          if ( res$val.types[i,pot.types[ind]] > res$val[i,1] ) other.discharge.class[i] <- pot.types[ind]
        }
      }
    }
  }
  
  # V3: change to poor vegetation due to low coverage:
  # --------------------------------------------------
  
  # submerse and helophyte rivers:
  
  change.poorveg[res$types.scheme.obs %in% c(ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict),
                                             ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict),
                                             ecoval.translate("L_macrophytes_rivertype_class_smallsubmergedhelophyte",dict),
                                             ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)) &
                 !is.na(res$val[,1]) & res$val[,1] < 0.6 &
                 res$attrib.species[,ecoval.translate("A_macrophytes_taxa_aquatic_abscover_percent",dict)] +
                   res$attrib.species[,ecoval.translate("A_macrophytes_taxa_helophytes_abscover_percent",dict)] < 10 &
                 res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.1] <- 
    ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)
  
  change.poorveg[res$types.scheme.obs %in% c(ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict),
                                             ecoval.translate("L_macrophytes_rivertype_class_mediumsubmergedhelophyte",dict),
                                             ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict)) &
                 !is.na(res$val[,1]) & res$val[,1] < 0.6 &
                 res$attrib.species[,ecoval.translate("A_macrophytes_taxa_aquatic_abscover_percent",dict)] +
                   res$attrib.species[,ecoval.translate("A_macrophytes_taxa_helophytes_abscover_percent",dict)] < 10 &
                 res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.2] <- 
    ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)
  
  change.poorveg[res$types.scheme.obs %in% c(ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict)) &
                 !is.na(res$val[,1]) & res$val[,1] < 0.6 &
                 res$attrib.species[,ecoval.translate("A_macrophytes_taxa_aquatic_abscover_percent",dict)] +
                   res$attrib.species[,ecoval.translate("A_macrophytes_taxa_helophytes_abscover_percent",dict)] < 10 &
                 res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.3] <- 
    ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)
  
  # helophyte-moss rivers:
  
  change.poorveg[res$types.scheme.obs %in% c(ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict)) &
                  !is.na(res$val[,1]) & res$val[,1] < 0.6 &
                 res$attrib.species[,ecoval.translate("A_macrophytes_taxa_helophytes_abscover_percent",dict)] +
                   res$attrib.species[,ecoval.translate("A_macrophytes_taxa_bryophytes_abscover_percent",dict)] < 10 &
                 res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.1] <- 
    ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)
  
  change.poorveg[res$types.scheme.obs %in% c(ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)) &
                 !is.na(res$val[,1]) & res$val[,1] < 0.6 &
                 res$attrib.species[,ecoval.translate("A_macrophytes_taxa_helophytes_abscover_percent",dict)] +
                   res$attrib.species[,ecoval.translate("A_macrophytes_taxa_bryophytes_abscover_percent",dict)] < 10 &
                 res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.2] <- 
    ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)
  
  # moss rivers:

  change.poorveg[res$types.scheme.obs %in% c(ecoval.translate("L_macrophytes_rivertype_class_smallbryophyte",dict)) &
                 !is.na(res$val[,1]) & res$val[,1] < 0.6 &
                 res$attrib.species[,ecoval.translate("A_macrophytes_taxa_bryophytes_abscover_percent",dict)] < 10 &
                 res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.1] <- 
    ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)
  
  change.poorveg[res$types.scheme.obs %in% c(ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict),
                                             ecoval.translate("L_macrophytes_rivertype_class_largebryophyte",dict)) &
                 !is.na(res$val[,1]) & res$val[,1] < 0.6 &
                 res$attrib.species[,ecoval.translate("A_macrophytes_taxa_bryophytes_abscover_percent",dict)] < 10 &
                 res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.2] <- 
    ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)
  
  change.poorveg[res$types.scheme.obs %in% c(ecoval.translate("L_macrophytes_rivertype_class_verylargebryophyte",dict)) &
                 !is.na(res$val[,1]) & res$val[,1] < 0.6 &
                 res$attrib.species[,ecoval.translate("A_macrophytes_taxa_bryophytes_abscover_percent",dict)] < 10 &
                 res$types.scheme.probs[,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)] > 0.3] <- 
    ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict)

  # V4: change to river type of same discharge class with better valuation:
  # -----------------------------------------------------------------------
  
  ind1 <- res$types.val.obs %in% c(ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict),
                                   ecoval.translate("L_macrophytes_rivertype_class_smallbryophyte",dict)) &
          res$val.types[,ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict)] > res$val[,1] & 
          res$attrib.species[,ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict)] > 0
  ind2 <- res$types.val.obs %in% c(ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict),
                                   ecoval.translate("L_macrophytes_rivertype_class_smallbryophyte",dict)) &
          res$val.types[,ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)] > res$val[,1] & 
          res$attrib.species[,ecoval.translate("A_macrophytes_taxa_helophytes_richness_count",dict)] > 0
  change.better.valuation[ind1 & !ind2] <- ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict)
  change.better.valuation[ind2 & !ind1] <- ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)
  change.better.valuation[ind1 & ind2]  <- paste(ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict),
                                                 ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict),sep=",")
  
  ind1 <- res$types.val.obs %in% c(ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict),
                                   ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict)) &
          res$val.types[,ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict)] > res$val[,1] &
          res$attrib.species[,ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict)] > 0
  ind2 <- res$types.val.obs %in% c(ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict),
                                   ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict)) &
          res$val.types[,ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict)] > res$val[,1] &
          res$attrib.species[,ecoval.translate("A_macrophytes_taxa_helophytes_richness_count",dict)] > 0
  change.better.valuation[ind1 & !ind2] <- ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict)
  change.better.valuation[ind2 & !ind1] <- ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict)
  change.better.valuation[ind1 & ind2]  <- paste(ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict),
                                                 ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict),sep=",")
  
  change.better.valuation[res$types.val.obs == ecoval.translate("L_macrophytes_rivertype_class_largebryophyte",dict) &
                          res$val.types[,ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict)] > res$val[,1] &
                          res$attrib.species[,ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict)] > 0] <-
    ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict)

  change.better.valuation[res$types.val.obs == ecoval.translate("L_macrophytes_rivertype_class_verylargebryophyte",dict) &
                          res$val.types[,ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict)] > res$val[,1] &
                          res$attrib.species[,ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict)] > 0] <-
    ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict)

  ind <- which( res$types.val.obs == ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict) &
                apply(res$val.types,1,max,na.rm=T) >= 0.6 )
  types <- colnames(res$val.types)
  if ( length(ind) > 0 )
  {
    for ( i in ind )
    {
      change.better.valuation[i] <- paste(types[res$val.types[i,] >= 0.6],collapse="," )
    }
  }
  
  # V: collect suggestions for type change:
  # ---------------------------------------
  
  plaus.suggest <- cbind(small.submerged.without.aquatic.grfo,
                         change.macrophyte.high.cover,
                         change.macrophyte.medium.cover,
                         change.moss.high.cover,
                         other.base.type,
                         other.discharge.class,
                         change.poorveg,
                         change.better.valuation)
  colnames(plaus.suggest) <- c(ecoval.translate("R_macrophytes_plaus_suggesttype_smallsubmwithoutaquaticgrfo",dict),
                               ecoval.translate("R_macrophytes_plaus_suggesttype_macrophyteriverduetohighcoverage",dict),
                               ecoval.translate("R_macrophytes_plaus_suggesttype_macrophyteriverduetomediumcoverage",dict),
                               ecoval.translate("R_macrophytes_plaus_suggesttype_mossriverduetohighcoverage",dict),
                               ecoval.translate("R_macrophytes_plaus_suggesttype_otherbasetypefortransitiontype",dict),
                               ecoval.translate("R_macrophytes_plaus_suggesttype_otherdischargeclassbettervalue",dict),
                               ecoval.translate("R_macrophytes_plaus_suggesttype_poorvegduetocoverage",dict),
                               ecoval.translate("R_macrophytes_plaus_suggesttype_bettervaltypesamedischarge",dict))
  collapse.char <- function(x)
  {
    y <- unique(x[!is.na(x) & x!=""])
    paste(y,collapse=",")
  }
  
  # set up template for plausibilization by user:
  # =============================================
  
  plaus.template <- data.frame(matrix(NA,ncol=21,nrow=n),stringsAsFactors = FALSE)
  colnames(plaus.template) <- c(ecoval.translate("A_macrophytes_rivertypescheme_orig_class",dict),
                                ecoval.translate("A_macrophytes_rivertypescheme_proposed_class",dict),
                                ecoval.translate("A_macrophytes_rivertypescheme_plaus_class",dict),
                                ecoval.translate("A_macrophytes_rivertypeval_orig_class",dict),
                                ecoval.translate("A_macrophytes_rivertypeval_proposed_class",dict),
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
  
  plaus.template[,ecoval.translate("A_macrophytes_rivertypescheme_orig_class",dict)] <- res$types.scheme.obs
  plaus.template[,ecoval.translate("A_macrophytes_rivertypeval_orig_class",dict)]    <- res$types.val.obs
  
  plaus.template[,ecoval.translate("A_macrophytes_rivertypescheme_proposed_class",dict)] <- apply(plaus.suggest[,c(2:4,6:8),drop=FALSE],1,collapse.char)
  plaus.template[,ecoval.translate("A_macrophytes_rivertypeval_proposed_class",dict)]    <- apply(plaus.suggest[,c(1,5),drop=FALSE],1,collapse.char)
  
  return(data.frame(plaus.warning,
                    plaus.suggest,
                    plaus.template))
}

