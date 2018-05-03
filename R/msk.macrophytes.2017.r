# This script contains of the value function for macrophyte community assessment
#
# VERSION 19 / 01.03.2017 CM
# 
# Update history:
# CM, 11.12.2015:    Update script to newest Kerntypen typology, types implemented KH, MH, MS, SGS
# CM, 28.01.2016:    Some corrections, e.g. removed regime from input of newest functions
#                    Changed function names to "Cur", indicating that this is the current function
# CM, 22.02.2016:    Value functions adjusted according to "Flusstypen_TypisierungWertefunktion_160222.xls",
#                    including introduction of all value functions for new type KHS
# CM, 02.03.2016:    Adjusted value function for number growthforms VdW and Herbids-Graminoids for river
#                    river types KH und MH, changes documented in "Flusstypen_TypisierungWertefunktion_160229.xlsx"
# CM, 18.03.2016:    i.) Included "required = T" for the conditional knots "Diversity", "AbsCoverage", "GrowthformsComp"
#                    and "Species Composition". ii.) Removed abbreviation "vdW" from growthforms knot names.
#                    iii.) Added value function for number of aquatic growthforms
# CM/PR, 07.04.2016: Adjusted all value functions KH, MH, KHS, MS, SGS according to recommondations from TaskForce:
#                    Added bonus aggregation (function: addminbonusmalus / PR) for Richness Taxa and
#                    Growthform nodes. Added additional value functions for algal cover and removed value function
#                    assessoing relative cover of higher macrophytes.
# CM, 12.04.2016:    Updated value function according to meeting BK/CM/PR at AWEL ZH: Introduced addmin aggregation
#                    in most nodes. Clarified attribute not relevant for type by setting negative range.
# CM, 13.04.2016     Defined value function(s) for river types KH-KM, MH-MM, SGH, SGH-SGM.
# CM, 27.04.2016     Added node for assessment of species quality according to Leitarten concept, Infoflora.
#                    Attributes describing the porportion Leitarten and the mean quality
#                    of all species in the site were added.Attributes joined by additive minimum.
# CM, 02.05.2016     Updated aggregation node "Qualitaet Arten" to summarize nutrient and Leitarten Indices; Also,
#                    Set weight of nutrient index to 0 to be ignored in aggregation
# CM/PR, 03.05.2016  Adjusted script to introduce different weights for addmin aggregation at the four levels of
#                    the value function, adjusted for validation.
# CM, 18.05.2016     Corrected Species richness attribute value function for type MS and SGS; Adjusted value function
#                    Aggregate algal cover with macrophyte cover using a malus, as well as restructured community composition
#                    node to aggregate macrophytes using addmin, which is then aggregated with algal proportion using malus
#                    weighted aggregation
# PR, 27.05.2016     Value functions with discrete counts
#                      "Viele Arten xx"
#                      "Arten WF Helophyten xx"
#                      "Arten WF Aquatisch xx"
#                      "Arten WF Moose xx"
#                      "Anzahl WF xx"
#                      "Anzahl helophytische WF xx"
#                      "Anzahl aquatische WF xx"
#                    modified from lower bounds of classes to class mid-point if there is only one discrete value 
#                    within the class (and similar for two)
# PR, 13.06.2016     Changes according to spreadsheet Barbara ("Wertfunktion_MS_KH_KHS.xls", Email PR 13.06.2016)
# CM, 15.06.2016     Checked previous adjustments, and adopted changes PR (27.05/13.06) also to types SGH and SGH-SGM
# CM, 30.06.2016     Initiated value function for bryophyte rivers
# CM, 05.07.2016     Finalized value function for bryophyte and macrophyte rivers, value function for species quality
# CM, 09.08.2016     Adjusted value function to include neophyte cover as malus among community composition, beta-diversity
#                    aspect (wgt.mean.quality.leitarten) as parallel attribute to overall species richness in diversity
#                    cluster, and priotity of species as overall bonus on total assessment.
# CM, 29.08.2016     Readjusted structure to include assessment goal "Hohe Qualitaet Arten". In addition several corrections
#                    of values functions
# CM, 05.09.2016     Updated nodes "Hohe Prioritaet Arten" und "Hohe Qualitaet Leitarten" mit neuer Knotenfunktion
#                    utility.endnode.classcounts.create (PR, 04.09.206)
# CM, 15.09.2016     Adjusted value functions and aggregations according to discussion in TaskForce 7
# CM, 03.10.2016     Adjusted value functions for dominance according to new Heip index 0-1, plus calculated with Bryophytes
#                    only considered as a single species
# CM, 04.10.2016     Adjusted bryophyte river value functions. Proportion bryophytes now assessed with attribut calculated as proportion
#                    bryophytes on natural substrate plu half the proportion on artififcial substrate.Assessment of Leitarten now via
#                    adjusted  value-function for Leitartn in bryophyte rivers. Since Quality levels have been assigend more strict for
#                    bryophytes a class 2 and 3 value is given a bigger increment in bryophyte rivers.
# CM, 08.10.2016     introduced seperate value functions for the small an medium bryophyte river types (KM, MM)
# CM, 10.10.2016     Adjusted value function according to Emails BK 09.10.2016, river types KM, MM, KH. reduce n.grfo class boundaries
#                    by 1, adjusted weight "Typger. Zusammensetzung":Dominanz to 1:1, and introduced overall weights
#                    for Diversitaet:Zusammensetzung:Deckung to 6:4:2
# CM, 12.10.2016     Adjusted Leitarten value function for bryophyte rivers after meeting with Niklaus Mueller, as well as the aquatic
#                    species attribute (n.aquat, n.gro.aquat. dg.aquat.rel) function for river-type KHS
# CM, 17.10.2016     Adjusted weights for weighted average aggregation at node "Gute typger. Gemeinschaft" to 8:4:2 (Email BK 16.10.2016);
#                    introduced aggregation node for seperate assessment of Leitarten for macrophytes and bryophytes
# CM, 20.10.2016     Adjusted value function for KHS, and changed aggregation of assessment Leitarten Moose and Leitarten Makrophyten
#                    that in each river type group the main vegetation assessment ist aggregated with 2:1 with the other assessment
#                    Updated all higher macrophyte river type functions to incorporate bryophyte attributes, and defined value functions
#                    for transition types Helophyte-Submerged-Bryophyte rivers
# CM, 26.10.2016     Several minor adjustments to value functions assessing number of species and growthforms. INcrement initially smaller,
#                    and increasing with more species in site. Moved good status value for absolute cover macrophytes to 90% in the small
#                    macrophyte river types KH, KHS, and KSHM
# CM, 01.11.2016     Adjusted value functions for Richness and Richness growth to a minimum number necessary to achieve a orange assessment.
# PR, 06.11.2016     Adjustments to all core value functions according to email Barbara Kaenel, 6.11.16
# PR, 10.11.2016     Adjustments to core value functions according to email Barbara Kaenel, 10.11.16
# CM, 15.02.2017     Checked script to agree to finale assessment goals submitted to BAFU. Some corrections to achieve cleaner code.
# CM, 01.03.2017     Adjusted attribute name for proportion of bryophytes on artificial substrate to "moos.anteilkuenst"
# PR, 27.08.1017     Conversion to ecoval package completed
# PR, 13.11.2017     New labelling of nodes of objectives hierarchy

######################################################################################################

msk.macrophytes.2017.create<- function(language     = "English",
                                       dictionaries = NA,
                                       col          = "black")
{
  # dictionary for node, attribute and attribute level names:
  # =========================================================
  
  dict <- ecoval.dict(language,dictionaries)
  
  # create macrophyte river and bryophyte river hierarchies:
  # ========================================================
  
  MacrophytesRiverAssessment <- 
    msk.macrophytes.2017.macrophytesriver.create(language,dictionaries,col)
  
  BryophytesRiverAssessment <- 
    msk.macrophytes.2017.bryophytesriver.create(language,dictionaries,col)
  
  # aggregate over the two overarching river types and return:
  # ==========================================================
  
  MacrophytesAssessment <- utility.aggregation.create(
    name.node     = ecoval.translate("N_macrophytes_macbryriver_goodstate",dict), 
    nodes         = list(MacrophytesRiverAssessment,BryophytesRiverAssessment), 
    name.fun      = "utility.aggregate.min",
    required      = TRUE,
    par           = numeric(0))
  
  return(MacrophytesAssessment)
}
  
######################################################################################################

# Aggregation addminbonusmalus:
# =============================

# parameter (n is the number of subobjectives to aggregate):
# par[1:n]:  weights
# par[n+1]:  weight of additive aggregation (weight of minimum aggregation is 1-par[n+1])
# par[(n+2):(2*n+1)]:  indicators -1: malus, +1 bonus, 0 addmin

msk.macrophytes.2017.addminbonusmalus <- function(u,par)
{
  n <- length(u)
  if ( length(par) != 2*n+1 ) 
  {
    warning("Length of parameter vector not equal to twice the number of utilities/values + 1:",
            " par: ",length(par)," u: ",length(u))
    return(NA)
  }  
  par.bonusmalus <- par[(n+2):(2*n+1)]
  
  # indices of main sub-objectives:
  ind.0 <- abs(par.bonusmalus) < 0.5
  if ( sum(ind.0) == 0 ) return(NA)
  
  # evaluate for main sub-objectives:
  u.main <- utility.aggregate.addmin(u[ind.0],c(par[1:n][ind.0],par[n+1]))
  u.loc <- u
  
  # check for bonus:
  ind.p1 <- par.bonusmalus >= 0.5
  if ( sum(ind.p1) > 0 ) u.loc[ind.p1] <- ifelse(u[ind.p1]>u.main,u[ind.p1],NA)
  
  # check for malus:
  ind.m1 <- par.bonusmalus <= -0.5
  if ( sum(ind.m1) > 0 ) u.loc[ind.m1] <- ifelse(u[ind.m1]<u.main,u[ind.m1],NA)
  
  # evaluate with "active" bonus and malus sub-objectives:
  return(utility.aggregate.addmin(u.loc,par[1:(n+1)]))
}

######################################################################################################

# Internal function to create joint elements for macrophytes and brypophytes rivers:
# ==================================================================================

create.joint.elements <- function(language     = "English",
                                  dictionaries = NA,
                                  col          = "black")
{
  # dictionary for node, attribute and attribute level names:
  # =========================================================
  
  dict <- ecoval.dict(language,dictionaries)
  
  # Define attribute value functions that are independent of the river type
  # =======================================================================
  
  # The functions are formulated with node names for the macrophyte rivers. 
  # These names will be replaced by the correct node names before application 
  # in the functions creating the value functions for each overarching river
  # type.
  
  # Assessment of index species for macrophytes

  IndexSpeciesMac <- utility.endnode.classcounts.create(
    name.node   = ecoval.translate("N_macrophytes_indexspeciesmac",dict),
    name.attrib = c(ecoval.translate("A_macrophytes_indexspeciesmac3_count",dict),
                    ecoval.translate("A_macrophytes_indexspeciesmac2_count",dict),
                    ecoval.translate("A_macrophytes_indexspeciesmac1_count",dict)),
    u.max.inc   = list(c(0.53, 0.06,0.04,0.01),
                       c(0.17, 0.04,0.01),
                       c(0.00,0.01)),
    exceed.next = FALSE,
    utility     = FALSE)
  
  # Assessment of index species for bryophytes:
    
  IndexSpeciesBry <- utility.endnode.classcounts.create(
    name.node   = ecoval.translate("N_macrophytes_indexspeciesbry",dict),
    name.attrib = c(ecoval.translate("A_macrophytes_indexspeciesbry3_count",dict),
                    ecoval.translate("A_macrophytes_indexspeciesbry2_count",dict),
                    ecoval.translate("A_macrophytes_indexspeciesbry1_count",dict)),
    u.max.inc   = list(c(0.70, 0.10,0.08,0.01),
                       c(0.50, 0.08,0.01),
                       c(0.00, 0.01)),
    exceed.next = TRUE,
    utility     = FALSE)
  
  # Assessment of priority species, accord. to Infoflora/FUB AG
  
  PriorityAll <- utility.endnode.classcounts.create(
    name.node   = ecoval.translate("N_macrophytes_priorityspecies",dict),
    name.attrib = c(ecoval.translate("A_macrophytes_priorityspecies1_count",dict),
                    ecoval.translate("A_macrophytes_priorityspecies2_count",dict),
                    ecoval.translate("A_macrophytes_priorityspecies3_count",dict),
                    ecoval.translate("A_macrophytes_priorityspecies4_count",dict)),
    u.max.inc   = list(c(0.95, 0.10,0.10,0.10,0.08),
                       c(0.85, 0.10,0.10,0.08),
                       c(0.70, 0.10,0.08),
                       c(0.59, 0.08)),
    exceed.next = TRUE,
    utility     = FALSE)
  
  # Assessment of proportion of neophyte biomass in community:
  
  NeophytesAll <- utility.endnode.intpol1d.create(
    name.node   = ecoval.translate("N_macrophytes_neophytes",dict), 
    name.attrib = ecoval.translate("A_macrophytes_neophytes_relcover_percent",dict),
    range       = c(0,100), 
    x           = c(0  , 0.0001, 20 , 30 , 40 , 50, 100), 
    u           = c(1.0, 0.79 , 0.6, 0.4, 0.2, 0 , 0), 
    utility     = FALSE)

  # return elements:
  
  return(list(IndexSpeciesMac  = IndexSpeciesMac,
              IndexSpeciesBry  = IndexSpeciesBry,
              PriorityAll      = PriorityAll,
              NeophytesAll     = NeophytesAll))
}
  
######################################################################################################

msk.macrophytes.2017.macrophytesriver.create <- function(language     = "English",
                                                         dictionaries = NA,
                                                         col          = "black")
{
  # dictionary for node, attribute and attribute level names:
  # =========================================================
  
  dict <- ecoval.dict(language,dictionaries)
  
  # joint elements (adapt node names):
  # ==================================
  
  joint.elements <- create.joint.elements(language,dictionaries,col)
  
  IndexSpeciesMac      <- joint.elements$IndexSpeciesMac
  IndexSpeciesMac$name <- ecoval.translate("N_macrophytes_indexspeciesmac",dict)
  IndexSpeciesBry      <- joint.elements$IndexSpeciesBry
  IndexSpeciesBry$name <- ecoval.translate("N_macrophytes_indexspeciesbry",dict)
  PriorityAll          <- joint.elements$PriorityAll
  PriorityAll$name     <- ecoval.translate("N_macrophytes_priorityspecies",dict)
  NeophytesAll         <- joint.elements$NeophytesAll
  NeophytesAll$name    <- ecoval.translate("N_macrophytes_neophytes",dict)
  
  # construction of end nodes:
  # ==========================
  
  # ---------
  # Diversity
  # ---------
  
  # Taxa richness:
  # ==============
  
  # Taxa richness value functions per river type:
  
  Richness.KH <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manytaxa",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_all_richness_count",dict), 
    range        = c(0,30), 
    x            = c(0, 1   , 2   , 3   , 4   , 5   , 6   , 7   , 8, 30), 
    u            = c(0, 0.05, 0.10, 0.21, 0.41, 0.65, 0.90, 0.95, 1,  1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.MH <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manytaxa",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_all_richness_count",dict), 
    range        = c(0,30), 
    x            = c(0, 1   , 2   , 3   , 4   , 5   , 6   ,  7   , 8   , 9, 30),
    u            = c(0, 0.05, 0.10, 0.21, 0.35, 0.41, 0.65,  0.90, 0.95, 1, 1),
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.KHS <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manytaxa",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_all_richness_count",dict), 
    range        = c(0,30), 
    x            = c(0, 1   , 2   , 3   , 4   , 5   , 6   , 7   , 8   , 9  , 10, 30), 
    u            = c(0, 0.05, 0.10, 0.21, 0.31, 0.41, 0.55, 0.70, 0.90, 0.95, 1 , 1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.MS <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manytaxa",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_all_richness_count",dict), 
    range        = c(0,30), 
    x            = c(0, 1   , 2  , 3   , 4   , 5   , 6   , 7   , 8   , 9   , 10  , 11  , 12  , 13, 30), 
    u            = c(0, 0.03, 0.1, 0.17, 0.25, 0.33, 0.41, 0.55, 0.70, 0.74, 0.78, 0.90, 0.95, 1 , 1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.SGS <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manytaxa",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_all_richness_count",dict), 
    range        = c(0,30), 
    x            = c(0, 1    , 2   , 3   , 4   , 5   , 6   , 7   , 8   , 9  , 10 , 11, 30), 
    u            = c(0, 0.075, 0.15, 0.25, 0.35, 0.45, 0.55, 0.70, 0.85, 0.93, 1  , 1 , 1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.KSHM <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manytaxa",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_all_richness_count",dict), 
    range        = c(0,30), 
    x            = c(0, 1   , 2   , 3   , 4   , 5   , 6   , 7   , 8   , 9, 30), 
    u            = c(0, 0.05, 0.10, 0.21, 0.41, 0.65, 0.77, 0.90, 0.95, 1,  1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.MSHM <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manytaxa",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_all_richness_count",dict), 
    range        = c(0,30), 
    x            = c(0, 1   , 2   , 3   , 4   , 5   , 6   , 7   , 8   , 11, 30),
    u            = c(0, 0.05, 0.10, 0.21, 0.31, 0.41, 0.65, 0.85, 0.95,  1,  1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  # Richness endnode, conditional on river type:
  
  attrib.levels = data.frame(c(ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_macrophytes_rivertype_class",dict)
  Richness.Taxa <- utility.endnode.cond.create(
    name.node     = ecoval.translate("N_macrophytes_manytaxa",dict), 
    attrib.levels = attrib.levels, 
    nodes         = list(Richness.KH, Richness.MH, Richness.KHS,
                         Richness.MS, Richness.MS, Richness.SGS,
                         Richness.KSHM, Richness.MSHM), 
    utility      = FALSE, 
    required     = FALSE)
  
  # Taxa richness within typical growth form:
  # =========================================
  
  # Value functions for number of taxa of helophytic growthforms per river type:
  
  Richness.Helo.KH <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manyhelophytes",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_helophytes_richness_count",dict), 
    range        = c(0,30), 
    x            = c(0, 1   , 2   , 3   , 4   , 5  , 6   , 7, 30), 
    u            = c(0, 0.05, 0.15, 0.30, 0.65, 0.9, 0.95, 1,  1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.Helo.MH <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manyhelophytes",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_helophytes_richness_count",dict), 
    range        = c(0,30), 
    x            = c(0, 1   , 2   , 3   , 4   , 5   , 6   , 7, 30), 
    u            = c(0, 0.05, 0.15, 0.30, 0.65, 0.90, 0.95, 1,  1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.Helo.KHS <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manyhelophytes",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_helophytes_richness_count",dict), 
    range        = c(0,30), 
    x            = c(0, 1   , 2   , 3   , 4   , 5   , 6   , 7, 30), 
    u            = c(0, 0.05, 0.15, 0.30, 0.61, 0.85, 0.95, 1,  1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.Helo.MS <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manyhelophytes",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_helophytes_richness_count",dict), 
    range        = c(0,30), 
    x            = c(0, 1   , 2   , 3   , 4   , 5   , 6   , 7, 30), 
    u            = c(0, 0.05, 0.15, 0.30, 0.61, 0.85, 0.95, 1,  1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.Helo.SGS <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manyhelophytes",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_helophytes_richness_count",dict), 
    range        = c(0,30), 
    x            = c(0, 1   , 2   , 3   , 4   , 5  , 6, 30), 
    u            = c(0, 0.15, 0.35, 0.65, 0.90, 0.95, 1, 1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.Helo.KSHM <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manyhelophytes",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_helophytes_richness_count",dict), 
    range        = c(0,30), 
    x            = c(0, 1   , 2   , 3   , 4   , 5   , 6, 30), 
    u            = c(0, 0.05, 0.19, 0.41, 0.75, 0.95, 1,  1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.Helo.MSHM <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manyhelophytes",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_helophytes_richness_count",dict), 
    range        = c(0,30), 
    x            = c(0, 1   , 2   , 3   , 4   , 5   , 6, 30), 
    u            = c(0, 0.05, 0.19, 0.45, 0.70, 0.95, 1,  1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  # Value functions for number of taxa of helophytic growthforms conditional on river type:
  
  attrib.levels = data.frame(c(ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_macrophytes_rivertype_class",dict)
  Richness.Helo.Taxa <- utility.endnode.cond.create(
    name.node     = ecoval.translate("N_macrophytes_manyhelophytes",dict),
    attrib.levels = attrib.levels, 
    nodes         = list(Richness.Helo.KH, Richness.Helo.MH, Richness.Helo.KHS,
                         Richness.Helo.MS, Richness.Helo.MS, Richness.Helo.SGS,
                         Richness.Helo.KSHM, Richness.Helo.MSHM), 
    utility      = FALSE, 
    required     = FALSE)
  
  # Value functions for number of taxa of aquatic growthforms per river type:
  
  Richness.Aquat.KH <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manyaquatic",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict), 
    range        = c(-2,-1), 
    x            = c(0,30), 
    u            = c(1,1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.Aquat.MH <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manyaquatic",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict), 
    range        = c(-2,-1), 
    x            = c(0,30), 
    u            = c(1,1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.Aquat.KHS <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manyaquatic",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict), 
    range        = c(0.1, 30), 
    x            = c(0.1, 1   , 2   , 3   , 4, 30), 
    u            = c(0  , 0.50, 0.70, 0.95, 1, 1),
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.Aquat.MS <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manyaquatic",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict), 
    range        = c(0,30), 
    x            = c(0, 1   , 2   , 3   , 4   , 5  , 6, 30), 
    u            = c(0, 0.15, 0.35, 0.70, 0.90, 0.95, 1,  1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.Aquat.SGS <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manyaquatic",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict), 
    range        = c(0,30), 
    x            = c(0, 1   , 2   , 3   , 4   , 5   , 6, 30), 
    u            = c(0, 0.15, 0.35, 0.70, 0.90, 0.95, 1,  1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.Aquat.KSHM <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manyaquatic",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict), 
    range        = c(-2,-1), 
    x            = c(0,30), 
    u            = c(1,1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.Aquat.MSHM <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manyaquatic",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict), 
    range        = c(-2,-1), 
    x            = c(0,30), 
    u            = c(1,1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  # Value functions for number of taxa of aquatic growthforms conditional on river type:
  
  attrib.levels = data.frame(c(ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_macrophytes_rivertype_class",dict)
  Richness.Aquat.Taxa <- utility.endnode.cond.create(
    name.node     = ecoval.translate("N_macrophytes_manyaquatic",dict), 
    attrib.levels = attrib.levels, 
    nodes         = list(Richness.Aquat.KH, Richness.Aquat.MH, Richness.Aquat.KHS,
                         Richness.Aquat.MS, Richness.Aquat.MS, Richness.Aquat.SGS,
                         Richness.Aquat.KSHM, Richness.Aquat.MSHM), 
    utility      = FALSE, 
    required     = FALSE)
  
  # Value functions for number of taxa of bryophyte growthforms per river type:

  Richness.Bryo.KH <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manybryophytes",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_bryophytes_richness_count",dict), 
    range        = c(-2,-1), 
    x            = c(0,30), 
    u            = c(1,1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.Bryo.MH <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manybryophytes",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_bryophytes_richness_count",dict), 
    range        = c(-2,-1), 
    x            = c(0,30), 
    u            = c(1,1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.Bryo.KHS <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manybryophytes",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_bryophytes_richness_count",dict), 
    range        = c(-2,-1), 
    x            = c(0, 30), 
    u            = c(1,1),
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.Bryo.MS <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manybryophytes",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_bryophytes_richness_count",dict), 
    range        = c(-2,-1), 
    x            = c(0,30), 
    u            = c(1,1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.Bryo.SGS <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manybryophytes",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_bryophytes_richness_count",dict), 
    range        = c(-2,-1), 
    x            = c(0,30), 
    u            = c(1,1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.Bryo.KSHM <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manybryophytes",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_bryophytes_richness_count",dict), 
    range        = c(0,30), 
    x            = c(0, 1   , 2   , 3   , 4   , 5, 30), 
    u            = c(0, 0.19, 0.50, 0.79, 0.95, 1,  1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.Bryo.MSHM <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manybryophytes",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)), 
    name.attrib  = ecoval.translate("A_macrophytes_taxa_bryophytes_richness_count",dict), 
    range        = c(0,30), 
    x            = c(0, 1   , 2   , 3   , 4   , 5, 30), 
    u            = c(0, 0.19, 0.45, 0.75, 0.95, 1, 1 ), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  # Value functions for number of taxa of bryophyte growthforms conditional on river type:
  
  attrib.levels = data.frame(c(ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_macrophytes_rivertype_class",dict)
  Richness.Bryo.Taxa <- utility.endnode.cond.create(
    name.node     = ecoval.translate("N_macrophytes_manybryophytes",dict), 
    attrib.levels = attrib.levels, 
    nodes         = list(Richness.Bryo.KH, Richness.Bryo.MH, Richness.Bryo.KHS,
                         Richness.Bryo.MS, Richness.Bryo.MS, Richness.Bryo.SGS,
                         Richness.Bryo.KSHM, Richness.Bryo.MSHM), 
    utility      = FALSE, 
    required     = FALSE)
  
  # Richness in growth forms:
  # =========================
  
  # Richness growthforms CH value functions per river type:
  
  Growthforms.KH <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manygrowthforms",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_growthform_all_richness_count",dict), 
    range         = c(0,20), 
    x             = c(0, 1   , 2   , 3   , 4   , 5, 20), 
    u             = c(0, 0.10, 0.25, 0.61, 0.90, 1, 1), 
    utility       = FALSE)
  
  Growthforms.MH <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manygrowthforms",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_growthform_all_richness_count",dict), 
    range         = c(0,20), 
    x             = c(0, 1   , 2   , 3   , 4   , 5   , 6, 20), 
    u             = c(0, 0.10, 0.21, 0.41, 0.65, 0.90, 1,  1), 
    utility       = FALSE)
  
  Growthforms.KHS <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manygrowthforms",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_growthform_all_richness_count",dict), 
    range         = c(0,20), 
    x             = c(0, 1   , 2   , 3   , 4   , 5   , 6   , 7, 20), 
    u             = c(0, 0.10, 0.21, 0.41, 0.65, 0.85, 0.95, 1,  1), 
    utility       = FALSE)
  
  Growthforms.MS <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manygrowthforms",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_growthform_all_richness_count",dict), 
    range         = c(0,20), 
    x             = c(0, 1   , 2   , 3   , 4   ,  5   , 6   , 7   , 8   , 9, 20), 
    u             = c(0, 0.10, 0.25, 0.41, 0.55,  0.65, 0.75, 0.85, 0.95, 1,  1), 
    utility       = FALSE)
  
  Growthforms.SGS <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manygrowthforms",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_growthform_all_richness_count",dict), 
    range         = c(0,20), 
    x             = c(0, 1   , 2   , 3   , 4   , 5   , 6   , 7, 20), 
    u             = c(0, 0.10, 0.41, 0.65, 0.75, 0.85, 0.95, 1, 1), 
    utility       = FALSE)
  
  Growthforms.KSHM <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manygrowthforms",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_growthform_all_richness_count",dict), 
    range         = c(0,20), 
    x             = c(0, 1   , 2   , 3   , 4   , 5   , 6, 20), 
    u             = c(0, 0.10, 0.21, 0.41, 0.65, 0.90, 1, 1), 
    utility       = FALSE)
  
  Growthforms.MSHM <- utility.endnode.intpol1d.create(
    name.node    = paste(ecoval.translate("N_macrophytes_manygrowthforms",dict),
                         ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_growthform_all_richness_count",dict), 
    range         = c(0,20), 
    x             = c(0, 1   , 2   , 3   , 4   , 5   , 6, 20), 
    u             = c(0, 0.10, 0.21, 0.41, 0.65, 0.90, 1,  1), 
    utility       = FALSE)
  
  # Richness growthforms CH value functions conditional on river type:

  attrib.levels = data.frame(c(ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_macrophytes_rivertype_class",dict)
  Growthforms <- utility.endnode.cond.create(
    name.node     = ecoval.translate("N_macrophytes_manygrowthforms",dict), 
    attrib.levels = attrib.levels, 
    nodes         = list(Growthforms.KH, Growthforms.MH, Growthforms.KHS,
                         Growthforms.MS, Growthforms.MS, Growthforms.SGS,
                         Growthforms.KSHM, Growthforms.MSHM), 
    utility       = FALSE)
  
  # Number type specific growthforms CH:
  # ====================================
  
  # Number of helophytic growthforms CH per river type:
  
  Helophytes.Grfo.KH <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_manygrowthformshelophytes",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_growthform_helophytes_richness_count",dict), 
    range         = c(0,20), 
    x             = c(0, 1   , 2   , 3   , 4   , 5, 20), 
    u             = c(0, 0.10, 0.30, 0.65, 0.95, 1, 1), 
    utility       = FALSE)
  
  Helophytes.Grfo.MH <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_manygrowthformshelophytes",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_growthform_helophytes_richness_count",dict), 
    range         = c(0,20), 
    x             = c(0, 1   , 2   , 3   , 4   , 5, 20), 
    u             = c(0, 0.10, 0.30, 0.65, 0.95, 1,  1), 
    utility       = FALSE)
  
  Helophytes.Grfo.KHS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_manygrowthformshelophytes",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_growthform_helophytes_richness_count",dict), 
    range         = c(0,20), 
    x             = c(0, 1   , 2   , 3   , 4   , 5, 20), 
    u             = c(0, 0.10, 0.30, 0.61, 0.90, 1,  1), 
    utility       = FALSE)
  
  Helophytes.Grfo.MS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_manygrowthformshelophytes",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_growthform_helophytes_richness_count",dict), 
    range         = c(0,20), 
    x             = c(0, 1   , 2   , 3   , 4  , 5, 20),
    u             = c(0, 0.10, 0.30, 0.61, 0.9, 1,  1), 
    utility       = FALSE)
  
  Helophytes.Grfo.SGS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_manygrowthformshelophytes",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_growthform_helophytes_richness_count",dict), 
    range         = c(0,20), 
    x             = c(0, 1   , 2   , 3  , 4, 20), 
    u             = c(0, 0.19, 0.61, 0.9, 1,  1), 
    utility       = FALSE)
  
  Helophytes.Grfo.KSHM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_manygrowthformshelophytes",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_growthform_helophytes_richness_count",dict), 
    range         = c(0,20), 
    x             = c(0, 1   , 2   , 3   , 4   , 5, 20), 
    u             = c(0, 0.15, 0.41, 0.70, 0.95, 1, 1), 
    utility       = FALSE)
  
  Helophytes.Grfo.MSHM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_manygrowthformshelophytes",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_growthform_helophytes_richness_count",dict), 
    range         = c(0,20), 
    x             = c(0, 1   , 2   , 3   , 4   , 5, 20), 
    u             = c(0, 0.15, 0.41, 0.70, 0.95, 1,  1), 
    utility       = FALSE)
  
  # Number of helophytic growthforms CH conditional on river type:

  attrib.levels = data.frame(c(ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_macrophytes_rivertype_class",dict)
  Helophytes.Grfo <- utility.endnode.cond.create(
    name.node     = ecoval.translate("N_macrophytes_manygrowthformshelophytes",dict), 
    attrib.levels = attrib.levels, 
    nodes         = list(Helophytes.Grfo.KH, Helophytes.Grfo.MH, Helophytes.Grfo.KHS,
                         Helophytes.Grfo.MS, Helophytes.Grfo.MS, Helophytes.Grfo.SGS,
                         Helophytes.Grfo.KSHM, Helophytes.Grfo.MSHM), 
    utility       = FALSE)
  
  # Number of aquatic growthforms CH per river type:
  
  Aquatic.Grfo.KH <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_manygrowthformsaquatic",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_growthform_aquatic_richness_count",dict), 
    range         = c(-2,-1), 
    x             = c(0,20), 
    u             = c(1,1), 
    utility       = FALSE)
  
  Aquatic.Grfo.MH <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_manygrowthformsaquatic",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_growthform_aquatic_richness_count",dict), 
    range         = c(-2,-1), 
    x             = c(0,20), 
    u             = c(1,1), 
    utility       = FALSE)
  
  Aquatic.Grfo.KHS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_manygrowthformsaquatic",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_growthform_aquatic_richness_count",dict), 
    range         = c(0.1,20), 
    x             = c(0.1, 1   , 2   , 3   , 4, 20), 
    u             = c(0  , 0.50, 0.65, 0.95, 1, 1), 
    utility       = FALSE)
  
  Aquatic.Grfo.MS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_manygrowthformsaquatic",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_growthform_aquatic_richness_count",dict), 
    range         = c(0,20), 
    x             = c(0, 1   , 2   , 3   , 4   , 5, 20), 
    u             = c(0, 0.15, 0.41, 0.65, 0.95, 1,  1), 
    utility       = FALSE)
  
  Aquatic.Grfo.SGS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_manygrowthformsaquatic",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_growthform_aquatic_richness_count",dict), 
    range         = c(0,20), 
    x             = c(0, 1   , 2   , 3   , 4, 20), 
    u             = c(0, 0.19, 0.65, 0.95, 1,  1), 
    utility       = FALSE)
  
  Aquatic.Grfo.KSHM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_manygrowthformsaquatic",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_growthform_aquatic_richness_count",dict), 
    range         = c(-2,-1), 
    x             = c(0,20), 
    u             = c(1,1), 
    utility       = FALSE)
  
  Aquatic.Grfo.MSHM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_manygrowthformsaquatic",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_growthform_aquatic_richness_count",dict), 
    range         = c(-2,-1), 
    x             = c(0,20), 
    u             = c(1,1), 
    utility       = FALSE)
  
  # Number of aquatic growthforms CH conditional on river type:

  attrib.levels = data.frame(c(ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_macrophytes_rivertype_class",dict)
  Aquatic.Grfo <- utility.endnode.cond.create(
    name.node     = ecoval.translate("N_macrophytes_manygrowthformsaquatic",dict), 
    attrib.levels = attrib.levels, 
    nodes         = list(Aquatic.Grfo.KH, Aquatic.Grfo.MH, Aquatic.Grfo.KHS,
                         Aquatic.Grfo.MS, Aquatic.Grfo.MS, Aquatic.Grfo.SGS,
                         Aquatic.Grfo.KSHM, Aquatic.Grfo.MSHM), 
    utility       = FALSE)

  # ---------------------
  # Community composition 
  # ---------------------
  
  # Growthform composition:
  # =======================
  
  # Proportion of helophytes per river type:
  
  Prop.Helo.KH <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fracthelophytes",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_taxa_helophytes_relcover_percent",dict), 
    range         = c(0,100), 
    x             = c(0, 40 , 50 , 60 , 70 , 80, 100), 
    u             = c(0, 0.2, 0.4, 0.6, 0.8, 1 , 1), 
    utility       = FALSE)
  
  Prop.Helo.MH <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fracthelophytes",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_taxa_helophytes_relcover_percent",dict), 
    range         = c(0,100), 
    x             = c(0, 30 , 40 , 50 , 60 , 70, 100), 
    u             = c(0, 0.2, 0.4, 0.6, 0.8, 1,   1), 
    utility       = FALSE)
  
  Prop.Helo.KHS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fracthelophytes",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_taxa_helophytes_relcover_percent",dict), 
    range         = c(0,100), 
    x             = c(0, 10 , 20 , 30 , 40 , 50, 100), 
    u             = c(0, 0.2, 0.4, 0.6, 0.8, 1 , 1), 
    utility       = FALSE)
  
  Prop.Helo.MS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fracthelophytes",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_taxa_helophytes_relcover_percent",dict), 
    range         = c(0,100), 
    x             = c(0, 10 , 20 , 30 , 40 , 50, 100), 
    u             = c(0, 0.2, 0.4, 0.6, 0.8, 1 , 1), 
    utility       = FALSE)
  
  Prop.Helo.SGS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fracthelophytes",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_taxa_helophytes_relcover_percent",dict), 
    range         = c(0,100), 
    x             = c(0, 10 , 15 , 20 , 30 , 40, 100), 
    u             = c(0, 0.2, 0.4, 0.6, 0.8, 1 , 1), 
    utility       = FALSE)
  
  Prop.Helo.KSHM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fracthelophytes",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_taxa_helophytes_relcover_percent",dict), 
    range         = c(0,100), 
    x             = c(0, 10 , 20 , 30 , 40 , 50, 100), 
    u             = c(0, 0.2, 0.4, 0.6, 0.8, 1 , 1), 
    utility       = FALSE)
  
  Prop.Helo.MSHM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fracthelophytes",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_taxa_helophytes_relcover_percent",dict), 
    range         = c(0,100), 
    x             = c(0, 10 , 15 , 25 , 35 , 45, 100), 
    u             = c(0, 0.2, 0.4, 0.6, 0.8, 1,   1), 
    utility       = FALSE)
  
  # Proportion of helophytes conditional on river type:
  
  attrib.levels = data.frame(c(ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_macrophytes_rivertype_class",dict)
  Prop.Helo <- utility.endnode.cond.create(
    name.node     = ecoval.translate("N_macrophytes_fracthelophytes",dict), 
    attrib.levels = attrib.levels, 
    nodes         = list(Prop.Helo.KH, Prop.Helo.MH, Prop.Helo.KHS,
                         Prop.Helo.MS, Prop.Helo.MS, Prop.Helo.SGS,
                         Prop.Helo.KSHM, Prop.Helo.MSHM), 
    utility       = FALSE)
  
  # Proportion of aquatic growthforms per river type:


  Prop.Aquat.KH <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractaquatic",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_taxa_aquatic_relcover_percent",dict), 
    range         = c(-2,-1), 
    x             = c(0,100), 
    u             = c(1,1), 
    utility       = FALSE)
  
  Prop.Aquat.MH <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractaquatic",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_taxa_aquatic_relcover_percent",dict), 
    range         = c(-2,-1), 
    x             = c(0,100), 
    u             = c(1,1), 
    utility       = FALSE)
  
  Prop.Aquat.KHS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractaquatic",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_taxa_aquatic_relcover_percent",dict), 
    range         = c(0.01, 100), 
    x             = c(0.01, 5  , 10 , 20 , 30 , 40, 100), 
    u             = c(0   , 0.2, 0.4, 0.6, 0.8,  1, 1), 
    utility       = FALSE)
  
  Prop.Aquat.MS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractaquatic",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_taxa_aquatic_relcover_percent",dict), 
    range         = c(0,100), 
    x             = c(0,  10 ,  20,   30 ,  40 ,  50,   100), 
    u             = c(0,  0.2,  0.4,  0.6,  0.8,  1 ,   1), 
    utility       = FALSE)
  
  Prop.Aquat.SGS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractaquatic",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_taxa_aquatic_relcover_percent",dict), 
    range         = c(0,100), 
    x             = c(0,  20 ,  30 ,  40 ,  50 ,  60, 100), 
    u             = c(0,  0.2,  0.4,  0.6,  0.8,  1 , 1), 
    utility       = FALSE)
  
  Prop.Aquat.KSHM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractaquatic",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_taxa_aquatic_relcover_percent",dict), 
    range         = c(-2,-1), 
    x             = c(0,100), 
    u             = c(1,1), 
    utility       = FALSE)
  
  Prop.Aquat.MSHM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractaquatic",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_taxa_aquatic_relcover_percent",dict), 
    range         = c(-2,-1), 
    x             = c(0,100), 
    u             = c(1,1), 
    utility       = FALSE)
  
  # Proportion of aquatic growthforms conditional on river type:

  attrib.levels = data.frame(c(ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_macrophytes_rivertype_class",dict)
  Prop.Aquat <- utility.endnode.cond.create(
    name.node     = ecoval.translate("N_macrophytes_fractaquatic",dict), 
    attrib.levels = attrib.levels, 
    nodes         = list(Prop.Aquat.KH, Prop.Aquat.MH, Prop.Aquat.KHS,
                         Prop.Aquat.MS, Prop.Aquat.MS, Prop.Aquat.SGS,
                         Prop.Aquat.KSHM, Prop.Aquat.MSHM), 
    utility       = FALSE)
  
  # Proportion of bryophytes per river type:
  
  Prop.Bryo.KH <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractbryophytesadj",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_taxa_bryophytes_adjusted_relcover_percent",dict),
    range         = c(-2,-1),
    x             = c(0,100),
    u             = c(1,1),
    utility       = FALSE)

  Prop.Bryo.MH <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractbryophytesadj",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_taxa_bryophytes_adjusted_relcover_percent",dict),
    range         = c(-2,-1),
    x             = c(0,100),
    u             = c(1,1),
    utility       = FALSE)

  Prop.Bryo.KHS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractbryophytesadj",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_taxa_bryophytes_adjusted_relcover_percent",dict),
    range         = c(-2,-1),
    x             = c(0,100),
    u             = c(1,1),
    utility       = FALSE)

  Prop.Bryo.MS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractbryophytesadj",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_taxa_bryophytes_adjusted_relcover_percent",dict),
    range         = c(-2,-1),
    x             = c(0,100),
    u             = c(1,1),
    utility       = FALSE)

  Prop.Bryo.SGS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractbryophytesadj",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_taxa_bryophytes_adjusted_relcover_percent",dict),
    range         = c(-2,-1),
    x             = c(0,100),
    u             = c(1,1),
    utility       = FALSE)

  Prop.Bryo.KSHM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractbryophytesadj",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_taxa_bryophytes_adjusted_relcover_percent",dict),
    range         = c(0,100),
    x             = c(0, 5  , 12.5, 20 , 25 , 40, 100),
    u             = c(0, 0.2, 0.4, 0.6, 0.8, 1 , 1),
    utility       = FALSE)

  Prop.Bryo.MSHM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractbryophytesadj",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_taxa_bryophytes_adjusted_relcover_percent",dict),
    range         = c(0,100),
    x             = c(0, 10 , 20 , 30 , 40 , 50, 100),
    u             = c(0, 0.2, 0.4, 0.6, 0.8, 1 , 1  ),
    utility       = FALSE)
  
  # Proportion of bryophytes conditional on river type:

  attrib.levels = data.frame(c(ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_macrophytes_rivertype_class",dict)
  Prop.Bryo <- utility.endnode.cond.create(
    name.node     = ecoval.translate("N_macrophytes_fractbryophytesadj",dict), 
    attrib.levels = attrib.levels, 
    nodes         = list(Prop.Bryo.KH, Prop.Bryo.MH, Prop.Bryo.KHS,
                         Prop.Bryo.MS, Prop.Bryo.MS, Prop.Bryo.SGS,
                         Prop.Bryo.KSHM, Prop.Bryo.MSHM), 
    utility       = FALSE)
  
  # Proportion of filamentous algae per river type:

  Prop.Algae.KH <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractalgae",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_filamentousgreenalgae_relcover_percent",dict),
    range         = c(0,100),
    x             = c(100, 75, 50 , 30 , 20 , 10 , 0),
    u             = c(  0,  0, 0.2, 0.4, 0.6, 0.8, 1),
    utility       = FALSE)

  Prop.Algae.MH <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractalgae",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_filamentousgreenalgae_relcover_percent",dict),
    range         = c(0,100),
    x             = c(100, 80, 60 , 40 , 20 , 10 , 0),
    u             = c(0  , 0 , 0.2, 0.4, 0.6, 0.8, 1),
    utility       = FALSE)

  Prop.Algae.KHS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractalgae",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_filamentousgreenalgae_relcover_percent",dict),
    range         = c(0,100),
    x             = c(100, 75, 50 , 30 , 20 , 10 , 0),
    u             = c(0  , 0 , 0.2, 0.4, 0.6, 0.8, 1),
    utility       = FALSE)

  Prop.Algae.MS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractalgae",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_filamentousgreenalgae_relcover_percent",dict),
    range         = c(0,100),
    x             = c(100, 75, 50 , 30 , 20 , 10 , 0),
    u             = c(0  , 0 , 0.2, 0.4, 0.6, 0.8, 1),
    utility       = FALSE)

  Prop.Algae.SGS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractalgae",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_filamentousgreenalgae_relcover_percent",dict),
    range         = c(0,100),
    x             = c(100, 80, 60 , 40 , 20 , 10 , 0),
    u             = c(0  , 0 , 0.2, 0.4, 0.6, 0.8, 1),
    utility       = FALSE)

  Prop.Algae.KSHM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractalgae",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_filamentousgreenalgae_relcover_percent",dict),
    range         = c(0,100),
    x             = c(100, 75, 50  , 30  , 20  , 10  , 0),
    u             = c(  0,  0, 0.2,  0.4,  0.6,  0.8, 1),
    utility       = FALSE)

  Prop.Algae.MSHM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractalgae",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_filamentousgreenalgae_relcover_percent",dict),
    range         = c(0,100),
    x             = c(100, 80, 60 , 40 , 20 , 10 , 0),
    u             = c(0  , 0 , 0.2, 0.4, 0.6, 0.8, 1),
    utility       = FALSE)
  
  # Proportion of filamentous algae conditional on river type:

  attrib.levels = data.frame(c(ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_macrophytes_rivertype_class",dict)
  Prop.Algae <- utility.endnode.cond.create(
    name.node     = ecoval.translate("N_macrophytes_fractalgae",dict), 
    attrib.levels = attrib.levels, 
    nodes         = list(Prop.Algae.KH, Prop.Algae.MH, Prop.Algae.KHS,
                         Prop.Algae.MS, Prop.Algae.MS, Prop.Algae.SGS,
                         Prop.Algae.KSHM, Prop.Algae.MSHM), 
    utility       = FALSE)

  # Dominance per river type:
  
  Dominance.KH <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_dominance",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)),
    name.attrib  = ecoval.translate("A_macrophytes_heipindex_bryophyteonespecies",dict), 
    range        = c(0,1), 
    x            = c(0, 0.20, 0.40, 0.55, 0.70, 0.85, 1.0), 
    u            = c(0, 0.2 , 0.4 , 0.6 , 0.8 , 1   , 1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Dominance.MH <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_dominance",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict)),
    name.attrib  = ecoval.translate("A_macrophytes_heipindex_bryophyteonespecies",dict), 
    range        = c(0,1), 
    x            = c(0, 0.20, 0.40, 0.55, 0.70, 0.85, 1.0), 
    u            = c(0, 0.2 , 0.4 , 0.6 , 0.8 , 1   , 1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Dominance.KHS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_dominance",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict)),
    name.attrib  = ecoval.translate("A_macrophytes_heipindex_bryophyteonespecies",dict), 
    range        = c(0,1), 
    x            = c(0, 0.15, 0.30, 0.45, 0.60, 0.75, 1.0), 
    u            = c(0, 0.2 , 0.4 , 0.6 , 0.8 , 1   , 1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Dominance.MS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_dominance",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict)),
    name.attrib  = ecoval.translate("A_macrophytes_heipindex_bryophyteonespecies",dict), 
    range        = c(0,1), 
    x            = c(0, 0.15, 0.30, 0.45, 0.60, 0.75, 1.0), 
    u            = c(0, 0.2 , 0.4 , 0.6 , 0.8 , 1   , 1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Dominance.SGS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_dominance",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict)),
    name.attrib  = ecoval.translate("A_macrophytes_heipindex_bryophyteonespecies",dict), 
    range        = c(0,1), 
    x            = c(0, 0.10, 0.25, 0.40, 0.55, 0.7, 1.0), 
    u            = c(0, 0.2 , 0.4 , 0.6 , 0.8 , 1  , 1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Dominance.KSHM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_dominance",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict)),
    name.attrib  = ecoval.translate("A_macrophytes_heipindex_bryophyteonespecies",dict), 
    range        = c(-2,-1), 
    x            = c(0,1), 
    u            = c(1,1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Dominance.MSHM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_dominance",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)),
    name.attrib  = ecoval.translate("A_macrophytes_heipindex_bryophyteonespecies",dict), 
    range        = c(-2,-1), 
    x            = c(0,1), 
    u            = c(1,1), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  

  # Dominance conditional on river type:

  attrib.levels = data.frame(c(ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_macrophytes_rivertype_class",dict)
  Dominance.Taxa <- utility.endnode.cond.create(
    name.node     = ecoval.translate("N_macrophytes_dominance",dict), 
    attrib.levels = attrib.levels, 
    nodes         = list(Dominance.KH, Dominance.MH, Dominance.KHS , 
                         Dominance.MS, Dominance.MS, Dominance.SGS,
                         Dominance.KSHM, Dominance.MSHM), 
    utility       = FALSE)
  
  # -------------- 
  # Absolute cover
  # --------------
  
  # Absolute cover macrophytes value functions per river type:
  
  AbsCover.Macro.KH <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_cover",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict), 
    range         = c(0,100), 
    x             = c(0, 10 , 20 , 30 , 40 , 50, 80, 85 , 90 , 93 , 96 , 100), 
    u             = c(0, 0.2, 0.4, 0.6, 0.8, 1 , 1,  0.8, 0.6, 0.4, 0.2, 0), 
    utility       = FALSE)
  
  AbsCover.Macro.MH <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_cover",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict), 
    range         = c(0,100), 
    x             = c(0, 10 , 15 , 20 , 25 , 30, 60, 65 , 75 , 80 , 90 , 100), 
    u             = c(0, 0.2, 0.4, 0.6, 0.8, 1 , 1 , 0.8, 0.6, 0.4, 0.2,   0), 
    utility       = FALSE)
  
  AbsCover.Macro.KHS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_cover",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict), 
    range         = c(0,100), 
    x             = c(0, 10 ,  20 ,  30 ,  40 , 50, 80, 85 , 90 , 93 , 96 , 100), 
    u             = c(0, 0.2,  0.4,  0.6,  0.8,  1, 1 , 0.8, 0.6, 0.4, 0.2, 0), 
    utility       = FALSE)
  
  AbsCover.Macro.MS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_cover",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict), 
    range         = c(0,100), 
    x             = c(0, 10 , 20 , 30 , 40 , 50, 75, 80 , 85 , 90 , 95 , 100), 
    u             = c(0, 0.2, 0.4, 0.6, 0.8, 1 , 1 , 0.8, 0.6, 0.4, 0.2, 0), 
    utility       = FALSE)
  
  AbsCover.Macro.SGS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_cover",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict), 
    range         = c(0,100), 
    x             = c(0, 10 , 15 , 20 , 25  , 30, 70, 75 , 80 , 90 , 95 , 100), 
    u             = c(0, 0.2, 0.4, 0.6, 0.8 , 1 , 1 , 0.8, 0.6, 0.4, 0.2, 0), 
    utility       = FALSE)
  
  AbsCover.Macro.KSHM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_cover",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict), 
    range         = c(0,100), 
    x             = c(0, 10 , 15 , 20 , 25 , 30, 75, 80 , 85 , 90 , 95 , 100), 
    u             = c(0, 0.2, 0.4, 0.6, 0.8, 1 , 1,  0.8, 0.6, 0.4, 0.2, 0), 
    utility       = FALSE)
  
  AbsCover.Macro.MSHM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_cover",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict), 
    range         = c(0,100), 
    x             = c(0, 5  , 10 , 15 , 20 , 25, 70, 75 , 80 , 85 , 90 , 100), 
    u             = c(0, 0.2, 0.4, 0.6, 0.8, 1 , 1 , 0.8, 0.6, 0.4, 0.2, 0), 
    utility       = FALSE)
  
  # Absolute cover macrophytes value functions conditional on river type:
  
  attrib.levels = data.frame(c(ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_macrophytes_rivertype_class",dict)
  AbsCover.Macro <- utility.endnode.cond.create(
    name.node     = ecoval.translate("N_macrophytes_cover",dict), 
    attrib.levels = attrib.levels, 
    nodes         = list(AbsCover.Macro.KH, AbsCover.Macro.MH, AbsCover.Macro.KHS,
                         AbsCover.Macro.MS, AbsCover.Macro.MS, AbsCover.Macro.SGS,
                         AbsCover.Macro.KSHM, AbsCover.Macro.MSHM),
    required      = FALSE,
    utility       = FALSE)
  
  # Absolute cover filamentous algae value functions per river type:
  
  AbsCover.Algae.KH <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_coveralgae",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_filamentousgreenalgae_abscover_percent",dict), 
    range         = c(0,100), 
    x             = c(0  , 10 , 20 , 30 , 40 , 50, 100), 
    u             = c(1.0, 0.8, 0.6, 0.4, 0.2, 0 , 0), 
    utility       = FALSE)
  
  AbsCover.Algae.MH <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_coveralgae",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_filamentousgreenalgae_abscover_percent",dict), 
    range         = c(0,100), 
    x             = c(0  , 10 , 20 , 30 , 40 , 50, 100), 
    u             = c(1.0, 0.8, 0.6, 0.4, 0.2, 0 , 0), 
    utility       = FALSE)
  
  AbsCover.Algae.KHS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_coveralgae",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_filamentousgreenalgae_abscover_percent",dict), 
    range         = c(0,100), 
    x             = c(0  , 10 , 20 , 30 , 40 , 50, 100), 
    u             = c(1.0, 0.8, 0.6, 0.4, 0.2, 0 , 0),
    utility       = FALSE)
  
  AbsCover.Algae.MS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_coveralgae",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumsubmergede",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_filamentousgreenalgae_abscover_percent",dict), 
    range         = c(0,100), 
    x             = c(0  , 10 , 20 , 30 , 40 , 50, 100), 
    u             = c(1.0, 0.8, 0.6, 0.4, 0.2, 0 , 0), 
    utility       = FALSE)
  
  AbsCover.Algae.SGS <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_coveralgae",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_filamentousgreenalgae_abscover_percent",dict), 
    range         = c(0,100), 
    x             = c(0  , 10 , 20 , 30 , 40 , 50, 100), 
    u             = c(1.0, 0.8, 0.6, 0.4, 0.2, 0, 0),  
    utility       = FALSE)
  
  AbsCover.Algae.KSHM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_coveralgae",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_filamentousgreenalgae_abscover_percent",dict), 
    range         = c(0,100), 
    x             = c(0  , 10 , 20 , 30 , 40 , 50, 100), 
    u             = c(1.0, 0.8, 0.6, 0.4, 0.2, 0 , 0), 
    utility       = FALSE)
  
  AbsCover.Algae.MSHM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_coveralgae",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_filamentousgreenalgae_abscover_percent",dict), 
    range         = c(0,100), 
    x             = c(0  , 10 , 20 , 30 , 40 , 50, 100), 
    u             = c(1.0, 0.8, 0.6, 0.4, 0.2, 0 , 0), 
    utility       = FALSE)
  
  # Absolute cover filamentous algae value functions conditional on river type:

  attrib.levels = data.frame(c(ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_macrophytes_rivertype_class",dict)
  AbsCover.Algae <- utility.endnode.cond.create(
    name.node     = ecoval.translate("N_macrophytes_coveralgae",dict), 
    attrib.levels = attrib.levels, 
    nodes         = list(AbsCover.Algae.KH, AbsCover.Algae.MH, AbsCover.Algae.KHS,
                         AbsCover.Algae.MS, AbsCover.Algae.MS, AbsCover.Algae.SGS,
                         AbsCover.Algae.KSHM, AbsCover.Algae.MSHM),
    required      = FALSE,
    utility       = FALSE)
  
  # -----------------
  # Aggregation nodes
  # -----------------

  # Richness of taxa by growth form:
  
  Richness.GF.Taxa <- utility.aggregation.create(
    name.node     = ecoval.translate("N_macrophytes_manytaxaoftypicalgrowthform",dict), 
    nodes         = list(Richness.Bryo.Taxa, Richness.Helo.Taxa, Richness.Aquat.Taxa), 
    name.fun      = "msk.macrophytes.2017.addminbonusmalus", 
    par           = c(1,1,1,0.5,0,0,0))
  
  # Richness of taxa and growth forms:
  
  Richness.Taxa.Conditional <- utility.aggregation.create(
    name.node     = ecoval.translate("N_macrophytes_typicalnumtaxa",dict), 
    nodes         = list(Richness.Taxa, Richness.GF.Taxa), 
    name.fun      = "msk.macrophytes.2017.addminbonusmalus", 
    par           = c(1,2,1,1,0))
  
  # Richness of growth forms:
  
  Growthforms.Number <- utility.aggregation.create(
    name.node     = ecoval.translate("N_macrophytes_manytypicalgrowthforms",dict), 
    nodes         = list(Helophytes.Grfo, Aquatic.Grfo), 
    name.fun      = "msk.macrophytes.2017.addminbonusmalus", 
    par           = c(1,1,0.5,0,0))
  
  # Typical number of growth forms:
  
  Richness.Growthforms.Conditional <- utility.aggregation.create(
    name.node     = ecoval.translate("N_macrophytes_typicalnumgrowthforms",dict), 
    nodes         = list(Growthforms, Growthforms.Number), 
    name.fun      = "msk.macrophytes.2017.addminbonusmalus", 
    par           = c(1,2,1,1,0))
  
  # Typical fraction of macrophytes:
  
  Prop.Macrophytes <- utility.aggregation.create(
    name.node     = ecoval.translate("N_macrophytes_typicalfractmacrophytes",dict), 
    nodes         = list(Prop.Bryo, Prop.Helo, Prop.Aquat), 
    name.fun      = "msk.macrophytes.2017.addminbonusmalus", 
    par           = c(1,1,1,0.5,0,0,0))
  
  # Typical fractions of growth forms:
  
  Growthforms.Prop <- utility.aggregation.create(
    name.node     = ecoval.translate("N_macrophytes_typicalfractofgrowthforms",dict), 
    nodes         = list(Prop.Algae, NeophytesAll, Prop.Macrophytes), 
    name.fun      = "msk.macrophytes.2017.addminbonusmalus", 
    par           = c(1,1,2,1,-1,-1,0))
  
  # Typical diversity:
  
  Diversity <-  utility.aggregation.create(
    name.node     = ecoval.translate("N_macrophytes_typicaldiversity",dict), 
    nodes         = list(Richness.Taxa.Conditional, Richness.Growthforms.Conditional), 
    name.fun      = "msk.macrophytes.2017.addminbonusmalus",
    required      = TRUE,
    par           = c(1,1,1,0,0))
  
  # Typical community composition:
  
  CommunityComposition <- utility.aggregation.create(
    name.node     = ecoval.translate("N_macrophytes_typicalcomposition",dict), 
    nodes         = list(Growthforms.Prop, Dominance.Taxa), 
    name.fun      = "msk.macrophytes.2017.addminbonusmalus",
    required      = TRUE,
    par           = c(1,1,1,0,-1))
  
  # Typical absolute cover:
  
  AbsCover<- utility.aggregation.create(
    name.node     = ecoval.translate("N_macrophytes_typicalcover",dict), 
    nodes         = list(AbsCover.Macro, AbsCover.Algae), 
    name.fun      = "msk.macrophytes.2017.addminbonusmalus",
    required      = TRUE, 
    par           = c(1,1,1,0,-1))
  
  # ------------------------------------
  # Good macrophyte community assessment
  # ------------------------------------
  
  CommunityAssessmentMacrophytesRiver <- utility.aggregation.create(
    name.node     = ecoval.translate("N_macrophytes_typicalcommunity",dict), 
    nodes         = list(Diversity, CommunityComposition, AbsCover), 
    name.fun      = "msk.macrophytes.2017.addminbonusmalus",
    required      = TRUE,
    par           = c(8,4,2,0.75,0,0,0))

  # --------------------------
  # Species Conservation Value 
  # --------------------------
  
  # Index taxa:
  
  Leitarten.Makro  <- utility.aggregation.create(
    name.node     = ecoval.translate("N_macrophytes_highvaluekeytaxa",dict), 
    nodes         = list(IndexSpeciesMac,IndexSpeciesBry), 
    name.fun      = "msk.macrophytes.2017.addminbonusmalus",
    required      = FALSE,
    par           = c(2,1,1,0,1))
  
  # Taxa quality:
  
  SpeciesConservationMacrophytesRiver <- utility.aggregation.create(
    name.node     = ecoval.translate("N_macrophytes_highqualitytaxa",dict), 
    nodes         = list(Leitarten.Makro, PriorityAll), 
    name.fun      = "utility.aggregate.max",
    required      = FALSE,
    par           = NA)
  
  # -----------------------------------------------
  # Overall aggregation node for macrophytes rivers 
  # -----------------------------------------------
  
  MacrophytesRiverAssessment <- utility.aggregation.create(
    name.node     = ecoval.translate("N_macrophytes_goodstate",dict), 
    nodes         = list(CommunityAssessmentMacrophytesRiver,SpeciesConservationMacrophytesRiver), 
    name.fun      = "msk.macrophytes.2017.addminbonusmalus",
    required      = FALSE,
    par           = c(1,1,1,0,1))

  return(MacrophytesRiverAssessment)
}

######################################################################################################

msk.macrophytes.2017.bryophytesriver.create <- function(language     = "English",
                                                        dictionaries = NA,
                                                        col          = "black")
{
  # dictionary for node, attribute and attribute level names:
  # =========================================================
  
  dict <- ecoval.dict(language,dictionaries)
  
  # joint elements (adapt node names):
  # ==================================
  
  joint.elements <- create.joint.elements(language,dictionaries,col)
  
  IndexSpeciesMac      <- joint.elements$IndexSpeciesMac
  IndexSpeciesMac$name <- paste(ecoval.translate("N_macrophytes_indexspeciesmac",dict),ecoval.translate("N_macrophytes_bryophytesriver",dict))
  IndexSpeciesBry      <- joint.elements$IndexSpeciesBry
  IndexSpeciesBry$name <- paste(ecoval.translate("N_macrophytes_indexspeciesbry",dict),ecoval.translate("N_macrophytes_bryophytesriver",dict))
  PriorityAll          <- joint.elements$PriorityAll
  PriorityAll$name     <- paste(ecoval.translate("N_macrophytes_priorityspecies",dict),ecoval.translate("N_macrophytes_bryophytesriver",dict))
  NeophytesAll         <- joint.elements$NeophytesAll
  NeophytesAll$name    <- paste(ecoval.translate("N_macrophytes_neophytes",dict),ecoval.translate("N_macrophytes_bryophytesriver",dict))

  # construction of end nodes:
  # ==========================
  
  # ---------
  # Diversity
  # ---------
  
  # Taxa richness per river type:
  
  Richness.KM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_manytaxa",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallbryophyte",dict),
                          ecoval.translate("N_macrophytes_bryophytesriver",dict)),
    name.attrib  = ecoval.translate("A_macrophytes_taxa_all_richness_count",dict), 
    range        = c(0,30), 
    x            = c(0, 1  , 2   , 3   , 4  , 5   , 6   , 7, 30), 
    u            = c(0, 0.1, 0.21, 0.45, 0.6, 0.75, 0.90, 1, 1 ), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  Richness.MM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_manytaxa",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict),
                          ecoval.translate("N_macrophytes_bryophytesriver",dict)),
    name.attrib  = ecoval.translate("A_macrophytes_taxa_all_richness_count",dict), 
    range        = c(0,30), 
    x            = c(0, 1  , 2   , 3   , 4   , 5  , 6   , 7   , 8, 30), 
    u            = c(0, 0.1, 0.21, 0.31, 0.45, 0.6, 0.75, 0.95, 1, 1 ), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  # Taxa richness conditional on river type:

  attrib.levels = data.frame(c(ecoval.translate("L_macrophytes_rivertype_class_smallbryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_largebryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_verylargebryophyte",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_macrophytes_rivertype_class",dict)
  Richness.Taxa.MB <- utility.endnode.cond.create(
    name.node     = paste(ecoval.translate("N_macrophytes_manytaxa",dict),
                          ecoval.translate("N_macrophytes_bryophytesriver",dict)), 
    attrib.levels = attrib.levels, 
    nodes         = list(Richness.KM, Richness.MM,
                         Richness.MM, Richness.MM), 
    utility      = FALSE, 
    required     = FALSE)
  
  # Number of species of typical growth form (bryophytes) per river type:
  
  Richness.Moos.KM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_manybryophytes",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallbryophyte",dict),
                          ecoval.translate("N_macrophytes_bryophytesriver",dict)),
    name.attrib  = ecoval.translate("A_macrophytes_taxa_bryophytes_richness_count",dict), 
    range        = c(0,30), 
    x            = c(0, 1   , 2   , 3   , 4   , 5   , 6, 30), 
    u            = c(0, 0.10, 0.25, 0.65, 0.85, 0.95, 1, 1 ), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0) 
  
  Richness.Moos.MM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_manybryophytes",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict),
                          ecoval.translate("N_macrophytes_bryophytesriver",dict)),
    name.attrib  = ecoval.translate("A_macrophytes_taxa_bryophytes_richness_count",dict), 
    range        = c(0,30), 
    x            = c(0, 1   , 2   , 3   , 4  , 5   , 6   , 7, 30), 
    u            = c(0, 0.05, 0.15, 0.35, 0.7, 0.85, 0.95, 1, 1 ), 
    utility      = FALSE, 
    required     = FALSE, 
    shift.levels = 0)
  
  # Number of species of typical growth form (bryophytes) conditional on river type:

    attrib.levels = data.frame(c(ecoval.translate("L_macrophytes_rivertype_class_smallbryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_largebryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_verylargebryophyte",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_macrophytes_rivertype_class",dict)
  Richness.Moos.Taxa.MB <- utility.endnode.cond.create(
    name.node     = paste(ecoval.translate("N_macrophytes_manybryophytes",dict),
                          ecoval.translate("N_macrophytes_bryophytesriver",dict)), 
    attrib.levels = attrib.levels, 
    nodes         = list(Richness.Moos.KM, Richness.Moos.MM,
                         Richness.Moos.MM, Richness.Moos.MM), 
    utility      = FALSE, 
    required     = FALSE)
  
  # ---------------------
  # Community composition 
  # ---------------------
  
  # Proportion of bryophytes per river type:
  
  Prop.Bryo.KM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractbryophytesadj",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallbryophyte",dict),
                          ecoval.translate("N_macrophytes_bryophytesriver",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_taxa_bryophytes_adjusted_relcover_percent",dict),
    range         = c(0,100), 
    x             = c(0, 20,  40,  60,  80,  90, 100), 
    u             = c(0, 0.2, 0.4, 0.6, 0.8, 1 , 1), 
    utility       = FALSE)
  
  # Proportion bryophytes value functions
  Prop.Bryo.MM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractbryophytesadj",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict),
                          ecoval.translate("N_macrophytes_bryophytesriver",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_taxa_bryophytes_adjusted_relcover_percent",dict),
    range         = c(0,100), 
    x             = c(0, 20,  30,  50,  70,  90, 100), 
    u             = c(0, 0.2, 0.4, 0.6, 0.8, 1 , 1), 
    utility       = FALSE)
  
  # Proportion of bryophytes conditional on river type:
  
  attrib.levels = data.frame(c(ecoval.translate("L_macrophytes_rivertype_class_smallbryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_largebryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_verylargebryophyte",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_macrophytes_rivertype_class",dict)
  Prop.Bryophytes.MB <- utility.endnode.cond.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractbryophytesadj",dict),
                          ecoval.translate("N_macrophytes_bryophytesriver",dict)), 
    attrib.levels = attrib.levels, 
    nodes         = list(Prop.Bryo.KM, Prop.Bryo.MM,
                         Prop.Bryo.MM, Prop.Bryo.MM), 
    utility       = FALSE)
  
  # Proportion pf bryophytes on artfifical per river type:
  
  Prop.Kuenst.MB <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_smallfractbryophytesartsubst",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict),
                          ecoval.translate("N_macrophytes_bryophytesriver",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_taxa_bryophytes_artificialsubstrate_relcover_percent",dict),
    range         = c(0,100), 
    x             = c(100, 80 ,  60,  40,  20,  0), 
    u             = c(0  , 0.2, 0.4, 0.6, 0.8, 1.0), 
    utility       = FALSE)
  
  # Proportion pf bryophytes on artfifical conditional on river type:

  attrib.levels = data.frame(c(ecoval.translate("L_macrophytes_rivertype_class_smallbryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_largebryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_verylargebryophyte",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_macrophytes_rivertype_class",dict)
  Prop.Kuenstlich.MB <- utility.endnode.cond.create(
    name.node     = paste(ecoval.translate("N_macrophytes_smallfractbryophytesartsubst",dict),
                          ecoval.translate("N_macrophytes_bryophytesriver",dict)), 
    attrib.levels = attrib.levels, 
    nodes         = list(Prop.Kuenst.MB, Prop.Kuenst.MB,
                         Prop.Kuenst.MB, Prop.Kuenst.MB), 
    utility       = FALSE)
  
  
  # Proportion of filamentous algae per river type:
  
  Prop.Algae.KM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractalgae",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_smallbryophyte",dict),
                          ecoval.translate("N_macrophytes_bryophytesriver",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_filamentousgreenalgae_relcover_percent",dict),
    range         = c(0,100), 
    x             = c(100, 80 ,  60,  40,  20, 0), 
    u             = c(0  , 0.2, 0.4, 0.6, 0.8, 1), 
    utility       = FALSE)
  
  Prop.Algae.MM <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractalgae",dict),
                          ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict),
                          ecoval.translate("N_macrophytes_bryophytesriver",dict)),
    name.attrib   = ecoval.translate("A_macrophytes_filamentousgreenalgae_relcover_percent",dict),
    range         = c(0,100), 
    x             = c(100, 80,  60,  40,  20,  0), 
    u             = c(  0, 0.2, 0.4, 0.6, 0.8, 1), 
    utility       = FALSE)
  
  # Proportion of filamentous algae conditional on river type:

  attrib.levels = data.frame(c(ecoval.translate("L_macrophytes_rivertype_class_smallbryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_largebryophyte",dict),
                               ecoval.translate("L_macrophytes_rivertype_class_verylargebryophyte",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_macrophytes_rivertype_class",dict)
  Prop.Algae.MB <- utility.endnode.cond.create(
    name.node     = paste(ecoval.translate("N_macrophytes_fractalgae",dict),
                          ecoval.translate("N_macrophytes_bryophytesriver",dict)), 
    attrib.levels = attrib.levels, 
    nodes         = list(Prop.Algae.KM, Prop.Algae.MM,
                         Prop.Algae.MM, Prop.Algae.MM), 
    utility       = FALSE)
  
  # -------------- 
  # Absolute cover
  # --------------
  
  # Absolute cover of filamentous algae:
  
  AbsCover.MB <- utility.endnode.intpol1d.create(
    name.node     = paste(ecoval.translate("N_macrophytes_coveralgae",dict),
                          ecoval.translate("N_macrophytes_bryophytesriver",dict)), 
    name.attrib   = ecoval.translate("A_macrophytes_filamentousgreenalgae_abscover_percent",dict), 
    range         = c(0,100), 
    x             = c(0  , 10 , 20 , 30 , 40 , 50, 100), 
    u             = c(1.0, 0.8, 0.6, 0.4, 0.2, 0 , 0),
    required      = TRUE,
    utility       = FALSE)
  
  # -----------------
  # Aggregation nodes
  # -----------------
  
  # Diversity:
  
  Diversity.MB <- utility.aggregation.create(
    name.node     = paste(ecoval.translate("N_macrophytes_typicaldiversity",dict),
                          ecoval.translate("N_macrophytes_bryophytesriver",dict)), 
    nodes         = list(Richness.Taxa.MB, Richness.Moos.Taxa.MB), 
    name.fun      = "msk.macrophytes.2017.addminbonusmalus",
    required      = TRUE,
    par           = c(1,2,1,1,0))
  
  # Proportion of bryophytes:
  
  Prop.BryoAdj.MB <- utility.aggregation.create(
    name.node     = paste(ecoval.translate("N_macrophytes_typicalfractmacrophytes",dict),
                          ecoval.translate("N_macrophytes_bryophytesriver",dict)), 
    nodes         = list(Prop.Kuenstlich.MB, Prop.Bryophytes.MB), 
    name.fun      = "msk.macrophytes.2017.addminbonusmalus",
    required      = FALSE,
    par           = c(0,1,1,0,0))
  
  ### Community composition:
  
  CommunityComposition.MB <- utility.aggregation.create(
    name.node     = paste(ecoval.translate("N_macrophytes_typicalcomposition",dict),
                          ecoval.translate("N_macrophytes_bryophytesriver",dict)), 
    nodes         = list(Prop.Algae.MB, NeophytesAll, Prop.BryoAdj.MB), 
    name.fun      = "msk.macrophytes.2017.addminbonusmalus",
    required      = TRUE,
    par           = c(1,1,2,1,-1,-1,0))
  
  # ------------------------------
  # Bryophyte community assessment
  # ------------------------------
  
  CommunityAssessmentBryophytesRiver <- utility.aggregation.create(
    name.node     = paste(ecoval.translate("N_macrophytes_typicalcommunity",dict),
                          ecoval.translate("N_macrophytes_bryophytesriver",dict)), 
    nodes         = list(Diversity.MB, CommunityComposition.MB, AbsCover.MB), 
    name.fun      = "msk.macrophytes.2017.addminbonusmalus",
    required      = TRUE,
    par           = c(8,4,2,0.75,0,0,-1))
  
  # --------------------------
  # Species Conservation Value 
  # --------------------------
  
  # Index taxa:

  Leitarten.Bryo  <- utility.aggregation.create(
    name.node     = paste(ecoval.translate("N_macrophytes_highvaluekeytaxa",dict),
                          ecoval.translate("N_macrophytes_bryophytesriver",dict)), 
    nodes         = list(IndexSpeciesMac,IndexSpeciesBry), 
    name.fun      = "msk.macrophytes.2017.addminbonusmalus",
    required      = FALSE,
    par           = c(1,2,1,1,0))
  
  # Taxa quality:
  
  SpeciesConservationBryophytesRiver <- utility.aggregation.create(
    name.node     = paste(ecoval.translate("N_macrophytes_highqualitytaxa",dict),
                          ecoval.translate("N_macrophytes_bryophytesriver",dict)), 
    nodes         = list(Leitarten.Bryo, PriorityAll), 
    name.fun      = "utility.aggregate.max",
    required      = FALSE,
    par           = NA)
  
  # ----------------------------------------------
  # Overall aggregation node for bryophytes rivers 
  # ----------------------------------------------
  
  BryophytesRiverAssessment <- utility.aggregation.create(
    name.node     = paste(ecoval.translate("N_macrophytes_goodstate",dict),ecoval.translate("N_macrophytes_bryophytesriver",dict)),
    nodes         = list(CommunityAssessmentBryophytesRiver,SpeciesConservationBryophytesRiver), 
    name.fun      = "msk.macrophytes.2017.addminbonusmalus",
    required      = FALSE,
    par           = c(1,1,1,0,1))

  return(BryophytesRiverAssessment)
}

######################################################################################################

