# ==============================================
# Create value function for lakeshore morphology
# ==============================================

# Peter Reichert, 19.01.2017


# lake.morphol.2016.create
# ========================

lake.morphol.2016.create <- function(language     = "English",
                                     dictionaries = NA,
                                     col          = NA)
{
  # ============================================================================
  #
  # References:
  #
  # Buederberger, K., Rey, P., Reichert, P., Schlosser, J., Helg, U.,
  # Haertel-Borer, S., Binderheim, E. (2016)
  # Methodenn zur Untersuchung und Beurteilung der Seen. 
  # Modul: Oekomorphologie Seeufer.
  # Bundesamt fuer Umwelg, Bern. Umwelt-Vollzug Nr. 1632. 73 S.
  # http://www.modul-stufen-konzept.ch
  3
  # Schlosser, J.A., Haertel-Borer, S., Liechti, P., Reichert, P. (2013)
  # Konzept fuer die Untersuchung und Beurteilung der Seen in der Schweiz.
  # Anleitung zur Entwicklung und Anwendung von Beurteilungsmethoden.
  # Bundesamt fuer Umwelt, Bern. Umwelt-Wissen Nr. 1326. 38 S.
  #
  # Langhans, S.D. und Reichert, P. (2011), Einbettung von Verfahren zur Fliess-
  # gewaesserbewertung in ein uebergeordnetes Gewaessermanagementkonzept - 
  # Vorschlaege am Beispiel des Modulstufenkonzepts, 
  # Wasser Energie Luft 103(3), 204-214. 
  #
  # ============================================================================
  
  # dictionary for node, attribute and attribute level names:
  # =========================================================
  
  dict <- ecoval.dict(language,dictionaries)

  # construction of end nodes:
  # ==========================
  
  # node "Keine Sohlenveraenderungen in der Flachwasserzone":
  # ---------------------------------------------------------
  
  attrib.levels = data.frame("E01_Sohlenveraenderungen_FWZ"=
                               c(ecoval.translate("L_lake_morphol_E01_E01.01",dict),
                                 ecoval.translate("L_lake_morphol_E01_E01.02",dict),
                                 ecoval.translate("L_lake_morphol_E01_E01.03",dict),
                                 ecoval.translate("L_lake_morphol_E01_E01.04",dict),
                                 ecoval.translate("L_lake_morphol_E01_E01.05",dict),
                                 ecoval.translate("L_lake_morphol_E01_E01.06",dict),
                                 ecoval.translate("L_lake_morphol_E01_E01.0601",dict),
                                 ecoval.translate("L_lake_morphol_E01_E01.0602",dict),
                                 ecoval.translate("L_lake_morphol_E01_E01.0603",dict),
                                 ecoval.translate("L_lake_morphol_E01_E01.07",dict),
                                 ecoval.translate("L_lake_morphol_E01_E01.08",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_lake_morphol_E01",dict)
  FlachwasserSohle <- utility.endnode.discrete.create(
    name.node     = ecoval.translate("N_lake_morphol_bedmod_shallowwater",dict), 
    attrib.levels = attrib.levels,
    u             = c(1,
                      0.75,
                      0.75,
                      0.5,
                      0.5,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0), 
    utility       = FALSE,
    required      = TRUE,
    col           = if(is.na(col)) "black" else col,
    shift.levels  = 1)
  
  # node "Keine Anlagen und Strukturen in der Flachwasserzone":
  # -----------------------------------------------------------

  attrib.levels = data.frame("E02_Anlagen_Strukturen_FWZ"=
                               c(ecoval.translate("L_lake_morphol_E02_E02.01",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.02",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0201",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0202",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0203",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0204",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.03",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0301",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0302",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0303",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0304",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0305",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.04",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0401",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0402",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0403",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0404",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0405",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0406",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0407",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0408",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0409",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0410",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0411",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0412",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0413",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.05",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0501",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0502",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0503",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0504",dict),
                                 ecoval.translate("L_lake_morphol_E02_E02.0505",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_lake_morphol_E02",dict)
  FlachwasserAnlagen <- utility.endnode.discrete.create(
    name.node     = ecoval.translate("N_lake_morphol_facilities_shallowwater",dict), 
    attrib.levels = attrib.levels,
    u             = c(1,
                      0.75,
                      0.75,
                      0.75,
                      0.75,
                      0.75,
                      0.5,
                      0.5,
                      0.5,
                      0.5,
                      0.5,
                      0.5,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0, 
                      0, 
                      0, 
                      0, 
                      0, 
                      0), 
    utility       = FALSE,
    required      = TRUE,
    col           = if(is.na(col)) "black" else col,
    shift.levels  = 1)
  
  # node "Naturnahe Anbindung des Fliessgewaessers":
  # ------------------------------------------------
    
  attrib.levels = data.frame("B02_Fliessgewaesseranbindung"=
                               c(ecoval.translate("L_lake_morphol_B02_B02.01",dict),
                                 ecoval.translate("L_lake_morphol_B02_B02.02",dict),
                                 ecoval.translate("L_lake_morphol_B02_B02.03",dict),
                                 ecoval.translate("L_lake_morphol_B02_B02.04",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_lake_morphol_B02",dict)
  Fliessgewaesseranbindung <- utility.endnode.discrete.create(
    name.node     = ecoval.translate("N_lake_morphol_tributary_mouth",dict), 
    attrib.levels = attrib.levels,
    u             = c(1,
                      1,
                      0.25,
                      0), 
    utility       = FALSE,
    required      = FALSE,
    col           = if(is.na(col)) "black" else col,
    shift.levels  = 1)
  
    # node "Keine Verbauung der Uferlinie":
    # -------------------------------------
    
  attrib.levels = data.frame("B01_Verbauung_UL"=
                               c(ecoval.translate("L_lake_morphol_B01_B01.01",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.02",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.0201",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.0202",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.0203",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.03",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.0301",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.0302",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.0303",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.04",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.0401",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.0402",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.0403",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.0404",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.0405",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.05",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.0501",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.0502",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.0503",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.0504",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.0505",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.0506",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.0507",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.0508",dict),
                                 ecoval.translate("L_lake_morphol_B01_B01.06",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_lake_morphol_B01",dict)
  Uferverbauung <- utility.endnode.discrete.create(
    name.node     = ecoval.translate("N_lake_morphol_shorline_stabilization",dict), 
    attrib.levels = attrib.levels,
    u             = c(1,
                      0.75,
                      0.75,
                      0.75,
                      0.75,
                      0.5,
                      0.5,
                      0.5,
                      0.5,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0, 
                      0, 
                      0, 
                      0, 
                      0, 
                      0, 
                      0, 
                      0, 
                      0, 
                      0), 
    utility       = FALSE,
    required      = FALSE,
    col           = if(is.na(col)) "black" else col,
    shift.levels  = 1)

  # node "Naturnahe Ufersaumvegetation":
  # ------------------------------------
 
  attrib.levels = data.frame("C06_Ufersaumvegetation_US"=
                               c(ecoval.translate("L_lake_morphol_C06_C06.01",dict),
                                 ecoval.translate("L_lake_morphol_C06_C06.02",dict),
                                 ecoval.translate("L_lake_morphol_C06_C06.03",dict),
                                 ecoval.translate("L_lake_morphol_C06_C06.04",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_lake_morphol_C06",dict)
  UfersaumVegetation <- utility.endnode.discrete.create(
    name.node     = ecoval.translate("N_lake_morphol_shore_stripe_vegetation",dict), 
    attrib.levels = attrib.levels,
    u             = c(1,
                      1,
                      0,
                      0), 
    utility       = FALSE,
    required      = TRUE,
    col           = if(is.na(col)) "green" else col)
    
  # node "Keine Siedlung, Gewerbe, Industrie":
  # ------------------------------------------
    
  attrib.levels = data.frame("C01_Siedlung_Gewerbe_Industrie_US"=
                               c(ecoval.translate("L_lake_morphol_C01_C01.01",dict),
                                 ecoval.translate("L_lake_morphol_C01_C01.02",dict),
                                 ecoval.translate("L_lake_morphol_C01_C01.03",dict),
                                 ecoval.translate("L_lake_morphol_C01_C01.0301",dict),
                                 ecoval.translate("L_lake_morphol_C01_C01.0302",dict),
                                 ecoval.translate("L_lake_morphol_C01_C01.0303",dict),
                                 ecoval.translate("L_lake_morphol_C01_C01.04",dict),
                                 ecoval.translate("L_lake_morphol_C01_C01.05",dict),
                                 ecoval.translate("L_lake_morphol_C01_C01.0501",dict),
                                 ecoval.translate("L_lake_morphol_C01_C01.0502",dict),
                                 ecoval.translate("L_lake_morphol_C01_C01.0503",dict),
                                 ecoval.translate("L_lake_morphol_C01_C01.0504",dict),
                                 ecoval.translate("L_lake_morphol_C01_C01.06",dict),
                                 ecoval.translate("L_lake_morphol_C01_C01.0601",dict),
                                 ecoval.translate("L_lake_morphol_C01_C01.0602",dict),
                                 ecoval.translate("L_lake_morphol_C01_C01.0603",dict),
                                 ecoval.translate("L_lake_morphol_C01_C01.0604",dict),
                                 ecoval.translate("L_lake_morphol_C01_C01.0605",dict),
                                 ecoval.translate("L_lake_morphol_C01_C01.07",dict),
                                 ecoval.translate("L_lake_morphol_C01_C01.0701",dict),
                                 ecoval.translate("L_lake_morphol_C01_C01.0702",dict),
                                 ecoval.translate("L_lake_morphol_C01_C01.0703",dict),
                                 ecoval.translate("L_lake_morphol_C01_C01.0704",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_lake_morphol_C01",dict)
  UferstreifenSiedlung <- utility.endnode.discrete.create(
    name.node     = ecoval.translate("N_lake_morphol_settlements_industry",dict), 
    attrib.levels = attrib.levels,
    u             = c(1,
                      0.75,
                      0.5,
                      0.5,
                      0.5,
                      0.5,
                      0.5,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0), 
    utility       = FALSE,
    required      = TRUE,
    col           = if(is.na(col)) "black" else col)
  
  # node "Keine Freizeitnutzung":
  # -----------------------------
    
  attrib.levels = data.frame("C02_Freizeitnutzung_US"=
                               c(ecoval.translate("L_lake_morphol_C02_C02.01",dict),
                                 ecoval.translate("L_lake_morphol_C02_C02.02",dict),
                                 ecoval.translate("L_lake_morphol_C02_C02.03",dict),
                                 ecoval.translate("L_lake_morphol_C02_C02.0301",dict),
                                 ecoval.translate("L_lake_morphol_C02_C02.0302",dict),
                                 ecoval.translate("L_lake_morphol_C02_C02.0303",dict),
                                 ecoval.translate("L_lake_morphol_C02_C02.0304",dict),
                                 ecoval.translate("L_lake_morphol_C02_C02.0305",dict),
                                 ecoval.translate("L_lake_morphol_C02_C02.0306",dict),
                                 ecoval.translate("L_lake_morphol_C02_C02.04",dict),
                                 ecoval.translate("L_lake_morphol_C02_C02.0401",dict),
                                 ecoval.translate("L_lake_morphol_C02_C02.0402",dict),
                                 ecoval.translate("L_lake_morphol_C02_C02.0403",dict),
                                 ecoval.translate("L_lake_morphol_C02_C02.0404",dict),
                                 ecoval.translate("L_lake_morphol_C02_C02.0405",dict),
                                 ecoval.translate("L_lake_morphol_C02_C02.0406",dict),
                                 ecoval.translate("L_lake_morphol_C02_C02.05",dict),
                                 ecoval.translate("L_lake_morphol_C02_C02.0501",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_lake_morphol_C02",dict)
  UferstreifenFreizeit <- utility.endnode.discrete.create(
    name.node     = ecoval.translate("N_lake_morphol_recreational_landuse",dict), 
    attrib.levels = attrib.levels,
    u             = c(1,
                      0.75,
                      0.5,
                      0.5,
                      0.5,
                      0.5,
                      0.5,
                      0.5,
                      0.5,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0,
                      0), 
    utility       = FALSE,
    required      = TRUE,
    col           = if(is.na(col)) "black" else col)
  
  # node "Keine Verkehrswege und -flaechen":
  # ----------------------------------------
  
  attrib.levels = data.frame("C03_Verkehrswege_Flaechen_US"=
                               c(ecoval.translate("L_lake_morphol_C03_C03.01",dict),
                                 ecoval.translate("L_lake_morphol_C03_C03.02",dict),
                                 ecoval.translate("L_lake_morphol_C03_C03.03",dict),
                                 ecoval.translate("L_lake_morphol_C03_C03.04",dict),
                                 ecoval.translate("L_lake_morphol_C03_C03.05",dict),
                                 ecoval.translate("L_lake_morphol_C03_C03.06",dict),
                                 ecoval.translate("L_lake_morphol_C03_C03.07",dict),
                                 ecoval.translate("L_lake_morphol_C03_C03.08",dict),
                                 ecoval.translate("L_lake_morphol_C03_C03.09",dict),
                                 ecoval.translate("L_lake_morphol_C03_C03.10",dict),
                                 ecoval.translate("L_lake_morphol_C03_C03.11",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_lake_morphol_C03",dict)
  UferstreifenVerkehr <- utility.endnode.discrete.create(
    name.node     = ecoval.translate("N_lake_morphol_traffic_areas",dict), 
    attrib.levels = attrib.levels,
    u             = c(1,
                      0.75,
                      0.5,
                      0.5,
                      0.25,
                      0.25,
                      0,
                      0,
                      0,
                      0,
                      0), 
    utility       = FALSE,
    required      = TRUE,
    col           = if(is.na(col)) "black" else col)
  
  # node "Keine Land- und Forstwirtschaft":
  # ---------------------------------------
  
  attrib.levels = data.frame("C04_Land_Forstw_Nutzung_US"=
                               c(ecoval.translate("L_lake_morphol_C04_C04.01",dict),
                                 ecoval.translate("L_lake_morphol_C04_C04.02",dict),
                                 ecoval.translate("L_lake_morphol_C04_C04.0201",dict),
                                 ecoval.translate("L_lake_morphol_C04_C04.0202",dict),
                                 ecoval.translate("L_lake_morphol_C04_C04.0203",dict),
                                 ecoval.translate("L_lake_morphol_C04_C04.0204",dict),
                                 ecoval.translate("L_lake_morphol_C04_C04.04",dict),
                                 ecoval.translate("L_lake_morphol_C04_C04.03",dict),
                                 ecoval.translate("L_lake_morphol_C04_C04.0301",dict),
                                 ecoval.translate("L_lake_morphol_C04_C04.0302",dict),
                                 ecoval.translate("L_lake_morphol_C04_C04.0303",dict),
                                 ecoval.translate("L_lake_morphol_C04_C04.0304",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_lake_morphol_C04",dict)
  UferstreifenLandwirtschaft <- utility.endnode.discrete.create(
    name.node     = ecoval.translate("N_lake_morphol_agriculture_forestry",dict), 
    attrib.levels = attrib.levels,
    u             = c(1,
                      0.75,
                      0.75,
                      0.75,
                      0.75,
                      0.75,
                      0.5,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0.25), 
    utility       = FALSE,
    required      = TRUE,
    col           = if(is.na(col)) "black" else col)
  
  # node "Keine Verbauung des Fliessgewaessers":
  # --------------------------------------------
  
  attrib.levels = data.frame("C05_Fliessgewaesserverbauung_US"=
                               c(ecoval.translate("L_lake_morphol_C05_C05.01",dict),
                                 ecoval.translate("L_lake_morphol_C05_C05.02",dict),
                                 ecoval.translate("L_lake_morphol_C05_C05.03",dict),
                                 ecoval.translate("L_lake_morphol_C05_C05.04",dict),
                                 ecoval.translate("L_lake_morphol_C05_C05.05",dict),
                                 ecoval.translate("L_lake_morphol_C05_C05.06",dict),
                                 ecoval.translate("L_lake_morphol_C05_C05.07",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_lake_morphol_C05",dict)
  Fliessgewaesserverbauung <- utility.endnode.discrete.create(
    name.node     = ecoval.translate("N_lake_morphol_river_control_structures",dict), 
    attrib.levels = attrib.levels,
    u             = c(1,
                      1,
                      0.5,
                      0.25,
                      0,
                      0,
                      0), 
    utility       = FALSE,
    required      = TRUE,
    col           = if(is.na(col)) "black" else col)
  
  # node "Keine Nutzung im Hinterlandstreifen":
  # -------------------------------------------
  
  attrib.levels = data.frame("D01_Nutzung_HL"=
                               c(ecoval.translate("L_lake_morphol_D01_D01.01",dict),
                                 ecoval.translate("L_lake_morphol_D01_D01.02",dict),
                                 ecoval.translate("L_lake_morphol_D01_D01.0201",dict),
                                 ecoval.translate("L_lake_morphol_D01_D01.0202",dict),
                                 ecoval.translate("L_lake_morphol_D01_D01.0203",dict),
                                 ecoval.translate("L_lake_morphol_D01_D01.0204",dict),
                                 ecoval.translate("L_lake_morphol_D01_D01.03",dict),
                                 ecoval.translate("L_lake_morphol_D01_D01.0301",dict),
                                 ecoval.translate("L_lake_morphol_D01_D01.0302",dict),
                                 ecoval.translate("L_lake_morphol_D01_D01.0303",dict),
                                 ecoval.translate("L_lake_morphol_D01_D01.0304",dict),
                                 ecoval.translate("L_lake_morphol_D01_D01.04",dict),
                                 ecoval.translate("L_lake_morphol_D01_D01.0401",dict),
                                 ecoval.translate("L_lake_morphol_D01_D01.0402",dict),
                                 ecoval.translate("L_lake_morphol_D01_D01.0403",dict),
                                 ecoval.translate("L_lake_morphol_D01_D01.0404",dict),
                                 ecoval.translate("L_lake_morphol_D01_D01.0405",dict),
                                 ecoval.translate("L_lake_morphol_D01_D01.0406",dict),
                                 ecoval.translate("L_lake_morphol_D01_D01.05",dict),
                                 ecoval.translate("L_lake_morphol_D01_D01.0501",dict),
                                 ecoval.translate("L_lake_morphol_D01_D01.0502",dict),
                                 ecoval.translate("L_lake_morphol_D01_D01.0503",dict),
                                 ecoval.translate("L_lake_morphol_D01_D01.0504",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_lake_morphol_D01",dict)
  HinterlandNutzung <- utility.endnode.discrete.create(
    name.node     = ecoval.translate("N_lake_morphol_human_landuse_fartherarea",dict), 
    attrib.levels = attrib.levels,
    u             = c(1,
                      0.75,
                      0.75,
                      0.75,
                      0.75,
                      0.75,
                      0.5,
                      0.5,
                      0.5,
                      0.5,
                      0.5,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0.25,
                      0,
                      0,
                      0,
                      0,
                      0), 
    utility       = FALSE,
    required      = TRUE,
    col           = if(is.na(col)) "black" else col)
  
  # node "Naturnahe Hinterland-Uebergangsvegetation":
  # -------------------------------------------------
  
  attrib.levels = data.frame("D02_Uebergangsvegetation_HL"=
                               c(ecoval.translate("L_lake_morphol_D02_D02.01",dict),
                                 ecoval.translate("L_lake_morphol_D02_D02.02",dict),
                                 ecoval.translate("L_lake_morphol_D02_D02.03",dict),
                                 ecoval.translate("L_lake_morphol_D02_D02.04",dict)))
  colnames(attrib.levels) <- ecoval.translate("A_lake_morphol_D02",dict)
  HinterlandVegetation <- utility.endnode.discrete.create(
    name.node     = ecoval.translate("N_lake_morphol_vegetation_bordertofartherarea",dict), 
    attrib.levels = attrib.levels,
    u             = c(1,
                      1,
                      0,
                      0), 
    utility       = FALSE,
    required      = TRUE,
    col           = if(is.na(col)) "green" else col)
  
  # construction of aggregation nodes:
  # ==================================
  
  # node "Naturnahe Flachwasserzone":
  # ---------------------------------
  
  Flachwasserzone <- utility.aggregation.create(
    name.node = ecoval.translate("N_lake_morphol_shallowwaterzone",dict), 
    nodes     = list(FlachwasserSohle,FlachwasserAnlagen), 
    name.fun  = "utility.aggregate.min", 
    par       = NA,
    required  = TRUE,
    col       = if(is.na(col)) "red" else col)
  
  # node "Naturnahe Uferlinie":
  # ---------------------------

  Uferlinie <- utility.aggregation.create(
    name.node     = ecoval.translate("N_lake_morphol_shoreline",dict), 
    nodes         = list(Fliessgewaesseranbindung,Uferverbauung),
    name.fun      = "utility.aggregate.min", 
    par           = c(1),
    required      = TRUE,
    col           = if(is.na(col)) "blue" else col)
  
  # node "Keine Nutzung im Uferstreifen":
  # -------------------------------------
  
  UferstreifenNutzung <- utility.aggregation.create(
    name.node     = ecoval.translate("N_lake_morphol_human_landuse_neararea",dict), 
    nodes         = list(UferstreifenSiedlung,UferstreifenFreizeit,UferstreifenVerkehr,UferstreifenLandwirtschaft,Fliessgewaesserverbauung),
    name.fun      = "utility.aggregate.min", 
    par           = c(1),
    required      = TRUE,
    col           = if(is.na(col)) "blue" else col)
  
  # node "Naturnaher Uferstreifen":
  # -------------------------------
  
  Uferstreifen <- utility.aggregation.create(
    name.node     = ecoval.translate("N_lake_morphol_nearshorearea",dict), 
    nodes         = list(UfersaumVegetation,UferstreifenNutzung),
    name.fun      = "utility.aggregate.bonusmalus", 
    par           = c(1,3/7,NA),
    required      = TRUE,
    col           = if(is.na(col)) "blue" else col)
  
  # node "Naturnaher Hinterlandstreifen":
  # -------------------------------------

  Hinterlandstreifen <- utility.aggregation.create(
    name.node = ecoval.translate("N_lake_morphol_farthershorearea",dict), 
    nodes     = list(HinterlandNutzung,HinterlandVegetation),
    name.fun  = "utility.aggregate.bonusmalus", 
    par       = c(1,NA,3/7),
    required  = TRUE,
    col       = if(is.na(col)) "red" else col)
  
  # node "Naturnahe Uferzone":
  # --------------------------
  
  Uferzone <- utility.aggregation.create(
    name.node = ecoval.translate("N_lake_morphol_shorearea",dict), 
    nodes     = list(Uferstreifen,Hinterlandstreifen),
    name.fun  = "utility.aggregate.bonusmalus", 
    par       = c(1,NA,-1),
    required  = TRUE,
    col       = if(is.na(col)) "blue" else col)
  
  # node "Naturnahe Oekomorphologie Seeufer":
  # -----------------------------------------
  
  SeeuferOekomorphologie <- utility.aggregation.create(
    name.node   = ecoval.translate("N_lake_morphol_lakeshoremorphology",dict), 
    nodes       = list(Flachwasserzone,Uferlinie,Uferzone), 
    name.fun    = "utility.aggregate.bonusmalus", 
    par         = c(1,1,2,0.5,-7/3,NA,NA),
    required    = TRUE,
    col         = if(is.na(col)) "blue" else col,
    add.arg.fun = "utility.aggregate.addsplitpower")
  
  return(SeeuferOekomorphologie)
}


# lake.morphol.2016.read.attrib
# =============================

# Funciton to read individual attributes from different files with different segmentation.
# Return value is a data frame with unified segmentation for all attributes.
# Peter Reichert 13.01.2017

lake.morphol.2016.read.attrib <- function(directory    = ".",
                                          language     = "English",
                                          dictionaries = NA,
                                          attrib.names = NA,
                                          col.names    = NA)
{
  # initialization:
    
  dict <- ecoval.dict(language,dictionaries)
  
  if ( is.na(attrib.names[1]) ) attrib.names <- c(ecoval.translate("A_lake_morphol_E01",dict),
                                                  ecoval.translate("A_lake_morphol_E02",dict),
                                                  ecoval.translate("A_lake_morphol_B02",dict),
                                                  ecoval.translate("A_lake_morphol_B01",dict),
                                                  ecoval.translate("A_lake_morphol_C06",dict),
                                                  ecoval.translate("A_lake_morphol_C01",dict),
                                                  ecoval.translate("A_lake_morphol_C02",dict),
                                                  ecoval.translate("A_lake_morphol_C03",dict),
                                                  ecoval.translate("A_lake_morphol_C04",dict),
                                                  ecoval.translate("A_lake_morphol_C05",dict),
                                                  ecoval.translate("A_lake_morphol_D01",dict),
                                                  ecoval.translate("A_lake_morphol_D02",dict))
  
  if ( is.na(col.names[1]) )    col.names    <- c(ecoval.translate("A_lake_morphol_datacol_from",dict),
                                                  ecoval.translate("A_lake_morphol_datacol_to",dict),
                                                  ecoval.translate("A_lake_morphol_datacol_code",dict),
                                                  ecoval.translate("A_lake_morphol_datacol_id",dict),
                                                  ecoval.translate("A_lake_morphol_datacol_comments",dict))
  
  # consistency checks:
  
  if ( sum(is.na(col.names[1:3])) > 0 )
  {
    print("*** please provide at least the first three column names")
    return(NA)
  }
  col.name.id <- "id.generated"; if ( length(col.names)>3 & !is.na(col.names[4]) ) col.name.id <- col.names[4]
  sup.col.names <- character(0); if ( length(col.names)>4 ) 
  {
    sup.col.names <- col.names[5:length(col.names)]
    sup.col.names <- sup.col.names[!is.na(sup.col.names)]
  }
  if ( length(attrib.names) == 1 )
  {
    print("*** please provide at least one attribute name")
    return(NA)
  }
  attrib.names <- attrib.names[!is.na(attrib.names)]
  
  # read list of tables of individual attributes data, complement missing columns with empty data:
  
  attrib.list <- list()
  files  <- list.files(directory)
  ids <- character(0)
  for ( i in 1:length(attrib.names) )
  {
    ind.file <- match(attrib.names[i],substr(files,1,nchar(attrib.names[i])))
    if ( is.na(ind.file) )
    {
      attrib.list[[attrib.names[i]]]  <- NA
      print(paste("*** attrib",attrib.names[i],"not found"))
    }
    else
    {
      attrib.tmp <- read.table(paste(directory,files[ind.file],sep="/"),
                               header=TRUE,sep=",",stringsAsFactors=FALSE,na.strings="")
      if ( nrow(attrib.tmp) == 0 )
      {
        print(paste("*** no data found for attrib",attrib.names[i]))
        attrib.list[[attrib.names[i]]] <- NA
      }
      else
      {
        cols <- colnames(attrib.tmp)
        not.found <- character(0)
        for ( col.name in col.names[!is.na(col.names)] ) 
        {
          if ( is.na(match(col.name,cols)) ) not.found <- c(not.found,col.name)
        }
        if ( length(not.found) > 0 )
        {
          print(paste("*** attrib",attrib.names[i],": colnames ",paste(not.found,collapse=","),"not found"),sep="")
        }
        if ( sum(is.na(match(col.names[1:3],colnames(attrib.tmp)))) > 0 )
        {
          attrib.list[[attrib.names[i]]] <- NA
        }
        else
        {
          attrib.list[[attrib.names[i]]] <- attrib.tmp[,col.names[1:3]]
          if ( !is.na(match(col.name.id,colnames(attrib.tmp))) )
          {
            attrib.list[[attrib.names[i]]] <- cbind(attrib.list[[attrib.names[i]]],attrib.tmp[,col.name.id])
          }
          else
          {
            id.gen <- data.frame(id.generated=rep("",nrow(attrib.tmp)),stringsAsFactors=FALSE)
            attrib.list[[attrib.names[i]]] <- cbind(attrib.list[[attrib.names[i]]],id.gen)
          }
          colnames(attrib.list[[attrib.names[i]]]) <- c(col.names[1:3],col.name.id)
          if ( length(sup.col.names) > 0 )
          {
            sup <- data.frame(matrix(NA,nrow=nrow(attrib.tmp),ncol=length(sup.col.names)),stringsAsFactors=FALSE)
            colnames(sup) <- sup.col.names
            ind <- !is.na(match(sup.col.names,colnames(attrib.tmp)))
            if ( sum(ind) > 0 )
            {
              sup[,sup.col.names[ind]] <- attrib.tmp[,sup.col.names[ind]]
            }
            attrib.list[[attrib.names[i]]] <- cbind(attrib.list[[attrib.names[i]]],sup)
          }
          ids <- unique(c(ids,attrib.list[[attrib.names[i]]][,col.name.id]))
        }
      }
    }
  }
  
  # extract split points:
  
  split.points <- list()
  for ( k in 1:length(ids) )
  {
    split.points[[k]] <- numeric(0)
    for ( i in 1:length(attrib.list) )
    {
      ind <- attrib.list[[i]][,col.name.id] == ids[k]
      if ( sum(ind) > 0 )
      {
        split.points[[k]] <- unique(c(split.points[[k]],attrib.list[[i]][ind,1],attrib.list[[i]][ind,2]))
      }
    }
    split.points[[k]] <- sort(split.points[[k]])
  }
  names(split.points) <- ids
  keep <- rep(TRUE,length(ids))
  for ( k in 1:length(ids) ) 
  { 
    if (length(split.points[[k]]) < 2 )
    {
      keep[k] <- FALSE
      print(paste("*** no reach with length > 0 defined for id",col.name.id,"=",ids[k],"- omitting  this id"))
    }
  }
  ids <- ids[keep]
  split.points <- split.points[keep]
  if ( length(ids) == 0 ) return(NA)
  start.reach <- numeric(0)
  end.reach   <- numeric(0)
  id.lake     <- character(0)
  for ( k in 1:length(split.points) )
  {
    start.reach <- c(start.reach,split.points[[k]][-length(split.points[[k]])])
    end.reach   <- c(end.reach,split.points[[k]][-1])
    id.lake     <- c(id.lake,rep(ids[k],length(split.points[[k]])-1))
  }
  
  # construct attribute table:
  
  attrib <- matrix(NA,nrow=length(start.reach),ncol=3+length(attrib.names)+length(sup.col.names))
  if ( length(ids) == 1 ) rownames(attrib) <- paste(start.reach,"-",end.reach)
  else                    rownames(attrib) <- paste(id.lake,"|",start.reach,"-",end.reach)
  colnames(attrib) <- c(col.name.id,col.names[1:2],attrib.names,sup.col.names)
  attrib <- data.frame(attrib)
  attrib[,1] <- id.lake
  attrib[,2] <- start.reach
  attrib[,3] <- end.reach
  for ( i in 1:nrow(attrib) )
  {
    id <- attrib[i,1]
    x  <- 0.5*(attrib[i,2]+attrib[i,3])
    for ( j in 1:length(attrib.names) )
    {
      if ( length(dim(attrib.list[[attrib.names[j]]])) == 2 )
      {
        val <- attrib.list[[attrib.names[j]]]
        if ( nrow(val) > 0 )
        {
          k <- id == val[,col.name.id] & 
            ( ( x >= val[,col.names[1]] & x <= val[,col.names[2]] ) |
              ( x <= val[,col.names[1]] & x >= val[,col.names[2]] ) )
          if ( sum(k) > 0 )
          {
            code <- unique(val[k,col.names[3]])
            if ( length(code) > 1 )
            {
              print(paste("*** Code of attribute",attrib.names[j],"for",col.name.id,id,"at",x,"is not unique:",paste(code,collapse=",")))
              attrib[i,attrib.names[j]] <- NA
            }
            else
            {
              attrib[i,attrib.names[j]] <- code
            }
            if ( length(sup.col.names) > 0 )
            {
              rs <- which(k)
              for ( l in 1:length(sup.col.names) )
              {
                comments <- paste(attrib.names[j],":",sep="")
                n0 <- nchar(comments)
                for ( r in rs )
                {
                  if ( !is.na(val[r,sup.col.names[l]]) & nchar(as.character(val[r,sup.col.names[l]])) > 0 )
                  {
                    if ( nchar(comments) == n0 ) comments <- paste(comments,val[r,sup.col.names[l]])
                    else                         comments <- paste(comments,val[r,sup.col.names[l]],sep=",")
                  }
                }
                if ( nchar(comments) > n0 )
                {
                  if ( is.na(attrib[i,sup.col.names[l]]) ) attrib[i,sup.col.names[l]] <- comments
                  else                                     attrib[i,sup.col.names[l]] <- paste(attrib[i,sup.col.names[l]],comments,sep=" | ")
                }
              }
            }
          }
        }
      }
    }
  }
  
  # return attribute table:  
  
  return(attrib)
}


# lake.morphol.2016.get.segments (for internal use only)
# ======================================================

# function to extract lake ids and start and end coordinates from row names:

lake.morphol.2016.get.segments <- function(x)
{
  n <- length(x)
  if ( n == 0 ) return(NA)
  id    <- rep("",n)
  start <- rep(NA,n)
  end   <- rep(NA,n)
  
  x.splitted <- strsplit(x,split=" | ",fixed=TRUE)
  
  for ( i in 1:n )
  {
    if ( length(x.splitted[[i]]) > 1 )
    {
      id[i] <- x.splitted[[i]][1]
      x.splitted[[i]] <- x.splitted[[i]][2]
    }
    x.resplitted <- strsplit(x.splitted[[i]][1],split=" - ",fixed=TRUE)[[1]]
    if ( length(x.resplitted) > 1 )
    {
      start[i] <- as.numeric(x.resplitted[1])
      end[i]   <- as.numeric(x.resplitted[2])
    }
  }
  
  return(data.frame(id=id,start=start,end=end,stringsAsFactors=FALSE))
}

# lake.morphol.2016.plot.val.spatial
# ==================================

lake.morphol.2016.plot.val.spatial <- 
  function(u,
           uref      = NA,
           nodes     = NA,
           main      = "",
           col       = c("red","orange","yellow","green","blue"),
           gridlines = FALSE,
           ...)
{
  # define offsets for positioning of uref and text:
  
  delta.uref <- 0.075
  delta.text <- 0.2
  
  # determine columns:
  
  if ( is.na(nodes[1]) ) nodes <- colnames(u)
  n <- length(nodes)
  if ( n== 0 ) return()
  
  # determine segments:
  
  seg.u <- lake.morphol.2016.get.segments(rownames(u))
  ids <- unique(seg.u$id)
  uref.available <- length(dim(uref)) == 2
  if ( uref.available )
  {
    seg.uref <- lake.morphol.2016.get.segments(rownames(uref))
    ids <- unique(ids,seg.uref$id)
  }

  # loop over different lake ids:
  
  
  for ( id in ids )
  {
    ind.id <- which(seg.u$id==id) 
    x.min  <- min(c(seg.u$start[ind.id],seg.u$end[ind.id]),na.rm=TRUE)
    x.max  <- max(c(seg.u$start[ind.id],seg.u$end[ind.id]),na.rm=TRUE)
    if ( uref.available )
    {
      ind.uref.id <- which(seg.uref$id==id)
      x.min <- min(c(x.min,seg.uref$start[ind.uref.id],seg.uref$end[ind.uref.id]),na.rm=TRUE)
      x.max <- max(c(x.max,seg.uref$start[ind.uref.id],seg.uref$end[ind.uref.id]),na.rm=TRUE)
    }

    # open plot:
  
    plot(numeric(0),numeric(0),xlim=c(x.min,x.max),ylim=c(0.5,length(nodes)+0.5),
         type="n",xlab="",ylab="",main=paste(main,id),...)
    
    # plot gridlines:
    
    if ( gridlines )
    {
      if ( !uref.available )
      {
        x <- unique(c(seg.u$start[ind.id],seg.u$end[ind.id]))
      }
      else
      {
        x <- unique(c(seg.u$start[ind.id],seg.u$end[ind.id],
                      seg.uref$start[ind.uref.id],seg.uref$end[ind.uref.id]))
      }
      x <- x[!is.na(x)]
      abline(v=x,lty="dotted")
    }
    
    # plot valuations:
    
    for ( j in 1:n ) 
    {
      text(0.5*(x.min+x.max),n+1-j+delta.text,nodes[j],adj=c(0.5,0.0))
      
      if ( !uref.available )
      {
        offset <- 0
      }
      else
      {
        offset <- delta.uref
      }
      for ( i in 1:length(ind.id) )
      {
        if( !is.na(seg.u$start[ind.id[i]]) & 
            !is.na(seg.u$end[ind.id[i]]) &
            !is.na(u[ind.id[i],nodes[j]]) )
        {
          lines(x    = c(seg.u$start[ind.id[i]],seg.u$end[ind.id[i]]),
                y    = (n+1-j)*c(1,1) + offset,
                col  = utility.get.colors(u[ind.id[i],nodes[j]],col),
                lend = 1,
                ...)
        }
      }
      if ( uref.available )
      {
        for ( i in 1:length(ind.uref.id) )
        {
          if ( !is.na(seg.uref$start[ind.uref.id[i]]) &
               !is.na(seg.uref$end[ind.uref.id[i]]) &
               !is.na(uref[ind.uref.id[i],nodes[j]]) )
          {
            lines(x    = c(seg.uref$start[ind.uref.id[i]],seg.uref$end[ind.uref.id[i]]),
                  y    = (n+1-j)*c(1,1) - offset,
                  col  = utility.get.colors(uref[ind.uref.id[i],nodes[j]],col),
                  lend = 1,
                ...)
          }
        }
      }
    }
  }
}


# lake.morphol.2016.overlap (local use only)
# ==========================================

lake.morphol.2016.overlap <- function(x1,x2,y1,y2)
{
  x.min <- min(x1,x2)
  x.max <- max(x1,x2)
  y.min <- min(y1,y2)
  y.max <- max(y1,y2)
  if ( x.max <= y.min | x.min >= y.max) return(0)
  return(min(x.max,y.max)-max(x.min,y.min))
}


# lake.morphol.2016.aggregate.val.spatial
# =======================================

lake.morphol.2016.aggregate.val.spatial <- function(u,breakpoints)
{

  # determine segments:
  
  seg.u <- lake.morphol.2016.get.segments(rownames(u))
  ids <- unique(seg.u$id)

  # initializations and consistency checks:

  breaks <- breakpoints
  if ( is.vector(breaks,mode="numeric") ) { breaks <- list(); breaks[[1]] <- breakpoints }
  if ( length(breaks) != length(ids) )
  {
    print(paste("*** list of vectors of breakpoints (",length(breaks),
                ") does not have as many elements as there are lake ids (",
                length(ids),")",sep=""))
    return(NA)
  }
  
  # loop over different lake ids:
  
  u.aggregated <- matrix(NA,ncol=ncol(u),nrow=0)
  colnames(u.aggregated) <- colnames(u)
  u.aggregated <- as.data.frame(u.aggregated)
  for ( k in 1:length(ids) )
  {
    id <- ids[k]
    ind.id <- which(seg.u$id==id) 

    if ( length(ind.id) > 0 )
    {
      x.start <- seg.u$start[ind.id]
      x.end   <- seg.u$end[ind.id]
      
      x.min <- min(c(x.start,x.end),na.rm=TRUE)
      x.max <- max(c(x.start,x.end),na.rm=TRUE)
    
      breaks.k <- sort(unique(breaks[[k]]))
      if ( length(breaks.k) > 1 )
      {
        y.start <- breaks.k[-length(breaks.k)]
        y.end   <- breaks.k[-1]
      
        u.aggregated.k <- matrix(NA,ncol=ncol(u),nrow=length(y.start))
        colnames(u.aggregated.k) <- colnames(u)
        if ( length(ids) > 1 & ids[1]!="" ) rownames(u.aggregated.k) <- paste(id," | ",y.start," - ",y.end,sep="")
        else                                rownames(u.aggregated.k) <- paste(y.start," - ",y.end,sep="")
        u.aggregated.k <- as.data.frame(u.aggregated.k)

        # determine overlapping relative lengths:
        
        weights <- matrix(NA,nrow=length(x.start),ncol=length(y.start))
        for ( i in 1:length(x.start) )
        {
          for ( j in 1:length(y.start) )
          {
            weights[i,j] <- lake.morphol.2016.overlap(y.start[j],y.end[j],
                                                      x.start[i],x.end[i])/(y.end[j]-y.start[j])
          }
        }
        sums <- apply(weights,2,sum) 
        for ( i in 1:ncol(weights) ) if ( sums[i] < 0.99 ) weights[,i] <- NA
        
        # calcuate weighted averages of values:
        
        for ( i in 1:length(y.start) )
        {
          for ( j in 1:ncol(u) )
          {
            ind <- which(!is.na(u[ind.id,j]))
            if ( length(ind) == 0 )
            {
              u.aggregated.k[i,j] <- NA
            }
            else
            {
              sum.w.available <- sum(weights[ind,i])
              if ( is.na(sum.w.available) | !sum.w.available > 0 )
              {
                u.aggregated.k[i,j] <- NA
              }
              else
              {
                u.aggregated.k[i,j] <- 0
                for ( l in ind )
                {
                  u.aggregated.k[i,j] <- u.aggregated.k[i,j] + weights[l,i] * u[ind.id[l],j]
                }
                u.aggregated.k[i,j] <- u.aggregated.k[i,j]/sum.w.available
              }
            }
          }
        }
        u.aggregated <- rbind(u.aggregated,u.aggregated.k)
      }
    }
  }
  
  return(u.aggregated)
}

