
# ========================================================================================
# This script contains functions to generate fact-sheet PDFs for the MSK macrophyte module
# 
# Authors:
# Christian Michel and Peter Reichert, EAWAG, Duebendorf, Switzerland
# =======================================================================================


# Functions to resize images
# ==========================

resize.img.color <- function(img,height,width)
{
  new_img <- apply(img,2,function(y){return (spline(y,n=height)$y)})
  new_img <- t(apply(new_img,1,function(y){return (spline(y,n=width)$y)}))
  new_img[new_img < 0] = 0
  new_img[new_img > 1] = 1
  return (new_img)
}

resize.img <- function(img,height,width)
{
  if ( length(dim(img)) == 2 ) return(resize.img.color(img,height,width))
  if ( length(dim(img)) == 3 )
  {
    colors <- dim(img)[3]
    new_img <- array(dim=c(height,width,colors))
    for ( i in 1:colors )
    {
      new_img[,,i] <- resize.img.color(img[,,i],height,width)
    }
    return(new_img)
  }
  return(img)
}

# Function for site information A4 PDF
# ====================================

msk.macrophytes.2017.doc.site <- function(res,row.no,pic.folder)
{
  # create dictionary
  dict <- ecoval.dict(res$language,res$dictionaries)
  
  # Check data availability and extract data for plotting
  if( is.na(match("data.site", names(res))) ) return()
  if( row.no  <= 0 | row.no > nrow(res$data.site) )    return()
  data.site <- res$data.site
  
  # get strings for plotting river types
  macro.types <- c(ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict),
                   ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict),
                   ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict),
                   ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict),
                   ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict),
                   ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict),
                   ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict),
                   ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict))
  
  moss.types <- c(ecoval.translate("L_macrophytes_rivertype_class_smallbryophyte",dict),
                  ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict),
                  ecoval.translate("L_macrophytes_rivertype_class_largebryophyte",dict),
                  ecoval.translate("L_macrophytes_rivertype_class_verylargebryophyte",dict))
  
  # original scheme type
  type.scheme.orig <- ""

  if(  !is.na(match("types.scheme.obs",names(res))) ) {
    type.scheme.orig <- res$types.scheme.obs[row.no]

    type.scheme.orig <- paste(ifelse(is.na(type.scheme.orig),"",type.scheme.orig),
                              paste("(",ecoval.translate("R_macrophytes_doc_typeorig",dict),")",sep=""))
    
  }
  
  # final scheme type
  type.scheme.final <- ""
  if(  !is.na(match("types.scheme.final",names(res))) ) {
    type.scheme.final <- res$types.scheme.final[row.no]
    type.scheme.final <- paste(ifelse(is.na(type.scheme.final), "",type.scheme.final),
                               paste("(",ecoval.translate("R_macrophytes_doc_typeplaus",dict),")",sep=""))
  }
  
  # original valuation type
  type.val.orig <- ""
  assess.orig <- NA
  if(  !is.na(match("types.val.obs",names(res))) ) {
    type.val.orig <- res$types.val.obs[row.no]
    
    if( !is.na(match("val",names(res))) & !is.na(type.val.orig) ) {
      
      if( !is.na(match(type.val.orig, macro.types)) ) {
        assess.orig <- res$val[row.no,ecoval.translate("N_macrophytes_goodstate",dict)]
      }
      
      if( !is.na(match(type.val.orig, moss.types)) ) {
        assess.orig <- res$val[row.no,paste(ecoval.translate("N_macrophytes_goodstate",dict),ecoval.translate("N_macrophytes_bryophytesriver",dict))]
      }
    }
    
    type.val.orig <- paste(ifelse(is.na(type.val.orig), "",type.val.orig),
                           paste("(",ecoval.translate("R_macrophytes_doc_typeorig",dict),")",sep=""))
    
  }
  
  # final valuation type
  type.val.final <- ""
  assess.final <- NA
  if(  !is.na(match("types.val.final",names(res))) ) {
    type.val.final <- res$types.val.final[row.no]
    
    if( !is.na(match("val.final",names(res))) & !is.na(type.val.final) ) {
      
      if( !is.na(match(type.val.final, macro.types)) ) {
        assess.final <- res$val.final[row.no,ecoval.translate("N_macrophytes_goodstate",dict)]
      }
      
      if( !is.na(match(type.val.final, moss.types)) ) {
        assess.final <- res$val.final[row.no,paste(ecoval.translate("N_macrophytes_goodstate",dict),ecoval.translate("N_macrophytes_bryophytesriver",dict))]
      }
    }
    
    type.val.final <- paste(ifelse(is.na(type.val.final), "",type.val.final),
                            paste("(",ecoval.translate("R_macrophytes_doc_typeplaus",dict),")",sep=""))
  }
  
  
  # define colors for msk quality classes
  msk.colors <- c("white",utility.calc.colors(n = 5))
  
  # create function to check if parameter column names are defined in dictionary (i.e. != "") and present in data, else
  # NA is returned for plotting
  get.value <- function(data.plot, row.no, colname) {
    value.plot <- NA
    if ( length(row.no)>0 & length(colname)>0 )
    {
      if ( row.no >= 1 & row.no <= nrow(data.plot) & colname != "" & sum(colname == colnames(data.plot)) != 0 ) value.plot <- data.plot[row.no,colname]
    }
    return(value.plot)
  }
  
  # create function to translate continous valuation in quality class
  calc.class <- function (u, classes) 
  {
    class.ind <- 1 + floor(u * length(classes) + 1e-15)
    class.ind <- ifelse(class.ind > length(classes), length(classes), class.ind)
    class <- classes[class.ind]
    class <- ifelse(is.na(class.ind), 0, class)
    return(class)
  }
  
  # set expansion factors for fonts in plot
  cex.title   <- 1.5
  cex.text    <- 1.2
  
  # create plot device with layout
  layout.mat.site <- matrix(c(0,1,1,0,
                              0,2,2,0,
                              0,3,4,0,
                              0,5,5,0,
                              0,6,6,0), ncol = 4, nrow = 5, byrow = T)
  layout.mat.widths  <- c(1.0,6.5,9.5,1.0)
  layout.mat.heights <- c(1.5,1.75,10,13,1.5)
  
  axis.info <- FALSE
  
  layout.plot <- layout(layout.mat.site,
                        widths = layout.mat.widths,
                        heights = layout.mat.heights,
                        respect = FALSE)
  
  par(mai = c(0,0,0,0))
  
  ## HEADER
  plot(0, 0, axes = axis.info, type="n", xaxs="i", yaxs="i", xlim=c(0,10), ylim=c(0,10))
  text(x = 0, y = 5, labels = ecoval.translate("R_macrophytes_doc_site",dict), font = 2, cex = cex.title, pos=4)
  abline(h = 3)
  
  ## WINDOW 2 - Basic information of sampling event as header in grey box
  plot(0, 0, axes = axis.info, type="n", xaxs="i", yaxs="i", xlim=c(0,10), ylim=c(0,10))
  rect(0,0.5,10,9.5, col = "grey85", border = NA)
  
  # Line 1
  text(x = 0, y = 7.5,   labels =  paste(ecoval.translate("R_macrophytes_doc_codesite",dict),":",sep=""), cex = cex.text, pos=4, font=2)
  value.plot <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_siteid",dict))
  text(x = 1.4, y = 7.5, labels =  ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
  
  text(x = 2.7, y = 7.5, labels =  paste(ecoval.translate("R_macrophytes_doc_canton",dict),":",sep=""), cex = cex.text, pos=4, font = 2)
  value.plot <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_canton",dict))
  text(x = 4, y = 7.5,   labels = ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
  
  text(x = 6.5, y = 7.5,   labels =  paste(ecoval.translate("R_macrophytes_doc_coordinates",dict),":",sep=""), cex = cex.text, pos=4, font = 2)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_coordinatex_swissgrid",dict))
  value.plot1 <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_coordinatey_swissgrid",dict))
  text(x = 8.3, y = 7.5, labels =  paste(ifelse(is.na(value.plot), "", value.plot),
                                       ifelse(is.na(value.plot1), "", value.plot1), sep = " / "),
       cex = cex.text, pos=4)
  
  # Line 2
  text(x = 0, y = 5,   labels =  paste(ecoval.translate("R_macrophytes_doc_river",dict),":",sep=""), cex = cex.text, pos=4, font=2)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_waterbody",dict))
  text(x = 1.4, y = 5, labels = ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
  
  text(x = 2.7, y = 5, labels =  paste(ecoval.translate("R_macrophytes_doc_location",dict),":",sep=""), cex = cex.text, pos=4, font = 2)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_location",dict))
  text(x = 4, y = 5,   labels =  ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)

  # Line 3
  text(x = 0, y = 2.5,   labels =  paste(ecoval.translate("R_macrophytes_doc_samplingdate",dict),":",sep=""), cex = cex.text, pos=4, font=2)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_samplingdate",dict))
  text(x = 1.4, y = 2.5, labels = ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
  
  text(x = 2.7, y = 2.5, labels =  paste(ecoval.translate("R_macrophytes_doc_observer",dict),":",sep=""), cex = cex.text, pos=4, font = 2)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_operator",dict))
  text(x = 4, y = 2.5,   labels =  ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
  
  text(x = 6.5, y = 2.5,   labels =  paste(ecoval.translate("R_macrophytes_doc_sectionlength",dict),":",sep=""), cex = cex.text, pos=4, font = 2)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_lengthsection_m",dict))
  text(x = 8.3, y = 2.5, labels =  ifelse(is.na(value.plot), "", paste(value.plot, "m")), cex = cex.text, pos=4)
  
  ## WINDOW 3 - Site parameters
  plot(0, 0, axes = axis.info, type="n", xaxs="i", yaxs="i", xlim=c(0,5), ylim=c(0,10))
  text(x = 0, y = 9, labels =  ecoval.translate("R_macrophytes_doc_siteparameters",dict), col = "dodgerblue", cex = cex.title, pos=4, font=2)
  
  # typology parameters
  x.start <- 0.1
  y.pos <- 8.25 - 0.5 * seq(0,20,1)
  x.pos <- 2.2
  
  text(x = x.start, y = y.pos[1], labels = paste(ecoval.translate("R_macrophytes_doc_discharge",dict),":",sep=""), font = 2, cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_discharge_lpers",dict))
  text(x = x.pos,   y = y.pos[1], labels = ifelse(is.na(value.plot), "", paste(round(value.plot,0), "l/s")), font = 2, cex = cex.text, pos=4)
  
  text(x = x.start+0.1, y = y.pos[2], labels = paste(ecoval.translate("R_macrophytes_doc_dischargesource",dict),":",sep=""), col = "grey60", cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_discharge_source",dict))
  text(x = x.pos,       y = y.pos[2], labels = ifelse(is.na(value.plot), "", value.plot), col = "grey60", cex = cex.text, pos=4)
  
  text(x = x.start, y = y.pos[3], labels = paste(ecoval.translate("R_macrophytes_doc_slope",dict),":",sep=""), font = 2, cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_slope_percent",dict))
  text(x = x.pos,   y = y.pos[3], labels = ifelse(is.na(value.plot), "", paste(round(value.plot,2), "%")), font = 2, cex = cex.text, pos=4)
  
  text(x = x.start+0.1, y = y.pos[4], labels = paste(ecoval.translate("R_macrophytes_doc_slopesource",dict),":",sep=""), col = "grey60", cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_slope_source",dict))
  text(x = x.pos,       y = y.pos[4], labels = ifelse(is.na(value.plot), "", value.plot), col = "grey60", cex = cex.text, pos=4)
  
  text(x = x.start, y = y.pos[5], labels = paste(ecoval.translate("R_macrophytes_doc_meandepth",dict),":",sep=""), font = 2, cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_waterdepth_m",dict))
  text(x = x.pos,   y = y.pos[5], labels = ifelse(is.na(value.plot), "", paste(formatC(round(value.plot,2),format="f",digits=2), "m")), font = 2, cex = cex.text, pos=4)
  
  text(x = x.start, y = y.pos[6], labels = paste(ecoval.translate("R_macrophytes_doc_fractstones",dict),":",sep=""), font = 2, cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_coarsegravel_percent",dict))
  text(x = x.pos,   y = y.pos[6], labels = ifelse(is.na(value.plot), "", paste(round(value.plot,1), "%")), font = 2, cex = cex.text, pos=4)
  
  text(x = x.start, y = y.pos[7], labels = paste(ecoval.translate("R_macrophytes_doc_shading",dict),":",sep=""), font = 2, cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_shading_percent",dict))
  text(x = x.pos,  y = y.pos[7], labels = ifelse(is.na(value.plot), "", paste(round(value.plot,1), "%")), font = 2, cex = cex.text, pos=4)
  
  # additional parameters recorded in the field
  text(x = x.start, y = y.pos[9], labels =  paste(ecoval.translate("R_macrophytes_doc_elevation",dict),":",sep=""), cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_elevation_m_asl",dict))
  text(x = x.pos,   y = y.pos[9], labels = ifelse(is.na(value.plot), "", paste(round(value.plot,1), "m")),
       cex = cex.text, pos=4)
  
  text(x = x.start, y = y.pos[10], labels = paste(ecoval.translate("R_macrophytes_doc_bedwidth",dict),":",sep=""), cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_morphol_riverbed_width_m",dict))
  text(x = x.pos, y = y.pos[10], labels =  ifelse(is.na(value.plot), "", paste(round(value.plot,1), "m")),
       cex = cex.text, pos=4)
  
  text(x = x.start, y = y.pos[11], labels = paste(ecoval.translate("R_macrophytes_doc_wettedwidth",dict),":",sep=""), cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_wettedwidth_m",dict))
  text(x = x.pos,   y = y.pos[11], labels =  ifelse(is.na(value.plot), "", paste(round(value.plot,1), "m")), cex = cex.text, pos=4)
  
  # additional facultative parameters (e.g. from swiss river typology)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_riverorder",dict))
  text(x = x.start, y = y.pos[13], labels =  paste(ecoval.translate("R_macrophytes_doc_streamorder",dict),":",sep=""), cex = cex.text, pos=4)
  text(x = x.pos,   y = y.pos[13], labels = ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
  
  text(x = x.start, y = y.pos[14], labels =  paste(ecoval.translate("R_macrophytes_doc_flowregimetype",dict),":",sep=""), cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_regimetype",dict))
  text(x = x.pos,   y = y.pos[14], labels =  ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
  
  text(x = x.start, y = y.pos[15], labels =  paste(ecoval.translate("R_macrophytes_doc_biogeographicregion",dict),":",sep=""), cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_biogeographicregion",dict))
  text(x = x.pos,   y = y.pos[15], labels =  ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
  
  text(x = x.start, y = y.pos[16], labels =  paste(ecoval.translate("R_macrophytes_doc_geology",dict),":",sep=""), cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_geology",dict))
  text(x = x.pos,   y = y.pos[16], labels =  ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
  
  ## WINDOW 4 - RIVER TYPE AND PICTURE
  
  # Write river type and picture in window 4 (2 column row)
  # Add river-types
  plot(0, 0, axes = axis.info, type="n", xaxs="i", yaxs="i", xlim=c(0,5), ylim=c(0,10))
  
  # Add scheme river-types
  text(x = 0.1, y = 8.70, labels = paste(ecoval.translate("R_macrophytes_doc_typescheme",dict),":",sep=""), cex = cex.text, font = 2, pos=4)
  
  if ( !is.na(type.scheme.orig) & nchar(type.scheme.orig) > 0 ) # check "nchar" kann evtl. noch entfernt werden!
  {
    # text(x = 0.1, y = 8.60, labels = "Schema:", cex = cex.text, pos=4)
    # char.ext <- 0.092
    
    if ( !is.na(type.scheme.final) & nchar(type.scheme.final) > 0 ) # check "nchar" kann evtl. noch entfernt werden!
    {
      text(x = 1.30, y = 8.70, labels = paste(type.scheme.orig,type.scheme.final, sep = "     "), cex = cex.text, pos=4)
    }
    else
    {
      text(x = 1.30, y = 8.70, labels = type.scheme.orig, cex = cex.text, pos=4)
    }
  }
  
  # Add valuation types
  if ( !is.na(type.val.orig) & nchar(type.val.orig) > 0 ) # check "nchar" kann evtl. noch entfernt werden!
  {
    text(x = 0.1, y = 8.25, labels = paste(ecoval.translate("R_macrophytes_doc_typevaluation",dict),":",sep=""), font = 2, cex = cex.text, pos=4)
    char.ext <- 0.092
    
    if ( !is.na(type.val.final) & nchar(type.val.final) > 0 )
    {
      
      if( !is.na(assess.orig) ) {
        x2.orig <- nchar(type.val.orig)*char.ext
        class <- calc.class(assess.orig, classes = c(1,1,2,2,3,3,4,4,5,5))
        rect(1.31,8.10,1.25+x2.orig,8.47, col = msk.colors[class+1], border = NA)
      }
      
      if( !is.na(assess.final) ) {
        x2.orig <- nchar(type.val.orig)*char.ext
        x2.final <- nchar(type.val.final)*char.ext
        class <- calc.class(assess.final, classes = c(1,1,2,2,3,3,4,4,5,5))
        rect(1.25+x2.orig+1.1*char.ext,8.10,1.20+x2.orig+char.ext+x2.final,8.47, col = msk.colors[class+1], border = NA)
      }
      
      text(x = 1.30, y = 8.25, labels = paste(type.val.orig,type.val.final, sep = "     "), cex = cex.text, pos=4)
    }
    else
    {
      if( !is.na(assess.orig) ) {
        x2.orig <- nchar(type.val.orig)*char.ext
        class <- calc.class(assess.orig, classes = c(1,1,2,2,3,3,4,4,5,5))
        rect(1.30,8.10,1.30+x2.orig,8.47, col = msk.colors[class+1], border = NA)
      }
      
      text(x = 1.30, y = 8.25, labels = type.val.orig, cex = cex.text, pos=4)
    }
  }
  
  # Add picture
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_picture1",dict))
  pic <- value.plot
  pic.ext <-  c(".jpg", ".JPG", ".jpeg", ".JPEG")
  pic.test <- paste(pic, pic.ext, sep="")  %in% list.files(pic.folder)
  
  if( !is.na(match(TRUE,pic.test)) ) {
    pic <- readJPEG(paste(pic.folder,"/", pic, pic.ext[match(TRUE,pic.test)], sep = ""), native = FALSE) # INPUT: Maybe open to other picture formats?
    aspect <- ncol(pic)/nrow(pic)
    new_height <- 780
    new_width  <- round(new_height*aspect)
    if ( new_height < nrow(pic) ) pic <- resize.img(pic,new_height,new_width)

    if(aspect > 1) {
      rasterImage(pic,0.15,7.75-yinch(3.8/aspect),0.2+xinch(3.8),7.9)
      text(x = 0.1, y = 7.5-yinch(3.8/aspect), labels =  paste(paste(ecoval.translate("R_macrophytes_doc_filepicture",dict),":",sep=""), ifelse(is.na(value.plot), "", value.plot)),
           cex = 0.9, col = "darkgrey", pos=4)
    }
    
    if(aspect <= 1) {
      rasterImage(pic,0.15,7.75-yinch(3),0.2+xinch(3*aspect),7.9)
      text(x = 0.1, y = 7.5-yinch(3.8/aspect), labels =  paste(paste(ecoval.translate("R_macrophytes_doc_filepicture",dict),":",sep=""), ifelse(is.na(value.plot), "", value.plot)),
           cex = 0.9, col = "darkgrey", pos=4)
    }
  } else {
    text(x = 0.1, y = 4.125, labels = paste("*",ecoval.translate("R_macrophytes_doc_picturemissing",dict),"*"), col = "grey60", cex = cex.title, pos=4)
  }
  

  ## WINDOW 5 - Ecomorphology, Appearance and additional information on catchment area and hydrology
  
  # General coordinates for plotting in this window
  x.start <- 0.1
  y.pos <- 8.71 - 0.31 * seq(0,30,1)
  para.small <- 0.31/2
  x.pos <- 2.3
  
  # Open plot device
  plot(0, 0, axes = axis.info, type="n", xaxs="i", yaxs="i", xlim=c(0,10),ylim=c(0,10))
  
  # Additional site information
  text(x = 0, 9.25, labels = ecoval.translate("R_macrophytes_doc_complementarysiteparameters",dict), col = "dodgerblue", cex = cex.title, font = 2, pos=4)
  
  text(x = x.start,   y = y.pos[1], labels = ecoval.translate("R_macrophytes_doc_dischargehydrom",dict), font = 2,  cex = cex.text, pos=4)
  
  text(x = x.start,   y = y.pos[2], labels = paste(ecoval.translate("R_macrophytes_doc_dischargehydrom_avg",dict),":",sep=""), cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_discharge_hydrometricavg_lpers",dict))
  text(x = x.pos,     y = y.pos[2], labels = ifelse(is.na(value.plot), "", paste(round(value.plot,0),"l/s")), cex = cex.text, pos=4)
  
  text(x = x.start,   y = y.pos[3], labels = paste(ecoval.translate("R_macrophytes_doc_dischargehydrom_day",dict),":",sep=""), cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_discharge_hydrometricday_lpers",dict))
  text(x = x.pos,     y = y.pos[3], labels = ifelse(is.na(value.plot), "", paste(round(value.plot,0), "l/s")),cex = cex.text, pos=4)
  
  text(x = x.start+(x.pos*2.5), y = y.pos[1], labels = ecoval.translate("R_macrophytes_doc_waterdepth",dict), font = 2,  cex = cex.text, pos=4)
  
  text(x = x.start+(x.pos*2.5), y = y.pos[2], labels = paste(ecoval.translate("R_macrophytes_doc_waterdepth_daymean",dict),":",sep=""), cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_waterdepth_dayavg_m",dict))
  text(x = x.start+(x.pos*3.2), y = y.pos[2], labels = ifelse(is.na(value.plot), "", paste(formatC(round(value.plot,2),format="f",digits=2), "m")),cex = cex.text, pos=4)
  
  text(x = x.start+(x.pos*2.5), y = y.pos[3], labels = paste(ecoval.translate("R_macrophytes_doc_waterdepth_daymax",dict),":",sep=""), cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_waterdepth_daymax_m",dict))
  text(x = x.start+(x.pos*3.2), y = y.pos[3], labels = ifelse(is.na(value.plot), "", paste(formatC(round(value.plot,2),format="f",digits=2), "m")),cex = cex.text, pos=4)
  
  # Substratstabilitaet
  class.names <- c("",
                   ecoval.translate("R_macrophytes_doc_substratestability_immobile",dict),
                   ecoval.translate("R_macrophytes_doc_substratestability_slightlymobile",dict),
                   ecoval.translate("R_macrophytes_doc_substratestability_mobile",dict))
  text(x = x.start,   y = y.pos[4]-para.small, labels = paste(ecoval.translate("R_macrophytes_doc_substratestability",dict),":",sep=""), font = 2, cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_substratestability_class",dict))
  text(x = x.pos,     y = y.pos[4]-para.small, labels = class.names[ifelse(is.na(value.plot), 0, value.plot)+1], cex = cex.text, pos=4)
  
  # Stroemung
  text(x = x.start+(x.pos*2.5), y = y.pos[4]-para.small, labels = paste(ecoval.translate("R_macrophytes_doc_velocity",dict),":",sep=""), font = 2, cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_flowvelocity_mpers",dict))
  text(x = x.start+(x.pos*3.2), y = y.pos[4]-para.small, labels = ifelse(is.na(value.plot), "", paste(round(value.plot,2), "m/s")),cex = cex.text, pos=4)
  
  
  # Ecomorphology
  text(x = 0, y = y.pos[6]-3*para.small, labels =  ecoval.translate("R_macrophytes_doc_ecomorphology",dict), col = "dodgerblue", cex = cex.title, font = 2, pos=4)
  
  # General ecomorphological state (MSK Oek F)
  value.plot  <- get.value(data.site,row.no,gsub(" ","_",ecoval.translate("N_morphol",dict)))
  class <- calc.class(value.plot, classes = c(1,1,1,2,2,2,3,3,4,4))
  value.plot <- ifelse(is.na(value.plot), "", value.plot)
  text(x = x.start, y = y.pos[7]-4*para.small, labels = paste(ecoval.translate("R_macrophytes_doc_ecomorphologicalstate",dict),":",sep=""), cex = cex.text, font = 2, pos=4)
  rect(x.pos,y.pos[7]-4*para.small-0.16,x.pos+2.5,y.pos[7]-4*para.small+0.23, col = msk.colors[-3][class+1], border = NA)
  text(x = x.pos,   y = y.pos[7]-4*para.small, 
       labels = c("",
                  ecoval.translate("R_macrophytes_doc_ecomorphology_IV_artificial",dict),
                  ecoval.translate("R_macrophytes_doc_ecomorphology_III_stronglydegraded",dict),
                  ecoval.translate("R_macrophytes_doc_ecomorphology_II_slightlydegraded",dict),
                  ecoval.translate("R_macrophytes_doc_ecomorphology_I_natural",dict))[class+1],
       font = 2, cex = cex.text, pos=4)
  
  # Define vector with modification classes for plotting
  class.names.oekf <- c("",
                        ecoval.translate("R_macrophytes_doc_variability_strong",dict),
                        ecoval.translate("R_macrophytes_doc_variability_limited",dict),
                        ecoval.translate("R_macrophytes_doc_variability_none",dict))
  names(class.names.oekf) <- c("0",
                               ecoval.translate("L_morphol_widthvar_class_high",dict),
                               ecoval.translate("L_morphol_widthvar_class_moderate",dict),
                               ecoval.translate("L_morphol_widthvar_class_none",dict))
  
  text(x = x.start, y = y.pos[8]-4*para.small, labels = paste(ecoval.translate("R_macrophytes_doc_widthvariability",dict),":",sep=""), cex = cex.text, pos=4)
  value.plot  <- as.character(get.value(data.site,row.no,ecoval.translate("A_morphol_widthvar_class",dict)))
  text(x = x.pos,   y = y.pos[8]-4*para.small, labels = class.names.oekf[ifelse(is.na(value.plot), "0", value.plot)], cex = cex.text, pos=4)
  
  text(x = x.start, y = y.pos[9]-4*para.small, labels = paste(ecoval.translate("R_macrophytes_doc_depthvariability",dict),":",sep=""), cex = cex.text, pos=4)
  value.plot  <- as.character(get.value(data.site,row.no,ecoval.translate("A_morphol_depthvar_class",dict)))
  text(x = x.pos,   y = y.pos[9]-4*para.small, labels = class.names.oekf[ifelse(is.na(value.plot), "0", value.plot)], cex = cex.text, pos=4)
  
  # Appearance (MSK Aeusserer Aspekt F)
  text(x = x.pos*2.5, y = y.pos[6]-3*para.small, labels = ecoval.translate("R_macrophytes_doc_physappearance",dict), col = "dodgerblue", cex = cex.title, font = 2, pos=4)
  
  class.names.colm <- c("",
                        ecoval.translate("R_macrophytes_doc_physappearance_clogging_none",dict),
                        ecoval.translate("R_macrophytes_doc_physappearance_clogging_intermediate",dict),
                        ecoval.translate("R_macrophytes_doc_physappearance_clogging_strong",dict))
  text(x = x.start+(x.pos*2.5), y = y.pos[7]-4*para.small,   labels = paste(ecoval.translate("R_macrophytes_doc_physappearance_clogging",dict),":",sep=""), cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_clogging_class",dict))
  text(x = x.start+(x.pos*3.2),   y = y.pos[7]-4*para.small, labels = class.names.colm[ifelse(is.na(value.plot), 0, value.plot)+1], cex = cex.text, pos=4)
  
  class.names.turb <- c("",
                        ecoval.translate("R_macrophytes_doc_physappearance_turbidity_none",dict),
                        ecoval.translate("R_macrophytes_doc_physappearance_turbidity_intermediate",dict),
                        ecoval.translate("R_macrophytes_doc_physappearance_turbidity_strong",dict))
  text(x = x.start+(x.pos*2.5), y = y.pos[8]-4*para.small,   labels = paste(ecoval.translate("R_macrophytes_doc_physappearance_turbidity",dict),":",sep=""), cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_turbidity_class",dict))
  text(x = x.start+(x.pos*3.2), y = y.pos[8]-4*para.small, labels = class.names.turb[ifelse(is.na(value.plot), 0, value.plot)+1], cex = cex.text, pos=4)
  
  # General modifications (MSK Oek F)
  
  text(x = x.start,    y = y.pos[10]-5*para.small, labels = ecoval.translate("R_macrophytes_doc_construction",dict), font = 2,  cex = cex.text, pos=4)
  text(x = x.pos,      y = y.pos[10]-5*para.small, labels = ecoval.translate("R_macrophytes_doc_construction_degree",dict), font = 2, cex = cex.text, pos=4)
  text(x = x.pos*2.25, y = y.pos[10]-5*para.small, labels = ecoval.translate("R_macrophytes_doc_construction_type",dict), font = 2, cex = cex.text, pos=4)

  # Define vectors with modification classes and materials for plotting
  class.names.bed     <- c("",
                           "",
                           ecoval.translate("R_macrophytes_doc_construction_bedfract_0",dict),
                           ecoval.translate("R_macrophytes_doc_construction_bedfract_0to10",dict),
                           ecoval.translate("R_macrophytes_doc_construction_bedfract_10to30",dict),
                           ecoval.translate("R_macrophytes_doc_construction_bedfract_30to60",dict),
                           ecoval.translate("R_macrophytes_doc_construction_bedfract_30to100",dict),
                           ecoval.translate("R_macrophytes_doc_construction_bedfract_60to100",dict),
                           ecoval.translate("R_macrophytes_doc_construction_bedfract_100",dict))
  names(class.names.bed) <- c("0",
                              "00",
                              ecoval.translate("L_morphol_bedmod_fract_class_0",dict),
                              ecoval.translate("L_morphol_bedmod_fract_class_0to10",dict),
                              ecoval.translate("L_morphol_bedmod_fract_class_10to30",dict),
                              ecoval.translate("L_morphol_bedmod_fract_class_30to60",dict),
                              ecoval.translate("L_morphol_bedmod_fract_class_30to100",dict),
                              ecoval.translate("L_morphol_bedmod_fract_class_60to100",dict),
                              ecoval.translate("L_morphol_bedmod_fract_class_100",dict))
  if ( "0" %in% names(class.names.bed)[-1] ) class.names.bed <- class.names.bed[-1] # "0" is interpreted as NA unless it is a specific code
  class.names.bank    <- c("",
                           "",
                           ecoval.translate("R_macrophytes_doc_construction_bankfract_0",dict),
                           ecoval.translate("R_macrophytes_doc_construction_bankfract_0to10",dict),
                           ecoval.translate("R_macrophytes_doc_construction_bankfract_10to30",dict),
                           ecoval.translate("R_macrophytes_doc_construction_bankfract_30to60",dict),
                           ecoval.translate("R_macrophytes_doc_construction_bankfract_60to100",dict),
                           ecoval.translate("R_macrophytes_doc_construction_bankfract_100",dict))
  names(class.names.bank) <- c("0",
                               "00",
                               ecoval.translate("L_morphol_bankmod_fract_class_0",dict),
                               ecoval.translate("L_morphol_bankmod_fract_class_0to10",dict),
                               ecoval.translate("L_morphol_bankmod_fract_class_10to30",dict),
                               ecoval.translate("L_morphol_bankmod_fract_class_30to60",dict),
                               ecoval.translate("L_morphol_bankmod_fract_class_60to100",dict),
                               ecoval.translate("L_morphol_bankmod_fract_class_100",dict))
  if ( names(class.names.bank)[1] %in% names(class.names.bank)[-1] ) class.names.bank <- class.names.bank[-1] # "0" is interpreted as NA unless it is a specific code
  mat.names.bed <- c("",
                     ecoval.translate("R_macrophytes_doc_construction_bedmat_riprap1",dict),
                     ecoval.translate("R_macrophytes_doc_construction_bedmat_riprap2",dict),
                     ecoval.translate("R_macrophytes_doc_construction_bedmat_riprap3",dict),
                     ecoval.translate("R_macrophytes_doc_construction_bedmat_riprap4",dict),
                     ecoval.translate("R_macrophytes_doc_construction_bedmat_riprap5",dict),
                     ecoval.translate("R_macrophytes_doc_construction_bedmat_other1",dict),
                     ecoval.translate("R_macrophytes_doc_construction_bedmat_other2",dict),
                     ecoval.translate("R_macrophytes_doc_construction_bedmat_other3",dict),
                     ecoval.translate("R_macrophytes_doc_construction_bedmat_other4",dict),
                     ecoval.translate("R_macrophytes_doc_construction_bedmat_other5",dict))
  names(mat.names.bed) <- c("0",
                            ecoval.translate("L_morphol_bedmod_type_class_riprap1",dict),
                            ecoval.translate("L_morphol_bedmod_type_class_riprap2",dict),
                            ecoval.translate("L_morphol_bedmod_type_class_riprap3",dict),
                            ecoval.translate("L_morphol_bedmod_type_class_riprap4",dict),
                            ecoval.translate("L_morphol_bedmod_type_class_riprap5",dict),
                            ecoval.translate("L_morphol_bedmod_type_class_other1",dict),
                            ecoval.translate("L_morphol_bedmod_type_class_other2",dict),
                            ecoval.translate("L_morphol_bedmod_type_class_other3",dict),
                            ecoval.translate("L_morphol_bedmod_type_class_other4",dict),
                            ecoval.translate("L_morphol_bedmod_type_class_other5",dict))
  mat.names.bank <- c("",
                     ecoval.translate("R_macrophytes_doc_construction_bankmat_perm1",dict),
                     ecoval.translate("R_macrophytes_doc_construction_bankmat_perm2",dict),
                     ecoval.translate("R_macrophytes_doc_construction_bankmat_perm3",dict),
                     ecoval.translate("R_macrophytes_doc_construction_bankmat_perm4",dict),
                     ecoval.translate("R_macrophytes_doc_construction_bankmat_perm5",dict),
                     ecoval.translate("R_macrophytes_doc_construction_bankmat_imperm1",dict),
                     ecoval.translate("R_macrophytes_doc_construction_bankmat_imperm2",dict),
                     ecoval.translate("R_macrophytes_doc_construction_bankmat_imperm3",dict),
                     ecoval.translate("R_macrophytes_doc_construction_bankmat_imperm4",dict),
                     ecoval.translate("R_macrophytes_doc_construction_bankmat_imperm5",dict))
  names(mat.names.bank) <- c("0",
                            ecoval.translate("L_morphol_bankmod_perm_class_perm1",dict),
                            ecoval.translate("L_morphol_bankmod_perm_class_perm2",dict),
                            ecoval.translate("L_morphol_bankmod_perm_class_perm3",dict),
                            ecoval.translate("L_morphol_bankmod_perm_class_perm4",dict),
                            ecoval.translate("L_morphol_bankmod_perm_class_perm5",dict),
                            ecoval.translate("L_morphol_bankmod_perm_class_imperm1",dict),
                            ecoval.translate("L_morphol_bankmod_perm_class_imperm2",dict),
                            ecoval.translate("L_morphol_bankmod_perm_class_imperm3",dict),
                            ecoval.translate("L_morphol_bankmod_perm_class_imperm4",dict),
                            ecoval.translate("L_morphol_bankmod_perm_class_imperm5",dict))
  
  text(x = x.start,    y = y.pos[11]-5*para.small, labels = ecoval.translate("R_macrophytes_doc_construction_bed",dict), cex = cex.text, pos=4)
  value.plot  <- as.character(get.value(data.site,row.no,ecoval.translate("A_morphol_bedmod_fract_class",dict)))
  text(x = x.pos,      y = y.pos[11]-5*para.small, labels = class.names.bed[ifelse(is.na(value.plot), "00", value.plot)], cex = cex.text, pos=4)
  value.plot  <- as.character(get.value(data.site,row.no,ecoval.translate("A_morphol_bedmod_type_class",dict)))
  text(x = x.pos*2.25, y = y.pos[11]-5*para.small, labels = mat.names.bed[ifelse(is.na(value.plot), "0", value.plot)], cex = cex.text, pos=4)

  text(x = x.start,    y = y.pos[12]-5*para.small, labels = paste(ecoval.translate("R_macrophytes_doc_construction_bank",dict),", ",ecoval.translate("R_macrophytes_doc_construction_left",dict),sep=""), cex = cex.text, pos=4)
  value.plot  <- as.character(get.value(data.site,row.no,ecoval.translate("A_morphol_bankmod_fract_left_class",dict)))
  text(x = x.pos,      y = y.pos[12]-5*para.small, labels = class.names.bank[ifelse(is.na(value.plot), "00", value.plot)], cex = cex.text, pos=4)
  value.plot  <- as.character(get.value(data.site,row.no,ecoval.translate("A_morphol_bankmod_perm_left_class",dict)))
  text(x = x.pos*2.25, y = y.pos[12]-5*para.small, labels = mat.names.bank[ifelse(is.na(value.plot), "0", value.plot)], cex = cex.text, pos=4)

  text(x = x.start,    y = y.pos[13]-5*para.small, labels = paste(ecoval.translate("R_macrophytes_doc_construction_bank",dict),", ",ecoval.translate("R_macrophytes_doc_construction_right",dict),sep=""), cex = cex.text, pos=4)
  value.plot  <- as.character(get.value(data.site,row.no,ecoval.translate("A_morphol_bankmod_fract_right_class",dict)))
  text(x = x.pos,      y = y.pos[13]-5*para.small, labels = class.names.bank[ifelse(is.na(value.plot), "00", value.plot)], cex = cex.text, pos=4)
  value.plot  <- as.character(get.value(data.site,row.no,ecoval.translate("A_morphol_bankmod_perm_right_class",dict)))
  text(x = x.pos*2.25, y = y.pos[13]-5*para.small, labels = mat.names.bank[ifelse(is.na(value.plot), "0", value.plot)], cex = cex.text, pos=4) #PARA

  # Riparian width and surrounding area

  # Define vector with names of surrounding area for plotting
  riparian.names.long <- c("",
                      ecoval.translate("R_macrophytes_doc_riparian_natural_gravel",dict),
                      ecoval.translate("R_macrophytes_doc_riparian_natural_reeds",dict),
                      ecoval.translate("R_macrophytes_doc_riparian_natural_forest",dict),
                      ecoval.translate("R_macrophytes_doc_riparian_natural_treesshrub",dict),
                      ecoval.translate("R_macrophytes_doc_riparian_seminatural_perennials",dict),
                      ecoval.translate("R_macrophytes_doc_riparian_seminatural_meadow",dict),
                      ecoval.translate("R_macrophytes_doc_riparian_seminatural_alley",dict),
                      ecoval.translate("R_macrophytes_doc_riparian_artificial",dict))
  names(riparian.names.long) <- c("00",
                             ecoval.translate("L_morphol_riparzone_vegmat_class_gravel",dict),
                             ecoval.translate("L_morphol_riparzone_vegmat_class_reeds",dict),
                             ecoval.translate("L_morphol_riparzone_vegmat_class_forest",dict),
                             ecoval.translate("L_morphol_riparzone_vegmat_class_treesshrub",dict),
                             ecoval.translate("L_morphol_riparzone_vegmat_class_perennials",dict),
                             ecoval.translate("L_morphol_riparzone_vegmat_class_meadow",dict),
                             ecoval.translate("L_morphol_riparzone_vegmat_class_alley",dict),
                             ecoval.translate("L_morphol_riparzone_vegmat_class_artificial",dict))
  riparian.names.short <- c("",
                           ecoval.translate("R_macrophytes_doc_riparian_natural",dict),
                           ecoval.translate("R_macrophytes_doc_riparian_seminatural",dict),
                           ecoval.translate("R_macrophytes_doc_riparian_artificial",dict))
  names(riparian.names.short) <- c("00",
                                  ecoval.translate("L_morphol_riparzone_veg_class_natural",dict),
                                  ecoval.translate("L_morphol_riparzone_veg_class_seminatural",dict),
                                  ecoval.translate("L_morphol_riparzone_veg_class_artificial",dict))
  
  surarea.names <- c("",
                     ecoval.translate("R_macrophytes_doc_surrounding_mixedforest",dict),
                     ecoval.translate("R_macrophytes_doc_surrounding_coniferforest",dict),
                     ecoval.translate("R_macrophytes_doc_surrounding_broadleafforest",dict),
                     ecoval.translate("R_macrophytes_doc_surrounding_fertilizedmeadow",dict),
                     ecoval.translate("R_macrophytes_doc_surrounding_poorgrassland",dict),
                     ecoval.translate("R_macrophytes_doc_surrounding_reeds",dict),
                     ecoval.translate("R_macrophytes_doc_surrounding_wetlands",dict),
                     ecoval.translate("R_macrophytes_doc_surrounding_urban",dict),
                     ecoval.translate("R_macrophytes_doc_surrounding_others",dict))

  text(x = x.start,    y = y.pos[14]-6*para.small, labels = ecoval.translate("R_macrophytes_doc_riparian",dict), font = 2,  cex = cex.text, pos=4)
  text(x = x.pos,      y = y.pos[14]-6*para.small, labels = ecoval.translate("R_macrophytes_doc_riparian_left",dict), font = 2, cex = cex.text, pos=4)
  text(x = x.pos*2.25, y = y.pos[14]-6*para.small, labels = ecoval.translate("R_macrophytes_doc_riparian_right",dict), font = 2, cex = cex.text, pos=4)

  text(x = x.start,    y = y.pos[15]-6*para.small, labels = ecoval.translate("R_macrophytes_doc_riparian_width",dict), cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_morphol_riparzone_width_left_m",dict))
  text(x = x.pos,      y = y.pos[15]-6*para.small, labels = ifelse(is.na(value.plot), "", paste(value.plot,"m")), cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_morphol_riparzone_width_right_m",dict))
  text(x = x.pos*2.25, y = y.pos[15]-6*para.small, labels = ifelse(is.na(value.plot), "", paste(value.plot,"m")), cex = cex.text, pos=4)

  text(x = x.start,    y = y.pos[16]-6*para.small, labels = ecoval.translate("R_macrophytes_doc_riparian_texture",dict), cex = cex.text, pos=4)
  if ( ecoval.translate("A_morphol_riparzone_vegmat_left_class",dict) %in% colnames(data.site) )
  {
    value.plot  <- as.character(get.value(data.site,row.no,ecoval.translate("A_morphol_riparzone_vegmat_left_class",dict)))
    text(x = x.pos,      y = y.pos[16]-6*para.small, labels = riparian.names.long[ifelse(is.na(value.plot), "00", value.plot)], cex = cex.text, pos=4)
    value.plot  <- as.character(get.value(data.site,row.no,ecoval.translate("A_morphol_riparzone_vegmat_right_class",dict)))
    text(x = x.pos*2.25, y = y.pos[16]-6*para.small, labels = riparian.names.long[ifelse(is.na(value.plot), "00", value.plot)], cex = cex.text, pos=4)
  }
  else
  {
    value.plot  <- as.character(get.value(data.site,row.no,ecoval.translate("A_morphol_riparzone_veg_left_class",dict)))
    text(x = x.pos,      y = y.pos[16]-6*para.small, labels = riparian.names.short[ifelse(is.na(value.plot), "00", value.plot)], cex = cex.text, pos=4)
    value.plot  <- as.character(get.value(data.site,row.no,ecoval.translate("A_morphol_riparzone_veg_right_class",dict)))
    text(x = x.pos*2.25, y = y.pos[16]-6*para.small, labels = riparian.names.short[ifelse(is.na(value.plot), "00", value.plot)], cex = cex.text, pos=4)
  }

  text(x = x.start,    y = y.pos[17]-6*para.small, labels = ecoval.translate("R_macrophytes_doc_surrounding",dict), cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_morphol_surarea_mat_left_class",dict))
  text(x = x.pos,      y = y.pos[17]-6*para.small, labels = surarea.names[ifelse(is.na(value.plot), 0, value.plot)+1], cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_morphol_surarea_mat_right_class",dict))
  text(x = x.pos*2.25, y = y.pos[17]-6*para.small, labels = surarea.names[ifelse(is.na(value.plot), 0, value.plot)+1], cex = cex.text, pos=4)
  
  # Catchment area
  text(x = 0, y.pos[19]-8*para.small, labels =  ecoval.translate("R_macrophytes_doc_areafractions",dict), col = "dodgerblue",
       cex = cex.title, font = 2, pos=4)
  
  text(x = x.start,   y = y.pos[20]-9*para.small, labels = ecoval.translate("R_macrophytes_doc_areafraction_rural",dict), cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_catchment_agricultureall_percent",dict))
  text(x = x.pos*1.5, y = y.pos[20]-9*para.small, labels = ifelse(is.na(value.plot), "", paste(round(value.plot,0),"%")),cex = cex.text, pos=4)
  
  text(x = x.start,   y = y.pos[21]-9*para.small, labels = ecoval.translate("R_macrophytes_doc_areafraction_urban",dict), cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_catchment_urban_percent",dict))
  text(x = x.pos*1.5, y = y.pos[21]-9*para.small, labels = ifelse(is.na(value.plot), "", paste(round(value.plot,0), "%")),
       cex = cex.text, pos=4)
  
  text(x = x.start,   y = y.pos[22]-9*para.small, labels = ecoval.translate("R_macrophytes_doc_areafraction_forest",dict), cex = cex.text, pos=4)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_catchment_forest_percent",dict))
  text(x = x.pos*1.5, y = y.pos[22]-9*para.small, labels = ifelse(is.na(value.plot), "", paste(round(value.plot,0), "%")),cex = cex.text, pos=4)

  # Write footer
  plot(0, 0, axes = axis.info, type="n", xaxs="i", yaxs="i", xlim=c(0,10),ylim=c(0,10))
  abline(h = 8)
  
  text(x = 10, y = 5, format(Sys.Date(), "%d.%m.%Y"), pos = 2)
}

# Function for typology information A4 PDF
# ========================================

msk.macrophytes.2017.doc.typology <- function(res, row.no) {
  
  # get dictionary
  dict <- ecoval.dict(res$language,res$dictionaries)
  
  # check data availability and extract data for plotting
  
  if( is.na(match("data.site", names(res))) )          return()
  if( is.na(match("types.scheme.probs", names(res))) ) return()
  if( is.na(match("types.val.probs", names(res))) )    return()
  if( is.na(match("types.grfo.probs", names(res))) )   return()
  if( row.no  <= 0 | row.no > nrow(res$data.site) )    return()
  
  data.site    <- res$data.site
  probs.scheme <- res$types.scheme.probs
  probs.val    <- res$types.val.probs
  probs.grfo   <- res$types.grfo.probs
  
  
  
  # create function to check if parameter column names are defined in dictionary (i.e. != "") and present in date, else
  # NA is returned for plotting
  get.value <- function(data.plot, row.no, colname) {
    value.plot <- NA
    if ( length(row.no)>0 & length(colname)>0 )
    {
      if ( row.no >= 1 & row.no <= nrow(data.plot) & colname != "" & sum(colname == colnames(data.plot)) != 0 ) value.plot <- data.plot[row.no,colname]
    }
    return(value.plot)
  }
  
  # set expansion factors for fonts in plot
  cex.title   <- 1.5
  cex.text    <- 1.2
  
  # create ploting device wit appropriate layout
  layout.mat <- matrix(c(0,1,1,0,
                         0,2,2,0,
                         0,3,4,0,
                         0,3,5,0,
                         0,6,6,0,
                         0,7,7,0,
                         0,8,8,0), ncol = 4, nrow = 7, byrow = T)
  layout.mat.widths  <- c(1.0,6.5,9.5,1.0)
  layout.mat.heights <- c(1.5,1.75,3,7,2,11,1.5)
  
  axis.info <- FALSE
  
  layout.plot.typology <- layout(layout.mat,
                                 widths = layout.mat.widths,
                                 heights = layout.mat.heights,
                                 respect = FALSE)
  
  par(mai = c(0,0,0,0))
  
  ## HEADER
  plot(0, 0, axes = axis.info, type="n", xaxs="i", yaxs="i", xlim=c(0,10), ylim=c(0,10))
  text(x = 0, y = 5, labels = ecoval.translate("R_macrophytes_doc_typology",dict), font = 2, cex = cex.title, pos=4)
  abline(h = 3)
  
  ## WINDOW 2 - Basic information of sampling event as header in grey box
  plot(0, 0, axes = axis.info, type="n", xaxs="i", yaxs="i", xlim=c(0,10), ylim=c(0,10))
  rect(0,0.5,10,9.5, col = "grey85", border = NA)
  
  # Line 1
  text(x = 0, y = 7.5,   labels =  paste(ecoval.translate("R_macrophytes_doc_codesite",dict),":",sep=""), cex = cex.text, pos=4, font=2)
  value.plot <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_siteid",dict))
  text(x = 1.4, y = 7.5, labels =  ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
  
  text(x = 2.7, y = 7.5, labels =  paste(ecoval.translate("R_macrophytes_doc_canton",dict),":",sep=""), cex = cex.text, pos=4, font = 2)
  value.plot <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_canton",dict))
  text(x = 4, y = 7.5,   labels = ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
  
  text(x = 6.5, y = 7.5,   labels =  paste(ecoval.translate("R_macrophytes_doc_coordinates",dict),":",sep=""), cex = cex.text, pos=4, font = 2)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_coordinatex_swissgrid",dict))
  value.plot1 <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_coordinatey_swissgrid",dict))
  text(x = 8.3, y = 7.5, labels =  paste(ifelse(is.na(value.plot), "", value.plot),
                                         ifelse(is.na(value.plot1), "", value.plot1), sep = " / "),cex = cex.text, pos=4)
  
  # Line 2
  text(x = 0, y = 5,   labels =  paste(ecoval.translate("R_macrophytes_doc_river",dict),":",sep=""), cex = cex.text, pos=4, font=2)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_waterbody",dict))
  text(x = 1.4, y = 5, labels = ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
  
  text(x = 2.7, y = 5, labels =  paste(ecoval.translate("R_macrophytes_doc_location",dict),":",sep=""), cex = cex.text, pos=4, font = 2)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_location",dict))
  text(x = 4, y = 5,   labels =  ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
  
  # Line 3
  text(x = 0, y = 2.5,   labels =  paste(ecoval.translate("R_macrophytes_doc_samplingdate",dict),":",sep=""), cex = cex.text, pos=4, font=2)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_samplingdate",dict))
  text(x = 1.4, y = 2.5, labels = ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
  
  text(x = 2.7, y = 2.5, labels =  paste(ecoval.translate("R_macrophytes_doc_observer",dict),":",sep=""), cex = cex.text, pos=4, font = 2)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_operator",dict))
  text(x = 4, y = 2.5,   labels =  ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
  
  text(x = 6.5, y = 2.5,   labels =  paste(ecoval.translate("R_macrophytes_doc_sectionlength",dict),":",sep=""), cex = cex.text, pos=4, font = 2)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_lengthsection_m",dict))
  text(x = 8.3, y = 2.5, labels =  ifelse(is.na(value.plot), "", paste(value.plot, "m")), cex = cex.text, pos=4)
  
  
  ## WINDOW 3 - Probabilities of river types
  plot(0, 0, axes = axis.info, type="n", xaxs="i", yaxs="i", xlim=c(0,5), ylim=c(0,10))
  text(x = 0, y = 9, labels =  ecoval.translate("R_macrophytes_doc_typology_probabilities",dict), col = "dodgerblue", cex = cex.title, pos=4, font=2)
  
  # Set plot parameters for this window
  x.start <- 0.1
  y.pos <- 8.25 - 0.44 * seq(0,20,1)
  para.small <- 0.43/2
  x.pos <- 2.2
  
  # Section Header: River type probabilities
  text(x = 0, y = y.pos[1]+0.15, labels =  ecoval.translate("R_macrophytes_doc_typology_types",dict), cex = cex.title, pos=4, font=2)
  
  # Column headers
  text(x = x.start,   y = y.pos[2], labels = ecoval.translate("R_macrophytes_doc_typology_type",dict) , cex = cex.text, pos=4, font = 2)
  text(x = x.pos*0.7, y = y.pos[2], labels = ecoval.translate("R_macrophytes_doc_typology_scheme",dict) , cex = cex.text, pos=4, font = 2)
  text(x = x.pos*1.2, y = y.pos[2], labels = ecoval.translate("R_macrophytes_doc_typology_modified",dict), cex = cex.text, pos=4, font = 2)
  
  # Kleiner Submersenbach KS
  text(x = x.start,   y = y.pos[3], labels = ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.scheme,row.no,ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict))
  text(x = x.pos*0.7, y = y.pos[3], labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.val,row.no,ecoval.translate("L_macrophytes_rivertype_class_smallsubmerged",dict))
  text(x = x.pos*1.2, y = y.pos[3], labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  
  # Mittlerer Submersenbach MS
  text(x = x.start,   y = y.pos[4], labels = ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.scheme,row.no,ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict))
  text(x = x.pos*0.7, y = y.pos[4], labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.val,row.no,ecoval.translate("L_macrophytes_rivertype_class_mediumsubmerged",dict))
  text(x = x.pos*1.2, y = y.pos[4], labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  
  # Grosser Submersenbach GS
  text(x = x.start,   y = y.pos[5], labels = ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.scheme,row.no,ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict))
  text(x = x.pos*0.7, y = y.pos[5], labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.val,row.no,ecoval.translate("L_macrophytes_rivertype_class_largesubmerged",dict))
  text(x = x.pos*1.2, y = y.pos[5], labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  
  # Sehr Grosser Submersenbach SGS
  text(x = x.start,   y = y.pos[6], labels = ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.scheme,row.no,ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict))
  text(x = x.pos*0.7, y = y.pos[6], labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.val,row.no,ecoval.translate("L_macrophytes_rivertype_class_verylargesubmerged",dict))
  text(x = x.pos*1.2, y = y.pos[6], labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  
  # Kleiner Submersen - Helophyten Uebergangstyp
  text(x = x.start,   y = y.pos[7]-para.small, labels = ecoval.translate("L_macrophytes_rivertype_class_smallsubmergedhelophyte",dict),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.scheme,row.no,ecoval.translate("L_macrophytes_rivertype_class_smallsubmergedhelophyte",dict))
  text(x = x.pos*0.7, y = y.pos[7]-para.small, labels = ifelse(is.na(value.plot), "",paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  arrows(x0 = x.pos*1.15, y0 = y.pos[7]-para.small, x1 = x.pos*1.3, y1 = y.pos[6]-1.2*para.small, length = 0.08)
  
  # Mittlerer Submersen - Helophyten Uebergangstyp
  text(x = x.start,   y = y.pos[8]-para.small, labels = ecoval.translate("L_macrophytes_rivertype_class_mediumsubmergedhelophyte",dict),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.scheme,row.no,ecoval.translate("L_macrophytes_rivertype_class_mediumsubmergedhelophyte",dict))
  text(x = x.pos*0.7, y = y.pos[8]-para.small, labels = ifelse(is.na(value.plot), "",paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  arrows(x0 = x.pos*1.15, y0 = y.pos[8]-para.small, x1 = x.pos*1.3, y1 = y.pos[9]-0.8*para.small, length = 0.08)
  
  # Kleiner Helophytenbach KH
  text(x = x.start,   y = y.pos[9]-2*para.small, labels = ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.scheme,row.no,ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict))
  text(x = x.pos*0.7, y = y.pos[9]-2*para.small, labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.val,row.no,ecoval.translate("L_macrophytes_rivertype_class_smallhelophyte",dict))
  text(x = x.pos*1.2, y = y.pos[9]-2*para.small, labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  
  # Mittlerer Helophytenbach MH
  text(x = x.start,   y = y.pos[10]-2*para.small, labels = ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.scheme,row.no,ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict))
  text(x = x.pos*0.7, y = y.pos[10]-2*para.small, labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.val,row.no,ecoval.translate("L_macrophytes_rivertype_class_mediumhelophyte",dict))
  text(x = x.pos*1.2, y = y.pos[10]-2*para.small, labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  
  # Kleiner Helophyten - Moos Uebergangstyp KH-KM
  text(x = x.start,   y = y.pos[11]-3*para.small, labels = ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.scheme,row.no,ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict))
  text(x = x.pos*0.7, y = y.pos[11]-3*para.small, labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.val,row.no,ecoval.translate("L_macrophytes_rivertype_class_smallhelophytebryophyte",dict))
  text(x = x.pos*1.2, y = y.pos[11]-3*para.small, labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  
  # Mittlerer Helophyten - Moos Uebergangstyp MH-MM
  text(x = x.start,   y = y.pos[12]-3*para.small, labels = ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.scheme,row.no,ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict))
  text(x = x.pos*0.7, y = y.pos[12]-3*para.small, labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.val,row.no,ecoval.translate("L_macrophytes_rivertype_class_mediumhelophytebryophyte",dict))
  text(x = x.pos*1.2, y = y.pos[12]-3*para.small, labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  
  # Kleiner Moosbach KM
  text(x = x.start,   y = y.pos[13]-4*para.small, labels = ecoval.translate("L_macrophytes_rivertype_class_smallbryophyte",dict),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.scheme,row.no,ecoval.translate("L_macrophytes_rivertype_class_smallbryophyte",dict))
  text(x = x.pos*0.7, y = y.pos[13]-4*para.small, labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.val,row.no,ecoval.translate("L_macrophytes_rivertype_class_smallbryophyte",dict))
  text(x = x.pos*1.2, y = y.pos[13]-4*para.small, labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  
  # Mittlerer Moosbach MM
  text(x = x.start,   y = y.pos[14]-4*para.small, labels = ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.scheme,row.no,ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict))
  text(x = x.pos*0.7, y = y.pos[14]-4*para.small, labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.val,row.no,ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict))
  text(x = x.pos*1.2, y = y.pos[14]-4*para.small, labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  
  # Grosser Moosbach GM
  text(x = x.start,   y = y.pos[15]-4*para.small, labels = ecoval.translate("L_macrophytes_rivertype_class_largebryophyte",dict),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.scheme,row.no,ecoval.translate("L_macrophytes_rivertype_class_largebryophyte",dict))
  text(x = x.pos*0.7, y = y.pos[15]-4*para.small, labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.val,row.no,ecoval.translate("L_macrophytes_rivertype_class_largebryophyte",dict))
  text(x = x.pos*1.2, y = y.pos[15]-4*para.small, labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  
  # Sehr Grosser Moosbach SGM
  text(x = x.start,   y = y.pos[16]-4*para.small, labels = ecoval.translate("L_macrophytes_rivertype_class_verylargebryophyte",dict),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.scheme,row.no,ecoval.translate("L_macrophytes_rivertype_class_verylargebryophyte",dict))
  text(x = x.pos*0.7, y = y.pos[16]-4*para.small, labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.val,row.no,ecoval.translate("L_macrophytes_rivertype_class_verylargebryophyte",dict))
  text(x = x.pos*1.2, y = y.pos[16]-4*para.small, labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  
  # Summe aller Vegetationsarmen Typen VA
  text(x = x.start,   y = y.pos[17]-5*para.small, labels = ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.grfo,row.no,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict))
  text(x = x.pos*0.7, y = y.pos[17]-5*para.small, labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  value.plot  <- get.value(probs.grfo,row.no,ecoval.translate("L_macrophytes_rivertype_class_poorveg",dict))
  text(x = x.pos*1.2, y = y.pos[17]-5*para.small, labels = ifelse(is.na(value.plot), "", paste(round(100*value.plot,1), "%")),
       cex = cex.text, pos=4)
  
  ## WINDOW 4 - Header for barplot with growthform type probabilities
  plot(0, 0, axes = axis.info, type="n", xaxs="i", yaxs="i", xlim=c(0,5), ylim=c(0,10))
  text(x = 0, y = 4.6, labels =  ecoval.translate("R_macrophytes_doc_typology_growthformgroups",dict), cex = cex.title, pos=4, font=2)

  ## WINDOW 5 - Apply function to create barplot
  par(mai = c(0.3,0.5,0.2,0.2))
  msk.macrophytes.2017.plot.types.grfo(res,
                                       i         = row.no,
                                       las       = 1,
                                       cex.axis  = 1.25, 
                                       cex.names = 1.25)
  
  ## WINDOW 6 - Header for typology scheme with probabilities with shading
  par(mai = c(0,0,0,0))
  plot(0, 0, axes = axis.info, type="n", xaxs="i", yaxs="i", xlim=c(0,5), ylim=c(0,10))
  text(x = 0, y = 2, labels = ecoval.translate("R_macrophytes_doc_typology_summary",dict), col = "dodgerblue",
       cex = cex.title, pos=4, font=2)
  
  ## WINDOW 7 - Typology scheme matrix with probabilities of river types
  par(mai = c(0.3,0.3,0.3,0.3))
  msk.macrophytes.2017.plot.types.scheme(res,i=row.no,cex=1.3)
  
  # Write footer
  par(mai = c(0,0,0,0))
  plot(0, 0, axes = axis.info, type="n", xaxs="i", yaxs="i", xlim=c(0,10),ylim=c(0,10))
  abline(h = 8)
  
  text(x = 10, y = 5, format(Sys.Date(), "%d.%m.%Y"), pos = 2)
  
}

# Function for vegetation information A4 PDF
# ==========================================

msk.macrophytes.2017.doc.vegetation <- function(res, row.no) {
  
  # get dictionary:
  
  dict <- ecoval.dict(res$language,res$dictionaries)
  
  # get taxalist:
  
  taxalist.dat <- res$taxalist
  
  # check data availability and extract data for plotting
  if( is.na(match("data.site", names(res))) )       return()
  if( is.na(match("attrib.species", names(res))) )  return()
  if( is.na(match("species.assess", names(res))) )  return()
  if( row.no  <= 0 | row.no > nrow(res$data.site) ) return()
  
  data.site           <- res$data.site
  data.attrib         <- res$attrib.species
  data.species.assess <- res$species.assess
  
  # create function to check if parameter column names are defined in dictionary (i.e. != "") and present in date, else
  # NA is returned for plotting
  get.value <- function(data.plot, row.no, colname) {
    value.plot <- NA
    if ( length(row.no)>0 & length(colname)>0 )
    {
      if ( row.no >= 1 & row.no <= nrow(data.plot) & colname != "" & sum(colname == colnames(data.plot)) != 0 ) value.plot <- data.plot[row.no,colname]
    }
    return(value.plot)
  }
  
  # extract species data for current assessment and add relevant information from taxalist for plotting
  
  data.species <- data.species.assess[data.species.assess[,ecoval.translate("A_macrophytes_site_sampleid", dict)] ==
                                        row.names(data.attrib)[row.no],]
  
  if ( nrow(data.species) > 0 ) {
    
    data.species$Leitwert <- NA
    data.species$Prioritaet <- NA
    data.species$Rote.Liste <- NA
    data.species$Artinfo <- NA
    
    # add relative cover
    data.species$DG.Rel <- NA
    for( k in 1:nrow(data.species) ) {
      if(!is.na(data.species[k,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)])) {
        data.species[k,"DG.Rel"] <- 100 * data.species[k,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)] /
          sum(data.species[data.species[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)] != 
                             ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_bry",dict) &
                           data.species[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)] != 
                             ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_cha",dict),
                           ecoval.translate("A_macrophytes_species_absolutecover_percent",dict)])
      }
    }
    
    
    
    # add species information from taxalist
    for( k in 1:nrow(data.species) ) {
      row.id <- match(data.species[k,ecoval.translate("A_macrophytes_species_number_msk",dict)],
                      taxalist.dat[,ecoval.translate("A_macrophytes_species_number_msk",dict)])
      
      if( !is.na(taxalist.dat[row.id,ecoval.translate("A_macrophytes_taxalist_conservationinfo",dict)]) ) {
        data.species[k,"Artinfo"] <- taxalist.dat[row.id,ecoval.translate("A_macrophytes_taxalist_conservationinfo",dict)]
      }
      
      if( !is.na(taxalist.dat[row.id,ecoval.translate("A_macrophytes_taxalist_indexspecies",dict)]) ) {
        data.species[k,"Leitwert"] <- taxalist.dat[row.id,ecoval.translate("A_macrophytes_taxalist_indexspecies",dict)]
      }
      
      if( !is.na(taxalist.dat[row.id,ecoval.translate("A_macrophytes_taxalist_priorityspecies",dict)]) ) {
        data.species[k,"Prioritaet"] <- taxalist.dat[row.id,ecoval.translate("A_macrophytes_taxalist_priorityspecies",dict)]
      }
      
      if( !is.na(taxalist.dat[row.id,ecoval.translate("A_macrophytes_taxalist_redliststatuts",dict)]) ) {
        data.species[k,"Rote.Liste"] <- taxalist.dat[row.id,ecoval.translate("A_macrophytes_taxalist_redliststatuts",dict)]
      }
    }
  }
  
  # create plotting device with appropriate layout
  layout.mat <- matrix(c(0,1,0,
                         0,2,0,
                         0,3,0,
                         0,4,0,
                         0,5,0), ncol = 3, nrow = 5, byrow = T)
  layout.mat.widths  <- c(1.0,16,1.0)
  layout.mat.heights <- c(1.5,1.75,9,14,1.5)
  
  axis.info <- FALSE
  
  layout.plot.typology <- layout(layout.mat,
                                 widths = layout.mat.widths,
                                 heights = layout.mat.heights,
                                 respect = FALSE)
  
  #layout.show(layout.plot.typology)
  
  # set expansion factors for fonts in plot
  cex.title   <- 1.5
  cex.text    <- 1.2
  
  # set margin space to 0 inches
  par(mai = c(0,0,0,0))
  ## HEADER
  plot(0, 0, axes = axis.info, type="n", xaxs="i", yaxs="i", xlim=c(0,10), ylim=c(0,10))
  text(x = 0, y = 5, labels = ecoval.translate("R_macrophytes_doc_vegetation",dict), font = 2, cex = cex.title, pos=4)
  abline(h = 3)
  
  ## WINDOW 2 - Basic information of sampling event as header in grey box
  plot(0, 0, axes = axis.info, type="n", xaxs="i", yaxs="i", xlim=c(0,10), ylim=c(0,10))
  rect(0,0.5,10,9.5, col = "grey85", border = NA)
  
  # Line 1
  text(x = 0, y = 7.5,   labels =  paste(ecoval.translate("R_macrophytes_doc_codesite",dict),":",sep=""), cex = cex.text, pos=4, font=2)
  value.plot <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_siteid",dict))
  text(x = 1.4, y = 7.5, labels =  ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
  
  text(x = 2.7, y = 7.5, labels =  paste(ecoval.translate("R_macrophytes_doc_canton",dict),":",sep=""), cex = cex.text, pos=4, font = 2)
  value.plot <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_canton",dict))
  text(x = 4, y = 7.5,   labels = ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
  
  text(x = 6.5, y = 7.5,   labels =  paste(ecoval.translate("R_macrophytes_doc_coordinates",dict),":",sep=""), cex = cex.text, pos=4, font = 2)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_coordinatex_swissgrid",dict))
  value.plot1 <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_coordinatey_swissgrid",dict))
  text(x = 8.3, y = 7.5, labels =  paste(ifelse(is.na(value.plot), "", value.plot),
                                         ifelse(is.na(value.plot1), "", value.plot1), sep = " / "),
       cex = cex.text, pos=4)
  
  # Line 2
  text(x = 0, y = 5,   labels =  paste(ecoval.translate("R_macrophytes_doc_river",dict),":",sep=""), cex = cex.text, pos=4, font=2)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_waterbody",dict))
  text(x = 1.4, y = 5, labels = ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
  
  text(x = 2.7, y = 5, labels =  paste(ecoval.translate("R_macrophytes_doc_location",dict),":",sep=""), cex = cex.text, pos=4, font = 2)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_location",dict))
  text(x = 4, y = 5,   labels =  ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
  
  # Line 3
  text(x = 0, y = 2.5,   labels =  paste(ecoval.translate("R_macrophytes_doc_samplingdate",dict),":",sep=""), cex = cex.text, pos=4, font=2)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_samplingdate",dict))
  text(x = 1.4, y = 2.5, labels = ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
  
  text(x = 2.7, y = 2.5, labels =  paste(ecoval.translate("R_macrophytes_doc_observer",dict),":",sep=""), cex = cex.text, pos=4, font = 2)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_operator",dict))
  text(x = 4, y = 2.5,   labels =  ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
  
  text(x = 6.5, y = 2.5,   labels =  paste(ecoval.translate("R_macrophytes_doc_sectionlength",dict),":",sep=""), cex = cex.text, pos=4, font = 2)
  value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_lengthsection_m",dict))
  text(x = 8.3, y = 2.5, labels =  ifelse(is.na(value.plot), "", paste(value.plot, "m")), cex = cex.text, pos=4)
  
  ## WINDOW 3 - Vegetation attributes
  
  # set plot parameters for this window
  x.start <- 0.1
  y.pos <- 8.25 - 0.5 * seq(0,20,1)
  para.small <- 0.5/2
  x.pos <- 2.2
  
  # Vegetation attributes
  plot(0, 0, axes = axis.info, type="n", xaxs="i", yaxs="i", xlim=c(0,10),ylim=c(0,10))
  text(x = 0, y = 9, labels =  ecoval.translate("R_macrophytes_doc_vegetation_attributes",dict), col = "dodgerblue", cex = cex.title, font = 2, pos=4)
  
  # If no species data is available a comment is added to the fact-sheet, else normal fact-sheet wih species attributes 
  # and taxa-list is written
  if ( nrow(data.species) == 0 ) {
    text(x = 0,   y = y.pos[2], labels = ecoval.translate("R_macrophytes_doc_vegetation_nodata",dict), cex = cex.text, font = 2, pos=4)
    
    plot(0, 0, axes = axis.info, type="n", xaxs="i", yaxs="i", xlim=c(0,10),ylim=c(0,10))
    text(x = 0, y = 9.32, labels =  "Artenliste", col = "dodgerblue", cex = cex.title, font = 2, pos=4)
    
  } else {
    
    # General Headers
    text(x = x.pos,     y = y.pos[1], labels = ecoval.translate("R_macrophytes_doc_vegetation_number",dict), cex = cex.text, font = 2, pos=4)
    text(x = x.pos*1.5, y = y.pos[1], labels = paste(ecoval.translate("R_macrophytes_doc_vegetation_coverage",dict),"[%]"), cex = cex.text, font = 2, pos=4)
    text(x = x.pos*2.5, y = y.pos[1], labels = paste(ecoval.translate("R_macrophytes_doc_vegetation_areafraction",dict),"[%]"), cex = cex.text, font = 2, pos=4)
    
    # add macrophyte attribute information
    
    # All macrophytes
    text(x = 0,   y = y.pos[2], labels = ecoval.translate("R_macrophytes_doc_vegetation_macrophytes",dict), cex = cex.text, font = 2, pos=4)
    value.plot  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_taxa_all_richness_count",dict))
    text(x = x.pos,     y = y.pos[2], labels = ifelse(is.na(value.plot), "", value.plot), cex = cex.text, font=2, pos=4)
    
    value.plot  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict))
    text(x = x.pos*1.5, y = y.pos[2], labels = ifelse(is.na(value.plot), "", round(value.plot,1)), cex = cex.text, font=2, pos=4)
    
    value.plot  <- 100-get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_filamentousgreenalgae_relcover_percent",dict))
    text(x = x.pos*2.5, y = y.pos[2], labels = ifelse(is.na(value.plot), "", round(value.plot,1)), cex = cex.text, font=2, pos=4)
    
    # aquatic species
    text(x = x.start,   y = y.pos[3], labels = ecoval.translate("R_macrophytes_doc_vegetation_aquatic",dict), cex = cex.text, pos=4)
    
    value.plot  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_taxa_aquatic_richness_count",dict))
    text(x = x.pos,     y = y.pos[3], labels = ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
    
    value.plot  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_taxa_aquatic_abscover_percent",dict))
    text(x = x.pos*1.5, y = y.pos[3], labels = ifelse(is.na(value.plot), "", round(value.plot,1)), cex = cex.text, pos=4)
    
    value.plot  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_taxa_aquatic_relcover_percent",dict))
    text(x = x.pos*2.5, y = y.pos[3], labels = ifelse(is.na(value.plot), "", round(value.plot,1)), cex = cex.text, pos=4)
    
    # helophyte species
    text(x = x.start,   y = y.pos[4], labels = ecoval.translate("R_macrophytes_doc_vegetation_helphytes",dict), cex = cex.text, pos=4)
    
    value.plot  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_taxa_helophytes_richness_count",dict))
    text(x = x.pos,     y = y.pos[4], labels = ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
    
    value.plot  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_taxa_helophytes_abscover_percent",dict))
    text(x = x.pos*1.5, y = y.pos[4], labels = ifelse(is.na(value.plot), "", round(value.plot,1)), cex = cex.text, pos=4)
    
    value.plot  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_taxa_helophytes_relcover_percent",dict))
    text(x = x.pos*2.5, y = y.pos[4], labels = ifelse(is.na(value.plot), "", round(value.plot,1)), cex = cex.text, pos=4)
    
    # bryophyte species
    text(x = x.start,   y = y.pos[5], labels = ecoval.translate("R_macrophytes_doc_vegetation_bryophytes",dict), cex = cex.text, pos=4)
    
    value.plot  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_taxa_bryophytes_richness_count",dict))
    text(x = x.pos,     y = y.pos[5], labels = ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
    
    value.plot  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_taxa_bryophytes_abscover_percent",dict))
    text(x = x.pos*1.5, y = y.pos[5], labels = ifelse(is.na(value.plot), "", round(value.plot,1)), cex = cex.text, pos=4)
    
    value.plot  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_taxa_bryophytes_relcover_percent",dict))
    value.plot1  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_taxa_bryophytes_adjusted_relcover_percent",dict))
    text(x = x.pos*2.5, y = y.pos[5], labels = ifelse(is.na(value.plot) | is.na(value.plot1), "", 
                                                      paste(round(value.plot,1), " (Adj.: ", round(value.plot1,1), ")", sep = "")), cex = cex.text, pos=4)
    
    # proportion bryophytes on artificial substrate
    text(x = x.start+0.1,   y = y.pos[6], labels = ecoval.translate("R_macrophytes_doc_vegetation_fractionartificial",dict), col = "grey60", cex = cex.text, pos=4)
    
    value.plot  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_taxa_bryophytes_artificialsubstrate_relcover_percent",dict))
    text(x = x.pos*2.5, y = y.pos[6], labels = ifelse(is.na(value.plot), "", round(value.plot,1)), col = "grey60", cex = cex.text, pos=4)
    
    # add growthform attribute information
    
    # all macrophytes
    text(x = 0,         y = y.pos[7]-para.small, labels = ecoval.translate("R_macrophytes_doc_vegetation_growthforms",dict), font = 2, cex = cex.text, pos=4)
    
    value.plot  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_growthform_all_richness_count",dict))
    text(x = x.pos, y = y.pos[7]-para.small, labels = ifelse(is.na(value.plot), "", value.plot), cex = cex.text, font=2, pos=4)
    
    # aquatic growthforms
    text(x = x.start,   y = y.pos[8]-para.small, labels = ecoval.translate("R_macrophytes_doc_vegetation_aquatic",dict), cex = cex.text, pos=4)
    
    value.plot  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_growthform_aquatic_richness_count",dict))
    text(x = x.pos, y = y.pos[8]-para.small, labels = ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
    
    # helophytic growthforms
    text(x = x.start,   y = y.pos[9]-para.small, labels = ecoval.translate("R_macrophytes_doc_vegetation_helphytes",dict), cex = cex.text, pos=4)
    
    value.plot  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_growthform_helophytes_richness_count",dict))
    text(x = x.pos, y = y.pos[9]-para.small, labels = ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
    
    # add filamentous algae and neophyte information
    
    # filamentous algae
    text(x = 0,         y = y.pos[10]-2*para.small, labels = ecoval.translate("R_macrophytes_doc_vegetation_algae",dict), cex = cex.text, font = 2, pos=4)
    
    value.plot  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_filamentousgreenalgae_abscover_percent",dict))
    text(x = x.pos*1.5, y = y.pos[10]-2*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,1)), cex = cex.text, font=2, pos=4)
    
    value.plot  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_filamentousgreenalgae_relcover_percent",dict))
    text(x = x.pos*2.5, y = y.pos[10]-2*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,1)), cex = cex.text, font=2, pos=4)
    
    # neophytes
    text(x = 0,         y = y.pos[11]-2*para.small, labels = ecoval.translate("R_macrophytes_doc_vegetation_neophytes",dict), cex = cex.text, font = 2, pos=4)
    
    value.plot  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_taxa_neophytes_richness_count",dict))
    text(x = x.pos, y = y.pos[11]-2*para.small, labels = ifelse(is.na(value.plot), "", value.plot), cex = cex.text, font=2, pos=4)
    
    value.plot  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_neophytes_relcover_percent",dict))/100*
                     (get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_allmacrophytes_abscover_percent",dict))+
                        get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_filamentousgreenalgae_abscover_percent",dict)))
    text(x = x.pos*1.5, y = y.pos[11]-2*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,1)), cex = cex.text, font=2, pos=4)
    
    value.plot  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_neophytes_relcover_percent",dict))
    text(x = x.pos*2.5, y = y.pos[11]-2*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,1)), cex = cex.text, font=2, pos=4)
    
    # add species quality and conservation status information
    
    # guidance species "Leitarten"
    text(x = 0,         y = y.pos[12]-3*para.small, labels = ecoval.translate("R_macrophytes_doc_vegetation_indexspecies",dict), cex = cex.text, font = 2, pos=4)
    
    # higher macrophytes
    text(x = x.start,   y = y.pos[13]-3*para.small, labels = ecoval.translate("R_macrophytes_doc_vegetation_macrophytes",dict), cex = cex.text, pos=4)
    
    value.plot  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_indexspeciesmac1_count",dict))
    value.plot1 <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_indexspeciesmac2_count",dict))
    value.plot2 <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_indexspeciesmac3_count",dict))
    
    n.leit <- sum(value.plot, value.plot1, value.plot2)
    
    text(x = x.pos,       y = y.pos[13]-3*para.small, cex = cex.text, pos=4,
         labels = ifelse(is.na(value.plot) | is.na(value.plot1) | is.na(value.plot2), "",
                         paste(ifelse(is.na(n.leit),"",n.leit),
                               " (",ecoval.translate("R_macrophytes_doc_vegetation_indexvalue",dict),"1: ", value.plot, 
                               "; ",ecoval.translate("R_macrophytes_doc_vegetation_indexvalue",dict),"2: ", value.plot1, 
                               "; ",ecoval.translate("R_macrophytes_doc_vegetation_indexvalue",dict),"3: ", value.plot2,")",sep="")))
    
    # bryophytes 
    text(x = x.start,   y = y.pos[14]-3*para.small, labels = ecoval.translate("R_macrophytes_doc_vegetation_bryophytes",dict), cex = cex.text, pos=4)
    
    value.plot  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_indexspeciesbry1_count",dict))
    value.plot1 <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_indexspeciesbry2_count",dict))
    value.plot2 <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_indexspeciesbry3_count",dict))
    
    n.leit <- sum(value.plot, value.plot1, value.plot2)
    
    text(x = x.pos,       y = y.pos[14]-3*para.small, cex = cex.text, pos=4,
         labels = ifelse(is.na(value.plot) | is.na(value.plot1) | is.na(value.plot2), "",
                         paste(ifelse(is.na(n.leit),"",n.leit),
                               " (",ecoval.translate("R_macrophytes_doc_vegetation_indexvalue",dict),"1: ", value.plot, 
                               "; ",ecoval.translate("R_macrophytes_doc_vegetation_indexvalue",dict),"2: ", value.plot1, 
                               "; ",ecoval.translate("R_macrophytes_doc_vegetation_indexvalue",dict),"3: ", value.plot2,")",sep="")))
    
    # priority of species "Prioritaet"
    text(x = 0,   y = y.pos[15]-4*para.small, labels = ecoval.translate("R_macrophytes_doc_vegetation_priorityspecies",dict), font = 2, cex = cex.text, pos=4)
    
    value.plot  <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_priorityspecies1_count",dict))
    value.plot1 <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_priorityspecies2_count",dict))
    value.plot2 <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_priorityspecies3_count",dict))
    value.plot3 <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_priorityspecies4_count",dict))
    
    n.prio <- sum(value.plot, value.plot1, value.plot2, value.plot3)
    
    text(x = x.pos, y = y.pos[15]-4*para.small, cex = cex.text, pos=4,
         labels = ifelse(is.na(value.plot) | is.na(value.plot1) | is.na(value.plot2) | is.na(value.plot3),"",
                         paste(ifelse(is.na(n.prio),"",n.prio),
                               " (",ecoval.translate("R_macrophytes_doc_vegetation_priorityvalue",dict),"1: ", value.plot, 
                               "; ",ecoval.translate("R_macrophytes_doc_vegetation_priorityvalue",dict),"2: ", value.plot1,
                               "; ",ecoval.translate("R_macrophytes_doc_vegetation_priorityvalue",dict),"3: ", value.plot2, 
                               "; ",ecoval.translate("R_macrophytes_doc_vegetation_priorityvalue",dict),"4: ", value.plot3,")",sep="")))
    
    
    ## WINDOW 4 - Species list with information for each taxon
    
    # General coordinates for plotting in this window
    x.start <- 0.1
    y.pos <- 8.71 - 0.31 * seq(0,30,1)
    para.small <- 0.31/2
    x.pos <- 2.2
    
    # Species list
    plot(0, 0, axes = axis.info, type="n", xaxs="i", yaxs="i", xlim=c(0,10),ylim=c(0,10))
    text(x = 0, y = 9.32, labels =  ecoval.translate("R_macrophytes_doc_vegetation_listoftaxa",dict), col = "dodgerblue", cex = cex.title, font = 2, pos=4)
    
    # General Headers
    text(x = x.pos*1.8, y = y.pos[1], labels = ecoval.translate("R_macrophytes_doc_vegetation_growthform",dict), cex = cex.text, font = 2, pos=4)
    text(x = x.pos*2.4, y = y.pos[1], labels = paste(ecoval.translate("R_macrophytes_doc_vegetation_coverage",dict),"[%]"), cex = cex.text, font = 2, pos=4)
    text(x = x.pos*3.0, y = y.pos[1], labels = paste(ecoval.translate("R_macrophytes_doc_vegetation_areafraction",dict),"[%]"), cex = cex.text, font = 2, pos=4)
    text(x = x.pos*3.6, y = y.pos[1], labels = ecoval.translate("R_macrophytes_doc_vegetation_speciesinfo",dict), cex = cex.text, font = 2, pos=4)
    text(x = x.pos*3.8, y = y.pos[1], labels = ecoval.translate("R_macrophytes_doc_vegetation_indexvalue",dict), cex = cex.text, font = 2, pos=4)
    text(x = x.pos*4.0, y = y.pos[1], labels = ecoval.translate("R_macrophytes_doc_vegetation_priorityvalue",dict), cex = cex.text, font = 2, pos=4)
    text(x = x.pos*4.2, y = y.pos[1], labels = ecoval.translate("R_macrophytes_doc_vegetation_redliststatus",dict), cex = cex.text, font = 2, pos=4)
    
    # Species according to growthform groups
    grfo.info <- c(ecoval.translate("L_macrophytes_taxalist_growthform_assess_aquatic",dict),
                   ecoval.translate("L_macrophytes_taxalist_growthform_assess_helophyte",dict),
                   ecoval.translate("L_macrophytes_taxalist_growthform_assess_bryophyte",dict))
    grfo.info.lab <- c(ecoval.translate("R_macrophytes_doc_vegetation_aquatic",dict),
                       ecoval.translate("R_macrophytes_doc_vegetation_helphytes",dict),
                       ecoval.translate("R_macrophytes_doc_vegetation_bryophytes",dict))
    plot.index <- 2
    plot.fact <- 0
    
    max.char <- 42
    for ( k in 1:3 ) {
      dat.plot <- data.species[data.species[,ecoval.translate("A_macrophytes_taxalist_growthform_assess",dict)] == grfo.info[k],]
      text(x = 0,   y = y.pos[plot.index]-plot.fact*para.small, labels = grfo.info.lab[k], cex = cex.text, font = 2, pos=4)
      plot.index <- plot.index+1
      
      # sort species alphabetically, except characea that are put to the end (again alphabetically)
      if ( nrow(dat.plot) > 0 )
      {
        ind.cha    <- dat.plot[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)] == 
          ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_cha",dict)
        ind.chasum <- dat.plot[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)] == 
          ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_chasum",dict)
        if ( sum(ind.cha) + sum(ind.chasum) == 0 )  # no characea
        {
          dat.plot <- dat.plot[order(dat.plot[,ecoval.translate("A_macrophytes_species_name_latin",dict)]),]
        }
        else
        {
          n.cha     <- sum(ind.cha)
          n.non.cha <- sum(!ind.cha&!ind.chasum)
          n.tot     <- nrow(dat.plot)
          # first non-characea:
          dat.plot <- dat.plot[c(which(!ind.cha&!ind.chasum),which(ind.chasum),which(ind.cha)),]
          # sort non-characea:
          if ( n.non.cha > 0 ) dat.plot[1:n.non.cha,] <- dat.plot[order(dat.plot[1:n.non.cha,ecoval.translate("A_macrophytes_species_name_latin",dict)]),] 
          if ( n.cha > 1 )     dat.plot[(n.tot-n.cha+1):n.tot,] <- dat.plot[n.tot-n.cha+order(dat.plot[(n.tot-n.cha+1):n.tot,ecoval.translate("A_macrophytes_species_name_latin",dict)]),] 
        }
      }
      
      if( k < 3 ) {
        value.plot1 <- NA
        value.plot2 <- NA
        if ( grfo.info[k] == ecoval.translate("L_macrophytes_taxalist_growthform_assess_aquatic",dict) )
        {
          value.plot1 <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_taxa_aquatic_abscover_percent",dict))
          value.plot2 <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_taxa_aquatic_relcover_percent",dict))
        }
        else
        {
          if ( grfo.info[k] == ecoval.translate("L_macrophytes_taxalist_growthform_assess_helophyte",dict) )
          {
            value.plot1 <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_taxa_helophytes_abscover_percent",dict))
            value.plot2 <- get.value(data.attrib,row.no,ecoval.translate("A_macrophytes_taxa_helophytes_relcover_percent",dict))
          }
        }
        text(x = x.pos*2.4,   y = y.pos[plot.index-1]-plot.fact*para.small,
             labels = ifelse(is.na(value.plot1),"",round(value.plot1,1)),
             cex = cex.text, font=2, pos=4)
        text(x = x.pos*3.0,   y = y.pos[plot.index-1]-plot.fact*para.small,
             labels = ifelse(is.na(value.plot2),"",round(value.plot2,1)),
             cex = cex.text, font=2, pos=4)
        if( nrow(dat.plot) > 0 ) {
          for ( j in 1:nrow(dat.plot) ) {
            value.plot <- get.value(dat.plot,j,ecoval.translate("A_macrophytes_species_name_latin",dict))
            value.plot <- ifelse(nchar(value.plot)<=max.char,value.plot,paste(substring(value.plot,1,max.char),"..."))
            
            # shift names of characea:
            if ( dat.plot[j,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)] == 
                   ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_cha",dict) )
            {
              text(x = x.start+0.1,   y = y.pos[plot.index]-plot.fact*para.small, labels = ifelse(is.na(value.plot), "", value.plot),
                   cex = cex.text, col = "grey60", pos=4)
            }
            else
            {
              text(x = x.start,   y = y.pos[plot.index]-plot.fact*para.small, labels = ifelse(is.na(value.plot), "", value.plot),
                   cex = cex.text, pos=4)
            }
            
            value.plot  <- get.value(dat.plot,j,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict))
            text(x = x.pos*1.9, y = y.pos[plot.index]-plot.fact*para.small, labels = ifelse(is.na(value.plot), "", value.plot),
                 cex = cex.text, pos=4)
            
            # write coverage in grey for characea:
            if ( dat.plot[j,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)] == 
                   ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_cha",dict) )
            {
              value.plot  <- get.value(dat.plot,j,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict))
              text(x = x.pos*2.4, y = y.pos[plot.index]-plot.fact*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,1)),
                   cex = cex.text, col = "grey60", pos=4)
            
              value.plot  <- get.value(dat.plot,j,"DG.Rel")
              text(x = x.pos*3.0, y = y.pos[plot.index]-plot.fact*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,1)),
                   cex = cex.text, col = "grey60", pos=4)
            }
            else
            {
              value.plot  <- get.value(dat.plot,j,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict))
              text(x = x.pos*2.4, y = y.pos[plot.index]-plot.fact*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,1)),
                   cex = cex.text, pos=4)
              
              value.plot  <- get.value(dat.plot,j,"DG.Rel")
              text(x = x.pos*3.0, y = y.pos[plot.index]-plot.fact*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,1)),
                   cex = cex.text, pos=4)
            }
            
            value.plot  <- get.value(dat.plot,j,"Artinfo")
            text(x = x.pos*3.6, y = y.pos[plot.index]-plot.fact*para.small, labels = ifelse(is.na(value.plot), "", value.plot),
                 cex = cex.text, pos=4)
            
            value.plot  <- get.value(dat.plot,j,"Leitwert")
            text(x = x.pos*3.8, y = y.pos[plot.index]-plot.fact*para.small, labels = ifelse(is.na(value.plot), "", value.plot),
                 cex = cex.text, pos=4)
            
            value.plot  <- get.value(dat.plot,j,"Prioritaet")
            text(x = x.pos*4.0, y = y.pos[plot.index]-plot.fact*para.small, labels = ifelse(is.na(value.plot), "", value.plot),
                 cex = cex.text, pos=4)
            
            value.plot  <- get.value(dat.plot,j,"Rote.Liste")
            text(x = x.pos*4.2, y = y.pos[plot.index]-plot.fact*para.small, labels = ifelse(is.na(value.plot), "", value.plot),
                 cex = cex.text, pos=4)
            plot.index <- plot.index+1
          }
          plot.fact <- plot.fact+1
        } else {
          text(x = x.start,   y = y.pos[plot.index]-plot.fact*para.small, labels = "-", cex = cex.text, pos=4)
          plot.fact <- plot.fact+1
          plot.index <- plot.index+1
        }
      }
      
      if( k == 3 ) {
        if( nrow(dat.plot) > 0 ) {
          value.plot <- get.value(dat.plot,
                                  which(dat.plot[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)] == ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_brysum",dict)),
                                  ecoval.translate("A_macrophytes_species_absolutecover_percent",dict))
          text(x = x.pos*2.4,   y = y.pos[plot.index-1]-plot.fact*para.small,
               labels = ifelse(is.na(value.plot),"",round(value.plot,1)),
               cex = cex.text, font=2, pos=4)
          value.plot <- get.value(dat.plot,
                                  which(dat.plot[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)] == ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_brysum",dict)),
                                  "DG.Rel")
          text(x = x.pos*3.0,   y = y.pos[plot.index-1]-plot.fact*para.small,
               labels = ifelse(is.na(value.plot),"",round(value.plot,1)),
               cex = cex.text, font=2, pos=4)
          
          dat.plot <- dat.plot[dat.plot[,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict)] != ecoval.translate("L_macrophytes_taxalist_growthform_abbrev_brysum",dict),]
          
          for ( j in 1:nrow(dat.plot) ) {
            value.plot <- get.value(dat.plot,j,ecoval.translate("A_macrophytes_species_name_latin",dict))
            value.plot <- ifelse(nchar(value.plot)<=max.char,value.plot,paste(substring(value.plot,1,max.char),"..."))
            text(x = x.start,   y = y.pos[plot.index]-plot.fact*para.small, labels = ifelse(is.na(value.plot), "", value.plot),
                 cex = cex.text, pos=4)
            
            value.plot  <- get.value(dat.plot,j,ecoval.translate("A_macrophytes_taxalist_growthform_abbrev",dict))
            text(x = x.pos*1.9, y = y.pos[plot.index]-plot.fact*para.small, labels = ifelse(is.na(value.plot), "", value.plot),
                 cex = cex.text, pos=4)
            
            value.plot  <- get.value(dat.plot,j,ecoval.translate("A_macrophytes_species_absolutecover_percent",dict))
            text(x = x.pos*2.4, y = y.pos[plot.index]-plot.fact*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,1)),
                 cex = cex.text, pos=4)
      
            value.plot  <- get.value(dat.plot,j,"DG.Rel")
            text(x = x.pos*3.0, y = y.pos[plot.index]-plot.fact*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,1)),
                 cex = cex.text, pos=4)

            value.plot  <- get.value(dat.plot,j,"Artinfo")
            text(x = x.pos*3.6, y = y.pos[plot.index]-plot.fact*para.small, labels = ifelse(is.na(value.plot), "", value.plot),
                 cex = cex.text, pos=4)
            
            value.plot  <- get.value(dat.plot,j,"Leitwert")
            text(x = x.pos*3.8, y = y.pos[plot.index]-plot.fact*para.small, labels = ifelse(is.na(value.plot), "", value.plot),
                 cex = cex.text, pos=4)
            
            value.plot  <- get.value(dat.plot,j,"Prioritaet")
            text(x = x.pos*4.0, y = y.pos[plot.index]-plot.fact*para.small, labels = ifelse(is.na(value.plot), "", value.plot),
                 cex = cex.text, pos=4)
            
            value.plot  <- get.value(dat.plot,j,"Rote.Liste")
            text(x = x.pos*4.2, y = y.pos[plot.index]-plot.fact*para.small, labels = ifelse(is.na(value.plot), "", value.plot),
                 cex = cex.text, pos=4)
            plot.index <- plot.index+1
          }
          plot.fact <- plot.fact+1
        } else {
          text(x = x.start,   y = y.pos[plot.index]-plot.fact*para.small, labels = "-", cex = cex.text, pos=4)
          plot.fact <- plot.fact+1
          plot.index <- plot.index+1
        }
      }
    }
  }
  
  # Write footer
  plot(0, 0, axes = axis.info, type="n", xaxs="i", yaxs="i", xlim=c(0,10),ylim=c(0,10))
  abline(h = 8)
  
  text(x = 10, y = 5, format(Sys.Date(), "%d.%m.%Y"), pos = 2)
}

# Function for valuation information A4 PDF
# =========================================
# Function
msk.macrophytes.2017.doc.valuation <- function(res,row.no) {
  
  # get dictionary:
  dict <- ecoval.dict(res$language,res$dictionaries)
  
  # define functions for plotting
  get.value <- function(data.plot, row.no, colname) {
    value.plot <- NA
    if ( length(row.no)>0 & length(colname)>0 )
    {
      if ( row.no >= 1 & row.no <= nrow(data.plot) & colname != "" & sum(colname == colnames(data.plot)) != 0 ) value.plot <- data.plot[row.no,colname]
    }
    return(value.plot)
  }
  
  # create function to translate continous valuation in quality class
  calc.class <- function (u, classes) 
  {
    class.ind <- 1 + floor(u * length(classes) + 1e-15)
    class.ind <- ifelse(class.ind > length(classes), length(classes), class.ind)
    class <- classes[class.ind]
    class <- ifelse(is.na(class.ind), 0, class)
    return(class)
  }
  
  # check if only original or original and final valuation is available, if the latter two factsheets are plotted
  if ( is.na(match("types.val.obs",names(res))) ) return()
  plot.versions <- "orig"
  if ( !is.na(match("types.val.final",names(res))) ) plot.versions <- c("final",plot.versions)
  
  # loop to plot both original and final valuation
  for ( plot.version in plot.versions ) {
    
    # get data for plotting
    data.site <- res$data.site
    
    if( plot.version == "orig" ) {
      data.val <- res$val
      #header.text <- "Bewertung / Original"
      river.type <- res$types.val.obs[row.no] 
      final.id <- FALSE
    } else {
      data.val <- res$val.final
      #header.text <- "Bewertung / Plausibilisiert"
      river.type <- res$types.val.final[row.no]
      final.id <- TRUE
    }
    
    # test if river type is bryophyte river
    moss <- c(ecoval.translate("L_macrophytes_rivertype_class_smallbryophyte",dict),
              ecoval.translate("L_macrophytes_rivertype_class_mediumbryophyte",dict),
              ecoval.translate("L_macrophytes_rivertype_class_largebryophyte",dict),
              ecoval.translate("L_macrophytes_rivertype_class_verylargebryophyte",dict))
    
    moss <- sum(river.type %in% moss, na.rm=T) > 0
    
    # expansion factors for fonts in plot
    cex.title   <- 1.5
    cex.text    <- 1.2
    
    # plot matrix for layout function
    layout.mat <- matrix(c(0,1,0,
                           0,2,0,
                           0,3,0,
                           0,4,0,
                           0,5,0,
                           0,6,0), ncol = 3, nrow = 6, byrow = T)
    layout.mat.widths  <- c(1.0,16,1.0)
    layout.mat.heights <- c(1.5,1.75,9,2,12,1.5)
    
    
    axis.info <- FALSE
    
    # create layout for PDF and plot data
    layout.plot.valuation <- layout(layout.mat,
                                    widths  = layout.mat.widths,
                                    heights = layout.mat.heights,
                                    respect = FALSE)
    
    #layout.show(layout.plot.valuation)
    
    par(mai = c(0,0,0,0))
    
    ## HEADER
    plot(0, 0, axes = axis.info, type="n", xaxs="i", yaxs="i", xlim=c(0,10), ylim=c(0,10))
    text(x = 0, y = 5, labels = ecoval.translate("R_macrophytes_doc_valuation",dict), font = 2, cex = cex.title, pos=4)
    abline(h = 3)
    
    ## WINDOW 2 - Basic information of sampling event as header in grey box
    plot(0, 0, axes = axis.info, type="n", xaxs="i", yaxs="i", xlim=c(0,10), ylim=c(0,10))
    rect(0,0.5,10,9.5, col = "grey85", border = NA)
    
    # Line 1
    text(x = 0, y = 7.5,   labels =  paste(ecoval.translate("R_macrophytes_doc_codesite",dict),":",sep=""), cex = cex.text, pos=4, font=2)
    value.plot <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_siteid",dict))
    text(x = 1.4, y = 7.5, labels =  ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
    
    text(x = 2.7, y = 7.5, labels =  paste(ecoval.translate("R_macrophytes_doc_canton",dict),":",sep=""), cex = cex.text, pos=4, font = 2)
    value.plot <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_canton",dict))
    text(x = 4, y = 7.5,   labels = ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
    
    text(x = 6.5, y = 7.5,   labels =  paste(ecoval.translate("R_macrophytes_doc_coordinates",dict),":",sep=""), cex = cex.text, pos=4, font = 2)
    value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_coordinatex_swissgrid",dict))
    value.plot1 <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_coordinatey_swissgrid",dict))
    text(x = 8.3, y = 7.5, labels =  paste(ifelse(is.na(value.plot), "", value.plot),
                                           ifelse(is.na(value.plot1), "", value.plot1), sep = " / "),
         cex = cex.text, pos=4)
    
    # Line 2
    text(x = 0, y = 5,   labels =  paste(ecoval.translate("R_macrophytes_doc_river",dict),":",sep=""), cex = cex.text, pos=4, font=2)
    value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_waterbody",dict))
    text(x = 1.4, y = 5, labels = ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
    
    text(x = 2.7, y = 5, labels =  paste(ecoval.translate("R_macrophytes_doc_location",dict),":",sep=""), cex = cex.text, pos=4, font = 2)
    value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_location",dict))
    text(x = 4, y = 5,   labels =  ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
    
    # Line 3
    text(x = 0, y = 2.5,   labels =  paste(ecoval.translate("R_macrophytes_doc_samplingdate",dict),":",sep=""), cex = cex.text, pos=4, font=2)
    value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_samplingdate",dict))
    text(x = 1.4, y = 2.5, labels = ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
    
    text(x = 2.7, y = 2.5, labels =  paste(ecoval.translate("R_macrophytes_doc_observer",dict),":",sep=""), cex = cex.text, pos=4, font = 2)
    value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_operator",dict))
    text(x = 4, y = 2.5,   labels =  ifelse(is.na(value.plot), "", value.plot), cex = cex.text, pos=4)
    
    text(x = 6.5, y = 2.5,   labels =  paste(ecoval.translate("R_macrophytes_doc_sectionlength",dict),":",sep=""), cex = cex.text, pos=4, font = 2)
    value.plot  <- get.value(data.site,row.no,ecoval.translate("A_macrophytes_site_lengthsection_m",dict))
    text(x = 8.3, y = 2.5, labels =  ifelse(is.na(value.plot), "", paste(value.plot, "m")), cex = cex.text, pos=4)
    
    
    ## WINDOW 3 - Valuation of assessment goals
    
    # Set plot parameters for this window
    x.start <- 0.1
    y.pos <- 8.25 - 0.5 * seq(0,20,1)
    para.small <- 0.5/2
    x.pos <- 2.3
    
    # Define MSK colors
    msk.colors <- c("white",utility.calc.colors(n = 5))
    
    # Valuation of assessment goals
    plot(0, 0, axes = axis.info, type="n", xaxs="i", yaxs="i", xlim=c(0,10),ylim=c(0,10))
    text(x = 0, y = 9, labels =  ecoval.translate("R_macrophytes_doc_valuationobjectives",dict), col = "dodgerblue", cex = cex.title, font = 2, pos=4)
    
    # Overall assessment
    text(x = 0,         y = y.pos[1], labels = ecoval.translate("N_macrophytes_goodstate",dict), cex = cex.text, font = 2, pos=4)
    node.header <- ifelse(!moss,ecoval.translate("N_macrophytes_goodstate",dict),
                          paste(ecoval.translate("N_macrophytes_goodstate",dict),
                                ecoval.translate("N_macrophytes_bryophytesriver",dict)))
    value.plot <- get.value(data.val,row.no,node.header)
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.pos*1.6,y.pos[1]-0.17,x.pos*1.6+0.6,y.pos[1]+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.pos*1.6, y = y.pos[1], labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    text(x = x.pos*2.3, y = y.pos[1], labels = ecoval.translate("N_macrophytes_typicalcommunity",dict), cex = cex.text, font = 2, pos=4)
    node.header <- ifelse(!moss,ecoval.translate("N_macrophytes_typicalcommunity",dict),
                          paste(ecoval.translate("N_macrophytes_typicalcommunity",dict),
                                ecoval.translate("N_macrophytes_bryophytesriver",dict)))
    value.plot <- get.value(data.val,row.no,node.header)
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.start+x.pos*4.0,y.pos[1]-0.17,x.start+x.pos*4.0+0.6,y.pos[1]+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.start+x.pos*4.0, y = y.pos[1], labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    # Individual assessment goals
    
    # Diversity
    text(x = 0,         y = y.pos[2]-para.small, labels = ecoval.translate("N_macrophytes_typicaldiversity",dict), cex = cex.text, font = 2, pos=4)
    node.header <- ifelse(!moss,ecoval.translate("N_macrophytes_typicaldiversity",dict),
                          paste(ecoval.translate("N_macrophytes_typicaldiversity",dict),
                                ecoval.translate("N_macrophytes_bryophytesriver",dict)))
    value.plot <- get.value(data.val,row.no,node.header)
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.pos*1.6,y.pos[2]-para.small-0.17,x.pos*1.6+0.6,y.pos[2]-para.small+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.pos*1.6, y = y.pos[2]-para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    # Species
    text(x = x.start,   y = y.pos[3]-2*para.small, labels = ecoval.translate("N_macrophytes_manytaxa",dict), cex = cex.text, pos=4)
    node.header <- ifelse(!moss,ecoval.translate("N_macrophytes_manytaxa",dict),
                          paste(ecoval.translate("N_macrophytes_manytaxa",dict),
                                ecoval.translate("N_macrophytes_bryophytesriver",dict)))
    value.plot <- get.value(data.val,row.no,node.header)
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.pos*1.6,y.pos[3]-2*para.small-0.17,x.pos*1.6+0.6,y.pos[3]-2*para.small+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.pos*1.6, y = y.pos[3]-2*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    text(x = x.start,   y = y.pos[4]-2*para.small, labels = ecoval.translate("N_macrophytes_manyaquatic",dict), cex = cex.text, pos=4)
    value.plot <- get.value(data.val,row.no,ecoval.translate("N_macrophytes_manyaquatic",dict))
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.pos*1.6,y.pos[4]-2*para.small-0.17,x.pos*1.6+0.6,y.pos[4]-2*para.small+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.pos*1.6, y = y.pos[4]-2*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    text(x = x.start,   y = y.pos[5]-2*para.small, labels = ecoval.translate("N_macrophytes_manyhelophytes",dict), cex = cex.text, pos=4)
    value.plot <- get.value(data.val,row.no,ecoval.translate("N_macrophytes_manyhelophytes",dict))
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.pos*1.6,y.pos[5]-2*para.small-0.17,x.pos*1.6+0.6,y.pos[5]-2*para.small+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.pos*1.6, y = y.pos[5]-2*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    text(x = x.start,   y = y.pos[6]-2*para.small, labels = ecoval.translate("N_macrophytes_manybryophytes",dict), cex = cex.text, pos=4)
    node.header <- ifelse(!moss,ecoval.translate("N_macrophytes_manybryophytes",dict),
                          paste(ecoval.translate("N_macrophytes_manybryophytes",dict),
                                ecoval.translate("N_macrophytes_bryophytesriver",dict)))
    value.plot <- get.value(data.val,row.no,node.header)
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.pos*1.6,y.pos[6]-2*para.small-0.17,x.pos*1.6+0.6,y.pos[6]-2*para.small+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.pos*1.6, y = y.pos[6]-2*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    
    # Growthforms
    text(x = x.start,   y = y.pos[7]-3*para.small, labels = ecoval.translate("N_macrophytes_manygrowthforms",dict), cex = cex.text, pos=4)
    value.plot <- get.value(data.val,row.no,ecoval.translate("N_macrophytes_manygrowthforms",dict)) # Mac
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.pos*1.6,y.pos[7]-3*para.small-0.17,x.pos*1.6+0.6,y.pos[7]-3*para.small+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.pos*1.6, y = y.pos[7]-3*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    text(x = x.start,   y = y.pos[8]-3*para.small, labels = ecoval.translate("N_macrophytes_manygrowthformsaquatic",dict), cex = cex.text, pos=4)
    value.plot <- get.value(data.val,row.no,ecoval.translate("N_macrophytes_manygrowthformsaquatic",dict)) # Mac
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.pos*1.6,y.pos[8]-3*para.small-0.17,x.pos*1.6+0.6,y.pos[8]-3*para.small+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.pos*1.6, y = y.pos[8]-3*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    text(x = x.start,   y = y.pos[9]-3*para.small, labels = ecoval.translate("N_macrophytes_manygrowthformshelophytes",dict), cex = cex.text, pos=4)
    value.plot <- get.value(data.val,row.no,ecoval.translate("N_macrophytes_manygrowthformshelophytes",dict)) # Mac
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.pos*1.6,y.pos[9]-3*para.small-0.17,x.pos*1.6+0.6,y.pos[9]-3*para.small+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.pos*1.6, y = y.pos[9]-3*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    # Community composition
    text(x = x.pos*2.3, y = y.pos[2]-para.small, labels = ecoval.translate("N_macrophytes_typicalcomposition",dict), cex = cex.text, font = 2, pos=4)
    node.header <- ifelse(!moss,ecoval.translate("N_macrophytes_typicalcomposition",dict),
                          paste(ecoval.translate("N_macrophytes_typicalcomposition",dict),
                                ecoval.translate("N_macrophytes_bryophytesriver",dict)))
    value.plot <- get.value(data.val,row.no,ecoval.translate("N_macrophytes_typicalcomposition",dict))
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.start+x.pos*4.0,y.pos[2]-para.small-0.17,x.start+x.pos*4.0+0.6,y.pos[2]-para.small+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.start+x.pos*4.0, y = y.pos[2]-para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    # Algae and Neophytes
    text(x = x.start+x.pos*2.3, y = y.pos[3]-2*para.small, labels = ecoval.translate("N_macrophytes_fractalgae",dict), cex = cex.text, pos=4)
    node.header <- ifelse(!moss,ecoval.translate("N_macrophytes_fractalgae",dict),
                          paste(ecoval.translate("N_macrophytes_fractalgae",dict),
                                ecoval.translate("N_macrophytes_bryophytesriver",dict)))
    value.plot <- get.value(data.val,row.no,node.header)
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.start+x.pos*4.0,y.pos[3]-2*para.small-0.17,x.start+x.pos*4.0+0.6,y.pos[3]-2*para.small+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.start+x.pos*4.0, y = y.pos[3]-2*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    text(x = x.start+x.pos*2.3, y = y.pos[4]-2*para.small, labels = ecoval.translate("N_macrophytes_neophytes",dict), cex = cex.text, pos=4)
    node.header <- ifelse(!moss,ecoval.translate("N_macrophytes_neophytes",dict),
                          paste(ecoval.translate("N_macrophytes_neophytes",dict),
                                ecoval.translate("N_macrophytes_bryophytesriver",dict)))
    value.plot <- get.value(data.val,row.no,node.header)
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.start+x.pos*4.0,y.pos[4]-2*para.small-0.17,x.start+x.pos*4.0+0.6,y.pos[4]-2*para.small+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.start+x.pos*4.0, y = y.pos[4]-2*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    # Macrophytes
    text(x = x.start+x.pos*2.3, y = y.pos[5]-3*para.small, labels = ecoval.translate("N_macrophytes_fractaquatic",dict), cex = cex.text, pos=4)
    value.plot <- get.value(data.val,row.no,ecoval.translate("N_macrophytes_fractaquatic",dict)) # Mac
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.start+x.pos*4.0,y.pos[5]-3*para.small-0.17,x.start+x.pos*4.0+0.6,y.pos[5]-3*para.small+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.start+x.pos*4.0, y = y.pos[5]-3*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    text(x = x.start+x.pos*2.3, y = y.pos[6]-3*para.small, labels = ecoval.translate("N_macrophytes_fracthelophytes",dict), cex = cex.text, pos=4)
    value.plot <- get.value(data.val,row.no,ecoval.translate("N_macrophytes_fracthelophytes",dict)) # Mac
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.start+x.pos*4.0,y.pos[6]-3*para.small-0.17,x.start+x.pos*4.0+0.6,y.pos[6]-3*para.small+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.start+x.pos*4.0, y = y.pos[6]-3*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    text(x = x.start+x.pos*2.3, y = y.pos[7]-3*para.small, labels = ecoval.translate("N_macrophytes_fractbryophytesadj",dict), cex = cex.text, pos=4)
    node.header <- ifelse(!moss,ecoval.translate("N_macrophytes_fractbryophytesadj",dict),
                          paste(ecoval.translate("N_macrophytes_fractbryophytesadj",dict),
                                ecoval.translate("N_macrophytes_bryophytesriver",dict)))
    value.plot <- get.value(data.val,row.no,ecoval.translate("N_macrophytes_fractbryophytesadj",dict)) # Mac-Bry
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.start+x.pos*4.0,y.pos[7]-3*para.small-0.17,x.start+x.pos*4.0+0.6,y.pos[7]-3*para.small+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.start+x.pos*4.0, y = y.pos[7]-3*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    # Dominance structure
    text(x = x.start+x.pos*2.3, y = y.pos[8]-4*para.small, labels = ecoval.translate("N_macrophytes_dominance",dict), cex = cex.text, pos=4)
    value.plot <- get.value(data.val,row.no,ecoval.translate("N_macrophytes_dominance",dict))
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.start+x.pos*4.0,y.pos[8]-4*para.small-0.17,x.start+x.pos*4.0+0.6,y.pos[8]-4*para.small+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.start+x.pos*4.0, y = y.pos[8]-4*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    
    # Biomass / Absolute cover
    text(x = 0,         y = y.pos[10]-5*para.small, labels = ecoval.translate("N_macrophytes_typicalcover",dict), font = 2, cex = cex.text, pos=4)
    value.plot <- get.value(data.val,row.no,ecoval.translate("N_macrophytes_typicalcover",dict))
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.pos*1.6,y.pos[10]-5*para.small-0.17,x.pos*1.6+0.6,y.pos[10]-5*para.small+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.pos*1.6, y = y.pos[10]-5*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    text(x = x.start,   y = y.pos[11]-6*para.small, labels = ecoval.translate("N_macrophytes_cover",dict), cex = cex.text, pos=4)
    value.plot <- get.value(data.val,row.no,ecoval.translate("N_macrophytes_cover",dict))
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.pos*1.6,y.pos[11]-6*para.small-0.17,x.pos*1.6+0.6,y.pos[11]-6*para.small+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.pos*1.6, y = y.pos[11]-6*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    text(x = x.start,   y = y.pos[12]-6*para.small, labels = ecoval.translate("N_macrophytes_coveralgae",dict), cex = cex.text, pos=4)
    node.header <- ifelse(!moss,ecoval.translate("N_macrophytes_coveralgae",dict),
                          paste(ecoval.translate("N_macrophytes_coveralgae",dict),
                                ecoval.translate("N_macrophytes_bryophytesriver",dict)))
    value.plot <- get.value(data.val,row.no,node.header)
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.pos*1.6,y.pos[12]-6*para.small-0.17,x.pos*1.6+0.6,y.pos[12]-6*para.small+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.pos*1.6, y = y.pos[12]-6*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    # Species quality
    text(x = x.pos*2.3,         y = y.pos[10]-5*para.small, labels = ecoval.translate("N_macrophytes_highqualitytaxa",dict), font = 2, cex = cex.text, pos=4)
    node.header <- ifelse(!moss,ecoval.translate("N_macrophytes_highqualitytaxa",dict),
                          paste(ecoval.translate("N_macrophytes_highqualitytaxa",dict),
                                ecoval.translate("N_macrophytes_bryophytesriver",dict)))
    value.plot <- get.value(data.val,row.no,node.header)
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.start+x.pos*4.0,y.pos[10]-5*para.small-0.17,x.start+x.pos*4.0+0.6,y.pos[10]-5*para.small+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.start+x.pos*4.0, y = y.pos[10]-5*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    text(x = x.start+x.pos*2.3, y = y.pos[11]-6*para.small, labels = ecoval.translate("N_macrophytes_indexspeciesmac",dict), cex = cex.text, pos=4)
    node.header <- ifelse(!moss,ecoval.translate("N_macrophytes_indexspeciesmac",dict),
                          paste(ecoval.translate("N_macrophytes_indexspeciesmac",dict),
                                ecoval.translate("N_macrophytes_bryophytesriver",dict)))
    value.plot <- get.value(data.val,row.no,ecoval.translate("N_macrophytes_indexspeciesmac",dict))
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.start+x.pos*4.0,y.pos[11]-6*para.small-0.17,x.start+x.pos*4.0+0.6,y.pos[11]-6*para.small+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.start+x.pos*4.0, y = y.pos[11]-6*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    text(x = x.start+x.pos*2.3, y = y.pos[12]-6*para.small, labels = ecoval.translate("N_macrophytes_indexspeciesbry",dict), cex = cex.text, pos=4)
    node.header <- ifelse(!moss,ecoval.translate("N_macrophytes_indexspeciesbry",dict),
                          paste(ecoval.translate("N_macrophytes_indexspeciesbry",dict),
                                ecoval.translate("N_macrophytes_bryophytesriver",dict)))
    value.plot <- get.value(data.val,row.no,ecoval.translate("N_macrophytes_indexspeciesbry",dict))
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.start+x.pos*4.0,y.pos[12]-6*para.small-0.17,x.start+x.pos*4.0+0.6,y.pos[12]-6*para.small+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.start+x.pos*4.0, y = y.pos[12]-6*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    text(x = x.start+x.pos*2.3, y = y.pos[13]-7*para.small, labels = ecoval.translate("N_macrophytes_priorityspecies",dict), cex = cex.text, pos=4)
    node.header <- ifelse(!moss,ecoval.translate("N_macrophytes_priorityspecies",dict),
                          paste(ecoval.translate("N_macrophytes_priorityspecies",dict),
                                ecoval.translate("N_macrophytes_bryophytesriver",dict)))
    value.plot <- get.value(data.val,row.no,ecoval.translate("N_macrophytes_priorityspecies",dict))
    val.col  <- calc.class(value.plot, classes = c(1,1,2,2,3,3,4,4,5,5))
    rect(x.start+x.pos*4.0,y.pos[13]-7*para.small-0.17,x.start+x.pos*4.0+0.6,y.pos[13]-7*para.small+0.24, col = msk.colors[val.col+1], border = NA)
    text(x = x.start+x.pos*4.0, y = y.pos[13]-7*para.small, labels = ifelse(is.na(value.plot), "", round(value.plot,2)), cex = cex.text, pos=4)
    
    ## WINDOW 4 - Header for Value function plot
    par(mai = c(0,0,0,0))
    plot(0, 0, axes = axis.info, type="n", xaxs="i", yaxs="i", xlim=c(0,5), ylim=c(0,10))
    text(x = 0, y = 3, labels = ecoval.translate("R_macrophytes_doc_valuationhierarchy",dict), col = "dodgerblue",
         cex = cex.title, pos=4, font=2)
    
    
    # WINDOW 5 - Valuation plotted as value hierachy
    if( !final.id ) main.text <- paste(ecoval.translate("R_macrophytes_doc_rivertype",dict),": ", ifelse(is.na(river.type),"",river.type), " (",ecoval.translate("R_macrophytes_doc_typeorig",dict),")", sep = "")
    if(  final.id ) main.text <- paste(ecoval.translate("R_macrophytes_doc_rivertype",dict),": ", ifelse(is.na(river.type),"",river.type), " (",ecoval.translate("R_macrophytes_doc_typefinal",dict),")", sep = "")
    
    msk.macrophytes.2017.plot.hierarchy(res, row.no, final = final.id,
                                        cex.main = 1.25, cex.nodes = 0.8,
                                        with.attrib = FALSE, main = main.text)
    
    ## Window 6 - Footer
    plot(0, 0, axes = axis.info, type="n", xaxs="i", yaxs="i", xlim=c(0,10),ylim=c(0,10))
    abline(h = 8)
    text(x = 10, y = 5, format(Sys.Date(), "%d.%m.%Y"), pos = 2)
  }
}


