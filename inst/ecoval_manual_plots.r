# Plots of for manual
# ===================

if ( !require(ecoval) ) { install.packages("ecoval"); library(ecoval) }

dir.fig <- "./ecoval/man/figures"

pdf.lake.morphol.2016.width  <- 18
pdf.lake.morphol.2016.height <-  9
  
png.lake.morphol.2016.width  <- 1600
png.lake.morphol.2016.height <-  800
  


# lake shore morphology:
# ----------------------

lakeshore <- lake.morphol.2016.create()
pdf(paste(dir.fig,"lakemorphol2016.pdf",sep="/"),width=pdf.lake.morphol.2016.width,height=pdf.lake.morphol.2016.height)
plot(lakeshore,two.lines=TRUE,with.attrib=FALSE)
dev.off()
png(paste(dir.fig,"lakemorphol2016.png",sep="/"),width=png.lake.morphol.2016.width,height=png.lake.morphol.2016.height)
plot(lakeshore,two.lines=TRUE,with.attrib=FALSE)
dev.off()

lakeshore.German <- lake.morphol.2016.create("Deutsch")
pdf(paste(dir.fig,"lakemorphol2016German.pdf",sep="/"),width=pdf.lake.morphol.2016.width,height=pdf.lake.morphol.2016.height)
plot(lakeshore.German,two.lines=TRUE,with.attrib=FALSE)
dev.off()
png(paste(dir.fig,"lakemorphol2016German.png",sep="/"),width=png.lake.morphol.2016.width,height=png.lake.morphol.2016.height)
plot(lakeshore.German,two.lines=TRUE,with.attrib=FALSE)
dev.off()

lakeshore.French <- lake.morphol.2016.create("Francais")
pdf(paste(dir.fig,"lakemorphol2016French.pdf",sep="/"),width=pdf.lake.morphol.2016.width,height=pdf.lake.morphol.2016.height)
plot(lakeshore.French,two.lines=TRUE,with.attrib=FALSE)
dev.off()
png(paste(dir.fig,"lakemorphol2016French.png",sep="/"),width=png.lake.morphol.2016.width,height=png.lake.morphol.2016.height)
plot(lakeshore.French,two.lines=TRUE,with.attrib=FALSE)
dev.off()



