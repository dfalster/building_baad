
my_cols <- function(){
  c(b_blue="#6baed6", 
    b_grey="#969696", 
    b_purple="#9e9ac8", 
    b_green="#74c476", 
    b_pink="#e377c2", 
    b_orange="#FD8D3C",
    b_darkgrey="#2D2D2D"
  )
}

baad_variable_count <- function(baad) {

  par(oma=c(0,0,0,0), mar = c(4,13, 0.0,1.5))
  cols <- my_cols()

  data <- baad[["data"]][, sapply(baad[["data"]], is.numeric) & 
                !names(baad[["data"]]) %in% 
                c( "latitude", "longitude", "map", "mat", "lai")]

  records <- sort(sapply(data, function(x) sum(!is.na(x))))
  records <- records[records > 500]
  lab <- baad$dictionary$label[match(names(records), baad$dictionary$variable)]

  barplot(records, horiz=TRUE,
    las=1, names.arg= lab, cex = 1, cex.names =1, 
    col=cols["b_orange"], border =NA, xlim=c(0,2E4))
  mtext("Number of records", 1, line=3, cex=1.0)
}

allometry_LH <- function(baad, option=1){

  cols <- my_cols()

  data <- baad[["data"]]

  par(mar=c(5,5,1,1))

  A <- data[["a.lf"]]
  H <- data[["h.t"]]
  G <- data[["species"]]

  ii <- data$studyName == "Martin1998"

  ax <- seq(-5, 3, by=2)
  lab <- do.call(expression, lapply(ax, function(i) bquote(10^.(i))))

  ax2 <- seq(-5, 3)
  lab2 <- do.call(expression, lapply(ax2, function(i) bquote(10^.(i))))

  sm1 <- sma(H[ii] ~ A[ii] * G[ii], log = "xy")
  sm2 <- sma(H ~ A * G, log = "xy")

  blank_plot(xlim = c(1E-5, 5E3), ylim=c(0.001, 200), log = "xy")
  axis(1, at = 10^ax, labels = lab, las = 1, cex.axis = 0.8)
  axis(2, at = 10^ax2, labels = lab2, las = 1, cex.axis = 0.8)

  x <- c(1E-5, 5E3)
  y <- 5.44*x^0.306

  if(option==1){
    plot(sm1, add = TRUE, col = cols["b_orange"], pch = 19, lwd = 0,
    cex = 1, type = "p")
    lines(x,y, col = cols["b_orange"], lwd = 1, lty="dashed")
    plot(sm1, add = TRUE, col = cols["b_green"], lwd = 1, type = "l")
    species <- G[!is.na(H * A) & ii]
  } else if(option > 1){
    plot(sm2, add = TRUE, col = cols["b_grey"], pch = 19, lwd = 0,
    cex = 1, type = "p")
    if(option==2){
      plot(sm1, add = TRUE, col = cols["b_orange"], pch = 19, lwd = 0,
        cex = 1, type = "p")
      lines(x,y, col = cols["b_orange"], lwd = 1, lty="dashed")
    } else {
      plot(sm2, add = TRUE, col =cols["b_green"], lwd = 1, type = "l", p.lines.transparent =0.15)
    }
   species <- G[!is.na(H * A)]
  }
  mtext("Individual's height (m)", 2, line=3)
  mtext(expression("Individual's leaf area"~~(m^2)), 1, line=3)
  legend("topleft", legend = paste(length(species), " individuals\n", length(unique(species)), " species"), bty = "n", cex = 0.5)
 }

figure_map <- function(baad){
  data <- baad$data
  # var_def <- baad$dictionary
  # vars <- setdiff(var_def$variable[var_def$type == "numeric"],
  #                 c("map", "mat", "lai", "age")) # World plot colored by vegetation type
  cols <- make_transparent(c(my_cols(),"navajowhite4"),0.67)
  vegs <- c("BorF","Sav","TempF","TempRF","TropRF","TropSF","Wo")
  vegs_labels <- c("Boreal forest","Savannah","Temperate forest",
                   "Temperate rainforest","Tropical rainforest",
                   "Tropical seasonal forest","Woodland","Glasshouse, Common garden")

  drawWorldPlot(data[data$vegetation == vegs[1],], sizebyn=TRUE, pchcol=cols[1])
  for(i in 2:length(vegs)){
    drawWorldPlot(data[data$vegetation == vegs[i],], sizebyn=TRUE, pchcol=cols[i], add=TRUE)
  }
  drawWorldPlot(data[data$growingCondition == "GH",], sizebyn=TRUE,
                pchcol=cols[8], add=TRUE)
  par(xpd=NA)
  legend(-190, 40, vegs_labels[1:8], fill=cols[1:8], bty='n', cex=0.8)
  ns <- c(10,100,1000)
  X <- rep(-180,3)
  Y <- seq(-70,-50,by=10)
  symbols(x=X, y=Y, circles=log10(ns), inches=0.1,
          fg="black", bg=my_cols()[1], add=TRUE)
  text(x=X+5, y=Y, labels=as.character(ns),pos=4)
  text(x=X, y=Y[3]+10, labels="plants", pos=4)

}


drawWorldPlot <- function(data, sizebyn=FALSE, add=FALSE,
                          pchcol="red", legend=TRUE) {
  if (!add){
    map('world',col="grey80",bg="white",lwd=0.5,fill=TRUE,resolution=0,wrap=FALSE, border="grey80", ylim=c(-90, 90), mar=c(0,0,0,0))
    map('world',col="black",boundary=TRUE,lwd=0.2,interior=FALSE,fill=FALSE,add=TRUE,resolution=0,wrap=TRUE)
  }

  ## See reports-port
  # Remove all duplicates (increases speed and minimizes file size)
  latlon <- with(data, paste(latitude, longitude))
  lat <- data$latitude[!duplicated(latlon)]
  lon <- data$longitude[!duplicated(latlon)]

  j  <-  !is.na(lat) & !is.na(lon)

  # Location only sometimes missing - but lat/lon can still be in dataset anyway.
  # & data$loc != "NA" | is.na(data$loc)

  if(!any(j)){
    polygon(c(-100,95,95,-100), c(-10,-10,15,15), col=rgb(0,0,0,240,maxColorValue=255))
    text(-100, 0, expression(paste(bold("Missing coordinate/location"))), col="red", xpd=TRUE, pos=4, cex=0.8)
  } else {

    if(!sizebyn){
      points(lon,lat, pch=19, col=pchcol, cex=0.6)
    } else {
      n <- table(latlon)
      symbols(lon,lat, circles=log10(n), inches=0.1, fg="black", bg=pchcol, add=TRUE)
    }
  }
}
 


## Make colours semitransparent:
make_transparent <- function(col, opacity=0.5) {
  if (length(opacity) > 1 && any(is.na(opacity))) {
    n <- max(length(col), length(opacity))
    opacity <- rep(opacity, length.out = n)
    col <- rep(col, length.out = n)
    ok <- !is.na(opacity)
    ret <- rep(NA, length(col))
    ret[ok] <- Recall(col[ok], opacity[ok])
    ret
  } else {
    tmp <- col2rgb(col)/255
    rgb(tmp[1, ], tmp[2, ], tmp[3, ], alpha = opacity)
  }
}