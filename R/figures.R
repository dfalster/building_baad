
my_cols <- function(){
  c(b_blue="#6baed6", 
    b_darkgrey="#2D2D2D", 
    b_grey="#969696", 
    b_purple="#9e9ac8", 
    b_green="#74c476", 
    b_pink="#e377c2", 
    b_orange="#FD8D3C"
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
