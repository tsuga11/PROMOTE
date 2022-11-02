require(terra)
require(GEOmap)
require(rasterVis)


single_map <- function(x, x2, f, cols, type, l = T, lp = "topright", lt, label, hillshade){
  
    if(missing(hillshade))
      hill <- rast(paste0("./tif/hillshade_tcsi_30m.tif"))

    #
    # Figure 8 - Current and Future SOE scores
    if(class(x) != "SpatRaster")
      x <- rast(x)
    
    if(!missing(x2)){
      if(class(x2) != "SpatRaster")
        x2 <- rast(x2)
    }
    
    # dev.new(width = 9, height = 7, noRStudioGD = TRUE)
    png(filename = f,
        width = ifelse(missing(x2), 3.75, 7.5), height = 3.75, units = 'in', res = 300, bg = "white")
    
    if(!missing(x2))
      par(mfrow = c(1, 2), omi = c(0, 0, 0, 0), mai = c(0, 0, 0, 0))

    if(missing(cols)){
      col_rev <- ifelse(length(grep(pattern = "current|future", x = f, ignore.case = T)) == 1, T, F)
      these_cols <- hcl.colors(n = 8, palette = "Zissou 1", rev = col_rev)
      
      plot(x, breaks = seq(-1, 1, 0.25), col = these_cols, axes = F, legend = F, mar = c(0, 0, 0, 0), colNA = "#FFFFFF33")
      
      if(missing(lt))
        leg.txt <- c("-1.00 to -0.75", "-0.75 to -0.50", "-0.50 to -0.25",
                     '-0.25 to 0.00', "0.00 to 0.25", "0.25 to 0.50", "0.50 to 0.75",
                     "0.75 to 1.00")
      
    }
      
    else{
      these_cols <- cols
      
      if(!missing(lt))
        leg.txt <- lt
      
      if(missing(type))
        type <- "continuous"
      
      plot(x, col = these_cols, axes = F, legend = F, type = type, mar = c(0, 0, 0, 0), colNA = "#FFFFFF33")
    }
    
    
    plot(hill, col = grey(0:100/100, alpha = 0.2), add = T, legend = F, axes = F, mar = c(0, 0, 0, 0), colNA = "#FFFFFF33")

    if(l)
      legend(lp, fill = these_cols,
             legend = leg.txt, cex = 0.6, box.lty = 0)
    
    
    if(!missing(label))
      legend("topleft", box.lty = 0, legend = label[1], cex = 1.3)
    
    
    #
    # Plot second plot if you want
    if(!missing(x2)){
      
      if(missing(cols)){

        plot(x2, breaks = seq(-1, 1, 0.25), col = these_cols, axes = F, legend = F, mar = c(0, 0, 0, 0), colNA = "#FFFFFF33")

      }
      
      else{
        these_cols <- cols

        if(missing(type))
          type <- "continuous"
        
        plot(x2, col = these_cols, axes = F, legend = F, type = type, mar = c(0, 0, 0, 0), colNA = "#FFFFFF33")
      }
      
      plot(hill, col = grey(0:100/100, alpha = 0.2), add = T, legend = F, axes = F, mar = c(0, 0, 0, 0), colNA = "#FFFFFF33")
      
      if(!missing(label))
        legend("topleft", box.lty = 0, legend = label[2], cex = 1.3)
    }
    
    dev.off()
    
    
}


maps <- function(){
  
  # Carbon director
  main_dir <- "D:/Dropbox/TCSI/EMDS/Blueprint/Quad_Scores_XY_30Dec21/carbon/tif"
  
  # Manuscript director
  out_dir <- "D:/Dropbox/TCSI/Manuscripts/DSS Climate Change"
  
  hill <- rast("D:/Dropbox/TCSI/Data/Topography/hillshade_tcsi_30m.tif")  
  
  # Degrees minutes seconds
  # lon_degs <- seq(-121.2, -120, 0.2)
  # lon_degs <- dms(lon_degs)
  # lon_labs <- sapply(1:length(lon_degs[[1]]), function(i) bquote(.(lon_degs$d[i])*degree ~ .(lon_degs$m[i])*minute ~ W))
  
  # axis(1, at = lon_degs, lab = do.call(expression, lon_labs))
  
  #
  # Figure 8 - Current and Future SOE scores
  curr <- rast(paste0(main_dir, '/current.tif'))
  fut <- rast(paste0(main_dir, '/future.tif'))
  
  # dev.new(width = 9, height = 7, noRStudioGD = TRUE)
  png(filename = "D:/Dropbox/TCSI/Manuscripts/DSS Climate Change/Figure - Current Future.png",
      width = 7, height = 5, units = 'in', res = 300, bg = "white")
  # par(mfrow=c(1,3), omi = c(0, 0, 0, 0), mai = c(0, 0, 0, 0))
  layout(rbind(c(1, 1, 2, 2),
               c(1, 1, 2, 2),
               c(1, 1, 2, 2)))#,
               # c(1, 1, 2, 2),
              # c(1, 1, 2, 2),
               # c(3, 3, 3, 3)))
  plot(curr, breaks = seq(-1, 1, 0.25), col = hcl.colors(n = 8, palette = "Zissou 1", rev = T), axes = F, legend = F, mar = NA)
  plot(hill, col = grey(0:100/100, alpha = 0.2), add = T, legend = F, axes = F)
  legend('topleft', legend = "A", cex = 1.5, box.lty = 0, inset = c(-0.1, 0.18))
  legend("bottomleft",
         fill = hcl.colors(n = 8, palette = "Zissou 1", rev = T),
         legend = c("-1.00 to -0.75", "-0.75 to -0.50", "-0.50 to -0.25",
                    '-0.25 to 0.00', "0.00 to 0.25", "0.25 to 0.50", "0.50 to 0.75",
                    "0.75 to 1.00"), horiz = F,
         cex = 0.8, box.lty = 0)
  
  plot(fut, breaks = seq(-1, 1, 0.25), col = hcl.colors(n = 8, palette = "Zissou 1", rev = T), axes = F, legend = F, mar = NA)
  plot(hill, col = grey(0:100/100, alpha = 0.2), add = T, legend = F, axes = F)
  legend('topleft', legend = "B", cex = 1.5, box.lty = 0, inset = c(-0.1, 0.18))
  # par(xpd = NA)
  # plot(0, xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n"); box(col = 'white')
  # legend("center",
  #        fill = hcl.colors(n = 8, palette = "Zissou 1", rev = T),
  #        legend = c("-1.00 to -0.75", "-0.75 to -0.50", "-0.50 to -0.25",
  #                   '-0.25 to 0.00', "0.00 to 0.25", "0.25 to 0.50", "0.50 to 0.75",
  #                   "0.75 to 1.00"), horiz = T,
  #        cex = 0.8, box.lty = 0)
  dev.off()
  
  # rasterVis levelplot
  soeTheme <- rasterTheme(region = hcl.colors(n = 8, palette = "Zissou 1", rev = T))
  hsTheme <- modifyList(GrTheme(), list(regions=list(alpha=0.2)))
  
  x <- raster::stack(c(raster(curr), raster(fut)))
  
  # hillshade levelplot
  x11(width = 8, height = 6)
  
  levelplot(x, par.settings = soeTheme)
  
  l1 <- levelplot(x[[1]], par.settings = soeTheme, margin = F, colorkey = F) + levelplot(hill, par.settings = hsTheme, margin = F)
  l2 <- levelplot(x[[2]], par.settings = soeTheme, margin = F, colorkey = F) + levelplot(hill, par.settings = hsTheme, margin = F)
  
  png(filename = "D:/Dropbox/TCSI/Manuscripts/DSS Climate Change/tmp.png",
      width = 7, height = 5, units = 'in', res = 300, bg = "white")
  grid.arrange(l1, l2, ncol=2)
  dev.off()
}
