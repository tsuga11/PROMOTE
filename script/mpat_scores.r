require(terra)


#
# Calculate quadrant (strategy) and adapt-protect (A|P, impact) scores
#

# 2-minutes for TCSI 15-m
# x is a stack with current and future rasters in that order....
mpat_scores <- function(x, dir_name, overwrite = T){
  
  pos_one <- (x[[1]] * 0) + 1
  neg_one <- pos_one * -1
  
  # Distance from each cell on the 2-D graph to the corners of the graph, which represents
  #  quadrant membership, here represented as distance to quadrant corner.
  m <- euc(x1 = x[[1]], y1 = x[[2]], x2 = pos_one, y2 = pos_one)
  p <- euc(x1 = x[[1]], y1 = x[[2]], x2 = pos_one, y2 = neg_one)
  a <- euc(x1 = x[[1]], y1 = x[[2]], x2 = neg_one, y2 = pos_one)
  t. <- euc(x1 = x[[1]], y1 = x[[2]], x2 = neg_one, y2 = neg_one)
  
  rm(pos_one, neg_one); gc()
  
  # Convert from distance to SOE score, where higher SOE scores go to those cells closer to a given corner
  m_rc <- app(m, rescale.range, orig.min = 0, orig.max = 2.828427, new.min = 1, new.max = -1)
  p_rc <- app(p, rescale.range, orig.min = 0, orig.max = 2.828427, new.min = 1, new.max = -1)
  a_rc <- app(a, rescale.range, orig.min = 0, orig.max = 2.828427, new.min = 1, new.max = -1)
  t_rc <- app(t., rescale.range, orig.min = 0, orig.max = 2.828427, new.min = 1, new.max = -1)
  
  rm(m, p, a, t.); gc()
  
  #
  # Calculate A|P score (max of protect and adapt)
  ap <- max(rast(list(a_rc, p_rc)))
  ap_rc <- app(ap, rescale.range, orig.min = -0.4142136276, orig.max = 1, new.min = -1, new.max = 1)
  
  out <- rast(list(x[[1]], x[[2]], m_rc, p_rc, a_rc, t_rc, ap, ap_rc))
  names(out) <- c('current', 'future', 'monitor', 'protect', 'adapt', 'transform', 'ap', 'ap_rescale')
  
  rm(m_rc, p_rc, a_rc, t_rc); gc()
  
  if(!missing(dir_name)){
    dir.create(paste0(dir_name, '/tif'), recursive = T)
    dir.create(paste0(dir_name, '/png'), recursive = T)
    
    if((!file.exists(paste0(dir_name, "/tif/", names(out)[1], '.tif')) | overwrite))
      writeRaster(out, 
                  filename = paste0(dir_name, "/tif/", names(out), '.tif'), 
                  overwrite = T)
    
    for(i in 1:nlyr(out)){
      this_quad <- ifelse(names(out)[i] %in% c("current", "future"), "soe", names(out)[i])
      # print(paste0(this_quad, " | ", date()))
      
      plot_quad_scores(x = out[[i]], 
                       quad = this_quad, 
                       filename = paste0(dir_name, "/png/", names(out)[i], '.png'))
    }
    
  }
  
  
  return(out)
  
}



euc <- function(x1, y1, x2, y2)
  sqrt(((x2 - x1)^2) + ((y2 - y1)^2))



rescale.range <- function(x,
                          orig.min = NA,
                          orig.max = NA,
                          new.min = NA,
                          new.max = NA){
  if(is.na(orig.min))
    a. <- range(x, na.rm = T)[1]
  else
    a. <- orig.min
  if(is.na(orig.max))
    b. <- range(x, na.rm = T)[2]
  else
    b. <- orig.max
  
  c. <- new.min
  d. <- new.max
  
  r <-(-(((-b. * c.) + (a. * d.))/(-a. + b.))) + (((-c. + d.) * x)/(-a. + b.))
  
  r
}




rescale_ap_resilience <- function(x){
  q. <- quantile(x[], probs = c(0, 0.1, 0.9, 1.0), na.rm = T)
  
  a. <- q.[2]
  b. <- q.[3]
  c. <- -1
  d. <- 1
  
  tmp <- (-(((-b. * c.)+(a. * d.))/(-a. + b.))) + (((-c. + d.) * x)/(-a. + b.))
  tmp <- ifel(tmp > 1, 1, ifel(tmp < -1, -1, tmp))

  tmp
}




plot_quad_scores <- function(x, quad = "adapt", filename){
  
  quad <- tolower(quad)

  if(quad == "adapt")
    cols <- hcl.colors(n = 8, palette = "Purples", rev = T)
  
  if(quad == "protect")
    cols <- hcl.colors(n = 8, palette = "Oranges", rev = T)
  
  if(quad == "monitor")
    cols <- hcl.colors(n = 8, palette = "Blues", rev = T)
  
  if(quad == "transform")
    cols <- hcl.colors(n = 8, palette = "Reds", rev = T)
  
  if(quad %in% c("ap", "ap_rescale"))
    cols <- hcl.colors(n = 8, palette = "Zissou", rev = F)
  
  if(quad == "soe")
    cols <- hcl.colors(n = 8, palette = "Zissou", rev = T)
  
  if(missing(filename)){
    plot(x, breaks = seq(-1, 1, 0.25), col = cols)
  }
  else{
    png(filename = filename, width = 7, height = 7, res = 300, units = 'in')
    plot(x, breaks = seq(-1, 1, 0.25), col = cols)
    dev.off()
  }
}


plot_soe <- function(x)
  plot(x, breaks = seq(-1, 1, 0.25), col = hcl.colors(n = 8, palette = "Zissou 1", rev = T), axes = F)


