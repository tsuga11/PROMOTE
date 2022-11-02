source("./script/promote_maps_w_hillshade.r")
source("./script/mpat_scores.r")

cp <- function(){
  
  tcsi_dir <- paste0("D:/Dropbox/TCSI")
  tif_dir <- paste0(tcsi_dir, '/tif')
  
  mask_015 <- rast(paste0(tif_dir, "/mask_015m.tif"))
  mask_landis <- rast(paste0(tif_dir, "/mask_180m_landis.tif"))
  b <- vect(paste0(tif_dir, "/tcsi_15m_bounds01.shp"))
  quad_subdir <- paste0(tcsi_dir, "/Results/Carbon_Pillar_Simple")
  dir.create(quad_subdir)
  
  
  #
  # <-- CARBON -->
  #
  print(paste0("STEP 1. Carbon --> ", date()))
  
  cn_e01c <- rast(paste0(tif_dir, "/carbon_current.tif"))
  
  # Change this to only a single metric for simplicity
  cn_e01f <- rast(paste0(tif_dir, "/carbon_future.tif"))
  
  cn_e01 <- rast(list(cn_e01c, cn_e01f))
  names(cn_e01) <- c('current', 'future')
  
  #
  # PILLAR: CARBON
  #
  cn <- terra::project(x = cn_e01, y = mask_015, method = 'ngb')
  
  cn_w <- wrap(cn)
  save(cn_w, file = paste0(quad_subdir, '/cn.rda'))
  
  rm(cn_e01, cn_e01c, cn_e01f, cn_w)
  gc()
  
  #
  # CARBON
  #
  # STEP 2. Calculate Adapt-Protect (Impact) and Quadrant (Strategy) scores
  #          Sample the 2-D graph area with 1) A/P, 2) Monitor, 3) Protect, 4) Adapt, 5) Transform scoring
  #          Do this at the 1) Element, 2) Pillar, and 3) Ecosystem levels
  #
  print(paste0("CARBON SEQUESTRATION ", date()))
  cn_scores <- quad_scores_xy(cn, dir_name = quad_subdir)
  timestamp()
  
  dir.create(paste0(quad_subdir, '/tif'))
  dir.create(paste0(quad_subdir, '/png'))
  
  single_map(x = paste0(quad_subdir, "/tif/current.tif"), 
             x2 = paste0(quad_subdir, "/tif/future.tif"), 
             f = paste0(quad_subdir, "/png/Figure - Current - Future SOE.png"), 
             l = T, 
             lp = "bottomleft", 
             label = c("A", "B"))
  
  single_map(x = paste0(quad_subdir, "/tif/ap.tif"), 
             x2 = paste0(quad_subdir, "/tif/ap_rescale.tif"), 
             f = paste0(quad_subdir, "/png/Figure - AdaptProtect.png"), 
             l = T, 
             lp = "bottomleft", 
             label = c("A", "B"))
  
  
  # Composite strategy score (strong, weak)
  s <- rast(paste0(quad_subdir, "/tif/", c("monitor.tif", "protect.tif", "adapt.tif", "transform.tif")))
  wm <- which.max(s)
  m <- max(s)
  wm1 <- wm * 10
  # Changed to 0.5
  w1 <- ifel(m >= 0.5, 1, 0)
  
  wms <- wm1 + w1
  
  
  strong_cols <- c("#046C9A", "#F98400", "#3D1778", "#5B1A18")
  weak_cols <- c("#6C9CCC", "#F2AD00", "#8B7EBB", "#FD6467")
  cols <- c(weak_cols[1], strong_cols[1], weak_cols[2], strong_cols[2], weak_cols[3], strong_cols[3], weak_cols[4], strong_cols[4])
  # x11(); plot(wms, col = cols, type = "classes")
  
  d <- as.data.frame(expand.grid(c("Weak ", "Strong "), c("monitor", "protect", "adapt", "transform")))
  dl <- list(); for(i in 1:nrow(d)){dl[[i]] <- paste0(d[i, 1], d[i, 2])}
  lt <- unlist(dl)
  single_map(x = wms, 
             f = paste0(quad_subdir, "/png/strategy_score_hillshade.png"), 
             cols = cols, type = "classes", l = T, lp = "bottomleft", lt = lt)
  
  
  
}
