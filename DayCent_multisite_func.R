daycent_multisite <- function(path, stn_nm, do_spin, veg_type, n) {
  # Run DayCent and the list100 utility for this simulation
  og_wd <- getwd()
  setwd(path)
  file.copy(paste0("soils_", veg_type, ".in"), "soils.in", overwrite = T)
  
  if(stn_nm == "academy") {
  schfilename <- paste0("academySD_", veg_type)
  } else if(stn_nm == "crookston") {
    schfilename <- paste0("crookstonMN_", veg_type)
  } else if(stn_nm == "minot") {
    schfilename <- paste0("minotND_", veg_type)
  } else if(stn_nm == "oakes") {
    schfilename <- paste0("oakesND_", veg_type)
  } else if(stn_nm == "webster_city") {
    schfilename <- paste0("webstercityIA_", veg_type)
  }
  
  # -------------------------- Spinup ---------------------------
  if (do_spin) {
    # Remove any old files that may be hanging around from previous runs
    unlink(paste(schfilename, "_spin.bin", sep = ""))
    unlink(paste(schfilename, "_spin.lis", sep = ""))
    file.copy(paste0("outfiles_", veg_type, "_spin.in"), "outfiles.in", overwrite = T)
    
    print( paste0("DayCent_CABBI.exe -s ", schfilename, "_spin -n ", schfilename, "_spin") )
    system( paste0("DayCent_CABBI.exe -s ", schfilename, "_spin -n ", schfilename, "_spin") ) 
  }
  
  #  ------------------------- Simulation -------------------------
  
  # Read in schedule file, find the lines with the .wth filename, the starting year, the last year
  #  and replace them with the correct info (rewrite to use the nth .wth file (nth realization), use correct years)
  schfile_lines <- read_lines( paste0(schfilename, ".sch"), -1) #read in as lines
  schfile_lines[which(str_detect(schfile_lines, ".wth"))] <- paste0("swg_out_", n, ".wth") #find .wth line and replace
  schfile_lines[which(str_detect(schfile_lines, "Starting year"))] <-
    paste0(sim_yr_range$st_yr, "          Starting year") #find Starting year line and replace
  schfile_lines[which(str_detect(schfile_lines, "starting year"))] <-
    paste0(sim_yr_range$st_yr, "          Output starting year") #find Output starting year line and replace
  schfile_lines[which(str_detect(schfile_lines, "Last year"))] <-
    paste0(sim_yr_range$end_yr, "          Last year") #find Last year lines and replace
  write_lines(schfile_lines, paste0(schfilename, ".sch")) #write new .sch file
  
  # Copy the wth file to directory with exe
  file.copy(paste0("input/swg_out_", n, ".wth"), paste0("swg_out_", n, ".wth"), overwrite = T)
  
  # Remove any old files that may be hanging around from previous runs
  unlink(paste(schfilename, ".bin", sep = ""))
  file.copy(paste0("outfiles_", veg_type, ".in"), "outfiles.in", overwrite = T)
  
  print( paste0("DayCent_CABBI.exe -s ", schfilename, " -n ", schfilename, " -e ", schfilename, "_spin") )
  system( paste0("DayCent_CABBI.exe -s ", schfilename, " -n ", schfilename, " -e ", schfilename, "_spin") )
  
  # create directory in which to move output
  if(sim_type == "obs") {
    path_out <- paste0("output/", sim_type, "/")
  } else if(sim_type == "probs") {
    path_out <- paste0("output/", sim_type, "/", sea_sim_type_prcp, "_", sea_sim_type_tmp, "/")
  }
  dir.create(path_out, recursive = T, showWarnings = F)
  
  # move and rename standard output filename to indicate vegetation type
  {
    file.rename("dc_sip.csv", paste0(path_out, "dc_sip_", stn_nm, "_", veg_type, "_", n, ".csv") )
    file.rename("watrbal.csv", paste0(path_out, "watrbal_", stn_nm, "_", veg_type, "_", n, ".csv") )
    file.rename("wfps.csv", paste0(path_out, "wfps_", stn_nm, "_", veg_type, "_", n, ".csv") )
    file.rename("livec.csv", paste0(path_out, "livec_", stn_nm, "_", veg_type, "_", n, ".csv") )
    file.rename("deadc.csv", paste0(path_out, "deadc_", stn_nm, "_", veg_type, "_", n, ".csv") )
    file.rename("bio.csv", paste0(path_out, "bio_", stn_nm, "_", veg_type, "_", n, ".csv") )
    file.rename("psyn.csv", paste0(path_out, "psyn_", stn_nm, "_", veg_type, "_", n, ".csv") )
    file.rename("potcrp.csv", paste0(path_out, "potcrp_", stn_nm, "_", veg_type, "_", n, ".csv") )
  }
  
  # Remove wth file
  file.remove(paste0("swg_out_", n, ".wth"))
  
  unlink(paste(schfilename, ".lis", sep = ""))
  print( paste0("list100_DayCent_CABBI.exe ", schfilename, " ", schfilename, " outvars_", veg_type, ".txt") )
  system( paste0("list100_DayCent_CABBI.exe ", schfilename, " ", schfilename, " outvars_", veg_type, ".txt") )
  setwd(og_wd)
}