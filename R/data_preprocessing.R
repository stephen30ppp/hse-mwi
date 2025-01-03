# file: R/data_preprocessing.R
library(readxl)
data_folder <- "D:/software/hse-mwi-main/Data"
territories <<- c("AS", "FM", "GU", "MH", "MP", "PW", "PR", "VI")
index_types <<- c("Population" = "pop", "Black" = "black")
# 如果需要依赖 global.R 中的变量/包，需要确保在 global.R 之后 source
# 这里默认 global.R 已经执行，所以我们可以直接用 show_mitre, data_folder 等

# 如果需要 source("app_config.R")，也可在此执行（若在 global.R 中已执行则无需重复）
# source("app_config.R")

# ---- 复制原始 app.R 中与"load data"和"app_preprocess"相关的所有内容 ----

app_preprocess <- function(m_reg, info_df, mwi, app_start = TRUE){
  
  # create measure name to overall category
  meas_col_to_type <- setNames(m_reg$Category, m_reg$Measure)
  meas_col_to_type["Mental Wellness Index"] <- "Mental Wellness Index"
  
  # create measure names
  avail_measures <- measure_to_names <- avail_meas_w_weights <- list()
  # group into list for display
  avail_meas_list <- m_to_type <- list()
  for (idx in index_types){
    avail_measures[[idx]] <- colnames(mwi[[idx]])[-1]
    names(avail_measures[[idx]]) <- 
      c("Mental Wellness Index", 
        m_reg[
          gsub("*_pop$","",
               gsub("*_black$","",colnames(mwi[[idx]])[-c(1:2)])), "Measure"
        ]
      )
    
    measure_to_names[[idx]] <- setNames(names(avail_measures[[idx]]), avail_measures[[idx]])
    
    # group into list for display
    avail_meas_list[[idx]] <- list()
    
    # measure column to type
    m_to_type[[idx]] <- meas_col_to_type[measure_to_names[[idx]][avail_measures[[idx]]]]
    
    # add the weights to the name
    avail_meas_w_weights[[idx]] <- avail_measures[[idx]]
    names(avail_meas_w_weights[[idx]]) <- 
      paste0(names(avail_measures[[idx]]),
             " (Weight: ", 
             round(info_df[avail_measures[[idx]], "Effective_Weights"], 2),
             ")")
    # unmet need score doesn't have a weight
    names(avail_meas_w_weights[[idx]])[1] <- names(avail_measures[[idx]])[1]
    # add them to the list
    for (t in unique(m_to_type[[idx]])){
      avail_meas_list[[idx]][[t]] <- avail_meas_w_weights[[idx]][m_to_type[[idx]] == t]
    }
  }
  
  if ((app_start &
       !"HSE_MWI_ZCTA_full_shapefile_US.RData" %in% 
       list.files(file.path(data_folder, "Cleaned")))){
    
    # add counties/states to mwi
    for (idx in index_types){
      mwi[[idx]][, colnames(cty_cw)[-1]] <- cty_cw[mwi[[idx]]$ZCTA, -1]
    }
    
    # get zip code data
    zips <- zctas(cb = TRUE, year = 2020)
    colnames(zips)[colnames(zips) == "GEOID20"] <- "GEOID"
    zips <- zips[zips$GEOID %in% mwi$pop$ZCTA,]
    zips <- st_transform(zips, crs = "+proj=longlat +datum=WGS84")
    
    geodat <- geopts <- list()
    for (idx in index_types){
      geodat[[idx]] <- dplyr::left_join(zips, mwi[[idx]], by = c("GEOID" = "ZCTA"))
      
      # sort by state code and zcta
      geodat[[idx]] <- geodat[[idx]][order(geodat[[idx]]$STATE,
                                           geodat[[idx]]$GEOID),]
      
      # convert to points for US visualization -- ignore warnings
      geopts[[idx]] <- st_centroid(geodat[[idx]])
    }
    
    if (app_start &
        !"HSE_MWI_ZCTA_full_shapefile_US.RData" %in% 
        list.files(file.path(data_folder, "Cleaned"))){
      save(list = c("geodat", "geopts"), 
           file = file.path(data_folder, 
                            "Cleaned", "HSE_MWI_ZCTA_full_shapefile_US.RData"))
    }
  } else if ("HSE_MWI_ZCTA_full_shapefile_US.RData" %in% 
             list.files(file.path(data_folder, "Cleaned"))){
    
    load(file.path(data_folder, "Cleaned", "HSE_MWI_ZCTA_full_shapefile_US.RData"))
  }
  
  # get available zctas -- both will have the same
  avail_zctas <- geodat[["pop"]]$GEOID
  names(avail_zctas) <- paste0(geodat[["pop"]]$GEOID, 
                               " (State: ", geodat[["pop"]]$STATE_NAME, ")")
  
  if (app_start){
    return(list(
      meas_col_to_type = meas_col_to_type,
      avail_measures = avail_measures,
      measure_to_names = measure_to_names,
      avail_meas_list = avail_meas_list,
      geodat = geodat,
      geopts = geopts
    ))
  } else {
    return(list(
      meas_col_to_type = meas_col_to_type,
      avail_measures = avail_measures,
      measure_to_names = measure_to_names,
      avail_meas_list = avail_meas_list
    ))
  }
}

# ---- 以下加载原始数据集与相关文件的代码也放进来 ----
# 这些从原 app.R "load data ----" 复制过来
# -------------------------------------------------------------

# folder where all the data and information for the pipeline is
# data_folder 在 global.R 中设定过，这里可以直接用

# load measure registry -- first sheet
m_reg <- as.data.frame(read_excel(file.path(data_folder, "Metadata.xlsx"), sheet = 1))
m_reg <- m_reg[!is.na(m_reg$Numerator),]
rownames(m_reg) <- m_reg$Numerator

# create a subset for the "create your own MWI" part
sub_m <- m_reg[, c("Measure", "Category", "Weights", "Weights")]
colnames(sub_m)[ncol(sub_m)-1] <- "Original Weights"
colnames(sub_m)[ncol(sub_m)] <- "Updated Weights"
rownames(sub_m) <- rownames(m_reg)

# load index weighting/output
info_df <- read.csv(file.path(data_folder, "Cleaned", "HSE_MWI_Data_Information.csv"))
rownames(info_df) <- info_df$Numerator

# load mapped measure data excat values
meas_df <- read.csv(file.path(data_folder, "Cleaned", "HSE_MWI_ZCTA_Converted_Measures.csv"),
                    colClasses = c("GEOID" = "character"))
meas_df <- meas_df[meas_df$GEOID != "",]
rownames(meas_df) <- meas_df$GEOID

# load zip codes to zcta
zip_cw <- read.csv(file.path(data_folder, "Resources", "Zip_to_zcta_crosswalk_2021.csv"),
                   colClasses = c("ZIP_CODE" = "character","ZCTA" = "character"))
zip_cw <- zip_cw[!zip_cw$STATE %in% territories,]
zip_to_zcta <- setNames(zip_cw$ZCTA, zip_cw$ZIP_CODE)
zcta_to_zip <- aggregate(ZIP_CODE ~ ZCTA, data = zip_cw, 
                         FUN = function(x){paste(x, collapse = ", ")})
zcta_to_zip <- setNames(zcta_to_zip$ZIP_CODE, zcta_to_zip$ZCTA)

# load crosswalk files
county_cw <- read.csv(file.path(data_folder, "Resources","zcta_county_rel_20.csv"),
                      colClasses = c("ZCTA5" = "character","GEOID" = "character"))
county_cw$STATE <- substr(county_cw$GEOID, 1, 2)
county_cw$COUNTY <- substr(county_cw$GEOID, 3, 5)

# collapse so that there's one zcta for every row (states get collapsed by pipe)
cty_cw <- 
  aggregate(STATE ~ ZCTA5, data = county_cw, 
            FUN = function(x){paste(x, collapse = "|")})
rownames(cty_cw) <- cty_cw$ZCTA5
un_st <- lapply(strsplit(cty_cw$STATE, "|", fixed = TRUE), unique)
cty_cw$STATE <- sapply(un_st, `[`, 1)
cty_cw$STATE_2 <- sapply(un_st, `[`, 2)

data("fips_codes")
f_st <- setNames(unique(fips_codes$state_name),unique(fips_codes$state_code))
f_st <- f_st[f_st %in% c(state.name, "District of Columbia")]

st_to_fips <- setNames(names(f_st), f_st)
cty_cw$STATE_NAME <- f_st[cty_cw$STATE]
cty_cw$COUNTY <- aggregate(COUNTY ~ ZCTA5, data = county_cw, 
                           FUN = function(x){paste(x, collapse = "|")})[,2]
cty_cw$GEOID <- aggregate(GEOID ~ ZCTA5, data = county_cw, 
                          FUN = function(x){paste(x, collapse = "|")})[,2]

no_dir_perc_meas_df <- read.csv(file.path(data_folder, "Cleaned",
  "HSE_MWI_ZCTA_No_Directionality_Percentile_Ranked_Measures.csv"),
  colClasses = c("GEOID" = "character"))
colnames(no_dir_perc_meas_df)[colnames(no_dir_perc_meas_df) == "GEOID"] <- "ZCTA"
no_dir_perc_meas_df <- no_dir_perc_meas_df[no_dir_perc_meas_df$ZCTA != "",]
rownames(no_dir_perc_meas_df) <- no_dir_perc_meas_df$ZCTA

mwi <- list()
mwi[["pop"]] <- read.csv(file.path(data_folder, "Cleaned",
                           "HSE_MWI_ZCTA_Mental_Wellness_Index_Population.csv"),
                         colClasses = c("ZCTA" = "character"))
mwi[["pop"]] <- mwi[["pop"]][mwi[["pop"]]$ZCTA != "",]

mwi[["black"]] <- read.csv(file.path(data_folder, "Cleaned",
                             "HSE_MWI_ZCTA_Mental_Wellness_Index_Black.csv"),
                           colClasses = c("ZCTA" = "character"))
mwi[["black"]] <- mwi[["black"]][mwi[["black"]]$ZCTA != "",]

# create a pretty set of names for later
st_abbrev_to_full <- c(state.name, "District of Columbia", "All States")
names(st_abbrev_to_full) <- c(state.abb, "DC", "All")

all_pop_df <- read.csv(file.path(data_folder, "Resources", "ACS_ZCTA_Total_Populations.csv"),
                       colClasses = c("GEOID" = "character"))
all_pop_df$perc_black <- all_pop_df$total_black / all_pop_df$total_pop * 100
all_pop_df$perc_pop <- 100
rownames(all_pop_df) <- all_pop_df$GEOID

meas_colors <- c(
  "purples", # SDOH
  "greens",  # health status
  "blues",   # healthcare acess
  "purple_blue_green" # MWI
)
names(meas_colors) <- c(unique(m_reg$Category), "Mental Wellness Index")

meas_max_colors <- c(
  "#5d499e", # SDOH
  "#157ba7", # health status
  "#70ad47", # health access
  "#00441b"  # MWI
)
names(meas_max_colors) <- names(meas_colors)

meas_min_colors <- c(
  "#fcfbfd", # SDOH
  "#f7fbff", # health status
  "#f7fcf5", # health access
  "#3f157d"  # MWI
)
names(meas_min_colors) <- names(meas_colors)

meas_colors <- lapply(1:length(meas_min_colors), function(x){
  if (x != length(meas_min_colors)){
    colorRamp(c(meas_min_colors[x], meas_max_colors[x]), interpolate = "linear")
  } else { # MWI has something in the middle
    colorRamp(c(meas_min_colors[x], "#c6dbef", meas_max_colors[x]), interpolate = "linear")
  }
})
meas_colors_pal <- lapply(1:length(meas_min_colors), function(x){
  if (x != length(meas_min_colors)){
    colorRampPalette(c(meas_min_colors[x], meas_max_colors[x]), interpolate = "linear")
  } else { # MWI has something in the middle
    colorRampPalette(c(meas_min_colors[x], "#c6dbef", meas_max_colors[x]), interpolate = "linear")
  }
})
names(meas_colors) <- names(meas_colors_pal) <- names(meas_max_colors)

meas_max_colors["Mental Wellness Index"] <- meas_colors_pal[["Mental Wellness Index"]](7)[6]
meas_min_colors["Mental Wellness Index"] <- meas_colors_pal[["Mental Wellness Index"]](7)[2]

overall <- app_preprocess(m_reg, info_df, mwi, app_start = TRUE)
for (idx in index_types){
  mwi[[idx]][, colnames(cty_cw)[-1]] <- cty_cw[mwi[[idx]]$ZCTA, -1]
}
overall[["mwi"]] <- mwi

# END of data_preprocessing.R
