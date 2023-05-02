

## try sclass charts per LTA


# load packages
library(tidyverse)


# read and wrangle data

bps_scls_ltas <- read_csv("data/bps_scls_ltas.csv")
bps_aoi_atts <- read.csv("data/bps_aoi_attributes.csv")
View(bps_scls_ltas)



# read in and format refcon
## note: in modified ref con extra columns were deleted, NAs replaced by zeros and non reference sclass columns added
ref_con_modified <- read_csv("data/ref_con_modified.csv")
bps_names <- read_csv("data/bps_model_number_name.csv")

# pivot long


ref_con <- ref_con_modified %>%
  pivot_longer(!Model_Code, names_to = "refLabel", values_to = "refPercent") %>%
  unite(model_label, c("Model_Code", "refLabel"), remove = FALSE) %>%
  left_join(bps_names)

# get list of aoi BpS model numbers

aoi_bps_models <- bps_aoi_atts$BPS_MODEL

#subset ref_con to aoi
aoi_ref_con <- subset(ref_con, Model_Code %in% aoi_bps_models)

# calculate current sclass percents
bps_scls_ltas_curr <- bps_scls_ltas %>%
  group_by(MAP_UNIT_SYMBOL,  bps_aoi, BPS_MODEL) %>%
  mutate(total_count = sum(Count))%>%
  mutate(currentPercent = as.integer((Count/total_count)*100)) %>%
  unite(model_label, c("BPS_MODEL", "LABEL"))


# historic and current sclass amounts together  NEED TO HAVE ALL SCLASS COMBOS
bps_scls_ltas_ref_cur <- dplyr::left_join(bps_scls_ltas_curr,
                             ref_con,
                             by = "model_label")
# BPS_SCL misses combos where there is a current sclass missing

bps_scls_ltas_ref_cur2 <- dplyr::left_join(aoi_ref_con,
                              bps_scls_ltas_curr,
                              by = "model_label")

## write bps_scls2 with ltas
write.csv(bps_scls_ltas_ref_cur2, file = "data/bps_scls_ref_cur_ltas.csv")


#  OK now filter then wrangle data for charts

bps_scls_ref_cur_ltas <- read_csv("data/bps_scls_ref_cur_ltas.csv")
View(bps_scls_ref_cur_ltas)

one_lta_wrangled <- bps_scls_ref_cur_ltas %>%
  filter(MAP_UNIT_SYMBOL == "223Ba01") %>%
  group_by(bps_aoi) %>%
  mutate(total.count = sum(Count)) %>%
  ungroup() %>%
  dplyr::filter(dense_rank(desc(total.count)) < 7) %>%
  dplyr::select(c("BpS_Name", "refLabel",  "currentPercent", "refPercent")) %>%
  pivot_longer(
    cols = c(`refPercent`, `currentPercent`), 
    names_to = "refCur", 
    values_to = "Percent"
  )


  
  
  # order classes
  one_lta_wrangled$refLabel <- factor(one_lta_wrangled$refLabel, levels= c(
    "Developed",
    "Agriculture",
    "UE",
    "UN",
    "E",
    "D",
    "C",
    "B",
    "A"))
  
  
  
  
  sclasplot <-
    ggplot(one_lta_wrangled, aes(fill=factor(refCur), y=Percent, x=refLabel)) + 
    geom_col(width = 0.8, position = position_dodge()) +
    coord_flip() +
    facet_grid(. ~BpS) +
    scale_x_discrete(limits = (levels(one_lta_wrangled$refLabel))) +
    labs(
      title = "Succession Classes past and present",
      subtitle = "6 BpSs selected for illustration. Not all succession classes present in all BpSs",
      caption = "\nData from landfire.gov.",
      x = "",
      y = "Percent")+
    theme_minimal(base_size = 12)+
    theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
          plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
          plot.caption.position =  "plot") +
    scale_fill_manual(values = c("#3d4740", "#32a852" ), # present (grey), historical (green)
                      name = " ", 
                      labels = c("Present",
                                 "Past")) +
    facet_wrap(~BpS_Name, nrow(3),labeller = labeller(BpS_Name = label_wrap_gen())) +
    theme(panel.spacing = unit(.05, "lines"),
          panel.border = element_rect(color = "black", fill = NA, size = 1), 
          strip.background = element_rect(color = "black", size = 1))
  
  sclasplot

  
  
  

