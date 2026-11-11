#Analyses associated with Chapter 5 of my thesis: 
#Changes in cortical microstructure parallel patterns of expansion in early life 

#set everything up  ------------------------------------------
rm(list=ls())
list.of.packages <- c("dplyr", "readxl", "openxlsx", "ggplot2", "tidyr",  "writexl",  "reshape2", 
                      "lm.beta",  "sp", "imager", "ggseg", "ggseg3d",  "visreg", "viridis",
                      "patchwork", "ggsegGlasser",  "tidyverse", "modelr",
                       "parameters", "modelsummary", "psych", "tinytable", "lme4", "lmerTest", "foreign", 
                       "semTools", "ggsignif")
invisible(lapply(list.of.packages, library, character.only = TRUE))

#set your working directory to whatever file path is appropriate
setwd("C:/Users/lisa.gorham/Box/2023 PhD Files/Aim 2 Diffusion/")


#Load in the correct data ----------------------------------------------------

#load in data
potentialsubs <- read.csv("C:/Users/lisa.gorham/Box/2023 PhD Files/Aim 2 Diffusion/subjectswithtermto10expansiondata.csv")
FA_data <- read_xlsx("C:/Users/lisa.gorham/Box/2023 PhD Files/Aim 2 Diffusion/FinalFAforposter_6.26.26.xlsx")
MD_data <- read_xlsx("C:/Users/lisa.gorham/Box/2023 PhD Files/Aim 2 Diffusion/FinalMDforposter_6.26.26.xlsx")
RD_data <- read_xlsx("C:/Users/lisa.gorham/Box/2023 PhD Files/Aim 2 Diffusion/RD_6.26.26.xlsx")
AD_data <- read_xlsx("C:/Users/lisa.gorham/Box/2023 PhD Files/Aim 2 Diffusion/AD_6.26.26.xlsx")
expansion_data <- read.csv("C:/Users/lisa.gorham/Box/2023 PhD Files/Aim 2 Diffusion/N129_expansion_Glasser.csv")
extrademos <- read_xlsx("C:/Users/lisa.gorham/Box/2023 PhD Files/Aim 2 Diffusion/demographics.xlsx")

#get expansion data set up
colnames(expansion_data)[1] = "ID_910_USE"
expansion_long <- expansion_data %>%
  pivot_longer(
    cols = -ID_910_USE,
    names_to = "ROI",
    values_to = "Expansion"
  )
expansion_long$ROI <- gsub("_expansion", "", expansion_long$ROI)
expansion_long$ROI <- gsub("\\.", "-", expansion_long$ROI)

#convert FA, MD, RD, AD data to long format 
FA_long <- FA_data %>%
  pivot_longer(
    cols = -RealID,
    names_to = "ROI",
    values_to = "FA"
  )

MD_long <- MD_data %>%
  pivot_longer(
    cols = -RealID,
    names_to = "ROI",
    values_to = "MD"
  )

AD_long <- AD_data %>%
  pivot_longer(
    cols = -RealID,
    names_to = "ROI",
    values_to = "AD"
  )

RD_long <- RD_data %>%
  pivot_longer(
    cols = -RealID,
    names_to = "ROI",
    values_to = "RD"
  )

combineddiffusiondata <- FA_long %>%
  left_join(MD_long) %>%
  left_join(AD_long) %>%
  left_join(RD_long)

section1data <- potentialsubs %>% left_join(combineddiffusiondata) %>% filter(!is.na(FA))
extrademos <- extrademos %>% select(c(recruitmentid, regfam_subjecttype:birth_ga))
section1data <- section1data %>% left_join(extrademos, join_by(recruitmentid))
section1data <- section1data %>% left_join(expansion_long, join_by(ID_910_USE, ROI))

#getting the naming set up for plotting
section1data$ROI <- gsub("_ROI", "", section1data$ROI)
section1data <- section1data %>%
  mutate(hemi = ifelse(
    grepl("^L_", ROI), "left", "right"
  ))
section1data$region <- substr(section1data$ROI, 3, nchar(section1data$ROI))
section1data$region <- gsub("\\.", "-", section1data$region)

#filtering out things we think are noise (CSF or subplate or failed diffusion vertices)
filteredsection1 <- section1data %>% filter(MD > 0.9 & MD < 1.35)

plot(section1data$regvis_scanpma, section1data$MD)
plot(filteredsection1$regvis_scanpma, filteredsection1$MD)

diffusion_section2 <- read.csv("C:/Users/lisa.gorham/Box/2023 PhD Files/Aim 2 Diffusion/diffusiondata_10.16.26.csv")
expansion_section2 <- read.csv("C:/Users/lisa.gorham/Box/2023 PhD Files/Aim 2 Diffusion/expansiondata_10.16.26.csv")

diffusion_section2 <- diffusion_section2 %>%
  mutate(scan = paste0(subject, session),
         measure = paste0(DTI.metric, "_", threshold)) %>%
  select(c(scan, measure, R_V1_ROI_diffusion_val:L_p24_ROI_diffusion_val))

diffusion_section2 <- diffusion_section2 %>%
  mutate(
    measure = case_when(
      str_detect(measure, "_1p35$") ~ measure,
      str_detect(measure, "_(1p3|1p4|1p25|1p45)$") ~
        str_replace(measure, "_(1p3|1p4|1p25|1p45)$", "_subjectspecific"),
      TRUE ~ measure
    ))

diffusion_long <- diffusion_section2 %>%
  pivot_longer(
    cols = -c(scan, measure),
    names_to = "ROI", 
    values_to = "value"
  ) %>%
  select(scan, measure, ROI, value) %>%
  pivot_wider(
    names_from = measure,
    values_from = value
  ) %>%
  mutate(across(
    where(is.character),
    ~ na_if(.x, "NaN")
  ))%>%
  mutate(across(
    where(is.numeric),
    ~ replace(.x, is.nan(.x), NA_real_)
  )) %>%
  mutate(
    ROI = str_sub(ROI, 1, -19),
    subject = str_sub(scan, 1, 6),
    session = str_sub(scan, 7, 8)
  )

ages <- read_xlsx("C:/Users/lisa.gorham/Box/2023 PhD Files/Aim 2 Diffusion/agesatscan.xlsx")
colnames(ages)[1] = "scan"
diffusion_long <- diffusion_long %>% left_join(ages)

#Figure 1A: How do diffusion metrics change with age? spaghetti plots  -----------------------

#spaghetti plots
timesfrom910 <- filteredsection1 %>% select(c(
  RealID,
  group,
  regvis_scanpma,
  ROI,
  region,
  hemi,
  FA:RD
))

diffusion_long$ROI <- gsub("\\.", "-", diffusion_long$ROI)
diffusion_long <- diffusion_long %>% mutate(
  hemi = ifelse(
    grepl("^L_", ROI), "left", "right"
  )
)
diffusion_long$region <- substr(diffusion_long$ROI, 3, nchar(diffusion_long$ROI))



timesfrompreemie <- diffusion_long %>% select(c(
  scan,
  regvis_scanpma,
  ROI,
  region,
  hemi,
  FA_1p35,
  MD_1p35,
  AD_1p35,
  RD_1p35
))

timesfrompreemie <- timesfrompreemie %>%
  mutate(
    group = "VPT_noinjury"
  )
timesfrompreemie <- rename(timesfrompreemie, 
                           FA = FA_1p35, 
                           MD = MD_1p35,
                           AD = AD_1p35,
                           RD = RD_1p35,
                           RealID = scan)


completeallROIs <- rbind(timesfrom910, timesfrompreemie)
completeallROIs$subject <- substr(completeallROIs$RealID, 1, 6)

complete <- completeallROIs %>% filter(ROI == 'L_4' | ROI == 'R_IFSa')

#FA graph
ggplot(complete, aes(
  x = regvis_scanpma,
  y = FA,
  group = interaction(subject)
)) +
  geom_line(alpha = 0.4, color = "gray40") +
  geom_point(alpha = 0.5) +
  geom_smooth(
    data = subset(complete, regvis_scanpma >= 36),
    aes(color = group, group = group),
    method = "lm",
    se = FALSE,
    linewidth = 1
  ) +
  
  scale_x_continuous(
    breaks = seq(27, 42, by = 1),
    limits = c(27, 42)
  ) +
  scale_y_continuous(
    breaks = seq(0.1, 0.4, by = 0.05),
    limits = c(0.1, 0.4)
  ) +
  scale_color_manual(
    values = c(
      "FT" = "#619CFF",
      "VPT_noinjury" = "#00BA38",
      "VPT_injury" = "#F8766D"
    )
  ) +
  facet_wrap(~ROI) +
  labs(
    x = "Post Menstrual Age at Scan",
    y = "Fractional Anisotropy (FA)",
    color = "Subject Group"
  ) +
  theme_classic()

#AD graph
ggplot(complete, aes(
  x = regvis_scanpma,
  y = AD,
  group = interaction(subject)
)) +
  geom_line(alpha = 0.4, color = "gray40") +
  geom_point(alpha = 0.5) +
  geom_smooth(
    data = subset(complete, regvis_scanpma >= 36),
    aes(color = group, group = group),
    method = "lm",
    se = FALSE,
    linewidth = 1
  ) +
  scale_x_continuous(
    breaks = seq(27, 42, by = 1),
    limits = c(27, 42)
  ) +
  scale_y_continuous(
    breaks = seq(0.39, 0.55, by = 0.02),
    limits = c(0.39, 0.55)
  ) +
  scale_color_manual(
    values = c(
      "FT" = "#619CFF",
      "VPT_noinjury" = "#00BA38",
      "VPT_injury" = "#F8766D"
    )
  ) +
  facet_wrap(~ROI) +
  labs(
    x = "Post Menstrual Age at Scan",
    y = "Axial Diffusivity (AD)",
    color = "Subject Group"
  ) +
  theme_classic()

#RD graph
ggplot(complete, aes(
  x = regvis_scanpma,
  y = RD,
  group = interaction(subject)
)) +
  geom_line(alpha = 0.4, color = "gray40") +
  geom_point(alpha = 0.5) +
  geom_smooth(
    data = subset(complete, regvis_scanpma >= 36),
    aes(color = group, group = group),
    method = "lm",
    se = FALSE,
    linewidth = 1
  ) +
  scale_x_continuous(
    breaks = seq(27, 42, by = 1),
    limits = c(27, 42)
  ) +
  scale_y_continuous(
    breaks = seq(0.28, 0.42, by = 0.02),
    limits = c(0.28, 0.42)
  ) +
  scale_color_manual(
    values = c(
      "FT" = "#619CFF",
      "VPT_noinjury" = "#00BA38",
      "VPT_injury" = "#F8766D"
    )
  ) +
  facet_wrap(~ROI) +
  labs(
    x = "Post Menstrual Age at Scan",
    y = "Radial Diffusivity (RD)",
    color = "Subject Group"
  ) +
  theme_classic()


#Figure 1B: ggseg correlation plots with age before and after 34 weeks ------------------------------

after34 <- completeallROIs %>% filter(regvis_scanpma > 34)

#realized there were some repeats
after34 <- after34 %>%
  distinct(RealID, ROI, .keep_all = TRUE)

before34 <- completeallROIs %>% filter(regvis_scanpma <= 34)


#correlations before 34 weeks 
FAcorrsEarly <- before34 %>%
  group_by(ROI) %>%
  summarise(
    n = sum(is.finite(FA) & is.finite(regvis_scanpma)),
    r_FA = if (n >= 3) {
      cor(
        FA, regvis_scanpma,
        use = "complete.obs"
      )
    } else {
      NA_real_
    },
    p_FA = if (n >= 3 &&
               sd(FA, na.rm = TRUE) > 0 &&
               sd(regvis_scanpma, na.rm = TRUE) > 0) {
      cor.test(
        FA, regvis_scanpma,
        use = "complete.obs"
      )$p.value
    } else {
      NA_real_
    },
    .groups = "drop"
  )

ADcorrsEarly <- before34 %>%
  group_by(ROI) %>%
  summarise(
    n = sum(is.finite(AD) & is.finite(regvis_scanpma)),
    r_AD = if (n >= 3) {
      cor(
        AD, regvis_scanpma,
        use = "complete.obs"
      )
    } else {
      NA_real_
    },
    p_AD = if (n >= 3 &&
               sd(AD, na.rm = TRUE) > 0 &&
               sd(regvis_scanpma, na.rm = TRUE) > 0) {
      cor.test(
        AD, regvis_scanpma,
        use = "complete.obs"
      )$p.value
    } else {
      NA_real_
    },
    .groups = "drop"
  )

RDcorrsEarly <- before34 %>%
  group_by(ROI) %>%
  summarise(
    n = sum(is.finite(RD) & is.finite(regvis_scanpma)),
    r_RD = if (n >= 3) {
      cor(
        RD, regvis_scanpma,
        use = "complete.obs"
      )
    } else {
      NA_real_
    },
    p_RD = if (n >= 3 &&
               sd(RD, na.rm = TRUE) > 0 &&
               sd(regvis_scanpma, na.rm = TRUE) > 0) {
      cor.test(
        RD, regvis_scanpma,
        use = "complete.obs"
      )$p.value
    } else {
      NA_real_
    },
    .groups = "drop"
  )

completecorrsearly <- FAcorrsEarly %>% left_join(ADcorrsEarly) %>% left_join(RDcorrsEarly)
completecorrsearly <- completecorrsearly %>%
  mutate(
    hemi = ifelse(grepl("^L_", ROI), "left", "right"),
    region = sub("^[LR]_", "", ROI)
  )
completecorrsearly$region <- gsub("\\.", "-", completecorrsearly$region)

#making ggseg plot with all parcels showing
ggseg(completecorrsearly,
      colour = "black", 
      atlas = glasser, 
      position = "stacked",
      mapping = aes(fill = r_RD)) +
  theme_classic() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-0.5, 0.5),
    breaks = seq(-0.5, 0.5, by = 0.25),
    na.value = "white",
    oob = scales::squish
  ) +
  labs(title = "Correlation Between FA and PMA at Scan at TEA", 
       subtitle = "Scans <=34 weeks",
       fill = "R")

#correlations after 34 weeks 
FAcorrsLate <- after34 %>%
  group_by(ROI) %>%
  summarise(
    n = sum(is.finite(FA) & is.finite(regvis_scanpma)),
    r_FA = if (n >= 3) {
      cor(
        FA, regvis_scanpma,
        use = "complete.obs"
      )
    } else {
      NA_real_
    },
    p_FA = if (n >= 3 &&
               sd(FA, na.rm = TRUE) > 0 &&
               sd(regvis_scanpma, na.rm = TRUE) > 0) {
      cor.test(
        FA, regvis_scanpma,
        use = "complete.obs"
      )$p.value
    } else {
      NA_real_
    },
    .groups = "drop"
  )

ADcorrsLate <- after34 %>%
  group_by(ROI) %>%
  summarise(
    n = sum(is.finite(AD) & is.finite(regvis_scanpma)),
    r_AD = if (n >= 3) {
      cor(
        AD, regvis_scanpma,
        use = "complete.obs"
      )
    } else {
      NA_real_
    },
    p_AD = if (n >= 3 &&
               sd(AD, na.rm = TRUE) > 0 &&
               sd(regvis_scanpma, na.rm = TRUE) > 0) {
      cor.test(
        AD, regvis_scanpma,
        use = "complete.obs"
      )$p.value
    } else {
      NA_real_
    },
    .groups = "drop"
  )

RDcorrsLate <- after34 %>%
  group_by(ROI) %>%
  summarise(
    n = sum(is.finite(RD) & is.finite(regvis_scanpma)),
    r_RD = if (n >= 3) {
      cor(
        RD, regvis_scanpma,
        use = "complete.obs"
      )
    } else {
      NA_real_
    },
    p_RD = if (n >= 3 &&
               sd(RD, na.rm = TRUE) > 0 &&
               sd(regvis_scanpma, na.rm = TRUE) > 0) {
      cor.test(
        RD, regvis_scanpma,
        use = "complete.obs"
      )$p.value
    } else {
      NA_real_
    },
    .groups = "drop"
  )

completecorrslate <- FAcorrsLate %>% left_join(ADcorrsLate) %>% left_join(RDcorrsLate)
completecorrslate <- completecorrslate %>%
  mutate(
    hemi = ifelse(grepl("^L_", ROI), "left", "right"),
    region = sub("^[LR]_", "", ROI)
  )
completecorrslate$region <- gsub("\\.", "-", completecorrslate$region)

#making ggseg plot with all parcels showing
ggseg(completecorrslate,
      colour = "black", 
      atlas = glasser, 
      position = "stacked",
      mapping = aes(fill = r_RD)) +
  theme_classic() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-0.5, 0.5),
    breaks = seq(-0.5, 0.5, by = 0.25),
    na.value = "white",
    oob = scales::squish
  ) +
  labs(title = "Correlation Between FA and PMA at Scan at TEA", 
       subtitle = "Scans >34 weeks",
       fill = "") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )+guides(fill = guide_colorbar(
    direction = "horizontal",
    barwidth = 15,
    barheight = 0.6
  ))







#Supplemental Figure 1: Ages at scan for 6 preterm subjects -------------
makinglastfig <- timesfrompreemie %>% mutate(
  subject = substr(RealID, 1, 6)
)

ggplot(makinglastfig, aes(x = regvis_scanpma, y = subject, group = subject)) +
  geom_line(color = "gray80", linewidth = 0.4) +
  geom_point(
    size = 2.5) +
  theme_minimal() +
  labs(
    x = "PMA at Scan (Weeks)",
    y = "Subject ID"
  )+ 
  theme(axis.text.y = element_text(size = 10)) +
  scale_x_continuous(
    breaks = seq(26, 40, by = 1),
    limits = c(26, 40)
  )







#Figure 2: How do diffusion metrics at TEA differ by group, on both the parcel and whole brain level? ------------------

#parcel level
mean_by_group_roi <- filteredsection1 %>%
  group_by(group, ROI, hemi, region) %>%
  summarise(
    mean_FA = mean(FA, na.rm = TRUE),
    mean_MD = mean(MD, na.rm = TRUE),
    mean_AD = mean(AD, na.rm = TRUE),
    mean_RD = mean(RD, na.rm = TRUE),
    .groups = "drop"
  )

mean_by_group_roi_long <- mean_by_group_roi %>%
  pivot_longer(
    cols = c(mean_FA, mean_MD, mean_AD, mean_RD),
    names_to = "metric",
    values_to = "value"
  )

make_plot <- function(group_name, diffusion_metric) {
  
  plot_data <- mean_by_group_roi_long %>%
    filter(group == group_name) %>%
    filter(metric == diffusion_metric)
  
  palette <- switch(
    diffusion_metric,
    "mean_FA" = "viridis",
    "mean_MD" = "magma",
    "mean_AD" = "plasma",
    "mean_RD" = "cividis")
  
  metric_limits <- switch(
    diffusion_metric,
    "mean_FA" = c(0.12, 0.20),
    "mean_MD" = c(1.06, 1.23),
    "mean_AD" = c(0.41, 0.49),
    "mean_RD" = c(0.31, 0.38))
  
  group_title <- switch(
    group_name,
    "FT" = "Full Term",
    "VPT_noinjury" = "Preterm Without Injury",
    "VPT_injury" = "Preterm With Injury")
  
  ggseg(
    plot_data,
    atlas = glasser,
    position = "dispersed",
    colour = "black",
    mapping = aes(fill = value)) +
    scale_fill_viridis_c(
      option = palette, 
      na.value = "white",
      direction = 1,
      limits = metric_limits,
      oob = scales::squish,
      name = paste("Mean", sub("mean_", "", diffusion_metric),
                   "Across Subjects")
    ) +
    labs(title = group_title)+
    theme_classic() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 10)
    ) +
    guides(
      fill = guide_colorbar(
        direction = "horizontal",
        barwidth = unit(12, "cm"),
        barheight = unit(0.6, "cm"),
        title.position = "top"
      ))
}

p_FT_FA <- make_plot("FT", "mean_FA")
p_VPT_noinjury_FA <- make_plot("VPT_noinjury", "mean_FA")
p_VPT_injury_FA <- make_plot("VPT_injury", "mean_FA")

(p_FT_FA / p_VPT_noinjury_FA / p_VPT_injury_FA) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )

p_FT_MD <- make_plot("FT", "mean_MD")
p_VPT_noinjury_MD <- make_plot("VPT_noinjury", "mean_MD")
p_VPT_injury_MD <- make_plot("VPT_injury", "mean_MD")

(p_FT_MD / p_VPT_noinjury_MD / p_VPT_injury_MD) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )

p_FT_AD <- make_plot("FT", "mean_AD")
p_VPT_noinjury_AD <- make_plot("VPT_noinjury", "mean_AD")
p_VPT_injury_AD <- make_plot("VPT_injury", "mean_AD")

(p_FT_AD / p_VPT_noinjury_AD / p_VPT_injury_AD) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )

p_FT_RD <- make_plot("FT", "mean_RD")
p_VPT_noinjury_RD <- make_plot("VPT_noinjury", "mean_RD")
p_VPT_injury_RD <- make_plot("VPT_injury", "mean_RD")

(p_FT_RD / p_VPT_noinjury_RD / p_VPT_injury_RD) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )




#whole brain level
submeansacrossbrain <- filteredsection1 %>%
  group_by(RealID) %>%
  summarise(
    group = first(group),
    mean_FA = mean(FA, na.rm = TRUE),
    mean_MD = mean(MD, na.rm = TRUE),
    mean_AD = mean(AD, na.rm = TRUE),
    mean_RD = mean(RD, na.rm = TRUE)
  ) 

submeansacrossbrain <- submeansacrossbrain %>%
  mutate(group = factor(
    group,
    levels = c("FT", "VPT_noinjury", "VPT_injury")
  ))

submeans_long <- submeansacrossbrain %>%
  pivot_longer(
    cols = c(mean_FA, mean_MD, mean_AD, mean_RD),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    group = factor(
      group,
      levels = c("FT", "VPT_noinjury", "VPT_injury")
    ),
    metric = factor(
      metric,
      levels = c("mean_FA", "mean_MD", "mean_AD", "mean_RD"),
      labels = c("FA", "MD", "AD", "RD")
    )
  )

comparisons <- list(
  c("FT", "VPT_noinjury"),
  c("FT", "VPT_injury"),
  c("VPT_noinjury", "VPT_injury")
)

make_metric_plot <- function(metric_name, y_label) {
  plot_data <- submeans_long %>%
    filter(metric == metric_name)
  ggplot(
    plot_data,
    aes(x = group, y = value, fill = group, color = group)
  ) +
    geom_violin(trim = FALSE, alpha = 0.5) +
    geom_jitter(width = 0.1, size = 2) +
    stat_summary(
      fun = mean,
      geom = "crossbar",
      width = 0.4,
      color = "black",
      linewidth = 0.5
    ) +
    geom_signif(
      comparisons = comparisons,
      test = "t.test",
      map_signif_level = TRUE,
      step_increase = 0.12,
      color = "black",
      textsize = 4
    ) +
    scale_x_discrete(labels = c(
      "FT" = "Full Term",
      "VPT_noinjury" = "Preterm without Injury",
      "VPT_injury" = "Preterm with Injury"
    )) +
    scale_fill_manual(
      name = "Group",
      values = c(
        "FT" = "#4E79A7",
        "VPT_noinjury" = "#59A14F",
        "VPT_injury" = "#E15759"
      ),
      labels = c(
        "FT" = "Full Term",
        "VPT_noinjury" = "Preterm without Injury",
        "VPT_injury" = "Preterm with Injury"
      )
    ) +
    scale_color_manual(
      name = "Group",
      values = c(
        "FT" = "#4E79A7",
        "VPT_noinjury" = "#59A14F",
        "VPT_injury" = "#E15759"
      ),
      labels = c(
        "FT" = "Full Term",
        "VPT_noinjury" = "Preterm without Injury",
        "VPT_injury" = "Preterm with Injury"
      )
    ) +
    labs(
      x = "Group",
      y = y_label,
      title = metric_name
    )+ 
    scale_y_continuous(breaks = scales::breaks_pretty(n = 7)) +
    theme_classic() +
    theme(
      plot.title = element_text(
        size = 15,
        face = "bold",
        hjust = 0.5
      ),
      panel.border = element_rect(
        color = "grey50",
        fill = NA,
        linewidth = 0.8
      ),
      legend.position = "bottom",
      axis.text.x = element_text(size = 9),
      plot.margin = margin(10, 10, 10, 10)
    )
}

p_FA <- make_metric_plot("FA", "Mean FA Per Subject")
p_MD <- make_metric_plot("MD", "Mean MD Per Subject")
p_AD <- make_metric_plot("AD", "Mean AD Per Subject")
p_RD <- make_metric_plot("RD", "Mean RD Per Subject")

(p_FA + p_MD) / (p_AD + p_RD) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")





#Figure 3: How does expansion differ by time window (and later by group)? ---------------------------

#figure out the time windows
timewindows <- read_xlsx("C:/Users/lisa.gorham/Box/2023 PhD Files/Aim 2 Diffusion/expansionwindowsforpreemies.xlsx")
timewindows <- timewindows %>% mutate(age_years = NA)
timewindows <- timewindows %>% filter(subject_id %in% c("neo046", "caf049", "caf021", "caf019", "caf011", "caf009"))
extrarows<- read_xlsx("C:/Users/lisa.gorham/Box/2023 PhD Files/Aim 2 Diffusion/extrarows.xlsx")
completewindows <- rbind(timewindows, extrarows)
completewindows <- completewindows %>% mutate(
  starting.ses = substr(regvis_scanid_timepoint1, 7,8),
  ending.ses = substr(regvis_scanid_timepoint2, 7, 8)
)
colnames(completewindows)[1] = "subject"

#pull in the expansion data
colnames(expansion_section2)[3] = "ending.ses"
expansion_s2_long <- expansion_section2 %>%
  pivot_longer(
    cols = -c(subject, starting.ses, ending.ses),
    names_to = "ROI",
    values_to = "value"
  )%>%
  mutate(
    ROI = str_sub(ROI, 1, -15)
  )

four910scans <- section1data %>% filter(recruitmentid == "neo046" | recruitmentid == "caf049" | recruitmentid == "caf021" | recruitmentid == "caf019")
four910scans <- four910scans %>% select(c(RealID, recruitmentid, ROI, Expansion))
four910scans <- four910scans %>%
  mutate(
    starting.ses = substr(RealID, 7,8),
    ending.ses = "e",
    subject = recruitmentid
  ) %>%
  select(subject, starting.ses, ending.ses, ROI, Expansion)
colnames(four910scans)[5] = "value"
four910scans <- four910scans %>%
  mutate(
    subject = dplyr::recode(subject, "neo046" = "neo305")
  )

expansionsection2complete <- rbind(expansion_s2_long, four910scans)

#add it to the completewindows dataset
completewindows <- completewindows %>%
  mutate(
    subject = dplyr::recode(subject, "neo046" = "neo305")
  )
section2temporary <- completewindows %>% left_join(expansionsection2complete)
colnames(section2temporary)[13] = "expansion"

neonatalexpansion <- section2temporary %>% filter(pma_timepoint2 < 50)
neonatalexpansion <- neonatalexpansion %>% mutate(
  timebetween = pma_timepoint2 - pma_timepoint1,
  midpointage = (pma_timepoint2 + pma_timepoint1)/2,
  expansionperweek = expansion/timebetween,
  percentchange = ((2^expansion)-1)*100,
  percentchangeperweek = percentchange/timebetween
)

plot(neonatalexpansion$midpointage, neonatalexpansion$percentchangeperweek, xlab = "Midpoint age between scans", 
     ylab = "Expansion Per Week")


#could do average for scans with midpoint age between scans less than 30 weeks, 30-32 weeks, and >34 weeks?

lessthan30 <- neonatalexpansion %>% filter(midpointage < 30)
middle <- neonatalexpansion %>% filter(midpointage <34 & midpointage > 29)
olderthan34 <- neonatalexpansion %>% filter(midpointage > 34)

average_by_ROI_lessthan30 <- lessthan30 %>%
  dplyr::group_by(ROI) %>%
  dplyr::summarise(
    mean_percentchangeperweek = mean(percentchangeperweek),
    .groups = "drop"
  )

average_by_ROI_middle <- middle %>%
  dplyr::group_by(ROI) %>%
  dplyr::summarise(
    mean_percentchangeperweek = mean(percentchangeperweek),
    .groups = "drop"
  )

average_by_ROI_olderthan34 <- olderthan34 %>%
  dplyr::group_by(ROI) %>%
  dplyr::summarise(
    mean_percentchangeperweek = mean(percentchangeperweek),
    .groups = "drop"
  )

average_by_ROI_lessthan30 <- average_by_ROI_lessthan30 %>%
  mutate(
    hemi = ifelse(grepl("^L_", ROI), "left", "right"),
    region = sub("^[LR]_", "", ROI)
  )
average_by_ROI_lessthan30$region <- gsub("\\.", "-", average_by_ROI_lessthan30$region)


#making ggseg plot with all parcels showing
ggseg(average_by_ROI_lessthan30,
      colour = "black", 
      atlas = glasser, 
      position = "dispersed",
      mapping = aes(fill = mean_percentchangeperweek)) +
  theme_classic() +
  scale_fill_viridis_c(option = "turbo", na.value = "white", direction = 1,
                       limits = c(5, 20),
                       breaks = seq(5, 20, by = 5)) +
  labs(title = "Average Expansion per Week", 
       subtitle = "For Scans with a Midpoint Age < 30 Weeks",
       fill = "Percent Change")

average_by_ROI_middle <- average_by_ROI_middle %>%
  mutate(
    hemi = ifelse(grepl("^L_", ROI), "left", "right"),
    region = sub("^[LR]_", "", ROI)
  )
average_by_ROI_middle$region <- gsub("\\.", "-", average_by_ROI_middle$region)


#making ggseg plot with all parcels showing
ggseg(average_by_ROI_middle,
      colour = "black", 
      atlas = glasser, 
      position = "dispersed",
      mapping = aes(fill = mean_percentchangeperweek)) +
  theme_classic() +
  scale_fill_viridis_c(option = "turbo", na.value = "white", direction = 1,
                       limits = c(5, 20),
                       breaks = seq(5, 20, by = 5)) +
  labs(title = "Average Expansion per Week", 
       subtitle = "For Scans with a Midpoint Age Between 30 and 34 Weeks",
       fill = "Percent Change")

average_by_ROI_olderthan34 <- average_by_ROI_olderthan34 %>%
  mutate(
    hemi = ifelse(grepl("^L_", ROI), "left", "right"),
    region = sub("^[LR]_", "", ROI)
  )
average_by_ROI_olderthan34$region <- gsub("\\.", "-", average_by_ROI_olderthan34$region)


#making ggseg plot with all parcels showing
ggseg(average_by_ROI_olderthan34,
      colour = "black", 
      atlas = glasser, 
      position = "dispersed",
      mapping = aes(fill = mean_percentchangeperweek)) +
  theme_classic() +
  scale_fill_viridis_c(option = "turbo", na.value = "white", direction = 1,
                       limits = c(5, 20),
                       breaks = seq(5, 20, by = 5)) +
  labs(title = "Average Expansion per Week", 
       subtitle = "For Scans with a Midpoint Age > 34 Weeks",
       fill = "Percent Change")







#Figure 4: How do diffusion metrics relate to expansion in the preterm period? -----------------------

#since we want to split to before/after 34 weeks, first get expansion data set up
ac_expansion <- neonatalexpansion %>%
  filter(subject %in% c("caf009", "caf011", "neo305"))

ac_expansion_wide <- ac_expansion %>%
  pivot_wider(
    id_cols = c(subject, 
                window_timepoint1, 
                regvis_scanid_timepoint1, 
                window_timepoint2,
                regvis_scanid_timepoint2,
                pma_timepoint1,
                pma_timepoint2,
                change_window,
                age_years,
                starting.ses,
                ending.ses
    ),
    names_from = ROI,
    values_from = expansion
  )

meta <- c(
  "subject", "window_timepoint1", "regvis_scanid_timepoint1",
  "window_timepoint2", "regvis_scanid_timepoint2",
  "pma_timepoint1", "pma_timepoint2", "change_window",
  "age_years", "starting.ses", "ending.ses"
)

rois <- setdiff(names(ac_expansion_wide), meta)

ac_rows <- inner_join(
  filter(ac_expansion_wide, change_window == "a_to_b"),
  filter(ac_expansion_wide, change_window == "b_to_c"),
  by = "subject",
  suffix = c("_ab", "_bc")
)
for (r in rois) {
  ac_rows[[r]] <- ac_rows[[paste0(r, "_ab")]] +
    ac_rows[[paste0(r, "_bc")]]
}

ac_rows <- ac_rows %>%
  transmute(
    subject,
    window_timepoint1 = "week_28",
    regvis_scanid_timepoint1 = paste0(subject, "a"),
    window_timepoint2 = "week_34",
    regvis_scanid_timepoint2 = paste0(subject, "c"),
    pma_timepoint1 = pma_timepoint1_ab,
    pma_timepoint2 = pma_timepoint2_bc,
    change_window = "a_to_c",
    age_years = age_years_ab,
    starting.ses = "a",
    ending.ses = "c",
    across(all_of(rois))
  )

ac_rows_long <- ac_rows %>%
  pivot_longer(
    cols = all_of(rois),
    names_to = "ROI",
    values_to = "expansion"
  )

ac_rows_long <- ac_rows_long %>%
  mutate(
    timebetween = pma_timepoint2 - pma_timepoint1,
    midpointage = (pma_timepoint2 + pma_timepoint1)/2,
    expansionperweek = expansion/timebetween,
    percentchange = ((2^expansion)-1)*100,
    percentchangeperweek = percentchange/timebetween
  )

otherearlyrows <- neonatalexpansion %>% filter(subject %in% c("caf049", "caf021", "caf019")) %>% filter(pma_timepoint2 < 34)
expansionbefore34 <- rbind(ac_rows_long, otherearlyrows)
expansionbefore34$groupforplot <- "earlyscans"

expansionafter34 <- neonatalexpansion %>% filter(pma_timepoint1 >= 32)
expansionafter34$groupforplot <- "latescans"

completeforshowing <- rbind(expansionbefore34, expansionafter34)
completeforshowing$ROI <- gsub("\\.", "-", completeforshowing$ROI)
completeforshowing <- completeforshowing %>%
  mutate(
    hemi = ifelse(
      grepl("^L_", ROI), "left", "right"
    ),
    region = substr(ROI, 3, nchar(ROI))
  )


#pull in the diffusion data
diffusiontemp <- diffusion_long %>% select(c(subject, session, ROI, FA_1p35, MD_1p35, AD_1p35, RD_1p35))

completeforshowing <- completeforshowing %>%
  left_join(
    diffusiontemp %>%
      rename(
        starting.ses = session,
        FA_starting = FA_1p35,
        MD_starting = MD_1p35,
        AD_starting = AD_1p35,
        RD_starting = RD_1p35
      ),
    by = c("subject", "ROI", "starting.ses")
  )
completeforshowing <- completeforshowing %>%
  left_join(
    diffusiontemp %>%
      rename(
        ending.ses = session,
        FA_ending = FA_1p35,
        MD_ending = MD_1p35,
        AD_ending = AD_1p35,
        RD_ending = RD_1p35
      ),
    by = c("subject", "ROI", "ending.ses")
  )

completeforshowing <- completeforshowing %>%
  mutate(
    FA_difference = FA_ending - FA_starting,
    AD_difference = AD_ending - AD_starting,
    RD_difference = RD_ending - RD_starting,
    FA_diff_perweek = FA_difference/timebetween,
    AD_diff_perweek = AD_difference/timebetween,
    RD_diff_perweek = RD_difference/timebetween
  )

plot(completeforshowing$FA_difference, completeforshowing$percentchangeperweek)
cor.test(completeforshowing$FA_difference, completeforshowing$percentchangeperweek)

averagedifferences <- completeforshowing %>% 
  group_by(groupforplot, ROI) %>%
  summarise(
    mean_FA_difference_perweek = mean(FA_diff_perweek, na.rm = TRUE),
    mean_AD_difference_perweek = mean(AD_diff_perweek, na.rm = TRUE),
    mean_RD_difference_perweek = mean(RD_diff_perweek, na.rm = TRUE),
    mean_percentchange_perweek = mean(percentchangeperweek, na.rm = TRUE)
  )

averagedifferences$ROI <- gsub("\\.", "-", averagedifferences$ROI)
averagedifferences <- averagedifferences %>%
  mutate(
    hemi = ifelse(
      grepl("^L_", ROI), "left", "right"
    ),
    region = substr(ROI, 3, nchar(ROI))
  )
averagedifferences$region <- gsub("\\.", "-", averagedifferences$region)

expansionplot <- ggseg(
  averagedifferences,
  colour = "black",
  atlas = glasser,
  position = "dispersed",
  mapping = aes(fill = mean_percentchange_perweek)
) +
  theme_classic() +
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "white",
    direction = 1
  ) +
  facet_wrap(~groupforplot, nrow = 1)+
  labs(
    fill = "Expansion % Change Per Week"
  )
expansionplot

FAplot <- ggseg(
  averagedifferences,
  colour = "black",
  atlas = glasser,
  position = "dispersed",
  mapping = aes(fill = mean_FA_difference_perweek)
) +
  theme_classic() +
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "white",
    direction = -1
  ) +
  facet_wrap(~groupforplot, nrow = 1)+
  labs(
    fill = "FA Per Week"
  )
FAplot

ADplot <- ggseg(
  averagedifferences,
  colour = "black",
  atlas = glasser,
  position = "dispersed",
  mapping = aes(fill = mean_AD_difference_perweek)
) +
  theme_classic() +
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "white",
    direction = -1
  ) +
  facet_wrap(~groupforplot, nrow = 1)+
  labs(
    fill = "AD Per Week"
  )
ADplot

RDplot <- ggseg(
  averagedifferences,
  colour = "black",
  atlas = glasser,
  position = "dispersed",
  mapping = aes(fill = mean_RD_difference_perweek)
) +
  theme_classic() +
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "white",
    direction = 1
  ) +
  facet_wrap(~groupforplot, nrow = 1)+
  labs(
    fill = "RD Per Week"
  )
RDplot


(expansionplot / FAplot / ADplot / RDplot) +
  plot_layout(ncol = 1, heights = c(1, 1, 1, 1))

expansionplot <- expansionplot +
  facet_wrap(~groupforplot, nrow = 1)

# Remove facet labels from the remaining rows
FAplot <- FAplot + theme(strip.text = element_blank())
ADplot <- ADplot + theme(strip.text = element_blank())
RDplot <- RDplot + theme(strip.text = element_blank())

(expansionplot / FAplot / ADplot / RDplot) +
  plot_layout(ncol = 1, heights = c(1, 1, 1, 1))

#trying it splitting for all 8
expansion_early <- ggseg(
  filter(averagedifferences, groupforplot == "earlyscans"),
  colour = "black",
  atlas = glasser,
  position = "dispersed",
  mapping = aes(fill = mean_percentchange_perweek)
) +
  theme_classic() +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "white",
    direction = 1
  ) +
  labs(fill = "Expansion % Change Per Week", title = "Early scans")

# Expansion: late scans
expansion_late <- ggseg(
  filter(averagedifferences, groupforplot == "latescans"),
  colour = "black",
  atlas = glasser,
  position = "dispersed",
  mapping = aes(fill = mean_percentchange_perweek)
) +
  theme_classic() +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "white",
    direction = 1
  ) +
  labs(fill = "Expansion % Change Per Week", title = "Late scans")

expansion_early + expansion_late

FA_early <- ggseg(
  filter(averagedifferences, groupforplot == "earlyscans"),
  colour = "black",
  atlas = glasser,
  position = "dispersed",
  mapping = aes(fill = mean_FA_difference_perweek)
) +
  theme_classic() +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "white",
    direction = -1
  ) +
  labs(fill = "FA Change Per Week", title = "Early scans")

FA_late <- ggseg(
  filter(averagedifferences, groupforplot == "latescans"),
  colour = "black",
  atlas = glasser,
  position = "dispersed",
  mapping = aes(fill = mean_FA_difference_perweek)
) +
  theme_classic() +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "white",
    direction = -1
  ) +
  labs(fill = "FA Change Per Week", title = "Late scans")

AD_early <- ggseg(
  filter(averagedifferences, groupforplot == "earlyscans"),
  colour = "black",
  atlas = glasser,
  position = "dispersed",
  mapping = aes(fill = mean_AD_difference_perweek)
) +
  theme_classic() +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "white",
    direction = -1
  ) +
  labs(fill = "AD Change Per Week", title = "Early scans")

AD_late <- ggseg(
  filter(averagedifferences, groupforplot == "latescans"),
  colour = "black",
  atlas = glasser,
  position = "dispersed",
  mapping = aes(fill = mean_AD_difference_perweek)
) +
  theme_classic() +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "white",
    direction = -1
  ) +
  labs(fill = "AD Change Per Week", title = "Late scans")

RD_early <- ggseg(
  filter(averagedifferences, groupforplot == "earlyscans"),
  colour = "black",
  atlas = glasser,
  position = "dispersed",
  mapping = aes(fill = mean_RD_difference_perweek)
) +
  theme_classic() +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "white",
    direction = 1
  ) +
  labs(fill = "RD Change Per Week", title = "Early scans")

RD_late <- ggseg(
  filter(averagedifferences, groupforplot == "latescans"),
  colour = "black",
  atlas = glasser,
  position = "dispersed",
  mapping = aes(fill = mean_RD_difference_perweek)
) +
  theme_classic() +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "white",
    direction = 1
  ) +
  labs(fill = "RD Change Per Week", title = "Late scans")

#to see the 8 plots
expansion_early 
expansion_late
FA_early 
FA_late
AD_early 
AD_late
RD_early 
RD_late

#actually doing the correlations systematically

safe_cor <- function(x, y) {
  complete <- complete.cases(x, y) & is.finite(x) & is.finite(y)
  
  if (sum(complete) < 2 ||
      sd(x[complete]) == 0 ||
      sd(y[complete]) == 0) {
    return(NA_real_)
  }
  
  cor(x[complete], y[complete])
}

corrs <- completeforshowing %>%
  group_by(regvis_scanid_timepoint1, groupforplot ) %>%
  summarise(
    r_FA1exp = safe_cor(FA_starting, percentchangeperweek),
    r_AD1exp = safe_cor(AD_starting, percentchangeperweek),
    r_RD1exp = safe_cor(RD_starting, percentchangeperweek),
    r_FA2exp = safe_cor(FA_ending, percentchangeperweek),
    r_AD2exp = safe_cor(AD_ending, percentchangeperweek),
    r_RD2exp = safe_cor(RD_ending, percentchangeperweek),
    r_FAdiffexp = safe_cor(FA_diff_perweek, percentchangeperweek),
    r_ADdiffexp = safe_cor(AD_diff_perweek, percentchangeperweek),
    r_RDdiffexp = safe_cor(RD_diff_perweek, percentchangeperweek),
    .groups = "drop"
  )

result <- corrs %>%
  group_by(groupforplot) %>%
  summarise(
    across(
      where(is.numeric),
      ~ mean(.x, na.rm = TRUE)
    ))

result <- corrs %>%
  summarise(
    across(
      where(is.numeric),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      )
    )
  )%>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", ".value"),
    names_pattern = "(.*)_(mean|min|max)"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

hist(corrs$r_FAdiffexp,
     xlab = "Correlation",
     main = "Expansion and Change in FA")
hist(corrs$r_ADdiffexp,
     xlab = "Correlation",
     main = "Expansion and Change in AD")
hist(corrs$r_RDdiffexp,
     xlab = "Correlation",
     main = "Expansion and Change in RD")

corrs_long <- corrs %>%
  pivot_longer(
    cols = -c(regvis_scanid_timepoint1, groupforplot),
    names_to = "variable",
    values_to = "value"
  )

differencesonly <- corrs_long %>% filter(variable == "r_FAdiffexp" | variable == "r_ADdiffexp" | variable == "r_RDdiffexp")
differencesonly <- differencesonly %>%
  mutate(
    variable = case_when(
      variable == "r_FAdiffexp" ~ "Correlation Between Expansion and FA Difference",
      variable == "r_ADdiffexp" ~ "Correlation Between Expansion and AD Difference",
      variable == "r_RDdiffexp" ~ "Correlation Between Expansion and RD Difference"
    ),
    label = case_when(
      groupforplot == "earlyscans" ~ "Pairs of Scans < 34 Weeks",
      groupforplot == "latescans" ~ "Pairs of Scans > 34 Weeks"
    )
  )
differencesonly <- differencesonly %>%
  mutate(
    variable = factor(
      variable,
      levels = c(
        "Correlation Between Expansion and FA Difference",
        "Correlation Between Expansion and AD Difference",
        "Correlation Between Expansion and RD Difference"
      )
    )
  )
ggplot(differencesonly, aes(x = label, y = value, fill = label)) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted",
    color = "black"
  ) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_jitter(
    width = 0.1,
    alpha = 0.8,
    size = 2
  ) +
  geom_boxplot(
    width = 0.15,
    fill = "white",
    alpha = 0.8,
    outlier.shape = NA
  ) +
  facet_wrap(~variable) +
  theme_classic() +
  labs(
    x = "Time Window",
    y = "Correlation"
  )+ theme(legend.position = "none")

#testing kara's question about if change in diffusion during first window predicts expansion in second window
earlier <- completeforshowing %>%
  filter(change_window %in% c("a_to_b", "a_to_c")) %>%
  select(
    subject, ROI, change_window,
    FA_diff_perweek, AD_diff_perweek, RD_diff_perweek
  ) %>%
  rename(
    earlier_window = change_window,
    prior_FAchange = FA_diff_perweek,
    prior_ADchange = AD_diff_perweek,
    prior_RDchange = RD_diff_perweek
  )

later <- completeforshowing %>%
  filter(change_window %in% c("b_to_c", "c_to_d")) %>%
  select(
    subject, ROI, change_window, percentchangeperweek
  ) %>%
  rename(
    later_window = change_window,
    subsequent_expansion = percentchangeperweek
  )

lagged_data <- earlier %>%
  inner_join(later, by = c("subject", "ROI"))

subject_correlations <- lagged_data %>%
  group_by(subject) %>%
  summarise(
    cor_FA = cor(prior_FAchange, subsequent_expansion,
                 use = "complete.obs"),
    cor_AD = cor(prior_ADchange, subsequent_expansion,
                 use = "complete.obs"),
    cor_RD = cor(prior_RDchange, subsequent_expansion,
                 use = "complete.obs"),
    .groups = "drop"
  )

subject_correlations


#Figure 5: How do diffusion metrics at TEA relate to 10 year expansion? -------------------------

#first just plot average expansion per group
mean_expansion <- section1data %>%
  group_by(group, ROI, hemi, region) %>%
  summarise(
    mean_expansion = mean(Expansion, na.rm = TRUE),
    .groups = "drop"
  )

expansion_limits <- range(mean_expansion$mean_expansion, na.rm = TRUE)

make_plot <- function(group_name) {
  plot_data <- mean_expansion %>%
    filter(group == group_name)
  group_title <- switch(
    group_name,
    "FT" = "Full Term",
    "VPT_noinjury" = "Preterm Without Injury",
    "VPT_injury" = "Preterm With Injury")
  
  ggseg(
    plot_data,
    atlas = glasser,
    position = "dispersed",
    colour = "black",
    mapping = aes(fill = mean_expansion)) +
    scale_fill_viridis_c(
      option = "turbo", 
      na.value = "white",
      direction = 1,
      limits = expansion_limits,
      oob = scales::squish
    ) +
    labs(title = group_title)+
    theme_classic() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 10)
    ) +
    guides(
      fill = guide_colorbar(
        direction = "horizontal",
        barwidth = unit(12, "cm"),
        barheight = unit(0.6, "cm"),
        title.position = "top"
      ))
}

p_FT <- make_plot("FT")
p_VPT_noinjury <- make_plot("VPT_noinjury")
p_VPT_injury <- make_plot("VPT_injury")

(p_FT / p_VPT_noinjury / p_VPT_injury) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )

#looking at correlations for each subject
corrs <- filteredsection1 %>%
  group_by(RealID) %>%
  summarise(
    r_FAexp = cor(FA, Expansion),
    r_MDexp = cor(MD, Expansion),
    r_ADexp = cor(AD, Expansion),
    r_RDexp = cor(RD, Expansion)
  )
result <- corrs %>%
  summarise(
    across(
      where(is.numeric),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      )
    )
  )%>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", ".value"),
    names_pattern = "(.*)_(mean|min|max)"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))


par(mfrow = c(2, 2))

hist(corrs$r_FAexp,
     xlab = "FA and Expansion",
     main = "FA")

hist(corrs$r_MDexp,
     xlab = "MD and Expansion",
     main = "MD")

hist(corrs$r_ADexp,
     xlab = "AD and Expansion",
     main = "AD")

hist(corrs$r_RDexp,
     xlab = "RD and Expansion",
     main = "RD")

# Reset to one plot per page
par(mfrow = c(1, 1))




mean(corrs$r_FAexp)
mean(corrs$r_MDexp)
mean(corrs$r_ADexp)
mean(corrs$r_RDexp)


#making graphs for averages across all subs
means_for_all <- section1data %>%
  group_by(ROI, hemi, region) %>%
  summarise(
    mean_expansion = mean(Expansion, na.rm = TRUE),
    mean_FA = mean(FA, na.rm = TRUE),
    mean_AD = mean(AD, na.rm = TRUE),
    mean_RD = mean(RD, na.rm = TRUE),
    .groups = "drop"
  )

ggseg(
  means_for_all,
  atlas = glasser,
  position = "dispersed",
  colour = "black",
  mapping = aes(fill = mean_expansion)) +
  scale_fill_viridis_c(
    option = "turbo", 
    na.value = "white",
    direction = 1
  ) +
  labs(title = "Mean Expansion Across 71 Subjects")+
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10)
  ) +
  guides(
    fill = guide_colorbar(
      direction = "horizontal",
      barwidth = unit(12, "cm"),
      barheight = unit(0.6, "cm"),
      title.position = "top"
    ))

ggseg(
  means_for_all,
  atlas = glasser,
  position = "dispersed",
  colour = "black",
  mapping = aes(fill = mean_FA)) +
  scale_fill_viridis_c(
    option = "viridis", 
    na.value = "white",
    direction = 1,
    limits = c(0.12, 0.2),
    oob = scales::squish
  ) +
  labs(title = "Mean FA Across 71 Subjects")+
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10)
  ) +
  guides(
    fill = guide_colorbar(
      direction = "horizontal",
      barwidth = unit(12, "cm"),
      barheight = unit(0.6, "cm"),
      title.position = "top"
    ))

ggseg(
  means_for_all,
  atlas = glasser,
  position = "dispersed",
  colour = "black",
  mapping = aes(fill = mean_AD)) +
  scale_fill_viridis_c(
    option = "plasma", 
    na.value = "white",
    direction = 1,
    limits = c(0.4, 0.5),
    oob = scales::squish
  ) +
  labs(title = "Mean AD Across 71 Subjects")+
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10)
  ) +
  guides(
    fill = guide_colorbar(
      direction = "horizontal",
      barwidth = unit(12, "cm"),
      barheight = unit(0.6, "cm"),
      title.position = "top"
    ))

ggseg(
  means_for_all,
  atlas = glasser,
  position = "dispersed",
  colour = "black",
  mapping = aes(fill = mean_RD)) +
  scale_fill_viridis_c(
    option = "cividis", 
    na.value = "white",
    direction = 1,
    limits = c(0.3, 0.38),
    oob = scales::squish
  ) +
  labs(title = "Mean RD Across 71 Subjects")+
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10)
  ) +
  guides(
    fill = guide_colorbar(
      direction = "horizontal",
      barwidth = unit(12, "cm"),
      barheight = unit(0.6, "cm"),
      title.position = "top"
    ))

demosIcareabout <- section1data %>% select(c(RealID, group, regvis_scanpma, regfam_childsex, social_matedu_discharge, birth_ga)) %>% group_by(RealID) %>% slice(1)
corrs <- corrs %>% left_join(demosIcareabout)

model <- lm(r_FAexp ~ birth_ga + regvis_scanpma, data = corrs)
summary(model)
model <- lm(r_MDexp ~ birth_ga + regvis_scanpma, data = corrs)
summary(model)
model <- lm(r_ADexp ~ birth_ga + regvis_scanpma, data = corrs)
summary(model)
model <- lm(r_RDexp ~ birth_ga + regvis_scanpma, data = corrs)
summary(model)

