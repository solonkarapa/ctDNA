
library(stringr)
library(rebus)

# load data
load("/Users/solon/Cloud-Drive/Projects/ctDNA/pre-process:data/data/Antwerp/Treatments_simplified_Antwerp.Rdata")

# this will match treatments with the ones used in DETECT

# DO NOT change the order 
pattern_Exemestane_Everolimus <- c(START %R% or("Exemes", "Everol")  %R% 
                                       one_or_more(WRD) %R% one_or_more("+") %R% or("Everol", "Exemes") %R% 
                                       one_or_more(WRD) %R% END)
pattern_Trastuzumab <- exactly("Trastuzumab" %R% one_or_more("+") %R% "Trastuzumab")
pat1 <- exactly("Pertuzumab" %R% one_or_more("+") %R% "Trastuzumab")
pat2 <- exactly("Trastuzumab" %R% one_or_more("+") %R% "Pertuzumab")

Treatments_simplified_update <- Treatments_simplified %>% 
    mutate(Treatment_new = 
               case_when(
                   str_detect(Treatment, or("Capecitabine", "5FU", "Xeloda", "xeloda")) ~ "Capecitabine/5FU", 
                   str_detect(Treatment, or("capecitabine", "5FU")) ~ "Capecitabine/5FU", 
                   str_detect(Treatment, or("Carboplatin", "Gemcitabine", exactly("Gem/Carbo"))) ~ "Carboplatin/Gemcitabine",
                   str_detect(Treatment, or("Docetaxel", "Paclitaxel", "Abraxane", "Taxol", "docetaxel", "paclitaxel", "Taxotere")) ~ "Docetaxel/Paclitaxel",
                   str_detect(Treatment, or("Fluorouracil", "Cyclophosphamide", "CMF")) ~ "EFC/CMF", # should be before Epirubicin
                   str_detect(Treatment, or("Epirubicin", "epirubicine")) ~ "Epirubicin",
                   str_detect(Treatment, "Eribulin") ~ "Eribulin",
                   str_detect(Treatment, or("Vinorelbine", "Navelbine")) ~ "Vinorelbine",
                   str_detect(Treatment, or(pattern_Exemestane_Everolimus, "Everolimus- Exemestaan")) ~ "Exemestane/Everolimus",
                   str_detect(Treatment, or("Exemestane", "Aromasin", "exemestane", "aromasin")) ~ "Exemestane", # should be before Letrozole and after Exemestane/Everolimus
                   str_detect(Treatment, or("Letrozole", "letrozole", "Femara")) ~ "Letrozole", 
                   str_detect(Treatment, or("Emtansine", "T-DM1")) ~ "TDM-1", # should be after Letrozole 
                   str_detect(Treatment, or("Fulvestrant", "Faslodex", "faslodex")) ~ "Fulvestrant",
                   str_detect(Treatment, "Anastrazole") ~ "Anastrazole", # should be after TDM-1
                   str_detect(Treatment, or("Megestrol", "Megace")) ~ "Megestrol", # should be after Letrozole
                   str_detect(Treatment, or("Tamoxifen", "tamoxifen", "Nolvadex")) ~ "Tamoxifen",
                   str_detect(Treatment, "Denosumab") ~ "off-treatment",
                   str_detect(Treatment, or(exactly("Trastuzumab"), pattern_Trastuzumab, "trastuzumab")) ~ "Trastuzumab", 
                   str_detect(Treatment, or(pat1, pat2, exactly("Pertuzumab"))) ~ "Trastuzumab/Pertuzumab",
                   str_detect(Treatment, exactly("Lapatinib")) ~ "Lapatinib",
                   TRUE ~ "Trial Drug"))


setwd("/Users/solon/Cloud-Drive/Projects/ctDNA_original/ctDNA/validation/")
save(Treatments_simplified_update, file = "treatments_simplified_updated_Ant.Rdata")


