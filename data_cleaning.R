#load package
pacman::p_load(haven, tidyverse, readstata13, stats)

#read data
data <- haven::read_dta("C:/Users/Owner/Indian-cycling-B/replication data/113676-V1/dlhs_long_wdist.dta")

#filter data
data <- data %>%
  filter(state == 10 | state == 20)

#create variables
data <- data %>%
  mutate(
    inschool = ifelse(school == 1, 1, 0),
    currgrade = ifelse(inschool == 1, grade + 1, NA),
    enrollment_secschool = ifelse(currgrade == 9 | (grade >= 9 & !is.na(grade)), 1, 0),
    enrollment_middleschool = ifelse(currgrade == 8 | (grade >= 8 & !is.na(grade)), 1, 0),
    
    female = ifelse(sex == 2, 1, ifelse(sex == 1, 0, NA)),
    bihar = ifelse(state == 10, 1, ifelse(state == 20, 0, NA)),
    
    child_sample = ifelse(relationship %in% c(3, 5, 8, 10), 1, ifelse(is.na(relationship), 0, NA)),
    
    sc = ifelse(hv116b == 1, 1, if_else(is.na(hv116b), 0, 0)),
    st = ifelse(hv116b == 2, 1, if_else(is.na(hv116b), 0, 0)),
    obc = ifelse(hv116b == 3, 1, if_else(is.na(hv116b), 0, 0)),
    highcaste = ifelse(hv116b == 4, 1, if_else(is.na(hv116b), 0, 0)),
    scst = ifelse(hv116b %in% c(1, 2), 1, if_else(is.na(hv116b), 0, 0)),
    
    hindu = ifelse(hv115 == 1, 1, if_else(is.na(hv116b), 0, 0)),
    muslim = ifelse(hv115 == 2, 1, if_else(is.na(hv116b), 0, 0)),
    other = ifelse(hv115 > 2, 1, if_else(is.na(hv116b), 0, 0)),
    
    electricity = ifelse(hv129a == 1, 1, ifelse(hv129a == 2, 0, NA)),
    mattress = ifelse(hv129b == 1, 1, ifelse(hv129b == 2, 0, NA)),
    cooker = ifelse(hv129c == 1, 1, ifelse(hv129c == 2, 0, NA)),
    chair = ifelse(hv129d == 1, 1, ifelse(hv129d == 2, 0, NA)),
    sofa = ifelse(hv129e == 1, 1, ifelse(hv129e == 2, 0, NA)),
    bed = ifelse(hv129f == 1, 1, ifelse(hv129f == 2, 0, NA)),
    table = ifelse(hv129g == 1, 1, ifelse(hv129g == 2, 0, NA)),
    fan = ifelse(hv129h == 1, 1, ifelse(hv129h == 2, 0, NA)),
    sewing = ifelse(hv129l == 1, 1, ifelse(hv129l == 2, 0, NA)),
    phone = ifelse(hv129m == 1 | hv129n == 1, 1, ifelse(is.na(hv129m) & is.na(hv129n), 0, NA)),
    computer = ifelse(hv129o == 1, 1, ifelse(hv129o == 2, 0, NA)),
    fridge = ifelse(hv129p == 1, 1, ifelse(hv129p == 2, 0, NA)),
    washing_machine = ifelse(hv129q == 1, 1, ifelse(hv129q == 2, 0, NA)),
    watch = ifelse(hv129r == 1, 1, ifelse(hv129r == 2, 0, NA)),
    motor_cycle = ifelse(hv129t == 1, 1, ifelse(hv129t == 2, 0, NA)),
    animal_cart = ifelse(hv129u == 1, 1, ifelse(hv129u == 2, 0, NA)),
    car = ifelse(hv129v == 1, 1, ifelse(hv129v == 2, 0, NA)),
    tractor = ifelse(hv129w == 1, 1, ifelse(hv129w == 2, 0, NA)),
    water_pump = ifelse(hv129x == 1, 1, ifelse(hv129x == 2, 0, NA)),
    thresher = ifelse(hv129y == 1, 1, ifelse(hv129y == 2, 0, NA)),
    bike = ifelse(hv129s == 1, 1, ifelse(is.na(hv129s), 0, NA)),
    media = ifelse(hv129i == 1 | hv129j == 1 | hv129k == 1, 1, 0),
    #後で修正
    land = ifelse(hv130 == 2 | hv131 < 5, 1, 0),
    bpl = ifelse(hv134 == 1, 1, 0),
    
    secschool = ifelse(v115ca1 == 1, 1, ifelse(v115ca1 == 2, 0, NA)),
    secondarydist = ifelse(v115ca1 == 1 | v115cb1 == 1, 0, v115c2),
    longdist = ifelse(secondarydist > 3, 1, ifelse(secondarydist <= 3, 0, NA)),
    secondarydistsq = secondarydist * secondarydist,
    highsecondarydist = ifelse(v115da1 == 1 | v115db1 == 1, 0, v115d2)
  )

    
data <- data %>%
  mutate(
    secondarydist = round(secondarydist,1),
    highsecondarydist = round(highsecondarydist,1)
  )%>%
  filter(
    secondarydist !=99,
    highsecondarydist != 99
  )

data <- data %>% mutate(
  lsecondarydist = log(secondarydist),
  middle = if_else(v115ba1 == 1, 1, 0),
  middledist = if_else(v115bb1 == 1 | v115ba1 ==1, 0, v115b2),
  primary = if_else(v115aa1 == 1, 1, 0),
  primarydist = if_else(v115aa1 == 1 | v115ab1 ==1, 0, v115a2),
  pubcollege = if_else(v115ea1 == 1, 1, 0),
  pvtcollege = if_else(v115eb1 == 1, 1, 0),
  college = if_else(pubcollege ==1 | pvtcollege ==1, 1, 0),
  
  postoff = if_else(v122a==1, 1, 0),
  bank = if_else(v122d==1, 1, 0),
  
  towndist = v110,
  hqdist = v111,
  railwaydist = v112,
  busdist = v113,
  
  longraildist = if_else(railwaydist > 3, 1, 0),
  longbusdist = if_else(busdist > 3, 1, 0),
  longhqdist = if_else(hqdist > 3, 1, 0),
  longtowndist = if_else(towndist > 3, 1, 0),
  
  censuspop = v101a,
  lcensuspop= log(censuspop),
  currpop=v101b,
  lcurrpop = log(currpop),
  tothhvill=v102
)


data <- data %>% mutate(
  distborder = if_else(dist %in% c(1010, 1022, 1023, 1032, 1034, 1035, 1036, 
                                   1037, 2001, 2002, 2003, 2004, 2005, 2006, 
                                   2007, 2008, 2009, 2011), 1, 0),
  distborder = replace_na(distborder, 0),
  treat1 = case_when(age == 14 |age == 15 ~ 1,
                     age == 16 |age == 17 ~ 0,
                     TRUE~NA_real_
                     ),
  treat2 = case_when(age == 13 |age == 14 |age == 15 ~ 1,
                     age == 16 |age == 17 ~ 0,
                     TRUE~NA_real_
                     ) ,
  treat3 = case_when(age == 14 |age == 15 ~ 1,
                     age == 16 ~ 0,
                     TRUE~NA_real_
                     ) ,
  treat4 = case_when(age == 13 |age == 14 |age == 15 ~ 1,
                     age == 16 ~ 0,
                     TRUE~NA_real_
                     ) ,                       
  treat5 = case_when(age == 13 |age == 14  ~ 1,
                     age == 15|age == 16 ~ 0,
                     TRUE~NA_real_
                     ) 
)

data<-data %>% mutate(
  treat1_scst = treat1*scst,
  treat1_sc = treat1*sc,
  treat1_st = treat1*st,
  treat1_obc = treat1*obc,
  
  treat2_scst = treat2*scst,
  treat2_sc = treat2*sc,
  treat2_st = treat2*st,
  treat2_obc = treat2*obc,
  
  treat3_scst = treat3*scst,
  treat3_sc = treat3*sc,
  treat3_st = treat3*st,
  treat3_obc = treat3*obc,
  
  treat4_scst = treat4*scst,
  treat4_sc = treat4*sc,
  treat4_st = treat4*st,
  treat4_obc = treat4*obc,
  
  treat5_scst = treat5*scst,
  treat5_sc = treat5*sc,
  treat5_st = treat5*st,
  treat5_obc = treat5*obc,
  
  female_sc = female*sc,
  female_st = female*st,
  female_obc = female*obc,
  
  bihar_sc = bihar*sc,
  bihar_st = bihar*st,
  bihar_obc = bihar*obc,
  treat1_longdist = treat1*longdist,
  
  treat2_longdist = treat2*longdist,
  
  treat3_longdist = treat3*longdist,
  
  treat4_longdist = treat4*longdist,
  
  treat5_longdist = treat5*longdist,
  
  female_longdist = female*longdist,
  
  bihar_longdist = bihar*longdist,
  
  treat1_bihar_sc = treat1*bihar*sc,
  treat2_bihar_sc = treat2*bihar*sc,
  treat3_bihar_sc = treat3*bihar*sc,
  treat4_bihar_sc = treat4*bihar*sc,
  treat5_bihar_sc = treat5*bihar*sc,
  
  treat1_bihar_st = treat1*bihar*st,
  treat2_bihar_st = treat2*bihar*st,
  treat3_bihar_st = treat3*bihar*st,
  treat4_bihar_st = treat4*bihar*st,
  treat5_bihar_st = treat5*bihar*st,
  
  treat1_bihar_obc = treat1*bihar*obc,
  treat2_bihar_obc = treat2*bihar*obc,
  treat3_bihar_obc = treat3*bihar*obc,
  treat4_bihar_obc = treat4*bihar*obc,
  treat5_bihar_obc = treat5*bihar*obc,
  
  treat1_female_sc = treat1*female*sc,
  treat2_female_sc = treat2*female*sc,
  treat3_female_sc = treat3*female*sc,
  treat4_female_sc = treat4*female*sc,
  treat5_female_sc = treat5*female*sc,
  
  treat1_female_st = treat1*female*st,
  treat2_female_st = treat2*female*st,
  treat3_female_st = treat3*female*st,
  treat4_female_st = treat4*female*st,
  treat5_female_st = treat5*female*st,
  
  treat1_female_obc = treat1*female*obc,
  treat2_female_obc = treat2*female*obc,
  treat3_female_obc = treat3*female*obc,
  treat4_female_obc = treat4*female*obc,
  treat5_female_obc = treat5*female*obc,
  
  female_bihar_sc = female*bihar*sc,
  female_bihar_st = female*bihar*st,
  female_bihar_obc = female*bihar*obc,
  
  treat1_female_bihar_longdist = treat1*female*bihar*longdist,
  treat2_female_bihar_longdist = treat2*female*bihar*longdist,
  treat3_female_bihar_longdist = treat3*female*bihar*longdist,
  treat4_female_bihar_longdist = treat4*female*bihar*longdist,
  treat5_female_bihar_longdist = treat5*female*bihar*longdist,

  treat1_female_bihar_sc = treat1*female*bihar*sc,
  treat2_female_bihar_sc = treat2*female*bihar*sc,
  treat3_female_bihar_sc = treat3*female*bihar*sc,
  treat4_female_bihar_sc = treat4*female*bihar*sc,
  treat5_female_bihar_sc = treat5*female*bihar*sc,
  
  treat1_female_bihar_st = treat1*female*bihar*st,
  treat2_female_bihar_st = treat2*female*bihar*st,
  treat3_female_bihar_st = treat3*female*bihar*st,
  treat4_female_bihar_st = treat4*female*bihar*st,
  treat5_female_bihar_st = treat5*female*bihar*st,
  
  treat1_female_bihar_obc = treat1*female*bihar*obc,
  treat2_female_bihar_obc = treat2*female*bihar*obc,
  treat3_female_bihar_obc = treat3*female*bihar*obc,
  treat4_female_bihar_obc = treat4*female*bihar*obc,
  treat5_female_bihar_obc = treat5*female*bihar*obc,
  
 
  treat1_female_bihar = treat1*female*bihar,
  treat2_female_bihar = treat2*female*bihar,
  treat3_female_bihar = treat3*female*bihar,
  treat4_female_bihar = treat4*female*bihar,
  treat5_female_bihar = treat5*female*bihar,
  
  treat1_female = treat1*female,
  treat2_female = treat2*female,
  treat3_female = treat3*female,
  treat4_female = treat4*female,
  treat5_female = treat5*female,
  
  treat1_bihar = treat1*bihar,
  treat2_bihar = treat2*bihar,
  treat3_bihar = treat3*bihar,
  treat4_bihar = treat4*bihar,
  treat5_bihar = treat5*bihar,
  
  female_bihar = female*bihar,
  
  treat1_bihar_longdist = treat1*bihar*longdist,
  treat2_bihar_longdist = treat2*bihar*longdist,
  treat3_bihar_longdist = treat3*bihar*longdist,
  treat4_bihar_longdist = treat4*bihar*longdist,
  treat5_bihar_longdist = treat5*bihar*longdist,
  
  treat1_female_longdist = treat1*female*longdist,
  treat2_female_longdist = treat2*female*longdist,
  treat3_female_longdist = treat3*female*longdist,
  treat4_female_longdist = treat4*female*longdist,
  treat5_female_longdist = treat5*female*longdist,
  
  female_bihar_longdist = female*bihar*longdist
)

data <- data %>% 
  mutate(
    hhheadschool = replace_na(hhheadschool, 0)
  )

#line 512までOK

#主成分分析
pca_asset <- prcomp(data[, c("land", "bpl", "media", "electricity")], scale=TRUE)
data$pca_asset <- pca_asset$x[, 1]

pca_ses <- prcomp(data[, c("sc", "st", "obc", "hindu", "muslim", "hhheadschool",
                           "land", "bpl", "media", "electricity")], scale = TRUE)
