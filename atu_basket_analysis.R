---
title: "ATU BASKET ANALYSIS"
author: "Nale ALPASLAN VAROL"
date: "March 11, 2019"
---

# LIBRARY LOAD ------------------------------------------------------------

pacman::p_load(
  "readxl",
  "tidyverse",
  "arules",
  "arulesViz",
  "formattable",
  "ggplot2",
  "ggthemes",
  "RColorBrewer"
)

pacman::p_isloaded(
  "readxl",
  "tidyverse",
  "arules",
  "arulesViz",
  "formattable",
  "ggplot2",
  "ggthemes",
  "RColorBrewer"
)

# DATA LOAD & MERGE -------------------------------------------------------

df_1 <-
  bind_rows(
    "C:/Users/mali/Documents/R/atu_basket/HamData_R66-01-07.0718.xlsx" %>%
      excel_sheets() %>%
      set_names() %>%
      map(read_excel,
          path = "C:/Users/mali/Documents/R/atu_basket/HamData_R66-01-07.0718.xlsx")
  )

df_2 <-
  bind_rows(
    "C:/Users/mali/Documents/R/atu_basket/HamData_R66-08-14.07.18.xlsx" %>%
      excel_sheets() %>%
      set_names() %>%
      map(read_excel,
          path = "C:/Users/mali/Documents/R/atu_basket/HamData_R66-08-14.07.18.xlsx")
  )

df <- bind_rows(df_1, df_2)

colnames(df) <-
  c(
    "magaza_tipi",
    "tarih",
    "fis_saati",
    "tekil_fis_kodu",
    "magaza",
    "unifree_kategori_2",
    "grup",
    "alt_grup",
    "malzeme",
    "ghcode",
    "unicode",
    "marka",
    "musteri",
    "doğum_tarihi",
    "kimlik_no",
    "X__2",
    "to_airport",
    "yolcu_cinsiyeti",
    "pasaport_no",
    "milliyet",
    "pass_milliyet",
    "pasaport_no_dep",
    "taksit_sayısı",
    "kampanya_kart",
    "hasılat",
    "adet"
  )

rm(df_1, df_2)
df <- df[, -16] # sütun silme (X__2)
df <- df[, -16] # sütun silme (to airport)
View(df)

#Data iki excel çalışma kitabı ve 15 sayfası şeklindeydi. Ayrı ayrı yüklendi ve birleştirildi.
#İlk yüklemede 26 olan feature sayısı to airport ve X__2 adlı sütunların silinmesi ile 24'e düştü.

# NUMBER OF EXCLUDED OBSERVATIONS -------------------------------------------------------------
nrow(df[which(df$magaza_tipi == "Sonuç"), ]) #Mağaza Tipi "Sonuç" 15
nrow(df[which(df$tarih == "1.01.2018") , ]) #tarih 01.01.2018 olanlar 36707

# CLEAN THE REDUNDANT OBSERVATIONS  -----------------------------------------------------------
df <- df[-which(df$magaza_tipi == "Sonuç"),]
df <- df[-which(df$tarih == "1.01.2018"),]

#Datasetinde bulunan excel sayfalarından bir tanesi 1.01.2018 tarihli verileri içerdiğinden 36707
#satır datasetinden çıkarıldı.
#Toplam 15 sayfa olan excel sayfalarının altında yer alan Sonuç satırları datasetinden çıkarıldı.

# MISSING VALUE CONTROL  ----------------------------------------------------------------------
colSums(is.na(df))
#Data temizliğinden sonra datasetinde missing value kalmamıştır.

# DATA CLEANING --------------------------------------------------------
unique(df$tarih) #1-14 TEMMUZ 2018
unique(df$magaza) #23 MAGAZA
unique(df$unifree_kategori_2) #10 KATEGORI
unique(df$grup) #41 GRUP
unique(df$alt_grup) #126 ALT GRUP
unique(df$marka) #763 MARKA
unique(df$yolcu_cinsiyeti)

# FIX THE LEVELS OF "yolcu_cinsiyeti"
df$yolcu_cinsiyeti <- gsub("E$", "M", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("K$", "F", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("r$", "Diger", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("#$", "Diger", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("2$", "Diger", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("N$", "Diger", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("9$", "Diger", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("k$", "F", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("3$", "Diger", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("1$", "Diger", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("7$", "Diger", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("D$", "Diger", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("0$", "Diger", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("4$", "Diger", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("5$", "Diger", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("8$", "Diger", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("6$", "Diger", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("R$", "Diger", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("A$", "Diger", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("G$", "Diger", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("Y$", "Diger", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("T$", "Diger", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("B$", "Diger", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("P$", "Diger", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("Z$", "Diger", df$yolcu_cinsiyeti)
df$yolcu_cinsiyeti <- gsub("O$", "Diger", df$yolcu_cinsiyeti)

#Datasetinde bulunan featurelar faktör olarak kabul edildi ve herbirinin seviyeleri incelendi. Yolcu
#cinsiyeti haricindeki featurelarda değişiklik yapılmadı.
#Yolcu cinsiyetinde çok fazla seviye gözlendi. Bu seviyelerden "M" ve "F" kabul edildi. Türkçe
#ingilizce farkından kaynaklı "E" seviyesi de "M" olarak işaretlendi. Aynı şekilde "k","K" seviyeleri
#de "F" olarak işaretlendi. Geri kalan seviyeler "Diger" olarak işaretlendi.

unique(df$pass_milliyet)
unique(df$milliyet)

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        milliyet == "Türkiye",
      "TURKEY",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        milliyet == "Almanya",
      "GERMANY",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        milliyet == "Fransa",
      "FRANCE",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        milliyet == "Ukrayna",
      "UKRAINE",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        str_detect(milliyet, ".spanya") == TRUE,
      "SPAIN",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        str_detect(milliyet, ".ran") == TRUE,
      "ISLAMIC REPUBLIC OF IRAN",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        milliyet == "Kanada",
      "CANADA",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        milliyet == "Japonya",
      "JAPAN",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        milliyet == "Danimarka",
      "DENMARK",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        str_detect(milliyet, ".talya") == TRUE,
      "ITALY",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        milliyet == "Avusturya",
      "AUSTRIA",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        str_detect(milliyet, "Amerika.Birle.ik.Dev") == TRUE,
      "UNITED STATES OF AMERICA",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        milliyet == "Rusya",
      "RUSSIA",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        milliyet == "Çin",
      "CHINA",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        milliyet == "Azerbaycan",
      "AZERBAIJAN",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        milliyet == "Gürcistan",
      "GEORGIA",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        str_detect(milliyet, ".ngiltere") == TRUE,
      "UNITED KINGDOM",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        str_detect(milliyet, ".sviçre") == TRUE,
      "SWITZERLAND",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        milliyet == "Hollanda",
      "NETHERLANDS",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        str_detect(milliyet, ".srail") == TRUE,
      "ISRAEL",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        milliyet == "Portekiz",
      "PORTUGUAL",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        milliyet == "Belçika",
      "BELGIUM",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        str_detect(milliyet, ".sveç") == TRUE,
      "SWEDEN",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        milliyet == "Ermenistan",
      "ARMENIA",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        milliyet == "Avustralya",
      "AUSTRALIA",
      pass_milliyet
    )
  )

df <- df %>%
  mutate(
    pass_milliyet = ifelse(
      pass_milliyet == "Tayin edilmedi" &
        milliyet == "Yunanistan",
      "GREECE",
      pass_milliyet
    )
  )

MissingMilliyet <- df %>%
  group_by(pass_milliyet, milliyet) %>%
  summarise(ADET = n(), sayı = sum(ADET)) %>%
  filter(pass_milliyet == "Tayin edilmedi")
View(MissingMilliyet)

# SOLD ALONE OBSERVATIONS -------------------------------------------------

fis <- df %>%
  group_by(tekil_fis_kodu) %>%
  summarise(row = n(), adet = sum(adet))

discard <- fis[which(fis$row == 1), ]
discard <- discard[which(discard$adet == 1), ]

df_poly <-
  df[-which(df$tekil_fis_kodu %in% discard$tekil_fis_kodu),]

df_single <-
  df[which(df$tekil_fis_kodu %in% discard$tekil_fis_kodu),]

#Dataseti tekil fiş numarasına göre gruplandı. Her bir tekil fiş numarasına ait adet hesaplandı.
#Dataseti tekil fiş numarası ve ona ait satır sayısının her ikisi birlikte 1 olan satırlar datase-
#tinden ayrıldı. Bu satırlar analizin birliktelik kuralları kısmına dahil edilmeyecek.


# SPLIT THE DATASET -------------------------------------------------------

df_poly_arr <- df_poly[which(df_poly$magaza_tipi == "Arrival"),]

df_poly_dep <- df_poly[which(df_poly$magaza_tipi == "Departure"),]

df_poly_arr_tur <-
  df_poly_arr[which(df_poly_arr$pass_milliyet == "TURKEY"),]

df_poly_dep_china <-
  df_poly_dep[which(df_poly_dep$pass_milliyet == "CHINA"),]

# FREQUENCY TABLES --------------------------------------------------------

options(digits = 4)
s_magaza_tipi <- df_poly %>%
  group_by(magaza_tipi) %>%
  summarise(
    HASILAT = sum(hasilat),
    ADET = sum(adet),
    ROW = n(),
    AVG = (HASILAT / ADET)
  )
s_magaza_tipi <-
  s_magaza_tipi[order(s_magaza_tipi$HASILAT, decreasing = TRUE),]
sf_magaza_tipi <-
  formattable(
    head(s_magaza_tipi, 10),
    list(
      HASILAT = color_bar("lightblue"),
      ADET = color_bar("lightblue"),
      ROW = color_bar("lightblue"),
      AVG = color_bar("lightblue")
    )
  )
sf_magaza_tipi

s_pass_milliyet_dep <- df_poly_dep %>%
  group_by(pass_milliyet) %>%
  summarise(
    HASILAT = sum(hasilat),
    ADET = sum(adet),
    ROW = n(),
    AVG = (HASILAT / ADET)
  )
s_pass_milliyet_dep <-
  s_pass_milliyet_dep[order(s_pass_milliyet_dep$HASILAT, decreasing = TRUE), ]
sf_pass_milliyet_dep <-
  formattable(
    head(s_pass_milliyet_dep, 99),
    list(
      HASILAT = color_bar("lightblue"),
      ADET = color_bar("lightblue"),
      ROW = color_bar("lightblue"),
      AVG = color_bar("lightblue")
    )
  )
sf_pass_milliyet_dep

s_pass_milliyet_arr <- df_poly_arr %>%
  group_by(pass_milliyet) %>%
  summarise(
    HASILAT = sum(hasilat),
    ADET = sum(adet),
    ROW = n(),
    AVG = (HASILAT / ADET)
  )
s_pass_milliyet_arr <-
  s_pass_milliyet_arr[order(s_pass_milliyet_arr$HASILAT, decreasing = TRUE), ]
sf_pass_milliyet_arr <-
  formattable(
    head(s_pass_milliyet_arr, 99),
    list(
      HASILAT = color_bar("lightblue"),
      ADET =
        color_bar("lightblue"),
      ROW =
        color_bar("lightblue"),
      AVG =
        color_bar("lightblue")
    )
  )
sf_pass_milliyet_arr


# ARRIVAL - UNIFREE KATEGORİ 2 --------------------------------------------

# CREATE TRANSACTIONS
tr_arr_0718 <-
  paste(df_poly_arr$tekil_fis_kodu,
        df_poly_arr$unifree_kategori_2,
        sep = "\n")
write(tr_arr_0718, file = "tr_arr_0718")
tr_arr <-
  read.transactions("tr_arr_0718", format = "single", cols = c(1, 2))

# ECLAT
itemset_arr_uk2 <-
  eclat (tr_arr, parameter = list(supp = 0.001, minlen = 2))
inspect(head(sort(itemset_arr_uk2, by = "support"), 25))

# TOP 10 FREQUENT ITEMS
itemFrequencyPlot(
  tr_arr,
  topN = 25,
  col = brewer.pal(3, "Blues"),
  main = 'Top 10 Products',
  type = "relative",
  ylab = "Item Frequency(Relative)",
  cex  = 1
)

# APRIORI ALGORYTHM
rul_arr_uk2 <-
  apriori (tr_arr, parameter = list(
    supp = 0.001,
    conf = 0.05,
    minlen = 2
  ))
rul_arr_uk2_sorted <-
  sort (rul_arr_uk2, by = "support", decreasing = TRUE)

# CLEAR REDUNDANT RULES AND VISUALISING
rul_arr_uk2_red <-
  rul_arr_uk2_sorted[!is.redundant(rul_arr_uk2_sorted)]
rul_arr_uk2_sign <-
  rul_arr_uk2_red[is.significant(
    rul_arr_uk2_red,
    tr_arr,
    method = "fisher",
    alpha = .01,
    adjust = "bonferroni"
  )]
summary(rul_arr_uk2_sign)
rul_arr_uk2_sign <-
  sort(rul_arr_uk2_sign, by = "support", decreasing = TRUE)
inspect(head(rul_arr_uk2_sign, 20))

plot(
  rul_arr_uk2_sign,
  method = "graph",
  control = list(verbose = TRUE),
  engine = "html"
)



# ARRIVAL (TURKS) - UNIFREE KATEGORİ 2 ------------------------------------

# CREATE TRANSACTIONS
tr_arr_turk_0718 <-
  paste(df_poly_arr_tur$tekil_fis_kodu,
        df_poly_arr_tur$unifree_kategori_2,
        sep = "\n")
write(tr_arr_turk_0718, file = "tr_arr_turk_0718")
tr_arr_turk <-
  read.transactions("tr_arr_turk_0718", format = "single", cols = c(1, 2))

# ECLAT
itemset_arr_turk_uk2 <-
  eclat (tr_arr_turk, parameter = list(supp = 0.001, minlen = 2))
inspect(head(sort(itemset_arr_turk_uk2, by = "support"), 25))

# TOP 10 FREQUENT ITEMS
itemFrequencyPlot(
  tr_arr_turk,
  topN = 25,
  col = brewer.pal(3, "Blues"),
  main = 'Top 10 Products',
  type = "relative",
  ylab = "Item Frequency(Relative)",
  cex  = 1
)

# APRIORI ALGORYTHM
rul_arr_turk_uk2 <-
  apriori (tr_arr_turk, parameter = list(
    supp = 0.001,
    conf = 0.05,
    minlen = 2
  ))
rul_arr_turk_uk2_sorted <-
  sort (rul_arr_turk_uk2, by = "support", decreasing = TRUE)

# CLEAR REDUNDANT RULES AND VISUALISING
rul_arr_turk_uk2_redundant <-
  rul_arr_turk_uk2_sorted[!is.redundant(rul_arr_turk_uk2_sorted)]
rul_arr_turk_uk2_sign <-
  rul_arr_turk_uk2_redundant[is.significant(
    rul_arr_turk_uk2_redundant,
    tr_arr_turk,
    method = "fisher",
    alpha = .01,
    adjust = "bonferroni"
  )]
summary(rul_arr_turk_uk2_sign)
rul_arr_turk_uk2_sign <-
  sort(rul_arr_turk_uk2_sign, by = "confidence", decreasing = TRUE)
inspect(head(rul_arr_turk_uk2_sign, 20))

plot(
  rul_arr_turk_uk2_sign,
  method = "graph",
  control = list(verbose = TRUE),
  engine = "html"
)



# DEPARTURE - UNIFREE KATEGORİ 2 ------------------------------------------

# CREATE TRANSACTIONS
tr_dep_0718 <-
  paste(df_poly_dep$tekil_fis_kodu,
        df_poly_dep$unifree_kategori_2,
        sep = "\n")
write(tr_dep_0718, file = "tr_dep_0718")
tr_dep <-
  read.transactions("tr_dep_0718", format = "single", cols = c(1, 2))

# ECLAT
itemset_dep_uk2 <-
  eclat (tr_dep, parameter = list(supp = 0.001, minlen = 2))
inspect(head(sort(itemset_dep_uk2, by = "support"), 25))

# TOP 10 FREQUENT ITEMS
itemFrequencyPlot(
  tr_dep,
  topN = 25,
  col = brewer.pal(3, "Blues"),
  main = 'Top 10 Products',
  type = "relative",
  ylab = "Item Frequency(Relative)",
  cex  = 1
)

# APRIORI ALGORYTHM
rul_dep_uk2 <-
  apriori (tr_dep, parameter = list(
    supp = 0.001,
    conf = 0.05,
    minlen = 2
  ))
rul_dep_uk2_sorted <-
  sort (rul_dep_uk2, by = "support", decreasing = TRUE)

# CLEAR REDUNDANT RULES AND VISUALISING
rul_dep_uk2_redundant <-
  rul_dep_uk2_sorted[!is.redundant(rul_dep_uk2_sorted)]
rul_dep_uk2_sign <-
  rul_dep_uk2_redundant[is.significant(
    rul_dep_uk2_redundant,
    tr_dep,
    method = "fisher",
    alpha = .01,
    adjust = "bonferroni"
  )]
summary(rul_dep_uk2_sign)
rul_dep_uk2_sign <-
  sort(rul_dep_uk2_sign, by = "confidence", decreasing = TRUE)
inspect(head(rul_dep_uk2_sign, 20))

plot(
  rul_dep_uk2_sign,
  method = "graph",
  control = list(verbose = TRUE),
  engine = "html"
)


# DEPARTURE (CHINA)- UNIFREE KATEGORİ 2 -------------------------------------------------------

# CREATE TRANSACTIONS
tr_dep_china_0718 <-
  paste(df_poly_dep_china$tekil_fis_kodu,
        df_poly_dep_china$unifree_kategori_2,
        sep = "\n")
write(tr_dep_china_0718, file = "tr_dep_china_0718")
tr_dep_china <-
  read.transactions("tr_dep_china_0718", format = "single", cols = c(1, 2))

# ECLAT
itemset_dep_china_uk2 <-
  eclat (tr_dep_china, parameter = list(supp = 0.001, minlen = 2))
inspect(head(sort(itemset_dep_china_uk2, by = "support"), 25))

# TOP 10 FREQUENT ITEMS
itemFrequencyPlot(
  tr_dep_china,
  topN = 25,
  col = brewer.pal(3, "Blues"),
  main = 'Top 10 Products',
  type = "relative",
  ylab = "Item Frequency(Relative)",
  cex  = 1
)

# APRIORI ALGORYTHM
rul_dep_china_uk2 <-
  apriori (tr_dep_china, parameter = list(
    supp = 0.001,
    conf = 0.05,
    minlen = 2
  ))
rul_dep__china_uk2_sorted <-
  sort (rul_dep_china_uk2, by = "support", decreasing = TRUE)

# CLEAR REDUNDANT RULES AND VISUALISING
rul_dep_china_uk2_red <-
  rul_dep__china_uk2_sorted[!is.redundant(rul_dep__china_uk2_sorted)]
rul_dep_china_uk2_sign <-
  rul_dep_china_uk2_red[is.significant(
    rul_dep_china_uk2_red,
    tr_dep_china,
    method = "fisher",
    alpha = .01,
    adjust = "bonferroni"
  )]
summary(rul_dep_china_uk2_sign)
rul_dep_china_uk2_sign <-
  sort(rul_dep_china_uk2_sign, by = "confidence", decreasing = TRUE)
inspect(head(rul_dep_china_uk2_sign, 20))

plot(
  rul_dep_china_uk2_sign,
  method = "graph",
  control = list(verbose = TRUE),
  engine = "html"
)


# SOLD ALONE --------------------------------------------------------------

# CREATE TRANSACTIONS
tr_SoldAlone_0718 <-
  paste(df_single$tekil_fis_kodu,
        df_single$unifree_kategori_2,
        sep = "\n")
write(tr_SoldAlone_0718, file = "tr_SoldAlone_0718")
tr_SoldAlone <-
  read.transactions("tr_SoldAlone_0718", format = "single", cols = c(1, 2))

# ECLAT
itemset_SoldAlone_uk2 <-
  eclat (tr_SoldAlone, parameter = list(supp = 0.0000001, minlen = 2))
inspect(head(sort(itemset_SoldAlone_uk2, by = "support"), 25))

# TOP 10 FREQUENT ITEMS
itemFrequencyPlot(
  tr_SoldAlone,
  topN = 25,
  col = brewer.pal(3, "Blues"),
  main = 'Top 10 Products',
  type = "relative",
  ylab = "Item Frequency(Relative)",
  cex  = 1
)

# APRIORI ALGORYTHM
rul_SoldAlone_uk2 <-
  apriori (tr_SoldAlone, parameter = list(
    supp = 0.000000001,
    conf = 0.000000001,
    minlen  =1
  ))
rul_SoldAlone_uk2_sorted <-
  sort (rul_SoldAlone_uk2, by = "support", decreasing = TRUE)

summary(rul_SoldAlone_uk2_sorted)

inspect(head(rul_SoldAlone_uk2_sorted, 20))

plot(
  rul_SoldAlone_uk2_sorted,
  method = "graph",
  control = list(verbose = TRUE),
  engine = "html"
)

 

# ARRIVAL - MALZEME --------------------------------------------

# CREATE TRANSACTIONS
tr_arr_mlz_0718 <-
  paste(df_poly_arr$tekil_fis_kodu,
        df_poly_arr$malzeme,
        sep = "\n")
write(tr_arr_mlz_0718, file = "tr_arr_mlz_0718")
tr_arr_mlz <-
  read.transactions("tr_arr_mlz_0718", format = "single", cols = c(1, 2))

# ECLAT
itemset_arr_mlz <-
  eclat (tr_arr_mlz, parameter = list(supp = 0.001, minlen = 2))
inspect(head(sort(itemset_arr_mlz, by = "support"), 25))

# TOP 10 FREQUENT ITEMS
itemFrequencyPlot(
  tr_arr_mlz,
  topN = 25,
  col = brewer.pal(3, "Blues"),
  main = 'Top 10 Products',
  type = "relative",
  ylab = "Item Frequency(Relative)",
  cex  = 1
)

# APRIORI ALGORYTHM
rul_arr_mlz <-
  apriori (tr_arr_mlz, parameter = list(
    supp = 0.001,
    conf = 0.05,
    minlen = 2
  ))
rul_arr_mlz_sorted <-
  sort (rul_arr_mlz, by = "support", decreasing = TRUE)

# CLEAR REDUNDANT RULES AND VISUALISING
rul_arr_mlz_red <-
  rul_arr_mlz_sorted[!is.redundant(rul_arr_mlz_sorted)]
rul_arr_mlz_sign <-
  rul_arr_mlz_red[is.significant(
    rul_arr_mlz_red,
    tr_arr_mlz,
    method = "fisher",
    alpha = .01,
    adjust = "bonferroni"
  )]
summary(rul_arr_mlz_sign)
rul_arr_mlz_sign <-
  sort(rul_arr_mlz_sign, by = "support", decreasing = TRUE)
inspect(head(rul_arr_mlz_sign, 20))

plot(
  rul_arr_mlz_sign,
  method = "graph",
  control = list(verbose = TRUE),
  engine = "html"
)
# ARRIVAL (TURKS) - MALZEME ------------------------------------

# CREATE TRANSACTIONS
tr_arr_turk_mlz_0718 <-
  paste(df_poly_arr_tur$tekil_fis_kodu,
        df_poly_arr_tur$malzeme,
        sep = "\n")
write(tr_arr_turk_mlz_0718, file = "tr_arr_turk_mlz_0718")
tr_arr_turk_mlz <-
  read.transactions("tr_arr_turk_mlz_0718", format = "single", cols = c(1, 2))

# ECLAT
itemset_arr_turk_mlz <-
  eclat (tr_arr_turk_mlz, parameter = list(supp = 0.001, minlen = 2))
inspect(head(sort(itemset_arr_turk_mlz, by = "support"), 25))

# TOP 10 FREQUENT ITEMS
itemFrequencyPlot(
  tr_arr_turk_mlz,
  topN = 25,
  col = brewer.pal(3, "Blues"),
  main = 'Top 10 Products',
  type = "relative",
  ylab = "Item Frequency(Relative)",
  cex  = 1
)

# APRIORI ALGORYTHM
rul_arr_turk_mlz <-
  apriori (tr_arr_turk_mlz, parameter = list(
    supp = 0.001,
    conf = 0.05,
    minlen = 2
  ))
rul_arr_turk_mlz_sorted <-
  sort (rul_arr_turk_mlz, by = "support", decreasing = TRUE)

# CLEAR REDUNDANT RULES AND VISUALISING
rul_arr_turk_mlz_redundant <-
  rul_arr_turk_mlz_sorted[!is.redundant(rul_arr_turk_mlz_sorted)]
rul_arr_turk_mlz_sign <-
  rul_arr_turk_mlz_redundant[is.significant(
    rul_arr_turk_mlz_redundant,
    tr_arr_turk_mlz,
    method = "fisher",
    alpha = .01,
    adjust = "bonferroni"
  )]
summary(rul_arr_turk_mlz_sign)
rul_arr_turk_mlz_sign <-
  sort(rul_arr_turk_mlz_sign, by = "confidence", decreasing = TRUE)
inspect(head(rul_arr_turk_mlz_sign, 20))

plot(
  rul_arr_turk_mlz_sign,
  method = "graph",
  control = list(verbose = TRUE),
  engine = "html"
)



# DEPARTURE - MALZEME ------------------------------------------

# CREATE TRANSACTIONS
tr_dep_mlz_0718 <-
  paste(df_poly_dep$tekil_fis_kodu,
        df_poly_dep$malzeme,
        sep = "\n")
write(tr_dep_mlz_0718, file = "tr_dep_mlz_0718")
tr_dep_mlz <-
  read.transactions("tr_dep_mlz_0718", format = "single", cols = c(1, 2))

# ECLAT
itemset_dep_mlz <-
  eclat (tr_dep_mlz, parameter = list(supp = 0.001, minlen = 2))
inspect(head(sort(itemset_dep_mlz, by = "support"), 25))

# TOP 10 FREQUENT ITEMS
itemFrequencyPlot(
  tr_dep_mlz,
  topN = 25,
  col = brewer.pal(3, "Blues"),
  main = 'Top 10 Products',
  type = "relative",
  ylab = "Item Frequency(Relative)",
  cex  = 1
)

# APRIORI ALGORYTHM
rul_dep_mlz <-
  apriori (tr_dep_mlz, parameter = list(
    supp = 0.001,
    conf = 0.05,
    minlen = 2
  ))
rul_dep_mlz_sorted <-
  sort (rul_dep_mlz, by = "support", decreasing = TRUE)

# CLEAR REDUNDANT RULES AND VISUALISING
rul_dep_mlz_redundant <-
  rul_dep_mlz_sorted[!is.redundant(rul_dep_mlz_sorted)]
rul_dep_mlz_sign <-
  rul_dep_mlz_redundant[is.significant(
    rul_dep_mlz_redundant,
    tr_dep_mlz,
    method = "fisher",
    alpha = .01,
    adjust = "bonferroni"
  )]
summary(rul_dep_mlz_sign)
rul_dep_mlz_sign <-
  sort(rul_dep_mlz_sign, by = "confidence", decreasing = TRUE)
inspect(head(rul_dep_mlz_sign, 20))

plot(
  rul_dep_mlz_sign,
  method = "graph",
  control = list(verbose = TRUE),
  engine = "html"
)


# DEPARTURE (CHINA)- MALZEME -------------------------------------------------------

# CREATE TRANSACTIONS
tr_dep_china_mlz_0718 <-
  paste(df_poly_dep_china$tekil_fis_kodu,
        df_poly_dep_china$malzeme,
        sep = "\n")
write(tr_dep_china_mlz_0718, file = "tr_dep_china_mlz_0718")
tr_dep_china_mlz <-
  read.transactions("tr_dep_china_mlz_0718", format = "single", cols = c(1, 2))

# ECLAT
itemset_dep_china_mlz <-
  eclat (tr_dep_china_mlz, parameter = list(supp = 0.001, minlen = 2))
inspect(head(sort(itemset_dep_china_mlz, by = "support"), 25))

# TOP 10 FREQUENT ITEMS
itemFrequencyPlot(
  tr_dep_china_mlz,
  topN = 25,
  col = brewer.pal(3, "Blues"),
  main = 'Top 10 Products',
  type = "relative",
  ylab = "Item Frequency(Relative)",
  cex  = 1
)

# APRIORI ALGORYTHM
rul_dep_china_mlz <-
  apriori (tr_dep_china_mlz, parameter = list(
    supp = 0.001,
    conf = 0.05,
    minlen = 2
  ))
rul_dep__china_mlz_sorted <-
  sort (rul_dep_china_mlz, by = "support", decreasing = TRUE)

# CLEAR REDUNDANT RULES AND VISUALISING
rul_dep_china_mlz_red <-
  rul_dep__china_mlz_sorted[!is.redundant(rul_dep__china_mlz_sorted)]
rul_dep_china_mlz_sign <-
  rul_dep_china_mlz_red[is.significant(
    rul_dep_china_mlz_red,
    tr_dep_china_mlz,
    method = "fisher",
    alpha = .01,
    adjust = "bonferroni"
  )]
summary(rul_dep_china_mlz_sign)
rul_dep_china_mlz_sign <-
  sort(rul_dep_china_mlz_sign, by = "confidence", decreasing = TRUE)
inspect(head(rul_dep_china_mlz_sign, 20))

plot(
  rul_dep_china_mlz_sign,
  method = "graph",
  control = list(verbose = TRUE),
  engine = "html"
)




# AKSESUAR - TEKSTİL ------------------------------------------------------

rows <-
  df_poly[which(df_poly$unifree_kategori_2 == "Aksesuar-Tekstil"), ]
index <- unique(rows$tekil_fis_kodu)
df_aksesuar <- df_poly[which(df_poly$tekil_fis_kodu %in% index),]

# AKSESUAR - TEKSTİL DEPARTURE ---------------------------------------------------------------

df_aksesuar_dep <- df_aksesuar[which(df_aksesuar$magaza_tipi == "Departure"),]

# CREATE TRANSACTIONS
tr_dep_aks_0718 <-
  paste(df_aksesuar_dep$tekil_fis_kodu,
        df_aksesuar_dep$malzeme,
        sep = "\n")
write(tr_dep_aks_0718, file = "tr_dep_aks_0718")
tr_dep_aks <-
  read.transactions("tr_dep_aks_0718", format = "single", cols = c(1, 2))

# ECLAT
itemset_dep_aks <-
  eclat (tr_dep_aks, parameter = list(supp = 0.001, minlen = 2))
inspect(head(sort(itemset_dep_aks, by = "support"), 25))

# TOP 10 FREQUENT ITEMS
itemFrequencyPlot(
  tr_dep_aks,
  topN = 25,
  col = brewer.pal(3, "Blues"),
  main = 'Top 10 Products',
  type = "relative",
  ylab = "Item Frequency(Relative)",
  cex  = 1
)

# APRIORI ALGORYTHM
rul_dep_aks <-
  apriori (tr_dep_aks, parameter = list(
    supp = 0.001,
    conf = 0.05,
    minlen = 2
  ))
rul_dep_aks_sorted <-
  sort (rul_dep_aks, by = "support", decreasing = TRUE)

# CLEAR REDUNDANT RULES AND VISUALISING
rul_dep_aks_redundant <-
  rul_dep_aks_sorted[!is.redundant(rul_dep_aks_sorted)]
rul_dep_aks_sign <-
  rul_dep_aks_redundant[is.significant(
    rul_dep_aks_redundant,
    tr_dep_aks,
    method = "fisher",
    alpha = .01,
    adjust = "bonferroni"
  )]
summary(rul_dep_aks_sign)
rul_dep_aks_sign <-
  sort(rul_dep_aks_sign, by = "confidence", decreasing = TRUE)
inspect(head(rul_dep_aks_sign, 20))

plot(
  rul_dep_aks_sign,
  method = "graph",
  control = list(verbose = TRUE),
  engine = "html",
  max = 200
  )

# AKSESUAR - TEKSTİL DEPARTURE (CHINA) -------------------------------------------------------

df_aksesuar_dep_china <-
  df_aksesuar_dep[which(df_aksesuar_dep$pass_milliyet == "CHINA"),]

# CREATE TRANSACTIONS
tr_dep_china_aks_0718 <-
  paste(df_aksesuar_dep_china$tekil_fis_kodu,
        df_aksesuar_dep_china$malzeme,
        sep = "\n")
write(tr_dep_china_aks_0718, file = "tr_dep_china_aks_0718")
tr_dep_china_aks <-
  read.transactions("tr_dep_china_aks_0718", format = "single", cols = c(1, 2))

# ECLAT
itemset_dep_china_aks <-
  eclat (tr_dep_china_aks, parameter = list(supp = 0.001, minlen = 2))
inspect(head(sort(itemset_dep_china_aks, by = "support"), 25))

# TOP 10 FREQUENT ITEMS
itemFrequencyPlot(
  tr_dep_china_aks,
  topN = 25,
  col = brewer.pal(3, "Blues"),
  main = 'Top 10 Products',
  type = "relative",
  ylab = "Item Frequency(Relative)",
  cex  = 1
)

# APRIORI ALGORYTHM
rul_dep_china_aks <-
  apriori (tr_dep_china_aks, parameter = list(
    supp = 0.001,
    conf = 0.05,
    minlen = 2
  ))
rul_dep__china_aks_sorted <-
  sort (rul_dep_china_aks, by = "support", decreasing = TRUE)

# CLEAR REDUNDANT RULES AND VISUALISING
rul_dep_china_aks_red <-
  rul_dep__china_aks_sorted[!is.redundant(rul_dep__china_aks_sorted)]
rul_dep_china_aks_sign <-
  rul_dep_china_aks_red[is.significant(
    rul_dep_china_aks_red,
    tr_dep_china_aks,
    method = "fisher",
    alpha = .01,
    adjust = "bonferroni"
  )]
summary(rul_dep_china_aks_sign)
rul_dep_china_aks_sign <-
  sort(rul_dep_china_aks_sign, by = "confidence", decreasing = TRUE)
inspect(head(rul_dep_china_aks_sign, 20))

plot(
  rul_dep_china_aks_sign,
  method = "graph",
  control = list(verbose = TRUE),
  engine = "html",
  max = 200
)

# BAZAAR ------------------------------------------------------------------

df_poly_dep_bazaar <-
  df_poly_dep[which(df_poly_dep$magaza == "Ist Bazaar"),]

