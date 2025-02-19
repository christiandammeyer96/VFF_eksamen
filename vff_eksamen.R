pacman::p_load(tidyverse, tidymodels, caret, rvest, rlist, 
               rjson, Rcrawler, hrbrthemes, knitr, glmnet, hms, leaps, readxl, 
               randomForest, gbm, httr, jsonlite, dplyr, rjstat, lubridate,
               DBI, RSQLite, dbplyr, ggplot2, readxl, httr, jsonlite, tidyverse, rlist, rjstat, rjson, Rcrawler, 
               repurrrsive, tidymodels, caret, MASS, ISLR2, glmnet, boot, leaps,
               viridis, pls)

# ------------------- Indlæsning af guld og første inspektion ------------------

# Indlæs rå data fra Excel-filen
rå_guld_data <- read_xlsx("data/Guld.xlsx", col_names = FALSE)


# Inspektér datasættet
head(rå_guld_data)
str(rå_guld_data)

# Indlæs data igen og håndter tomme felter som NA
guld_data <- read_xlsx("data/Guld.xlsx", na = "")

# Ændr kolonnenavne til småt
colnames(guld_data) <- tolower(colnames(guld_data))

# Inspektion af datastruktur
glimpse(guld_data)
summary(guld_data)

# ----------------------- Databehandling ---------------------------------------

# Håndter datoformat med fejlhåndtering
guld_data_cleaned <- guld_data |> 
  mutate(
    dato = case_when(
      # Hvis 'dato' er numerisk, konverter med Excel's origin
      !is.na(as.numeric(dato)) ~ as.Date(as.numeric(dato), origin = "1899-12-30"),
      
      # Hvis 'dato' er tekst, konverter med tekst-format
      TRUE ~ as.Date(dato, format = "%d.%m.%Y")
    ))

# Tjek unikke datoer
unique(guld_data_cleaned$dato)

# Filtrér data for kun at inkludere relevante rækker
guld_data_filtered <- guld_data_cleaned |> 
  filter(
    !is.na(guld_menu_stk), 
    !is.na(antal_bestilte),
    dato >= as.Date("2021-07-31")) |>
  dplyr::select(-gule_poletter_stk)  # Fjern unødvendig kolonne

# Konverter kategoriske variabler
guld_data_transformed <- guld_data_filtered |> 
  mutate(
    kamp = as.factor(kamp)
  )

# Tjek for stavefejl eller unøjagtigheder i kamp
table(guld_data_transformed$kamp)

# SDR er en fejl, og skal hedde SJF
guld_data_transformed <- guld_data_transformed |> 
  mutate(kamp = if_else(kamp == "SDR", "SJF", kamp))

# -------------------- Ekstra datatransformationer --------------------

# Separér dato i år, måned og dag for yderligere analyse
guld_data_final <- guld_data_transformed |> 
  # separate(dato, into = c("år", "måned", "dag"), sep = "-") |>
  mutate(
    # år = as.factor(år),
    # måned = as.factor(måned),
    # dag = as.factor(dag),
    dato = as_date(dato),
    kamp_type = case_when(
      kamp %in% c("BIF", "FCK", "AGF", "FCM") ~ "a_kamp",
      kamp %in% c("SIF", "FCN", "RFC") ~ "b_kamp",
      kamp %in% c("HIF", "ACH", "SJF", "OB", "VB", "AaB", "LBK") ~ "c_kamp")
  )

unique(guld_data_transformed$kamp)
# Tjek for NA-værdier i datasættet
colSums(is.na(guld_data_final))

# Inspektion af den rensede og transformerede data
str(guld_data_final)
head(guld_data_final)
#view(guld_data_final)



#----------------Webscraping af superstats, 2021 - 2024-------------------------

# Initialiser en liste til at gemme data for hvert år
superstats_data <- list()

for (y in 2021:2023) { 
  # Dynamisk URL til hvert år
  url <- paste0("https://superstats.dk/program?aar=", y, "%2F", y + 1)
  
  # Læs HTML-indholdet fra URL'en
  alle_tabeller <- read_html(url, encoding = "UTF-8") |> 
    html_nodes("div#club table") |> 
    html_table(header = FALSE, convert = FALSE)
  
  # Filtrer relevante tabeller (fjerner de sidste 5 irrelevante)
  #fjerner ikke længere de sidste fem tabeller skulle bruges i denne årrække
  udvalgte_tabeller <- alle_tabeller[1:(length(alle_tabeller))]
  
  # Gem i listen med et navn baseret på år
  superstats_data[[paste0(y, "/", y + 1)]] <- udvalgte_tabeller
}

# Kør for at se data fra Superstats
superstats_data

# Saml data til én samlet dataframe
superstats_dataframe <- bind_rows(superstats_data, .id = "runde")


# Tilpasning af variabler og fjernelse af tomme felter
superstats_dataframe <- superstats_dataframe |> 
  dplyr::select(-runde, -X6, -X7) |> 
  rename(
    "ugedag" = "X1", 
    "dato" = "X2",
    "kamp" = "X3",
    "resultat" = "X4",
    "tilskuere" = "X5"
  ) |>
  filter(!kamp %in% c(NA, " ", ""))

# Splitting af kamp og resultat i separate kolonner
superstats_dataframe <- superstats_dataframe |> 
  separate(kamp, into = c("hjemmehold", "udehold"), sep = "-") |> 
  separate(resultat, into = c("hjemmescore", "udescore"), sep = "-")

# Filtrer hjemmekampe for VFF
superstats_dataframe <- superstats_dataframe |> 
  filter(str_detect(hjemmehold, "VFF"))



# Splitting af dato og tidspunkt
superstats_dataframe <- superstats_dataframe |> 
  separate(dato, into = c("dato", "tidspunkt"), sep = " ") |>
  mutate(
    dato = as.Date(paste0(dato, "/", guld_data_final$dato), format = "%d/%m/%Y"),
    tidspunkt = as.factor(tidspunkt),
    hjemmehold = as.character(hjemmehold),
    udehold = trimws(as.character(udehold)),
    hjemmescore = as.integer(hjemmescore),
    udescore = as.integer(udescore),
    tilskuere = as.numeric(tilskuere),
    kamp_type = case_when(
      udehold %in% c("BIF", "FCK", "AGF", "FCM") ~ "a_kamp",
      udehold %in% c("SIF", "FCN", "RFC") ~ "b_kamp",
      udehold %in% c("HIF", "ACH", "SJF", "OB", "VB", "AaB", "LBK") ~ "c_kamp")
  )

# Inspektion af data
str(superstats_dataframe)
head(superstats_dataframe)
#view(superstats_dataframe)

# ------------------------ Superstats til RDS ----------------------------------

saveRDS(superstats_dataframe, file = "data/superstats_final.rds")

#Load RDS
superstats_data <- readRDS("data/superstats_final.rds")

#------------------ Prøver på Join af Guld og Supepstats------------------------

# #Prøver på join
# #Test join af Guld og Superstats
# Join_test <- guld_data_final |> 
#   left_join(superstats_data, join_by("dato" == "dato" & "kamp" == "udehold")) 
# 
# Join_test

#-----------------------------DST Ledighed:-------------------------------------

# base url til api'et hos danmarks statistik
base_url <- "https://api.statbank.dk/v1/data/"

# endepunkt for specifikt datasæt (pris112) og dataformat (jsonstat)
data_url_ledighed <- "auf02/jsonstat?"

# query-parametre for første datasæt
info_url_ledighed <- "ydelsestype=tot&alder=tot&tid=2021m01%2c2021m02%2c2021m03%2c2021m04%2c2021m05%2c2021m06%2c2021m07%2c2021m08%2c2021m09%2c2022m11%2c2021m11%2c2021m12%2c2022m01%2c2022m02%2c2022m03%2c2022m04%2c2022m05%2c2022m06%2c2022m07%2c2022m08%2c2022m09%2c2022m10%2c2022m12%2c2023m01%2c2023m02%2c2023m03%2c2023m04%2c2023m05%2c2023m06%2c2023m07%2c2023m08%2c2023m09%2c2023m10%2c2023m11%2c2023m12%2c2024m01%2c2024m02%2c2024m03%2c2024m04%2c2021m10%2c2022m11&område=791&køn=tot"

# samlet url
full_url_ledighed <- paste0(base_url, data_url_ledighed, info_url_ledighed)
full_url_ledighed

# kald api med get request
api_call_ledighed <- httr::GET(full_url_ledighed)
api_call_ledighed$status_code

# hent og parse json data
json_list_ledighed <- fromJSONstat(full_url_ledighed)
str(json_list_ledighed)

# opret tibble
data_tibble_ledighed <- as_tibble(json_list_ledighed)
data_tibble_ledighed

# rensning og valg af relevante kolonner
tidy_data_ledighed <- data_tibble_ledighed |>  
  pull(1) |> #Fjerner dollartegn i første række 
  mutate(
    år = substr(tid, 1, 4),       # tager de første 4 tegn (år)
    måned = substr(tid, 6, 7)    # # tager tegnene på position 6 og 7 (måned)
  ) |>   
  rename(ledighed = value) |>         # omdøber kolonnen "value" til "ledighed"
  dplyr::select(år, måned, ledighed)

# det strømlinede datasæt
head(tidy_data_ledighed)
# view(tidy_data_ledighed)

#----------------- dst FPI = forbrugerprisindeks: ------------------------

# base url til api'et hos danmarks statistik
base_url <- "https://api.statbank.dk/v1/data/"

# endepunkt for specifikt datasæt (pris113) og dataformat (jsonstat)
data_url_fpi <- "pris113/jsonstat?"

# query-parametre for datasæt
info_url_fpi <- "tid=2021m01%2c2021m02%2c2021m03%2c2021m04%2c2021m05%2c2021m06%2c2021m07%2c2021m08%2c2021m09%2c2022m11%2c2021m11%2c2021m12%2c2022m01%2c2022m02%2c2022m03%2c2022m04%2c2022m05%2c2022m06%2c2022m07%2c2022m08%2c2022m09%2c2022m10%2c2022m12%2c2023m01%2c2023m02%2c2023m03%2c2023m04%2c2023m05%2c2023m06%2c2023m07%2c2023m08%2c2023m09%2c2023m10%2c2023m11%2c2023m12%2c2024m01%2c2024m02%2c2024m03%2c2024m04%2c2024m05%2c2021m10%2c2022m11"

# samlet url
full_url_fpi<- paste0(base_url, data_url_fpi, info_url_fpi)
full_url_fpi

# kald api med get request
api_call_fpi <- httr::GET(full_url_fpi)
api_call_fpi$status_code

# hent og parse json data
json_list_fpi <- fromJSONstat(full_url_fpi)
str(json_list_fpi)

# opret tibble
data_tibble_fpi <- as_tibble(json_list_fpi)
data_tibble_fpi

# rensning af fpi
tidy_data_fpi <- data_tibble_fpi |>  
  pull(1) |> 
  mutate(
    år = substr(tid, 1, 4),         # udtrækker år fra tid
    måned = substr(tid, 6, 7),     # udtrækker måned fra tid
    fpi = (value - 100)
  ) |>  
  dplyr::select(måned, år, fpi)

# det strømlinede datasæt
head(tidy_data_fpi)
# view(tidy_data_fpi)

#---------------------- join dst: ledighed og fpi-------------------------

# join af ledighed og fpi
dst_final <- tidy_data_ledighed |>  # udfører en left join mellem 'tidy_data_ledighed' og 'tidy_data_fpi'
  left_join(tidy_data_fpi, join_by("måned", "år")) |>
  mutate(
    måned = as.factor(måned),
    år = as.factor(år),
    fpi = as.double(fpi)
  )


# ---------------------------- DST til RDS -------------------------------------

saveRDS(dst_final, file = "data/DST_final.rds")

#Load RDS
dst_data <- readRDS("data/DST_final.rds")


#-------------------------------------------------------------------------------
# Base URL og API-nøgle
dmi_base_url <- "https://dmigw.govcloud.dk/v2/"
dmi_info_url <- "metObs/collections/observation/items?"
dmi_api_key <- "&api-key=796dc1af-40cf-424b-a71b-22e562f9e8ee"

# Funktion til at hente og behandle data
hent_dmi_data <- function(station_id, parameter_id, start_date, end_date, limit) {
  query <- paste0(
    "stationId=", station_id,
    "&parameterId=", parameter_id,
    "&datetime=", start_date, "Z/", end_date, "Z",
    "&limit=", limit
  )
  
  full_url <- paste0(dmi_base_url, dmi_info_url, query, dmi_api_key)
  
  # API-kald
  response <- GET(full_url)
  
  # Tjek status
  if (status_code(response) != 200) {
    stop("Fejl ved API-kald: Status ", status_code(response))
  }
  
  # Parse response
  content_json <- fromJSON(rawToChar(response$content), flatten = TRUE)
  
  # Omdan til dataframe
  df <- as.data.frame(do.call(cbind, as.list(content_json)))
  
  # Udtræk relevante kolonner og omdøb
  df_selected <- df %>%
    dplyr::select(
      observationstidspunkt = features.properties.observed,
      værdi = features.properties.value
    ) %>%
    as_tibble()
  
  return(df_selected)
}

# Hent data for vindhastighed
wind_speed_21_22 <- hent_dmi_data("06060", "wind_speed_past1h", "2021-07-31T00:00:00", "2022-05-15T23:59:00", 6928)
wind_speed_22_23 <- hent_dmi_data("06060", "wind_speed_past1h", "2022-07-17T00:00:00", "2023-05-29T23:59:00", 7600)
wind_speed_23_24 <- hent_dmi_data("06060", "wind_speed_past1h", "2023-07-28T00:00:00", "2024-05-25T23:59:00", 43588)

# Hent data for nedbør
precip_21_22 <- hent_dmi_data("06060", "precip_dur_past1h", "2021-07-31T00:00:00", "2022-05-15T23:59:00", 6932)
precip_22_23 <- hent_dmi_data("06060", "precip_dur_past1h", "2022-07-17T00:00:00", "2023-05-29T23:59:00", 7600)
precip_23_24 <- hent_dmi_data("06060", "precip_dur_past1h", "2023-07-28T00:00:00", "2024-05-25T23:59:00", 7264)

# Hent data for temperatur
temperatur_21_22 <- hent_dmi_data("06060", "temp_mean_past1h", "2021-07-31T00:00:00", "2022-05-15T23:59:00", 6930)
temperatur_22_23 <- hent_dmi_data("06060", "temp_mean_past1h", "2022-07-17T00:00:00", "2023-05-29T23:59:00", 45630)
temperatur_23_24 <- hent_dmi_data("06060", "temp_mean_past1h", "2023-07-28T00:00:00", "2024-05-25T23:59:00", 7266)

# Samlet dataframe for alle data
dmi_all <- bind_rows(
  wind_speed_21_22 %>% mutate(parameter = "vindhastighed", season = "21/22"),
  wind_speed_22_23 %>% mutate(parameter = "vindhastighed", season = "22/23"),
  wind_speed_23_24 %>% mutate(parameter = "vindhastighed", season = "23/24"),
  precip_21_22 %>% mutate(parameter = "nedbør", season = "21/22"),
  precip_22_23 %>% mutate(parameter = "nedbør", season = "22/23"),
  precip_23_24 %>% mutate(parameter = "nedbør", season = "23/24"),
  temperatur_21_22 %>% mutate(parameter = "temperatur", season = "21/22"),
  temperatur_22_23 %>% mutate(parameter = "temperatur", season = "22/23"),
  temperatur_23_24 %>% mutate(parameter = "temperatur", season = "23/24")
)
# Databehandling
dmi_rens <- dmi_all |>
  mutate(
    observationstidspunkt = as_datetime(observationstidspunkt, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    dato = as.Date(observationstidspunkt),
    tidspunkt = format(observationstidspunkt, "%H:%M:%S")
  ) |> 
  filter(tidspunkt >= "09:00:00" & tidspunkt <= "21:00:00") |> 
  dplyr::select(-observationstidspunkt)

# Filtrering på kampdage
dmi_filter <- dmi_rens |> filter(dato %in% guld_data_final$dato)

# Gennemsnit af variabler for kun at have en observation pr. dag
dmi_gns <- dmi_filter |>   
  group_by(dato, parameter) |> 
  summarise(værdi = mean(værdi), .groups = "drop") |> 
  pivot_wider(names_from = parameter, values_from = værdi)

# Endelig tabel
dmi_final <- dmi_gns

# -------------------------- DMI til RDS ---------------------------------------


saveRDS(dmi_final, file = "data/dmi_final.rds")

#Load RDS
dmi_data <- readRDS("data/dmi_final.rds")

#------------------------- Join i R --------------------------------------------
join_r <- guld_data_final |>
  left_join(superstats_data, join_by("dato" == "dato")) |>
  left_join(dmi_data, join_by("dato" == "dato")) 

ss_guld_dmi <- join_r |>
  separate(dato, into = c("år", "måned", "dag"), sep = "-") |>
  mutate(
    år = as.factor(år),
    måned = as.factor(måned),
    dag = as.factor(dag))

join_all <- ss_guld_dmi |> 
  left_join(dst_data, join_by("måned" == "måned", "år" == "år"))

str(join_all)

join_final <- join_all |> 
  mutate(dato = as.Date(paste(år, måned, dag, sep ="-"), format ="%Y-%m-%d"),
         kamp_type = kamp_type.x,
         mål_diff_lag = hjemmescore - lag(udescore, default = 0),#sidste hjemmekamp i 16/17 (sidste gang i superliga) var kampen uafgjort
         tilskuere_lag = lag(tilskuere, default = 8.619), #sidste hjemmekamp i 16/17 (sidste gang i superliga) var der 8619 tilskuere
         nedbør_dummy = if_else(nedbør > 0, 1, 0),
         vindhastighed =round(vindhastighed,2),
         temperatur =round(temperatur,2),
         fpi =round(fpi,2),
         tidspunkt = case_when(
           tidspunkt %in% c("14:00", "15:00", "16:00") ~ "eftermiddag",
           tidspunkt %in% c("17:00", "18:00", "19:00", "20:00") ~ "aften"),
         ugedag = case_when(
           ugedag %in% c("Man", "Fre")~ "hverdag",
           ugedag %in% c("Lør", "Søn") ~ "weekend"))|>  
  dplyr::select(-år, -måned, -dag, -kamp_type.y, -kamp_type.x, -kamp, -antal_max, 
                -guld_menu_stk, -udehold, -hjemmehold, -hjemmescore, -udescore, 
                -tilskuere, -nedbør) |> 
  relocate(antal_bestilte)

str(join_final)

final_data <- join_final |> 
  mutate(
    ugedag = as.factor(ugedag),
    kamp_type = as.factor(kamp_type),
    dato = case_when(
      #Hvis 'dato' er numerisk, konverter med Excel's origin
      !is.na(as.numeric(dato)) ~ as.Date(as.numeric(dato), origin = "1969-12-31"),
      # Hvis 'dato' er tekst, konverter med tekst-format
      TRUE ~ as.Date(dato, format = "%d.%m.%Y"))
  ) |> 
  na.omit()

final_data

# -------------------- Modellering ------ --------------------------------------
# Definerer x variabler og y variabel
x <- model.matrix(antal_bestilte ~ ., final_data)[, -1]
y <- final_data$antal_bestilte

# Lave et grid til Lambda
grid <- 10^seq(10, -2, length = 100)

# Lave et trænings og et test sæt på data
set.seed(123)
# 66,666 % til tærning og 33 til test
train <- sample(1:nrow(x), nrow(x)*2/3)
test <- (-train)
y.test <- y[test]


# Ridge lave en ridge på alle trænings data
ridge.mod <- glmnet(
  x[train, ], 
  y[train], 
  alpha = 0,
  lambda = grid, #grid er noget vi selv vælger
  thresh = 1e-12
)
# Ridge med Cross-validation
set.seed(123)
ridge_cv.out <- cv.glmnet(
  x[train, ], 
  y[train], 
  alpha = 0,
  lambda = grid,
  nfolds = 5                    )

# Finde den bedste lambda
bestlam <- ridge_cv.out$lambda.min
# Finde RMSE på træning ridge cross-validation med den bedste lambda
rmse_ridge_cv <- sqrt(ridge_cv.out$cvm[ridge_cv.out$lambda == bestlam])

# Tester ridge på test data, med lambda
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test, ]) 
# RMSE på test data
rmse_ridge_test <- sqrt(mean((ridge.pred - y.test)^2))


# Lasso på trænings data
lasso.mod <- glmnet(
  x[train, ], 
  y[train], 
  alpha = 1,
  lambda = grid,
  thresh = 1e-12
)
coef(lasso.mod)

# Lasso på cross-validation trænings data
set.seed(123)
lasso_cv.out <- cv.glmnet(
  x[train, ], 
  y[train], 
  alpha = 1,
  lambda = grid,
  nfolds = 5                    )

# Finde den bedste lambda
bestlam <- lasso_cv.out$lambda.min
# Finder den bedste rmse på trænings data med brug af den bedste lambda
rmse_lasso_cv <- sqrt(lasso_cv.out$cvm[lasso_cv.out$lambda == bestlam])

# Teste på test data
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test, ]) 
# Regne test rmse ud på lasso
rmse_lasso_test <- sqrt(mean((lasso.pred - y.test)^2))

#Simpel model
# 0 features
set.seed(123)
# Definerer y og x variablerne
glm.fit <- glm(antal_bestilte ~ 1, data = final_data[train, ])
# udregne rmse for trænings data
rmse_0_cv <- sqrt(cv.glm(final_data[train, ], glm.fit , K = 5)$delta[1])
# udrenge rmse for test data
rmse_0_test <- sqrt(mean((final_data[test, ]$antal_bestilte - predict(glm.fit, final_data[test, ]))^2))

# Best subset selection
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

data_train <- final_data[train,]
data_test <- final_data[test,]

k <- 5 # Vi danner 5 folds
n <- nrow(data_train) # registrerer hvor mange observationer, vi har.
set.seed(123) 
folds <- sample(rep(1:k, length = n)) #Vi tildeler en værdi mellem 1 og
dim(data_train)[2]  # Der er 12 variabler og dermed 11 prædiktorer

cv.errors <- matrix(NA, k, 11,
                    dimnames = list(NULL, paste(1:11)))
cv.errors

for (j in 1:k) { # her gennemløbes alle folds
  best.fit <- regsubsets(antal_bestilte ~ .,
                         data = data_train[folds != j, ],
                         nvmax = (dim(data_train)[2]-1))
  for (i in 1:11) { # her gennemløbes alle kandidatmodeller
    pred <- predict(best.fit, data_train[folds == j, ], id = i)
    # predict-funktionen ovenfor kalder den funktion, vi har lavet tidligere. 
    cv.errors[j, i] <-
      mean((data_train$antal_bestilte[folds == j] - pred)^2) # Her udregnes MSE for hver 
    # fold for hver kandidatmodel 
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean) # apply er en smart funktion, der 
# gennemløber alle rækker og tager gennemsnittet henover hver søjle, som svarer 
# til hver kandidatmodel. # Kan også bruge en map funktion
mean.cv.errors # Vi får altså en gennemsnitlig MSE for hver kandidatmodel.
par(mfrow = c(1,1))
plot(mean.cv.errors, type = "b", xlab = "Modelstørrelse", ylab = "Gennemsnitlig CV MSE")
which.min(mean.cv.errors)

# Her fittes modellen til ALLE træningsdata ud folds
reg.best <- regsubsets(antal_bestilte ~ ., data = data_train,
                       nvmax = (dim(data_train)[2]-1))
coef(reg.best, 1) #Skal være 1 nu 

pred_best_subset <- predict(reg.best, data_test, id = 1) #og her 1

mse_best_subset <- mean((final_data[test,]$antal_bestilte - pred_best_subset)^2)
rmse_bestsubset_test <- sqrt(mse_best_subset)
rmse_bestsubset_cv <- sqrt(min(mean.cv.errors))

#RMSE test
rmse_0_test
rmse_bestsubset_test
rmse_lasso_test
rmse_ridge_test

#RMSE træning
rmse_0_cv
rmse_bestsubset_cv
rmse_lasso_cv
rmse_ridge_cv
# ------------------------------- plot af RMSE --------------------------------
# Lave en temporary data frame med den data som skal bruges i plottet
rmse_data <- data.frame(
  Model = c("Simple Model", "Best Subset", "Lasso", "Ridge"),
  RMSE = c(rmse_0_test, rmse_bestsubset_test, rmse_lasso_test, rmse_ridge_test)
)

# Lave plottet med ggplot
ggplot(rmse_data, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity", color = "black") +
  theme_minimal() +
  labs(
    title = "Test RMSE for Forskellige Modeller",
    x = "Model",
    y = "Test RMSE"
  ) 

# Kører Subset på alt data
regfit.best_fuld <- regsubsets(antal_bestilte ~ ., data = final_data,
                               nvmax = 11)
coef(regfit.best_fuld, 1)

pred_best_subset_fuld <- predict(regfit.best_fuld, final_data, id = which.min(mean.cv.errors))

mse_best_subset_fuld <- mean((final_data$antal_bestilte - pred_best_subset_fuld)^2)
mse_best_subset_fuld
sqrt(mse_best_subset_fuld)


# ----------------------------- Best Subset Pred -------------------------------

# Træning af model med Best Subset Selection
best_subset_model <- lm(antal_bestilte ~ fpi, #+ kamp_type, 
                        data = final_data[train,])

# Opretter forudsigelsesdata for en specifik kampsituation
prediction_data <- data.frame(fpi = 10.7)#, kamp_type = "c_kamp")

# Forudsigelser for den specifikke kamp baseret på den bedste model
best_model_size <- which.min(mean.cv.errors)  # Vælger den bedste modelstørrelse

predicted_antal_bestilte <- predict(best_subset_model, prediction_data, id = best_model_size)

# Beregning af MSE og RMSE for den specifikke kamp
mse_ny <- mean((predicted_antal_bestilte - final_data$antal_bestilte)^2)
rmse_ny <- sqrt(mse_ny)

# Forudsigelser på testdatasættet baseret på den bedste model
predicted_antal_bestilte_test <- predict(best_subset_model, final_data, id = best_model_size)

# Beregning af MSE og RMSE for testdatasættet
mse_test <- mean((predicted_antal_bestilte_test - final_data[test,]$antal_bestilte)^2)
rmse_test <- sqrt(mse_test)

# Udskriver resultater
print(predicted_antal_bestilte)
