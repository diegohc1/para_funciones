library(tidyverse)


#z
# *********************************************************************************************

#para aplicar algunos ejemplos
# nn <- 5001
#
# df <- data.frame(
#   p01 = sample(c("a", "b"), nn, replace = T),
#   p02_01 = sample(c("a", "b", "c"), nn, replace = T),
#   p02_02 = sample(c("a", "b", "c"), nn, replace = T),
#   p02_03 = sample(c("a", "b", "c"), nn, replace = T),
#   gestion = sample(c("Estatal", "No estatal"), nn, replace = T),
#   sexo = sample(c("Hombre", "Mujer"), nn, replace = T),
#   area = sample(c("Urbano", "Rural"), nn, replace = T),
#   medida = runif(nn, min = 450, max = 650),
#   peso = sample(c(0.9, 1, 1.1, 1.2, 1.3), nn, replace = T))

#********************************************************************************************

#pega_lista -----
# primero agrega una columna con el nombre de la lista, despues pega las listas
# util cuando un df se separa en listas segun algun estrato y se le aplican las
# mismas funciones para luego pegarlas otra vez...

#  pega_lista(data, nc)
#    data: lista de dataframe
#    nc: nombre de la columna que tendra un vector con el nombre de la lista

pega_lista <- function(data, nc){

  if(!inherits(data, "list")) stop("Se aplica sobre una lista")
  if(length(unique(unlist(lapply(data, ncol)))) != 1) warning("Hay una cantidad diferente de columnas")
  if(length(unique(unlist(lapply(data, names)))) != 1) warning("Las columnas tienen nombres diferentes")

  temp <- mapply(function(x, y) within(x, {ncol = y}), data, names(data), SIMPLIFY = FALSE)
  #temp <- do.call("rbind", temp)
  temp <- dplyr::bind_rows(temp) #bind_rows tambien pega si es que no son las mismas columnas
  names(temp)[names(temp) == 'ncol'] <- nc
  rownames(temp) <- NULL
  return(temp)

}

#ejemplo:
# ejem <- list(hom = data.frame(v = 1:3), muj = data.frame(v = 4:6))
# ejem2 <- list(A = data.frame(a = 1:10000, b = sample(letters, 10000, replace = TRUE)),
#               B = data.frame(a = 1:10000, b = sample(letters, 10000, replace = TRUE)))
# pega_lista(ejem, "nombre")

# ejem
#
# ejem2
#
# pega_lista(ejem, "nombre")
# pega_lista(ejem2, "nombre")
# dplyr::bind_rows(ejem, .id = "nombre")
# dplyr::bind_rows(ejem2, .id = "nombre")
#
#
# ej <- list(hom = data.frame(v = 1:3), muj = data.frame(x = 4:6))
# pega_lista(ej, "nombre")
#
# ej <- list(hom = data.frame(v = 1:3), muj = data.frame(x = 4:6, z = 1:3))
# pega_lista(ej, "nombre")
#
# dplyr::bind_rows(ej, .id = "nombre")

#************************************************************************************************

# tabla_freq ----
# tabla de frecuencias para una columna
# lo mismo que table()... pero devuelve los % redondeados, en dataframe y acepta pesos

tabla_freq <- function(data, x, peso = NULL){

  if(!is.null(peso)){
    f <- paste0(peso, "~", x)
    df1 <- aggregate(as.formula(f), data = data, FUN = sum)
    names(df1)[names(df1) == peso] <- "Freq"
  }else{
    df1 <- as.data.frame(table(data[x]))
  }

  df2 <- within(df1, {prop = round(prop.table(Freq)*100, 1)})
  names(df2) <- c("opcion", "n", "prop")

  return(df2)

}

#
# df1 <- df
#
# tabla_freq(df1, "p01")
# tabla_freq(df1, "p01", "peso")
#
# df$peso[1] <- NA
# df1$p01[5] <- NA
#
# df$peso <- na.omit(df$peso)
#df <- na.omit(df)
#df[, complete.cases(df[, "p01"])]

# f <- paste0(peso, "~", x)
# df1 <- aggregate(peso~p01, data = df1, FUN = sum)
# aggregate(peso~p01, data = df1, FUN = sum, na.action=NULL)


#****************************************************************************************

#tabla_freq_columnas ----

# tabla de frecuencias (tabla_freq) para varias columnas

# data: data.frame
# nomvar: vector con el nombre de las columnas. Si se quiere aplicar sobre columnas que comparten
#         el mismo nombre (ej: p02_01, p02_02) se coloca "p02" y starts = TRUE.

tabla_freq_columnas <- function(data, nomvar, peso = NULL, starts = NULL){

  if(!is.null(starts)){
    nom <- names(data[, grep(paste0("^", nomvar), names(data), value = TRUE)])
    names(nom) <- nom
  }else{
    nom <- nomvar
    names(nom) <- nom
  }

  df2 <- lapply(nom, function(x) tabla_freq(data, x, peso = peso))

  #pega_lista(df2, "var")
  dplyr::bind_rows(df2, .id = "var")

}

# tabla_freq_columnas(df, "p01") #es lo mismo que tabla_freq
# tabla_freq_columnas(df, nomvar = c("p01", "p02_01", "sexo"))
# tabla_freq_columnas(df, nomvar = "p02", starts = TRUE)
# tabla_freq_columnas(df, nomvar = "p02", peso = "peso", starts = TRUE)


#******************************************************************************************

#tabla_freq_estrato
# tabla de frecuencias segun estrato
# probando el performance de la funcion, la funcion se vuelve mas lenta al
#  aumentar el numero de grupos. Pero es muy util para 1 o 2 grupos.
#  (suficiente para un reporte/excel [segun gestion o segun gestion y sexo, por ejemplo])


tabla_freq_estrato <- function(data, var, grupo, peso = NULL, compara = NULL){

  if(length(grupo) == 1){
    names(data)[names(data) == grupo] <- "group_temp"
  }else{
    data$group_temp <- apply(data[, grupo], 1, paste, collapse = "-")
  }

  df2 <- lapply(split(data, data[["group_temp"]]), function(x) tabla_freq(x, var, peso = peso))
  df3 <- pega_lista(df2, "estrato")
  #df3 <- dplyr::bind_rows(df2, .id = "estrato")

  if(length(grupo) == 1){
    names(df3)[names(df3) == 'estrato'] <- grupo
  }else{
    df3 <- data.frame(df3[1:3], do.call(rbind, strsplit(df3[["estrato"]], "-")))
    names(df3) <- c(names(df3[1:3]), grupo)
  }

  if(length(grupo) == 1 & !is.null(compara)){ #util para comparar los %
    df3 <- reshape(df3[-2], idvar = "opcion", timevar = grupo, direction = "wide")
  }

  return(df3)

}

# tabla_freq_estrato(df, "p02_01", grupo = "gestion")
# tabla_freq_estrato(df, "p02_01", grupo = "gestion", compara = TRUE)
# tabla_freq_estrato(df, "p02_01", grupo = c("gestion", "area"), peso = "peso")


#***********************************************************************************

library(lavaan)

#reporte_lavaan ----

# toma un objeto cfa lavaan para acomodarlo para el reporte psicometrico
# acomoda las cargas factoriales e indicadores de ajuste en una tabla

# model_cfa_lavaan: objeto cfa lavaan
# puntajes: logical si es que se quiere puntajes

reporte_lavaan <- function(model_cfa_lavaan, puntajes = TRUE){

  #cargas factoriales estandarizadas
  m <- lavaan::standardizedSolution(model_cfa_lavaan, ci = FALSE, cov.std = FALSE)
  m <- m[which(m$op == "=~"), ]
  m <- within(m, {stars =
    ifelse(pvalue < 0.001, "***",  ifelse(pvalue < 0.01, "**",  ifelse(pvalue < 0.05, "*", " ")))})
  m <- m[c("lhs", "rhs", "est.std", "se", "stars")]
  m <- setNames(m, c("Escala", "Item", "Est", "SE", "sig."))
  vredo <- c("Est", "SE") #para redondear
  # m[vredo] <- apply(m[vredo], 2, function(x) format(round(x, 3), decimal.mark = ","))

  #indicadores de ajuste
  fit1 <- lavaan::fitmeasures(model_cfa_lavaan,
                              c("cfi", "tli", "srmr", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))
  indicadores1 <- data.frame(Indicadores = names(fit1)[1:4], Valores = unclass(fit1)[1:4], row.names = NULL)
  indicadores1 <- within(indicadores1, {'IC al 90%' =
    ifelse(Indicadores == "rmsea",  paste0("[", round(fit1[5], 3), "-", round(fit1[6], 3), "]"), "")})

  # el paquete indica que "reliability" esta deprecated, indica usar "compRelSEM"
  confiabilidad <- semTools::compRelSEM(model_cfa_lavaan)

  if(puntajes == TRUE){
    puntajes1 <- as.data.frame(lavaan::lavPredict(model_cfa_lavaan))
  }

  if(puntajes == TRUE){
    return(list(cargas = m, indicadores = indicadores1, confiabilidad = confiabilidad, puntajes = puntajes1))}
  else {return(list(cargas = m, indicadores = indicadores1, confiabilidad = confiabilidad))}

}

# lavaan::parameterEstimates(m1, standardized = TRUE)
# m1 <- cfa(mm, data = bd3[-1], ordered = TRUE, mimic = "Mplus", estimator = "WLSMV")
# fit1 <- round(lavaan::fitmeasures(m1, c("cfi", "tli", "srmr", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper")), 4)
# indicadores1 <- data.frame(Indicadores = names(fit1)[1:4], Valores = unclass(fit1)[1:4], row.names = NULL)
# indicadores1 <- within(indicadores1, {ci =
#   ifelse(Indicadores == "rmsea",  paste0("[", fit1[5], "-", fit1[6], "]"), "")})
#
# summary(m1)
# summary(m1, fit.measures=TRUE)
#
# paste0("[", fit1[5], "-", fit1[6], "]")

#**************************************


invarianza1 <- function(m, data, grupo){

  #m = string del modelo, "constructo=~item1+item2+..."
  #data = base de datos
  #grupo = grupo, "gestion2", "area", "sexo" o también puede ser c("gestion2","area)

  cfa_WLS <- function(...) cfa(estimator = "WLSMV", ...)

  m_pool <- map(grupo,~cfa_WLS(mm, data)) %>% set_names(grupo)
  m_conf <- map(grupo,~cfa_WLS(mm, data, group = .x)) %>% set_names(grupo)
  m_metr <- map(grupo,~cfa_WLS(mm, data, group = .x, group.equal = c("loadings"))) %>% set_names(grupo)
  m_esca <- map(grupo,~cfa_WLS(mm, data, group = .x, group.equal = c("loadings", "intercepts"))) %>% set_names(grupo)

  map_df(lst(m_pool,m_conf, m_metr, m_esca),
         function(x) map_df(x,~fitMeasures(.x, c("cfi","tli","rmsea","srmr")),.id="grupo"),.id="tip_inv") %>%
    as_tibble() %>%
    mutate(across(c(cfi:srmr),~round(as.numeric(.),4))) %>%
    pivot_longer(cols=c(cfi:srmr),names_to="indicadores",values_to="val") %>%
    pivot_wider(names_from =tip_inv,values_from = "val") %>%
    mutate(inv_metr = abs(round(m_metr - m_conf , 3)),
           inv_sca = abs(round(m_metr - m_esca, 3))) %>%
    mutate(inv_metr_t = case_when(
      indicadores %in% c("cfi", "tli") & inv_metr <= 0.020 ~ "cumple",
      indicadores %in% c("rmsea", "srmr") & inv_metr <= 0.030 ~ "cumple",
      TRUE~"no cumple"),
      inv_sca_t = case_when(
        indicadores %in% c("cfi", "tli") & inv_sca <= 0.020 ~ "cumple",
        indicadores %in% c("rmsea", "srmr") & inv_sca <= 0.010 ~ "cumple",
        TRUE~"no cumple"))
}


#***************************************************************************************

library(psych)

#pca_umc_reporte ----

#toma un data.frame y aplica PCA
#devuelve las varianza explicada y cargas del primer componente

# x: data.frame
# corr: por defecto aplica correlacion de pearson, podemos colocar 'poly' para indicar
#       correlacion policorica
# puntajes: logical si es que se quiere puntajes

pca_umc_reporte <- function(x, corr = NULL, puntajes = TRUE){

  if(is.null(corr)){
    val <- eigen(cor(x))$values #autovalores
    t_vec <- t(eigen(cor(x))$vectors) # transpuesta de autovectores

  }else{
    cor_pol <- psych::polychoric(x)$rho
    val <- eigen(cor_pol)$values #autovalores
    t_vec <- t(eigen(cor_pol)$vectors) # transpuesta de autovectores
    confiabilidad <- psych::alpha(cor_pol, warnings = FALSE)$feldt$alpha[[1]] #Confiabilidad
  }

  media_x <- colMeans(x)
  sd_x <- purrr::map_dbl(x, sd)
  cargas_x <- t_vec*sqrt(val) #cargas de cada una de las variables
  peso_x <- cargas_x/val[1] #peso de cada una de las variables

  #en algunas ocaciones, por una razón desconocida, los signos de las cargas se voltean
  # si todas están en negativo, cambiar a positivo
  # PENDIENTE revisar ....
  if(all(cargas_x[1, ] < 0)) {cargas_x <- cargas_x*-1; peso_x <- peso_x*-1}

  if(puntajes == TRUE){
    para_pegar <- rep(1, nrow(x))
    for(j in 1:length(x)){
      temp <- (t(t(x) - media_x)[, j]*peso_x[1, j])/sd_x[j]
      para_pegar <- cbind(para_pegar, temp)
    }
    indice1 <- rowSums(para_pegar[, -1])
  }

  cargas <- data.frame(Item = names(x), Pesos = peso_x[1, ], Cargas = cargas_x[1, ])
  vr <- c("Pesos", "Cargas")
  cargas[vr] <- apply(cargas[vr], 2, function(x) round(x, 3))
  varex <- round(val[1]/sum(val)*100, 2)

  if(puntajes == TRUE){
    return(list(puntajes = indice1, indicadores = varex, cargas = cargas, confiabilidad = confiabilidad))}
  else {
    return(list(indicadores = varex, cargas = cargas, confiabilidad = confiabilidad))}

}


#devuelve puntajes e insumos para el reporte de las escalas, segun pca o cfa
reporte_insumos <- function(data, tipo, model_lavaan, puntajes = TRUE){

  data2 <- mutate(data, across(everything(), as.numeric))

  if(tipo == "PCA"){
    list_insumos <- pca_umc_reporte(data2, corr = "poly", puntajes = puntajes)
  }else{
    m1 <- cfa(model_lavaan, data = data2, ordered = TRUE, mimic = "Mplus", estimator = "WLSMV")
    list_insumos <- reporte_lavaan(m1, puntajes = puntajes)
  }

  return(list_insumos)

}


#*************************************************************************************************************

# asigna_label -----

#asigna labels a las columnas que le indicamos
# vec_label: vector con los label
# colnombre: columnas a las que le asignas el label
#            si length(veclabel) == ncol(data), asignara a todas las columnas, en el orden de veclabel

asigna_label <- function(data, vec_label, colnombre = NULL){

  if(!require(labelled)) stop("'labelled' debe estar instalado")

  if(length(names(data)) == length(vec_label)){
    names(vec_label) <- names(data)
  }else{
    if(missing(colnombre))
      stop("¡cuidado!, necesitas especificar los nombres de las columnas que quieres cambiar  (no seas gil)")
    names(vec_label) <- colnombre
  }

  data[names(vec_label)] <- set_variable_labels(data[names(vec_label)], .labels = vec_label)
  return(data)

}



#**************************************************************************

#mean_estrato ------

#explorando....

mean_prop_estrato <- function(data, medida, peso = "NULL"){

  options(dplyr.summarise.inform = FALSE) #para que no aparezca el mensaje de agrupado

  if(!is_grouped_df(data)) stop("Los datos deben estar agrupados, 'dplyr::group_by()'")
  #se podria agregar el grupo como argumento en la funcion, pero se complica
  # si son varios porque hay que poner '...',

  f1 <- function(d) drop_na(d) %>%  mutate(freq = round((n/sum(n)*100), 1))
  f2 <- function(d) select(ungroup(drop_na(d)), media)

  if(rlang::ensym(peso) != "NULL"){ #con pesos
    #no estoy seguro si rlang::ensym() sea lo mas correcto, pero funciona U_U
    bind_cols(
      summarise(data,  n = sum({{peso}}, na.rm = TRUE)) %>% f1(),
      summarise(data, media = weighted.mean({{medida}}, w = {{peso}}, na.rm = TRUE)) %>% f2()
    )

  }else{ #sin pesos
    bind_cols(
      summarise(data, n = sum(n(), na.rm = TRUE)) %>% f1(),
      summarise(data, media = mean({{medida}}, na.rm = TRUE)) %>% f2()
    )
  }
}

# df_gr <- group_by(df, gestion, sexo)
# mean_prop_estrato(df_gr, medida)
# mean_estrato(df, medida)
# mean_prop_estrato(df_gr, medida, peso)
# df_gr <- group_by(df, gestion, sexo)
# mean_estrato(df_gr, medida, peso)


#********************************************************************************************************

# correlaciones -----

cor2 <- function(data, round = 2) round(cor(data, use = "pairwise.complete.obs"), round)

cor_long <- function(data, round = 2){

  mc <- round(cor(data, use = "pairwise.complete.obs"), round)

  df_mc <- data.frame(
    v1 = rownames(mc)[row(mc)],
    v2 = colnames(mc)[col(mc)],
    corr = c(mc))

  df_mc <- subset(df_mc, v1 != v2) #quitamos repetidos

  return(df_mc)

}

#*******************************************************************************
#para ver el patron de missing

g_patron_missing <- function(data, preg){

  data[preg] %>%
    mutate(id = row_number()) %>%
    gather(-id, key = "key", value = "val") %>%
    mutate(isna = is.na(val)) %>%
    ggplot(aes(rev(key), id, fill = isna)) +
    geom_raster(alpha = 0.8) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text.y = element_text(size = 4.2, color = "black", hjust = 0),
          axis.text.x = element_blank(),
          legend.position = "bottom",
          axis.title = element_text(size = 8.5),
          legend.text = element_text(size = 8, color = "#5A5D63"),
          plot.margin = unit(c(0, 0, 0.0, 0), "cm")) +
    scale_fill_manual(name = "", values = c('gray', 'black'), labels = c("Presente", "Missing")) +
    labs(x = 'Pregunta\n', y = "Observaciones", title = " ") +
    scale_x_discrete(labels = rev(preg)) +
    coord_flip()

}

#************************************************************************************
# hace el split y saca el grupo con el que se hixo el split
# porsiaca un argumento para retirar otra columna
# util para realizar correlaciones por algun estrato
split2 <- function(data, grupo, retira = NULL){
  data2 <- split(data[, !names(data) %in% c(grupo, retira)], data[[grupo]])
  return(data2)
}


#***********************************************************************************************
#calcular icc desde objeto de lmer
calc_icc <- function(lmer_out){ #lmer es el output de lmer()
  var <- as.data.frame(lme4::VarCorr(lmer_out))
  icc <- (var$vcov[1]/(var$vcov[1]+var$vcov[2]))
  return(icc)
}
#calc_icc(hlm0)



#anterior, guardarlo porsiaca
#cargas factoriales
# m <- lavaan::parameterEstimates(model_cfa_lavaan, standardized = TRUE)
# m <- m[which(m$op == "=~"), ]
# m <- within(m, {stars =
#   ifelse(pvalue < 0.001, "***",  ifelse(pvalue < 0.01, "**",  ifelse(pvalue < 0.05, "*", " ")))})
# m <- m[c("lhs", "rhs", "est", "se", "std.all", "stars")]
# m <- setNames(m, c("Escala", "Item", "Beta", "SE", "Beta_std", "sig."))
# vredo <- c("Beta", "SE", "Beta_std") #para redondear
# m[vredo] <- apply(m[vredo], 2, function(x) format(round(x, 3), decimal.mark = ","))

#***************************************************************************************************************

#rendimiento promedio con diseño muestral


#media_estrato2
#calcula el rendimiento promedio de algun estrato con grupos/niveles/categorias

# media_estrato2(data, medida, peso, estrato, diseno, size_g)

#  data: bases de datos
#  medida: medida para calcular el promedio
#  peso: peso
#  estrato: grupo, caracteristica, por ej: sexo, gestion, likert colapsado, q1 y q4, solo dos grupos
#  diseno: si es TRUE, calcula pvalue usando el diseño muestral, tienen que estar todas las variables
#          asociadas (dni_est, cod_mod7, pikIE, Prob_alumno, gestion)
#  size_g: si es TRUE, calcula la g de hegdes [mmm, aqui deberiamos usar la varianza con dm?]

media_estrato_dm <- function(data, medida, peso, grupos, diseno = FALSE, size_g = FALSE){

  options(survey.lonely.psu = "certainty") #opcion de survey
  options(dplyr.summarise.inform = FALSE) #para que no aparezca el mensaje de agrupado

  d1 <- group_by(data, {{grupos}}) #datos agrupados

  temp <- d1 %>%
    summarise(media = weighted.mean({{medida}}, w = {{peso}}, na.rm = TRUE),
              VariW = Hmisc::wtd.var({{medida}}, w = {{peso}}, na.rm = TRUE),
              N = n()) %>%
    drop_na()

  d2 <- select(temp, 1:2) #tabla con el promedio

  if(size_g == TRUE){

    if(n_distinct(pull(data, {{grupos}})) > 2)
      stop("El grupo tiene más de dos categorias, no se puede calcular la g de hedges.
           En el futuro se podria implementar el eta (?)")

    a <- temp %>% #acomodaciones
      pivot_longer(col = c("media","VariW","N"), names_to = "var") %>%
      pivot_wider(names_from = 1, values_from = "value")

    #calculo de g
    v <- (a[1,2]-a[1,3])/sqrt(((a[3,2]-1)*a[2,2]+(a[3,3]-1)*a[2,3])/(a[3,2]+a[3,3]-2))

    d3 <- mutate(d2, size_g = pull(v)) #solo g
  }

  if(diseno == TRUE){

    if(n_distinct(pull(data, {{grupos}})) > 2)
      stop("Por el momento, para las pruebas de hipótesis, la función acepta estratos de dos grupos")

    vars_dis_m <- c("cod_mod7", "dni_est", "gestion", "pikIE", "Prob_alumno")
    if(!all(vars_dis_m %in% names(data)))
      stop("La base de datos no tiene las variables necesarias para implementar el diseño muestral,
            es necesario que esten presentes: 'cod_mod7', 'dni_est', 'estrato', 'pikIE' y 'Prob_alumno'")

    grupo <- names(attributes(d1)$groups)[1]
    medida_ <- enquo(medida)

    bd_design <- svydesign(data = data, id = ~cod_mod7+dni_est, strata = ~estrato,
                           fpc = ~pikIE+Prob_alumno, nest = TRUE, pps = "brewer")

    p_val <- svyttest(as.formula(paste(as_label(medida_), "~", grupo)), design = bd_design)$p.value

    d4 <- mutate(d2, p_val = p_val) #p.val sin g

    if(size_g == TRUE){d5 <- mutate(d3, p_val = p_val)} #p.val con g

  }

  if(diseno == TRUE & size_g == TRUE){return(d5)}
  else if (diseno == TRUE & size_g == FALSE){return(d4)}
  else if (diseno == FALSE & size_g == TRUE){return(d3)}
  else {return(d2)}

}

#ejemplos:

#media_estrato2(bd2, M500_EM_ESAE_6P_2020_CT, Peso_lectura,  sexo)

# media_estrato_dm(bd2,
#                medida = M500_EM_ESAE_6P_2020_CT,
#                peso = Peso_lectura,
#                estrato = p04_01,
#                diseno = TRUE,
#                size_g = FALSE)


#**************************************

#función auxiliar para media_estrato2_varios
# pega_lista <- function(data, nc){
#   if(!is.list(data)) stop("Se aplica sobre una lista")
#   imap(data, ~mutate(.x, !!nc := .y)) %>% bind_rows() #para bind listas
# }

#media_estrato2_varios
# para varios indices y varios grupos

media_estrato2_varios <- function(data, medidas, grupos, peso, diseno = FALSE, size_g = FALSE){
  map(medidas, function(x) map(grupos, function(y)
    media_estrato_dm(data, medida = .data[[x]], peso = {{peso}},
                     grupos = .data[[y]], diseno = diseno, size_g = size_g))) %>%
    set_names(medidas) %>% map(~set_names(.x, grupos)) %>% #nombres a las listas
    map_depth(2, ~rename(.x, opcion = 1)) %>% #nombre de la columna
    map(~pega_lista(.x, "grupo")) %>% pega_lista("medida") #juntamos todas las tablas
}

#ejemplo:

#definimos:
# vmedidas <- c("indice1", "indice2", "indice3")
# vgrupos <- c("grupo1", "grupo2", "grupo3")

# media_estrato2_varios(bd2,
#                       medidas = vmedidas,
#                       grupos = vgrupos,
#                       peso = Peso_lectura,
#                       diseno = TRUE,
#                       size_g = TRUE)


#****************************************************************************************

# pairwise.svyttest -----
# lo mismo que pairwise.prop.test() pero con la informacion de svyttest
# ajusta los pvalues con el metodo de Bonferroni

#declaramos el diseño muestral
# md <- svydesign(data = fam_mat, id = ~cod_mod7+dni_est, strata = ~estrato, fpc = ~pikIE+Prob_alumno, nest = TRUE, pps = "brewer")
# pairwise.svyttest(md, "M500_EVA_4P_2021_MA", "p14_01")

#https://www.oecd-ilibrary.org/docserver/9789264056275-12-en.pdf?expires=1644254523&id=id&accname=guest&checksum=436BE5717F3033FD6E5E484138158A84

pairwise.svyttest <- function(dm, var, g){

  categ <- levels(dm$variables[[g]])
  compara <- map_chr(1:length(categ), ~paste(categ[c(-.x)], collapse = "-"))
  f <- as.formula(paste0(var, "~", g))

  #aplicamos ttest para cada par
  tt <- suppressWarnings( map(categ, ~svyttest(f, subset(dm, get(g) != .x))) )

  #extraemos info
  pval <- map_dbl(tt, ~.x$p.value)
  pval_adj <- round(p.adjust(pval, method = "bonferroni"), 3)

  data <- data.frame(
    comparacion = compara,
    diff = map_dbl(tt, ~.x$estimate),
    pval_adj_bonf = pval_adj)

  return(data)

}


#*************************************************************************************

#calcula_nse -----

# agrega una columna 'NSE' a la base con las categorias del nivel socioeconomico

# calcula_nse(data, ise)
## data: base de datos
## ise: columna que contiene el puntaje del ise

calcula_nse <- function(data, ise){

  if(!require(dplyr)) stop("'dplyr' debe estar instalado")

  isevec <- pull(data, {{ise}})
  cortes <- quantile(isevec, c(.35, .60, .85), na.rm = TRUE)
  lev <- c("NSE alto", "NSE medio", "NSE bajo", "NSE muy bajo")

  data2 <- data %>%
    mutate(NSE = case_when(
      {{ise}} <= cortes[[1]] ~ "NSE muy bajo",
      {{ise}} > cortes[[1]] & {{ise}} <= cortes[[2]] ~ "NSE bajo",
      {{ise}} > cortes[[2]] & {{ise}} <= cortes[[3]] ~ "NSE medio",
      {{ise}} > cortes[[3]] ~ "NSE alto",
    )) %>%
    mutate(NSE = factor(NSE, labels = lev, levels = lev))

  return(data2)

}

# ejemplo:
#simulamos
# bd <- data.frame(ISE = rnorm(1000))
#
# #aplicamos la funcion
# bd1 <- calcula_nse(bd, ISE)
# head(bd1)
#
# #comprobamos que sean  los cortes
# # alto (15%) medio (25%) bajo (25%) muy bajo (35%)
# prop.table(table(bd1$NSE))

#library(ggplot2)
#ggplot(bd1, aes(x = NSE, y = ISE)) +
#  geom_jitter()


# ***********************************************************

tabla_freq_dplyr <- function(data, x, peso = NULL, na = NULL, compara = NULL){

  if(!is.null(peso)){peso_s <- dplyr::sym(peso)}
  x_s <- dplyr::sym(x)

  data2 <- data %>%
    {if(!is.null(peso)) count(., {{x_s}}, wt = {{peso_s}}) else count(., {{x_s}})} %>%
    {if(is.null(na)) filter(., !is.na({{x_s}})) else .} %>%
    mutate(prop = round(prop.table(n)*100, 1))

  names(data2)[names(data2) == x] <- "opcion"

  if(is_grouped_df(data) & !is.null(compara)){ #util para comparar los %
    data2 <- pivot_wider(data2, -n, names_from = names(group_keys(data)), values_from = prop)
  }

  return(data2)

}

tabla_freq_columnas_dplyr <- function(data, nomvar, peso = NULL, starts = NULL, na = NULL){

  if(!is.null(starts)){
    nom <- names(data[, grep(paste0("^", nomvar), names(data), value = TRUE)])
    names(nom) <- nom
  }else{
    nom <- nomvar
    names(nom) <- nom
  }

  df2 <- lapply(nom, function(x) tabla_freq_dplyr(data, x, peso = peso, na = na))
  dplyr::bind_rows(df2, .id = "var")

}


