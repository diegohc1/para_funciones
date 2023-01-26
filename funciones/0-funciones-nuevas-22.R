# *********************************************************************************************
# para aplicar algunos ejemplos
# nn <- 100000
#
# df <- data.frame(
#    p01 = sample(c("a", "b"), nn, replace = T),
#    p02_01 = sample(c("a", "b", "c"), nn, replace = T),
#    p02_02 = sample(c("a", "b", "c"), nn, replace = T),
#    p02_03 = sample(c("a", "b", "c"), nn, replace = T),
#    gestion = sample(c("Estatal", "No estatal"), nn, replace = T),
#    sexo = sample(c("Hombre", "Mujer"), nn, replace = T),
#    area = sample(c("Urbano", "Rural"), nn, replace = T),
#    medida = runif(nn, min = 450, max = 650),
#    peso = sample(c(0.9, 1, 1.1, 1.2, 1.3, 1.5, 1.6, 1.7), nn, replace = T))
#
# df$p01 <- as.factor(df$p01)
# df$p02_01 <- as.factor(df$p02_01)
# df$p02_02 <- as.factor(df$p02_02)
# df$p02_03 <- as.factor(df$p02_03)
# df$gestion <- as.factor(df$gestion)
# df$sexo <- as.factor(df$sexo)
# ********************************************************************************************

# funciones:

# DESCRIPTIVOS ----

pega_lista <- function(data, nc){

  # primero agrega una columna con el nombre de la lista, despues pega las listas
  # util cuando un df se separa en listas segun algun estrato y se le aplican las
  # mismas funciones para luego pegarlas otra vez...

  #  pega_lista(data, nc)
  #    data: lista de dataframe
  #    nc: nombre de la columna que tendra un vector con el nombre de la lista

  if(!inherits(data, "list")) stop("Se aplica sobre una lista")
  if(length(unique(unlist(lapply(data, ncol)))) != 1) warning("Hay una cantidad diferente de columnas")
  if(length(unique(unlist(lapply(data, names)))) != 1) warning("Las columnas tienen nombres diferentes")

  temp <- mapply(function(x, y) within(x, {ncol = y}), data, names(data), SIMPLIFY = FALSE)
  #temp <- do.call("rbind", temp)
  temp <- dplyr::bind_rows(temp) #bind_rows tambien pega si es que no son las mismas columnas
  names(temp)[names(temp) == 'ncol'] <- nc
  rownames(temp) <- NULL
  return(temp)

  # ejemplo:
  # ejem <- list(hom = data.frame(v = 1:3), muj = data.frame(v = 4:6))
  # pega_lista(ejem, "nombre")

}


tabla_freq_dplyr <- function(data, x, peso = NULL, na = NULL, compara = NULL){

  # tabla de frecuencias para una columna
  # lo mismo que table()... pero devuelve los % redondeados, en dataframe y acepta pesos

  # ejemplo:
  # tabla_freq_dplyr(df1, "p01")

  # df1 %>%
  #   group_by(gestion) %>%
  #   tabla_freq_dplyr("p01")

  if(!require(dplyr)) stop("'dplyr' debe estar instalado")

  if(!is.null(peso)){peso_s <- dplyr::sym(peso)}
  if(!is.null(peso)){
    if(!sum(is.na(data[[peso]])) == 0) stop("La variable de pesos tiene missing")
  }

  x_s <- dplyr::sym(x)

  data2 <- data %>%
    {if(!is.null(peso)) dplyr::count(., {{x_s}}, wt = {{peso_s}}) else dplyr::count(., {{x_s}})} %>%
    {if(is.null(na)) dplyr::filter(., !is.na({{x_s}})) else .} %>%
    dplyr::mutate(prop = round(prop.table(n)*100, 1))

  names(data2)[names(data2) == x] <- "opcion"

  if(dplyr::is_grouped_df(data) & !is.null(compara)){ #util para comparar los %
    data2 <- tidyr::pivot_wider(data2, -n, names_from = names(group_keys(data)), values_from = prop)
  }

  return(data2)


}


tabla_freq_columnas_dplyr <- function(data, nomvar, peso = NULL, starts = NULL, na = NULL){

  # tabla de frecuencias (tabla_freq) para varias columnas

  # data: data.frame
  # nomvar: vector con el nombre de las columnas. Si se quiere aplicar sobre columnas que comparten
  #         el mismo nombre (ej: p02_01, p02_02) se coloca "p02" y starts = TRUE.

  #ejemplo:
  #tabla_freq_columnas(df1, nomvar = "p02", starts = TRUE)

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

#***********************************************************************************

# PISCOMETRICO ----

reporte_lavaan <- function(model_cfa_lavaan, puntajes = TRUE){

  # toma un objeto cfa lavaan para acomodarlo para el reporte psicometrico
  # acomoda las cargas factoriales e indicadores de ajuste en una tabla

  # model_cfa_lavaan: objeto cfa lavaan
  # puntajes: logical si es que se quiere puntajes

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


pca_umc_reporte <- function(x, corr = NULL, puntajes = TRUE){

  #toma un data.frame y aplica PCA
  #devuelve las varianza explicada, cargas del primer componente y alpha de cronbach

  # x: data.frame
  # corr: por defecto aplica correlacion de pearson, podemos colocar 'poly' para indicar
  #       correlacion policorica
  # puntajes: logical si es que se quiere puntajes

  if(sum(sapply(x, function(xx) sum(is.na(xx)))) > 0) stop("Es preferible que no hayan NAs en las columnas =)")

  if(is.null(corr)){
    ee <- eigen(cor(x), symmetric = FALSE) # symmetric=FALSE previene cargas negativas [espero]
  }else{
    cor_pol <- psych::polychoric(x)$rho
    ee <- eigen(cor_pol, symmetric = FALSE)
    alpha <- psych::alpha(cor_pol, warnings = FALSE)$feldt$alpha[[1]] #Confiabilidad
  }

  #calculamos varianza (val), cargas (l), pesos (w)
  val <- ee$values
  val_sq <- sqrt(val) #desviacion
  l <- ee$vectors %*% diag(val_sq)
  w <- ee$vectors %*% diag(1/val_sq)

  if(all(l[, 1] < 0)) {l[, 1] <- l[, 1]*-1; w[, 1] <- w[, 1]*-1} # ¿por que? U_U

  if(puntajes == TRUE){
    z <- as.matrix(scale(x)) # datos estandarizados y matrix
    s <- z %*% l # datos estandarizados por sus cargas
    s <- scale(s)
  }

  cargas <- data.frame(Item = names(x), Pesos = w[, 1], Cargas = l[, 1])
  vr <- c("Pesos", "Cargas")
  cargas[vr] <- apply(cargas[vr], 2, function(x) format(round(x, 3), decimal.mark = ","))
  varex <- format(round(val[1]/sum(val)*100, 2), decimal.mark = ",")

  if(puntajes == TRUE){
    return(list(puntajes = s[, 1], indicadores = varex, cargas = cargas, confiabilidad = alpha))}
  else {
    return(list(indicadores = varex, cargas = cargas, confiabilidad = alpha))}

}

pca_1 <- function(x){
  # para inspeccionar datos asociados al PCA
  # aplica correlacion policorica
  ee <- eigen(psych::polychoric(x)$rho, symmetric = FALSE)
  val <- ee$values #varianza
  l <- ee$vectors %*% diag(sqrt(val)) #cargas
  if(all(l[, 1] < 0)) {l[, 1] <- l[, 1]*-1} # ¿por que? U_U
  cargas <- data.frame(Item = names(x), Cargas = l[, 1])
  varex <- val[1]/sum(val)
  return(list(cargas = cargas, varex = varex))

}

cfa_recursivo <- function(data, model_lavaan, recursivo = TRUE, puntajes = TRUE){

  mod1 <- cfa(model_lavaan, data = data, ordered = TRUE, mimic = "Mplus", estimator = "WLSMV")

  if(recursivo){

    indi <- lavaan::fitmeasures(mod1,  c("cfi", "tli", "srmr", "rmsea"))
    cargafac <- subset(lavaan::parameterEstimates(mod1), op == "=~")


    if(
      any(c(purrr::map_lgl(indi[c("cfi", "tli")], ~.x < 0.95),
            purrr::map_lgl(indi[c("srmr", "rmsea")], ~.x > 0.10)))
    ){

      indi_nueva = indi
      cargafac_nueva = cargafac

      repeat{
        if(length(lavNames(mod1, type = "lv")) == 1){
          if(nrow(cargafac_nueva) <= 4){break} # 1 var latente: si son 4 o menos items, pará
        }else{
          # 2 var latente: si son 3 o menos items en alguno, pará
          if(any(purrr::map_lgl(lapply(split(cargafac_nueva, cargafac_nueva$lhs), nrow), ~.x <= 3))){break}
        }

        if(any(c(purrr::map_lgl(indi_nueva[c("cfi", "tli")], ~.x < 0.95),
                 purrr::map_lgl(indi_nueva[c("srmr", "rmsea")], ~.x > 0.10)))){

          # identificamos items
          if(nrow(filter(cargafac_nueva, est < 0.4)) == 0){ # si no hay items con cargas menores a 0.4, identificamos el menor
            eliminar = filter(cargafac_nueva, est == min(est))$rhs
          }else{
            eliminar = filter(cargafac_nueva, est < 0.4)$rhs # identificamos items con cargas menores a 0.4
          }

          cargafac_nueva = filter(cargafac_nueva, !rhs %in% all_of(eliminar)) # nuevo modelo

          modstring <- split(cargafac_nueva, cargafac_nueva$lhs) %>%
            map(~paste(pull(.x, rhs), collapse = "+")) %>%
            imap(~paste(.y, .x, sep = '=~')) %>%
            paste(collapse = "\n")

          mod2 <- cfa(modstring, data = data, ordered = TRUE, mimic = "Mplus", estimator = "WLSMV")

        }else{break} # paramos
      }

      cfa_inicial <- reporte_lavaan(mod1, puntajes = FALSE)
      cfa_sugerido <- reporte_lavaan(mod2, puntajes = puntajes)

      lista_cfa <- list(cfa_inicial = cfa_inicial, cfa_sugerido = cfa_sugerido)

      cargas <- map(lista_cfa, "cargas") %>%
        map(~select(.x, -4, -5)) %>%
        reduce(~left_join(.x, .y, by = c("Escala", "Item"), suffix = c(".inicial", ".sugerido")))

      indicadores <- map(lista_cfa, "indicadores") %>%
        map(~select(.x, -3)) %>%
        reduce(~left_join(.x, .y, by = c("Indicadores"), suffix = c(".inicial", ".sugerido")))

      return(list(cargas = cargas, indicadores = indicadores))

    }else{

      cfa_inicial <- reporte_lavaan(mod1, puntajes = puntajes)
      return(cfa_inicial)

    }

  }

  cfa_inicial <- reporte_lavaan(mod1, puntajes = puntajes)
  return(cfa_inicial)


}


acomoda_string_lavaan <- function(data_preg){
  if(length(unique(data_preg$Cod_indice2)) == 1){
    mm <- paste(unique(data_preg$Cod_indice), paste(data_preg$cod_preg, collapse = '+'), sep = '=~')

  }else{
    mm <- split(data_preg, data_preg$Cod_indice2) %>%
      map(~paste(pull(.x, cod_preg), collapse = "+")) %>%
      imap(~paste(.y, .x, sep = '=~')) %>%
      paste(collapse = "\n")
  }
  return(mm)
}


pca_recursivo <- function(data, recursivo = TRUE, puntajes = TRUE){

  pca_uno <- pca_1(data)

  if(recursivo){

    indi <- pca_uno$varex
    cargafac <- pca_uno$cargas

    if(indi < .50){

      indi_nueva = indi
      cargafac_nueva = cargafac
      data2 = data


      repeat{
        if(nrow(cargafac_nueva) <= 4){break} # si son 4 o menos items, pará

        if(indi_nueva < .50){

          # identificamos items
          if(nrow(filter(cargafac_nueva, abs(Cargas) < 0.4)) == 0){ # si no hay items con cargas menores a 0.4, identificamos el menor
            eliminar = filter(cargafac_nueva, Cargas == min(abs(Cargas)))$Item
          }else{
            eliminar = filter(cargafac_nueva, abs(Cargas) < 0.4)$Item # identificamos items con cargas menores a 0.4
          }

          # retiramos las columnas y nuevo modelo
          data2 <- data2[, !(names(data2) %in% eliminar)]
          pca_dos <- pca_1(data2)
          indi_nueva <- pca_dos$varex
          cargafac_nueva <- pca_dos$cargas

        }else{break} # paramos
      }

      pca_inicial <- pca_umc_reporte(data, corr = "poly", puntajes = FALSE)
      pca_sugerido <- pca_umc_reporte(data2, corr = "poly", puntajes = puntajes)

      lista_pca <- list(pca_inicial = pca_inicial, pca_sugerido = pca_sugerido)

      cargas <- map(lista_pca, "cargas") %>%
        map(~select(.x, 1, 3)) %>%
        reduce(~left_join(.x, .y, by = c("Item"), suffix = c(".inicial", ".sugerido")))

      indicadores <- map_df(lista_pca, "indicadores")

      return(list(cargas = cargas, indicadores = indicadores))

    }else{

      pca_inicial <- pca_umc_reporte(data, corr = "poly", puntajes = puntajes)
      return(pca_inicial)

    }

  }

  pca_inicial <- pca_umc_reporte(data, corr = "poly", puntajes = puntajes)
  return(pca_inicial)


}



reporte_insumos <- function(data, tipo, model_lavaan, puntajes = TRUE){

  #devuelve puntajes e insumos para el reporte de las escalas, segun pca o cfa

  data2 <- mutate(data, across(everything(), as.numeric))

  if(tipo == "PCA"){
    list_insumos <- pca_umc_reporte(data2, corr = "poly", puntajes = puntajes)
  }else{
    m1 <- cfa(model_lavaan, data = data2, ordered = TRUE, mimic = "Mplus", estimator = "WLSMV")
    list_insumos <- reporte_lavaan(m1, puntajes = puntajes)
  }

  return(list_insumos)

}

#******************************************************************

# OTRAS FUNCIONES:

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

calcula_nse <- function(data, ise, w = NULL){

  if(!require(dplyr)) stop("'dplyr' debe estar instalado")

  isevec <- pull(data, {{ise}})

  if(is.null(w)){
    cortes <- quantile(isevec, c(.35, .60, .85), na.rm = TRUE)
  } else {
    cortes <- Hmisc::wtd.quantile(isevec, probs = c(.35, .60, .85), weights = w)
  }

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


#*************************************************************************************

# Función para sacar proporciones (funciona con una o con dos variables y múltiples veces) ----

proporcion_estrato2=function(data, var, var2=NULL,FUN=proporcion_estrato,...){

  proporcion_estrato <- function(data, grupo1, grupo2,label=NULL,peso=NULL,size_h=NULL,diseno=NULL){
    #Estimación de la proporción no pesada
    if (missing(peso)){
      b=data %>%
        group_by({{grupo1}},{{grupo2}}) %>%
        summarise(n=n()) %>%
        drop_na() %>%
        mutate(freq=round((n/sum(n)*100),2))
    }else{#Estimación de la proporción pesada
      b=data %>%
        group_by({{grupo1}},{{grupo2}}) %>%
        summarise(n=sum({{peso}},na.rm=T)) %>%
        drop_na() %>%
        mutate(freq=round((n/sum(n)*100),2))
    }

    if(!missing(label)){
      if(Hmisc::label(data[[as_label(enquo(grupo2))]])=="") stop("Hay una columna o columnas que no tiene(n) label(s)")

      c=as.data.frame(Hmisc::label(data[[as_label(enquo(grupo2))]])) %>%
        rename(label_opcion = 1)

      b=cbind(b,c)

    }

    #Estimación de la h de cohen
    if(!missing(size_h)){
      group_ <- enquo(grupo1)
      h_size=b %>% mutate(freq=freq/100) %>% select(-n) %>% pivot_wider(names_from =as_label(group_),
                                                                        values_from = "freq") %>%
        ungroup() %>%
        mutate(h=abs(2*asin(sqrt(.[[2]]))-2*asin(sqrt(.[[3]]))))

      b=left_join(b,h_size[,c(1,ncol(h_size))])
    }

    #Estimación de la significancia
    if(!missing(diseno)){
      if(!is.list(diseno)) stop("Debes meter un diseño de acuerdo a los parámetros de survey")
      options(survey.lonely.psu = "certainty") #opcion de survey
      group_ <- enquo(grupo1)
      # bd_design <- diseno
      labs <- enquo(grupo2)
      bd_design <- svydesign(data = data, id = as.formula(diseno[[1]]),
                             strata = as.formula(diseno[[2]]),
                             fpc = as.formula(diseno[[3]]), nest = TRUE, pps = "brewer")
      p_val=svychisq(as.formula(paste0("~",as_label(labs),"+",as_label(group_))),design=bd_design,na.rm=T,statistic="Chisq")$p.value
      b=cbind(b,p_val=p_val)
    }

    return(b)

  }

  if(missing(var2)){
    purrr::map(var,function(x)
      FUN(data,grupo1=.data[[x]],...)) %>%
      set_names(var) %>% #purrr::map(~set_names(.x, vari2)) %>% #nombres a las listas
      purrr::map(~rename(.x, opcion = 1)) %>% #nombre de la columna
      pega_lista("grupo") #juntamos todas las tablas
  }else{
    purrr::map(var,function(x)
      purrr::map(var2, function(y)
        FUN(data,.data[[x]],.data[[y]],...))) %>%
      set_names(var) %>% purrr::map(~set_names(.x, var2)) %>% #nombres a las listas
      map_depth(2, ~rename(.x, estrato = 1,opcion=2)) %>% #nombre de la columna
      purrr::map(~pega_lista(.x, "grupo")) %>% pega_lista("medida") #juntamos todas las tablas
  }

}

# Función para sacar múltiples significancias (funciona con una o con dos) ----

post_proporcion_estrato2 <- function(data, var, var2,FUN=post_proporcion_estrato,...){
  post_proporcion_estrato <- function(data, grupo1, grupo2,label=NULL,peso=NULL,size_h=NULL,diseno=NULL){

    nombres1=as.character(data %>% select({{grupo1}}) %>% distinct() %>% drop_na() %>% as_vector())
    nombres2=as.character(data %>% select({{grupo2}}) %>% distinct() %>% drop_na() %>% as_vector())

    combo1=map(1:ncol(combn(nombres1,2)),~combn(nombres1,2)[,.x])
    combo2=map(1:ncol(combn(nombres2,2)),~combn(nombres2,2)[,.x])

    gru1=map_chr(1:ncol(combn(nombres1,2)),~paste0(combn(nombres1,2)[,.x],collapse="_"))
    gru2=map_chr(1:ncol(combn(nombres2,2)),~paste0(combn(nombres2,2)[,.x],collapse="_"))


    if(length(nombres1)<=2&length(nombres2)<=2)
      stop("Los dos grupos que hay acá tienen solo dos categorías.
         Al menos uno de los grupos a comparar debe tener más de dos categorías")
    if(missing(diseno))
      stop("Debes colocar el diseño para sacar significancia")

    group_ <- enquo(grupo1)
    labs <- enquo(grupo2)

    if(length(nombres1)>2&length(nombres2)<3){

      a=map(combo1,~data %>% filter({{grupo1}} %in% .x) %>%
              mutate(estrx=fct_drop({{grupo1}}))) %>%
        set_names(gru1) %>%
        map(~svydesign(data = .x, id = as.formula(diseno[[1]]),
                       strata = as.formula(diseno[[2]]),
                       fpc = as.formula(diseno[[3]]), nest = TRUE, pps = "brewer"))

      f=a %>% map(~svychisq(as.formula(paste0("~",as_label(labs),"+estrx")),
                            design=.x,na.rm=T,statistic="Chisq")$p.value) %>%
        map(~as.data.frame(.x)) %>%
        pega_lista("comparacion1") %>% bind_cols("comparacion2"=gru2)

      rownames(f)=NULL
      colnames(f)=c("p_val","grupo_comparacion1","grupo_comparacion2")
      f$p_val=round(f$p_val,5)

    }else if(length(nombres1)<3&length(nombres2)>2){
      a=map(combo2,~data %>% filter({{grupo2}} %in% .x) %>%
              mutate(estrx=fct_drop({{grupo2}}))) %>%
        set_names(gru2) %>%
        map(~svydesign(data = .x, id = as.formula(diseno[[1]]),
                       strata = as.formula(diseno[[2]]),
                       fpc = as.formula(diseno[[3]]), nest = TRUE, pps = "brewer"))

      f=a %>% map(~svychisq(as.formula(paste0("~","estrx","+",as_label(group_))),
                            design=.x,na.rm=T,statistic="Chisq")$p.value) %>%
        map(~as.data.frame(.x)) %>%
        pega_lista("comparacion2") %>% bind_cols("comparacion1"=gru1)

      rownames(f)=NULL
      colnames(f)=c("p_val","grupo_comparacion2","grupo_comparacion1")
      f$p_val=round(f$p_val,5)
    }else{

      a=map(combo1,function(x)
        map(combo2,function(y)
          (data %>% filter({{grupo1}} %in% x,{{grupo2}} %in% y) %>%
             mutate(estrx1=fct_drop({{grupo1}}),
                    estrx2=fct_drop({{grupo2}}))))) %>%
        set_names(gru1) %>% purrr::map(~set_names(.x, gru2)) %>%
        map_depth(2,~svydesign(data = .x, id = as.formula(diseno[[1]]),
                               strata = as.formula(diseno[[2]]),
                               fpc = as.formula(diseno[[3]]), nest = TRUE, pps = "brewer"))

      f=a %>% map_depth(2,~svychisq(as.formula(paste0("~","estrx2","+","estrx1")),
                                    design=.x,na.rm=T,statistic="Chisq")$p.value) %>%
        map_depth(2,~as.data.frame(.x)) %>%
        purrr::map(~pega_lista(.x, "comparacion2")) %>% pega_lista("comparacion1")

      rownames(f)=NULL
      colnames(f)=c("p_val","grupo_comparacion2","grupo_comparacion1")
      f$p_val=round(f$p_val,5)

    }

    return(f)
  }

  purrr::map(var,function(x)
    purrr::map(var2, function(y)
      post_proporcion_estrato(data,.data[[x]],.data[[y]],...))) %>%
    set_names(var) %>% purrr::map(~set_names(.x, var2)) %>% #nombres a las listas
    #map_depth(2, ~rename(.x, estrato = 1,opcion=2)) %>% #nombre de la columna
    purrr::map(~pega_lista(.x, "grupo2")) %>% pega_lista("grupo1") #juntamos todas las tablas

}



#Ejemplo de diseño: dis = list(id="~cod_mod8+dni_est",
#            strata= "~estrato",
#            fpc="~pikIE+prob_est")


# util para generar archivos de excel temporales
show_in_excel <- function(.data){
  tmp <- paste0(tempfile(), ".xlsx")
  rio::export(.data, tmp)
  browseURL(url = tmp)
}

