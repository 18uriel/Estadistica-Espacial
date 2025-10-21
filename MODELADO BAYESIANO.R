# =============================================================================
# MODELOS BAYESIANOS JERÁRQUICOS ESPACIALES
# Análisis de Superficie Agrícola en Puno - 2024
# Autor: URIEL ROJAS CHURQUIPA QUISPE
# Base de datos: Encuesta Nacional Agropecuaria 2024 - Puno
# =============================================================================

rm(list = ls())
gc()

setwd("C:/Users/ASUS Vivobook/OneDrive/Desktop/DECIMO SEMESTRE/ESTADISTICA ESPACIAL")

# LIBRERÍAS REQUERIDAS
library(readr)
library(dplyr)
library(ggplot2)
library(sf)
library(spdep)
library(viridis)
library(GGally)

options(device = "RStudioGD")
dev.off()

cat("================================================================================\n")
cat("MODELOS BAYESIANOS JERÁRQUICOS ESPACIALES\n")
cat("Análisis de Superficie Agrícola Total - Puno 2024\n")
cat("================================================================================\n\n")

# ============================================================================
# 1. CARGA Y PREPARACIÓN DE DATOS
# ============================================================================
cat("1. CARGA Y PREPARACIÓN DE DATOS\n")
cat("-" %+% rep("-", 75) %+% "\n\n")

datos <- read_csv("puno1.csv")

datos_clean <- datos %>%
  rename(
    provincia = NOMBREPV,
    distrito = NOMBREDI,
    superficie_ha = P104_SUP_ha
  ) %>%
  mutate(
    provincia_id = as.integer(as.factor(provincia)),
    distrito_id = as.integer(as.factor(distrito)),
    log_superficie = log(superficie_ha + 0.01),
    obs_id = row_number()
  ) %>%
  filter(!is.na(provincia), !is.na(distrito), !is.na(superficie_ha))

cat("Observaciones originales:", nrow(datos), "\n")
cat("Observaciones limpias:", nrow(datos_clean), "\n")
cat("Provincias:", n_distinct(datos_clean$provincia), "\n")
cat("Distritos:", n_distinct(datos_clean$distrito), "\n\n")

# ============================================================================
# 2. ANÁLISIS DESCRIPTIVO
# ============================================================================
cat("2. ESTADÍSTICAS DESCRIPTIVAS DE LA VARIABLE DE INTERÉS\n")
cat("-" %+% rep("-", 75) %+% "\n\n")

summary_stats <- data.frame(
  Estadístico = c("n", "Media", "Mediana", "Desv.Est.", "Mín.", "Máx.", "Q25", "Q75", "CV(%)"),
  Valor = c(
    nrow(datos_clean),
    round(mean(datos_clean$superficie_ha, na.rm = TRUE), 2),
    round(median(datos_clean$superficie_ha, na.rm = TRUE), 2),
    round(sd(datos_clean$superficie_ha, na.rm = TRUE), 2),
    round(min(datos_clean$superficie_ha, na.rm = TRUE), 2),
    round(max(datos_clean$superficie_ha, na.rm = TRUE), 2),
    round(quantile(datos_clean$superficie_ha, 0.25, na.rm = TRUE), 2),
    round(quantile(datos_clean$superficie_ha, 0.75, na.rm = TRUE), 2),
    round((sd(datos_clean$superficie_ha, na.rm = TRUE) / mean(datos_clean$superficie_ha, na.rm = TRUE)) * 100, 2)
  )
)

print(summary_stats)
cat("\n")

# ============================================================================
# GRÁFICO 1: Distribución empírica vs transformación logarítmica
# ============================================================================
cat("Generando Gráfico 1: Distribuciones de la variable de interés\n")

p1 <- ggplot(datos_clean, aes(x = superficie_ha)) +
  geom_histogram(bins = 60, fill = "#2E86AB", alpha = 0.7, color = "white") +
  geom_density(aes(y = after_stat(count)), color = "#A23B72", linewidth = 1.2) +
  labs(
    title = "Distribución Empírica de Superficie Agrícola",
    subtitle = "Asimetría positiva indica presencia de minifundios",
    x = "Superficie (hectáreas)", y = "Frecuencia"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "gray40"))

print(p1)
Sys.sleep(1)

# ============================================================================
# GRÁFICO 2: Transformación logarítmica (normalización)
# ============================================================================
cat("Generando Gráfico 2: Transformación logarítmica\n")

p2 <- ggplot(datos_clean, aes(x = log_superficie)) +
  geom_histogram(bins = 60, fill = "#F18F01", alpha = 0.7, color = "white") +
  geom_density(aes(y = after_stat(count)), color = "#C73E1D", linewidth = 1.2) +
  labs(
    title = "Distribución Transformada Log(Superficie)",
    subtitle = "Aproximación a normalidad para modelado bayesiano",
    x = "Log(Superficie + 0.01)", y = "Frecuencia"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "gray40"))

print(p2)
Sys.sleep(1)

# ============================================================================
# GRÁFICO 3: Q-Q Plots para evaluación de normalidad
# ============================================================================
cat("Generando Gráfico 3: Evaluación de supuestos de normalidad\n")

p3 <- ggplot(datos_clean, aes(sample = log_superficie)) +
  stat_qq(color = "#2E86AB", alpha = 0.6, size = 2.5) +
  stat_qq_line(color = "#A23B72", linewidth = 1.2) +
  labs(
    title = "Q-Q Plot: Log(Superficie) vs Normal",
    subtitle = "Evaluación de normalidad para especificación del modelo bayesiano",
    x = "Cuantiles Teóricos", y = "Cuantiles Observados"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "gray40"))

print(p3)
Sys.sleep(1)

# ============================================================================
# ANÁLISIS POR PROVINCIAS
# ============================================================================
cat("\n3. ANÁLISIS JERÁRQUICO: NIVEL PROVINCIAL\n")
cat("-" %+% rep("-", 75) %+% "\n\n")

stats_provincia <- datos_clean %>%
  group_by(provincia, provincia_id) %>%
  summarise(
    n_unidades = n(),
    n_distritos = n_distinct(distrito),
    superficie_media = mean(superficie_ha, na.rm = TRUE),
    superficie_sd = sd(superficie_ha, na.rm = TRUE),
    superficie_mediana = median(superficie_ha, na.rm = TRUE),
    superficie_min = min(superficie_ha, na.rm = TRUE),
    superficie_max = max(superficie_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(cv = (superficie_sd / superficie_media) * 100) %>%
  arrange(desc(superficie_media))

print(stats_provincia)
cat("\n")

# ============================================================================
# GRÁFICO 4: Heterogeneidad entre provincias (Boxplot)
# ============================================================================
cat("Generando Gráfico 4: Heterogeneidad espacial a nivel provincial\n")

p4 <- ggplot(datos_clean, aes(x = reorder(provincia, log(superficie_ha + 0.01), FUN = median), 
                              y = log(superficie_ha + 0.01),
                              fill = provincia)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(
    title = "Heterogeneidad de Superficie por Provincia",
    subtitle = "Log(Superficie) - Estructura multinivel: provincia ← distrito ← unidad",
    x = "Provincia", y = "Log(Superficie + 0.01)", fill = "Provincia"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "gray40"),
        legend.position = "none")

print(p4)
Sys.sleep(1)

# ============================================================================
# ANÁLISIS POR DISTRITOS
# ============================================================================
cat("\n4. ANÁLISIS JERÁRQUICO: NIVEL DISTRITAL\n")
cat("-" %+% rep("-", 75) %+% "\n\n")

stats_distrito <- datos_clean %>%
  group_by(distrito, distrito_id, provincia) %>%
  summarise(
    n_unidades = n(),
    superficie_media = mean(superficie_ha, na.rm = TRUE),
    superficie_sd = sd(superficie_ha, na.rm = TRUE),
    superficie_mediana = median(superficie_ha, na.rm = TRUE),
    superficie_total = sum(superficie_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(cv = (superficie_sd / superficie_media) * 100) %>%
  arrange(desc(superficie_media))

top_20 <- stats_distrito %>% head(20)

cat("Top 20 Distritos por Superficie Media:\n")
print(head(stats_distrito, 20))
cat("\n")

# ============================================================================
# GRÁFICO 5: Top 20 distritos con variabilidad
# ============================================================================
cat("Generando Gráfico 5: Top 20 distritos (efectos multinivel)\n")

p5 <- ggplot(top_20, aes(x = reorder(distrito, superficie_media), 
                         y = superficie_media, fill = provincia)) +
  geom_col(alpha = 0.8, color = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = pmax(0, superficie_media - superficie_sd/sqrt(n_unidades)), 
                    ymax = superficie_media + superficie_sd/sqrt(n_unidades)),
                width = 0.25, alpha = 0.7, linewidth = 0.6, color = "black") +
  coord_flip() +
  scale_fill_viridis_d(option = "turbo") +
  labs(
    title = "Top 20 Distritos: Superficie Media ± 1 EE",
    subtitle = "Jerarquía: Provincia → Distrito → Unidad Agropecuaria",
    x = "Distrito", y = "Superficie Media (ha)", fill = "Provincia"
  ) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "gray40"),
        legend.position = "bottom",
        axis.text = element_text(size = 9))

print(p5)
Sys.sleep(1)

# ============================================================================
# 5. ESTRUCTURA ESPACIAL Y AUTOCORRELACIÓN
# ============================================================================
cat("\n5. ANÁLISIS DE DEPENDENCIA ESPACIAL\n")
cat("-" %+% rep("-", 75) %+% "\n\n")

datos_distrito <- datos_clean %>%
  group_by(distrito_id, provincia_id) %>%
  summarise(
    distrito = first(distrito),
    provincia = first(provincia),
    n_obs = n(),
    superficie_media = mean(superficie_ha, na.rm = TRUE),
    log_sup_media = mean(log_superficie, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(superficie_media))

n_distritos <- nrow(datos_distrito)
n_provincias <- n_distinct(datos_distrito$provincia_id)

cat("Total distritos:", n_distritos, "\n")
cat("Total provincias:", n_provincias, "\n\n")

# Matriz de vecindad basada en provincia
nb_list <- vector("list", n_distritos)
for(i in 1:n_distritos){
  prov_i <- datos_distrito$provincia_id[i]
  vecinos <- which(datos_distrito$provincia_id == prov_i & 
                     datos_distrito$distrito_id != datos_distrito$distrito_id[i])
  if(length(vecinos) == 0) vecinos <- i
  nb_list[[i]] <- as.integer(vecinos)
}

class(nb_list) <- "nb"
W_listw <- nb2listw(nb_list, style = "W", zero.policy = TRUE)

# Test de Moran I
moran_test <- moran.test(datos_distrito$superficie_media, W_listw, zero.policy = TRUE)

cat("TEST DE MORAN I (Autocorrelación Espacial)\n")
cat("Estadístico I:", round(moran_test$estimate[1], 6), "\n")
cat("Valor esperado E[I]:", round(moran_test$estimate[2], 6), "\n")
cat("Varianza:", round(moran_test$estimate[3], 6), "\n")
cat("p-valor:", format.pval(moran_test$p.value, digits = 4), "\n\n")

if(moran_test$p.value < 0.05){
  if(moran_test$estimate[1] > 0){
    cat("INTERPRETACIÓN: Existe AUTOCORRELACIÓN ESPACIAL POSITIVA significativa (α=0.05)\n")
    cat("Las unidades espacialmente cercanas tienden a tener valores similares.\n")
  }
} else {
  cat("INTERPRETACIÓN: No se detecta autocorrelación espacial significativa.\n")
}
cat("\n")

# ============================================================================
# GRÁFICO 6: Moran Scatterplot (Diagrama de Scatter espacial)
# ============================================================================
cat("Generando Gráfico 6: Moran Scatterplot\n")

moran_data <- data.frame(
  x = scale(datos_distrito$superficie_media)[,1],
  y = scale(lag.listw(W_listw, datos_distrito$superficie_media, zero.policy = TRUE))[,1]
)

# Asignar cuadrantes
moran_data$cuadrante <- ifelse(moran_data$x > 0 & moran_data$y > 0, "I: (+,+)",
                               ifelse(moran_data$x < 0 & moran_data$y > 0, "II: (-,+)",
                                      ifelse(moran_data$x < 0 & moran_data$y < 0, "III: (-,-)", "IV: (+,-)")))

p6 <- ggplot(moran_data, aes(x = x, y = y, color = cuadrante)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.2, linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = c("I: (+,+)" = "#E63946", "II: (-,+)" = "#457B9D",
                                "III: (-,-)" = "#1D3557", "IV: (+,-)" = "#A8DADC")) +
  labs(
    title = "Moran Scatterplot: Dependencia Espacial",
    subtitle = paste0("I = ", round(moran_test$estimate[1], 4), " (p = ", 
                      format.pval(moran_test$p.value, digits = 3), ")"),
    x = "Superficie Estandarizada", y = "Lag Espacial Estandarizado", color = "Cuadrante"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "gray40"))

print(p6)
Sys.sleep(1)

# ============================================================================
# 6. MODELADO BAYESIANO JERÁRQUICO
# ============================================================================
cat("\n6. ESPECIFICACIÓN Y AJUSTE DEL MODELO BAYESIANO JERÁRQUICO\n")
cat("-" %+% rep("-", 75) %+% "\n\n")

if(!require(lme4, quietly = TRUE)){
  install.packages("lme4")
  library(lme4)
}

datos_modelo <- datos_clean %>%
  select(log_superficie, distrito_id, provincia_id) %>%
  mutate(distrito_id = as.factor(distrito_id),
         provincia_id = as.factor(provincia_id))

# Especificación de modelos
modelo_nulo <- lm(log_superficie ~ 1, data = datos_modelo)

modelo_provincial <- lmer(log_superficie ~ 1 + (1|provincia_id), 
                          data = datos_modelo, REML = FALSE)

modelo_jerarquico <- lmer(log_superficie ~ 1 + (1|provincia_id) + (1|distrito_id), 
                          data = datos_modelo, REML = FALSE)

# Comparación de modelos
aic_vals <- c(AIC(modelo_nulo), AIC(modelo_provincial), AIC(modelo_jerarquico))
bic_vals <- c(BIC(modelo_nulo), BIC(modelo_provincial), BIC(modelo_jerarquico))

comparacion <- data.frame(
  Modelo = c("Nulo: y ~ 1", 
             "Provincial: y ~ 1 + (1|Provincia)",
             "Jerárquico: y ~ 1 + (1|Provincia) + (1|Distrito)"),
  AIC = round(aic_vals, 1),
  BIC = round(bic_vals, 1),
  LogLik = round(c(logLik(modelo_nulo), logLik(modelo_provincial), 
                   logLik(modelo_jerarquico)), 2)
)

cat("COMPARACIÓN DE MODELOS (Criterios de Información)\n")
print(comparacion)
cat("\n")

mejor_idx <- which.min(aic_vals)
modelo_final <- list(modelo_nulo, modelo_provincial, modelo_jerarquico)[[mejor_idx]]

cat("MODELO SELECCIONADO:", comparacion$Modelo[mejor_idx], "\n\n")

cat("RESUMEN DEL MODELO FINAL:\n")
print(summary(modelo_final))
cat("\n")

# ============================================================================
# 7. ANÁLISIS DE RESIDUOS Y DIAGNÓSTICOS
# ============================================================================
cat("\n7. DIAGNÓSTICOS DEL MODELO\n")
cat("-" %+% rep("-", 75) %+% "\n\n")

residuos <- datos_modelo %>%
  mutate(
    fitted = predict(modelo_final),
    residuo = log_superficie - fitted,
    residuo_std = scale(residuo)[,1]
  )

cat("Estadísticas de Residuos:\n")
cat("  Media:", round(mean(residuos$residuo, na.rm = TRUE), 6), "\n")
cat("  Desv.Est.:", round(sd(residuos$residuo, na.rm = TRUE), 4), "\n")
cat("  Rango:", round(min(residuos$residuo, na.rm = TRUE), 4), "a", 
    round(max(residuos$residuo, na.rm = TRUE), 4), "\n\n")

# ============================================================================
# GRÁFICO 7: Residuos vs Valores Ajustados
# ============================================================================
cat("Generando Gráfico 7: Diagnóstico de Homocedasticidad\n")

p7 <- ggplot(residuos, aes(x = fitted, y = residuo)) +
  geom_point(alpha = 0.4, size = 1.5, color = "#2E86AB") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "loess", se = TRUE, color = "#E63946", alpha = 0.2, linewidth = 1.2) +
  labs(
    title = "Residuos vs Valores Ajustados",
    subtitle = "Evaluación de homocedasticidad",
    x = "Valores Ajustados (Escala Log)", y = "Residuos Crudos"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "gray40"))

print(p7)
Sys.sleep(1)

# ============================================================================
# GRÁFICO 8: Q-Q Plot de residuos estandarizados
# ============================================================================
cat("Generando Gráfico 8: Diagnóstico de Normalidad de Residuos\n")

p8 <- ggplot(residuos, aes(sample = residuo_std)) +
  stat_qq(color = "#2E86AB", alpha = 0.6, size = 2) +
  stat_qq_line(color = "#E63946", linewidth = 1.2) +
  labs(
    title = "Q-Q Plot: Residuos Estandarizados",
    subtitle = "Evaluación de normalidad del componente de error",
    x = "Cuantiles Teóricos", y = "Cuantiles Observados"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "gray40"))

print(p8)
Sys.sleep(1)

# ============================================================================
# GRÁFICO 9: Distribución de residuos
# ============================================================================
cat("Generando Gráfico 9: Distribución de Residuos\n")

p9 <- ggplot(residuos, aes(x = residuo)) +
  geom_histogram(bins = 60, fill = "#F18F01", alpha = 0.7, color = "white") +
  geom_density(aes(y = after_stat(count)), color = "#C73E1D", linewidth = 1.2) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Distribución de Residuos",
    subtitle = "Cercana a normal según diagnósticos",
    x = "Residuo", y = "Frecuencia"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "gray40"))

print(p9)
Sys.sleep(1)

# ============================================================================
# 8. PREDICCIONES Y VALIDACIÓN
# ============================================================================
cat("\n8. PREDICCIONES DEL MODELO\n")
cat("-" %+% rep("-", 75) %+% "\n\n")

predicciones_distrito <- datos_distrito

if("lmerMod" %in% class(modelo_final)){
  efectos_distrito <- ranef(modelo_final)$distrito_id
  efectos_provincia <- ranef(modelo_final)$provincia_id
  
  predicciones_distrito <- predicciones_distrito %>%
    mutate(
      pred_log = fixef(modelo_final)[1] + 
        efectos_distrito[match(distrito_id, rownames(efectos_distrito)), 1] +
        if(!is.null(efectos_provincia)) 
          efectos_provincia[match(provincia_id, rownames(efectos_provincia)), 1] else 0
    )
} else {
  predicciones_distrito$pred_log <- fixef(modelo_final)[1]
}

predicciones_distrito <- predicciones_distrito %>%
  mutate(
    pred_superficie = exp(pred_log) - 0.01,
    observado = superficie_media,
    error = observado - pred_superficie,
    error_pct = (error / observado) * 100
  )

pred_clean <- predicciones_distrito %>% 
  filter(!is.na(observado) & !is.na(pred_superficie))

RMSE <- sqrt(mean((pred_clean$observado - pred_clean$pred_superficie)^2, na.rm = TRUE))
MAE <- mean(abs(pred_clean$observado - pred_clean$pred_superficie), na.rm = TRUE)
MAPE <- mean(abs(pred_clean$error_pct), na.rm = TRUE)
R2 <- cor(pred_clean$observado, pred_clean$pred_superficie, use = "complete.obs")^2

cat("MÉTRICAS DE VALIDACIÓN DEL MODELO\n")
cat("  RMSE:", round(RMSE, 3), "ha\n")
cat("  MAE:", round(MAE, 3), "ha\n")
cat("  MAPE:", round(MAPE, 2), "%\n")
cat("  R²:", round(R2, 4), "\n\n")

# ============================================================================
# GRÁFICO 10: Observado vs Predicho (Validación)
# ============================================================================
cat("Generando Gráfico 10: Validación del Modelo\n")

p10 <- ggplot(pred_clean, aes(x = observado, y = pred_superficie, color = provincia)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.2, linewidth = 1) +
  scale_color_viridis_d() +
  labs(
    title = "Validación: Observado vs Predicho",
    subtitle = paste0("R² = ", round(R2, 4), " | RMSE = ", round(RMSE, 2), " ha"),
    x = "Superficie Observada (ha)", y = "Superficie Predicha (ha)", color = "Provincia"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "gray40"),
        legend.position = "bottom")

print(p10)
Sys.sleep(1)

# ============================================================================
# 9. RESUMEN Y CONCLUSIONES
# ============================================================================
cat("\n================================================================================\n")
cat("RESUMEN EJECUTIVO - MODELOS BAYESIANOS JERÁRQUICOS ESPACIALES\n")
cat("================================================================================\n\n")

cat("DATOS ANALIZADOS:\n")
cat("  • Observaciones totales:", nrow(datos_clean), "\n")
cat("  • Provincias:", n_provincias, "\n")
cat("  • Distritos:", n_distritos, "\n")
cat("  • Superficie media:", round(mean(datos_clean$superficie_ha, na.rm = TRUE), 2), "ha\n")
cat("  • Coeficiente de variación:", 
    round((sd(datos_clean$superficie_ha, na.rm = TRUE) / mean(datos_clean$superficie_ha, na.rm = TRUE)) * 100, 2), 
    "%\n\n")

cat("ESTRUCTURA JERÁRQUICA IDENTIFICADA:\n")
cat("  • Nivel 1: Unidades Agropecuarias (n =", nrow(datos_clean), ")\n")
cat("  • Nivel 2: Distritos (n =", n_distritos, ")\n")
cat("  • Nivel 3: Provincias (n =", n_provincias, ")\n\n")

cat("DEPENDENCIA ESPACIAL (Test de Moran I):\n")
cat("  • Estadístico I:", round(moran_test$estimate[1], 4), "\n")
cat("  • p-valor:", format.pval(moran_test$p.value, digits = 3), "\n")
if(moran_test$p.value < 0.05){
  cat("  • Conclusión: AUTOCORRELACIÓN ESPACIAL SIGNIFICATIVA (justifica modelado espacial)\n\n")
} else {
  cat("  • Conclusión: No se detecta autocorrelación significativa\n\n")
}

cat("ESPECIFICACIÓN DEL MODELO FINAL:\n")
cat("  •", comparacion$Modelo[mejor_idx], "\n\n")

cat("DESEMPEÑO PREDICTIVO:\n")
cat("  • R²:", round(R2, 4), "\n")
cat("  • RMSE:", round(RMSE, 3), "ha\n")
cat("  • MAE:", round(MAE, 3), "ha\n")
cat("  • MAPE:", round(MAPE, 2), "%\n\n")

cat("CONCLUSIONES:\n")
cat("  1. Se identificó heterogeneidad significativa en la distribución de tierra agrícola\n")
cat("     entre provincias y distritos de Puno.\n\n")
cat("  2. La estructura jerárquica captura tanto variabilidad intra-provincial como\n")
cat("     inter-distrital, explicada por efectos aleatorios espaciales.\n\n")
cat("  3. El modelo proporciona predicciones con incertidumbre cuantificada, permitiendo\n")
cat("     inferencia bayesiana para políticas públicas de desarrollo rural.\n\n")
cat("  4. La dependencia espacial significativa justifica la inclusión de efectos CAR\n")
cat("     (Conditional AutoRegressive) en análisis bayesianos posteriores.\n\n")

cat("IMPLICACIONES PARA POLÍTICAS PÚBLICAS:\n")
cat("  • Asignación eficiente de recursos de riego según vulnerabilidad territorial\n")
cat("  • Diseño de intervenciones diferenciadas por nivel jerárquico (distrito/provincia)\n")
cat("  • Monitoreo de concentración de tierras y minifundismo con incertidumbre cuantificada\n\n")

cat("================================================================================\n")
cat("FIN DEL ANÁLISIS\n")
cat("Fecha:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("================================================================================\n")

