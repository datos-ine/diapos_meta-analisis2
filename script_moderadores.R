
# Cargar paquetes ---------------------------------------------------------
# remotes::install_github("MathiasHarrer/dmetar")
# remotes::install_github("daniel1noble/orchaRd")

pacman::p_load(
  janitor,
  meta,
  metafor,
  orchaRd,
  metaviz,
  dmetar,
  tidyverse
)


# Cargar datos ------------------------------------------------------------
## Análisis moderadores
datos <- dat.crisafulli2020 |> 
  # Crear variable para el país
  mutate(pais_cat = if_else(
    country  == "IT", # Condición a testear
    "Italia",         # Valor si la condición se cumple
    "Otro/s" ))       # Valor si la condición no se cumple


## Modelos multinivel
datos_multi <- dat.bornmann2007 |> 
  # Crear identificador único a partir de número de fila
  rowid_to_column(var = "id")


##  Explorar datos
glimpse(datos)

glimpse(datos_multi)


# Análisis de moderadores -------------------------------------------------
## Análisis de subgrupos
mod_sub <- metaprop(
  event = cases,          # Casos observados
  n = total,              # Tamaño de la muestra
  studlab = study,        # Identificador del estudio
  data = datos,           # Conjunto de datos
  sm = "PLOGIT",          # Transformación logit
  common = FALSE,         # Omitir modelo de efectos fijos
  random = TRUE,          # Modelo de efectos aleatorios
  pscale = 100000,        # Escala a casos/100 000 habitantes
  subgroup = pais_cat     # Moderador categórico
)

# Salida del modelo
mod_sub

# Significancia
mod_sub$pval.Q.b.random

# Forest plot
forest(mod_sub, layout = "subgroup")


## Metarregresión
# Modelo sin moderadores
mod <- update(mod_sub, subgroup = NULL)

# Año de publicación como predictor
mod_year <- metareg(mod, 
                    formula = ~ pubyear,
                    intercept = TRUE)


# Salida del modelo
summary(mod_year)

# Bubble plot
bubble(
  mod_year,
  cex = (1/mod_year$vi * .01),  # Ajusta la escala de los símbolos
  col.line = "magenta"
)

## Metarregresión múltiple
reg1 <- metareg(mod, formula = ~ pubyear + pais_cat)

summary(reg1)


# Sesgo de publicación ---------------------------------------------------
# Test de Egger
metabias(mod)

# Test de Begg
metabias(mod, method.bias = "begg")

# Trim-and-fill
trimfill(mod)

# Funnel plot
funnel(mod)


# Modelos multinivel ------------------------------------------------------
## Modelo multinivel con meta
mod_multi <- metabin(
  event.e = waward,
  n.e = wtotal,
  event.c = maward,
  n.c = mtotal,
  data = datos, 
  sm = "OR",         
  common = FALSE,
  subgroup = type,
  studlab = id,
  cluster = study
) 

# Salida del modelo
mod_multi


## Modelo multinivel con metafor
# Calcular estimadores de efecto individuales
datos_multi <- escalc(measure = "OR",
                ai = waward,
                n1i = wtotal,
                ci = maward,
                n2i = mtotal,
                data = datos_multi,
                slab = study)

# Ajustar modelo
mod <- rma.mv(yi = yi,
              V = vi,
              data = datos_multi,
              slab = study,
              mods = ~ type + year,
              random = ~ 1|study/id,
              intercept = TRUE)

# Salida del modelo
summary(mod)

# Salida del modelo (orchaRd)
mod_results(mod, group = "study", mod = "type")

# Forest plot (orchaRd)
orchard_plot(mod, 
             group = "study",
             mod = "type",
             xlab = "log-OR") +
  scale_fill_viridis_d()

# Bubble plot
bubble_plot(mod, 
            group = "study",
            mod = "year",
            xlab = "Año de publicación",
            by = "type")

# Funnel plot (metaviz)
viz_funnel(x = datos_multi |> select(yi, vi),
           group = datos$type,
           contours_col = "Purples") +
  # Cambiar colores de los puntos
  scale_color_viridis_d()

# Leave-one-out (orchaRd)
leave1 <- leave_one_out(mod, group = "study")

orchard_leave1out(leave1out = leave1,    
                  xlab = "log(OR)"))

# I-cuadrado (orchaRd)
i2_ml(mod)

# Gráfico de la varianza (dmetar)
mlm.variance.distribution(mod) |> 
  plot()

# Bondad de ajuste (orchaRd)
r2_ml(mod)