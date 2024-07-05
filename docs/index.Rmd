---
title: "Estimativas Intervalares Proporção Gênero/Raça nos Eventos PDC 2013"
author: "Felipe Augusto Oliveira Rezende"
date: "2024-07-04"
output:
  ioslides_presentation:
---

<style>
/* Ajustar o tamanho do texto dos códigos R */
pre, code {
  font-size: 0.5em; /* Ajuste este valor conforme necessário */
}

/* Evitar que o texto dos códigos e resultados seja cortado */
pre, code {
  overflow-x: auto;
}

/* Ajustar o tamanho do conteúdo dos slides */
.slide-content {
  font-size: 0.7em; /* Ajuste este valor conforme necessário */
}

/* Ajustar o tamanho da fonte dos gráficos */
figure {
  font-size: 0.7em;
}
</style>

## Introdução

<div style="font-size: 16px;">
Nesta apresentação, serão apresentadas as estimativas intervalares para a proporção de gênero e raça nos eventos culturais do PDC 2013. O foco será analisar a paridade de gênero e raça nos eventos, considerando as proporções e seus intervalos de confiança.
</div>

```{r setup, include=FALSE}

knitr::opts_chunk$set(
  echo = TRUE,
  fig.width = 7,
  fig.height = 5,
  fig.retina = 1,
  warning = FALSE,
  message = FALSE,
  size = 'tiny'
)
```

```{r, echo=FALSE, include=FALSE}

library(xlsx)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rmarkdown)
library(xaringan)

read_excel("C:/Users/Felipe/Desktop/Doutorado IRel UnB/Fontes e Materiais de Pesquisa Empírica Doutorado/Base de Dados PDC 2013_MetQuanti_FelipeAugustoOliveiraRezende.xlsx")
PDC_2013 <- read_excel("C:/Users/Felipe/Desktop/Doutorado IRel UnB/Fontes e Materiais de Pesquisa Empírica Doutorado/Base de Dados PDC 2013_MetQuanti_FelipeAugustoOliveiraRezende.xlsx")

```

## Estimativas Intervalares

### Proporção de Gênero/Raça nos Eventos PDC 2013

<div style="font-size: 16px;">
Verificando a proporcionalidade de eventos de difusão cultural identificados pelos marcadores de gênero (PDC_2013_genero), raça (PDC_2013_raca) e raça & gênero (PDC_2013_gen_raca), em face ao número total de ações de difusão cultural PDC 2013 (n_total).
</div>

```{r}
PDC_2013_genero <- PDC_2013 %>% filter(`Paridade Gênero` == 1)
PDC_2013_raca <- PDC_2013 %>% filter(`Paridade Raça` == 1)
PDC_2013_gen_raca <- PDC_2013 %>% filter(`Paridade Gênero` == 1, `Paridade Raça` == 1)

n_total <- nrow(PDC_2013)
```
---
```{r}
prop_test_genero <- prop.test(n = n_total, x = nrow(PDC_2013_genero))
prop_test_raca <- prop.test(n = n_total, x = nrow(PDC_2013_raca))
prop_test_gen_raca <- prop.test(n = n_total, x = nrow(PDC_2013_gen_raca))

props <- c(prop_test_genero$estimate, prop_test_raca$estimate, prop_test_gen_raca$estimate)
ci_lowers <- c(prop_test_genero$conf.int[1], prop_test_raca$conf.int[1], prop_test_gen_raca$conf.int[1])
ci_uppers <- c(prop_test_genero$conf.int[2], prop_test_raca$conf.int[2], prop_test_gen_raca$conf.int[2])
```

```{r}
prop_df <- data.frame(
  Categoria = c("Paridade Gênero", "Paridade Raça", "Paridade Gênero e Raça"),
  Proporcao = props,
  CI_lower = ci_lowers,
  CI_upper = ci_uppers
)
```
---
### Gráfico de Estimativas Intervalares

```{r, echo=FALSE, include=TRUE}
ggplot(prop_df, aes(x = Categoria, y = Proporcao)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  ylim(0, 1) +
  labs(title = "Estimativas Intervalares / Intervalos de Confiança",
       x = "Categorias",
       y = "Proporção") +
  theme_minimal()
```

## Configuração de Hipóteses Testáveis

<div style="font-size: 16px;">
Hipótese Nula (H0): não há diferença significativa entre as proporções de paridade de gênero, paridade de raça e paridade de gênero e raça.

Hipótese Alternativa (H1): há pelo menos uma diferença significativa entre as proporções de paridade de gênero, paridade de raça e paridade de gênero e raça.
</div>
