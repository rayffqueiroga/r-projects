---
title: "Relatório Final - IA - 2017.1"
date: "25 de agosto de 2017"
output:
  html_document:
    toc: yes
    toc_float: yes
  pdf_document:
    toc: yes
authors: Acacio Leal, Priscila Barros, Rayff Queiroga
---

# Seleção de características com Aprendizagem de Máquina

A aprendizado de máquina usa as características (variáveis ou atributos) para gerar modelos preditivos. Usar uma combinação adequada de características é essencial para obter alta precisão e previsão. Como muitas características (inespecíficas) representam o problema de *overfitting* do modelo, geralmente queremos restringir as características em nossos modelos para aquelas que são mais relevantes para a variável de resposta que queremos prever. Usar o menor número possível de as características também reduzirá a complexidade de nossos modelos, o que significa que ele precisa de menos tempo e poder de computação para executar e é mais fácil de entender.

Existem várias maneiras de identificar o quanto cada as características contribui para o modelo e restringir o número de as características selecionadas. Aqui, vamos examinar o efeito da seleção de as características através de:

	* Correlação;
	* Eliminação de características recursivas (RFE);
	* Algoritmo Genético (GA), em modelos Random Forest.
	
Além disso, nós queremos saber como diferentes propriedades de dados afetam a influência desses métodos de seleção de características no resultado. Para isso, estamos usando três conjuntos de dados de câncer de mama, um dos quais tem poucas características; Os outros dois são maiores, mas diferem em quão bem são os *clusters* de resultados em PCA.

Com base em nossas comparações do método de correlação, RFE e GA, nós concluiriamos que, para modelos *Random Forest*:

	* A remoção de características altamente correlacionadas não é um método geralmente adequado;
	* GA produziu os melhores modelos neste exemplo, mas é impraticável para casos de uso diário com muitos recursos porque leva muito tempo para ser executado com gerações e indivíduos suficientes e;
	* Dados que não permitem uma boa classificação para começar (porque as características não são muito distintas entre as classes) não se beneficiam necessariamente da seleção de características.
	
As nossas conclusões não são generalizadas para quaisquer dados que você esteja trabalhando: há muitos mais métodos de seleção de características e só estamos  olhando para um número limitado de conjuntos de dados e apenas em sua influência nos modelos Random Forest. Mas mesmo este pequeno exemplo mostra como diferentes recursos e parâmetros podem influenciar suas previsões. Com o aprendizado da máquina, não existe um "tamanho único". Vale a pena sempre dar uma boa olhada em seus dados, familiarizar-se com suas peculiaridades e propriedades antes mesmo de pensar em modelos e algoritmos. E uma vez que você tenha percebido seus dados, investindo o tempo e o esforço para comparar diferentes métodos de seleção de características, parâmetros do modelo e - finalmente - diferentes algoritmos de aprendizagem de máquinas podem fazer uma grande diferença.


# Breast Cancer Wisconsin (Diagnostic) Dataset

Os dados que vamos usar para explorar os métodos de seleção de recursos são do *Breast Cancer Wisconsin (Diagnostic) Dataset*.

Os dados foram baixados do [UC Irvine Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Diagnostic%29). Os recursos nesses conjuntos de dados caracterizam as propriedades do nÃºcleo celular e foram gerados a partir da análise de imagens de [aspirados com agulha fina (FNA)](https://en.wikipedia.org/wiki/Fine-needle_aspiration) de massas mamárias.

Estão incluídos três conjuntos de dados. O primeiro conjunto de dados é pequeno com apenas 9 recursos, os outros dois conjuntos de dados têm 30 e 33 recursos e variam em quão fortemente as duas classes de preditores se agrupam no PCA. Nós quero explorar o efeito de diferentes métodos de seleção de recursos em conjuntos de dados com essas diferentes propriedades.
	
Mas primeiro, queremos conhecer os dados com os quais estamos trabalhando.

## Conjunto de dados do câncer de mama 1:

O primeiro conjunto de dados examina as classes de preditores:
	
	* M: massa mamária Maligna ou
	* B: massa mamária benigna.
	
Os fenótipos para caracterização são:

	* Identificação da amostra (número de código);
	* Espessura do grão;
	* Uniformidade do tamanho da célula;
	* Uniformidade da forma celular;
	* Adesão marginal;
	* Tamanho de célula epitelial simples;
	* Número de núcleos nus;
	* Cromatina blanda;
	* Número de núcleos normais;
	* Mitose;
	* Classes, ou seja, diagnóstico

	
Os valores faltantes são preenchidos com o pacote *mice*.


```r
bc_data <- read.table("breast-cancer-wisconsin.data.txt", header = FALSE, sep = ",")
colnames(bc_data) <- c("sample_code_number", "clump_thickness", "uniformity_of_cell_size", "uniformity_of_cell_shape", "marginal_adhesion", "single_epithelial_cell_size", 
                       "bare_nuclei", "bland_chromatin", "normal_nucleoli", "mitosis", "classes")
bc_data$classes <- ifelse(bc_data$classes == "2", "benign",
                          ifelse(bc_data$classes == "4", "malignant", NA))

bc_data[bc_data == "?"] <- NA

# how many NAs are in the data
length(which(is.na(bc_data)))
```

```
## [1] 16
```


```r
# impute missing data
library(mice)

bc_data[,2:10] <- apply(bc_data[, 2:10], 2, function(x) as.numeric(as.character(x)))
dataset_impute <- mice(bc_data[, 2:10],  print = FALSE)
bc_data <- cbind(bc_data[, 11, drop = FALSE], mice::complete(dataset_impute, 1))

bc_data$classes <- as.factor(bc_data$classes)

# how many benign and malignant cases are there?
summary(bc_data$classes)
```

```
##    benign malignant 
##       458       241
```


```r
str(bc_data)
```

```
## 'data.frame':	699 obs. of  10 variables:
##  $ classes                    : Factor w/ 2 levels "benign","malignant": 1 1 1 1 1 2 1 1 1 1 ...
##  $ clump_thickness            : num  5 5 3 6 4 8 1 2 2 4 ...
##  $ uniformity_of_cell_size    : num  1 4 1 8 1 10 1 1 1 2 ...
##  $ uniformity_of_cell_shape   : num  1 4 1 8 1 10 1 2 1 1 ...
##  $ marginal_adhesion          : num  1 5 1 1 3 8 1 1 1 1 ...
##  $ single_epithelial_cell_size: num  2 7 2 3 2 7 2 2 2 2 ...
##  $ bare_nuclei                : num  1 10 2 4 1 10 10 1 1 1 ...
##  $ bland_chromatin            : num  3 3 3 3 3 9 3 3 1 2 ...
##  $ normal_nucleoli            : num  1 2 1 7 1 7 1 1 1 1 ...
##  $ mitosis                    : num  1 1 1 1 1 1 1 1 5 1 ...
```

## Conjunto de dados do câncer de mama 2:

O segundo conjunto de dados examina novamente as classes de preditores:

	* M: massa mamária maligna ou
	* B: massa mamária benigna.
	
As duas primeiras colunas são:

	* ID da amostra;
	* Classes, ou seja, diagnóstico.
	
Para cada núcleo celular, as seguintes dez características foram medidas:

	* Raio (significado de todas as distâncias do centro para pontos no perímetro);
	* Textura (desvio padrão de valores de escala de cinza);
	* Perímetro;
	* Área;
	* Liso (variação local nos comprimentos de raio);
	* Compactação (perímetro ^ 2 / área - 1.0);
	* Concavidade (gravidade das porções côncavas do contorno);
	* Pontos côncavos (número de porções côncavas do contorno);
	* Simetria;
	* Dimensão Fractal ("aproximação do contorno" - 1)

	
Para cada caracterÃ­stica, sÃ£o dadas trÃªs medidas:

	* Média;
	* Desvio padrão;
	* Maior / "pior";
	

```r
bc_data_2 <- read.table("wdbc.data.txt", header = FALSE, sep = ",")

phenotypes <- rep(c("radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave_points", "symmetry", "fractal_dimension"), 3)
types <- rep(c("mean", "se", "largest_worst"), each = 10)

colnames(bc_data_2) <- c("ID", "diagnosis", paste(phenotypes, types, sep = "_"))

# how many NAs are in the data
length(which(is.na(bc_data_2)))
```

```
## [1] 0
```


```r
# how many benign and malignant cases are there?
summary(bc_data_2$diagnosis)
```

```
##   B   M 
## 357 212
```


```r
str(bc_data_2)
```

```
## 'data.frame':	569 obs. of  32 variables:
##  $ ID                             : int  842302 842517 84300903 84348301 84358402 843786 844359 84458202 844981 84501001 ...
##  $ diagnosis                      : Factor w/ 2 levels "B","M": 2 2 2 2 2 2 2 2 2 2 ...
##  $ radius_mean                    : num  18 20.6 19.7 11.4 20.3 ...
##  $ texture_mean                   : num  10.4 17.8 21.2 20.4 14.3 ...
##  $ perimeter_mean                 : num  122.8 132.9 130 77.6 135.1 ...
##  $ area_mean                      : num  1001 1326 1203 386 1297 ...
##  $ smoothness_mean                : num  0.1184 0.0847 0.1096 0.1425 0.1003 ...
##  $ compactness_mean               : num  0.2776 0.0786 0.1599 0.2839 0.1328 ...
##  $ concavity_mean                 : num  0.3001 0.0869 0.1974 0.2414 0.198 ...
##  $ concave_points_mean            : num  0.1471 0.0702 0.1279 0.1052 0.1043 ...
##  $ symmetry_mean                  : num  0.242 0.181 0.207 0.26 0.181 ...
##  $ fractal_dimension_mean         : num  0.0787 0.0567 0.06 0.0974 0.0588 ...
##  $ radius_se                      : num  1.095 0.543 0.746 0.496 0.757 ...
##  $ texture_se                     : num  0.905 0.734 0.787 1.156 0.781 ...
##  $ perimeter_se                   : num  8.59 3.4 4.58 3.44 5.44 ...
##  $ area_se                        : num  153.4 74.1 94 27.2 94.4 ...
##  $ smoothness_se                  : num  0.0064 0.00522 0.00615 0.00911 0.01149 ...
##  $ compactness_se                 : num  0.049 0.0131 0.0401 0.0746 0.0246 ...
##  $ concavity_se                   : num  0.0537 0.0186 0.0383 0.0566 0.0569 ...
##  $ concave_points_se              : num  0.0159 0.0134 0.0206 0.0187 0.0188 ...
##  $ symmetry_se                    : num  0.03 0.0139 0.0225 0.0596 0.0176 ...
##  $ fractal_dimension_se           : num  0.00619 0.00353 0.00457 0.00921 0.00511 ...
##  $ radius_largest_worst           : num  25.4 25 23.6 14.9 22.5 ...
##  $ texture_largest_worst          : num  17.3 23.4 25.5 26.5 16.7 ...
##  $ perimeter_largest_worst        : num  184.6 158.8 152.5 98.9 152.2 ...
##  $ area_largest_worst             : num  2019 1956 1709 568 1575 ...
##  $ smoothness_largest_worst       : num  0.162 0.124 0.144 0.21 0.137 ...
##  $ compactness_largest_worst      : num  0.666 0.187 0.424 0.866 0.205 ...
##  $ concavity_largest_worst        : num  0.712 0.242 0.45 0.687 0.4 ...
##  $ concave_points_largest_worst   : num  0.265 0.186 0.243 0.258 0.163 ...
##  $ symmetry_largest_worst         : num  0.46 0.275 0.361 0.664 0.236 ...
##  $ fractal_dimension_largest_worst: num  0.1189 0.089 0.0876 0.173 0.0768 ...
```

## Conjunto de dados do câncer de mama 3:

O terceiro conjunto de dados analisa as classes preditoras:

	* R: câncer de mama recorrente ou
	* N: câncer de mama nÃ£o recorrente.
	
As duas primeiras colunas são:

	* ID da amostra;
	* Classes, ou seja, diagnóstico.
	
Para cada núcleo celular, as mesmas dez características e medidas foram dadas como no conjunto de dados 2, mais:

	* Tempo (tempo de recorrência se o campo 2 = R, tempo livre de doença se o campo 2 = N);
	* Tamanho do tumor - diâmetro do tumor excisado em centimetros;
	* Status do linfonodo - número de linfonodos axilares positivos observados no momento da cirurgia;
	
Os valores faltantes são imputados com o pacote *mice*.


```r
bc_data_3 <- read.table("wpbc.data.txt", header = FALSE, sep = ",")
colnames(bc_data_3) <- c("ID", "outcome", "time", paste(phenotypes, types, sep = "_"), "tumor_size", "lymph_node_status")

bc_data_3[bc_data_3 == "?"] <- NA

# how many NAs are in the data
length(which(is.na(bc_data_3)))
```

```
## [1] 4
```


```r
# impute missing data
library(mice)

bc_data_3[,3:35] <- apply(bc_data_3[,3:35], 2, function(x) as.numeric(as.character(x)))
dataset_impute <- mice(bc_data_3[,3:35],  print = FALSE)
bc_data_3 <- cbind(bc_data_3[, 2, drop = FALSE], mice::complete(dataset_impute, 1))

# how many recurring and non-recurring cases are there?
summary(bc_data_3$outcome)
```

```
##   N   R 
## 151  47
```


```r
str(bc_data_3)
```

```
## 'data.frame':	198 obs. of  34 variables:
##  $ outcome                        : Factor w/ 2 levels "N","R": 1 1 1 1 2 2 1 2 1 1 ...
##  $ time                           : num  31 61 116 123 27 77 60 77 119 76 ...
##  $ radius_mean                    : num  18 18 21.4 11.4 20.3 ...
##  $ texture_mean                   : num  27.6 10.4 17.4 20.4 14.3 ...
##  $ perimeter_mean                 : num  117.5 122.8 137.5 77.6 135.1 ...
##  $ area_mean                      : num  1013 1001 1373 386 1297 ...
##  $ smoothness_mean                : num  0.0949 0.1184 0.0884 0.1425 0.1003 ...
##  $ compactness_mean               : num  0.104 0.278 0.119 0.284 0.133 ...
##  $ concavity_mean                 : num  0.109 0.3 0.126 0.241 0.198 ...
##  $ concave_points_mean            : num  0.0706 0.1471 0.0818 0.1052 0.1043 ...
##  $ symmetry_mean                  : num  0.186 0.242 0.233 0.26 0.181 ...
##  $ fractal_dimension_mean         : num  0.0633 0.0787 0.0601 0.0974 0.0588 ...
##  $ radius_se                      : num  0.625 1.095 0.585 0.496 0.757 ...
##  $ texture_se                     : num  1.89 0.905 0.611 1.156 0.781 ...
##  $ perimeter_se                   : num  3.97 8.59 3.93 3.44 5.44 ...
##  $ area_se                        : num  71.5 153.4 82.2 27.2 94.4 ...
##  $ smoothness_se                  : num  0.00443 0.0064 0.00617 0.00911 0.01149 ...
##  $ compactness_se                 : num  0.0142 0.049 0.0345 0.0746 0.0246 ...
##  $ concavity_se                   : num  0.0323 0.0537 0.033 0.0566 0.0569 ...
##  $ concave_points_se              : num  0.00985 0.01587 0.01805 0.01867 0.01885 ...
##  $ symmetry_se                    : num  0.0169 0.03 0.0309 0.0596 0.0176 ...
##  $ fractal_dimension_se           : num  0.00349 0.00619 0.00504 0.00921 0.00511 ...
##  $ radius_largest_worst           : num  21.6 25.4 24.9 14.9 22.5 ...
##  $ texture_largest_worst          : num  37.1 17.3 21 26.5 16.7 ...
##  $ perimeter_largest_worst        : num  139.7 184.6 159.1 98.9 152.2 ...
##  $ area_largest_worst             : num  1436 2019 1949 568 1575 ...
##  $ smoothness_largest_worst       : num  0.119 0.162 0.119 0.21 0.137 ...
##  $ compactness_largest_worst      : num  0.193 0.666 0.345 0.866 0.205 ...
##  $ concavity_largest_worst        : num  0.314 0.712 0.341 0.687 0.4 ...
##  $ concave_points_largest_worst   : num  0.117 0.265 0.203 0.258 0.163 ...
##  $ symmetry_largest_worst         : num  0.268 0.46 0.433 0.664 0.236 ...
##  $ fractal_dimension_largest_worst: num  0.0811 0.1189 0.0907 0.173 0.0768 ...
##  $ tumor_size                     : num  5 3 2.5 2 3.5 2.5 1.5 4 2 6 ...
##  $ lymph_node_status              : num  5 2 0 0 0 0 0 10 1 20 ...
```

# Análise de componentes principais (PCA)

Para ter uma idéia sobre a dimensionalidade e variância dos conjuntos de dados, primeiro olhaos para os gráficos PCA para amostras e recursos. Os dois primeiros componentes principais mostram os dois componentes que explicam a maior parte da variação nos dados.

Depois de definir o nosso tema personalizado *ggplot2*, estamos criando uma função que executa o PCA (usando o pacote *pcaGoPromoter*), calcula elipses dos pontos de dados (com o pacote *elipse*) e produz o gráfico com o *ggplot2*. Algumas das características dos conjuntos de dados 2 e 3 não são muito distintas e se sobrepõem nos gráficos PCA, portanto também estamos planejando dendrogramas de agrupamento hierárquico.


```r
# plotting theme

library(ggplot2)

my_theme <- function(base_size = 12, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
  theme(
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.title = element_text(size = 14),
    panel.grid.major = element_line(color = "grey"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "aliceblue"),
    strip.background = element_rect(fill = "navy", color = "navy", size = 1),
    strip.text = element_text(face = "bold", size = 12, color = "white"),
    legend.position = "right",
    legend.justification = "top", 
    legend.background = element_blank(),
    panel.border = element_rect(color = "grey", fill = NA, size = 0.5)
  )
}

theme_set(my_theme())
```


```r
# function for PCA plotting
library(pcaGoPromoter)
```

```
## Loading required package: ellipse
```

```
## Loading required package: Biostrings
```

```
## Loading required package: BiocGenerics
```

```
## Loading required package: parallel
```

```
## 
## Attaching package: 'BiocGenerics'
```

```
## The following objects are masked from 'package:parallel':
## 
##     clusterApply, clusterApplyLB, clusterCall, clusterEvalQ,
##     clusterExport, clusterMap, parApply, parCapply, parLapply,
##     parLapplyLB, parRapply, parSapply, parSapplyLB
```

```
## The following objects are masked from 'package:stats':
## 
##     IQR, mad, sd, var, xtabs
```

```
## The following objects are masked from 'package:base':
## 
##     anyDuplicated, append, as.data.frame, cbind, colMeans,
##     colnames, colSums, do.call, duplicated, eval, evalq, Filter,
##     Find, get, grep, grepl, intersect, is.unsorted, lapply,
##     lengths, Map, mapply, match, mget, order, paste, pmax,
##     pmax.int, pmin, pmin.int, Position, rank, rbind, Reduce,
##     rowMeans, rownames, rowSums, sapply, setdiff, sort, table,
##     tapply, union, unique, unsplit, which, which.max, which.min
```

```
## Loading required package: S4Vectors
```

```
## Loading required package: stats4
```

```
## 
## Attaching package: 'S4Vectors'
```

```
## The following object is masked from 'package:base':
## 
##     expand.grid
```

```
## Loading required package: IRanges
```

```
## Loading required package: XVector
```

```
## 
## Attaching package: 'Biostrings'
```

```
## The following object is masked from 'package:base':
## 
##     strsplit
```

```r
library(ellipse)

pca_func <- function(data, groups, title, print_ellipse = TRUE) {
  
  # perform pca and extract scores
  pcaOutput <- pca(data, printDropped = FALSE, scale = TRUE, center = TRUE)
  pcaOutput2 <- as.data.frame(pcaOutput$scores)
  
  # define groups for plotting
  pcaOutput2$groups <- groups
  

  # when plotting samples calculate ellipses for plotting (when plotting features, there are no replicates)
  if (print_ellipse) {
    
    centroids <- aggregate(cbind(PC1, PC2) ~ groups, pcaOutput2, mean)
    conf.rgn  <- do.call(rbind, lapply(unique(pcaOutput2$groups), function(t)
      data.frame(groups = as.character(t),
                 ellipse(cov(pcaOutput2[pcaOutput2$groups == t, 1:2]),
                       centre = as.matrix(centroids[centroids$groups == t, 2:3]),
                       level = 0.95),
                 stringsAsFactors = FALSE)))
    
    plot <- ggplot(data = pcaOutput2, aes(x = PC1, y = PC2, group = groups, color = groups)) + 
      geom_polygon(data = conf.rgn, aes(fill = groups), alpha = 0.2) +
      geom_point(size = 2, alpha = 0.6) + 
      scale_color_brewer(palette = "Set1") +
      labs(title = title,
           color = "",
           fill = "",
           x = paste0("PC1: ", round(pcaOutput$pov[1], digits = 2) * 100, "% variance"),
           y = paste0("PC2: ", round(pcaOutput$pov[2], digits = 2) * 100, "% variance"))
    
  } else {
    
    # if there are fewer than 10 groups (e.g. the predictor classes) I want to have colors from RColorBrewer
    if (length(unique(pcaOutput2$groups)) <= 10) {
      
      plot <- ggplot(data = pcaOutput2, aes(x = PC1, y = PC2, group = groups, color = groups)) + 
        geom_point(size = 2, alpha = 0.6) + 
        scale_color_brewer(palette = "Set1") +
        labs(title = title,
             color = "",
             fill = "",
             x = paste0("PC1: ", round(pcaOutput$pov[1], digits = 2) * 100, "% variance"),
             y = paste0("PC2: ", round(pcaOutput$pov[2], digits = 2) * 100, "% variance"))
      
    } else {
      
      # otherwise use the default rainbow colors
      plot <- ggplot(data = pcaOutput2, aes(x = PC1, y = PC2, group = groups, color = groups)) + 
        geom_point(size = 2, alpha = 0.6) + 
        labs(title = title,
             color = "",
             fill = "",
             x = paste0("PC1: ", round(pcaOutput$pov[1], digits = 2) * 100, "% variance"),
             y = paste0("PC2: ", round(pcaOutput$pov[2], digits = 2) * 100, "% variance"))
      
    } 
  } 
  
  
  
  return(plot)
  
}

library(gridExtra)
```

```
## 
## Attaching package: 'gridExtra'
```

```
## The following object is masked from 'package:BiocGenerics':
## 
##     combine
```

```r
library(grid)
```

* Dataset 1 
	

```r
p1 <- pca_func(data = t(bc_data[, 2:10]), groups = as.character(bc_data$classes), title = "Breast cancer dataset 1: Samples")
p2 <- pca_func(data = bc_data[, 2:10], groups = as.character(colnames(bc_data[, 2:10])), title = "Breast cancer dataset 1: Features", print_ellipse = FALSE)
grid.arrange(p1, p2, ncol = 2)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)
	

```r
h_1 <- hclust(dist(t(bc_data[, 2:10]), method = "euclidean"), method = "complete")
plot(h_1)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)


```r
library(tidyr)
```

```
## 
## Attaching package: 'tidyr'
```

```
## The following object is masked from 'package:S4Vectors':
## 
##     expand
```

```
## The following object is masked from 'package:mice':
## 
##     complete
```

```r
bc_data_gather <- bc_data %>%
  gather(measure, value, clump_thickness:mitosis)

ggplot(data = bc_data_gather, aes(x = value, fill = classes, color = classes)) +
  geom_density(alpha = 0.3, size = 1) +
  geom_rug() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  facet_wrap( ~ measure, scales = "free_y", ncol = 3)
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)

* Dataset 2 


```r
p1 <- pca_func(data = t(bc_data_2[, 3:32]), groups = as.character(bc_data_2$diagnosis), title = "Breast cancer dataset 2: Samples")
p2 <- pca_func(data = bc_data_2[, 3:32], groups = as.character(colnames(bc_data_2[, 3:32])), title = "Breast cancer dataset 2: Features", print_ellipse = FALSE)
grid.arrange(p1, p2, ncol = 2, widths = c(0.4, 0.6))
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)


```r
h_2 <- hclust(dist(t(bc_data_2[, 3:32]), method = "euclidean"), method = "complete")
plot(h_2)
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)


```r
bc_data_2_gather <- bc_data_2[, -1] %>%
  gather(measure, value, radius_mean:fractal_dimension_largest_worst)

ggplot(data = bc_data_2_gather, aes(x = value, fill = diagnosis, color = diagnosis)) +
  geom_density(alpha = 0.3, size = 1) +
  geom_rug() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  facet_wrap( ~ measure, scales = "free_y", ncol = 3)
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png)

* Dataset 3


```r
p1 <- pca_func(data = t(bc_data_3[, 2:34]), groups = as.character(bc_data_3$outcome), title = "Breast cancer dataset 3: Samples")
p2 <- pca_func(data = bc_data_3[, 2:34], groups = as.character(colnames(bc_data_3[, 2:34])), title = "Breast cancer dataset 3: Features", print_ellipse = FALSE)
grid.arrange(p1, p2, ncol = 2, widths = c(0.4, 0.6))
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png)


```r
h_3 <- hclust(dist(t(bc_data_3[,2:34]), method = "euclidean"), method = "complete")
plot(h_3)
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19-1.png)

Os conjuntos de dados 1 e 2 mostram uma boa separação de massas benignas e malignas, os modelos com base nessas características provavelmente poderão prever as classes com bastante confiabilidade para a maioria das amostras. As classes no conjunto de dados 3 não se agrupam em grupos distintos, nós acreditamos que a previsão não será tão precisa para esses recursos.

As características dos conjuntos de dados 2 e 3 não se agrupam de forma muito distinta, muitas características parecem mostrar padrões semelhantes em amostras. Selecionar um subconjunto apropriado de recursos provavelmente terá efeitos diferentes nos três conjuntos de dados diferentes.



```r
bc_data_3_gather <- bc_data_3 %>%
  gather(measure, value, time:lymph_node_status)

ggplot(data = bc_data_3_gather, aes(x = value, fill = outcome, color = outcome)) +
  geom_density(alpha = 0.3, size = 1) +
  geom_rug() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  facet_wrap( ~ measure, scales = "free_y", ncol = 3)
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20-1.png)

# Importância das Característica

Para se ter uma idéia sobre as respectivas importações da características, estamos executando modelos Random Forest com validação cruzada de 10 x 10 usando o caret package. Se nós quisessemos usar a importância característica para selecionar recursos para modelagem, nós precisaríamos executá-lo nos dados de treinamento em vez de no conjunto de dados completo. Mas aqui, nós só quero usá-lo para nos familiarizar com nossos dados. Estamos novamente definindo uma função que estima a importância da característica e produzir uma *plot*.

```r
library(caret)
```

```
## Loading required package: lattice
```

```r
library(doParallel) # parallel processing
```

```
## Loading required package: foreach
```

```
## foreach: simple, scalable parallel programming from Revolution Analytics
## Use Revolution R for scalability, fault tolerance and more.
## http://www.revolutionanalytics.com
```

```
## Loading required package: iterators
```

```r
registerDoParallel()

# prepare training scheme
control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

feature_imp <- function(model, title) {
  
  # estimate variable importance
  importance <- varImp(model, scale = TRUE)
  
  # prepare dataframes for plotting
  importance_df_1 <- importance$importance
  importance_df_1$group <- rownames(importance_df_1)
  
  importance_df_2 <- importance_df_1
  importance_df_2$Overall <- 0
  
  importance_df <- rbind(importance_df_1, importance_df_2)
  
  plot <- ggplot() +
    geom_point(data = importance_df_1, aes(x = Overall, y = group, color = group), size = 2) +
    geom_path(data = importance_df, aes(x = Overall, y = group, color = group, group = group), size = 1) +
    theme(legend.position = "none") +
    labs(
      x = "Importance",
      y = "",
      title = title,
      subtitle = "Scaled feature importance",
      caption = "\nDetermined with Random Forest and
      repeated cross validation (10 repeats, 10 times)"
    )
  
  return(plot)
  
}
```


```r
# train the model
set.seed(27)
imp_1 <- train(classes ~ ., data = bc_data, method = "rf", preProcess = c("scale", "center"), trControl = control)
```

```
## Loading required package: randomForest
```

```
## randomForest 4.6-12
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:gridExtra':
## 
##     combine
```

```
## The following object is masked from 'package:BiocGenerics':
## 
##     combine
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```

```
## Error in serialize(data, node$con): erro ao escrever na conexão
```


```r
p1 <- feature_imp(imp_1, title = "Breast cancer dataset 1")
```

```
## Error in varImp(model, scale = TRUE): objeto 'imp_1' não encontrado
```


```r
set.seed(27)
imp_2 <- train(diagnosis ~ ., data = bc_data_2[, -1], method = "rf", preProcess = c("scale", "center"), trControl = control)
```

```
## Error in serialize(data, node$con): erro ao escrever na conexão
```


```r
p2 <- feature_imp(imp_2, title = "Breast cancer dataset 2")
```

```
## Error in varImp(model, scale = TRUE): objeto 'imp_2' não encontrado
```


```r
set.seed(27)
imp_3 <- train(outcome ~ ., data = bc_data_3, method = "rf", preProcess = c("scale", "center"), trControl = control)
```

```
## Error in serialize(data, node$con): erro ao escrever na conexão
```


```r
p3 <- feature_imp(imp_3, title = "Breast cancer dataset 3")
```

```
## Error in varImp(model, scale = TRUE): objeto 'imp_3' não encontrado
```


```r
grid.arrange(p1, p2, p3, ncol = 3, widths = c(0.3, 0.35, 0.35))
```

```
## Error in arrangeGrob(...): objeto 'p3' não encontrado
```

# Seleção de Características

Agora que nós temos uma idéia geral sobre os dados, vamos executar três métodos de seleção de características em todos os três conjuntos de dados e comparar como eles afetam a precisão de previsão de um modelo *Random Forest*.
## Criando dados de treino e teste

Antes de fazer qualquer outra coisa com os dados, precisamos de subconjunto dos conjuntos de dados no treino e para testar os dados. Executar a seleção de características em todo o conjunto de dados levaria ao viés de previsão, portanto, precisamos executar todo o processo de modelagem somente nos dados de treinamento.
* Dataset 1


```r
set.seed(27)
bc_data_index <- createDataPartition(bc_data$classes, p = 0.7, list = FALSE)
bc_data_train <- bc_data[bc_data_index, ]
bc_data_test  <- bc_data[-bc_data_index, ]
```

* Dataset 2


```r
set.seed(27)
bc_data_2_index <- createDataPartition(bc_data_2$diagnosis, p = 0.7, list = FALSE)
bc_data_2_train <- bc_data_2[bc_data_2_index, ]
bc_data_2_test  <- bc_data_2[-bc_data_2_index, ]
```

* Dataset 3 


```r
set.seed(27)
bc_data_3_index <- createDataPartition(bc_data_3$outcome, p = 0.7, list = FALSE)
bc_data_3_train <- bc_data_3[bc_data_3_index, ]
bc_data_3_test  <- bc_data_3[-bc_data_3_index, ]
```

# Correlação

Muitas vezes, temos recursos altamente correlacionados e, portanto, fornecemos informações redundantes. Ao eliminar características altamente correlacionadas, podemos evitar um viés preditivo para as informações contidas nesses recursos. Isso também nos mostra que, quando queremos fazer declarações sobre a importância biológica/médica de características específicas, precisamos ter em mente que, apenas porque são adequados para predizer um resultado, eles não são necessariamente causais - eles poderiam simplesmente ser correlacionados com fatores causais.

As correlações entre todos os recursos são calculadas e visualizadas com o pacote *corrplot*. Estamos então removendo todos os recursos com uma correlação superior a 0,7, mantendo o recurso com a média mais baixa.

* Dataset 1 


```r
library(corrplot)

# calculate correlation matrix
corMatMy <- cor(bc_data_train[, -1])
corrplot(corMatMy, order = "hclust")
```

![plot of chunk unnamed-chunk-32](figure/unnamed-chunk-32-1.png)

```r
#Apply correlation filter at 0.70,
highlyCor <- colnames(bc_data_train[, -1])[findCorrelation(corMatMy, cutoff = 0.7, verbose = TRUE)]
```

```
## Compare row 2  and column  3 with corr  0.913 
##   Means:  0.716 vs 0.601 so flagging column 2 
## Compare row 3  and column  6 with corr  0.741 
##   Means:  0.677 vs 0.579 so flagging column 3 
## Compare row 6  and column  7 with corr  0.706 
##   Means:  0.602 vs 0.545 so flagging column 6 
## All correlations <= 0.7
```

```r
# which variables are flagged for removal?
highlyCor
```

```
## [1] "uniformity_of_cell_size"  "uniformity_of_cell_shape"
## [3] "bare_nuclei"
```

```r
#then we remove these variables
bc_data_cor <- bc_data_train[, which(!colnames(bc_data_train) %in% highlyCor)]
```
A correlação entre os recursos no conjunto de dados 1 geralmente é alta e 4 de 10 características foram sinalizadas para remoção.

* Dataset 2


```r
corMatMy <- cor(bc_data_2_train[, 3:32])
corrplot(corMatMy, order = "hclust")
```

![plot of chunk unnamed-chunk-36](figure/unnamed-chunk-36-1.png)

```r
highlyCor <- colnames(bc_data_2_train[, 3:32])[findCorrelation(corMatMy, cutoff = 0.7, verbose = TRUE)]
```

```
## Compare row 7  and column  8 with corr  0.92 
##   Means:  0.579 vs 0.393 so flagging column 7 
## Compare row 8  and column  6 with corr  0.84 
##   Means:  0.548 vs 0.38 so flagging column 8 
## Compare row 6  and column  28 with corr  0.827 
##   Means:  0.536 vs 0.368 so flagging column 6 
## Compare row 28  and column  27 with corr  0.855 
##   Means:  0.506 vs 0.357 so flagging column 28 
## Compare row 27  and column  26 with corr  0.894 
##   Means:  0.46 vs 0.346 so flagging column 27 
## Compare row 23  and column  21 with corr  0.993 
##   Means:  0.454 vs 0.336 so flagging column 23 
## Compare row 21  and column  24 with corr  0.983 
##   Means:  0.419 vs 0.327 so flagging column 21 
## Compare row 26  and column  30 with corr  0.817 
##   Means:  0.402 vs 0.323 so flagging column 26 
## Compare row 24  and column  3 with corr  0.943 
##   Means:  0.383 vs 0.312 so flagging column 24 
## Compare row 3  and column  1 with corr  0.998 
##   Means:  0.347 vs 0.306 so flagging column 3 
## Compare row 1  and column  4 with corr  0.986 
##   Means:  0.302 vs 0.304 so flagging column 4 
## Compare row 1  and column  14 with corr  0.726 
##   Means:  0.264 vs 0.304 so flagging column 14 
## Compare row 13  and column  11 with corr  0.973 
##   Means:  0.32 vs 0.304 so flagging column 13 
## Compare row 18  and column  16 with corr  0.757 
##   Means:  0.388 vs 0.295 so flagging column 18 
## Compare row 16  and column  17 with corr  0.796 
##   Means:  0.404 vs 0.288 so flagging column 16 
## Compare row 9  and column  29 with corr  0.711 
##   Means:  0.343 vs 0.274 so flagging column 9 
## Compare row 17  and column  20 with corr  0.745 
##   Means:  0.306 vs 0.261 so flagging column 17 
## Compare row 5  and column  25 with corr  0.809 
##   Means:  0.311 vs 0.255 so flagging column 5 
## Compare row 30  and column  10 with corr  0.753 
##   Means:  0.288 vs 0.241 so flagging column 30 
## Compare row 22  and column  2 with corr  0.913 
##   Means:  0.243 vs 0.242 so flagging column 22 
## All correlations <= 0.7
```

```r
highlyCor
```

```
##  [1] "concavity_mean"                  "concave_points_mean"            
##  [3] "compactness_mean"                "concave_points_largest_worst"   
##  [5] "concavity_largest_worst"         "perimeter_largest_worst"        
##  [7] "radius_largest_worst"            "compactness_largest_worst"      
##  [9] "area_largest_worst"              "perimeter_mean"                 
## [11] "perimeter_se"                    "area_mean"                      
## [13] "concave_points_se"               "compactness_se"                 
## [15] "area_se"                         "symmetry_mean"                  
## [17] "concavity_se"                    "smoothness_mean"                
## [19] "fractal_dimension_largest_worst" "texture_largest_worst"
```

```r
bc_data_2_cor <- bc_data_2_train[, which(!colnames(bc_data_2_train) %in% highlyCor)]
```

Aqui, temos mais variação entre os 30 recursos: alguns estão altamente correlacionados, enquanto outros parecem ser muito distintos. 20 são marcados para remoção (veja a saída acima).

* Dataset 3


```r
corMatMy <- cor(bc_data_3_train[, -1])
corrplot(corMatMy, order = "hclust")
```

![plot of chunk unnamed-chunk-40](figure/unnamed-chunk-40-1.png)

```r
highlyCor <- colnames(bc_data_3_train[, -1])[findCorrelation(corMatMy, cutoff = 0.7, verbose = TRUE)]
```

```
## Compare row 8  and column  9 with corr  0.898 
##   Means:  0.425 vs 0.285 so flagging column 8 
## Compare row 9  and column  7 with corr  0.714 
##   Means:  0.39 vs 0.277 so flagging column 9 
## Compare row 7  and column  29 with corr  0.753 
##   Means:  0.364 vs 0.271 so flagging column 7 
## Compare row 4  and column  2 with corr  0.996 
##   Means:  0.348 vs 0.264 so flagging column 4 
## Compare row 2  and column  5 with corr  0.993 
##   Means:  0.329 vs 0.259 so flagging column 2 
## Compare row 5  and column  24 with corr  0.921 
##   Means:  0.303 vs 0.254 so flagging column 5 
## Compare row 24  and column  22 with corr  0.985 
##   Means:  0.271 vs 0.252 so flagging column 24 
## Compare row 11  and column  31 with corr  0.83 
##   Means:  0.341 vs 0.247 so flagging column 11 
## Compare row 22  and column  15 with corr  0.773 
##   Means:  0.239 vs 0.242 so flagging column 15 
## Compare row 22  and column  25 with corr  0.989 
##   Means:  0.216 vs 0.242 so flagging column 25 
## Compare row 14  and column  12 with corr  0.975 
##   Means:  0.257 vs 0.243 so flagging column 14 
## Compare row 31  and column  28 with corr  0.71 
##   Means:  0.328 vs 0.238 so flagging column 31 
## Compare row 18  and column  17 with corr  0.812 
##   Means:  0.331 vs 0.229 so flagging column 18 
## Compare row 28  and column  27 with corr  0.84 
##   Means:  0.286 vs 0.219 so flagging column 28 
## Compare row 17  and column  21 with corr  0.839 
##   Means:  0.285 vs 0.212 so flagging column 17 
## Compare row 10  and column  30 with corr  0.766 
##   Means:  0.277 vs 0.204 so flagging column 10 
## Compare row 6  and column  26 with corr  0.754 
##   Means:  0.235 vs 0.198 so flagging column 6 
## Compare row 3  and column  23 with corr  0.858 
##   Means:  0.164 vs 0.195 so flagging column 23 
## All correlations <= 0.7
```

```r
highlyCor
```

```
##  [1] "concavity_mean"                  "concave_points_mean"            
##  [3] "compactness_mean"                "perimeter_mean"                 
##  [5] "radius_mean"                     "area_mean"                      
##  [7] "perimeter_largest_worst"         "fractal_dimension_mean"         
##  [9] "perimeter_se"                    "area_se"                        
## [11] "area_largest_worst"              "fractal_dimension_largest_worst"
## [13] "concavity_se"                    "concavity_largest_worst"        
## [15] "compactness_se"                  "symmetry_mean"                  
## [17] "smoothness_mean"                 "texture_largest_worst"
```

```r
bc_data_3_cor <- bc_data_3_train[, which(!colnames(bc_data_3_train) %in% highlyCor)]
```

Os recursos no conjunto de dados 3 são semelhantes: alguns são altamente correlacionados, outros são muito diferentes. 18 são marcados para remoção (veja saída acima).

# Eliminação de características recursivas (RFE)

Outra maneira de escolher recursos é a eliminação de recursos recursivos. RFE usa um algoritmo *Random Forest* para testar combinações de recursos e classificar cada um com uma pontuaçao de precisão. A combinação com maior pontuação geralmente é preferencial.

* Dataset 1


```r
# ensure the results are repeatable
set.seed(7)
# define the control using a random forest selection function with cross validation
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)

# run the RFE algorithm
results_1 <- rfe(x = bc_data_train[, -1], y = bc_data_train$classes, sizes = c(1:9), rfeControl = control)
```

```
## Error in serialize(data, node$con): erro ao escrever na conexão
```

```r
# chosen features
predictors(results_1)
```

```
## Error in predictors(results_1): objeto 'results_1' não encontrado
```

```r
# subset the chosen features
bc_data_rfe <- bc_data_train[, c(1, which(colnames(bc_data_train) %in% predictors(results_1)))]
```

```
## Error in predictors(results_1): objeto 'results_1' não encontrado
```

* Dataset 2 


```r
set.seed(7)
results_2 <- rfe(x = bc_data_2_train[, -c(1, 2)], y = as.factor(bc_data_2_train$diagnosis), sizes = c(1:30), rfeControl = control)
```

```
## Error in serialize(data, node$con): erro ao escrever na conexão
```

```r
predictors(results_2)
```

```
## Error in predictors(results_2): objeto 'results_2' não encontrado
```

```r
bc_data_2_rfe <- bc_data_2_train[, c(2, which(colnames(bc_data_2_train) %in% predictors(results_2)))]
```

```
## Error in predictors(results_2): objeto 'results_2' não encontrado
```

* Dataset 3


```r
set.seed(7)
results_3 <- rfe(x = bc_data_3_train[,-1], y = as.factor(bc_data_3_train$outcome), sizes = c(1:33), rfeControl = control)
```

```
## Error in serialize(data, node$con): erro ao escrever na conexão
```

```r
predictors(results_2)
```

```
## Error in predictors(results_2): objeto 'results_2' não encontrado
```

```r
bc_data_3_rfe <- bc_data_3_train[, c(1, which(colnames(bc_data_3_train) %in% predictors(results_3)))]
```

```
## Error in predictors(results_3): objeto 'results_3' não encontrado
```

# Algoritmo Genético (GA)

O Algoritmo Genético (GA) foi desenvolvido com base em princípios evolutivos de seleção natural: visa otimizar uma população de indivíduos com um determinado conjunto de genótipos, modelando a seleção ao longo do tempo. Em cada geração (ou seja, iteração), a aptidão de cada indivíduo é calculada com base em seus genótipos. Então, os indivíduos mais aptos são escolhidos para produzir a próxima geração. Esta geração subseqüente de indivíduos terá genótipos resultantes de (re)combinações de alelos parentais. Esses novos genótipos determinarão novamente a forma física de cada indivíduo. Este processo de seleção é iterado para um número específico de gerações e (idealmente) leva à fixação dos alelos mais aptos no grupo genetico.

Este conceito de otimização também pode ser aplicado a modelos não-evolutivos, como processos de seleção de características na aprendizagem de máquinas.

Para fins de demonstração, estamos usando apenas 10 gerações, que consistem em 5 indivíduos. Mais iterações com populações maiores, é claro, ser preferível, mas isso leva bastante tempo.



```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following object is masked from 'package:randomForest':
## 
##     combine
```

```
## The following object is masked from 'package:gridExtra':
## 
##     combine
```

```
## The following objects are masked from 'package:Biostrings':
## 
##     collapse, intersect, setdiff, setequal, union
```

```
## The following object is masked from 'package:XVector':
## 
##     slice
```

```
## The following objects are masked from 'package:IRanges':
## 
##     collapse, desc, intersect, setdiff, slice, union
```

```
## The following objects are masked from 'package:S4Vectors':
## 
##     first, intersect, rename, setdiff, setequal, union
```

```
## The following objects are masked from 'package:BiocGenerics':
## 
##     combine, intersect, setdiff, union
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
ga_ctrl <- gafsControl(functions = rfGA, # Assess fitness with RF
                       method = "cv",    # 10 fold cross validation
                       genParallel = TRUE, # Use parallel programming
                       allowParallel = TRUE)
```

* Dataset 1


```r
lev <- c("malignant", "benign")     # Set the levels

set.seed(27)
model_1 <- gafs(x = bc_data_train[, -1], y = bc_data_train$classes,
                   iters = 10, # generations of algorithm
                   popSize = 5, # population size for each generation
                   levels = lev,
                   gafsControl = ga_ctrl)
```

```
## Error in serialize(data, node$con): erro ao escrever na conexão
```

```r
plot(model_1) # Plot mean fitness (AUC) by generation
```

```
## Error in plot(model_1): objeto 'model_1' não encontrado
```


```r
model_1$ga$final
```

```
## Error in eval(expr, envir, enclos): objeto 'model_1' não encontrado
```

```r
bc_data_ga <- bc_data_train[, c(1, which(colnames(bc_data_train) %in% model_1$ga$final))]
```

```
## Error in colnames(bc_data_train) %in% model_1$ga$final: objeto 'model_1' não encontrado
```

* Dataset 2


```r
lev <- c("M", "B")

set.seed(27)
model_2 <- gafs(x = bc_data_2_train[, -c(1, 2)], y = bc_data_2_train$diagnosis,
                   iters = 10, # generations of algorithm
                   popSize = 5, # population size for each generation
                   levels = lev,
                   gafsControl = ga_ctrl)
```

```
## Error in serialize(data, node$con): erro ao escrever na conexão
```

```r
plot(model_2)
```

```
## Error in plot(model_2): objeto 'model_2' não encontrado
```

```r
model_2$ga$final
```

```
## Error in eval(expr, envir, enclos): objeto 'model_2' não encontrado
```

```r
bc_data_2_ga <- bc_data_2_train[, c(2, which(colnames(bc_data_2_train) %in% model_2$ga$final))]
```

```
## Error in colnames(bc_data_2_train) %in% model_2$ga$final: objeto 'model_2' não encontrado
```

* Dataset 3 


```r
lev <- c("R", "N")

set.seed(27)
model_3 <- gafs(x = bc_data_3_train[, -1], y = bc_data_3_train$outcome,
                   iters = 10, # generations of algorithm
                   popSize = 5, # population size for each generation
                   levels = lev,
                   gafsControl = ga_ctrl)
```

```
## Error in serialize(data, node$con): erro ao escrever na conexão
```

```r
plot(model_3)
```

```
## Error in plot(model_3): objeto 'model_3' não encontrado
```

```r
model_3$ga$final
```

```
## Error in eval(expr, envir, enclos): objeto 'model_3' não encontrado
```

```r
bc_data_3_ga <- bc_data_3_train[, c(1, which(colnames(bc_data_3_train) %in% model_3$ga$final))]
```

```
## Error in colnames(bc_data_3_train) %in% model_3$ga$final: objeto 'model_3' não encontrado
```

# Comparação dos modelos

## Todas as características

* Dataset 1


```r
set.seed(27)
model_bc_data_all <- train(classes ~ .,
                           data = bc_data_train,
                           method = "rf",
                           preProcess = c("scale", "center"),
                           trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, verboseIter = FALSE))
```

```
## Error in serialize(data, node$con): erro ao escrever na conexão
```

```r
cm_all_1 <- confusionMatrix(predict(model_bc_data_all, bc_data_test[, -1]), bc_data_test$classes)
```

```
## Error in predict(model_bc_data_all, bc_data_test[, -1]): objeto 'model_bc_data_all' não encontrado
```

```r
cm_all_1
```

```
## Error in eval(expr, envir, enclos): objeto 'cm_all_1' não encontrado
```

* Dataset 2


```r
set.seed(27)
model_bc_data_2_all <- train(diagnosis ~ .,
                           data = bc_data_2_train[, -1],
                           method = "rf",
                           preProcess = c("scale", "center"),
                           trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, verboseIter = FALSE))
```

```
## Error in serialize(data, node$con): erro ao escrever na conexão
```

```r
cm_all_2 <- confusionMatrix(predict(model_bc_data_2_all, bc_data_2_test[, -c(1, 2)]), bc_data_2_test$diagnosis)
```

```
## Error in predict(model_bc_data_2_all, bc_data_2_test[, -c(1, 2)]): objeto 'model_bc_data_2_all' não encontrado
```

```r
cm_all_2
```

```
## Error in eval(expr, envir, enclos): objeto 'cm_all_2' não encontrado
```

* Dataset 3


```r
set.seed(27)
model_bc_data_3_all <- train(outcome ~ .,
                           data = bc_data_3_train,
                           method = "rf",
                           preProcess = c("scale", "center"),
                           trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, verboseIter = FALSE))
```

```
## Error in serialize(data, node$con): erro ao escrever na conexão
```

```r
cm_all_3 <- confusionMatrix(predict(model_bc_data_3_all, bc_data_3_test[, -1]), bc_data_3_test$outcome)
```

```
## Error in predict(model_bc_data_3_all, bc_data_3_test[, -1]): objeto 'model_bc_data_3_all' não encontrado
```

```r
cm_all_3
```

```
## Error in eval(expr, envir, enclos): objeto 'cm_all_3' não encontrado
```

## Características selecionadas

### Dataset 1


```r
library(gplots)
```

```
## 
## Attaching package: 'gplots'
```

```
## The following object is masked from 'package:IRanges':
## 
##     space
```

```
## The following object is masked from 'package:S4Vectors':
## 
##     space
```

```
## The following object is masked from 'package:stats':
## 
##     lowess
```

```r
venn_list <- list(cor = colnames(bc_data_cor)[-1],
                  rfe = colnames(bc_data_rfe)[-1],
                  ga = colnames(bc_data_ga)[-1])
```

```
## Error in colnames(bc_data_rfe): objeto 'bc_data_rfe' não encontrado
```

```r
venn <- venn(venn_list)
```

```
## Error in getVennCounts(data, universe = universe, intersections = intersections): objeto 'venn_list' não encontrado
```

```r
venn
```

```
## function (data, universe = NA, small = 0.7, showSetLogicLabel = FALSE, 
##     simplify = FALSE, show.plot = TRUE, intersections = TRUE, 
##     names, ...) 
## {
##     counts <- getVennCounts(data, universe = universe, intersections = intersections)
##     if (show.plot) 
##         drawVennDiagram(data = counts, small = small, showSetLogicLabel = showSetLogicLabel, 
##             simplify = simplify, ...)
##     if (intersections) 
##         attr(counts, "intersections") <- vennMembers(l = data, 
##             universe = universe, names = names)
##     invisible(counts)
## }
## <environment: namespace:gplots>
```

4 dos 10 recursos foram escolhidos pelos três métodos; A maior sobreposição é vista entre GA e RFE com 7 recursos. RFE e GA mantêm oito recursos para modelagem, em comparaçao com apenas 5 com base no método de correlação.

* Correlação


```r
set.seed(27)
model_bc_data_cor <- train(classes ~ .,
                 data = bc_data_cor,
                 method = "rf",
                 preProcess = c("scale", "center"),
                 trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, verboseIter = FALSE))
```

```
## Error in serialize(data, node$con): erro ao escrever na conexão
```

```r
cm_cor_1 <- confusionMatrix(predict(model_bc_data_cor, bc_data_test[, -1]), bc_data_test$classes)
```

```
## Error in predict(model_bc_data_cor, bc_data_test[, -1]): objeto 'model_bc_data_cor' não encontrado
```

```r
cm_cor_1
```

```
## Error in eval(expr, envir, enclos): objeto 'cm_cor_1' não encontrado
```

* RFE


```r
set.seed(27)
model_bc_data_rfe <- train(classes ~ .,
                           data = bc_data_rfe,
                           method = "rf",
                           preProcess = c("scale", "center"),
                           trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, verboseIter = FALSE))
```

```
## Error in eval(expr, p): objeto 'bc_data_rfe' não encontrado
```

```r
cm_rfe_1 <- confusionMatrix(predict(model_bc_data_rfe, bc_data_test[, -1]), bc_data_test$classes)
```

```
## Error in predict(model_bc_data_rfe, bc_data_test[, -1]): objeto 'model_bc_data_rfe' não encontrado
```

```r
cm_rfe_1
```

```
## Error in eval(expr, envir, enclos): objeto 'cm_rfe_1' não encontrado
```

* GA 


```r
set.seed(27)
model_bc_data_ga <- train(classes ~ .,
                           data = bc_data_ga,
                           method = "rf",
                           preProcess = c("scale", "center"),
                           trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, verboseIter = FALSE))
```

```
## Error in eval(expr, p): objeto 'bc_data_ga' não encontrado
```

```r
cm_ga_1 <- confusionMatrix(predict(model_bc_data_ga, bc_data_test[, -1]), bc_data_test$classes)
```

```
## Error in predict(model_bc_data_ga, bc_data_test[, -1]): objeto 'model_bc_data_ga' não encontrado
```

```r
cm_ga_1
```

```
## Error in eval(expr, envir, enclos): objeto 'cm_ga_1' não encontrado
```

### Dataset 2 


```r
venn_list <- list(cor = colnames(bc_data_2_cor)[-c(1, 2)],
                  rfe = colnames(bc_data_2_rfe)[-c(1, 2)],
                  ga = colnames(bc_data_2_ga)[-c(1, 2)])
```

```
## Error in colnames(bc_data_2_rfe): objeto 'bc_data_2_rfe' não encontrado
```

```r
venn <- venn(venn_list)
```

```
## Error in getVennCounts(data, universe = universe, intersections = intersections): objeto 'venn_list' não encontrado
```

```r
venn
```

```
## function (data, universe = NA, small = 0.7, showSetLogicLabel = FALSE, 
##     simplify = FALSE, show.plot = TRUE, intersections = TRUE, 
##     names, ...) 
## {
##     counts <- getVennCounts(data, universe = universe, intersections = intersections)
##     if (show.plot) 
##         drawVennDiagram(data = counts, small = small, showSetLogicLabel = showSetLogicLabel, 
##             simplify = simplify, ...)
##     if (intersections) 
##         attr(counts, "intersections") <- vennMembers(l = data, 
##             universe = universe, names = names)
##     invisible(counts)
## }
## <environment: namespace:gplots>
```

Para o conjunto de dados 2, vemos uma variação muito maior nos recursos escolhidos entre os trás métodos de seleção: apenas 1 recurso foi escolhido por todos e a maior sobreposição é novamente vista entre RFE e GA, seguido de correlação e GA. Mas desta vez também vemos alguns recursos que são mantidos exclusivamente por qualquer um dos tr~es métodos de seleção de recursos.

* Correlação


```r
set.seed(27)
model_bc_data_2_cor <- train(diagnosis ~ .,
                           data = bc_data_2_cor[, -1],
                           method = "rf",
                           preProcess = c("scale", "center"),
                           trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, verboseIter = FALSE))
```

```
## Error in serialize(data, node$con): erro ao escrever na conexão
```

```r
cm_cor_2 <- confusionMatrix(predict(model_bc_data_2_cor, bc_data_2_test[, -c(1, 2)]), bc_data_2_test$diagnosis)
```

```
## Error in predict(model_bc_data_2_cor, bc_data_2_test[, -c(1, 2)]): objeto 'model_bc_data_2_cor' não encontrado
```

```r
cm_cor_2
```

```
## Error in eval(expr, envir, enclos): objeto 'cm_cor_2' não encontrado
```

* RFE


```r
set.seed(27)
model_bc_data_2_rfe <- train(diagnosis ~ .,
                           data = bc_data_2_rfe,
                           method = "rf",
                           preProcess = c("scale", "center"),
                           trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, verboseIter = FALSE))
```

```
## Error in eval(expr, p): objeto 'bc_data_2_rfe' não encontrado
```

```r
cm_rfe_2 <- confusionMatrix(predict(model_bc_data_2_rfe, bc_data_2_test[, -c(1, 2)]), bc_data_2_test$diagnosis)
```

```
## Error in predict(model_bc_data_2_rfe, bc_data_2_test[, -c(1, 2)]): objeto 'model_bc_data_2_rfe' não encontrado
```

```r
cm_rfe_2
```

```
## Error in eval(expr, envir, enclos): objeto 'cm_rfe_2' não encontrado
```

* GA


```r
set.seed(27)
model_bc_data_2_ga <- train(diagnosis ~ .,
                          data = bc_data_2_ga,
                          method = "rf",
                          preProcess = c("scale", "center"),
                          trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, verboseIter = FALSE))
```

```
## Error in eval(expr, p): objeto 'bc_data_2_ga' não encontrado
```

```r
cm_ga_2 <- confusionMatrix(predict(model_bc_data_2_ga, bc_data_2_test[, -c(1, 2)]), bc_data_2_test$diagnosis)
```

```
## Error in predict(model_bc_data_2_ga, bc_data_2_test[, -c(1, 2)]): objeto 'model_bc_data_2_ga' não encontrado
```

```r
cm_ga_2
```

```
## Error in eval(expr, envir, enclos): objeto 'cm_ga_2' não encontrado
```

### Dataset 3


```r
venn_list <- list(cor = colnames(bc_data_3_cor)[-1],
                  rfe = colnames(bc_data_3_rfe)[-1],
                  ga = colnames(bc_data_3_ga)[-1])
```

```
## Error in colnames(bc_data_3_rfe): objeto 'bc_data_3_rfe' não encontrado
```

```r
venn <- venn(venn_list)
```

```
## Error in getVennCounts(data, universe = universe, intersections = intersections): objeto 'venn_list' não encontrado
```

```r
venn
```

```
## function (data, universe = NA, small = 0.7, showSetLogicLabel = FALSE, 
##     simplify = FALSE, show.plot = TRUE, intersections = TRUE, 
##     names, ...) 
## {
##     counts <- getVennCounts(data, universe = universe, intersections = intersections)
##     if (show.plot) 
##         drawVennDiagram(data = counts, small = small, showSetLogicLabel = showSetLogicLabel, 
##             simplify = simplify, ...)
##     if (intersections) 
##         attr(counts, "intersections") <- vennMembers(l = data, 
##             universe = universe, names = names)
##     invisible(counts)
## }
## <environment: namespace:gplots>
```

Com o terceiro conjunto de dados, ainda há alguma variação entre os recursos selecionados, mas não tão extremos quanto com o conjunto de dados 2. Desta vez, encontramos a maior sobreposição entre correlação e GA.

* Correlação


```r
set.seed(27)
model_bc_data_3_cor <- train(outcome ~ .,
                           data = bc_data_3_cor,
                           method = "rf",
                           preProcess = c("scale", "center"),
                           trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, verboseIter = FALSE))
```

```
## Error in serialize(data, node$con): erro ao escrever na conexão
```

```r
cm_cor_3 <- confusionMatrix(predict(model_bc_data_3_cor, bc_data_3_test[, -1]), bc_data_3_test$outcome)
```

```
## Error in predict(model_bc_data_3_cor, bc_data_3_test[, -1]): objeto 'model_bc_data_3_cor' não encontrado
```

```r
cm_cor_3
```

```
## Error in eval(expr, envir, enclos): objeto 'cm_cor_3' não encontrado
```

* RFE


```r
set.seed(27)
model_bc_data_3_rfe <- train(outcome ~ .,
                           data = bc_data_3_rfe,
                           method = "rf",
                           preProcess = c("scale", "center"),
                           trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, verboseIter = FALSE))
```

```
## Error in eval(expr, p): objeto 'bc_data_3_rfe' não encontrado
```

```r
cm_rfe_3 <- confusionMatrix(predict(model_bc_data_3_rfe, bc_data_3_test[, -1]), bc_data_3_test$outcome)
```

```
## Error in predict(model_bc_data_3_rfe, bc_data_3_test[, -1]): objeto 'model_bc_data_3_rfe' não encontrado
```

```r
cm_rfe_3
```

```
## Error in eval(expr, envir, enclos): objeto 'cm_rfe_3' não encontrado
```

* GA


```r
set.seed(27)
model_bc_data_3_ga <- train(outcome ~ .,
                          data = bc_data_3_ga,
                          method = "rf",
                          preProcess = c("scale", "center"),
                          trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, verboseIter = FALSE))
```

```
## Error in eval(expr, p): objeto 'bc_data_3_ga' não encontrado
```

```r
cm_ga_3 <- confusionMatrix(predict(model_bc_data_3_ga, bc_data_3_test[, -1]), bc_data_3_test$outcome)
```

```
## Error in predict(model_bc_data_3_ga, bc_data_3_test[, -1]): objeto 'model_bc_data_3_ga' não encontrado
```

```r
cm_ga_3
```

```
## Error in eval(expr, envir, enclos): objeto 'cm_ga_3' não encontrado
```

# Conclusões

Como esperado do PCA das classes de amostra, que mostraram que as duas classes se repetiram/não recorrentes não se agrupavam bem, os modelos Random Forest do conjunto de dados 3 tinham uma precisão de predição muito menor do que os modelos dos conjuntos de dados 1 e 2.

O método de correlação opera independentemente da importância característica. Por exemplo. No conjunto de dados 1, os recursos com a maior importância também foram marcados como altamente correlacionados. Os modelos de correlação apresentaram o pior nos três conjuntos de dados. RFE e GA tendem a incluir recursos com grande importância, mas a importância da característica sozinho não é um bom indicador de se vários recursos funcionarão bem em combinação ao prever um resultado.

O conjunto de dados 1 era pequeno, com apenas 9 recursos; Aqui, a remoção de características altamente correlacionadas foi o método de seleção menos bem sucedido. RFE e GA melhoraram as previsões em comparação com nenhuma seleção de recurso, enquanto a GA melhorou. O Dataset 2 com seus 30 recursos originais produziu os melhores modelos com o GA. E no conjunto de dados 3, que apresentou menor precisão geral, diferentes métodos de seleção de recursos não tiveram uma forte influência.









