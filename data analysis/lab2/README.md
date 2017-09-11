# Activities Instructions

Here are the Activities Instructions in portuguese (sadly there's 
no english version available and I whish I cloud traslate it but
I don't want to :cry:).

## Part 2

É sabido que os períodos iniciais do curso de computação estão
entre os mais difíceis de todo o curso. Entre os fatores que podem
explicar isso, estão disciplinas que são consideradas difíceis para
alunos recém saídos do ensino médio, como, por exemplo, Cálculo I,
Álgebra Vetorial, P1 e LP1.

A porcentagem de evasão nos primeiros períodos também é bastante alta
em relação a períodos posteriores, o que corrobora essa premissa.
Isso nos leva a conjecturar que os alunos que apresentam bom desempenho
nos períodos iniciais terão um bom desempenho no curso como um todo,
ou seja, os alunos que foram bem na parte mais difícil do curso 
provavelmente irão bem também na parte menos difícil.
Podemos colocar essa conjectura à prova por meio da seguinte pergunta:

O desempenho dos alunos nos dois primeiros períodos consegue explicar,
em algum grau, seus desempenhos no curso como um todo?

Vamos tentar responder essa pergunta usando regressão linear.
Vamos fazer isso quebrando a pergunta anterior nas seguintes perguntas:

1. Um modelo de regressão múltipla com todas as variáveis é plausível
   para explicar a variação em y? Em que grau?

2. Todas as variáveis são úteis para o modelo de regressão?

3. Se a resposta para a pergunta anterior foi não, construa um novo 
   modelo sem essas variáveis e o compare ao modelo com todas as
   variáveis (e.g. em termos de R2 e RSE).

4. Analise os plots de resíduos de cada variável e veja se algum 
   (um ou mais) deles indica não aleatoriedade dos erros.

5. Que período consegue explicar melhor o desempenho final (primeiro ou segundo)?

6. Use o melhor modelo encontrado para predizer o seu próprio desempenho e
   compare a predição com o seu CRA atual. Comente o resultado.
 
## Part 3

Nesta parte construiremos modelos preditivos de regressão para a predição do CRA
baseados nas disciplinas do primeiro e segundo período. As atividades esperadas
para essa etapa são descritas a seguir:

1. Baixe os dados de treino e teste.

2. Calcule o CRA dos alunos com base no script de preprocessamento 
   (Links para um site externo) do lab anterior.

3. Usando todas as variáveis disponíveis (disciplinas do primeiro e segundo período), 
   use validação cruzada (nos dados de treino) para tunar um modelo de regressão Ridge.

4. Mesmo que o item acima mas usando um modelo de regressão Lasso.

5. Compare os dois modelos nos dados de teste em termos de RMSE.

6. Quais as variáveis mais importantes segundo o modelo de regressão Lasso?
   Alguma variável foi descartada? Quais?

7. Re-treine o melhor modelo (dessa vez nos dados de treino sem validação cruzada) e
   reporte o RMSE no teste.

8. Use ou tente melhorar o seu modelo para prever os dados de teste que disponibilizamos
   por meio da plataforma Kaggle: 
   https://inclass.kaggle.com/c/previsao-do-cra-dos-alunos-de-computacao

Ideias para melhorar o modelo:

* Tente criar novas variáveis a partir das variáveis existentes, por exemplo, 
  média de notas do período, elevando variáveis ao quadrado, cubo, etc. e 
  multiplicando variáveis.

* Usando subconjuntos das variáveis disponíveis, por exemplo, escolhendo um 
  ponto de corte baseado nos valores dos coeficientes. 
  Por exemplo, usar as variáveis cujos valores de coeficientes
  são maiores que 0,2.

* Usando outros métodos de regressão. 
  Por exemplo, SVR, Árvores de Regressão e Florestas Aleatórias.

