## LCC Projeto 3ºano

### Descrição/dicas dos updates
__________________________________________________________________________________________________

#### Update-11 24/04/2021 - Rafa e Matias

O Rafa fez um commit na branch dele em que adicionou um pie chart de teste que vai dar jeito e melhorou bastante o gráfico das idades para o país.
De resto, estive a fazer merge dos dados e a corrigir erros que ficaram no o último commit.
Em príncipio está tudo a funcionar corretamente. Falta só pensar em novos gráficos e melhorar o aspeto de aquilo que já temos.

Gráficos a pensar:
   - Estatísticas do tipo de vacina por região
   - Percentagem de pessoas por região já vacinadas
   - Melhoramento dos gráficos já existentes no separador das regiões para ficarem no formato que falei á professora (https://moderndive.com/4-tidy.html)

#### Update-10 22/04/2021 - Matias

Temos duas tabs:
- Uma para o país que será uma visão geral.
- Outra para as regiões: Nesta tab vão haver os separadores de cada região do país, onde vamos ter a opção de escolher o tipo de vacina que queremos ver, assim como escolher o tipo de vacina. NOTA: Rever o que se está a passar na tab de Lisboa e na tab dos Açores.

Não tinha reparado que tinhas feito commit mas no fds vou dar uma vista de olhos para juntarmos tudo. 
NOTA2: Não esquecer de adicionar o gráfico que a professora falou. (Ficou parcialmente feito)

#### Update-9 22/04/2021 - Rafa

#### As alterações guardei nas minha branch caso alguem queira ver. Em termos de trabalho em si não é nada por aí fora, mas pode-nos ajudar no futuro.
Ando a tentar trabalhar num mapa mas vou pôr em stand-by e continuo no fim-de-semana. 
Tentei desenvolver umas variáveis que penso que nos podem dar jeito mais para a frente. Fui filtrando os datasets para conseguir variaveis como por exemplo:
* Total de gente vacinada em portugal (tendo já em conta que está vacinada após a segunda vacina. Quando tivermos outra vacina que seja com um nº de aplicações diferentes, penso que desta forma conseguimos fazer filtros facilmente)
* Total de pessoas vacinadas de cada vacina...

Entretanto (esteticamente) estive também a ver como podemos alterar as cores dos bins dos graficos caso nos possa ser util no futuro.




#### Update-8 20/04/2021 - Matias

Alterei o visual da aplicação. 
Reparei que quando selecionamos os dados por região, não temos a possibilidade de selecionar por faixa etária o que nos deixa limitados nos gráficos por região.
Deixei com duas tabs: Uma para Portugal e outra para as regiões. Falta adicionar o gráfico que vimos com a professora e os gráficos das vacinas que tinha adicionado mas tirei para organizar tudo melhor.
Faltar melhorar o aspeto do código. Penso que podemos tornar algumas coisas globais mas não sei como o possamos fazer.
Não apaguem o último update que têm pois posso ter-me enganado em (ou esquecido de) algumas coisas.

#### Update-7 18/04/2021 - Matias

Adicionei 2 novos tabs:
   - Um em que Tentei implementar a função da mesma forma que a professora mas parece quevamos ter problemas devido ao formato da data.
   - Outro onde criei um histograma do número de primeiras doses administradas de cada vacina.

##### NOTA: Limpar um bocado o código que estamos a repetir várias vezes algumas coisas que basta ter apenas uma vez num reactive. Não esquecer de tratar do select.

#### Update-6 16/04/2021 - Matias

Os dados do gráfico desenhado na Visão Geral não eram aqueles. Troquei para os que estavam antes.

#### Update-5 16/04/2021 - Matias

A disposição já está a funcionar bem, era apenas um ';' x).
Depois temos de mudar o nome do label no gráfico da primeira tab e tentar resolver o problema do select.

#### Update-4 15/04/2021 - Rafa

Alterei a forma de como pômos aqui os updates para ser mais facil ver. Os recentes aparecem logo em cima.
Ora bem... Tentei dar alguma organização à forma como temos a informação disposta nos eixos x e y dos gráficos (para não ficar informação sobreposta uma à outra).
Estive também a arranjar forma de que, quando selecionamos um tema do shiny, os nossos gráficos também são abrangidos por esses temas. Para tal necessitei de instalar novos packages que estão no ui.R. Só instalar aquilo e estao fixes... Também algumas cenas cosméticas. Ah e destrui a posição dos graficos que o Matias tinha. Já andei para aqui às voltas a tentar perceber o que fiz mas ainda não encontrei.



#### Update-3 11/04/2021 - Matias

Pus por tabs para estar mais organizado e não ficar muito pesado. E na primeira tab que será mais geral (se concordarem claro) estava a pensar pôr as percentagens da população por regiao e por idades e também no total que já levaram a segunda dose da vacina, ou seja, que já estão passaram pelo proceso todo. O mapa de Portugal, se o conseguirmos fazer, talvez fique melhor neste tab.



#### Update-2 10/04/2021 - Matias

Acrescentei outro select para termos por região e também por idades, pus só a apresentar um gráfico (não me parecia lá mto bem ter os dois a aparecer) e também mudei o tema
(Não se esqueçam de instalar o shinythemes).



#### Update-1 10/04/2021 - Matias

Mudei um bocado o html para ficar melhor encaixado e pus os gráficos a abrir um acima do outro (tive de passar os dados para reactive).
A imagem é apenas de teste de como poderá ficar caso consigamos pôr o mapa interativo.
