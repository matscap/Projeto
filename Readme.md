## LCC Projeto 3ºano

### Descrição/dicas dos updates
__________________________________________________________________________________________________

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
