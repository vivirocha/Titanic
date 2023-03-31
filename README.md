## Titanic
O Titanic foi um famoso navio de passageiros britânico que afundou no oceano Atlântico em 15 de abril de 1912, durante sua viagem inaugural de Southampton, no Reino Unido, para Nova York, nos Estados Unidos. O navio, que era considerado um dos maiores e mais luxuosos de sua época, colidiu com um iceberg e afundou em menos de três horas, resultando na morte de mais de 1.500 pessoas, entre passageiros e tripulantes. O desastre do Titanic é considerado um dos mais trágicos da história marítima e teve um impacto duradouro na indústria naval e na consciência pública sobre a segurança em alto mar.

## O desafio
Neste desafio, pedimos que você construa um modelo preditivo que responda à pergunta: “que tipo de pessoa tem maior probabilidade de sobreviver?” usando os dados do passageiro (ou seja, nome, idade, sexo, classe socioeconômica, etc).

## Quais dados estarão disponíveis nesta competição?

Nesta competição, teremos acesso a dois conjuntos de dados semelhantes que incluem informações do passageiro, como nome, idade, sexo, classe socioeconômica etc. Um conjunto de dados é intitulado train.csv e o outro é intitulado test.csv.
Train.csv conterá os detalhes de um subconjunto dos passageiros a bordo (891 para ser exato) e, mais importante, revelará se eles sobreviveram ou não, também conhecido como “verdade terrestre”.
O conjunto de dados test.csv contém informações semelhantes, mas não revela a “verdade básica” para cada passageiro. É seu trabalho prever esses resultados.
Usando os padrões encontrados nos dados train.csv, preveja se os outros 418 passageiros a bordo (encontrados em test.csv) sobreviveram.

<b> Dicionário de dados </b><br>
survival - Survival 	0 = No, 1 = Yes <br>
pclass Ticket - class 	1 = 1st, 2 = 2nd, 3 = 3rd <br>
sex -	Sex 	<br>
Age - Age in years 	<br>
sibsp - of siblings / spouses aboard the Titanic 	<br>
parch - of parents / children aboard the Titanic 	<br>
ticket - Ticket number 	<br>
fare - Passenger fare 	<br>
cabin - Cabin number 	<br>
embarked - Port of Embarkation 	C = Cherbourg, Q = Queenstown, S = Southampton <br>
<br>
<b> Notas </b> <br>
pclass: Status socioeconomico <br>
1st = Primeira classe <br>
2nd = Segunda classe <br>
3rd = Terceira classe <br>

age: A idade é fracionária se for menor que 1. Se a idade for estimada, ela está na forma de xx,5 <br>

sibsp: O conjunto de dados define as relações familiares desta forma... <br>
Sibling = irmão, irmã, meio-irmão, meia-irmã <br>
Spouse = marido, esposa (amantes e noivos foram ignorados) <br>

parch: O conjunto de dados define as relações familiares desta forma... <br>
Parent = mãe, pai <br>
Child = filha, filho, enteada, enteado <br>
Algumas crianças viajaram apenas com uma babá, portanto parch=0 para elas. <br>

1 [Titanic utilizando média](https://github.com/vivirocha/Titanic/blob/main/ApenasMedia.R) <br> 
2 [Titanic com Escalonamento](https://github.com/vivirocha/Titanic/blob/main/EscalonamentoDeTudo.R) <br>
3 [Titanic com SVM](https://github.com/vivirocha/Titanic/blob/main/SVM.R) <br>
4 [Análises](https://github.com/vivirocha/Titanic/blob/main/Analises.R) <br>
5 [Titanic com Mediana e kNN](https://github.com/vivirocha/Titanic/blob/main/MEDIANA%20EM%20FARE%20E%20kNN%20em%20AGE.R) <br>
6 [Titanic com Outliers e Escalonamento](https://github.com/vivirocha/Titanic/blob/main/Mesclagem%20de%20outliers%20e%20escalonamento%20de%20tudo.R) <br>
7 [Titanic com Outliers](https://github.com/vivirocha/Titanic/blob/main/Outliers.R) <br>
 
