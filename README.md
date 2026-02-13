# Banco de dados aleatório 
O banco de dados para o projeto [dash_pacientes](https://github.com/GodoyTO/dash_pacientes) foi extraído da página [FAKE NAME GENERATOR](https://www.fakenamegenerator.com/order.php) usando os preset:

Name sets: Arabic, Brazil, Chinese, Czech, Eritrean, Finnish, French, German, Hispanic, Igbo, Italian, Japanese (Anglicized), Scottish, Vietnamese

Countries: Brazil

Age range: 0 - 100 years old

Gender: 50% male, 50% female

Quantity: 100,000 identities 

Todos os campos foram incluídos, exceto Weight(pounds) e Height(feet/inches).

Após essa extração ele foi tratado selecionando um subset de variáveis de interesse (nome, idade, cidade, lat e long) 
ou que poderiam ser usadas para criar outras variáveis (veículo, companhia, signo, cor, etc.)

Além disso ele foi cortado para representar uma pirâmide etária específica e uma coluna de raça foi adicionada além da coluna de etinicidade. Essa coluna de raça foi criada a partir de probabilidades para cada caso de etnicidade diferente.

Foi criado uma coluna com o estágio da doença fictícia baseado no número de palavras que o campo occupation possuia.

Também foi criado um campo de data de diagnóstico baseado nos campos vehicle, principalmente o ano do veiculo, e o campo nm_sign para se obter o mês.

Além da idade do paciente no momento do diagnóstico. Ajustes foram realizados para que a data de diagnóstico sempre seja depois da data de nascimento do paciente.
