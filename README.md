# Suavizacao-e-Acuracia-Varejo-MA
A previsão de vendas é crucial para o meu negócio. Por isso, decidi utilizar técnicas de suavização de séries temporais, como o modelo SES e a suavização de Holt-Winters, para prever o volume de vendas no estado do Maranhão.

Procedimentos de Suavização:
Para aplicar o modelo SES, precisei determinar o parâmetro de suavização. Usei o método de validação cruzada para encontrar o melhor valor do parâmetro. Já para a suavização de Holt-Winters, precisei determinar três parâmetros: alfa, beta e gama. Novamente, utilizei o método de validação cruzada para encontrar os melhores valores.

Procedimentos de Acurácia:
Para avaliar a acurácia das previsões, usei o erro médio absoluto (MAE) e o erro quadrático médio (MSE) como métricas. Essas métricas me permitiram avaliar o desempenho dos modelos de previsão e identificar possíveis problemas nos dados.

Resultados:
Os resultados mostraram que a suavização de Holt-Winters apresentou um desempenho superior em relação ao modelo SES na previsão do volume de vendas no estado do Maranhão. As métricas de acurácia também indicaram um melhor desempenho do modelo de Holt-Winters em relação ao modelo SES.
