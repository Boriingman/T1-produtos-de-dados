library(jsonlite)

novos_dados <- data.frame(professores = c(500,55), 
                          despesa_por_aluno = c(10000, 3000))

jsonlite::write_json(novos_dados, 
                     path = "entradas/novas_observacoes.json", pretty = TRUE)
