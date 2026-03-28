# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Sobre o Projeto

Pacote R `dpc5868` com materiais do curso **DPC5868 - Pesquisa Empírica: Instituições e Processos** (pós-graduação em Direito, USP). O pacote está em estágio inicial de desenvolvimento.

## Comandos Principais

```r
# Gerar documentação (Roxygen2)
devtools::document()

# Verificar pacote
devtools::check()

# Instalar localmente
devtools::install()

# Renderizar README
rmarkdown::render("README.Rmd")
```

## Arquitetura

- Funções em `R/` documentadas com Roxygen2 (markdown habilitado)
- Documentação de datasets em `R/data.R`
- `NAMESPACE` gerado automaticamente — nunca editar manualmente
- `README.md` gerado a partir de `README.Rmd` — nunca editar o `.md` diretamente
