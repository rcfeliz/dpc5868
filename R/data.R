#' Julgados de primeira grau do TJSP — Ações Civis Públicas
#'
#' Resultado da consulta ao CJPG (Consulta de Julgados de Primeira Grau) do
#' Tribunal de Justiça de São Paulo, filtrando processos da classe Ação Civil
#' Pública. Coletado em maio de 2026 para o trabalho em grupo da disciplina
#' DPC5868.
#'
#' @format Um `tibble` com 55.666 linhas e 13 variáveis:
#' \describe{
#'   \item{processo}{`character`. Número do processo no formato CNJ.}
#'   \item{pagina}{`integer`. Página da consulta em que o processo foi coletado.}
#'   \item{hora_coleta}{`POSIXct`. Horário em que o registro foi coletado.}
#'   \item{duplicado}{`logical`. Indica se o processo aparece mais de uma vez na consulta.}
#'   \item{classe}{`character`. Classe processual (ex.: "Ação Civil Pública").}
#'   \item{assunto}{`character`. Assunto do processo conforme tabela CNJ.}
#'   \item{magistrado}{`character`. Nome do magistrado responsável.}
#'   \item{comarca}{`character`. Comarca onde o processo tramita.}
#'   \item{foro}{`character`. Foro onde o processo tramita.}
#'   \item{vara}{`character`. Vara onde o processo tramita.}
#'   \item{disponibilizacao}{`Date`. Data de disponibilização do julgado no CJPG.}
#'   \item{julgado}{`character`. Texto completo do julgado.}
#'   \item{cd_doc}{`character`. Código interno do documento no ESAJ.}
#' }
#' @source Tribunal de Justiça de São Paulo — CJPG (<https://esaj.tjsp.jus.br/cjpg/>)
"cjpg"


#' Capa dos processos do TJSP — Ações Civis Públicas
#'
#' Dados da capa (informações cadastrais) dos processos recuperados via CPOPG
#' (Consulta de Processos de Primeiro Grau) do TJSP, a partir dos números de
#' processo presentes em [cjpg].
#'
#' @format Um `tibble` com uma linha por processo e 14 variáveis:
#' \describe{
#'   \item{processo}{`character`. Número do processo no formato CNJ.}
#'   \item{cd_processo}{`character`. Código interno do processo no ESAJ.}
#'   \item{tipo_processo}{`character`. Tipo do processo (ex.: "Principal").}
#'   \item{digital}{`logical`. Indica se o processo é digital.}
#'   \item{situacao}{`character`. Situação atual do processo (ex.: "Extinto").}
#'   \item{classe}{`character`. Classe processual (ex.: "Ação Civil Pública").}
#'   \item{assunto}{`character`. Assunto do processo conforme tabela CNJ.}
#'   \item{foro}{`character`. Foro onde o processo tramita.}
#'   \item{vara}{`character`. Vara onde o processo tramita.}
#'   \item{juiz}{`character`. Nome do juiz responsável.}
#'   \item{dt_dist}{`Date`. Data de distribuição do processo.}
#'   \item{area}{`character`. Área do direito (ex.: "Cível").}
#'   \item{controle}{`character`. Número de controle interno do foro.}
#'   \item{valor_da_acao}{`numeric`. Valor da causa em reais.}
#' }
#' @source Tribunal de Justiça de São Paulo — CPOPG (<https://esaj.tjsp.jus.br/cpopg/>)
#' @seealso [partes], [movimentacoes]
"capa"


#' Partes dos processos do TJSP — Ações Civis Públicas
#'
#' Dados das partes (autores, réus e terceiros) dos processos recuperados via
#' CPOPG do TJSP, a partir dos números de processo presentes em [cjpg].
#' Cada linha corresponde a uma parte em um processo.
#'
#' @format Um `tibble` com uma linha por parte e 8 variáveis:
#' \describe{
#'   \item{processo}{`character`. Número do processo no formato CNJ.}
#'   \item{cd_processo}{`character`. Código interno do processo no ESAJ.}
#'   \item{id_parte}{`integer`. Identificador sequencial da parte no processo.}
#'   \item{nome}{`character`. Nome da parte.}
#'   \item{tipo}{`character`. Tipo do registro (ex.: "Parte").}
#'   \item{tipo_parte}{`character`. Polo da parte (ex.: "Reqte", "Reqdo").}
#'   \item{tipo_representante}{`character`. Tipo do representante legal, se houver.}
#'   \item{obs}{`character`. Observações adicionais sobre a parte.}
#' }
#' @source Tribunal de Justiça de São Paulo — CPOPG (<https://esaj.tjsp.jus.br/cpopg/>)
#' @seealso [capa], [movimentacoes]
"partes"


#' Movimentações dos processos do TJSP — Ações Civis Públicas
#'
#' Histórico de movimentações processuais dos processos recuperados via CPOPG
#' do TJSP, a partir dos números de processo presentes em [cjpg]. Cada linha
#' corresponde a uma movimentação em um processo.
#'
#' @format Um `tibble` com uma linha por movimentação e 9 variáveis:
#' \describe{
#'   \item{processo}{`character`. Número do processo no formato CNJ.}
#'   \item{cd_processo}{`character`. Código interno do processo no ESAJ.}
#'   \item{id_mov}{`numeric`. Identificador sequencial da movimentação no processo.}
#'   \item{dt_mov}{`Date`. Data da movimentação.}
#'   \item{movimento}{`character`. Descrição padronizada da movimentação.}
#'   \item{descricao}{`character`. Texto livre com detalhes da movimentação.}
#'   \item{cd_documento}{`character`. Código do documento vinculado, se houver.}
#'   \item{recurso_acessado}{`character`. Nome do recurso acessado na movimentação.}
#'   \item{url}{`character`. URL do documento vinculado no ESAJ, se houver.}
#' }
#' @source Tribunal de Justiça de São Paulo — CPOPG (<https://esaj.tjsp.jus.br/cpopg/>)
#' @seealso [capa], [partes]
"movimentacoes"
