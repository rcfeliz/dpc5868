# CLAUDE.md — Trabalho Individual: Seletividade de Risco da Amil contra Autistas

## Contexto

O **Tema 1082 do STJ** decidiu sobre as condições de validade da rescisão unilateral
de planos de saúde coletivos pelas operadoras. O acórdão transitou em julgado em
17/03/2025. A partir de uma decisão intermediária desse tema — cujo efeito prático
é detectado empiricamente em **03/05/2024** —, a Amil passou a rescindir em massa
contratos de planos de saúde coletivos.

A hipótese investigada é a de **seletividade de risco**: a Amil estaria rescindindo
contratos de beneficiários autistas de forma desproporcional, com o objetivo de
eliminar da carteira os segurados com maior custo esperado de assistência.

## Pergunta de Pesquisa

A pergunta central é:

> P(rescindiu | autista) > P(autista)?

Ou seja: a probabilidade de ter o contrato rescindido, dado que o beneficiário é
autista, é maior do que a proporção de autistas na carteira? Se sim, há evidência
de seletividade de risco.

## O Problema de Identificação

### A base ideal (que não temos)

A base ideal teria uma linha por contrato ativo na Amil:

| Variável      | Descrição                                              |
|---------------|--------------------------------------------------------|
| `id_contrato` | Identificador do contrato                              |
| `autista`     | Se o beneficiário é autista                            |
| `dt_ini`      | Início do contrato                                     |
| `dt_fim`      | Fim do contrato (`NA` se ainda vigente)                |
| `rescindiu`   | Se a Amil rescindiu unilateralmente após o Tema 1082   |
| `judicializou`| Se o beneficiário judicializou a rescisão (`NA` se não rescindiu) |

Com esses dados, a pergunta seria direta: comparar P(rescindiu | autista) com
P(autista na carteira).

### O que temos

Só temos acesso ao subconjunto em que `judicializou = TRUE`: processos judiciais
ajuizados no TJSP contra a Amil por rescisão unilateral. Portanto, o que
conseguimos medir é:

> P(rescindiu | autista, **judicializou**)

### Por que isso é um problema

Autistas são uma população politicamente organizada e têm maior propensão a
judicializar. Formalmente:

> P(judicializou | autista, rescindiu) > P(judicializou | não-autista, rescindiu)

Isso significa que nossa amostra superrepresenta autistas entre os rescindidos.
Se simplesmente compararmos a proporção de autistas nos processos com a proporção
esperada, vamos **superestimar** a seletividade.

## A Solução: Diferenças em Diferenças (DiD)

### Intuição

A maior propensão de autistas a judicializar é um **efeito fixo de grupo**: existe
antes e depois da mudança de política. Isso permite usar uma estratégia de
diferenças em diferenças para eliminá-lo.

Antes do Tema 1082 (período de controle), tanto autistas quanto não-autistas
judicializavam rescisões "normais". O nível de autistas era sistematicamente
maior, mas a trajetória temporal era paralela. Após o Tema 1082 (período de
tratamento), se houver seletividade de risco, os autistas deveriam mostrar um
**aumento adicional** nas ações, acima e além do que se esperaria da tendência
paralela.

### Estrutura do DiD

| Grupo        | Antes (pré-03/05/2024) | Depois (pós-03/05/2024) | Diferença |
|--------------|------------------------|--------------------------|-----------|
| Não-autista  | A                      | B                        | B − A     |
| Autista      | C                      | D                        | D − C     |
| **DiD**      |                        |                          | **(D−C) − (B−A)** |

O estimador DiD captura o aumento **diferencial** para autistas após o evento,
líquido do:
1. Efeito fixo de grupo (maior propensão de autistas a judicializar)
2. Tendência temporal comum (o choque geral de rescisões afetou todos)

### Hipótese de Identificação

A hipótese de tendências paralelas exige que, **na ausência de seletividade de
risco**, autistas e não-autistas teriam a mesma taxa de crescimento nas
ações judiciais após 03/05/2024. O viés de seleção (autistas judicializarem mais)
é um efeito de nível, não de tendência — e é justamente isso que o DiD elimina.

## Especificação do Modelo

A unidade de análise é o dia × grupo. O modelo é uma regressão de Poisson:

```
log(count_it) = α + β₁·autista_i + β₂·post_t + δ·(autista_i × post_t)
```

Onde:
- `count_it`: número de novas ações por dia t para o grupo i
- `autista_i`: indicador de grupo (autista = 1, não-autista = 0)
- `post_t`: indicador de período (após 03/05/2024 = 1)
- `δ`: estimador DiD — o parâmetro de interesse

**H₀**: δ = 0 (sem seletividade de risco)  
**H₁**: δ > 0 (seletividade de risco contra autistas)

## Dados Disponíveis

- Arquivo: `data/da_did.rda` (objeto `da_did`)
- N = 1.580 processos, todos da Amil
- Período: fev/2023 a mai/2024
- Variáveis: `processo`, `cd_processo`, `dt_dist`, `operadora`, `autista`
- Ponto de mudança detectado: **03/05/2024**

## Estrutura dos Arquivos

```
inst/trabalho-individual/
├── CLAUDE.md        # este arquivo
├── relatorio.qmd    # relatório acadêmico final
└── analise_did.R    # script de análise
```
