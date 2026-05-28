import requests
import pandas as pd
import time
from pathlib import Path

API_KEY = "cDZHYzlZa0JadVREZDJCendQbXY6SkJlTzNjLV9TRENyQk1RdnFKZGRQdw=="
URL = "https://api-publica.datajud.cnj.jus.br/api_publica_tjsp/_search"
HEADERS = {"Authorization": f"APIKey {API_KEY}", "Content-Type": "application/json"}

DATA_INICIO = "2015-01-01"
DATA_FIM = "2025-12-31"
PAGE_SIZE = 10000

output_dir = Path(__file__).parent.parent.parent.parent / "data-raw" / "csv" / "datajud_empresarial"
output_dir.mkdir(parents=True, exist_ok=True)


def build_query(search_after=None):
    query = {
        "size": PAGE_SIZE,
        "query": {
            "bool": {
                "must": [
                    {
                        "wildcard": {
                            "orgaoJulgador.nome": {
                                "value": "*empresarial*",
                                "case_insensitive": True
                            }
                        }
                    },
                    {
                        "range": {
                            "dataAjuizamento": {
                                "gte": DATA_INICIO,
                                "lte": DATA_FIM
                            }
                        }
                    }
                ]
            }
        },
        "sort": [
            {"dataAjuizamento": "asc"},
            {"_id": "asc"}
        ]
    }
    if search_after:
        query["search_after"] = search_after
    return query


def extrair_partes(processo):
    numero = processo.get("numeroProcesso", "")
    partes = processo.get("partes", [])
    rows = []
    for parte in partes:
        rows.append({
            "numero_processo": numero,
            "nome": parte.get("nome", ""),
            "tipo": parte.get("tipo", ""),
            "polo": parte.get("polo", ""),
            "advogados": "; ".join(
                adv.get("nome", "") for adv in parte.get("advogados", [])
            )
        })
    return rows


all_partes = []
search_after = None
pagina = 1

while True:
    print(f"pagina {pagina}\n")
    resp = requests.post(URL, headers=HEADERS, json=build_query(search_after))
    resp.raise_for_status()

    hits = resp.json()["hits"]["hits"]
    if not hits:
        break

    for hit in hits:
        all_partes.extend(extrair_partes(hit["_source"]))

    last = hits[-1]
    search_after = [last["sort"][0], last["sort"][1]]
    pagina += 1
    time.sleep(0.5)

print(f"{len(all_partes)} registros de partes extraidos\n")

df = pd.DataFrame(all_partes)
df.to_csv(output_dir / "partes.csv", index=False)
print(f"salvo em {output_dir / 'partes.csv'}\n")
