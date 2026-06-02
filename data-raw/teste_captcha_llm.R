img_path <- here::here("data-raw", "captcha", "27722419000103.jpg")

img_b64 <- base64enc::base64encode(img_path)

base_url <- "https://OpenAI-Jurimetria.openai.azure.com/openai/deployments/Jurimetria_GPT4omini/chat/completions?api-version=2024-02-01"

resp <- httr2::request(base_url) |>
  httr2::req_headers(
    `api-key`      = Sys.getenv("AZURE_OPENAI_API_KEY"),
    `Content-Type` = "application/json"
  ) |>
  httr2::req_body_json(list(
    max_tokens = 10,
    messages   = list(list(
      role    = "user",
      content = list(
        list(
          type      = "image_url",
          image_url = list(url = paste0("data:image/jpeg;base64,", img_b64))
        ),
        list(
          type = "text",
          text = "What are the characters shown in this CAPTCHA image? Reply with ONLY the characters, nothing else."
        )
      )
    ))
  )) |>
  httr2::req_perform()

codigo <- httr2::resp_body_json(resp)$choices[[1]]$message$content |> trimws()
codigo
