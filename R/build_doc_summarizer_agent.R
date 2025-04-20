#' Build a Document Summarizer Agent
#'
#' This function returns a document summarization workflow using a provided LLM.
#' It can process PDF, DOCX, PPTX, TXT, or plain text input and return a markdown-formatted summary.
#'
#'
#' @name build_doc_summarizer_agent
#' @param llm A function that takes a character prompt and returns a response.
#'
#' @return A function that accepts a file path or text input and returns a summary.
#' @examples
#' \dontrun{
#' summarizer <- build_doc_summarizer(llm = call_llm)
#' summary <- summarizer("my_paper.pdf")
#' cat(summary)
#' }
#' @export
NULL

build_doc_summarizer_agent <- function(llm) {
  cat("=== STARTING DOCUMENT SUMMARIZER AGENT ===\n")

  # 1. Ensure required packages (Suggests:) are available
  officer <- get_suggested("officer")
  pdftools <- get_suggested("pdftools")
  glue <- get_suggested("glue")
  purrr <- get_suggested("purrr")


  # Function to load different document types
  load_document <- function(input_data) {
    file_extension <- if (file.exists(input_data)) {
      tolower(tools::file_ext(input_data))
    } else {
      "text"
    }

    tryCatch({
      if (file_extension == "pdf") {
        text <- pdftools::pdf_text(input_data)
        list(list(page_content = paste(text, collapse = "\n"), metadata = list()))
      } else if (file_extension == "pptx") {
        ppt <- officer::read_pptx(input_data)
        slide_texts <- map(1:length(ppt), function(i) {
          paste(sapply(ppt$slide[[i]]$shapes, function(shape) {
            if (!is.null(shape$text)) shape$text else ""
          }), collapse = "\n")
        })
        map(slide_texts, ~list(page_content = ., metadata = list(title = "")))
      } else if (file_extension == "docx") {
        doc <- officer::read_docx(input_data)
        list(list(page_content = paste(docx_summary(doc)$text, collapse = "\n"),
                  metadata = list()))
      } else if (file_extension == "txt") {
        list(list(page_content = paste(readLines(input_data, warn = FALSE),
                                       collapse = "\n"), metadata = list()))
      } else {
        list(list(page_content = input_data, metadata = list()))
      }
    }, error = function(e) {
      stop(paste("Error loading document:", e$message))
    })
  }

  # Function to generate summaries using your LLM
  summarize_document <- function(docs) {
    prompt_template <- "
      Please provide a comprehensive summary of the following text in Markdown format.
      Include key points as numbered lists and highlight important sections in bold.

      Text to summarize:
      {text}

      Required format:
      # Summary Title
      ## Key Points
      1. First main point
      2. Second main point
      - **Important Detail**: Explanation
      3. Third main point

      ## Additional Insights
      ..."

    combined_text <- map_chr(docs, ~.$page_content) %>%
      paste(collapse = "\n\n")

    prompt <- glue::glue(prompt_template, text = combined_text)
    llm(prompt)  # Using your provided LLM wrapper
  }

  # Main workflow function
  function(input_data) {
    docs <- load_document(input_data)
    summary <- summarize_document(docs)
    return(summary)
  }
}
