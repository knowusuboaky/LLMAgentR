# ------------------------------------------------------------------------------
# Packages
# ------------------------------------------------------------------------------
get_suggested <- function(pkg, fun = NULL) {
  base_pkgs <- c("base", "utils", "stats", "graphics", "grDevices", "methods", "datasets")

  # Always pass base packages
  if (pkg %in% base_pkgs) {
    return(if (is.null(fun)) invisible(TRUE) else get(fun, envir = asNamespace(pkg)))
  }

  # Ensure package is available
  if (!requireNamespace(pkg, quietly = TRUE)) {
    warning(sprintf(
      "The '%s' package is required for this functionality. Please install it or move it to Imports.",
      pkg
    ), call. = FALSE)
  }

  # Return function if requested, or TRUE
  if (!is.null(fun)) {
    return(get(fun, envir = asNamespace(pkg)))
  }

  invisible(TRUE)
}


# ------------------------------------------------------------------------------
#' Build a Document Summarizer Agent
#'
#' Creates an LLM-powered document summarization workflow that processes PDF, DOCX,
#' PPTX, TXT, or plain text input and returns structured markdown summaries.
#'
#' @param llm A function that accepts a character prompt and returns an LLM response.
#' @param summary_template Optional custom summary template in markdown format.
#' @param chunk_size Maximum character length for document chunks (default: 4000).
#' @param overlap Character overlap between chunks (default: 200).
#' @param verbose Logical controlling progress messages (default: TRUE).
#'
#' @return A function that accepts file paths or text input and returns:
#' \itemize{
#'   \item summary - The generated markdown summary
#'   \item metadata - Document metadata if available
#'   \item chunks - Number of processing chunks used
#'   \item success - Logical indicating success
#' }
#'
#' @examples
#' \dontrun{
#' # Build document summarizer agent
#' summarizer_agent <- build_doc_summarizer_agent(
#'   llm = my_llm_wrapper,
#'   summary_template = NULL,
#'   chunk_size = 4000,
#'   overlap = 200,
#'   verbose = FALSE
#' )
#'
#' # Summarize document
#' final_state <- summarizer_agent("https://github.com/knowusuboaky/LLMAgentR/raw/main/\
#' tests/testthat/test-data/scrum.docx")
#' }
#' @export
build_doc_summarizer_agent <- function(
    llm,
    summary_template = NULL,
    chunk_size = 4000,
    overlap = 200,
    verbose = TRUE
) {
  if (verbose) message("=== STARTING DOCUMENT SUMMARIZER AGENT ===")

  # Check for required packages
  officer   <- get_suggested("officer")
  pdftools  <- get_suggested("pdftools")
  glue      <- get_suggested("glue")
  purrr     <- get_suggested("purrr")
  stringr   <- get_suggested("stringr")
  dplyr     <- get_suggested("dplyr")
  xml2      <- get_suggested("xml2")
  rvest     <- get_suggested("rvest")

  #tesseract <- get_suggested("tesseract")

  # OCR fallback for scanned PDFs
  read_pdf_ocr <- function(file) {
    pages <- pdftools::pdf_info(file)$pages
    txt   <- lapply(seq_len(pages), function(p) {
      img <- pdftools::pdf_render_page(file, page = p, dpi = 300)
      tesseract::ocr(img)
    })
    paste(txt, collapse = "\n")
  }

  # Download a URL to temp if needed
  download_if_needed <- function(url) {
    tmp <- file.path(tempdir(), basename(url))
    if (!file.exists(tmp)) {
      if (verbose) message("Downloading URL: ", url)
      download.file(url, destfile = tmp, mode = "wb", quiet = TRUE)
    }
    tmp
  }

  # Default summary template
  if (is.null(summary_template)) {
    summary_template <- paste(
      "Create a comprehensive markdown summary with:\n",
      "1. # Key Findings\n2. ## Methodology\n3. ## Conclusions\n\n",
      "Text to summarize:\n{text}\n\n",
      "Include:\n- Bullet points for key ideas\n- **Bold** for important terms\n- ```code``` blocks for technical details",
      sep = ""
    )
  }

  chunk_text <- function(text, size, overlap) {
    starts <- seq(1, nchar(text), by = size - overlap)
    ends   <- pmin(starts + size - 1, nchar(text))
    purrr::map2(starts, ends, \(s, e) substr(text, s, e))
  }

  # --------------------------------------------------------------------------
  #  Local file reading (with encryption & OCR checks)
  # --------------------------------------------------------------------------
  read_local_file <- function(file_path) {
    ext <- tolower(tools::file_ext(file_path))
    if (!file.exists(file_path)) {
      message("File not found: ", file_path)
      return(NULL)
    }
    message("Processing file: ", file_path)
    tryCatch({
      if (ext == "pdf") {
        info   <- pdftools::pdf_info(file_path)
        if (isTRUE(info$encrypted))
          stop("PDF is encrypted; please decrypt or supply a password.")
        txt_vec <- pdftools::pdf_text(file_path)
        if (all(nchar(txt_vec) == 0)) {
          message("No text layer-running OCR fallback.")

        # only run OCR if tesseract is available
          if (!requireNamespace("tesseract", quietly = TRUE)) {
            stop("Install the 'tesseract' package to enable OCR for scanned PDFs.")
            }
          txt_vec <- read_pdf_ocr(file_path)
        }
        content <- gsub("\n\n+", "\n", paste(txt_vec, collapse = "\n"))
        data.frame(
          source        = file_path,
          title         = ifelse(nzchar(info$title %||% ""), info$title, NA_character_),
          author        = ifelse(nzchar(info$author %||% ""), info$author, NA_character_),
          publishedDate = as.character(info$created %||% NA),
          description   = NA_character_,
          content       = content,
          url           = NA_character_,
          source_type   = "pdf",
          stringsAsFactors = FALSE
        )
      } else if (ext == "docx") {
        doc      <- officer::read_docx(file_path)
        props    <- officer::doc_properties(doc)
        text_vec <- officer::docx_summary(doc)$text
        content  <- gsub("\n\n+", "\n", paste(text_vec, collapse = "\n"))
        data.frame(
          source        = file_path,
          title         = ifelse(nzchar(props$title %||% ""), props$title, NA_character_),
          author        = ifelse(nzchar(props$author %||% ""), props$author, NA_character_),
          publishedDate = as.character(props$created %||% NA),
          description   = NA_character_,
          content       = content,
          url           = NA_character_,
          source_type   = "docx",
          stringsAsFactors = FALSE
        )
      } else if (ext == "pptx") {
        ppt      <- officer::read_pptx(file_path)
        props    <- officer::doc_properties(ppt)
        text_vec <- officer::pptx_summary(ppt)$text
        content  <- gsub("\n\n+", "\n", paste(text_vec, collapse = "\n"))
        data.frame(
          source        = file_path,
          title         = ifelse(nzchar(props$title %||% ""), props$title, NA_character_),
          author        = ifelse(nzchar(props$author %||% ""), props$author, NA_character_),
          publishedDate = as.character(props$created %||% NA),
          description   = NA_character_,
          content       = content,
          url           = NA_character_,
          source_type   = "pptx",
          stringsAsFactors = FALSE
        )
      } else if (ext == "txt") {
        lines   <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
        content <- gsub("\n\n+", "\n", paste(lines, collapse = "\n"))
        data.frame(
          source        = file_path,
          title         = NA_character_,
          author        = NA_character_,
          publishedDate = NA_character_,
          description   = NA_character_,
          content       = content,
          url           = NA_character_,
          source_type   = "txt",
          stringsAsFactors = FALSE
        )
      } else if (ext %in% c("html", "htm")) {
        doc         <- xml2::read_html(file_path)
        raw_text    <- rvest::html_text2(doc)
        content     <- gsub("\n\n+", "\n", raw_text)
        page_title  <- rvest::html_element(doc, "title") |> rvest::html_text(trim = TRUE)
        meta        <- rvest::html_elements(doc, "meta")
        description <- rvest::html_attr(meta[rvest::html_attr(meta, "name")=="description"], "content")
        author      <- rvest::html_attr(meta[rvest::html_attr(meta, "name")=="author"], "content")
        data.frame(
          source        = file_path,
          title         = ifelse(nzchar(page_title %||% ""), page_title, NA_character_),
          author        = ifelse(length(author)>0 && nzchar(author[1]), author[1], NA_character_),
          publishedDate = NA_character_,
          description   = ifelse(length(description)>0 && nzchar(description[1]), description[1], NA_character_),
          content       = content,
          url           = NA_character_,
          source_type   = "html",
          stringsAsFactors = FALSE
        )
      } else {
        message("Skipping unsupported extension: ", ext)
        NULL
      }
    }, error = function(e) {
      message("Error processing ", file_path, ": ", e$message)
      NULL
    })
  }

  read_local_files <- function(paths) {
    dfs <- list()
    for (p in paths) {
      if (grepl("^https?://", p)) {
        p <- download_if_needed(p)
      }
      if (dir.exists(p)) {
        files <- list.files(p, recursive = TRUE, full.names = TRUE)
        message("Found ", length(files), " files in dir: ", p)
        for (f in files) {
          tmp <- read_local_file(f)
          if (!is.null(tmp)) dfs[[length(dfs)+1]] <- tmp
        }
      } else if (file.exists(p)) {
        tmp <- read_local_file(p)
        if (!is.null(tmp)) dfs[[length(dfs)+1]] <- tmp
      } else {
        # missing path; will be caught later
      }
    }
    if (!length(dfs)) return(data.frame())
    dplyr::bind_rows(dfs)[, c(
      "source","title","author","publishedDate",
      "description","content","url","source_type"
    )]
  }

  # --------------------------------------------------------------------------
  #  Returned summarizer function
  # --------------------------------------------------------------------------
  function(input_data) {
    tryCatch({
      # normalize input vector
      inputs <- as.character(input_data)

      # if any missing URLs/paths, error out
      is_url    <- grepl("^https?://", inputs)
      to_check  <- inputs[!is_url]
      missing   <- to_check[! (file.exists(to_check) | dir.exists(to_check))]
      if (length(missing)) {
        stop("Cannot find file(s)/dir(s): ", paste(missing, collapse = ", "))
      }

      # load files or treat as raw text
      if (any(grepl("\\.(pdf|docx|pptx|txt|html?)$", inputs, ignore.case = TRUE)) ||
          any(is_url)) {
        df_meta <- read_local_files(inputs)
        if (!nrow(df_meta)) {
          stop("No valid files were processed; check paths or URLs.")
        }
        text_in  <- paste(df_meta$content, collapse = "\n")
        metadata <- df_meta
      } else {
        text_in  <- inputs
        metadata <- list()
      }

      clean_text <- stringr::str_squish(text_in)

      # bail out if blank
      if (nchar(clean_text) == 0) {
        if (verbose) message("No content to summarize; skipping LLM call.")
        return(list(
          summary  = "",
          metadata = metadata,
          chunks   = 0,
          success  = TRUE
        ))
      }

      # chunk if needed
      chunks <- if (nchar(clean_text) > chunk_size) {
        if (verbose) message("Splitting text into chunks...")
        chunk_text(clean_text, chunk_size, overlap)
      } else {
        list(clean_text)
      }

      # summarize each chunk
      if (verbose) message("Calling LLM for summarization...")
      partials <- purrr::map(chunks, \(txt) {
        prompt <- glue::glue(summary_template, text = txt)
        llm(prompt)
      })

      # combine partials if >1
      summary_final <- if (length(partials) > 1) {
        llm(glue::glue(
          "Combine these partial summaries into a cohesive markdown summary:\n\n",
          "{paste(partials, collapse='\\n\\n')}"
        ))
      } else {
        partials[[1]]
      }

      list(
        summary  = summary_final,
        metadata = metadata,
        chunks   = length(chunks),
        success  = TRUE
      )
    }, error = function(e) {
      warning("Summarization failed: ", e$message)
      list(
        summary  = NULL,
        metadata = list(),
        chunks   = 0,
        success  = FALSE,
        error    = e$message
      )
    })
  }
}

