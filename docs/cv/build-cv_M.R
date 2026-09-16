suppressPackageStartupMessages({
  library(googlesheets4)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
})

## ---------------- config ----------------
cv_url <- "https://docs.google.com/spreadsheets/d/1fgR-b6eb3aEoe97thfXuhBHGoI3-QXun8pe8x_DOi0E/edit?usp=sharing"
template <- "cv/cv-template.html"
out_html <- "cv/cv.html"
out_pdf <- "cv/cv.pdf"

## ---------------- small helpers ----------------
`%or%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a

flatten_cols <- function(df) {
  df[] <- lapply(df, function(col) {
    if (!is.list(col)) {
      return(col)
    }
    vapply(col, function(v) {
      if (is.null(v) || length(v) == 0 || all(is.na(v))) NA_character_ else as.character(v[[1]])
    }, character(1))
  })
  df
}

esc <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

clean <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  ifelse(x %in% c("null", "0"), "", x)
}

ensure_cols <- function(df, cols) {
  for (col in cols) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }
  df
}

to_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  suppressWarnings(ymd(substr(as.character(x), 1, 10)))
}

arr_desc <- function(df, col) {
  if (!col %in% names(df)) {
    return(df)
  }
  v <- suppressWarnings(as.numeric(df[[col]]))
  if (all(is.na(v))) v <- df[[col]]
  df[order(v, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
}

lead_bold <- function(s) {
  s <- esc(s)
  pos <- regexpr(",", s, fixed = TRUE)
  ifelse(
    pos > 0,
    paste0('<span class="t">', substr(s, 1, pos - 1), "</span>", substr(s, pos, nchar(s))),
    paste0('<span class="t">', s, "</span>")
  )
}

duties_list <- function(x) {
  vapply(x, function(cell) {
    if (is.na(cell) || !nzchar(trimws(cell))) {
      return("")
    }
    items <- str_split(cell, ";\\s*|\\n")[[1]]
    items <- trimws(items)
    items <- items[nzchar(items)]
    if (length(items) == 0) {
      return("")
    }
    paste0(
      '<ul class="cv-pubs">',
      paste(sprintf("<li>%s</li>", esc(items)), collapse = ""),
      "</ul>"
    )
  }, character(1))
}

section <- function(title, inner) {
  sprintf('<section class="cv-sec">\n  <h2 class="cv-h">%s</h2>\n  %s\n</section>', title, inner)
}

table_plain <- function(rows) {
  sprintf('<table class="cv-table"><tbody>\n%s\n</tbody></table>', paste(rows, collapse = "\n"))
}

## ---------------- load Google Sheets tabs ----------------
gs4_deauth()
message("cv: fetching data from Google Sheets...")

edu <- flatten_cols(suppressMessages(read_sheet(ss = cv_url, sheet = "edu")))
award <- flatten_cols(suppressMessages(read_sheet(ss = cv_url, sheet = "awards")))
employ <- flatten_cols(suppressMessages(read_sheet(ss = cv_url, sheet = "employ")))

conferences <- tryCatch(
  flatten_cols(suppressMessages(read_sheet(ss = cv_url, sheet = "conferences"))),
  error = function(e) NULL
)

activities <- tryCatch(
  flatten_cols(suppressMessages(read_sheet(ss = cv_url, sheet = "activities"))),
  error = function(e) NULL
)

pubs <- tryCatch(
  fetch_pubs(),
  error = function(e) {
    message("cv: Could not fetch Zotero publications: ", e$message)
    NULL
  }
)

## ---------------- section builders ----------------
## 1. Education
build_education <- function(d) {
  d <- ensure_cols(d, c("date", "degree", "school", "note"))
  rows <- vapply(seq_len(nrow(d)), function(i) {
    note_val <- clean(d$note[i])
    sub_note <- if (nzchar(note_val)) sprintf('<br><span class="cv-meta">%s</span>', esc(note_val)) else ""
    sprintf(
      '<tr><td class="c-when-narrow">%s</td><td><span class="t">%s</span>, %s%s</td></tr>',
      esc(d$date[i]), esc(d$degree[i]), esc(d$school[i]), sub_note
    )
  }, character(1))
  section("Education", table_plain(rows))
}

## 2. Awards
build_award <- function(d) {
  d <- arr_desc(d, "year")
  rows <- sprintf(
    '<tr><td class="c-when-narrow">%s</td><td><span class="t">%s.</span> <span class="cv-meta">%s.</span></td></tr>',
    esc(d$year), esc(d$description), esc(d$host)
  )
  section("Awards", table_plain(rows))
}

## 3. Experience
build_experience <- function(d) {
  d <- d |>
    mutate(across(c(date, job, aff), ~ ifelse(is.na(.) | !nzchar(trimws(.)), NA_character_, .))) |>
    tidyr::fill(date, job, aff, .direction = "down")

  d <- ensure_cols(d, c("project", "supervisor", "duties"))
  role_groups <- split(d, factor(paste(d$date, d$job, d$aff), levels = unique(paste(d$date, d$job, d$aff))))
  rows <- c()

  for (sub_d in role_groups) {
    first_row <- sub_d[1, ]
    full_title <- lead_bold(paste0(first_row$job, ", ", first_row$aff))

    for (i in seq_len(nrow(sub_d))) {
      proj <- clean(sub_d$project[i])
      sup <- clean(sub_d$supervisor[i])
      duts <- duties_list(sub_d$duties[i])

      proj_html <- if (nzchar(proj)) {
        sprintf('<div class="proj-head"><span class="proj-title">Project: %s</span></div>', esc(proj))
      } else {
        ""
      }

      sup_html <- if (nzchar(sup)) {
        sprintf('<div class="proj-sup">Supervisor: %s</div>', esc(sup))
      } else {
        ""
      }

      date_col <- if (i == 1) esc(first_row$date) else ""
      title_lead <- if (i == 1) sprintf('<div style="margin-bottom: 2px;">%s</div>', full_title) else ""

      body_content <- paste0(title_lead, proj_html, sup_html, duts)

      rows <- c(rows, sprintf(
        '<tr><td class="c-when-wide">%s</td><td style="padding-bottom: 5px;">%s</td></tr>',
        date_col, body_content
      ))
    }
  }

  section("Experience", table_plain(rows))
}

## 4. Conferences (Fixed column layout & subgroup row)
build_conferences <- function(d) {
  if (is.null(d) || nrow(d) == 0) {
    return("")
  }
  d <- ensure_cols(d, c("category", "year", "title", "event", "location", "authors", "url"))

  cat_levels <- unique(d$category[nzchar(trimws(d$category))])
  groups <- split(d, factor(d$category, levels = cat_levels))
  rows <- c()

  for (cat_name in names(groups)) {
    sub_d <- groups[[cat_name]]
    sub_d <- arr_desc(sub_d, "year")

    # Subgroup title: keep 2 distinct td cells so table column sizing never collapses
    rows <- c(rows, sprintf(
      '<tr class="cv-subgroup"><td class="c-when-narrow"></td><td>%s</td></tr>',
      esc(cat_name)
    ))

    for (i in seq_len(nrow(sub_d))) {
      yr <- esc(sub_d$year[i])
      ttl <- esc(sub_d$title[i])
      ev <- esc(sub_d$event[i])
      loc <- esc(sub_d$location[i])
      authors <- clean(sub_d$authors[i])
      u <- clean(sub_d$url[i])

      title_disp <- if (nzchar(u)) {
        sprintf('<a href="%s" target="_blank"><i>%s</i></a>', esc(u), ttl)
      } else {
        sprintf("<i>%s</i>", ttl)
      }

      author_prefix <- if (nzchar(authors)) {
        auth_str <- esc(authors)
        auth_str <- gsub("Duc Manh Nguyen", "<b>Duc Manh Nguyen</b>", auth_str, fixed = TRUE)
        auth_str <- gsub("Nguyen Duc Manh", "<b>Nguyen Duc Manh</b>", auth_str, fixed = TRUE)
        sprintf("%s. ", auth_str)
      } else {
        ""
      }

      event_loc <- if (nzchar(loc)) sprintf('%s, <span class="cv-meta">%s</span>', ev, loc) else ev
      details <- sprintf("%s%s. %s.", author_prefix, title_disp, event_loc)

      rows <- c(rows, sprintf(
        '<tr><td class="c-when-narrow">%s</td><td style="padding-bottom: 5px;">%s</td></tr>',
        yr, details
      ))
    }
  }

  section("Conferences & Presentations", table_plain(rows))
}

## 5. Other Activities
build_activities <- function(d) {
  if (is.null(d) || nrow(d) == 0) {
    return("")
  }
  d <- ensure_cols(d, c("year", "role", "event", "host", "url"))

  rows <- apply(d, 1, function(row) {
    yr <- esc(row["year"])
    role <- esc(row["role"])
    ev <- esc(row["event"])
    host <- esc(row["host"])
    u <- clean(row["url"])

    event_disp <- if (nzchar(u)) sprintf('<a href="%s" target="_blank">%s</a>', esc(u), ev) else ev
    details <- sprintf('<span class="t">%s</span> &mdash; %s, <span class="cv-meta">%s</span>', role, event_disp, host)

    sprintf('<tr><td class="c-when-narrow">%s</td><td style="padding-bottom: 5px;">%s</td></tr>', yr, details)
  })

  section("Other Activities", table_plain(rows))
}

## 6. Publication

# ZOTERO_USER_ID="11220119"
# ZOTERO_API_KEY="jLLSEgera0fKJoFS5b89dGwN"
# ZOTERO_COLLECTION_KEY="GTYWXBTY"

# fetch: publications (Zotero)

## ---------------- small helpers ----------------
`%or%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a

ensure_cols <- function(df, cols) {
  for (cl in cols) if (!cl %in% names(df)) df[[cl]] <- NA_character_
  df
}

# Define bold_name FIRST so build_publications has access to it
bold_name <- function(x) {
  x <- gsub("\\bNguyen M([A-Z]*)", "<b>Nguyen M\\1</b>", x)
  x <- gsub("\\bNguyen Duc M\\b", "<b>Nguyen Duc M</b>", x)
  x
}

process_authors <- function(creators) {
  if (is.null(creators) || !is.data.frame(creators) || nrow(creators) == 0) {
    return(NA_character_)
  }

  # Ensure necessary columns exist in the creators data frame
  creators <- ensure_cols(creators, c("creatorType", "lastName", "firstName", "name"))

  a <- creators[creators$creatorType %in% c("author"), , drop = FALSE]
  if (nrow(a) == 0) {
    return(NA_character_)
  }

  one <- vapply(seq_len(nrow(a)), function(i) {
    ln <- a$lastName[i]
    if (is.na(ln) || !nzchar(trimws(ln))) {
      return(ifelse(is.na(a$name[i]), "", trimws(a$name[i])))
    }
    fn <- ifelse(is.na(a$firstName[i]), "", trimws(a$firstName[i]))
    ini <- if (nzchar(fn)) {
      paste0(substr(unlist(strsplit(fn, "[- ]")), 1, 1), collapse = "")
    } else {
      ""
    }
    trimws(paste(ln, ini))
  }, character(1))

  paste(one[nzchar(one)], collapse = ", ")
}

fetch_pubs <- function() {
  key <- Sys.getenv("ZOTERO_API_KEY")
  usr <- Sys.getenv("ZOTERO_USER_ID")
  col <- Sys.getenv("ZOTERO_COLLECTION_KEY")

  if (!nzchar(key) || !nzchar(usr) || !nzchar(col)) {
    stop("ZOTERO_API_KEY, ZOTERO_USER_ID, or ZOTERO_COLLECTION_KEY is missing from your .Renviron")
  }

  url <- sprintf("https://api.zotero.org/users/%s/collections/%s/items", usr, col)
  r <- httr::GET(
    url,
    query = list(format = "json", limit = 100),
    httr::add_headers("Zotero-API-Key" = key),
    httr::timeout(60)
  )

  if (httr::status_code(r) != 200) {
    stop("Zotero API returned status code: ", httr::status_code(r))
  }

  parsed <- jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"))
  z <- parsed$data
  if (is.null(z) || !is.data.frame(z) || nrow(z) == 0) {
    message("cv: Zotero collection came back empty.")
    return(NULL)
  }

  z <- ensure_cols(z, c(
    "itemType", "title", "date", "journalAbbreviation",
    "publicationTitle", "volume", "issue", "pages", "DOI"
  ))

  z$authors <- if ("creators" %in% names(z)) {
    vapply(z$creators, function(cr) {
      if (is.data.frame(cr) && nrow(cr) > 0) process_authors(cr) else NA_character_
    }, character(1))
  } else {
    NA_character_
  }

  z[, c(
    "itemType", "authors", "title", "date", "journalAbbreviation",
    "publicationTitle", "volume", "issue", "pages", "DOI"
  )]
}

build_publications <- function(p) {
  if (is.null(p) || nrow(p) == 0) {
    return("")
  }

  p <- p |> dplyr::filter(itemType == "journalArticle")
  n_pubs <- nrow(p)
  if (n_pubs == 0) {
    return("")
  }

  sub_lead <- sprintf(
    '<p class="cv-lead">%d peer-reviewed journal article%s.</p>',
    n_pubs,
    if (n_pubs > 1) "s" else ""
  )



  pdate <- to_date(p$date)
  pyear <- ifelse(is.na(pdate), str_extract(p$date, "[0-9]{4}"), format(pdate, "%Y"))
  pyear <- ifelse(is.na(pyear), "", pyear)

  key <- pdate
  key[is.na(key)] <- suppressWarnings(ymd(paste0(pyear[is.na(key)], "-06-30")))
  ord <- order(key, decreasing = TRUE, na.last = TRUE)
  p <- p[ord, , drop = FALSE]
  pyear <- pyear[ord]

  jr <- clean(p$journalAbbreviation)
  jr <- ifelse(nzchar(jr), jr, clean(p$publicationTitle))
  jr <- stringr::str_squish(jr)

  title <- stringr::str_squish(p$title)
  vol <- stringr::str_squish(clean(p$volume))
  iss <- stringr::str_squish(clean(p$issue))
  pg <- stringr::str_squish(clean(p$pages))

  core <- ifelse(nzchar(vol), paste0(vol, ifelse(nzchar(iss), paste0("(", iss, ")"), "")), "")
  vp <- ifelse(nzchar(core) & nzchar(pg), paste0(core, ":", pg),
    ifelse(nzchar(core), core, pg)
  )

  tail <- ifelse(nzchar(vp), paste0(esc(pyear), ";", esc(vp), "."), paste0(esc(pyear), "."))

  doi <- clean(p$DOI)
  doi_a <- ifelse(nzchar(doi),
    sprintf(' <a href="https://doi.org/%s" target="_blank">[DOI]</a>', esc(doi)), ""
  )

  authors <- bold_name(esc(stringr::str_squish(p$authors)))
  ref <- sprintf("%s. %s. <i>%s</i>. %s%s", authors, esc(title), esc(jr), tail, doi_a)

  # 1. Inline padding-bottom on the li ensures Chrome PDF rendering cannot collapse it
  items <- paste(sprintf('<li style="padding-bottom: 12px; margin-bottom: 4px;">%s</li>', ref), collapse = "\n")

  # 2. Use class="cv-pub-list" rather than class="cv-pubs"
  section("Publications", sprintf('%s\n<ol class="cv-pub-list">\n%s\n</ol>', sub_lead, items))
}
## ---------------- assemble + write HTML ----------------
active_sections <- list(
  build_education(edu),
  build_award(award),
  build_experience(employ),
  if (!is.null(conferences)) build_conferences(conferences) else NULL,
  if (!is.null(activities)) build_activities(activities) else NULL,
  build_publications(pubs)
)

# Remove any NULL or empty sections
active_sections <- active_sections[!vapply(active_sections, is.null, logical(1))]
active_sections <- active_sections[vapply(active_sections, nzchar, logical(1))]

sections <- paste(active_sections, collapse = "\n\n")

tmpl <- paste(readLines(template, warn = FALSE), collapse = "\n")
if (!grepl("{{SECTIONS}}", tmpl, fixed = TRUE)) {
  stop("cv-template.html is missing the {{SECTIONS}} placeholder")
}

html <- sub("{{SECTIONS}}", sections, tmpl, fixed = TRUE)
html <- sub("{{UPDATED}}",
  paste(month.name[as.integer(format(Sys.Date(), "%m"))], format(Sys.Date(), "%Y")),
  html,
  fixed = TRUE
)

writeLines(html, out_html, useBytes = TRUE)
message("cv: wrote ", out_html)

## ---------------- PDF via chromote ----------------
if (!requireNamespace("chromote", quietly = TRUE)) stop("Install 'chromote'")
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install 'jsonlite'")

in_abs <- normalizePath(out_html, mustWork = TRUE)
out_abs <- file.path(dirname(in_abs), basename(out_pdf))
if (file.exists(out_abs)) file.remove(out_abs)

b <- chromote::ChromoteSession$new()
on.exit(
  {
    try(b$close(), silent = TRUE)
  },
  add = TRUE
)

b$Page$enable()
message("cv: loading HTML into headless Chrome...")
b$Page$navigate(paste0("file://", in_abs), wait_ = TRUE)

# Wait for fonts (EB Garamond) to rasterize
Sys.sleep(1.5)

footer_html <- paste0(
  '<div style="font-family: \'EB Garamond\', Georgia, serif; font-size: 8.5pt; color: #5f5952; ',
  "font-style: italic; width: 100%; box-sizing: border-box; display: flex; ",
  "justify-content: space-between; align-items: center; padding: 0 15mm 4mm 15mm; ",
  '-webkit-print-color-adjust: exact;">',
  '<span style="border-top: 0.75px solid #d9d4cf; width: 100%; display: flex; justify-content: space-between; padding-top: 4px;">',
  "<span>Manh Nguyen Duc &mdash; Curriculum Vitae</span>",
  '<span>Page <span class="pageNumber"></span> of <span class="totalPages"></span></span>',
  "</span>",
  "</div>"
)

message("cv: rendering PDF via CDP...")
pdf_res <- b$Page$printToPDF(
  displayHeaderFooter = TRUE,
  headerTemplate      = "<span></span>",
  footerTemplate      = footer_html,
  printBackground     = TRUE,
  preferCSSPageSize   = TRUE
)

writeBin(jsonlite::base64_dec(pdf_res$data), out_abs)

if (file.exists(out_abs) && file.size(out_abs) > 0) {
  message("cv: successfully wrote ", out_pdf, " (", round(file.size(out_abs) / 1024, 1), " KB)")
} else {
  warning("cv: PDF generation failed.")
}
