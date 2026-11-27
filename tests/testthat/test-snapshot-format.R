test_that("the standalone table opens with the default JASP number formats", {
  browsers <- c(Sys.getenv("CHROMOTE_CHROME"), Sys.which(c("google-chrome", "chromium", "chromium-browser", "chrome", "msedge")))
  browser <- unname(browsers[file.exists(browsers)])[1L]
  skip_if(is.na(browser), "headless Chromium is optional (set CHROMOTE_CHROME to its executable)")

  module <- withr::local_tempdir()
  path <- file.path(module, "tests", "testthat", "_snaps", "model", "coefficients.table.json")
  fields <- list(list(name = "term", type = "string"),
                 list(name = "b", type = "number", format = "sf:4;dp:3"),
                 list(name = "p", type = "pvalue", format = "dp:3;p:.001"),
                 list(name = "mixed", type = "mixed"))
  rows <- list(list(term = "00012.300", b = -1.7134593839651469, p = 1.2e-22,
                    mixed = list(type = "number", format = "dp:2", value = 1.23456)),
               list(term = "Slope", b = 9.99e-7, p = 0.13892,
                    mixed = list(type = "string", value = "fixed")))
  table <- list(schema = list(fields = fields), data = rows)
  writeSnapshotText(snapshotTableJSON(table), path)
  table$data[[1L]]$mixed$value <- 9.87654
  writeSnapshotText(snapshotTableJSON(table), file.path(dirname(path), "new.table.new.json"))
  report <- reviewSnapshots(module, file.path(module, "review.html"), open = FALSE, includeUnchanged = TRUE)

  # Exercise the cycle button on a first-run candidate and an approved reference.
  probe <- c('<output id="states"></output><script>',
    'const states = [], cycle = document.getElementById("cycle");',
    'for (const id of ["model/new.table.json", "model/coefficients.table.json"]) {',
    '  Array.from(document.querySelectorAll(".item")).find(b => b.firstChild.textContent === id).click();',
    '  if (cycle.hidden) throw Error("Missing precision control");',
    '  for (let i = 0; i < 2; i++) {',
    '    const value = document.querySelector(".snapshot-table tbody tr td:last-child").textContent;',
    '    states.push(document.getElementById("view-label").textContent + " " + value);',
    '    cycle.click();',
    '  }',
    '}',
    'document.getElementById("states").textContent = states.join(" | ");',
    '</script>')
  page <- readLines(report, encoding = "UTF-8", warn = FALSE)
  writeSnapshotText(paste(append(page, probe, after = match("</body>", page) - 1L), collapse = "\n"), report)

  dom <- file.path(module, "rendered.html")
  log <- file.path(module, "browser.log")
  # Hosted Ubuntu runners restrict Chrome's user namespaces. This browser
  # instance renders only the fixed fixture above in a temporary profile.
  hostedLinux <- identical(Sys.getenv("RUNNER_ENVIRONMENT"), "github-hosted") &&
    identical(Sys.getenv("RUNNER_OS"), "Linux")
  status <- system2(browser, c("--headless", "--no-first-run", "--disable-gpu", "--disable-background-networking",
                               if (hostedLinux) "--no-sandbox",
                               shQuote(paste0("--user-data-dir=", file.path(module, "profile"))),
                               "--dump-dom", shQuote(report)), stdout = dom, stderr = log, timeout = 120)
  expect_equal(status, 0L, info = paste(readLines(log, warn = FALSE), collapse = "\n"))
  if (status != 0L) return(invisible(NULL))
  html <- snapshotText(dom)
  table <- regmatches(html, regexpr('(?s)<table class="snapshot-table">.*?</table>', html, perl = TRUE))
  cells <- regmatches(table, gregexpr("<td[^>]*>[^<]*</td>", table))[[1L]]
  expect_identical(sub("^<td[^>]*>(.*?)</td>$", "\\1", cells),
    c("00012.300", "\u22121.713", "&lt;&nbsp;.001", "1.23",
      "Slope", "9.990\u00d710\u207b\u2077", ".139", "fixed"))
  expect_match(html, paste0('<output id="states">Candidate (formatted) 9.88 | Candidate (full precision) 9.87654',
    ' | Baseline (formatted) 1.23 | Baseline (full precision) 1.23456</output>'), fixed = TRUE)
})
