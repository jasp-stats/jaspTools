# setupJaspTools(jaspVersion = ...): which packages belong to a JASP version, and how they are installed.

manifest <- function(...) {
  m <- list(
    schema      = 1L,
    jaspVersion = "v0.99.1",
    jaspSyntax  = list(repo = "jasp-stats/jaspSyntax",   ref = "v0.99.1", releaseTag = "v0.99.1"),
    jaspDesktop = list(repo = "jasp-stats/jasp-desktop", ref = "v0.99.1", sha = "1204d1892b1233af145bb548fe80719f331ea693"),
    jaspBase    = list(repo = "jasp-stats/jaspBase",     sha = "71e9a4bc843d6d9e4ad5d7946b30b1cf0e6ef11b")
  )
  modifyList(m, list(...))
}

test_that("development is the default and needs no release", {
  withr::local_envvar(JASPTOOLS_JASP_VERSION = NA)
  expect_identical(defaultJaspVersion(), "development")
  withr::local_envvar(JASPTOOLS_JASP_VERSION = "v0.99.1")
  expect_identical(defaultJaspVersion(), "v0.99.1")

  local_mocked_bindings(downloadJaspVersionFile = function(...) stop("development must not download anything"))
  version <- readJaspVersion("development")
  expect_true(isDevelopmentJaspVersion(version))
  expect_identical(version$jaspDesktop$ref, "development")
})

test_that("a JASP version is read from the jasp-version.json of its release", {
  requested <- NULL
  local_mocked_bindings(downloadJaspVersionFile = function(jaspVersion) { requested <<- jaspVersion; manifest() })

  version <- readJaspVersion("v0.99.1")
  expect_identical(requested, "v0.99.1")
  expect_false(isDevelopmentJaspVersion(version))
  expect_identical(version$jaspSyntax$releaseTag, "v0.99.1")
  # jasp-desktop is downloaded by commit
  expect_identical(version$jaspDesktop$ref, "1204d1892b1233af145bb548fe80719f331ea693")
})

test_that("a JASP version that cannot be used gives a clear error", {
  expect_error(readJaspVersion(""), "must be")
  expect_error(readJaspVersion(c("a", "b")), "must be")
  expect_error(readJaspVersion("v0.99.1; rm -rf"), "is not a tag")

  local_mocked_bindings(downloadJaspVersionFile = function(jaspVersion) NULL)
  expect_error(readJaspVersion("v0.99.0"), "has no jasp-version.json")

  local_mocked_bindings(downloadJaspVersionFile = function(jaspVersion) manifest(schema = 2L))
  expect_error(readJaspVersion("v0.99.1"), "update jaspTools")

  local_mocked_bindings(downloadJaspVersionFile = function(jaspVersion) manifest(jaspBase = list(sha = NULL)))
  expect_error(readJaspVersion("v0.99.1"), "has no jaspBase.sha")
})

test_that("the packages of a JASP version are installed with the libraries of its release", {
  version <- validateJaspVersion(manifest(), "v0.99.1")

  installs <- list()
  local_mocked_bindings(
    isPkgLoaded = function(name) FALSE,
    installGithubPkg  = function(repo, quiet, force) {
      installs[[length(installs) + 1L]] <<- list(repo = repo, releaseTag = Sys.getenv("JASPSYNTAX_RELEASE_TAG", unset = NA))
    }
  )

  withr::local_envvar(JASPSYNTAX_RELEASE_TAG = "v-before")
  installJaspVersionPkgs(version, quiet = TRUE)

  expect_identical(installs, list(
    list(repo = "jasp-stats/jaspSyntax@v0.99.1",                                 releaseTag = "v0.99.1"),
    list(repo = "jasp-stats/jaspBase@71e9a4bc843d6d9e4ad5d7946b30b1cf0e6ef11b", releaseTag = "v-before")
  ))
  expect_identical(Sys.getenv("JASPSYNTAX_RELEASE_TAG"), "v-before")
})

test_that("the packages of a JASP version are not installed over loaded ones", {
  version <- validateJaspVersion(manifest(), "v0.99.1")
  local_mocked_bindings(
    isPkgLoaded = function(name) name == "jaspSyntax",
    installGithubPkg  = function(...) stop("must not install")
  )
  expect_error(installJaspVersionPkgs(version), "jaspSyntax is already loaded.*Restart R")
})

test_that("withEnvVars restores unset variables", {
  withr::local_envvar(JASPTOOLS_TEST_VAR = NA)
  expect_identical(withEnvVars(c(JASPTOOLS_TEST_VAR = "x"), Sys.getenv("JASPTOOLS_TEST_VAR")), "x")
  expect_true(is.na(Sys.getenv("JASPTOOLS_TEST_VAR", unset = NA)))
})
