# setupJaspTools(jaspVersion = ...): how the jaspSyntax of a JASP version is installed, and which jasp-desktop commit
# the html files and data sets come from.

test_that("development is the default and needs no release", {
  withr::local_envvar(JASPTOOLS_JASP_VERSION = NA)
  expect_identical(defaultJaspVersion(), "development")
  withr::local_envvar(JASPTOOLS_JASP_VERSION = "v0.99.1")
  expect_identical(defaultJaspVersion(), "v0.99.1")

  local_mocked_bindings(jaspSyntaxReleaseExists = function(...) stop("development must not download anything"))
  version <- readJaspVersion("development")
  expect_true(isDevelopmentJaspVersion(version))
  expect_identical(version$jaspDesktop$ref, "development")
})

test_that("a JASP version is the tag of a jaspSyntax release", {
  requested <- NULL
  local_mocked_bindings(jaspSyntaxReleaseExists = function(jaspVersion) { requested <<- jaspVersion; TRUE })

  version <- readJaspVersion("v0.99.1")
  expect_identical(requested, "v0.99.1")
  expect_false(isDevelopmentJaspVersion(version))
  expect_identical(version$jaspVersion, "v0.99.1")
})

test_that("a JASP version that cannot be used gives a clear error", {
  expect_error(readJaspVersion(""), "must be")
  expect_error(readJaspVersion(c("a", "b")), "must be")
  expect_error(readJaspVersion("v0.99.1; rm -rf"), "is not a tag")

  local_mocked_bindings(jaspSyntaxReleaseExists = function(jaspVersion) FALSE)
  expect_error(readJaspVersion("v0.99.0"), "has no release v0.99.0")
})

test_that("the jaspSyntax of a JASP version is installed with the library of its release, and nothing else", {
  installs <- list()
  local_mocked_bindings(
    isPkgLoaded = function(name) FALSE,
    installGithubPkg  = function(repo, quiet, force) {
      installs[[length(installs) + 1L]] <<- list(repo = repo, releaseTag = Sys.getenv("JASPSYNTAX_RELEASE_TAG", unset = NA))
    }
  )

  withr::local_envvar(JASPSYNTAX_RELEASE_TAG = "v-before")
  installJaspVersionSyntax(list(jaspVersion = "v0.99.1"), quiet = TRUE)

  # jaspBase is left alone: the module's own jaspBase runs the analyses.
  expect_identical(installs, list(
    list(repo = "jasp-stats/jaspSyntax@v0.99.1", releaseTag = "v0.99.1")
  ))
  expect_identical(Sys.getenv("JASPSYNTAX_RELEASE_TAG"), "v-before")
})

test_that("the jaspSyntax of a JASP version is not installed over a loaded one", {
  local_mocked_bindings(
    isPkgLoaded = function(name) name == "jaspSyntax",
    installGithubPkg  = function(...) stop("must not install")
  )
  expect_error(installJaspVersionSyntax(list(jaspVersion = "v0.99.1")), "jaspSyntax is already loaded.*Restart R")
})

test_that("the resources come from the jasp-desktop commit the installed library was built from", {
  sha <- "1204d1892b1233af145bb548fe80719f331ea693"
  version <- list(jaspVersion = "v0.99.1")

  local_mocked_bindings(installedJaspSyntaxProvenance = function()
    c(release_tag = "v0.99.1", source_bundle_jasp_desktop_ref = "v0.99.1", source_bundle_jasp_desktop_sha = sha))
  expect_identical(installedJaspSyntaxDesktopSha(version), sha)

  local_mocked_bindings(installedJaspSyntaxProvenance = function() c(release_tag = "v1.0.beta.5f8133a"))
  expect_error(installedJaspSyntaxDesktopSha(version), "library of release v1.0.beta.5f8133a, not of v0.99.1")

  local_mocked_bindings(installedJaspSyntaxProvenance = function() c(release_tag = NA_character_))
  expect_error(installedJaspSyntaxDesktopSha(version), "release <unknown>, not of v0.99.1")

  local_mocked_bindings(installedJaspSyntaxProvenance = function() c(release_tag = "v0.99.1", source_bundle_jasp_desktop_sha = ""))
  expect_error(installedJaspSyntaxDesktopSha(version), "does not record which jasp-desktop commit")
})

test_that("the provenance of the installed jaspSyntax is read from the file configure writes", {
  skip_if_not(nzchar(system.file(package = "jaspSyntax")), "jaspSyntax is not installed")
  prov <- installedJaspSyntaxProvenance()
  expect_type(prov, "character")
  expect_true("release_tag" %in% names(prov))
})

test_that("withEnvVars restores unset variables", {
  withr::local_envvar(JASPTOOLS_TEST_VAR = NA)
  expect_identical(withEnvVars(c(JASPTOOLS_TEST_VAR = "x"), Sys.getenv("JASPTOOLS_TEST_VAR")), "x")
  expect_true(is.na(Sys.getenv("JASPTOOLS_TEST_VAR", unset = NA)))
})
