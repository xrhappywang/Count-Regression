# AI Appendix

I used Anthropic's Claude (via Claude Code) throughout this project. This was only my second time building an R package, so I leaned on Claude mostly for three things: setting up the package framework, reminding me of R syntax, and debugging error messages. I made all statistical decisions and wrote the narrative text myself.

---

## Package Setup

**Prompt:** Asked Claude how to start an R package and what each standard folder is for (`R/`, `man/`, `tests/`, `vignettes/`).

**Outcome:** Claude walked me through `usethis::create_package()` and explained the file structure. I built the package and named it `smartcount` myself.

---

## fit.R — `fit_count`

**Prompt:** Asked Claude to remind me of `stats::glm()`, `MASS::glm.nb()`, and `pscl::zeroinfl()` syntax, and how to organize a function with multiple model branches.

**Outcome:** Claude gave me an `if/else if` skeleton. I filled in each model type and wrote the input validation and `attr(fit, "smartcount_model")` tagging.

---

## summarize.R — `summarize_count_data`

**Prompt:** Asked Claude what statistics are useful for diagnosing whether Poisson is appropriate before fitting.

**Outcome:** Claude suggested mean, variance, var/mean ratio, zero proportion. I wrote the implementation and edge-case handling (e.g., NA returned when `mean(y) == 0`).

---

## condition.R — `check_conditions`

**Prompt:** Asked Claude to help me understand the formulas for Pearson dispersion ratio and randomized quantile residuals, and how to dispatch to model-specific helpers.

**Outcome:** Claude reminded me of the formulas and helped debug a `&` vs `&&` bug with vector comparisons. I wrote the dispatching logic and the diagnosis text following Professor Cipolli's lecture slides.

---

## interpretation.R — `interpret`

**Prompt:** Asked Claude how to extract `summary(fit)$coefficients` for both `glm` and `zeroinfl`, and how to detect interaction terms.

**Outcome:** Claude reminded me that `zeroinfl`'s summary is a list (`$count` + `$zero`) while `glm`'s is a matrix. For interactions, Claude pointed out the three cases (continuous × continuous, continuous × categorical, categorical × categorical) and the corresponding tools (`emmeans`, `emtrends`, `johnson_neyman`). I wrote the sentence-construction logic myself.

---

## modelEvaluation.R — `evaluate_model`

**Prompt:** Asked Claude how `anova()` differs across model types and when to use Vuong vs likelihood-ratio.

**Outcome:** Claude clarified that ZIP/ZINB needs Vuong (non-nested) and that Poisson vs NB needs a boundary-corrected LRT (p-value divided by 2). I wrote `compareModel()` and the McFadden and Deviance R² formulas from the lecture.

---

## Generalized Poisson Extension

**Prompt:** Asked Claude how `glmmTMB` objects differ from `glm` (since `coef()` returns an empty list).

**Outcome:** Claude pointed me to `glmmTMB::fixef(fit)$cond` and noted that `confint()` returns three columns. I wrote a separate `interpret_glmmTMB()` helper and updated the dispatch in `interpret()`, `check_conditions()`, and `evaluate_model()`.

---

## Vignette

**Prompt:** Asked Claude what sections an R-package vignette typically has.

**Outcome:** Claude proposed a 10-section structure. I wrote the narrative, chose the datasets (`bioChemists` and the Bangladesh ANC dataset from Akter et al. 2025, PLOS ONE), and decided how to present each section.

---

## Unit Tests

**Prompt:** Asked Claude to remind me of the `testthat` API.

**Outcome:** Claude introduced `expect_equal`, `expect_s3_class`, `expect_error`, etc. I wrote the actual test cases for each function.

---

## Debugging

Throughout development, I used Claude to interpret R error messages. Examples:

- `"object 'y' not found"` — I had deleted the line that defined `y`
- `"3 arguments passed to 'attributes<-' which requires 2"` — I used `attributes()` instead of `attr()`
- `"family 'quasipoisson' not implemented"` — `DHARMa::testZeroInflation` doesn't support Quasi-Poisson; wrapped in `tryCatch`
- `"could not find function 'check_conditions'"` — Missing `@export` on the main function
- `"Can't add p2 to a ggplot object"` — `patchwork` needed explicit `::` prefix
- `"subscript out of bounds"` — `glmmTMB`'s `confint` row names didn't match what I expected

In each case I located and made the fix myself once Claude pointed to the cause.

---

## What was entirely my own

- All statistical decisions (which models to support, which conditions to check, how to interpret)
- All function design (signatures, return values, error messages)
- All narrative writing (vignette, this appendix)
- Choice of datasets and the Bangladesh ANC paper
