# Commit Message Instructions

## Format

One single line. No body. No period at the end. Max 72 characters.

```
<subject>: <short summary>
```

The subject identifies *what was changed*. The summary states *what was done*.

---

## Subject Rules

### Single function edited

Use the function name with parentheses:

```
estimate_roc(): add chi-squared dissimilarity method
detect_peak_points(): correct off-by-one in window index
prepare_data(): add validation for empty community matrix
```

### Specific topic or feature area

Use a plain descriptive label matching the topic:

```
Peak detection: switch default method to GAM-based approach
Dissimilarity coefficients: add Bray-Curtis normalisation step
Age uncertainty: correct sampling from Bchron output
```

### Tests

```
tests: add edge-case coverage for detect_peak_points
tests: update snapshot for plot_roc after axis flip
tests: add zero-row community matrix test for prepare_data
```

### Documentation

```
docs: update estimate_roc() @param for new method argument
docs: add workflow-example vignette section on peak detection
docs: regenerate man/ after roxygen2 update
```

### Package infrastructure

```
DESCRIPTION: add gratia to Imports after mgcv refactor
NAMESPACE: regenerate after adding export for make_trend
pkgdown: update reference index groupings
ci: update R-CMD-check workflow to r-lib/actions@v2
```

### Non-code changes

```
renv: update lockfile after package upgrades
chore: add .Rbuildignore entry for vscode folder
vscode: update settings for commit message generation
copilot: add debugging instructions and r-coding skill
refactor: simplify peak detection logic in detect_sni
```

---

## Banned Words

Do not use: *enhance*, *feat*, *feature*, *fix* (use a specific verb instead,
e.g. *correct*, *remove*, *add*, *update*, *switch*, *adjust*, *replace*).

---

## Examples

```
estimate_roc(): add chi-squared dissimilarity method
detect_peak_points(): correct off-by-one in window index
prepare_data(): add validation for empty community matrix
Peak detection: switch default method to GAM-based approach
tests: add edge-case coverage for detect_peak_points
docs: update estimate_roc() @param for new method argument
DESCRIPTION: add gratia to Imports after mgcv refactor
chore: add .Rbuildignore entry for vscode folder
copilot: add r-coding and debugging instructions
```
