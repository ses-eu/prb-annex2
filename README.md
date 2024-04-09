# PRB Monitoring website
This is the repository for the PRB monitoring website, owned by the PRB and managed by EUROCONTROL.

Production repo for PRB monitoring website published on sesperformance.eu.

For more information contact oscar.alfaro@eurocontrol.int.

# Production
The master branch is **PRODUCTION**, i.e. it contains what gets published on the Internet.

In particular the docs directory contains the generated dashboard pages that are automatically served by Netlify at

https://airport-dashboard.netlify.app/

So be aware that you need to be **CAREFUL** on `master`.

# Concrete steps to publish production
There are 2 possible scenarios:

1. merge from a development branch where docs has already been properly generated

2. change directly in master and regenerate the dashboard

Option 2. should only be used for of emergency/tiny changes not worth the overhead of creating a new branch and develop/PR/test/merge...(even if it is a safer way of working.)

Option 1. is more for a release preparation where different contributions eventually from different branches have been combined for publication in next release.

# Development
## Development of a feature
Development of a feature or change in the dashboards can happen in each person's favorite branch. When the changes are deemed ready for next release they can be merged in the relevant release branch.

(The preview possibilities via Netlify described below apply to any branch.)

## Development for a release
Development for a release happens in a branch conventionally named like `YYYYMM-release` for a planned release in month `MM` of year `YYYY`.

You can have a preview of the development branch if you create a pull request out of changes pushed on the corresponding branch. The preview is published automatically via Netlify at a URL like

> `https://deploy-preview-DIGITS--sesperformance.netlify.app/`

for the PR of number DIGITS, i.e.

https://deploy-preview-1--sesperformance.netlify.app/

for PR #1.

# Website generation
The dashboard is generated from templated Quarto documents (.qmd). There are different templates for the different types of pages to be generated, i.e. SES View, NM View, State View and overall website. The master `index.qmd` documents, together with the `_quarto.yml` that includes all page types, are stored in the folder `\_original_files`.

The steps to which are laid out in the script `build_site.R`.


