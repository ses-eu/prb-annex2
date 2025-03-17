# PRB Monitoring website
This is the repository for the PRB monitoring website, owned by the PRB and managed by EUROCONTROL.
The PRB monitoring website is published on sesperformance.eu.

For more information contact oscar.alfaro@eurocontrol.int.

# Production
The master branch is **main**, so be **CAREFUL** about the changes to make on this branch.

[placeholder for explanation of copying files to sesperformance]

# Concrete steps to publish to production
There are 2 possible scenarios:

1. merge from a development branch where docs has already been properly generated

2. change directly in master and regenerate either the whole site, or the pages (States/years) affected by the change

Option 2. should only be used for of emergency/tiny changes not worth the overhead of creating a new branch and develop/PR/test/merge...(even if it is a safer way of working.)

Option 1. is more for a release preparation where different contributions eventually from different branches have been combined for publication in next release.

# Development
## Development of a feature
Development of a feature or change in the dashboards can happen in each person's favorite branch. When the changes are deemed ready for next release they can be merged in the relevant release branch.

## Development for a release
Development for a release happens in a branch conventionally named like `YYYYMM-release` for a planned release in month `MM` of year `YYYY`.

[To be replaced by explanation of how to use the test site
You can have a preview of the development branch if you create a pull request out of changes pushed on the corresponding branch. The preview is published automatically via Netlify at a URL like

> `https://deploy-preview-DIGITS--sesperformance.netlify.app/`

for the PR of number DIGITS, i.e.

https://deploy-preview-1--sesperformance.netlify.app/

for PR #1.
]

# Website/pdf generation
The dashboard is generated from templated Quarto documents (.qmd). There are different templates for the different types of pages to be generated, i.e. SES RP3, Network Manager, MUAC, State and Home page. There are also different templates for the generation of the .pdf documents.

Each State/year combination is a fully independent quarto website, although it is all designed to look like an integrated website.

The master files .qmd, .yml and .css files are stored in the folder `\_original_files`.

The steps to which are laid out in the script `build_site.R`.

The script `create_pages.R` sets up the different quarto framework files (.yml, .qmd, etc) depending on the type of State/stakeholder.

# Source data/text
The source data and text feeding this dashboard is provided by, and the responsibility of, PETSCO.

The data and the text are stored in the folders defined in the `build_site.R` script:
1. `data_folder`: contains all data and the text for the PRB conclusions
2. `data_folder_a2`: contains all the text for the NSA input boxes in the dashboard

Note that if the folder 2. were to be moved to another location, the structure and names of its subfolders for the different years and KPAs should be kept so as the code can find them. 


# Publication
Go to [app.netlify.com](app.netlify.com) and login with email with the user performancereviewunit@gmail.com. Enrico and Oscar have the password.
Select 'sesperformance.eu' in the list of sites, go to 'deploys' and in the 'drag & drop' section just drop the production folder, typically `r fixed('\\ihx-vdm05\LIVE_var_www_performance$\oscar\prb-monitoring-prod')`.
