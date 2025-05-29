# quarto-templates project

This project folder contains all the supporting files to enable individual contributors to the NBA website to preview what their pages will look like in html format. Do not move, rename or edit any of these files or folders, but remember to periodically check for the latest versions on the [SANBI-NBA GitHub repo](https://github.com/SANBI-NBA/templates/).

The scientific.qmd quarto document template makes use of functions in the [nbaR package](https://github.com/SANBI-NBA/nbaR), so make sure you have an updated version of that package installed (i.e. reinstall it if you get any errors such as 'could not find function "nba_citation"), following the installation instructions on its landing page.

> [!TIP] 
> When working on your Quarto pages in RStudio, always start by opening the `quarto-templates.Rproj` file in this folder, which ensures that relative file paths are working correctly.

## Notes on the purpose and function of files and folders in this project

### \_brand.yml

[Brand](https://quarto.org/docs/authoring/brand.html) is a relatively new functionality (introduced to Quarto in 2024) that can be used to customize the appearance of documents using a single source file holding information on colour palettes, logos, typography, etc. This functionality does not work well yet, so at the moment this file is used to store the hex codes of NBA colours as a reference source. Referring to the colours in .qmds does not work - one has to for now copy the hex codes to where they are needed.

### \_quarto.yml

This file contains yaml metadata that is applied across all .qmd files in the project. In addition, this file is used to set up website functionality such as navigation panes.

### custom.scss

This file contains custom style settings (font types, colours and styling) that overrides Quarto's default theme settings.

### partials

This folder contains html templates that overrule Quarto's default set up for the author-affiliation block.

### sanbi.csl

This file contains the format settings for styling inline citations and bibliography lists.
