# Quarto folder

This folder contains all the files that must be edited by NBA content contributors. When sharing your NBA contribution, you only need to share this folder with the website compiler. Do not change the organisation of this folder - in other words, keep individual files where they are, otherwise it will make the website compilation (\>100 individual quarto files) much more difficult.

## Quarto templates

Quarto templates for contributing synthesis content to the NBA website are:

-   **basic.qmd** - a simple template for users contributing pages with mostly text and images as content.

-   **scientific.qmd** - a template containing all relevant functionality for scientific content such as author crediting, and example code blocks for figures and tables. It also lays out the general order of content for these pages.

## Working with templates

One .qmd file represents one page on the NBA website. It is up to you to decide how much content you want to include on each page. It is recommended that you follow the module structure to organise your content - **1 module = 1 .qmd file**. To create your NBA content, copy any one of these templates (keep them in this same folder), and rename the copy to clearly indicate which module or content is covered in the page.

**Remember to strictly follow file naming rules:**

1.  **No capital letters** in file names

2.  **No spaces** in file names - you may use dashes (-) or underscores (\_) instead

**Please use component prefixes** to help distinguish content that may have similar names (e.g. protection level or threat status):

| Component   | Recommended prefix |
|-------------|--------------------|
| Terrestrial | ter-               |
| Marine      | mar-               |
| Freshwater  | fw-                |
| Estuarine   | est-               |
| Coast       | cst-               |
| Species     | sp-                |
| Genetics    | gen-               |

### Editing the yaml header

Once you have created and renamed a quarto template, the first step is to edit the yaml header.

> [!WARNING\]
> yaml code is extremely sensitive to wrong code indentations (the space between the side of the screen and where the text begins). If you get any yaml-related errors when rendering, check that your yaml edits did not accidentally change indentations.

Follow these instructions for each template:

#### basic.qmd

``` yaml
---
title: "Basic template for synthesis content"
subtitle: "Add subtitle here"
title-block-banner: "#adb5bd"
title-block-banner-color: white
---
```

1.  Give your page a **title** - this is the text that will appear in the banner at the top of the page (compulsory).

2.  If your content has a **subtitle**, add it next to `subtitle:` - otherwise delete this line.

3.  Change the **background colour** of the banner to match your component. The background colour is set next to `title-block-banner:` - use the official hex codes in the nbaR package or in \_brand.yml in the root folder of the project. If your content is not related to any specific components (e.g. Key Messages, Integrated Findings) then keep the default grey colour (`"#adb5bd"`) here.

4.  DO NOT CHANGE `title-block-banner-color: white` - this sets the colour of the page title and subtitle text.

#### scientific.qmd

1.  Apply the same first 3 steps as for the basic template above.

2.  **Add authors**. Copy this code block and paste for each author of your page under `author:` (pay attention to indentations). Add author names in the order you want them listed in the citation.

``` yaml
  - name:
      given: Linda R.
      family: Harris
    orcid: 0000-0000-0000-0000
    affiliations:
      - ref: nmu
```

-   `given:` write out the author's first name in full. Add initials (with full stops) for middle names. No spaces between initials where multiple middle names.
-   `family:` the author's surname. For Afrikaans surnames, use capitalization as you would for name surname - e.g. Lara van Niekerk.
-   `orcid:` add the author's orcid ID here. If they do not have one, delete this line.
-   `affiliations:` add the id code of an affiliation listed under the `affiliations` block lower down next to `ref:`. It is possible to add multiple affiliations per author:

``` yaml
  - name:
      given: Lara
      family: van Niekerk
    orcid: 0000-0000-0000-0000
    affiliations:
      - ref: nmu
      - ref: csir
```

3.  **Add authors' affiliations**. Each unique affiliation needs to be listed only once. List and number affiliations in the order that they are associated with the author list above (as you would for a scientific paper submission). You can copy these code blocks for common affiliations (remember to set `number` to the correct number in the author-affiliation sequence).

**SANBI**

``` yaml

  - id: sanbi
    number: 
    name: South African National Biodiversity Institute
```

**NMU**

``` yaml

  - id: nmu
    number: 
    name: Nelson Mandela University
```

**CSIR**

``` yaml

  - id: csir
    number: 
    name: Council for Scientific and Industrial Research
```

**DFFE**

``` yaml

  - id: dffe
    number: 
    name: Department of Forestry, Fisheries and the Environment
```

**SANParks**

``` yaml

  - id: sanparks
    number: 
    name: South African National Parks
```

4.  **Rename the .bib file** something other than references.bib. Edit the reference to the .bib file in the yaml to match your .bib file name.

``` yaml
bibliography: references.bib
```

### Adding content

Page content is created below the yaml header (the end of the yaml header is indicated by three dashes `---`). There are no specific instructions regarding the structure and order of content for the basic template. For the scientific template, please follow the structure as specified in the template.

-   **Key statistics** - this section is set up following the format in NBA 2018. This section is optional - just delete the whole block if you don't want to include key statistics. If you just want to change the numbers, then use the visual editor to edit the statistics. If you want to add or remove columns or rows, it is best to change it in source mode. Make sure you use the correct inline styling option for the statistic you are reporting:

    -   **Threatened** - use `[00%]{.inline-style-threatened}`
    -   **Not protected** - use `[00%]{.inline-style-well-protected}`
    -   **Well protected** - use `[00%]{.inline-style-not-protected}`

-   **Summary** - use this section to highlight and summarise the key findings and messages of this page.

-   **Picture** - add a nice photograph illustrating something related to your page content. Remember to add a descriptive caption. Photographs are not included in numbered figure lists. To avoid the picture being given a number, leave ID field under picture Attributes blank. The picture must be saved in the imgs folder.

-   **Main content** - here is where you display and discuss maps, tables and graphs of indicators. Do not include excessive methodological details or other technical content - this section should contain content typically included in Results and Discussion sections of scientific papers. Provide appropriate headers as you see fit (do not use Header 1 - this is reserved for the page title). The template contains code snippets for recommended content such as tables, graphs, tabsets and information boxes. Copy and reuse these as you need.

-   **Technical documentation** - use this section to link to content describing the methods used to arrive at the indicators or other results discussed on this page. The template provides some examples of content that can be linked here. Adapt this section to your content as needed.

-   **Recommended citation** - this section contains a code block to generate a recommended citation for the page. It requires that the R packages `nbaR`, `knitr` and `rmarkdown` are installed. Do not edit this section - metadata from the page's yaml header are extracted to compose the citation - so just make sure you follow the instructions for capturing author details above, and this section should work.

-   **References** - if you insert any citations using `Insert > @ Citation` the citations will be collected in your .bib file and then added to this section. Note that this section is automatically generated when the file is rendered to html - do not edit anything here.

-   **Notes** - any footnotes added to your text will be listed here.

## Additional files and folders

### .bib file

Your folder must contain a .bib file if your page contains any literature citations. Quarto automatically collects the details of each reference in this file as you cite them. You can use the same .bib file for all your pages. Do not move this file to a different location.

The references.bib file included in this folder contains example references demonstrating how to structure different reference types to be compatible with the SANBI citation style.

### Input folders

The following folders must be included with all content submissions to the NBA website. Do not rename these folders and do not move them.

-   **data** - use this folder for storing small (\<50 MB) input data files. For example summary statistics in .csv format feeding into the generation of tables and graphs of headline indicators. Do not store any sensitive data here - these files will be publicly visible in the NBA website repository. For large or sensitive data files, store them in the NBA sharepoint folder and use the mklink function in Windows Command Prompt to create a virtual link to your data.
-   **imgs** - store all image files (.png or .webp) used within your quarto files here. Please reduce image resolution to a web-compatible size: maximum 800px width, and 72 DPI resolution.

Note that it is not necessary to create an output folder for the results of code chunks (such as Figures or Tables) unless you want to make these outputs available for download. Quarto will create the appropriate outputs when the website is rendered and insert it into the html.
