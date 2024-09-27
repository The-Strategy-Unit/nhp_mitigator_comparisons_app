# nhp_inputs_report_app

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

<!-- badges: end -->

## Purpose

An app to compare schemes' mitigator selections as part of the National Hospital Programme (NHP) modelling process. It's [deployed to Posit Connect](https://connect.strategyunitwm.nhs.uk/nhp/mitigator-comparisons/).

This tool is designed for use by model-relationship managers (MRMs) in discussion with schemes so that the mitigator selections can be refined before being finalised. Also, some of the scheme-level code here may support a standalone national-level report that's being developed separately.

This app replaces an original static report ([deployment](https://connect.strategyunitwm.nhs.uk/nhp/mitigators-comparison-report), [source](https://github.com/The-Strategy-Unit/nhp_peers_params)) that started to become tricky to manage and interpret without interactivity.

## Requirements

Some environmental variables are needed to fetch data, for example. You will need to add a `.Renviron` file to the project directory that contains the variables named in the `.Renviron.example` file. You can ask a member of the Data Science team for the values to populate this file. Remember to restart your session after you've updated your `.Renviron` file.

## Techincal

### Major packages

The app is built with [{shiny}](https://shiny.posit.co/), the [{golem}](https://thinkr-open.github.io/golem/) Shiny-as-a-package framework and [{bslib}](https://rstudio.github.io/bslib/) for theming.

### Mitigator data

The mitigators for each scheme's model scenarios are stored on Azure as large json files. One small element of these files is the 'params' item, which includes the mitigator selections.

To avoid the app having to read the entire json file for each scheme's model scenario, there is a system to pre-prepare the params alone. [A scheduled Quarto document on Posit Connect](https://connect.strategyunitwm.nhs.uk/nhp/tagged-runs-params-report/) has code to select the appropriate json file, extract the params and [save them as an RDS file to a pin](https://connect.strategyunitwm.nhs.uk/content/32c7f642-e420-448d-b888-bf655fc8fa8b/) on Posit Connect; it also saves [a CSV file to another pin](https://connect.strategyunitwm.nhs.uk/content/811dbaf9-18fe-43aa-bf8e-06b0df66004e/) that contains metadata about the model scenarios. The app then reads data from these pins using [the {pins} package](https://pins.rstudio.com/). A [blogpost by the Data Science team](https://the-strategy-unit.github.io/data_science/blogs/posts/2024-05-22-storing-data-safely/#posit-connect-pins) contains note on authenticating RStudio with Posit Connect, should you need to.

Schemes run many scenarios, but the app only displays data from a single json. The correct file is read because the MRMs tell the Data Science team which particular scenario should be labelled on Azure with a 'run stage' of 'initial', 'intermediate' or 'final'. There's [a handy lookup table](https://connect.strategyunitwm.nhs.uk/nhp/tagged_runs/nhp-tagged-runs.html) where you can see which files have been labelled for each scheme.

### Supporting data

Supporting data is fetched from Azure. This includes lookups for mitigators and schemes, as well as data from the National Elicitation Exercise (NEE).
