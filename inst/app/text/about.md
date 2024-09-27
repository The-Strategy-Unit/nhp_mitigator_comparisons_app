## Purpose

Compare activity mitigator values selected by schemes within the National Hospital Programme (NHP) modelling process.

This application is a **work-in-progress**. Please send feedback to the [Data Science Team](mailto:mlcsu.su.datascience@nhs.net).

## Content

From the navigation bar above you can visit the:

* 'Information' tab (current tab), which provides background information and instructions
* 'Point-ranges' tab, which shows a plot of the selected schemes' 80% prediction-interval range and midpoint for the selected mitigators
* 'Heatmap' tab, which colours the upper, lower, midpoint and range values of the 80% prediction-interval selections made by the selected schemes, as well as a binary version to indicate simply whether a mitigator was selected or not
* 'Data' tab, which presents the full underlying data and lookups for schemes and mitigators

## Mitigator values

* Schemes were asked to select mitigators in the NHP Inputs app and to provide an 80% prediction interval (a lower and upper value) for the expected reduction in activity by their chosen horizon (final) year.
* A value of 1 assumes no reduction in activity, while a value of 0 assumes a total (100%) reduction.
* For example, a scheme may select 0.7 to 0.9 as their interval, from which we can define a midpoint of 0.8. These are the values represented in the point-range and heatmap charts.
* In some cases, schemes have made a point estimate rather than a range.
* For reference purposes, the point-range charts also include the results of the National Elicitation Exercise (NEE), but note that some mitigators were not part of that exercise.
