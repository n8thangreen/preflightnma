# preflightnma <img src="man/figures/logo.png" align="right" height="180" alt="preflightnma package logo" />

<br>

> Preflight checks for network meta-analysis.

## About

There is a significant gap in the literature regarding practical, visualization-heavy workflows for analysts performing preliminary NMA checks, in the spirit of empirical data analysis (EDA) for NMA.

Most existing literature focuses on the theoretical requirements for feasibility (e.g., assessing transitivity, similarity, and consistency) rather than the applied data science and visualization pipeline needed to make those assessments.

### Why this is a good idea

- The "Gap" in Methodology: Many researchers struggle with how to translate high-level feasibility guidelines into actual code and visualizations.

- Transparency and Reproducibility: There is an increasing demand for open-science practices in HTA. How to systematically interrogate data before committing to a full NMA (or to justify why one cannot be performed) is highly useful to the community.

- Utility for Practitioners: Junior researchers and statisticians often lack a "blueprint" for this stage. We provide a structured workflow (e.g., "Step 1: Network Graph Visualization," "Step 2: Baseline Risk Distribution Plotting," "Step 3: Effect Modifier Exploration").

## Usage

The workflow is:

- Visualizing connectivity: Basic network plots, emphasizing how to handle disconnected networks or multi-arm trials.

- Visualizing potential modifiers: Scatter plots or "strip charts" of patient/study characteristics across treatment arms.

- Visualizing consistency/heterogeneity: Funnel plots, forest plots of pairwise vs. network estimates, or "league table heatmaps" that flag potential issues.

## Contributing

Add guidelines for contributing here.

## License

Add your license information here.

## References

* **Dias, S., Welton, N. J., Sutton, A. J., & Ades, A. E. (2011).** *NICE DSU Technical Support Document 2: A Generalised Linear Modelling Framework for Pairwise and Network Meta-Analysis of Randomised Controlled Trials.* National Institute for Health and Care Excellence (NICE).
* **Dias, S., Welton, N. J., Sutton, A. J., Caldwell, D. M., Lu, G., & Ades, A. E. (2011).** *NICE DSU Technical Support Document 4: Inconsistency in Networks of Evidence Based on Randomised Controlled Trials.* National Institute for Health and Care Excellence (NICE).
* **Ades, A. E., Caldwell, D. M., Reken, S., et al. (2012).** *Evidence Synthesis of Treatment Efficacy in Decision Making: A Reviewer's Checklist [Internet]. NICE DSU Technical Support Document No. 7.* London: National Institute for Health and Care Excellence (NICE).
* **Jansen, J. P., Fleurence, R., Devine, B., Itzler, R., Barrett, A., Hawkins, N., ... & Altman, D. G. (2011).** Interpreting Indirect Treatment Comparisons and Network Meta-Analysis for Health-Care Decision Making: Report of the ISPOR Task Force on Indirect Treatment Comparisons Good Research Practices: Part 1. *Value in Health*, 14(4), 417-428.
* **Hoaglin, D. C., Hawkins, N., Jansen, J. P., Scott, D. A., Itzler, R., Cappelleri, J. C., ... & Barrett, A. (2011).** Conducting indirect-treatment-comparison and network-meta-analysis studies: report of the ISPOR Task Force on Indirect Treatment Comparisons Good Research Practices: part 2. *Value in Health*, 14(4), 429-437.
* **Cope, S., Jansen, J. P., Stevens, J. W., & Schmid, C. H. (2014).** A process for assessing the feasibility of a network meta-analysis: a case study of everolimus in combination with hormonal therapy versus chemotherapy for advanced breast cancer. *BMC Medicine*, 12, 93.
* **Donegan, S., Williamson, P., D'Alessandro, U., & Tudur Smith, C. (2013).** Assessing key assumptions of network meta-analysis: a review of methods. *Research Synthesis Methods*, 4(4), 291-323.
* **Chaimani, A., Higgins, J. P., Mavridis, D., Spyridonos, P., & Salanti, G. (2013).** Graphical tools for network meta-analysis in STATA. *PLoS One*, 8(10), e76654.
* **Cipriani, A., Higgins, J. P., Geddes, J. R., & Salanti, G. (2013).** Conceptual and technical challenges in network meta-analysis. *Annals of Internal Medicine*, 159(2), 130-137.
* **Salanti, G. (2012).** Indirect and mixed-treatment comparison, network, or multiple-treatments meta-analysis: many names, many benefits, many concerns for the next generation evidence synthesis tool. *Research Synthesis Methods*, 3(2), 80-97.
