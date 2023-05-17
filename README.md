# Statistical population modelling for census support

[![DOI](https://zenodo.org/badge/383087930.svg)](https://zenodo.org/badge/latestdoi/383087930)

This github repo contains the raw teaching materials for the [**Statistical Population Modelling for Census Support workshop,**](https://wpgp.github.io/bottom-up-tutorial/) funded by the [United Nations Population Fund](<https://www.unfpa.org/>). It has been developed by the [WorldPop Research Group](<https://www.worldpop.org/>), University of Southampton and the [Leverhulme Center for Demographic Science](https://www.demography.ox.ac.uk/).

The repo consists in a series of tutorials in **Bayesian statistics for population modelling** with hands-on experience. It includes example code and other resources designed to expedite the learning curve.

The key concepts that are covered in the tutorial series include:
1.  Introduction to software for Bayesian statistical modelling:  R and Stan,
2.  Simple linear regression in a Bayesian context,
3.  Random effects to account for settlement type (e.g. urban/rural) and other types of stratification in survey data,
4.  Quantifying and mapping uncertainties in population estimates and
5.  Diagnostics to evaluate model performance (e.g. cross-validation).

The material has been used during an in-person workshop hosted by the Colombian National Administrative Department of Statistics in Bogota, Colombia in March 2023 and a remote workshop with the Brazilian Stats Office, Instituto Brasileiro de Geografia e Estatística (IBGE), in October 2021.

# Tutorials outline

-   [Introduction](https://github.com/wpgp/bottom-up-tutorial/tree/main/tutorials/)
-   [Tutorial 1](https://github.com/wpgp/bottom-up-tutorial/tree/main/tutorials/tutorial1/): How to think about population as a Bayesian?
-   [Tutorial 2](https://github.com/wpgp/bottom-up-tutorial/tree/main/tutorials/tutorial2/): How to model large-scale spatial variations?
-   [Tutorial 3](https://github.com/wpgp/bottom-up-tutorial/tree/main/tutorials/tutorial3/): How to model small-scale spatial variations?
-   [Tutorial 4](https://github.com/wpgp/bottom-up-tutorial/tree/main/tutorials/tutorial4/): Advanced model diagnostics and prediction
-   [Conclusion](https://github.com/wpgp/bottom-up-tutorial/tree/main/tutorials/)
-   [Workflow refresher](https://github.com/wpgp/bottom-up-tutorial/tree/main/tutorials/workflow/): Building a population model from start to end + a tale about hierarchical model parametrisation

# Folder content

The relevant materials are in the [`tutorials`](https://github.com/wpgp/bottom-up-tutorial/tree/main/tutorials) folder.

Each tutorial has its dedicated folder that contains:
- Tutorial material
  -  The `R` code
  -  The `stan` code
- Background files
  -  The `R markdown` code that has been used to produce the `html` page
  -  The `references.bib` with references used for the tutorial
  -  The `html` code for the website page  

The `data` folder contains outputs of the tutorial.

# Acknowledgements

The tutorials were written by Edith Darin from WorldPop, University of
Southampton and Douglas Leasure from Leverhulme Centre for Demographic
Science, University of Oxford, with supervision from Andrew Tatem, WorldPop, University of
Southampton.

Funding for the work was provided by the United Nations Population Fund (UNFPA), the Leverhulme Trust (RC-2018-003) and ESRC Impact Acceleration Account at the University of Oxford (2209-KEA-835).

# License

You are free to redistribute this document under the terms of a Creative Commons Attribution ([CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)) license.


# Suggested citation

Darin E, Leasure DR, Tatem AJ. 2023. Statistical population modelling for census support. United Nations Population Fund (UNFPA); Leverhulme Centre for Demographic Science, University of Oxford; and WorldPop, University of Southampton. https://wpgp.github.io/bottom-up-tutorial/, [doi:10.5281/zenodo.7945266](https://doi.org/10.5281/zenodo.7945266)

<br>

<br>


![alt](assets/pic/320px-UNFPA_logo.svg.png) ![alt](assets/pic/wp_logo_gray_low.png) ![alt](assets/pic/Ox_LCDS_logo_bw.png)
