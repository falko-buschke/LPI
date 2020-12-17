# Living Planet Index

Effect of population fluctuations on the Living Planet Index. This repository includes all the code used in the manuscript:

* Buschke, F.T., Hagan, J.G. Santini, L. & Coetzee, B.W.T. (unpublished) *Random population fluctuations bias the Living Planet Index*.

The code was accurate as of 15 December 2020. For enquiries, contact `falko.buschk@gmail.com`

## General comments

This code relies heavily on the population data used in the to calculate the *Living Planet Index*. This data is available from the [dedicated website](http://stats.livingplanetindex.org/) by the **Zoolological Society of London**. The use of this data is restricted by their [data use policy](https://livingplanetindex.org/documents/data_agreement.pdf). Since I am not the owner of these data, I have no record of updates or changes to the dataset.

The code presented here also relies on the deicated `rlpi` package for calculating the *Living Planet Index*. This code is not on the official CRAN repository, so it must be accessed and installed directly from the [rlpi GitHub repository](https://github.com/Zoological-Society-of-London/rlpi), which also requires the `devtools` package. The code needed to install these packages is included in the R-scripts.


## Repository structure

Most of the code includes dozen of hours of simulations, which are too laborious for a regular personal computer. Therefore, I recommend setting up  several cloud-based [RStudio servers using Amazon Web Services](https://www.louisaslett.com/RStudio_AMI/). 

The code has, therefore, been divided into folders depending on cloud-based simulations, or desktop-friendly code for recreating the Figures from the manuscript.

### Cloud Simulations folder

This folder includes three scripts:

  * `SimulationSpace.R`, which is a script needed to iterate combinations of starting populations and fluctuation size and calculates the Living Planet Index for each. This is used in **Figure 1d**.
  * `NonlinearTrajectories.R`, which is a script needed to simulate the null model used to account from random fluctuations in populations that decline along differnt trajectories. This is used in **Figure 4 e & f**.
  * `RandomDrift.R`, which is used to simulate how the empirical *Living Planet Index* would be affected if starting population fluctuated randomy with no obvious trend. This is used in **Figures 3 and S4**.
  * `NullModel.R`, which included the code needed to apply the randomisation null model to the empirical data used in the Living Planet Index. Outputs are used in **Figure 5**.
  
### Figure folders

There is a dedicated folder for each figure in the manuscript. The folder names refer to the relevant figure. Each foler also includes the code and input dataset required to replicate the figure. A copy of the figure is also included in the folder. There are 10 folders:

* `Figure1`: The Living Planet Index of fluctuating, but otherwise stable, populations. Relies on the file `LPISpace.txt`.
* `Figure2`: Starting population sizes of time-series added to the Living Planet Index between 1950 and 2015. Cannot be replicated without the data from the [Living Planet Database](http://stats.livingplanetindex.org/).
* `Figure3`: The effect of random fluctuations on the Living Planet Index, assuming stable populations on average. Requires the files `LPI_global_empirical.txt`, `LPI_global_drift1.txt`, `LPI_global_drift3.txt`, and `LPI_global_drift5.txt`.
* `Figure4`: The effect of random fluctuations on the Living Planet Index (LPI) for simulated declining populations. Requires the file `IterationOutputCurve.txt`
* `Figure5`: The distribution of the Living Planet Index (LPI) in 2016 for a null model that reshuffles population trajectories. Requires several output files for the different planetary systems and biogrographical realms.

### Supplementary files folders

This is a folder that includes all the infomation needed for the Supplementary files. It is made up of the following sub-folders:

* `FigureS1`: The nine steps to calculating the Living Planet Index (LPI).
* `FigureS2`: Generalised additive (GAM) for populations declining from 100 to 40 individuals.
* `FigureS3`: Generalised additive (GAM) for populations increasing from 100 to 160 individuals.
* `FigureS4`: The estimated LPI for otherwise stable populations that fluctuate by 1% (a), 3% (b) and 5% (c) each year. Requres several files for the different planetary systems and biogrographical realms. 
* `FigureS5`: The reshuffling null model used to account for random population fluctuations.
