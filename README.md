# Living Planet Index

Effect of population fluctuations on the Living Planet Index. This repository includes all the code used in the manuscript:

* Buschke, F.T., Hagan, J.G. Saniti, L. & Coetzee, B.W.T. (unpublished) *Random population fluctuations bias the Living Planet Index*.

The code was accurate as of 15 December 2020. For enquiries, contact `falko.buschk@gmail.com`

## General comments

This code relies heavily on the population data used in the to calculate the *Living Planet Index*. This data is available from the [dedicated website](http://stats.livingplanetindex.org/) by the **Zoolological Society of London**. The use of this data is restricted by their [data use policy](https://livingplanetindex.org/documents/data_agreement.pdf). Since I am not the owner of these data, I have no record of updates or changes to the dataset.

The code presented here also relies on the deicated `rlpi` package for calculating the *Living Planet Index*. This code is not one the official CRAN repository, so must be accessed and installed directly from the [rlpi GitHub repository](https://github.com/Zoological-Society-of-London/rlpi), which also requires the `devtools` package. The code needed to install these packages is included in the R-scripts.


## Repository structure

Most of the code includes dozen of hours of simulations, which are too laborious for a regular personal computer. Therefore, I recommend setting up  several cloud-based [RStudio servers using Amazon Web Services](https://www.louisaslett.com/RStudio_AMI/). 

The code has therefore been devided into folders depending on cloud-based simulations, or desktop-friendly code for recreating the Figures from the manuscript.

### Cloud Simulations folder

This folder includes three scripts:

  * `SimulationSpace.R`, which is a script needed to iterate combinations of starting populations and fluctuation size and calculates the Living Planet Index for each. This is used in **Figure 1d**.
  * `NonlinearTrajectories.R`, which is a script needed to simulate the null model used to account from random fluctuations in populations that decline along differnt trajectories. This is used in **Figure 4 e & f**.
  * `RandomDrift.R`, which is used to simulate how the empirical *Living Planet Index* would be affected if starting population fluctuated randomy with no obvious trend. This is used in **Figures 3 and S4**.
  * `NullModel.R`, which included the code needed to apply the randomisation null model to the empirical data used in the Living Planet Index. Outputs are used in **Figure 5**.
