# Power-analysis

Data is obtained from the supplementary material of [Kormann et al.]( https://doi.org/10.1002/eap.2441) (2021), can be found [here](data/overview_herbicide_biodiv_effects.csv). The values do not stand for actual values of species richness, they are the estimated coeficients from linear mixed model outputs. SDs are calculated by dividing the difference between coef estimates and lower/upper 95% CI limits by 1.96.

Detailed code used for analysis is [here](src/03-0.05_intercept.R).

## Question1: Post-hoc power calculation

Detailed results see [here](results/03-power.csv).

## Question2: Minimum sample size required for 80% power

-   Woody, Light \~ Control

![Woody, Light Group versus Control](results/03_woody_CL.png)

-   Woody, Moderate \~ Control

![Woody, Moderate Group versus Control](results/03_woody_CM.png)

-   Woody, Intensive \~ Control

![Woody, Intensive Group versus Control](results/03_woody_CI.png)

-   Flower, Light \~ Control

![Flower, Light Group versus Control](results/03_flowr_CL.png)

-   Flower, Moderate \~ Control

![Flower, Moderate Group versus Control](results/03_flowr_CM.png)

-   Flower, Intensive \~ Control

![Flower, Intensive Group versus Control](results/03_flowr_CI.png)
