# Power-analysis

Data is obtained from the supplementary material of [Kormann et al.]( https://doi.org/10.1002/eap.2441) (2021), can be found [here](data/overview_herbicide_biodiv_effects.csv). The values do not stand for actual values of species richness, they are the estimated coeficients from linear mixed model outputs. SDs are calculated by dividing the difference between coef estimates and lower/upper 95% CI limits by 1.96.

Detailed code used for analysis is [here](src/03-0.05_intercept.R).

## Question1: Post-hoc power calculation

Detailed results see [here](results/03-power.csv).

## Question2: Minimum sample size required for 80% power

-   Woody, Light \~ Control

![Woody, Light Group versus Control](results/03_woody_CL.png)

![Zoom in plot](results/03_woody_CL_finer.png)

Detailed data form can be found [here](results/03_woody_CL.csv). Minimum sample size (#Blocks) is **3**.

-   Woody, Moderate \~ Control

![Woody, Moderate Group versus Control](results/03_woody_CM.png)

![Zoom in plot](results/03_woody_CM_finer.png)

Detailed data form can be found [here](results/03_woody_CM.csv). Minimum sample size (#Blocks) is **3**.

-   Woody, Intensive \~ Control

![Woody, Intensive Group versus Control](results/03_woody_CI.png)

![Zoom in plot](results/03_woody_CI_finer.png)

Detailed data form can be found [here](results/03_woody_CI.csv). Minimum sample size (#Blocks) is **3**.

-   Flower, Light \~ Control

![Flower, Light Group versus Control](results/03_flowr_CL.png)

Detailed data form can be found [here](results/03_flowr_CL.csv). 

For further settling the exact value, I rerun the sample size from 90 to 110 one by one. Detailed data form can be found [here](results/03_flowr_finer_CL.csv). Minimum sample size (#Blocks) is **101**. Zoom-in plot shown below:

![Flower, Light Group, Zoom-in](results/03_flowr_finer_CL.png)

-   Flower, Moderate \~ Control

![Flower, Moderate Group versus Control](results/03_flowr_CM.png)

Detailed data form can be found [here](results/03_flowr_CM.csv).

For further settling the exact value, I rerun the sample size from 20 to 40 one by one. Detailed data form can be found [here](results/03_flowr_finer_CM.csv). Minimum sample size (#Blocks) is **22**. Zoom-in plot shown below:

![Flower, Moderate Group, Zoom-in](results/03_flowr_finer_CM.png)


-   Flower, Intensive \~ Control

![Flower, Intensive Group versus Control](results/03_flowr_CI.png)

![Zoom in plot](results/03_flowr_CI_finer.png)

Detailed data form can be found [here](results/03_flowr_CI.csv). Minimum sample size (#Blocks) is **4**.
