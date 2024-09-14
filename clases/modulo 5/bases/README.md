# Environmental heterogeneity on landslide slopes affects the long-term recoveries of forest ecosystem components

[https://doi.org/10.5061/dryad.m63xsj45n](https://doi.org/10.5061/dryad.m63xsj45n)

The raw data used in the following publication:
Hotta W, Morimoto J, Yanai S, Uchida Y, Nakamura F (2024) Environmental heterogeneity on landslide slopes affects the long-term recoveries of forest ecosystem components. CATENA 234: 107578 https://doi.org/10.1016/j.catena.2023.107578

We surveyed live trees, coarse woody debris, understory vegetation, and soil carbon and nitrogen.
We selected slopes where landslides occurred in 2018, 2003, 1981, and before 1947. We also selected reference stands.
We divided each slope into three zones (i.e., the initiation, the transport, and the deposition) from top to bottom.

## Description of the data and file structure

tree\_analysis\_revise.csv: the raw data of the live tree survey.
cwd2.csv: the raw data of the coarse woody debris survey.
understory\_analysis.csv: the raw data of the understory survey.
soil\_CN.csv: the raw data of carbon and nitrogen concentrations and bulk densities.
env\_quadrat\_data2.csv: the raw data of environmental variables (e.g., slope angle and soil moisture content)
plot\_slopeaspect.csv: the GIS-based data of the slope aspect of each plot.

## Data-specific information

Column headers of tree\_analysis\_revise.csv
\- Year: Year when a landslide occurred
\- LandslideID: ID of landslide slopes in the year
\- Zone: Zone within landslide slopes \(for landslides: Initiation/Transport/Deposition; for reference stands: Upper/Middle/Lower\)
\- Tree ID: ID of tree individuals
\- Species\_Jp: Tree species name in Japanese
\- DBH\_cm: Diameter at breast height \(cm\)
\- H\_m: Tree height \(m\)
\- BA: Basal area \(cm<sup>2</sup>)
\- BEF: Species\-age specific biomass extension factor
\- R: Root ratio
\- D1: Wood density
\- TimVol: Volume of a stem
\- AGBiomass\_mg: Aboveground biomass \(Mg\)

Column headers of cwd2.csv
\- Year: Year when a landslide occurred
\- LandslideID: ID of landslide slopes in the year
\- Zone: Zone within landslide slopes \(for landslides: Initiation/Transport/Deposition; for reference stands: Upper/Middle/Lower\)
\- CWDID: ID of CWD individuals
\- Da\_cm\, Db\_cm: Diameters of both ends of downed logs / Diameters at breast height of snags \(no data in column "Db\_cm"\)
\- L\_m: Length of downed logs or height of snags
\- Decay: Decay class \(1\, 2\, 3\, 4\, or 5\)
\- sp: Species \(conifer or broadleaves\)
\- type: CWD type \(downed logs or snags\)
\- Mass\_g: Mass of each CWD \(g\)

Column headers of understory\_analysis.csv
\- Year: Year when a landslide occurred
\- LandslideID: ID of landslide slopes in the year
\- Zone: Zone within landslide slopes \(for landslides: Initiation/Transport/Deposition; for reference stands: Upper/Middle/Lower\)
\- QuadratID: ID of quadrat in each plot
\- Species\_Jp: Species name of Japanese
\- Coverage\_%: percent cover of each species
\- Max\_H\_cm: Maximum height of each species \(cm\)
\- Species type: species habitat type \(Open land species/Generalist/Forest species/Unknown\)
\- Moisture type: species moisture preference \(Dry/Moderate/Wet/Unknown\)

Column headers of soil\_CN.csv
\- Year: Year when a landslide occurred
\- LandslideID: ID of landslide slopes in the year
\- Zone: Zone within landslide slopes \(for landslides: Initiation/Transport/Deposition; for reference stands: Upper/Middle/Lower\)
\- Type: Sample type \(L/FH/Twig/Mineral soils \(0\-5cm\)\)
\- Mass\_g: Dry weight of a sample including a sampling bag
\- bagmass\_g: Dry weight of a sampling bag
\- samplemass\_g: Dry weight of a sample
\- angle\_mean: Mean slope angle of a sampling plot
\- Carbon\_%: Carbon concentration
\- Nitrogen\_%: Nitrogen concentration
\- C\_mass\_mg: Carbon stock \(Mg\)
\- N\_mass\_mg: Nitrogen stock \(Mg\)

Column headers of env\_quadrat\_data2.csv
\- Year: Year when a landslide occurred
\- LandslideID: ID of landslide slopes in the year
\- Zone: Zone within landslide slopes \(for landslides: Initiation/Transport/Deposition; for reference stands: Upper/Middle/Lower\)
\- QuadratID: ID of quadrat in each plot
\- Angle: Slope angle in each quadrat \(degree; 0\-90\)
\- O\_depth: O layer depth in each quadrat \(cm\)
\- Moisture\_a\, Moisture\_b\, Moisture\_c\, Moisture\_d: Soil moisture content in each quadrat \(%; measured four times\)
\- Moisture\_mean: Mean soil moisture content \(%\)

Column headers of plot\_slopeaspect.csv
\- Year: Year when a landslide occurred
\- LandslideID: ID of landslide slopes in the year
\- Zone: Zone within landslide slopes \(for landslides: Initiation/Transport/Deposition; for reference stands: Upper/Middle/Lower\)
\- slope aspect: Slope aspect of each plot \(azimuth; 0\-360\)

## Sharing/Access information

All data here was collected by the authors.
If this data is used, please cite this data set and the following publication:
Hotta W, Morimoto J, Yanai S, Uchida Y, Nakamura F (2024) Environmental heterogeneity on landslide slopes affects the long-term recoveries of forest ecosystem components. CATENA 234: 107578 https://doi.org/10.1016/j.catena.2023.107578