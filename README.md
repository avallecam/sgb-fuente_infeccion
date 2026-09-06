# sgb-fuente_infeccion

Case-control analysis for a Guillain-Barré Syndrome (SGB) outbreak-response investigation
(Chiclayo/Lambayeque, Peru, 2019–2020), looking for the source of infection behind the outbreak.

> **Data is not included.** This repository documents the analysis code only — the KoboToolbox
> survey exports, cleaned datasets, and exported figures/tables are confidential
> public-health/outbreak-investigation data and are never committed (see the `README.md` in
> each data folder). There is no synthetic or shareable substitute, so **this repository is not
> reproducible from a clean clone**.

## Layout

- `kobo-raw/` — raw KoboToolbox survey exports (not committed)
- `data/` — cleaned intermediate data (not committed)
- `figure/` — exported maps/plots (not committed)
- `table/` — exported summary tables (not committed)

## Scripts

- `sample_size_sgb.R` / `sample_size_sgb.do` — sample size calculation for the case-control design
- `sgb-cleandb.R` — import and clean the survey data, then run the matched case-control analysis
  (bivariate `compareGroups` tables, age/sex-adjusted logistic models)
- `sgb_stats_clogit.do` — Stata cross-check of the conditional logistic regression (`clogit`)
- `osm_chiclayo.R` — market/slaughterhouse/farm location mapping around Chiclayo using OpenStreetMap
  data (`osmdata`, `nominatim`). Needs a Nominatim API key set as the `NOMINATIM_KEY` environment
  variable.

## Notes for the next field visit

- Record all market names as closed options in the questionnaire, to avoid free-text
  normalization work later (see the market-name matching in `sgb-cleandb.R`).

<!--
## References

- <https://ggplot2tutor.com/streetmaps/streetmaps/>
- <https://forum.openstreetmap.org/viewtopic.php?id=25490&p=11>
-->

## Acknowledgements

Many thanks to the Centro Nacional de Epidemiología, Prevención y Control de Enfermedades
[(CDC Perú)](https://www.dge.gob.pe/portalnuevo/) for the opportunity to work on this project.

## Authors

- [@avallecam](https://www.github.com/avallecam)

## License

[GPLv3](https://choosealicense.com/licenses/gpl-3.0/)
