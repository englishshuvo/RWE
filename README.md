# Real-world evidence

Dr. Rishad Ahmed. PhD in Public Health, MBA.

This is a portfolio of reproducible real-world evidence and epidemiology analyses of public data. A study here starts with a protocol and an analysis plan, then keeps the code, the tests, and the aggregate results. The work is mine. It is not a statement from Nebraska DHHS or from any employer.

More studies are coming.

## Studies

| Study | Question | Data | Design | Result |
| --- | --- | --- | --- | --- |
| [Usual sleep and frequent mental distress, BRFSS 2022](brfss-2022-sleep-fmd/) | Among adults in the 50 states and the District of Columbia, how much more common is frequent mental distress in people who report usual sleep under 7 hours than in people who report 7 to 9 hours? | Public CDC BRFSS 2022 combined landline and cell-phone file, `LLCP2022XPT.zip`. 445,132 records. Zip SHA-256 `b6319c949b7289f8020fc903c3c2d38fe7e0b28ad703dcd81547f5cd0c0dcfeb`. | Complex survey. Weight `_LLCPWT`, strata `_STSTR`, PSU `_PSU`. | Among 407,452 adults, frequent mental distress was 23.43% (95% CI 22.97% to 23.89%) under 7 hours and 10.61% (95% CI 10.36% to 10.86%) at 7 to 9 hours. The difference is 12.82 percentage points (95% CI 12.29 to 13.35). |

Those figures are the ones stored in `brfss-2022-sleep-fmd/output/` (`key-strings.txt` and `results-fragment.md`). `provenance.json` in the same folder records the file, the checksum, the record count, and the package versions from that run.

## Reproduce

Download the public 2022 LLCP transport zip from CDC. Do not commit it.

https://www.cdc.gov/brfss/annual_data/2022/files/LLCP2022XPT.zip

From the repository root:

```bash
Rscript -e 'install.packages("renv", repos="https://cloud.r-project.org")'
Rscript -e 'renv::restore(project="brfss-2022-sleep-fmd", prompt=FALSE)'
Rscript brfss-2022-sleep-fmd/run.R
quarto render
```

`run.R` checks the zip against the pinned SHA-256, audits the columns and the design, and refits the prespecified contrasts. Respondent-level data stay in `brfss-2022-sleep-fmd/data/`, which is gitignored. The tests do not need the microdata:

```bash
Rscript brfss-2022-sleep-fmd/run.R test
```

`quarto render` writes the protocol, the analysis plan, the amendments, and the case study to `_site/`.

The code in this repository is under the MIT license. The BRFSS file is a work of the United States government and is not covered by that license.
