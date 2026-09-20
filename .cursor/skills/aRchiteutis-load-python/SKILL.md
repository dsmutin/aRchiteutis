---
name: aRchiteutis-load-python
description: Transfer a dataset loaded in Python (pandas DataFrame, numpy array, CSV) into R for aRchiteutis visualization via inst/python/python2r.py and python2r(). Use when the user loaded microbiome counts in Python and wants R ggplot/base plots.
---

# Python → R (`python2r`)

Default path (no reticulate required):

```python
# In the Python session that already holds the table
import sys
sys.path.append("path/to/aRchiteutis/inst/python")  # or copy python2r.py
from python2r import export_abundance
export_abundance(df, "abundance.csv")   # pandas: index=taxa, columns=samples
```

CLI:

```bash
python3 inst/python/python2r.py input.csv abundance.csv
```

Then in R:

```r
library(aRchiteutis)
df <- python2r("abundance.csv", clade = "G", legend = "legend.csv")
df2composition(df)
df2barplot(df)
```

Live objects (optional): if `reticulate` is installed, `python2r(py_df)` converts a pandas DataFrame in memory.

Bundled demo CSV: `system.file("extdata", "python_abundance.csv", package = "aRchiteutis")`.

Script path helper: `python2r_script()`.

Next: `aRchiteutis-transform` / plot skills. Full walkthrough: `aRchiteutis`.
