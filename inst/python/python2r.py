#!/usr/bin/env python3
"""Export a Python-loaded abundance table for aRchiteutis.

The R helper ``python2r()`` reads the wide CSV this script writes (taxon names
in the first column, samples in the remaining columns) and returns a tidy
aRchiteutis table that can be plotted with any ``df2*`` function.

From a notebook or pipeline::

    from python2r import export_abundance
    export_abundance(df, "abundance.csv")          # pandas DataFrame
    export_abundance(matrix, "abundance.csv",
                     taxa=["Lactobacillus", "Bifidobacterium"],
                     samples=["s1", "s2", "s3"])

CLI (CSV/TSV in, aRchiteutis CSV out)::

    python3 python2r.py input.csv output.csv

Then in R::

    library(aRchiteutis)
    df <- python2r("abundance.csv", clade = "G")
    df2composition(df)
"""

from __future__ import annotations

import argparse
import csv
import json
import sys
from pathlib import Path
from typing import Any, Iterable, Optional, Sequence


def _as_list(values: Optional[Iterable[Any]], n: int, prefix: str) -> list[str]:
    if values is None:
        return [f"{prefix}{i + 1}" for i in range(n)]
    out = [str(v) for v in list(values)]
    if len(out) != n:
        raise ValueError(f"{prefix} names length {len(out)} != {n}")
    return out


def _from_pandas(data: Any) -> tuple[list[str], list[str], list[list[float]]]:
    taxa = [str(v) for v in data.index.tolist()]
    samples = [str(v) for v in data.columns.tolist()]
    rows = data.to_numpy().tolist()
    return taxa, samples, rows


def export_abundance(
    data: Any,
    path: str | Path,
    taxa: Optional[Sequence[Any]] = None,
    samples: Optional[Sequence[Any]] = None,
) -> Path:
    """Write a wide CSV suitable for ``aRchiteutis::python2r()``.

    ``data`` may be a pandas DataFrame (index = taxa, columns = samples), a
    numpy ndarray, a nested list, or a mapping of sample -> {taxon: value}.
    """
    path = Path(path)

    if hasattr(data, "to_numpy") and hasattr(data, "index") and hasattr(data, "columns"):
        row_names, col_names, rows = _from_pandas(data)
        if taxa is not None:
            row_names = _as_list(taxa, len(row_names), "taxon")
        if samples is not None:
            col_names = _as_list(samples, len(col_names), "sample")
    elif hasattr(data, "tolist") and not isinstance(data, (list, tuple, dict)):
        rows = data.tolist()
        if not isinstance(rows[0], (list, tuple)):
            rows = [rows]
        row_names = _as_list(taxa, len(rows), "taxon")
        col_names = _as_list(samples, len(rows[0]), "sample")
    elif isinstance(data, dict):
        # sample -> {taxon: value}  OR  taxon -> {sample: value}
        col_names = [str(k) for k in data.keys()]
        inner = data[next(iter(data))] if data else {}
        if hasattr(inner, "keys"):
            row_names = _as_list(
                taxa if taxa is not None else inner.keys(),
                len(inner) if taxa is None else len(taxa),
                "taxon",
            )
            rows = []
            for taxon in row_names:
                rows.append([float(data[s].get(taxon, 0) if hasattr(data[s], "get")
                                   else data[s][taxon]) for s in col_names])
            if samples is not None:
                col_names = _as_list(samples, len(col_names), "sample")
        else:
            raise TypeError("dict values must themselves be mappings of taxon -> count")
    else:
        rows = [list(r) for r in data]
        row_names = _as_list(taxa, len(rows), "taxon")
        col_names = _as_list(samples, len(rows[0]) if rows else 0, "sample")

    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.writer(handle)
        writer.writerow(["taxa", *col_names])
        for name, row in zip(row_names, rows):
            writer.writerow([name, *[("" if v is None else v) for v in row]])
    return path


def _read_table(path: Path) -> tuple[list[str], list[str], list[list[float]]]:
    sep = "\t" if path.suffix.lower() in {".tsv", ".txt"} else ","
    with path.open(newline="", encoding="utf-8") as handle:
        rows = list(csv.reader(handle, delimiter=sep))
    if not rows:
        raise ValueError(f"{path} is empty")
    header = rows[0]
    taxa = [r[0] for r in rows[1:]]
    samples = header[1:]
    values = [[float(v or 0) for v in r[1:]] for r in rows[1:]]
    return taxa, samples, values


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n", 1)[0])
    parser.add_argument("input", type=Path, help="Wide CSV/TSV (taxa in first column)")
    parser.add_argument("output", type=Path, help="Destination CSV for python2r()")
    parser.add_argument("--meta", type=Path, default=None,
                        help="Optional JSON dumped next to the CSV (not required)")
    args = parser.parse_args(argv)

    taxa, samples, rows = _read_table(args.input)
    export_abundance(rows, args.output, taxa=taxa, samples=samples)
    if args.meta is not None:
        args.meta.write_text(json.dumps({"taxa": taxa, "samples": samples}), encoding="utf-8")
    return 0


if __name__ == "__main__":
    sys.exit(main())
