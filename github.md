# MacroTools — Repository Change Log

## Tag: `v0.0.0.9000-pre-cleanup`
**Commit:** `ca4547a`
**Date:** February 8, 2026
**Purpose:** Archives the last full version of the package before the April 2026 cleanup. This tag marks the state of the repo before the visualization functions and their heavy dependencies were removed.

To restore any file from this snapshot:
```r
git checkout v0.0.0.9000-pre-cleanup -- <file>
```

To restore the entire repo to this state:
```r
git checkout v0.0.0.9000-pre-cleanup
```

---

## Commit: `cebdb01` — Dropping functionality
**Date:** April 8, 2026
**Author:** Alfredo Romero

### Motivation
The package was taking nearly 10 minutes to install due to heavy visualization dependencies (`tmap`, `leaflet`, `webshot2`, `gt`, `cdlTools`). These were pulled in by three `create_*` functions that were non-core and could be dropped without affecting the package's primary purpose of fetching and transforming macroeconomic data.

Additionally, `get_from_BLS()` had long-standing unqualified function calls (no `package::function()` prefix) that required `tidyverse` to be attached before loading `MacroTools`. This was also fixed in this commit.

### Files Removed
| File | Reason |
|------|--------|
| `R/create_states_clickable_map.R` | Non-core; only user of `leaflet`, `cdlTools` |
| `R/create_unemp_heatmap.R` | Non-core; only user of `gt`, `webshot2` |
| `R/create_states_unemp_heatmap.R` | Non-core; only user of `tmap`, `cdlTools` |
| `man/create_states_clickable_map.Rd` | Auto-generated docs for removed function |
| `man/create_unemp_heatmap.Rd` | Auto-generated docs for removed function |
| `man/create_states_unemp_heatmap.Rd` | Auto-generated docs for removed function |

### Files Modified
**`DESCRIPTION`** — Removed 5 imports:
- `cdlTools`
- `leaflet`
- `gt`
- `webshot2`
- `tmap`

**`NAMESPACE`** — Removed 3 exports:
- `create_states_clickable_map`
- `create_states_unemp_heatmap`
- `create_unemp_heatmap`

**`R/get_from_BLS.R`** — Fixed 10 unqualified function calls that required `tidyverse` to be loaded before `MacroTools`:

| Before | After |
|--------|-------|
| `rowwise()` | `dplyr::rowwise()` |
| `mutate()` | `dplyr::mutate()` |
| `transmute()` | `dplyr::transmute()` |
| `first()` | `dplyr::first()` |
| `pull()` | `dplyr::pull()` |
| `map_dfr()` | `purrr::map_dfr()` |
| `as_tibble()` | `tibble::as_tibble()` |
| `ym()` | `lubridate::ym()` |
| `select()` | `dplyr::select()` |
| `pivot_wider()` | `tidyr::pivot_wider()` |

**`CLAUDE.md`** — Added project documentation and coding rules for Claude Code.
