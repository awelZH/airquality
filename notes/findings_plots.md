# Plots: decisions and findings

Read before working on plots or the report (see also decision 9 in `decisions.md`).

## Plots: grouped legend (2026-09-19)

`ggplot_emissions()` shows one legend block per sector, with the sector as title and the subsectors
without the pasted sector (was "Sektor / Subsektor"); the subsector names in the lookup table were
shortened accordingly by the user. Generic functions, since 2026-09-21 in `airquality.methods`
(decision 10):
* `grouped_key(group, key, order, group_order)` – unique key `"group::key"` as factor; its levels set
  the order of stack and legend ("verschiedene" exists in several sectors)
* `add_grouped_legend(plot, aesthetic, sep, spacing, subtitle, key_spacing)` – sets
  `legendry::guide_legend_group(key_group_split(sep))` plus the theme (block titles from the plot's
  `legend.text`, group spacing 3 mm, `legend.key.spacing.y` 0). The plot stays an ordinary ggplot.

The NH3 special case (agriculture last) is the argument `ggplot_emissions(sectors_last = )` instead
of `%+%` on the finished plot. **Rejected: blocks in two columns** (restore point, commit `3f759b1`):
`legendry` arranges blocks only in one row or column, so the columns had to be drawn separately and
placed with `guide_custom()` – legend twice as wide, fixed when drawn, and building grobs needed a
temporary `ragg` device (a pdf device leaves `Rplots.pdf` and does not know Arial from `theme_ts`).
With the native legend, NMVOC (6 blocks, 11 entries) just fits a 5 in high figure – check when
figures get smaller. Findings: `longpollutant()` is called without the `airquality.methods::` prefix
in several functions of `R/plot.R` (works only while the package is attached; fixed in
`ggplot_emissions()`); neighbouring subsectors of a sector can get similar shades because the colour
ramp is assigned over all pollutants (e.g. PM2.5 Haushalte).
