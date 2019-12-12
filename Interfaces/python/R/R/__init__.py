# flake8: noqa

from .R import data_frame, pretty, ls, table, data_range, corr

from .plots import (
    abline,
    barplot,
    boxplot,
    hist,
    hosmer_lemeshow_plot,
    lines,
    plot,
    plot_dynamic_distribution,
    plot_many_ts,
    plot_grid_size,
    plot_ts,
    points,
    )

from .probability import dnorm, pnorm, qnorm, rnorm

from .stats import density