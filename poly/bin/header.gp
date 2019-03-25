unset surface 
set view map
set cntrparam levels disc 0
# set logscale zcb
# could also try setting monochrome png output?
set style increment userstyles 
# 0 gives error
set style line 1 linewidth 4;
set datafile nofpe_trap

unset border
unset colorbox
unset tics
unset key
# these have no effect in splot:
set lmargin 0
set rmargin 0
set tmargin 0
set bmargin 0
