### This script generates a figure of the brain where regions that mediate
### the association between violence and internalizing symptoms are highlighted
###
### Ellyn Butler
### February 21, 2023

from nilearn import plotting

# Amygdalae (244, 245):
#19.51_-1.85_-23.11
#-20.3 -2.27 -22.21

###### IE.M2

#    matnames  regnames
#2       M2.2   region2 -24.66_-97.84_-12.33    17Networks_LH_VisCent_ExStr_2 (primary visual cortex)
#12     M2.12  region14 -46.68_-50.91_-20.91    17Networks_LH_DorsAttnA_SPL_1 (dorsal attention network - inferior temporal gyrus)
#234   M2.235 region237 24.73_-11.25_-22.68     hippocampus
#256   M2.257 region261 -18.84_-5.08_-3.06      pallidum
#271   M2.274 region281 -5.72_-50.80_-40.84     medial inferior cerebellum

# Just hippocampus
med_coords = [(19.51, -1.85, -23.11),
              (-20.3, -2.27, -22.21),
              (24.73, -11.25, -22.68)]

view = plotting.view_markers(
    med_coords, ['red', 'red', 'black'], marker_size=12)

view.open_in_browser()
