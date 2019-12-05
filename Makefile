Main       = f95/main
Vis_2d     = f95/Vis_2d_cntr
Cmnvl      = f95/CmnVal_vis_vtk
Cmap       = f95/Setcmap_01
CmnVl_Cmap = f95/CmnVal_cmap
Out        = 1d


# Michi Macbook Pro
F95         = /usr/local/bin/gfortran-8 -fopenmp
PKG_CONFIG_ENV = PKG_CONFIG_PATH="/usr/local/Cellar/plplot/5.14.0_1/lib/pkgconfig:/usr/local/opt/libpng/lib/pkgconfig:/usr/local/opt/freetype/lib/pkgc\
onfig:/usr/local/opt/fontconfig/lib/pkgconfig:/usr/local/opt/libffi/lib/pkgconfig:/usr/local/opt/pcre/lib/pkgconfig:/usr/local/opt/glib/lib/pkgconfig:\
/usr/local/opt/pixman/lib/pkgconfig:/usr/local/opt/cairo/lib/pkgconfig:/usr/local/opt/isl/lib/pkgconfig:/usr/local/opt/mpfr/lib/pkgconfig:/usr/local/o\
pt/fribidi/lib/pkgconfig:/usr/local/opt/graphite2/lib/pkgconfig:/usr/local/opt/icu4c/lib/pkgconfig:/usr/local/opt/harfbuzz/lib/pkgconfig:/usr/local/op\
t/pango/lib/pkgconfig"
SOURCE = $(Cmnvl).f95 $(CmnVl_Cmap).f95 $(Main).f95 $(Vis_2d).f95 $(Cmap).f95
$(Out).o: $(SOURCE)
	$(F95) -o $@ $(SOURCE) $(shell $(PKG_CONFIG_ENV) pkg-config  --cflags --libs plplot-fortran) -lplfortrandemolib -I"/usr/local/Cellar/plplot/5.14.0_1/share/plplot5.14.0"/examples/fortran
