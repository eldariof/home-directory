#!/bin/sh
nvidia-settings -a InitialPixmapPlacement=2 -a GlyphCache=1 &
XLIB_SKIP_ARGB_VISUALS=1 
xset b off