#!/usr/bin/env sh

meson compile -C build && \
./build/cane && \
dot -Tpng cane.dot -o cane.png && \
nsxiv cane.png
