#!/bin/sh
erl -run dxf2js start ../DXF/$1.dxf $2 -run init stop -noshell > ../DXF/$1.html
