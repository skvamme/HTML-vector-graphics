#!/bin/sh
erl -run dxf2js start $1.dxf $2 -run init stop -noshell > $1.html
