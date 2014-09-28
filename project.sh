#/bin/sh

if [ X"$1" != X ]; then
    sbcl --noinform --load "project.lisp" --non-interactive --eval "($1)"
else
    sbcl --noinform --load "project.lisp"
fi
