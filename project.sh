#/bin/sh

if [ X"$1" != X ]; then
    sbcl --noinform --non-interactive --load "project.lisp" --eval "($1)"
else
    sbcl --noinform --load "project.lisp"
fi
