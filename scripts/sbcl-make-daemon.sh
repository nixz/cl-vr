#!/bin/sh
sbcl --eval "(ql:quickload :cl-vr)" --eval "(cl-vr:make-daemon)"
