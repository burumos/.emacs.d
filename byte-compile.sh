#!/bin/bash

emacs --batch -f batch-byte-compile init.el
emacs --batch -f batch-byte-compile ./elisp/*.el
