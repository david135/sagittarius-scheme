;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme lazy)
    (export lazy eager delay force)
    (import (srfi :45)))