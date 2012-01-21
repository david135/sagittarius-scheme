@; -*- mode:scribble; coding: utf-8 -*-

@section[:tag "srfi"]{Supporting SRFIs}

SRFI is a great libraries, so there is no reason not to support. Without
exception Sagittarius also supports several SRFIs. The following list is the
supported SRFI. Documents are not written for now. So if you need to refer the
functions, please look for SRFI's site. I might write it later.

For now, I just put pointer to @hyperlink[:href "http://srfi.schemers.org/"]{the SRFI's web site}

@table[]{
@tr{@th{SRFI number} @th{Library name}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-0/srfi-0.html"]{SRFI-0}}
    @td{(srfi :0 cond-expand)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-1/srfi-1.html"]{SRFI-1}}
    @td{(srfi :1 lists)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-2/srfi-2.html"]{SRFI-2}}
    @td{(srfi :2 and-let*)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-6/srfi-6.html"]{SRFI-6}}
    @td{(srfi :6 basic-string-ports)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-8/srfi-8.html"]{SRFI-8}}
    @td{(srfi :8 receive)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-13/srfi-13.html"]{SRFI-13}}
    @td{(srfi :13 strings)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-14/srfi-14.html"]{SRFI-14}}
    @td{(srfi :14 char-set)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-19/srfi-19.html"]{SRFI-19}}
    @td{(srfi :19 time)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-23/srfi-23.html"]{SRFI-23}}
    @td{(srfi :23 error)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-26/srfi-26.html"]{SRFI-26}}
    @td{(srfi :26 cut)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-37/srfi-37.html"]{SRFI-37}}
    @td{(srfi :37 args-fold)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-38/srfi-38.html"]{SRFI-38}}
    @td{(srfi :38 with-shared-structure)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-39/srfi-39.html"]{SRFI-39}}
    @td{(srfi :39 parameters)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-41/srfi-41.html"]{SRFI-41}}
    @td{(srfi :41 streams)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-42/srfi-42.html"]{SRFI-42}}
    @td{(srfi :42 eager-comprehensions)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-64/srfi-64.html"]{SRFI-64}}
    @td{(srfi :64 testing)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-98/srfi-98.html"]{SRFI-98}}
    @td{(srfi :98 os-environment-variables)}}
}

Each library can be imported like this:
@snipet{(import (srfi :1))}
So you don't have to type the long name.