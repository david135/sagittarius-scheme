Here you can see old release notes, usually nobody needs it. So I separated.

## Release Note 0.3.8 ##
Maintenance release. From this release R7RS draft 7 is supported.

Fixed bugs:
  * Local macro in `define-syntax` caused SEGV. [Issue 43](https://code.google.com/p/sagittarius-scheme/issues/detail?id=43).
  * Updating non-existing record signaled an error in `(dbd odbc)`. [Issue 48](https://code.google.com/p/sagittarius-scheme/issues/detail?id=48).
  * `get-bytevector-all` did not retrieve all data from socket port. [Issue 50](https://code.google.com/p/sagittarius-scheme/issues/detail?id=50).
  * `binary-port?` and `textual-port?` did not return #t for custom binary/textual port. [Issue 51](https://code.google.com/p/sagittarius-scheme/issues/detail?id=51).
  * Reader did not read #n= notation properly. [Issue 52](https://code.google.com/p/sagittarius-scheme/issues/detail?id=52).
  * `remainder` causes SEGV when the first argument was flonum. [Issue 53](https://code.google.com/p/sagittarius-scheme/issues/detail?id=53).
  * `(sqrt -1)` returned inexact value. [Issue 54](https://code.google.com/p/sagittarius-scheme/issues/detail?id=54).
  * `(zero? 0.0+0.0i)` returned #f. [Issue 55](https://code.google.com/p/sagittarius-scheme/issues/detail?id=55).
  * Single imaginary number changed its sign. [Issue 56](https://code.google.com/p/sagittarius-scheme/issues/detail?id=56).
  * Library version number did not accept 0. [Issue 57](https://code.google.com/p/sagittarius-scheme/issues/detail?id=57).
  * Library version reference was not properly handled. [Issue 58](https://code.google.com/p/sagittarius-scheme/issues/detail?id=58).
  * Calling `append` with single argument returned `()`. [Issue 59](https://code.google.com/p/sagittarius-scheme/issues/detail?id=59).
  * All unbounded export variables are fixed.

Improvements:
  * Compiler now optimise `(cond ((assq a b) => cdr))` to use builtin instruction.
  * Build process has been improved
    * Most of required libraries are searched using FIND\_PACKAGE command.
    * Bundled `libffi` can be used on Linux environment (tested with Ubuntu 32 bits)
  * ODBC blob now does not read all data at once.

New features:
  * Reader macro's range of affect has been changed. Now it affects per port not per file.
  * R7RS draft 7 has been supported.
  * File search procedure `glob` has been added.
  * `sash` option `-L` now can accept `-L'*'` style and added all directory to its load path.

New libraries:
  * SRFI-105 has been supported. It will be activated with `#!read-macro=curly-infix` notation not `#!curly-infix`.

Incompatible changes:
  * A lot of R7RS libraries have been modified without backward compatibilities.

[In Japanese](http://compassoftime.blogspot.nl/2012/11/sagittarius-038.html)

## Release Note 0.3.7 ##
Maintenance release.

Fixed bugs:
  * `append!` could destruct literal lists. [Issue 26](https://code.google.com/p/sagittarius-scheme/issues/detail?id=26).
  * `(cond-features)` returned a mutable list. [Issue 27](https://code.google.com/p/sagittarius-scheme/issues/detail?id=27).
  * `vector-reverse!` could destruct literal vectors. [Issue 28](https://code.google.com/p/sagittarius-scheme/issues/detail?id=28).
  * Builtin reader returned non Scheme object. [Issue 29](https://code.google.com/p/sagittarius-scheme/issues/detail?id=29).
  * `list-sort` raises &assertion when the given list is empty. [Issue 30](https://code.google.com/p/sagittarius-scheme/issues/detail?id=30).
  * `file-symbolic-link?` did not return #t for symbolic link. [Issue 31](https://code.google.com/p/sagittarius-scheme/issues/detail?id=31).
  * `with-library` made invalid cache file. [Issue 32](https://code.google.com/p/sagittarius-scheme/issues/detail?id=32).
  * `delete-directory` could not delete any directory on Windows environment.

New features:
  * Replaceable reader has been introduced. See document for more details.
  * `create-directory*`, `delete-directory*`, `copy-directory` and `build-path*` procedures have been added to `(util file)`
  * The _flags_ argument of `socket-send` and `socket-recv` is now optional. (Draft SRFI 106)

New libraries:
  * SRFI-49 has been supported. To activate this, use `#!reader=srfi/:49` directive.

Improvements:
  * `values` returns non first class object and up to 32 values VM won't allocate memory.
  * The stability on Windows environment has been improved.

Incompatible changes:
  * `derive-key&iv` generic method in `(rsa pkcs :5)` library now needs to return 2 values instead of a pair.

[In Japanese](http://compassoftime.blogspot.nl/2012/10/sagittarius-037.html)

## Release Note 0.3.6 ##
Maintenance release.

Fixed bugs:
  * SOBER-128 could not be initialised with `pseudo-random` procedure. [Issue 21](https://code.google.com/p/sagittarius-scheme/issues/detail?id=21).
  * `read-delimited-list` did not accept custom textual port. [Issue 22](https://code.google.com/p/sagittarius-scheme/issues/detail?id=22).
  * Readtable wasn't reset with `#!r7rs` nor `#!compatible`. [Issue 23](https://code.google.com/p/sagittarius-scheme/issues/detail?id=23).
  * Library prefix did not work properly. [Issue 24](https://code.google.com/p/sagittarius-scheme/issues/detail?id=24).
  * `(syntax bar)` did not resolve right library information. [Issue 25](https://code.google.com/p/sagittarius-scheme/issues/detail?id=25).

New features:
  * `mod-expt` and `mod-inverse` are implemented in C and more efficiently.
  * `syntax-case` and `syntax-rule` (exported from `(rnrs)` library) rename symbols.
  * `path-for-each`, `path-map`, `delete-directory*` and `create-directory*` have been added to `(util file)` library.
  * `copy-file` procedure has been added.
  * `list->string` now can take optional arguments _start_ and _end_.
  * `read-random-bytes!` has been added to `(math random)` library.

New libraries:
  * SRFI-86, SRFI-31, SRFI-29 and SRFI-43 have been supported.

Improvements:
  * `split-at` procedure has been re-written to tail recursive.
  * `cond` now supports SRFI-61 style.

Incompatible changes:
  * Custom hash's slot name `read-random` has been changed and the procedure should be passed will accept different arguments. For more details, see the document.

[In Japanese](http://compassoftime.blogspot.nl/2012/09/sagittarius-036.html)

## Release Note 0.3.5 ##
Maintenance release.

Fixed bugs:
  * `format` with colon directive did not work properly. [Issue 20](https://code.google.com/p/sagittarius-scheme/issues/detail?id=20).
  * verfiy with EMSA-PSS protocol sometimes failed.
  * `xsubstring` on SRFI-13 raised `&assertion`.

New features:
  * library name can take exact non negative integer. (R7RS 6th ballot)
  * `vector-append`, `bytevector-append` (R7RS 6th ballot) `vector-concatenate` and `bytevector-concatenate` have been added.
  * Immediate flonum has been introduced.
  * `(eqv? 0.0 -0.0)` now returns #f.
  * `port-for-each` and `port-map` have been added to `(util port)`
  * `drop*` has been added to `(util list)`

Improvements:
  * `utf8->string`, `string->utf8`, `bytevector->string` and `string->bytevector` now can take optional start and end arguments.
  * `equal?` is now check recursively for record.
  * `(exit #t)` is now the same as `(exit 0)` or `(exit)`. (R7RS 6th ballot)
  * Windows versions now don't depend on MSVC`*`.dlls
  * Improved regular expression performance.
  * Improved `list-sort` performance.

New libraries:
  * RFC 4122 library (rfc uuid) has been added.
  * RFC 1421 library (rfc pem) has been added. (not documented yet)

New documents:
  * `(util list)` has been documented.
  * `(sagittarius io)` has been documented.

[In Japanese](http://compassoftime.blogspot.nl/2012/08/sagittarius-035.html)

## Release Note 0.3.4 ##
Maintenance release.

Fixed bugs:
  * `bytevector-***-set!` (`***` is s32 to s64) does not accept minus values. [Issue 19](https://code.google.com/p/sagittarius-scheme/issues/detail?id=19).
  * `(lambda ()) and (begin) return bogus closure. [Issue 18](https://code.google.com/p/sagittarius-scheme/issues/detail?id=18).
  * Could not apply huge amount of arguments (more than max stack). [Issue 17](https://code.google.com/p/sagittarius-scheme/issues/detail?id=17).
  * Internal FFI call did not initialised callback properly.
  * `bytevector->integer` ignored optional arguments.
  * Fixed (net oauth consumer)'s `get-problem-report` not to raise error when header was empty.
  * Fixed export spec of SRFI-1 (it did not export every).

New features:
  * SRFI-17 generalized set! has been supported.
  * 64 bit environment build has been supported.

Improvements:
  * Compiled cache directory's structure has been modified to be able to handle different environment runtimes. (32 bit and 64 bit)
  * `fold-right` and `unfold` are now tail recursive.
  * SRFI-1 library's procedures now raise error when improper list was given.

New libraries:
  * General coercion and refer library `(sagittarius object)` has been added.
  * REPL support library `(sagittarius interaction support)` has been added.
  * eql specializer library `(sagittarius mop eql)` has been added.

New documents:
  * `(text sxml sxpath)` has been documented.
  * `(rfc uri)` has been documented.
  * `(util bytevector)` has been documented.

[In Japanese](http://compassoftime.blogspot.nl/2012/07/sagittarius-034.html)

## Release Note 0.3.3 ##
Maintenance release.

Fixed bugs:
  * sjis and euc-jp codec could not handle ASCII characters.
  * String ports had problem with reading linefeed.
  * String procedure could not detect '\0'.
  * Unicode conversion was not correct for some cases.
  * bytevector->string related procedures used LF eol style. (now using NONE).
  * It was impossible to re-use a transcoder.
  * string->symbol returned weird symbol in certain case.
  * Constant literal lists were not read properly.
  * (srfi :14 char-set) contained unbound variables.
  * PKCS EMSA encode and MGF-1 functions were not correct.
  * csv-read could not read multiple comment lines.

New functionalities:
  * object-apply has been added.
  * pointer-c-ref-pointer and decref have been added to (sagittarius ffi).
  * OAuth service provider procedures have been added.
  * regex related macros have been added. (not documented yet)
  * unwind-proceted has been added in (sagittarius control). (not documented yet)

Improvements:
  * map, for-each, fold-left and fold-right are now tail recursive.
  * define-method, define-class and define-generic are implemented with syntax-case instread of er-macrotransformer and match.
  * bytevector->integer and integer->bytevector performance are improved.
  * out-of-tree build has been supported.

New libraries:
  * (www cgi) has been added. (not documented yet)
  * (www fastcgi) has been added. (not documented yet)
  * (text html-lite) has been added. (not documented yet)
  * (text tree) has been added. (not documented yet)

Documents:
  * (rfc :5322) has been documented.
  * (rfc base64) has been documented.
  * (rfc quoted-printable) has been documented.
  * (packrat) has been documented.
  * (json) has been documented.
  * (text csv) has been documented.

[In Japanese](http://compassoftime.blogspot.nl/2012/06/sagittarius-033.html)

## Release Note 0.3.2 ##
Maintenance release.

Fixed bugs:
  * `call-next-method` can not be used in `syntax-case`. [Issue 14](https://code.google.com/p/sagittarius-scheme/issues/detail?id=14).
  * Certain type of procedure call does not work properly. [Issue 15](https://code.google.com/p/sagittarius-scheme/issues/detail?id=15).
  * Custom binary output port causes SEGV. [Issue 16](https://code.google.com/p/sagittarius-scheme/issues/detail?id=16).
  * Added missing R6RS macro `assert‘.
  * Fixed improper port class' hierarchy.
  * Fixed socket port's get-bytevector-all. It could not finish in certain situation.
  * Fixed unbound variable error with `make-basic-constains` in `(rfc x.509) library.
  * Fixed `open-inflating-input-port`'s read procedure. It did not work properly when reading more than one bytes.
  * Fixed loading certificate from keystore with `(rsa pkcs :12)` library. It loaded only private key.
  * Fixed bytevector output port's getter behaviour. It did not reset the buffer position.

Improvements:
  * Custom binary input port now reads more than one byte at once.
  * `bytevector-copy` procedure now can take optional arguments start and ends. See the document for more details.
  * `base64-decode-string` is now be able to return raw bytevector when optional transcoder is #f.
  * Improved bytevector output port and string output port performance. Approximately as twice as faster than before.
  * Psuedo random class is now be able to customise.
  * Cache mechanism now can be extended from Scheme using `<fasl-meta>` class.
  * Regular expression reader now returns pattern object instead of S expression.

Incompatible changes:
  * `custom-prng?` procedure is removed.

New libraries:
  * OAuth library `(net oauth)` has been added. This library supports OAuth 1.0.
  * TLS library `(rfc tls)` has been added. This library supports TLS 1.0 to TLS 1.2.
  * `(srfi :28 random-bits)` has been added.
  * Validator metaclass library `(sagittarius mop validator)` has been added.

[In Japanese](http://compassoftime.blogspot.com/2012/05/sagittarius-version-032.html)

## Release Note 0.3.1 ##
Maintainance release.

Fixed bugs:
  * Pattern variable can not cross over own library. [Issue 10](https://code.google.com/p/sagittarius-scheme/issues/detail?id=10).
  * `define-library` does not accept `cond-expand`. [Issue 11](https://code.google.com/p/sagittarius-scheme/issues/detail?id=11).
  * Reader does not read "\x0;". [Issue 12](https://code.google.com/p/sagittarius-scheme/issues/detail?id=12).
  * `bytevector-s64-set!` and `bytevector-s64-native-set!` do not accept #x-8000000000000000. [Issue 13](https://code.google.com/p/sagittarius-scheme/issues/detail?id=13).
  * `make-vector` filled bogus value when it was not passed fillter argument.

Improvements.
  * Introduced expandable cryptographic cipher and key generation. For more detail, see the documentation.
  * Introduced expandable hash algorithm. For more detail, see the documentation.
  * Rewrote (asn.1) library.
  * `define-class` now can handle `:metaclass` option.

New libraries:
  * (srfi :4) has been added.
  * HMAC library (rfc hmac) has been added.
  * PKCS#5 library (rsa pkcs :5) has been added.
  * PKCS#12 library (rsa pkcs :12) has been added. (not documented yet)
  * bytevector utility library (util bytevector) has been added. (not documented yet)
  * Class allocation class library (sagittarius mop allocation) has been added. (not documented yet)

[In Japanese](http://compassoftime.blogspot.com/2012/04/sagittarius-031.html)

## Release Note 0.3.0 ##
From this release, Sagittarius supports builtin CLOS (not whole conformed).

Fixed bugs:
  * internal define could not be refered from macro. This has been fixed.

New libraries:
  * (clos user) and (clos core) have been added. These provides CLOS related procedures and macros.
  * (getopt) has been added. This library is a wrapper for SRFI-37.
  * (text markdown) has been added. This library is parser for Markdown (not completely conformed)
  * (scribble plugin) (scribble convert) and (scribble parser) libraries have been added.
  * (sagittarius document html) library has been added.

Improvements:
  * `regex-replace-first` and `regex-replace-all` can take procedure for its replacement argument. For more detail, see document.
  * `binary-port?` and `textual-port?` are now R7RS conformed. It can take any object.
  * `get-bytevector-n` and `get-bytevector-some` do not allocate memory for its buffer it unless reached EOF.

New documents:
  * (sagittarius process) library has been documented.

[In Japanese](http://compassoftime.blogspot.com/2012/03/sagittarius-version-030.html)


## Release Note 0.2.4 ##
From this release, Common Lisp like reader macro has been introduces and R7RS small (draft 5) has been supported.
If you have already installed previous version of Sagittarius, please uninstall it first. (math) library's path has been changed, so it might cause a library search problem.
If you are using Linux and trying to build Sagittarius, please install `zlib` and `libffi` before run cmake command.

Fixed bugs.
  * GC realated bugs has been fixed. Now Sagittarius is using shared library of Boehm GC.
  * `define-with-key` did not work properly, this has been fixed.
  * Regular expression's char class reading has been fixed. It could not read [[:char-set:]] properly.
  * `equal-hash` did not terminate when it is given circular object. This has been fixed.
  * Equal hash table could not store bytevector. This has been fixed.
  * `list->string` did not check its contents. This has been fixed. Now it must be given a character list.
  * [issue 7](https://code.google.com/p/sagittarius-scheme/issues/detail?id=7) has been fixed.
  * Reader could not read `3.1415|10` number. This has been fixed. (However the precision will be ignored)
  * `quotient`, `remainder` and `modulo` caused SIGSEGV when it is given 0 as its second argument. This has been fixed.
  * Regular expression reader could not read `\0mnn`, `\xhh`, `\uhhhh` and `\Uhhhhhhhh`. This has been fixed.

New libraries.
  * (shorten) libarary has been added. You can use `^` instead of `lambda`. Also `^a` like `(lambda (a) ...)`.
  * R7RS libraries have been added, except `(scheme repl)`
  * JSON parsing library `(json)` has been added. (not documented yet)
  * Packrat parser `(packrat)` has been added. (not documented yet)

New functions.
  * `string-split` has been added to `(sagittarius regex)`.
  * `dolist`, `cond-list` and `slices` have been added to `(util list)`.
  * `cond-expand` is now built in syntax.
  * `define-library` syntax (R7RS) has been added.
  * `#!fold-case` and `#!no-fold-case` hash-bang flags have been added.
  * `include` and `include-ci` syntaxes have been added. These can be used out side of `define-library`.
  * `secure-random` procedure has been added to `(crypto)`.

New documents.
  * `(crypto)` library has been documented.
  * `(math)` library has been documented.
  * R7RS support page has been added to the document.

[In Japanese](http://compassoftime.blogspot.com/2012/01/version-024.html)

## Release Note 0.2.3 ##
Maintenance release.
(I can't believe until this release the tar ball did not have any extra libraries. From this release I have included it.)
  * Fixed mis-marking of library constable.
  * Fixed `bitwise-first-bit-set` for bignum. It returned not correct value.
  * Fixed rename export.

This library is available.
  * Performance profiler library (time)

Two libraries have been replaced but no API has been changed.
  * (srfi :14 char-set) has replaced by builtin library.
  * (sagittarius regex) has been replaced for future extension.


## Release Note 0.2.2 ##
From this release the compiler does constant folding and syntax define-constant has been
supported.
  * Fixed mis-flaging of library inlinable.
  * Fixed `get-bytevector-n!` mis-assert its reading range.
  * Fixed `let*-values`' wrong reference of compiling time environment.
  * Fixed `delete-file`. It did not raise any error when it failed to delete file.
  * Fixed rename import. It had restriction. Now it's removed.
  * Fixed `make-transcoder`'s EOL style on Windows.
  * Modified `current-directory` as a parameter.
  * Replaced Andrew Wright's match to Alex Shinn's match.

These libraries are available now.
  * Zlib compression library (rfc zlib)
  * Hashtable utility library (util hashtables)
  * Program argument processor library (srfi :37)

From this release the document is not OOo. It is similar to Racket's scribble, but does not have any compatibility. If you are building from source code, after compiling you can simply type

`$ make doc`

and Sagittarius generates the html document.

[In Japanese](http://compassoftime.blogspot.com/2011/11/version-022.html)

## Release Note 0.2.1 ##
This release is maintenance release.
  * Fixed syntax-rules bug. Now srfi-42 using R6RS syntax-rules.
  * Fixed mis-compiling.
  * Fixed rename importing bug.
  * Fixed loading bug when target file has #!r6rs or #!compatible hash-bang, it overwrote the VM mode.

These libraries are available now.
  * Added (util port) library.
  * Added (util file) library.
  * Added (text sxml serializer) library.
  * Added (srfi :38 with-shared-structure) for portability.
  * Updated (text sxml sxpath).

(text sxml ssax) is documented. Now it is officially supported.

## Release Note 0.2.0 ##
Internal structure has been changed from this release. More concretely, from this version VM does not create display closure for every let which has free variables. Thus there are less memory allocation and performance is improved, especially named let form.

  * Performance tuned version. Now, it is almost twice faster than previous version.
  * Document has been added. All libraries and functions not documented have possibilities that the APIs will be changed in future.
  * More SRFIs are supported. 0, 23, 41, 42 and 98.
  * Database Interface library (dbi) has been added, but not documented.
  * ODBC wrapper and DBD implementation has been added, but not documented.
  * Library importing related bugs are fixed.
  * (apropos) library has been added.

## Release Note 0.1.3 ##
  * FFI library (sagittarius ffi) is available. Compiling from source code, it requires libffi.
  * Cryptographic library (crypto) is available. This library's symmetric ciphers are using libtomcrypt.
  * ASN.1 library (asn.1) is available. For now it can only encode DER, no decode nor other encoding.
  * Parser generator (lalr) is available. This library is based on lalr-scm.
  * REPL is available.
  * Other small bugs are fixed.

## Release Note 0.1.2 ##
  * Regular Expression library (sagittarius regex) is available. This library is Java like regexp.
  * Socket library (sagittarius socket) is available. Its API is compatible with Ypsilon and mosh.
  * Compiled cache has been introduced. Until this release, Sagittarius compiled all library, including (rnrs) or so, each time, that made it really slow. Now, it's a little bit better.
  * Sxml library (text sxml ssax), (text sxml sxpath), (text sxml html) and (text sxml tree-trans) are available. This libraries are ported from Oleg Kiselyov's SSAX implementation.
  * Andrew Wright's pattern matching is available. This is in library (match).
  * Custom codec mechanism has been introduced. As an example, see (encoding sjis).
  * Srfi 2, 8, 13, 14, and 26 are available.
  * #!r6rs switches symbol reading rules. Now it is stricter than before.

## Release Note 0.1.1 ##
  * Multi thread library (sagittarius threads) is available. This library is almost compatible with srfi-18.

## Release Note 0.1.0 ##
  * Better stack trace.
  * Fixed syntax-rules renaming problem.
  * Introduced syntax-quote.

## Release Note 0.0.6 ##
  * All R6RS libraries are available.
  * Srfi 1, 6, 39 64 and 97 are available.
  * (rfc base64) library has been added.
  * #!r6rs switches write and few read behaviour.
    * **NOT** completely R6RS behaviour yet.
  * Compiling with Visual Studio 2010 has been supported. See README for detail.