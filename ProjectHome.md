R6RS/R7RS Scheme system.

## Features ##
  * Builtin CLOS.
  * Common Lisp like reader macro.
  * Cryptographic libraries.
  * Customisable cipher and hash algorithm.
  * Custom codec mechanism.
  * CL like keyword lambda syntax (taken from Gauche).
  * Constant definition form. (define-constant form).
  * Builtin regular expression
    * mostly works O(n)
  * Replaceable reader

## News ##
  * Repository has been moved to [bitbucket](https://bitbucket.org/ktakashi/sagittarius-scheme).


## Build tips ##
If you are using Ubuntu 11.10 (which I tested from scratch), you need to install these packages.

`cmake`, `libgc-dev`, `zlib1g-dev` and `libffi-dev`.

`cmake` must be installed the other can be installed during building, however it is not managed by package manager so it might cause problems.

On Debian Linux, default `cmake` version is 2.8.2 and Sagittarius requires 2.8.4 so it might complain. If you got the problem, please change the version number to 2.8.2 in the `CMakeLists.txt`.

On some platform, `libffi` is installed on the path CMake searches. (e.g /usr/local/lib/libffi-3.0.13 or so). In this case you can specify the search path like following options;

```
$ cmake . -DCMAKE_INCLUDE_PATH=/usr/local/lib/libffi-3.0.13/include -DCMAKE_LIBRARY_PATH=/usr/local/lib 
```

### For QNX environment ###
QNX environment has been supported (only x86 has been tested). To build Sagittarius on it, you need to use CMake tool chain like this;

```
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/Toolchain-QNX-8.0.0.cmake .
```

Then patch Boehm GC with following command before build;

```
patch -p < cmake/patches/gc.qnx.patch
```

If you are building with out of tree, then adjust above commands.

## Release Note 0.4.12 ##
Maintenance release

Fixed bugs:
  * Macro define macro didn't work with CLOS methods. [Issue 157](https://code.google.com/p/sagittarius-scheme/issues/detail?id=157).
  * `bytevector->integer` didn't return proper value when negative value was pssed. [Issue 159](https://code.google.com/p/sagittarius-scheme/issues/detail?id=159).
  * `mod-inverse` returned inorrect value randomly. [Issue 160](https://code.google.com/p/sagittarius-scheme/issues/detail?id=160).
  * `getenv` on Windows didn't return environment value properly. [Issue 161](https://code.google.com/p/sagittarius-scheme/issues/detail?id=161).
  * `get-output-bytevector` and `extract-output-bytevector` caused SEGV. [Issue 162](https://code.google.com/p/sagittarius-scheme/issues/detail?id=162).
  * Reading escaped symbol on R6RS mode raised `&lexical`. [Issue 164](https://code.google.com/p/sagittarius-scheme/issues/detail?id=164).
  * Cryptographic operation with CBC mode didn't work properly for second operation.
  * `(rfc mime)` did not handle multi part data correctly.

Improvements:
  * `cipher-block-size` now can accept mere cipher name instead of cipher instance.
  * `next-method?` can be used on method call to check if there is a next method.
  * `is-prime?` now using Lucas-Lehmer method to check big size prime number.
  * Better stack trace.
  * `access-protected-resource` on `(net oauth)` can handle multi part data.
  * `inflating-input-port` now only forward the original port's position with used byte size.
  * Better process creation for Cygwin.
  * `mime-compose-message-string` now can handle binary data.
  * Extractor of `open-bytevector-output-port` can handle transcoded port.

New features:
  * `socket-recv!` has been added.
  * `(binary data)` library has been added.
  * DSA sign/verify have been added.
  * `bytevector->sinteger` and `bytevector->uinteger` have been added.
  * `sinteger->bytevector` and `uinteger->bytevector` have been added.
  * `(rfc ssh)` has been added (not documented).
  * `compute-getter-n-setter` has been added.
  * `slot-ref-using-class`, `slot-set-using-class!` and `slot-bound-using-class?` have been added.

Incompatible change:
  * `null` library has been removed. Now it's `(core)`.

[Japanese http://compassoftime.blogspot.nl/2013/12/sagittarius-scheme-0412.html](In.md)

## Release Note 0.4.11 ##
Maintenance release

Fixed bugs:
  * Method packed rest argument wrongly. [Issue 153](https://code.google.com/p/sagittarius-scheme/issues/detail?id=153).
  * Custom port caused SEGV. [Issue 154](https://code.google.com/p/sagittarius-scheme/issues/detail?id=154).
  * `(/ 0)` raised `&compile`. [Issue 155](https://code.google.com/p/sagittarius-scheme/issues/detail?id=155).
  * `display` or `write` with custom textual port caused C level assertion. [Issue 156](https://code.google.com/p/sagittarius-scheme/issues/detail?id=156).

Improvements:
  * Better build process.
  * `define-class` now allows Gauche like slot definition.

New features:
  * `(dbm)` library has been added.
  * `dbi-do-query!` macro has been added to `(dbi)`
  * `dbi-query-fold`, `dbi-query-map` and `dbi-query-for-each` have been added to `(dbi)`.
  * `dbi-tables` and `dbi-table-columns` have been added to `(dbi) to retrieve database table information.
  * `(sagittarius debug)` library has been added to support Gauche like debug reader macro. (not documented)

New documents:
  * `(archive)` has been documented.

## Release Note 0.4.10 ##
Maintenance release

Fixed bugs:
  * `set-port-position!` for file port did not work properly. [Issue 141](https://code.google.com/p/sagittarius-scheme/issues/detail?id=141).
  * `(- 0 <bignum>)` returned always negative integer. [Issue 142](https://code.google.com/p/sagittarius-scheme/issues/detail?id=142).
  * The least fixnum wasn't read as a fixnum. [Issue 143](https://code.google.com/p/sagittarius-scheme/issues/detail?id=143).
  * The number -8388609 was read as 8388607. [Issue 144](https://code.google.com/p/sagittarius-scheme/issues/detail?id=144).
  * `bitwise-xor` was wrong for bignums along with negative arguments. [Issue 145](https://code.google.com/p/sagittarius-scheme/issues/detail?id=145).
  * `(bitwise-arithmetic-shift 0 65)` returned a bignum zero. [Issue 146](https://code.google.com/p/sagittarius-scheme/issues/detail?id=146).
  * `bitwise-arithmetic-shift-right` was wrong for big shift amounts. [Issue 147](https://code.google.com/p/sagittarius-scheme/issues/detail?id=147).
  * `fxdiv0-and-mod0` was wrong for some arguments. [Issue 148](https://code.google.com/p/sagittarius-scheme/issues/detail?id=148).
  * `fxbit-set?` was wrong for high indices. [Issue 149](https://code.google.com/p/sagittarius-scheme/issues/detail?id=149).
  * inflating-port raised an error when buffer size was small. [Issue 150](https://code.google.com/p/sagittarius-scheme/issues/detail?id=150).
  * `file-executable?` hunged up on Windows. [Issue 151](https://code.google.com/p/sagittarius-scheme/issues/detail?id=151).
  * `file-stat-atime`, `file-stat-ctime` and `file-stat-mtime` didn't return nano second of POSIX time. [Issue 152](https://code.google.com/p/sagittarius-scheme/issues/detail?id=152).
  * `copy-dicrectory*` did not handle top most file properly.
  * `url-server&path` procedure returned a path with `//` prefix.

Improvements:
  * Memory usage is improved. Now Sagittarius uses less memory.
  * Finding Boehm GC and libffi uses `pkg-config` if available.
  * `object-equal?` method has been added for RSA keys.
  * `import-public-key` and `import-private-key` for RSA now accepts bytevector as its input.
  * `parse-pem` procedure now accepts `:builder` keyword to pass custom object builder.
  * `with-args` macro now packs the rest argument even if it contains not listed arguments.

New features:
  * `file->bytevector` has been added to `(util file)`
  * Generic archive library `(archive)` has been added. (not documented yet)
  * Zip archive library `(archive core zip)` has been added. (not documented yet)
  * Tar archive library `(archive core tar)` has been added. (not documented yet)
  * GZIP library `(rfc gzip)` has been added. (not documented yet)

New document:
  * `(getopt)` library has been documented.

[In Japanese](http://compassoftime.blogspot.nl/2013/10/sagittarius-scheme-0410.html)

## Release Note 0.4.9 ##
Maintenance release

Fixed bugs:
  * TLS library did not handle handshake message which contains multiple messages in one record. [Issue 134](https://code.google.com/p/sagittarius-scheme/issues/detail?id=134).
  * `get-bytevector-all` broke input bytevector. [Issue 136](https://code.google.com/p/sagittarius-scheme/issues/detail?id=136).
  * 0 byte cache files were created on Windows. [Issue 138](https://code.google.com/p/sagittarius-scheme/issues/detail?id=138).
  * `get-addrinfo` caused SEGV.

Improvements:
  * JSON library `(json)` now can handle 'u' escape. (Patch from Atsushi Saito)
  * String normalization procedures' performance have been improved (approx 3 times faster).
  * I/O performance has been improved.
  * Improved performance of `fl+`, `fl*`, `fl-` and `fl/`.
  * Stack trace information show better source location.
  * `(rfc tls)` now sends SNI.

New features:
  * HTTP library `(rfc http)` now handles BASIC and Digest authentications.
  * Supported SRFI 110.
  * Supported SRFI 112.
  * Supported R7RS large library `(scheme inquiry)`.

Imcompatible Change:
  * #!r7rs now won't change the `let-syntax` and `letrec-syntax` behaviour.
  * `(scheme base)` and `(rnrs base)` exports different `let-syntax` and `letrec-syntax`.


## Release Note 0.4.8.1 ##
Patch release.

Fixed bug:
  * 32bit version of Sagittarius did not work on 32 bit Windows OS. [Issue 133](https://code.google.com/p/sagittarius-scheme/issues/detail?id=133).


## Release Note 0.4.8 ##
Maintenance release.

Fixed bugs:
  * `bytevector-u8-set!` accepted negative number and broke data. [Issue 127](https://code.google.com/p/sagittarius-scheme/issues/detail?id=127).
  * Macro expansion caused unbound variable `lambda`. [Issue 128](https://code.google.com/p/sagittarius-scheme/issues/detail?id=128).
  * `cond-expand` did not handle nested condition. [Issue 129](https://code.google.com/p/sagittarius-scheme/issues/detail?id=129).
  * `bitwise-ior`, `bitwise-xor` and `bitwise-and` did not accept 0 argument. [Issue 131](https://code.google.com/p/sagittarius-scheme/issues/detail?id=131).
  * `write-emv-tlv` did not calculate length bytes properly when the data is bigger than 0x7F bytes.

Improvements:
  * Performance of `bitwise-xor`, `bitwise-ior`, `bitwise-and`, `fxxor`, `fxior` and `fxand` are improved.

New features:
  * `bytevector-split-at*` and `bytevector-splices` have been added to `(util bytevector)`.
  * SRFI-106 has been supported.
  * CMAC library `(rfc cmac)` has been added.
  * `c-variable` macro has been added to `(sagittarius ffi)`
  * QNX environment (Blackberry 10) has been supported (only x86 has been tested).
  * `socket-sendto` and `socket-recvfrom` have been added to `(sagittarius socket)`.

Imcompatible change:
  * `let-syntax` and `letrec-syntax` are now default R6RS behaviour. `#!r7rs` will switch to R5RS/R7RS behaviour.

[In Japanese](http://compassoftime.blogspot.nl/2013/08/sagittarius-048.html)

## Release Note 0.4.7 ##
Maintenance release.

Fixed bugs:
  * Certain type of macro caused SEGV. [Issue 122](https://code.google.com/p/sagittarius-scheme/issues/detail?id=122).
  * `integer->pointer` didn't accept minus value. [Issue 123](https://code.google.com/p/sagittarius-scheme/issues/detail?id=123).
  * `sqrt` with big number didn't return proper value. [Issue 125](https://code.google.com/p/sagittarius-scheme/issues/detail?id=125).
  * Multiply of big complex number returns positive number. [Issue 126](https://code.google.com/p/sagittarius-scheme/issues/detail?id=126).
  * `(rfc tls)` didn't compute MAC properly.

Imcompatible change:
  * `pointer->integer` might return minus value now.


## Release Note 0.4.6 ##
Maintenance release.

Fixed bugs:
  * `parameterize` with `call/cc` didn't restore old value. [Issue 108](https://code.google.com/p/sagittarius-scheme/issues/detail?id=108).
  * Re-assignment of global variable held a parameter. [Issue 109](https://code.google.com/p/sagittarius-scheme/issues/detail?id=109).
  * `define-library` with `cond-expand` didn't work without import `(scheme base)`. [Issue 110](https://code.google.com/p/sagittarius-scheme/issues/detail?id=110).
  * `define-class` required other library like (rnrs). [Issue 111](https://code.google.com/p/sagittarius-scheme/issues/detail?id=111).
  * #x800000 returned minus value. [Issue 112](https://code.google.com/p/sagittarius-scheme/issues/detail?id=112).
  * `(sagittarius mop validator)`'s observer rose an error. [Issue 113](https://code.google.com/p/sagittarius-scheme/issues/detail?id=113).
  * Builtin generic function caused SEGV. [Issue 114](https://code.google.com/p/sagittarius-scheme/issues/detail?id=114).
  * `current-jiffy` returned non exact integer. [Issue 115](https://code.google.com/p/sagittarius-scheme/issues/detail?id=115).
  * `((and and))` caused SEGV on REPL. [Issue 116](https://code.google.com/p/sagittarius-scheme/issues/detail?id=116).
  * `datum->syntax` didn't make syntax object properly. [Issue 117](https://code.google.com/p/sagittarius-scheme/issues/detail?id=117).
  * `import` ignored `except` clause. [Issue 118](https://code.google.com/p/sagittarius-scheme/issues/detail?id=118).
  * Builtin method didn't allow to have other method qualifiers. [Issue 119](https://code.google.com/p/sagittarius-scheme/issues/detail?id=119).
  * `list-sort` didn't raise an error when _proc_ argument was not a procedure. [Issue 120](https://code.google.com/p/sagittarius-scheme/issues/detail?id=120).
  * Giving a minus size to `make-bytevector` caused SEGV. [Issue 121](https://code.google.com/p/sagittarius-scheme/issues/detail?id=121).
  * Importing `(clos user)` with prefix caused unbound variable error.

Improvements:
  * `define-c-struct` is now be able to be used as a local bindings.
  * FFI library can use `wchar_t*` type.
  * `c-funtion` accepts variable length arguments with `___` argument type.
  * MOP generic function has much more flexibilities.
  * Removed unused instruction emmiting from compiler.

New features:
  * `object->pointer` and `pointer->object` procedures have been added to `(sagittarius ffi)`.

[In Japanese](http://compassoftime.blogspot.nl/2013/06/sagittarius-046.html)

## Release Note 0.4.5 ##
Maintenance release.

Fixed bugs:
  * `define-c-struct` defined wrong size of struct. [Issue 106](https://code.google.com/p/sagittarius-scheme/issues/detail?id=106).
> > - `(sagittarius ffi)` now compute more precise struct alignment.
  * `parameterize` didn't restore old value properly. [Issue 107](https://code.google.com/p/sagittarius-scheme/issues/detail?id=107).
  * `(/ 1 -0.0)` returnes `+inf.0`.
  * `sash`'s `-I` option raised an error.

Improvements:
  * Cache file is now safer on multi process environment.
  * Parameter object consumes less memory than before.

New features:
  * Builtin eql specializer.
  * `open-shared-library` now accepts optional argument to raise an error when the given library is not found.
  * `generate-secret-key` with DES3 now accepts 8 and 16 bytes key.
  * `lock-port!` and `unlock-port!` have been added for file lock (not documented yet).
  * `call-with-port-lock` has been added to `(util port)` (not documented yet).

New documents:
  * `(dbi)` and `(odbc)` library have been documented.

[In Japanese](http://compassoftime.blogspot.nl/2013/05/sagittarius-045.html)

## Release Note 0.4.4 ##
Maintenance release.

Fixed bugs:
  * `sqrt` with bignum returned inexact number. [Issue 100](https://code.google.com/p/sagittarius-scheme/issues/detail?id=100).
  * `(atan 0.0)` raised an error. [Issue 101](https://code.google.com/p/sagittarius-scheme/issues/detail?id=101).
  * `string-scan` returned invalid value. [Issue 102](https://code.google.com/p/sagittarius-scheme/issues/detail?id=102).
  * `get-output-string` and `get-output-bytevector` reset its posision. [Issue 103](https://code.google.com/p/sagittarius-scheme/issues/detail?id=103).
  * `current-date` raised an error if local time zone is minus UTC. [Issue 104](https://code.google.com/p/sagittarius-scheme/issues/detail?id=104).
  * `apropos` raised en arror.

Improvements:
  * `expt` performance with bignum has been improved.

New features:
  * `shared-object-suffix` procedure has been added to `(sagittarius ffi)`
  * Default implementation of `dbi-fetch-all!` has been added.
  * `set-pointer-value!` procedure has been added to `(sagittarius ffi)`
  * `key-check-value` procedure has been added to `(crypto)`
  * `->tlv` and `read-tlv` procedures have been added to `(tlv)`
  * `64bit` and `32bit` cond features have been added.

## Release Note 0.4.3 ##
Maintenance release. Improved memory usage.

Fixed bugs:
  * Eval causes unbound variable error. [Issue 96](https://code.google.com/p/sagittarius-scheme/issues/detail?id=96).
  * Writing closed socket caused SIGPIPE. [Issue 97](https://code.google.com/p/sagittarius-scheme/issues/detail?id=97).
  * Reverse order library importing from cache file. [Issue 98](https://code.google.com/p/sagittarius-scheme/issues/detail?id=98).
  * `bytevector->string` might cause SIGILL. [Issue 99](https://code.google.com/p/sagittarius-scheme/issues/detail?id=99).
  * R7RS load could not use custom reader.

Improvements:
  * Using less memory.

New libraries:
  * Remote REPL library `(sagittarius remote-repl)` has been added. (not documented)

New features:
  * `(rfc tls)` now supports server socket and TLS 1.2.
  * `make-x509-basic-certificate`, `make-x509-issuer` and `make-validity` have been added to `(rfc x509)` to be able to make own certificate.
  * `x509-certificate->bytevector` has been added to `(rfc x509)`.

[In Japanese](http://compassoftime.blogspot.nl/2013/03/sagittarius-043.html)

## Release Note 0.4.2 ##
Maintenance release. More R6RS compliant.

Fixed bugs:
  * Exported variable was not importet properly in some cases. [Issue 84](https://code.google.com/p/sagittarius-scheme/issues/detail?id=84).
  * Macro expansion broke library scope. [Issue 85](https://code.google.com/p/sagittarius-scheme/issues/detail?id=85).
  * `datum->syntax` did not work with unbound variable. [Issue 86](https://code.google.com/p/sagittarius-scheme/issues/detail?id=86).
  * Local macro inside of syntax template caused compile time error. [Issue 87](https://code.google.com/p/sagittarius-scheme/issues/detail?id=87).
  * Cache caused SEGV. [Issue 88](https://code.google.com/p/sagittarius-scheme/issues/detail?id=88).
  * `environment` procedure did not accept 0 argument. [Issue 90](https://code.google.com/p/sagittarius-scheme/issues/detail?id=90).
  * `integer->bytevector` with optional arguments sometimes returned incorrect value. [Issue 91](https://code.google.com/p/sagittarius-scheme/issues/detail?id=91).
  * Import with prefix created unexpected symbol export. [Issue 92](https://code.google.com/p/sagittarius-scheme/issues/detail?id=92).
  * `make-variable-transformer` hid visible variables. [Issue 93](https://code.google.com/p/sagittarius-scheme/issues/detail?id=93).
  * Passing a bignum to `exact-integer-sqrt` caused infinite loop. [Issue 95](https://code.google.com/p/sagittarius-scheme/issues/detail?id=95).
  * Loading a library without shebang notation might cause unexpected compile result.

Improvements:
  * Optimised `(expt 2 x)` case.
  * Macro expander now tries to expand macros first as much as possible. [Issue 94](https://code.google.com/p/sagittarius-scheme/issues/detail?id=94).
  * Compiler now detects unbound variables and raises an &undefined on R6RS mode.
  * `bytevector-fill!` now accepts optional arguments _start_ and _end_.

New features:
  * `crc32` and `adler32` procedures have been added to `(rfc zlib)` library.
  * `split-key`, `combine-key-components` and `combine-key-components!` procedures have been added to `(crypto)` library.
  * `->odd-parity` procedure has been added to `(util bytevector)`.
  * `(tlv)` library now supports DGI style TLV data.

New libraries:
  * Binary pack and unpack library `(binary pack)` has been added.

Incompatible changes:
  * unbound variable error now raised `&undefined` instead of `&assertion`.

[In Japanese](http://compassoftime.blogspot.nl/2013/02/sagittarius-scheme-042.html)

## Release Note 0.4.1 ##
Maintenance release.

Fixed bugs:
  * The same internal define caused ASSERT error. [Issue 78](https://code.google.com/p/sagittarius-scheme/issues/detail?id=78).
  * Passing bignum to `sqrt` caused ASSERT error. [Issue 80](https://code.google.com/p/sagittarius-scheme/issues/detail?id=80).
  * Passing 0 to `bitwise-bit-count` and `fxbit-count` returned invalid result. [Issue 82](https://code.google.com/p/sagittarius-scheme/issues/detail?id=82).

Improvements:
  * ODBC library searching process has been improved. Now it can detect iODBC as well.
  * SRFI 42 library can work on R6RS mode.

New features:
  * `pointer->bytevector` procedure has been added to `(sagittarius ffi)`
  * Invoking `sash` with only toplevel syntax option `-t` has been added.

New libraries:
  * TLV data structure library `(tlv)` has been added.

[In Japanese](http://compassoftime.blogspot.nl/2013/01/sagittarius-041.html)

## Release Note 0.4.0 ##
Minor version up release.
Better R6RS conformity.
Supported all R7RS small (draft 8) requirements.

Fixed bugs:
  * `datum->syntax` did not resolve given template identifier. [Issue 1](https://code.google.com/p/sagittarius-scheme/issues/detail?id=1).
  * Writing deep nested list causes SEGV. [Issue 60](https://code.google.com/p/sagittarius-scheme/issues/detail?id=60).
  * `for` keyword in `import` was ignored. [Issue 63](https://code.google.com/p/sagittarius-scheme/issues/detail?id=63).
  * Internal define did not accept empty body. [Issue 64](https://code.google.com/p/sagittarius-scheme/issues/detail?id=64).
  * `let-syntax` or `letrec-syntax` created scope on R6RS mode. [Issue 66](https://code.google.com/p/sagittarius-scheme/issues/detail?id=66).
  * Custom codec could not be used with `transcoded-port`. [Issue 67](https://code.google.com/p/sagittarius-scheme/issues/detail?id=67).
  * Macro expansion did not resolve symbol properly. [Issue 68](https://code.google.com/p/sagittarius-scheme/issues/detail?id=68).
  * Invalid `quasisyntax` expansion. [Issue 69](https://code.google.com/p/sagittarius-scheme/issues/detail?id=69).
  * `path-map` and `path-for-each` did not stop when `:all` was #f. [Issue 70](https://code.google.com/p/sagittarius-scheme/issues/detail?id=70).
  * `bytevector-u64-set!` and `bytevector-s64-set!` set wrong value. [Issue 71](https://code.google.com/p/sagittarius-scheme/issues/detail?id=71).
  * local variable transformer macro caused compile error. [Issue 72](https://code.google.com/p/sagittarius-scheme/issues/detail?id=72).
  * `port-eof?` did not work on custom port. [Issue 73](https://code.google.com/p/sagittarius-scheme/issues/detail?id=73).
  * Macro defined in macro causes compile error. [Issue 74](https://code.google.com/p/sagittarius-scheme/issues/detail?id=74).
  * `random-integer` might return the same value as given _size_ argument. [Issue 75](https://code.google.com/p/sagittarius-scheme/issues/detail?id=75).
  * `free-identifier=?` returned #f if the given identifier's names were not the same. [Issue 76](https://code.google.com/p/sagittarius-scheme/issues/detail?id=76).
  * `(eqv? 0.0+0.0i 0.0-0.0i)` returned #t. [Issue 77](https://code.google.com/p/sagittarius-scheme/issues/detail?id=77).

Improvements:
  * Reader macro now only affects per port. [Issue 61](https://code.google.com/p/sagittarius-scheme/issues/detail?id=61).
  * No reading string length limit. [Issue 65](https://code.google.com/p/sagittarius-scheme/issues/detail?id=65).
  * Debian Linux has been supported.

New libraries:
  * SRFI-25 and SRFI-78 have been added.

[In Japanese](http://compassoftime.blogspot.nl/2012/12/sagittarius-040.html)