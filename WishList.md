# Introduction #

This page is just for my TODO (or wish) list.


# Details #

Libraries:
  1. ~~More cryptographic libraries.~~
> > ~~I need libraries which handle PKCS#5 and PKCS#12.~~
  1. ~~TLS support. Done!!~~
> > ~~Since we have cryptographic library, we can write it in pure Scheme.~~
  1. Future and promise.
> > Low priority. It is nice to have future task library since we run unit test with multi threading.

For the near future:
  1. Better stack trace and debug information.
> > Currently source code information is lost when VM shows stack trace. I am thinking to add new instruction to indicate source info such as `COMMENT`, `ANNOTATION` or so.

If I can:
  1. ~~Built in CLOS.~~
> > ~~Since we have a lot of collections, sometimes I want to write a generic method for it. String, vector and bytevector are sequence and hashtable and treemap are dictionary. If we have something to deal with it with just one generic method, it will be so convenient.~~
> > > Done! in 0.3.0. But not implemented sequence or dictionary method.
  1. Builtin debugger.

> > When I write code, it has always some bugs. And sometimes it's really difficult to fix. I wish I had a debugger.
  1. JIT
  1. Independent from Boehm GC