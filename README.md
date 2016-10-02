JSchemeMin is a implementation of the Scheme language on the Java platform.

As a implementation of the language described in the
[Revised<sup>7</sup> Report on the Algorithmic Language Scheme](http://trac.sacrideo.us/wg/attachment/wiki/WikiStart/r7rs.pdf)
(R7RS), JSchemeMin supports all standard features of Scheme,
including first-class procedures, proper tail calls,
continuations, user-defined records, libraries, exceptions, and
hygienic macro expansion.

As a JVM based implementation, JSchemeMin enables Scheme programs
to use the full power of Java's API. JSchemeMin also enables Java 
programs to evaluate Scheme's code, so Scheme can be used as an 
extensions language by Java's programs.

Currently, JSchemeMin only provides a interpreter, not compiler.
A basic mechanism for profiling is provided. 
Debugger and coverage tool is planed to be added in the future.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.
