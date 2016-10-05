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

JSchemeMin 是又一个JVM平台上的Scheme语言实现。

作为[Revised<sup>7</sup> Report on the Algorithmic Language Scheme](http://trac.sacrideo.us/wg/attachment/wiki/WikiStart/r7rs.pdf)
(R7RS)的实现，JSchemeMin支持Scheme的所有标准特性，包括头等公民地位的过程、尾递归优化、继续、用户定义记录、库（包括R7RS附录A中全部语法和过程，不只base）、异常和健康宏展开。

作为基于JVM的实现，JSchemeMin 让Scheme程序可以调用Java平台的API，也让Java程序运行Scheme代码，这使Scheme可作为Java（以至别的JVM语言）程序的一种扩展语言，类似Nashorn之于JavaScript。

目前，JSchemeMin 只提供解释器而非编译器。基本的性能监视机制已经存在。调试器和覆盖率工具也将提供。

性能并非JSchemeMin的主要设计目标，大部公情况下用明显的实现方式，不为性能牺牲实现的简单性，但相信足以满足预期的需要。

本软件为自由软件: 你可以在自由软件基金会的GNU通用公共许可，版本3或按你的意愿更新的版本，的条款下再分发它和/或修改它。
