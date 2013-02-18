## The Bluejelly Project

Bluejelly is my attempt to execute real-world pure lazy functional programs on the 
Java virtual machine. If features the following toolchain:

* **The Bluejelly Runtime:** a runtime system providing support for graph-reduction on the JVM.
    It is based on the technical report [The Lazy Virtual Machine Specification][lvm], by Daan 
    Leijen (Utretch University, 2001). The runtime is essentialy a Java rendition of the spec.
* **The Bluejelly Assembler:** An pure lazy assembly language, and its assembler. Assembly 
    programs can be assembled to Java classes that the Bluejelly Runtime can execute on a JVM.
    The assembler is implemented in Scala.
* **The Low Level Lazy Language (aka L4):** a low level pure functional language, powerful 
    enough to write real-world programs with it without *too much* pain. The L4 compiler
    compiles L4 modules to assembler, that in turn are compiled to Java classes by the 
    Bluejelly Assembler.

## Future Work

The main missing piece is a full fledged Haskell-like pure functional language built on
top of L4. Bluejelly progresses at the pace of my spare time, so this might take a while.

## Development

Bluejelly uses [sbt][]. Pull the code and enter sbt from the main project root folder. You can
then pick the runtime, assembler or l4, and run the `test`s. The assembler and l4 can be executed 
from sbt console using the `run` target, but not the runtime (it does some class loading trickery 
that `run` does not like).

Yet, for all projects there is a `dist` task that builds a zip file holding the application jar 
(built with [sbt-assembly][sbtasm]) and a (Linux) launcher script. Use the `dist` task to build an
executable runtime (or assembler, or l4 compiler).

It is possible to use [sbteclipse][] to develop from an Eclipse with a Scala IDE plugin installed.
The `.gitignore` file already includes the Eclipse stuff to ignore. You must set project flavor
equals Java for the runtime. Executing the tests for all projects requires some generated files
that sbt takes care of generating them when executing the `test` command. Hence, it is not a 
good idea to try to run the runtime, assembler or l4 compiler (or their tests) from Eclipse.
Please stick to the sbt console.

## Examples

Check `src/test/resources/testmods.src` in the assembler and l4 projects for example assembler
and l4 modules, respectively.

[lvm]:          http://www.cs.uu.nl/research/techreps/repo/CS-2004/2004-052.pdf
[sbt]:          http://www.scala-sbt.org/
[sbt-assembly]: https://github.com/sbt/sbt-assembly
[sbteclipse]:   https://github.com/typesafehub/sbteclipse

