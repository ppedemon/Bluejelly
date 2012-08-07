## The Bluejelly Project

Bluejelly is my attempt to execute real-world pure lazy functional programs on the 
Java virtual machine. So far, we provide two modules:

* **The Bluejelly Runtime:** a runtime system providing support for graph-reduction.
  It is based on the technical report [The Lazy Virtual Machine Specification]
  (www.cs.uu.nl/research/techreps/repo/CS-2004/2004-052.pd), by Daan Leijen 
  (Utretch University, 2001). The runtime is a Java rendition of the spec, with
  a couple of additions to handle ad-hoc polymorphism and to construct non-updatable
  thunks, thus enabling front ends to take advantage of update analysis. Experiments
  show that the performance improvements resulting from removing spurious updates 
  can be dramatic.
* **The Bluejelly Assembler:** An implementation of a pure lazy assembly language,
    which can be assembled to a Java class that, linked with the Bluejelly Runtime,
    can be executed on a JVM. This is implemented in Scala.

## Future Work

As time permits, I plan to stack more "usable" languages on top of the Assembler
(e.g., a Core lambda calculus implementation and something Haskell or Clean like
on top of that).

## Development

The project is distributed as a set of Eclipse projects under a common root. You can 
pull the repository, create an Eclipse workspace, and import the projects to that
workspace using the "Import External Project" option. You need:

* JDK +1.6
* Scala +2.9.x
* An Eclipse IDE new enough (Indigo onwards)
* The ScalaIDE plugin

I tried using sbt and Maven, but I was not satisfied with the results:

* Due to unknown (for me!) class loader issues, sbt failed to execute the Bluejelly
 runtime test suite. Namely, calls to `ClassLoader.getSystemClassLoader().loadClass()` 
  made by ObjectWeb `ClassReader` class were failing. In addition, I found sbt rather slow.
* Maven was my next option. Unlike sbt it worked, but the scala compiler plugin is
 _horribly_ slow (taking +30 secs to compile the whole Assembler). Conversely, Eclipse
 compiles Scala code in a background process (mostly) in a breeze.
* I tried to use my Maven project structure in Eclipse using m2e and m2eclipse-scala, but
  without success (m2eclipse-scala just did not work). IntelliJ-IDEA worked fine on the
  Maven project structure, but I did not like it --guess I am too used to Eclipse :(

The downside of using Eclipse is that I loose the notion of repository. Hence, the 
Bluejelly.Libs project providing the libraries used by the Runtime and the Assembler.

## Deployment

I am using plain old Ant. Each project holding code includes a `build.xml` file either
building a library or creating a distribution tarball. For deploying the Runtime or the
Assembler, just run the default targets of the build scripts.

## Examples

The `samples` folder include three assembler files:

1. `List.jas`: implentation of some classic functions on lists, such as `take` and `filter`
2. `Fib.jas`: compute the fibonacci sequence as an infinite list, take the initial 100 elements.
3. `Primes.jas`: compute the first 10000 primes using trail-division.

