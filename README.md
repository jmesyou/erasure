# Clojure Whitespace Compiler

`clj-whitespace` is a Whitespace compiler and runtime which produces an intermediate code representation in 
Clojure. 

This compiler was built for the term project of **COMPSCI 4TB3** at _McMaster University_
## Installation

### Dependencies

* [Java](https://www.java.com/en/)
* [Clojure](https://clojure.org/) (If building from source)

### Build from Source

Install [Leiningen](https://leiningen.org/)

From project root:
```
$ lein compile
$ lein uberjar
```
### Standalone Jar
Download from https://github.com/JacksYou/clj-whitespace/blob/master/target/uberjar/clj-whitespace-1.0-standalone.jar.

## Usage

```
$ java -jar clj-whitespace-1.0-standalone.jar [options] file
```

## Options

```
-i, --intermediate  Execute Clojure-Whitespace source generated by the compiler
-c, --compile       Compile whitespace program into intermediate code
-h, --help
```

## Test Cases

Test modules exist in `test/clj_whitespace`, they are minimal given the time constraints of the project.
However, they are fully extensible by any user (provided they know Clojure).

Tests can be executed using:

```
$ lein test
```

## Examples

There are two sample programs located in `resources`. `helloworld.ws` is purely Whitespace
source while `count.c` is a Whitespace program embedded into C source which mirrors functionality.

for example, to run `count.c`:

from standalone jar:

```
$ java -jar target/uberjar/clj-whitespace-1.0-standalone.jar resources/count.c
```

from lein:

```
$ lein repl
clj-whitespace.core=> (-main "resources/count.c")
```

## License

Distributed under the MIT Public License 