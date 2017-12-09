# Functional Programming in Scala (Take 2)

**Spoiler alert**: contains answers to exercises in *Functional Programming in Scala*.

<img src="images/fp-in-scala-cover.jpg" height="384" width="306">

## Notes

I also have [some notes](https://github.com/bryanesmith/notes/tree/master/chiusano-et-al.functional-programming-in-scala).

## Install

Easiest options are Intellij for IDE or sbt for terminal.

### Intellij
1. Install [Intellij](https://www.jetbrains.com/idea/download) (recommend 2017.2.5 or later)
2. Configure Java, Scala SDKs
    1. Go to File > Project Structure > SDKs, and add a JDK (recommend 1.8 or later)
    2. Go to Preferences > Plugins > Install JetBrains Plugin > Scala (recommended 2017.2.13 or later)
3. Import project
    1. File > New > Project from Existing Sources, and select this directory
    2. Select Import project from external model > SBT
    3. Click through until done
4. Can select source or test files in project navigator, then select Run > Run 'FooTest'

### sbt

1. Install [sbt](http://www.scala-sbt.org/download.html) (recommend 1.0.4 or later)
2. From terminal:
    ```
    $ cd fp-inscala-v2
    $ sbt
    ```
3. To run:
    ```
    sbt:fp-in-scala-v2> run
    ```
4. To run tests:
    ```
    sbt:fp-in-scala-v2> test
    ```
5. Interactive console:
    ```
    sbt:fp-in-scala-v2> console
    scala> import ch2.Main._
    import ch2.Main._

    scala> fib(7)
    res0: Int = 13

    scala>
    ```