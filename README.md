# Efficient BigInteger Implementation

This is an improved version of `java.math.BigInteger` that uses fast algorithms for multiplying and dividing large numbers. It is based on [Alan Eliasen's BigInteger patch](http://futureboy.us/temp/BigInteger.java) which provides the Karatsuba and Toom-Cook implementations.

Depending on the input size, numbers are multiplied using [Long Multiplication](http://en.wikipedia.org/wiki/Multiplication_algorithm#Long_multiplication), [Karatsuba](http://en.wikipedia.org/wiki/Karatsuba_algorithm), [Toom-Cook](http://en.wikipedia.org/wiki/Toom%E2%80%93Cook_multiplication), or [Schönhage-Strassen](http://en.wikipedia.org/wiki/Sch%C3%B6nhage%E2%80%93Strassen_algorithm).
For division, [Long Division](http://en.wikipedia.org/wiki/Long_division), [Burnikel-Ziegler Division](http://cr.yp.to/bib/1998/burnikel.ps), or [Barrett Division](http://en.wikipedia.org/wiki/Barrett_reduction) is used.

# Acknowledgements

YourKit is kindly supporting my BigInt implementation with its full-featured Java Profiler.
YourKit, LLC is the creator of innovative and intelligent tools for profiling
Java and .NET applications. Take a look at YourKit's leading software products:

[YourKit Java Profiler](http://www.yourkit.com/java/profiler/index.jsp)

[YourKit .NET Profiler](http://www.yourkit.com/.net/profiler/index.jsp)
