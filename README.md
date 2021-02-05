Ecstatic
=

Ecstatic is a Java command line application that utilizes the Java API of the Saxon XSLT and XQuery processor to check
an XPath expression or an XSLT stylesheet for static errors.

Ecstatic is copyright (c) 2021 by David Maus &lt;dmaus@dmaus.name&gt; and released under the MIT license.

Usage
-

Compile or download the 'fat' Java archive and execute one of the following classes:

```
java -jar <path/to/jar-with-dependencies> name.dmaus.util.ecstatic.CheckXslt [<filename>]
```

Checks <filename> for static errors. Reads from STDIN if filename is ommitted.

```
java -jar <path/to/jar-with-dependencies> name.dmaus.util.ecstatic.CheckXPath [<expression>]
```

Checks <expression> for static errors. Reads from STDIN if expression is ommitted.

