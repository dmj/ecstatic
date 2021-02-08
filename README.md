Ecstatic
=

Ecstatic is a Java command line application that utilizes the Java API of the Saxon XSLT and XQuery processor to check
an XPath expression or an XSLT stylesheet for static errors.

Ecstatic is copyright (c) 2021 by David Maus &lt;dmaus@dmaus.name&gt; and released under the MIT license.

Usage
-

Compile or download the 'fat' Java archive and execute one of the following classes:

### CheckXsl

```
java -jar <path/to/jar-with-dependencies> name.dmaus.util.ecstatic.CheckXslt [<filename>]
```

Checks <filename> for static errors. Reads from STDIN if filename is ommitted.

### CheckXPath

```
 java -jar <path/to/jar-with-dependencies> name.dmaus.util.ecstatic.CheckXPath [<expression>]
```

Checks <expression> for static errors. Reads from STDIN if expression
is ommitted.

### CheckXsltRepl

```
java -jar <path/to/jar-with-dependencies> name.dmaus.util.ecstatic.CheckXsltRepl
```

Continuosly waits for input from STDIN, checks it and reports any static errors found. The input is expected to be
terminated with U+0004 (END OF TRANSMISSION).

Ecstatic CheckXsltRepl Emacs library
-

The Emacs library implements a Flymake backend for XSLT stylesheets based on CheckXsltRepl.

Put it into ```load-path``` and ```(require 'ecstatic-checkxslt)```. To enable the Flymake backend turn on the
```ecstatic-checkxslt``` minor mode. Enabling it registers a Flymake backend function that starts a CheckXsltRepl
process for the current buffer and uses it to check for static errors.

You need to customize ```ecstatic-checkxslt-command``` with a commandline program that starts a CheckXstlRepl. The
variable can either be a string with the program name or a list with the program and command line arguments. It defaults
to ```checkxsltrepl```, assuming this to be a script available in PATH.
