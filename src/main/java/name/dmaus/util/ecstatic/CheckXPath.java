/*
 * Copyright (C) 2021 by David Maus <dmaus@dmaus.name>
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package name.dmaus.util.ecstatic;

import net.jcip.annotations.ThreadSafe;

import net.sf.saxon.s9api.Processor;
import net.sf.saxon.s9api.SaxonApiException;
import net.sf.saxon.s9api.XPathCompiler;


/**
 * Check XPath expression for static errors.
 */
@ThreadSafe
public final class CheckXPath
{
    private final XPathCompiler compiler;

    CheckXPath (final XPathCompiler compiler)
    {
        this.compiler = compiler;
    }

    public static void main (final String[] args) throws Exception
    {
        if (args.length == 0) {
            System.exit(2);
        }

        String expression = args[0];

        Processor saxon = new Processor(false);
        XPathCompiler compiler = saxon.newXPathCompiler();

        CheckXPath cmd = new CheckXPath(compiler);
        try {
            cmd.check(expression);
        } catch (SaxonApiException e) {
            System.err.println(e.getMessage());
            System.exit(1);
        }
        System.exit(0);
    }

    void check (final String expression) throws SaxonApiException
    {
        compiler.compile(expression);
    }
}
