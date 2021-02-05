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

import java.io.File;

import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;

import net.jcip.annotations.ThreadSafe;
import net.sf.saxon.s9api.Processor;
import net.sf.saxon.s9api.SaxonApiException;
import net.sf.saxon.s9api.XsltCompiler;

/**
 * Check XSLT for static errors.
 */
@ThreadSafe
public final class CheckXslt
{
    private final XsltCompiler compiler;

    CheckXslt (final XsltCompiler compiler)
    {
        this.compiler = compiler;
    }

    public static void main (final String[] args) throws Exception
    {
        Source source;
        if (args.length == 0) {
            source = new StreamSource(System.in);
        } else {
            source = new StreamSource(new File(args[0]));
        }
        
        Processor saxon = new Processor(false);
        XsltCompiler compiler = saxon.newXsltCompiler();
        
        CheckXslt cmd = new CheckXslt(compiler);
        try {
            cmd.check(source);
        } catch (SaxonApiException e) {
            System.exit(1);
        }
        System.exit(0);
    }

    void check (final Source source) throws SaxonApiException
    {
        compiler.compile(source);
    }
}
