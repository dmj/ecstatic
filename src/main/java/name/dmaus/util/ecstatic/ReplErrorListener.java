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

import javax.xml.transform.ErrorListener;
import javax.xml.transform.SourceLocator;
import javax.xml.transform.TransformerException;

import net.jcip.annotations.ThreadSafe;

/**
 * Print errors in the REPL.
 */
@ThreadSafe
final class ReplErrorListener implements ErrorListener
{
    public void error (final TransformerException exception)
    {
        print(exception, "error");
    }

    public void fatalError (final TransformerException exception)
    {
        print(exception, "error");
    }

    public void warning (final TransformerException exception)
    {
        print(exception, "warning");
    }

    private void print (final TransformerException exception, final String prefix)
    {
        int[] location = getLocation(exception);
        String message = exception.getMessage();
        System.out.format("%s:%d:%d:%s%n", prefix, location[0], location[1], message);
    }

    private int[] getLocation (final TransformerException exception)
    {
        int[] location = {0, 0};
        SourceLocator locator = exception.getLocator();
        if (locator == null) {
            return location;
        }
        if (locator.getLineNumber() >= 0) {
            location[0] = locator.getLineNumber();
        }
        if (locator.getColumnNumber() >= 0) {
            location[1] = locator.getColumnNumber();
        }
        return location;
    }

}
