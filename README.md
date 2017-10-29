sheet-parsing-tests
-------------------

Basic tests of parsing xlsx sheets by different libraries

Usage
======

Run with:

    stack bench

By default `testSheetNoPrologue.xml` used so `xeno` could be tested
(as of the moment of this writing DOM in `xeno` doesn't work with XML
Prologue).

To test with other file use:


    XMLFILE=<your file> stack bench
