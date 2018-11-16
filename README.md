---
title:  Stackshot  
author: David Baynard  
date:   16 Nov 2018  
fontfamily:   libertine
csl:    chemical-engineering-science.csl
link-citations: true
abstract: |  
    
...

Check updates for packages in a haskell snapshot, using `stack`.

# Use

    > stackshot --input _infile_ --output _outfile_

Take a snapshot _infile_ and produce a list of packages in _outfile_.

Handles some github packages.

Note that github packages will not be produced in the output — sadly these must be checked manually.
