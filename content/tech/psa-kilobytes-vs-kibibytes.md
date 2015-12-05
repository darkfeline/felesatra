---
contents:
- article
- psa
date: 2015-12-05T09:37:08-08:00
publishdate: 2015-12-05
subjects:
- programming
title: "PSA: Kilobytes vs kibibytes"
---

Units are important.  If you do not use units properly, you end up running into
problems, such as
[losing a $125 million USD spacecraft](http://spacemath.gsfc.nasa.gov/weekly/6Page53.pdf).
Oops.

There's a particularly prevalent variant of this problem in the computer world:
the confusion between metric prefixes and binary prefixes.

One kilometer (km) is one thousand meters.  One kilobyte (kB) is one thousand
bytes.  Not 1024 bytes, and no, just because computers work with ones and zeros
does not give bytes a free pass to start modifying standardized, internationally
recognized prefixes.  (Also note that it is abbreviated kB, not KB and not Kb
and not kb.)

Don't worry, though, because there are standardized, internationally recognized
binary prefixes to make things more convenient for computers.  One kibibyte
(written KiB) is 1024 bytes, and yes, one kibimeter is 1024 meters and not one
thousand meters.  Of course, there are binary prefixes for all of your favorite
metric prefixes: mebi (Mi) for mega (M), gibi (Gi) for giga (G), and so on.

Why does this matter?  Well, the next time you find that your 30 "GB" of
pictures don't fit on a hard drive with 31 GB of free space, blame the
engineers[^1] behind your operating system and programs; those 30 "GB" are actually
30 GiB, or 31.5 GB.  Oops.

We teach our middle school students about units, and yet grown adult engineers
and programmers can't even use them correctly.  That's a problem.

Luckily, you now know better, and with some more luck, you can tell your
friends, who will tell their friends, and perhaps one day we'll get this problem
fixed.

You can read more about binary prefixes on
[the Wikipedia article](https://en.wikipedia.org/wiki/Binary_prefix) for more
information.

[^1]: Some people might object to calling programmers engineers.  I think we
    should expect programmers to work under the same standards as engineers,
    considering how much software underlies modern society.
