---
contents:
- article
date: 2016-02-28
publishdate: 2016-02-28
subjects:
- programming
- solarized
title: On Solarized
---

[Solarized](http://ethanschoonover.com/solarized) is a popular color scheme made
by Ethan Schoonover which is very popular among programmers.  This article
discusses Solarized in more depth, or rather, criticizes it for some of its
failings.  You have been warned.

## Solarized's color palette

Solarized is first and foremost a color *palette*, and not a color *theme*.  For
now, I'll ignore how well Solarized works for, e.g., Vim, and talk strictly
about the set of colors that comprise it.

Solarized consists of sixteen colors (to match the sixteen colors of terminals):
8 accent colors, 4 content tones, and two sets of 2 background tones.  What this
means is that if you are using Solarized's color palette, you have the following
options for making a color theme: a standard background color, a "highlight"
background color, three levels of emphasis for the standard foreground color
(not four, see Solarized's page for details), and eight additional foreground
colors.  To be honest, this is quite sufficient for most user interface
purposes; using more colors (simultaneously, anyway) would make a user interface
into rainbow soup.

## Solarized's design

Here is where I have some of the biggest gripes with Solarized.  Solarized's
colors were picked from the CIELAB color space to maximize contrast and reduce
eye strain and so on and so forth.  This is a very programmer-like approach to
the problem of designing a color palette and the concept alone is likely to
appeal to programmers.

However, as a programmer, I object to the assumption that this is the ideal way
to choose a color palette:

- Artists have spent centuries accumulating knowledge on color theory that
  cannot be simplified to "pick a few colors at specified points on the color
  wheel".
- Each person perceives light differently.  (Refer to the notorious
  black/gold/white dress phenomenon for some food for thought.)
- How the color palette is used matters just as much as which colors you put in
  your palette.  Even if the palette is good, if the user interface uses it
  poorly, the end result will be poor.

## User interface design

I do not have a lot of experience with user interface design, but for various
reasons I'd like to think I have some authority to speak on the subject.  I am
aware that designing user interfaces is a difficult problem.  One approach to
the problem of coloring in particular is to select a color palette beforehand
and apply it consistently across the entire object to be designed (be it a
physical product, a web site, or a program's user interface).

This is a great approach generally, but it sucks for programmers.  This is best
demonstrated through an example:

Bob is a programmer working with his (fictional) Vimacs editor.  He has many
plugins installed, and he's using Solarized as his color theme.  One of the
plugins provides syntax highlighting, which makes use of the eight colors and
three foreground tones to add visual structure to edited code and increase Bob's
productivity.

However, another plugin which provides static code checking also makes use of
the same set of colors to highlight errors in the code.  Bob regularly gets
confused from the poor interaction between the two plugins, lowering his
productivity.  He wants to fix this problem.  However, due to Solarized's
limited color palette, and furthermore because this color palette is already
being fully used for highlighting, and *these colors were originally chosen to
maximize contrast, any additional color that Bob tries to use will have poor
contrast with the existing palette*.

Ultimately, it would be better to choose colors in a somewhat ad-hoc manner,
because whether or not a color provides good contrast depends on which colors it
is used with.  It's okay to use two similar colors if those colors will never
have to be distinguished from each other in the same context, and using two
different colors will give you more leeway in using colors in the big picture.
A programmer's text editor or IDE has a lot of parts integrating in various
ways, so having this leeway in color selection is valuable.

This is in fact an interesting variant on the graph coloring problem, except in
this case we aren't worried about using the minimum number of colors, but rather
maximizing the contrast between adjacent nodes while minimizing "glaring
contrast" and "eye strain".  It should be immediately apparent that the optimal
solution to this problem *cannot* be reached by limiting yourself to a small
palette of colors.

## Solarized in the terminal

Furthermore, Solarized is an especially poor choice for terminals.

Let's go over terminal colors quickly:

- Terminals have a color for the cursor.  Programs cannot change this color or
  use it for their foreground text or background color.
- Terminals have a foreground and background color.  Programs can use these
  colors as defaults.
- Color terminals also have 16 extra colors for programs to use: 8 regular color
  and 8 "bright" versions of those regular colors.  These are: Black, Red,
  Green, Yellow, Blue, Magenta, Cyan, and White.  Programs can use these colors
  for both foreground text and background highlight.  However, programs do not
  actually know what these colors are and can only assume that when, e.g.,
  they request Green that the terminal is configured to use a color that's kind
  of like green.
- Terminals with 256 color support additionally have extra colors numbered 16
  through 255 (where 0 through 15 are the aforementioned 16 extra colors).
  Again, while there is an expected standard for what these colors are, programs
  can only request, e.g., color 144 and hope that it is close to what it expects
  color 144 looks like.

Here's where Solarized fall apart.  The official theme assigns the 8 accent
colors to the 8 regular terminal colors and the 8 grey-blue tones to the 8
bright colors.  When a program decides to output "good" messages in bright Green
and "bad" messages in bright Red, you're going to get, surprise!, a shade of
grey-blue and a shade of grey-blue.

Every program will need custom support for your Solarized terminal theme,
because the standard colors that they expect aren't standard anymore.

But wait, there's even a kicker.  Remember the default foreground and background
colors?  Most programs expect that these are distinguishable from all of the 16
extra colors, except perhaps White and Black.

However, Solarized only provides 16 colors, so you're going to have to reuse two
of them.  Specifically, you're going to use one of Solarized's content tones and
one of Solarized's background tones, which have been assigned to the terminal's
8 bright colors already.  So what happens if a program outputs bright Green text
and that "bright Green" is the same color as your background color?  Yes, the
text is invisible.  Lovely.

## Pick your own

So what's the conclusion?

For the terminal, you're better off not touching any of the default colors, or
at least using colors that are similar to their defaults.  In particular, Green,
Red, and Yellow should probably look like green, red, and yellow.

For any programs you have to run in the terminal, you can use a color theme that
works with 256 colors and of course, use a terminal that supports 256 colors.

If a terminal curses program has a GUI, you should just use the GUI.
[Terminals suck](http://catern.com/posts/terminal_quirks.html), and (n)curses is
a ugly mess of its own.  By using the GUI, you get reliable key bindings and a
much larger color space to work with, as well as windows management from your
OS.

If the program doesn't have a GUI or it sucks, the Right Thing is to write an
Emacs package for it, and automatically gain both Emacs's GUI support and
Emacs's terminal support.

As far as choosing a color theme goes, the best solution is to just try many
different themes, pick a handful you like the most, and be prepared to tweak
them as needed.  Everyone's eyes are different, everyone's preferences are
different, every programmer's needs are different (in terms of languages and
syntax highlighting and plugins).  There's no such thing as a free lunch, and if
you start prepared it'll be less work in the long run.  Programmers love to
optimize, but as far as color themes go, there isn't really anything to optimize
beyond the point where everything is clearly visible and doesn't hurt your eyes.

There's nothing inherently *wrong* with Solarized either, so if it works for
you, then great!  But for the reasons I have covered, don't be surprised if you
encounter some sharp corners.  And there's nothing wrong with switching between
color themes as necessary.
