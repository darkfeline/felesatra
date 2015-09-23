---
title: Introduction to ed
date: 2015-02-22
publishdate: 2015-02-22
subjects:
  - "programming"
contents:
  - "article"
---

## What is ed?

ed is the standard UNIX text editor, although it is more specifically a line
editor: it edits lines of text.  It is required to be installed on UNIX
systems and many text editors are descendants of ed.

ed was originally written by Ken Thompson in 1971 and later worked on by
others, notably Dennis M. Ritchie.

## Why ed?

There are many reasons why one might want to become familiar with ed.

First, it is a powerful tool, despite its simplicity and age.  Many simple text
editors cannot match ed in features or ease of scripting even today.

Second, even if you were to use more powerful text editors in your work (which
is a smart thing to do), you may find yourself in a situation where ed is the
only text editor available, for it is the standard UNIX editor, and every UNIX
system is required to have it, unlike other editors such as vi or Emacs.
Alternatively, a system may become so broken that no other editors will work
any more, for example, due to terminal or library issues.  In such a situation,
knowledge of ed will help you immensely.

Third, many text editors are descended from ed, such as vi, Vim, sam, and acme,
and an understanding of ed will help you learn how to use these editors and
understand their design.  The standard UNIX tools sed and awk also draw from
ed.

Fourth, even if you have no interest in ed as a text editor, it still provides
a strong example of good UNIX design.

Finally, if you ever accidentally run ed, you will know how to exit it:

    golem$ ed

    ?
    help
    ?
    ?
    ?
    quit
    ?
    exit
    ?
    bye
    ?
    hello?
    ?
    eat flaming death
    ?
    ^C
    ?
    ^C
    ?
    ^D
    ?

(Taken from [ed humor][humor].)

[humor]: http://www.gnu.org/fun/jokes/ed.msg.html


## Where to get help

Like all UNIX software, the full and canonical documentation for ed can be
found in its man page.  Not all versions of ed are identical, so the man page
on the system you are using should be considered the most correct.  However,
the implementation of ed is pretty well-defined, so in practice you should
not have much problem using any man page for it found on the Internet.

I will be using [GNU ed].  [Its documentation][docs] can be found online.

[GNU ed]: https://www.gnu.org/software/ed/
[docs]: https://www.gnu.org/software/ed/manual/ed_manual.html

## Using ed

For the purposes of this tutorial, I will use the following passage from
<i>Alice's Adventures in Wonderland</i> by Lewis Carroll, taken from
[Project Gutenberg].

[Project Gutenberg]: http://www.gutenberg.org/

    ALICE was beginning to get very tired of sitting by her
    sister on the bank, and of having nothing to do: once or twice she had
    peeped into the book her sister was reading, but it had no pictures or
    conversations in it, "and what is the use of a book," thought Alice,
    "without pictures or conversations?"

### Starting ed

Starting ed is simple.

    $ ed

Notice that ed does not print anything.  This is an example of `ed`'s
terseness, which will become familiar as you use ed, and hopefully, even
comforting (if, God forbid, you find yourself needing to edit a text file with
only ticker tape as your output, printing at the speed of a single character
per second, this terseness will become like ambrosia).

The command to quit is also simple: `q`.  ed works similar to all command
line interfaces; you press Enter or Return after each command.

    $ ed
    q
    $

Whenever ed becomes unhappy (it reports an error or warning), it will print a
succinct `?`.  You can get more information using the command `h`.

    $ ed
    u
    ?
    h
    Nothing to undo

### Editing files

ed edits files by keeping a buffer that is then written to the respective
file after modification.  This is how all text editors work, but it's simple to
conceptualize with ed:

            read       ed      modify             save
    ( file ------> ) buffer ( --------> buffer ) ------> file

When you run `ed filename`, you tell ed to edit the given file.  That means
that ed will read the text in the file into its buffer and set its default
filename to the one you gave it, so that ed will save the buffer back to that
file by default.  The edit command is `e`, so `ed filename` is equivalent to:

    $ ed
    e filename

ed prints out the number of bytes it read whenever it reads in a file:

    $ ed alice.txt
    304

### Printing, moving around, scrolling

The `p` command is used for printing lines.

    $ ed alice.txt
    304
    p
    "without pictures or conversations?"

By default, `p` prints the current line, which is set to the last line after ed
reads in a file.  You can also print a specific line by number, or a range of
lines.

    $ ed alice.txt
    304
    1p
    ALICE was beginning to get very tired of sitting by her
    2p
    sister on the bank, and of having nothing to do: once or twice she had
    1,2p
    ALICE was beginning to get very tired of sitting by her
    sister on the bank, and of having nothing to do: once or twice she had

Note that the current line is set to the last line printed after every `p`
command.  As a general rule, ed will set the current line to the last line
affected by each command.

    $ ed alice.txt
    304
    p
    "without pictures or conversations?"
    3p
    peeped into the book her sister was reading, but it had no pictures or
    p
    peeped into the book her sister was reading, but it had no pictures or

You can use `.` to refer to the current line, `$` to refer to the last line in
the buffer, and `,` as a shorthand for `1,$`, i.e., the whole buffer.

    $ ed alice.txt
    304
    .p
    "without pictures or conversations?"
    1p
    ALICE was beginning to get very tired of sitting by her
    $p
    "without pictures or conversations?"
    1,$p
    ALICE was beginning to get very tired of sitting by her
    sister on the bank, and of having nothing to do: once or twice she had
    peeped into the book her sister was reading, but it had no pictures or
    conversations in it, "and what is the use of a book," thought Alice,
    "without pictures or conversations?"
    ,p
    ALICE was beginning to get very tired of sitting by her
    sister on the bank, and of having nothing to do: once or twice she had
    peeped into the book her sister was reading, but it had no pictures or
    conversations in it, "and what is the use of a book," thought Alice,
    "without pictures or conversations?"

You can also address lines relative to other lines.  For example, `.-` or `.-1`
means the line before the current line, `.--` or `.-2` means the line before
that, `.+` or `.+1` means the line after the current line, and `.++` or `.+2`
means the line after that.  Here are some more examples: `$---` means the line
three before the last line, and `30+5` means the fifth line after the 30th
line, or the 35th line line.

    $ ed alice.txt
    304
    # Typing a line address by itself is the same as a print: ed will print
    # that line and set it as the current line.
    3
    peeped into the book her sister was reading, but it had no pictures or
    # Print the range of lines starting from the line before the current line
    # to the line after the current line.
    -,+p
    sister on the bank, and of having nothing to do: once or twice she had
    peeped into the book her sister was reading, but it had no pictures or
    conversations in it, "and what is the use of a book," thought Alice,

Thus, you can move around the file using `-` and `+`.

    $ ed alice.txt
    304
    -
    conversations in it, "and what is the use of a book," thought Alice,
    -
    peeped into the book her sister was reading, but it had no pictures or
    -
    sister on the bank, and of having nothing to do: once or twice she had
    +
    peeped into the book her sister was reading, but it had no pictures or
    +
    conversations in it, "and what is the use of a book," thought Alice,

ed also provides the `z` command for scrolling through lines (I use a longer
text here for demonstration).

    $ ed alice2.txt
    1677
    1
    ALICE was beginning to get very tired of sitting by her
    z1 
    sister on the bank, and of having nothing to do: once or twice she had
    peeped into the book her sister was reading, but it had no pictures or
    z2
    conversations in it, "and what is the use of a book," thought Alice,
    "without pictures or conversations?"

    z3
    So she was considering in her own mind (as well as she could, for the
    hot day made her feel very sleepy and stupid) whether the pleasure of
    making a daisy-chain would be worth the trouble of getting up and
    picking the daisies, when suddenly a White Rabbit with pink eyes ran
    z4
    close by her.

    There was nothing so _very_ remarkable in that; nor did Alice think it
    so _very_ much out of the way to hear the Rabbit say to itself, "Oh
    dear! Oh dear! I shall be too late!" (when she thought it over
    z5
    afterwards, it occurred to her that she ought to have wondered at this,
    but at the time it all seemed quite natural); but when the Rabbit
    actually _took a watch out of its waistcoat-pocket_, and looked at it,
    and then hurried on, Alice started to her feet, for it flashed across
    her mind that she had never before seen a rabbit with either a
    waistcoat-pocket, or a watch to take out of it, and burning with

You can see this makes scrolling through a file much easier.  By default, `z`
prints as many lines as can fit in your window, which may be impractical if
your window is very tall.

### Inserting lines, changing lines

The `a` command lets us append new lines.  After invoking the command, you can
type lines for ed to add, followed by a single period on its own line to tell
ed you're finished.

    $ ed alice.txt
    304
    ,p
    ALICE was beginning to get very tired of sitting by her
    sister on the bank, and of having nothing to do: once or twice she had
    peeped into the book her sister was reading, but it had no pictures or
    conversations in it, "and what is the use of a book," thought Alice,
    "without pictures or conversations?"
    a

    You can use it as a doorstop.
    .
    ,p
    ALICE was beginning to get very tired of sitting by her
    sister on the bank, and of having nothing to do: once or twice she had
    peeped into the book her sister was reading, but it had no pictures or
    conversations in it, "and what is the use of a book," thought Alice,
    "without pictures or conversations?"

    You can use it as a doorstop.

By default, `a` adds the lines after the current line, but you can give ed a
different line.

    $ ed alice.txt
    304
    1a
    We interrupt this story for no reason.
    .
    ,p
    ALICE was beginning to get very tired of sitting by her
    We interrupt this story for no reason.
    sister on the bank, and of having nothing to do: once or twice she had
    peeped into the book her sister was reading, but it had no pictures or
    conversations in it, "and what is the use of a book," thought Alice,
    "without pictures or conversations?"

There's a complementary `i` command that inserts lines before instead of after.
It exists only for convenience.

Similarly, there's the `d` command for deleting lines.  By default it deletes
the current line, but you can give it a specific line or range of lines.

    $ ed alice.txt
    304
    d
    ,p
    ALICE was beginning to get very tired of sitting by her
    sister on the bank, and of having nothing to do: once or twice she had
    peeped into the book her sister was reading, but it had no pictures or
    conversations in it, "and what is the use of a book," thought Alice,
    2,3d
    ,p
    ALICE was beginning to get very tired of sitting by her
    conversations in it, "and what is the use of a book," thought Alice,

Knowing `a`, `d`, and `p` is enough to use ed to make any changes you want.
Often, though, you will want to change existing lines, and repeatedly typing
`d` and `i` gets old fast.  ed provides the `c` command to make changing lines
easier.  Functionally, it's identical to the `d` and `i` commands chained
together.

    $ ed alice.txt
    304
    1p
    ALICE was beginning to get very tired of sitting by her
    c
    Alice was beginning to get very tired of sitting by her
    .
    p
    Alice was beginning to get very tired of sitting by her

Another convenience command is `j`, which joins lines together.

    $ ed alice.txt
    304
    1,2c
    Here's my version of Alice.
    It's not very interesting.
    Oops, I didn't mean to break those up.
    .
    ,p
    Here's my version of Alice.
    It's not very interesting.
    Oops, I didn't mean to break those up.
    peeped into the book her sister was reading, but it had no pictures or
    conversations in it, "and what is the use of a book," thought Alice,
    "without pictures or conversations?"
    1,2j
    ,p
    Here's my version of Alice.It's not very interesting.
    Oops, I didn't mean to break those up.
    peeped into the book her sister was reading, but it had no pictures or
    conversations in it, "and what is the use of a book," thought Alice,
    "without pictures or conversations?"

Notice that `j` doesn't insert any spaces.  We could fix that mistake using
`c`, but retyping all of that is no fun.  As you may have noticed, ed deals
strictly with lines of text, so fixing errors within a line is difficult.
However, ed provides the `s` command to substitute text within lines.

    $ ed alice.txt
    304
    1p
    ALICE was beginning to get very tired of sitting by her
    s/ALICE/Alice/
    p
    Alice was beginning to get very tired of sitting by her

ed uses regular expressions, which are a different topic entirely so I won't
cover it here.  If you aren't familiar with them, think of regular expressions
as rules to match different patterns of strings.

By default, `s` only works on the first match in the line.  You can add a `g`
suffix to work on all matches, or a number to work on only the given match.

    $ ed alice.txt
    304
    1p
    ALICE was beginning to get very tired of sitting by her
    # Capitalize the second lowercase "t"
    s/t/T/2
    p
    ALICE was beginning to geT very tired of sitting by her
    # Capitalize all lowercase "t"s
    s/t/T/g
    p
    ALICE was beginning To geT very Tired of siTTing by her

### Writing

So far we've covered making changes, but make sure to write them to a file
before quitting.  If you try to do so, ed will helpfully warn you:

    $ ed alice.txt
    304
    d
    q
    ?
    h
    Warning: buffer modified

Use the `w` command to write the buffer to a file.  By default it will write to
the default filename (refer back to editing files for a reminder), but you can
supply it with a filename as well.  Like when it reads a file, ed will print
the number of bytes written afterward.

    $ ed alice.txt
    304
    d
    w tmp
    267

`w` will set the default filename to the one you've given it only if it wasn't
set before.  You can set the default filename manually using `f`.

If you want to quit without writing, you can use `Q`, or invoke `q` twice in a
row to silence ed's protests:

    $ ed alice.txt
    304
    d
    q
    ?
    q
    $

### Global command and shell interaction

ed has quite a few more features; I won't mention them, as they aren't
necessary to gain an understanding of basic ed usage.  There are two that are
important, however.

ed has four commands for running other commands on all of the lines that match
(or don't match) a pattern: `g`, `G`, `v`, and `V`.  These commands can be used
in a huge variety of ways limited only by your imagination; naturally, I won't
try to show them all.  One common way it is used is to print every line that
matches a pattern:

    $ ed alice.txt
    304
    # Print every line that contains "re"
    g/re/p
    ALICE was beginning to get very tired of sitting by her
    peeped into the book her sister was reading, but it had no pictures or
    "without pictures or conversations?"

This is in fact where the name for grep comes from; the "re" stands for
"regular expression".

Another important feature is shell interaction.  ed is UNIX's standard text
editor; it works together with the rest of UNIX to provide all the features of
a development environment or of more advanced editors.

You can run a shell command within ed using `!`.  ed will print output from the
command; when the command terminates, ed prints a single `!`.

    $ ed
    !date
    Sun Feb 22 02:17:21 EST 2015
    !

You can also read output from a shell command into the buffer or write lines
from the buffer to the input of a shell command.  All of this is consistent
with the UNIX philosophy of letting other tools do the work instead of adding
the same features to every single program.

    $ ed alice.txt
    304
    # We read the output of the date program into the buffer.  The "r" command
    # tells us how many bytes it read.
    r !date
    29
    ,p
    ALICE was beginning to get very tired of sitting by her
    sister on the bank, and of having nothing to do: once or twice she had
    peeped into the book her sister was reading, but it had no pictures or
    conversations in it, "and what is the use of a book," thought Alice,
    "without pictures or conversations?"
    Sun Feb 22 02:20:51 EST 2015
    # We sort the lines in the file using the sort program.  It prints them
    # out, followed by the number of bytes written, similar to "r".
    w !sort
    "without pictures or conversations?"
    ALICE was beginning to get very tired of sitting by her
    Sun Feb 22 02:20:51 EST 2015
    conversations in it, "and what is the use of a book," thought Alice,
    peeped into the book her sister was reading, but it had no pictures or
    sister on the bank, and of having nothing to do: once or twice she had
    333
    # We can write the sorted lines to a file and then read them back.
    w !sort > tmp
    333
    ,d
    r tmp
    333
    ,p
    "without pictures or conversations?"
    ALICE was beginning to get very tired of sitting by her
    Sun Feb 22 02:20:51 EST 2015
    conversations in it, "and what is the use of a book," thought Alice,
    peeped into the book her sister was reading, but it had no pictures or
    sister on the bank, and of having nothing to do: once or twice she had

## Conclusion

Thus concludes this introduction to ed.  I hope it was of use to you, dear
reader.
