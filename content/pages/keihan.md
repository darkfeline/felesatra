+++
title = "契絆想解詩"
date = "2015-09-22"
publishdate = "2015-08-28"
tablesorter = true
jquery = true
+++

## Introduction

<dl>
  <dt>Hiragana</dt>
  <dd>けいはんそうかいし</dd>

  <dt>Romanized</dt>
  <dd>Keihansoukaishi</dd>

  <dt>Localized</dt>
  <dd>Emotional Song Pact</dd>

  <dt>Abbreviated</dt>
  <dd>Keihan</dd>
</dl>

Keihan is a constructed language from the Surge Concerto video game series made
by GUST.  It is fairly complex; what follows are some of my notes on the
language.

## Resources

A lot of information can be found in officially released materials and other
pages:

- [Official Keihan generator](http://www.genomirai.com/WorldSettingDocument/language/)
- [Official Keihan World Setting page](http://www.genomirai.com/WorldSettingDocument/index.php?%25E5%25A5%2591%25E7%25B5%2586%25E6%2583%25B3%25E7%2595%258C%25E8%25A9%25A9)
- [EXA_PICO page (English translation)](http://artonelico.wikia.com/wiki/Emotional_Song_Pact)

## Writing

Since there does not exist a detailed description of Keihan's writing system at
this time, I have done some research and written one here.

Note that some of this information was derived experimentally and might be
inaccurate.  Please send me any contradictions you find.

### Characters

Keihan possesses a straightforward set of alphabet and punctuation characters.
Keihan may seem like a substitution of the Latin alphabet on the surface, but
that illusion shall disappear shortly.

Here is the alphabet:

<figure>
  <img src="/img/keihan/alphabet.jpg">
</figure>

There is no distinction between upper or lower case.

Here are the punctuation characters:

<figure>
  <img src="/img/keihan/punctuation.jpg">
</figure>

Ignore the leading `a` characters; they are there to force the official
character generator to render the punctuation.

From left to right, they are `;`, `?`, `!`, and `=`.  The semicolon is Keihan's
sentence terminator; it functions like a period.  The reason it is a semicolon
may be related to the strong computer themes in Surge Concerto (this
is obvious in the other language in the series, REON-4213).  The equal sign is
used as a topic or subject marker.  The question mark and exclamation mark
function like they do in English.

Keihan's numeric system is hexadecimal (again showing the computer theme):

<figure>
  <img src="/img/keihan/numbers.jpg">
</figure>

### Clusters

While the characters shown above are used for loanwords, they are never used
alone in Keihan itself.  Instead, they are combined into clusters consisting of
two, three, or four characters each.

#### Forms

Clusters come in seven different forms.

Clusters of two characters (2-clusters) have two possible forms.

<dl>
  <dt>Form A</dt>
  <dd>First at right, second at left.</dd>

  <dt>Form B</dt>
  <dd>First at bottom, second at top.</dd>
</dl>

<figure>
  <img src="/img/keihan/2-cluster.jpg">
</figure>

Clusters of three characters (3-clusters) have three possible forms.

<dl>
  <dt>Form C</dt>
  <dd>First at top right, second at bottom right, third at left</dd>

  <dt>Form D</dt>
  <dd>First at bottom right, second at bottom left, third at top</dd>

  <dt>Form E</dt>
  <dd>First at bottom, second at middle, third at top</dd>
</dl>

<figure>
  <img src="/img/keihan/3-cluster.jpg">
</figure>

Clusters of four characters (4-clusters) have two possible forms.

<dl>
  <dt>Form F</dt>
  <dd>First at bottom right, second at bottom middle, third at bottom left, forth at top</dd>

  <dt>Form G</dt>
  <dd>First at bottom, second at middle right, third at middle left, fourth at top</dd>
</dl>

<figure>
  <img src="/img/keihan/4-cluster.jpg">
</figure>

#### Rules

The rules for forming clusters is fairly complex.  Using the table below, find
the letter in the cluster with the highest priority and use the form indicated
by that letter.

These rules were determined experimentally from the official Keihan generator,
so they might be incorrect or sub-optimal.

(Table can be sorted with Javascript; try clicking on the the headers.)

<figure>
  <table class="tablesorter">
    <thead>
      <tr>
        <th>Letter</th>
        <th colspan="2">2-cluster</th>
        <th colspan="2">3-cluster</th>
        <th colspan="2">4-cluster</th>
      </tr>

      <tr>
        <th></th>
        <th>Form</th>
        <th>Priority</th>
        <th>Form</th>
        <th>Priority</th>
        <th>Form</th>
        <th>Priority</th>
      </tr>

    </thead>

    <col>
    <colgroup>
      <col>
      <col>
    </colgroup>
    <colgroup>
      <col>
      <col>
    </colgroup>
    <colgroup>
      <col>
      <col>
    </colgroup>

  <!-- BEGIN RECEIVE ORGTBL cluster-rules -->
<tr><td>a</td> <td>A</td> <td>1</td> <td>C</td> <td>1</td> <td>F</td> <td>1</td></tr>
<tr><td>a</td> <td>A</td> <td>1</td> <td>C</td> <td>1</td> <td>F</td> <td>1</td></tr>
<tr><td>b</td> <td>B</td> <td>2</td> <td>E</td> <td>2</td> <td>G</td> <td>2</td></tr>
<tr><td>c</td> <td>A</td> <td>1</td> <td>C</td> <td>1</td> <td>F</td> <td>1</td></tr>
<tr><td>d</td> <td>B</td> <td>2</td> <td>E</td> <td>2</td> <td>G</td> <td>2</td></tr>
<tr><td>e</td> <td>A</td> <td>1</td> <td>C</td> <td>1</td> <td>F</td> <td>1</td></tr>
<tr><td>f</td> <td>A</td> <td>1</td> <td>C</td> <td>1</td> <td>F</td> <td>1</td></tr>
<tr><td>g</td> <td>B</td> <td>2</td> <td>E</td> <td>2</td> <td>G</td> <td>2</td></tr>
<tr><td>h</td> <td>A</td> <td>1</td> <td>C</td> <td>1</td> <td>F</td> <td>1</td></tr>
<tr><td>i</td> <td>A</td> <td>1</td> <td>C</td> <td>1</td> <td>F</td> <td>1</td></tr>
<tr><td>j</td> <td>A</td> <td>3</td> <td>D</td> <td>3</td> <td>F</td> <td>3</td></tr>
<tr><td>k</td> <td>A</td> <td>1</td> <td>C</td> <td>1</td> <td>F</td> <td>1</td></tr>
<tr><td>l</td> <td>A</td> <td>1</td> <td>C</td> <td>1</td> <td>F</td> <td>1</td></tr>
<tr><td>m</td> <td>B</td> <td>4</td> <td>D</td> <td>3</td> <td>G</td> <td>6</td></tr>
<tr><td>n</td> <td>A</td> <td>1</td> <td>C</td> <td>1</td> <td>F</td> <td>1</td></tr>
<tr><td>o</td> <td>A</td> <td>1</td> <td>C</td> <td>1</td> <td>F</td> <td>1</td></tr>
<tr><td>p</td> <td>B</td> <td>2</td> <td>C</td> <td>1</td> <td>G</td> <td>4</td></tr>
<tr><td>q</td> <td>A</td> <td>1</td> <td>C</td> <td>1</td> <td>F</td> <td>1</td></tr>
<tr><td>r</td> <td>A</td> <td>1</td> <td>C</td> <td>1</td> <td>F</td> <td>1</td></tr>
<tr><td>s</td> <td>A</td> <td>1</td> <td>E</td> <td>4</td> <td>F</td> <td>5</td></tr>
<tr><td>t</td> <td>B</td> <td>2</td> <td>C</td> <td>1</td> <td>G</td> <td>4</td></tr>
<tr><td>u</td> <td>A</td> <td>1</td> <td>C</td> <td>1</td> <td>F</td> <td>1</td></tr>
<tr><td>v</td> <td>A</td> <td>1</td> <td>D</td> <td>3</td> <td>F</td> <td>3</td></tr>
<tr><td>w</td> <td>A</td> <td>1</td> <td>C</td> <td>1</td> <td>F</td> <td>1</td></tr>
<tr><td>x</td> <td>B</td> <td>2</td> <td>E</td> <td>2</td> <td>G</td> <td>2</td></tr>
<tr><td>y</td> <td>A</td> <td>1</td> <td>D</td> <td>3</td> <td>F</td> <td>3</td></tr>
<tr><td>z</td> <td>A</td> <td>1</td> <td>C</td> <td>1</td> <td>F</td> <td>1</td></tr>
  <!-- END RECEIVE ORGTBL cluster-rules -->
  <!--
  #+ORGTBL: SEND cluster-rules orgtbl-to-html-bare
  | a | A | 1 | C | 1 | F | 1 |
  | a | A | 1 | C | 1 | F | 1 |
  | b | B | 2 | E | 2 | G | 2 |
  | c | A | 1 | C | 1 | F | 1 |
  | d | B | 2 | E | 2 | G | 2 |
  | e | A | 1 | C | 1 | F | 1 |
  | f | A | 1 | C | 1 | F | 1 |
  | g | B | 2 | E | 2 | G | 2 |
  | h | A | 1 | C | 1 | F | 1 |
  | i | A | 1 | C | 1 | F | 1 |
  | j | A | 3 | D | 3 | F | 3 |
  | k | A | 1 | C | 1 | F | 1 |
  | l | A | 1 | C | 1 | F | 1 |
  | m | B | 4 | D | 3 | G | 6 |
  | n | A | 1 | C | 1 | F | 1 |
  | o | A | 1 | C | 1 | F | 1 |
  | p | B | 2 | C | 1 | G | 4 |
  | q | A | 1 | C | 1 | F | 1 |
  | r | A | 1 | C | 1 | F | 1 |
  | s | A | 1 | E | 4 | F | 5 |
  | t | B | 2 | C | 1 | G | 4 |
  | u | A | 1 | C | 1 | F | 1 |
  | v | A | 1 | D | 3 | F | 3 |
  | w | A | 1 | C | 1 | F | 1 |
  | x | B | 2 | E | 2 | G | 2 |
  | y | A | 1 | D | 3 | F | 3 |
  | z | A | 1 | C | 1 | F | 1 |
  -->

  </table>
</figure>

#### Mutations

In addition to forming clusters, each letter in Keihan also mutates according to its position in a cluster.

There are nine different mutations.

<dl>
  <dt>Pure</dt>
  <dd>Base form of character when not in a cluster.</dd>

  <dt>Vertical-0</dt>
  <dd>Used for the right character in form A.</dd>

  <dt>Horizontal-0</dt>
  <dd>Used for the bottom character in form B.</dd>

  <dt>Vertical-1</dt>
  <dd>Used for the left character in forms A and C.</dd>

  <dt>Horizontal-1</dt>
  <dd>Used for the top character in forms B, D, and F, and for the horizontal
    characters in forms E and G.</dd>

  <dt>Horizontal-2</dt>
  <dd>Used for the right characters in form C.</dd>

  <dt>Vertical-2</dt>
  <dd>Used for the bottom characters in form D.</dd>

  <dt>Vertical-3</dt>
  <dd>Used for the bottom characters in form F.</dd>

  <dt>Horizontal-3</dt>
  <dd>Used for the middle characters in form G.</dd>
</dl>

Here are all of the mutations for all letters:

<figure>
  <img src="/img/keihan/mutation-all.jpg">
</figure>

## Corrections

Please send me any corrections: darkfeline@felesatra.moe

## Notes

- The official Keihan generator seems to make the first cluster of each line
smaller.  I'm not sure if this is intentional or not.

## Changelog

### v1.1 2015-06-09

- Removed Appendix because it was poorly written.
- Removed alternate clustering rules because maintaining two sets of rules takes
extra work.
- Added rule generation algorithm, so now clustering rules are generated using
gathered data.  This should make things go more smoothly.
- Fixed clustering rules using observation by aquagon.
- Added sorting functionality to rules table.

### v1.0 2015-05-28

- Finished info on mutations.
- Typo correction by aquagon.
