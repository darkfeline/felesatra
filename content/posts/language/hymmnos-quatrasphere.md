---
title: Hymmnos Quatrasphere
date: 2015-01-04
publishdate: 2015-01-04
subjects:
  - "language"
contents:
  - "article"
---

[Hymmnos] is a language, or a set of dialects, created specifically for the Ar
Tonelico series of video games.  It is an extremely interesting language, being
a blend of a human language and a programming language with a heavy emphasis on
emotion, but there's one feature in particular that I want to talk about now:
Binasphere.

[Hymmnos]: http://conlang.wikia.com/wiki/Hymmnos

[Binasphere] is a construct whereby two lines of lyrics or text are interwoven,
such that they are said or sung simultaneously.  Here's an example shamelessly
pulled from the link above:

[Binasphere]: http://conlang.wikia.com/wiki/Hymmnos#Binasphere_Chorus

    => RRHA RRHA GUWO Ax GAx PEx GIS A GAx TIE INNx
    GIS NA GRAN GAx PAUL NOx TYUNY INI SAASH AR YANJE
    CIEL EN INI LA ZAx AR HHA CIEL RRHA RRHA Ax GUWO
    GA PEx GAx A TYUNY RA HARx AR CIEL
    TES EN YORA INI CHYET WAx SOR GAx LAx TYUNx SYE LA
    FORx GANx ART SA DAL FAYx WASSA RA CIEL
    EXEC hymme 2x1/0>>01101010

The text is decoded using the pattern provided at the very end, `01101010`.
Each binary digit indicates whether the corresponding word fragment belongs to
the first line or second line, and the pattern repeats until all of the words
are exhausted.  Word fragments that end in "x" are incomplete and should be
joined with the next word fragment in that line to form a complete word.

    RRHA           Ax     PEx     A GAx
    0    1    1    0  1   0   1   0/0   1   1
         RRHA GUWO    GAx     GIS       TIE INNx
    --------------------------------------------------
    RRHA Ax PEx A GAx
    RRHA GUWO GAx GIS TIE INNx

    GIS    GRAN     PAUL NOx           SAASH    YANJE
    0   1  0    1   0   /0   1     1   0     1  0
        NA      GAx          TYUNY INI       AR
    --------------------------------------------------
    GIS GRAN PAUL NOx SAASH YANJE
    NA GAx TYUNY INI AR

         EN INI        AR     CIEL      RRHA Ax
    1    0 /0   1  1   0  1   0    1    0   /0  1
    CIEL        LA ZAx    HHA      RRHA         GUWO
    --------------------------------------------------
    EN INI AR CIEL RRHA Ax
    CIEL LA ZAx HHA RRHA GUWO

       PEx     A       RA HARx
    1  0   1   0 1     0 /0    1  1
    GA     GAx   TYUNY         AR CIEL
    --------------------------------------------------
    PEx A RA HARx
    GA GAx TYUNY AR CIEL

    TES    YORA     CHYET WAx         LAx       SYE
    0   1  0    1   0    /0   1   1   0   1     0   1
        EN      INI           SOR GAx     TYUNx     LA
    --------------------------------------------------
    TES YORA CHYET WAx LAx SYE
    EN INI SOR GAx TYUNx LA

    FORx GANx        DAL      WASSA    CIEL
    0   /0    1   1  0   1    0     1  0
              ART SA     FAYx       RA
    --------------------------------------------------
    FORx GANx DAL WASSA CIEL
    ART SA FAYx RA

Then we join up all of the lines and combine word fragments.

The first chorus:

    RRHA Ax PEx A GAx
    GIS GRAN PAUL NOx SAASH YANJE
    EN INI AR CIEL RRHA Ax
    PEx A RA HARx
    TES YORA CHYET WAx LAx SYE
    FORx GANx DAL WASSA CIEL
    -------------------------------------------------------------
    Rrha apea gagis gran paul nosaash yanje en ini ar ciel.
    Rrha apea ra hartes yora chyet walasye forgandal wassa ciel.

    In this delightful trance, I feel that the way will be opened, for the
    goddess to purify this world forever.
    In the midst of this trance of delight, I will love your special persons,
    because of the festival of the world.

The second chorus:

    RRHA GUWO GAx GIS TIE INNx
    NA GAx TYUNY INI AR
    CIEL LA ZAx HHA RRHA GUWO
    GA GAx TYUNY AR CIEL
    EN INI SOR GAx TYUNx LA
    ART SA FAYx RA
    -------------------------------------------------------------
    Rrha guwo gagis tie innna gatyuny ini ar ciel la zahha.
    Rrha guwo ga gatyuny ar ciel en ini sor gatyunla art sa fayra.

    In this trance of hatred, I shall tie this curse to the inside of my mind,
    and then, I shall purify and make advance this world.
    In this transient anger, I shall purify this accursed world through the
    flames of the hell.

Note that these two choruses are being sung simultaneously, which in this case
sets their opposing feeling against one another in sharp relief: the first
chorus is singing about the act of purification with joy, and the second chorus
is singing about the same act with hatred.[^4]

[^4]:

    This fragment comes from EXEC_NULLASCENSION/., a song which has the power to
    destroy the world.  The power of the song reflects the usage of Binasphere
    within it; in the Ar Tonelico universe, the parallel execution of Binasphere
    is more powerful than a single line of lyric.

Here's an English example (if you actually want this feature in English, take a
look at [Babel]):

[Babel]: {{< ref "babel-metalanguage.md" >}}

    => BIx RDS THE YELLx OW SIx NG FLOx THEx WERS
    IR JOY SWx AY IN TO THE THE SKY WIND
    EXEC hymme 2x1/0>>1100011010

            THE YELLx OW        FLOx      WERS
    1   1   0   0     0  1   1  0    1    0
    BIx RDS              SIx NG      THEx
    ------------------------------------------
    THE YELLx OW FLOx WERS
    BIx RDS SIx NG THEx

           SWx AY IN        THE     WIND
    1   1  0  0   0  1  1   0   1   0
    IR JOY           TO THE     SKY
    ------------------------------------------
    SWx AY IN THE WIND
    IR JOY TO THE SKY

    THE YELLx OW FLOx WERS SWx AY IN THE WIND
    ------------------------------------------
    The yellow flowers sway in the wind.

    BIx RDS SIx NG THEx IR JOY TO THE SKY
    ------------------------------------------
    Birds sing their joy to the sky.

As a programmer, the Binasphere syntax is extremely attractive, and as a
programmer, I can't help but wonder whether we can do more, like, say, create a
Quatrasphere syntax which combines four lines into one.  First, though, we have
to break down the syntax to make sure we know how it works.

    => *text*
    EXEC hymme 2x1/0>>*pattern*

It's easy to see that `=>` is a beginning marker.  `EXEC` is "execute", as in
"run a process to decode the Binasphere text": `echo $text | exec decode
$pattern`. "Hymme" in Hymmnos means "sing, play (an instrument), resonate", but
as a noun, it means "waves, waving, vibration of formless things".[^1] In this
case, I'm willing to leave it as a "decode" symbol or function, as it's not
terribly important.

What is important, is the following `2x1/0>>01101010`.  This is Hymmnos Binary,
which follows the format `#x#>>####`.  The problem is figuring out what each of
the fields mean, since the syntax isn't used very often and therefore there
aren't many Hymmnos texts to reference.[^2] The third field is the binary
data—in this case, the pattern for the Binasphere.  The first field, `2` may
refer to a number of things.  It may be referring to the two lines of lyrics
that are interwoven in the Binasphere, or it may be referring to the fact that
the third field, the data field, is in base 2, binary.  Then there's the second
field, `1/0`, which may refer to the fact that the binary digits to use are 0
and 1, or that the range of acceptable digits are from 0 to 1, or it may be
indicating the association of the lyric lines to digits in reverse order.
Based on these guesses, the associated format for Quatrasphere may be any one
of the following:

    # 4 lines/base 4, digits ranging from 3 to 0, base 4 data
    4x3/0>>01230123

    # 4 lines/base 4, assign patterns, base 4 data
    4x3/2/1/0>>01230123

    # 4 lines, digits ranging from 11 to 0 base 2, binary data
    4x11/0>>0001101100011011

    # 4 lines, assign values, binary data
    4x11/10/1/0>>0001101100011011

    # 4 lines, patterns ranging from 11 to 00 base 2, binary data
    4x11/00>>0001101100011011

    # 4 lines, assing patterns, binary data
    4x11/10/01/00>>0001101100011011

    # binary data, values ranging from 11 to 0
    2x11/0>>0001101100011011

    # binary data, patterns ranging from 11 to 00
    2x11/00>>0001101100011011

    # binary data, assign values
    2x11/10/1/0>>0001101100011011

    # binary data, assign patterns
    2x11/10/01/00>>0001101100011011

[^1]:

    The Ar Tonelico universe also features its own laws of physics, called Wave
    Theory or Wave Science.  In reality, we have atoms in atomic physics (and
    strings in string theory), in Ar Tonelico, everything is made up of waves.
    Thus, "hymme" is an extremely important world in Hymmnos.

[^2]:

    Binasphere so far only appears in one text, the song EXEC_NULLASCENSION/.
    Hymmnos Binary also appears in EXEC_with.METHOD_METAFALICA/. ("Wee ki ra chs
    wasara dor en xest eazas yanyaue yor, xest 1x1101101001100110111000 >> syec
    mea."), but here it doesn't look very much like the one in Binasphere.

I think it's very likely that the data itself is in binary, as Hymmnos sticks
with binary for most data (for example, the song chmod b111000000/n uses
binary, instead of octal: chmod o700/n).[^3]  There isn't any way to figure out
the first or second fields though, so I just follow my hacker instincts and
assume that the first field indicates the number of lines, and the second field
indicates the range of values to use.  The grammar then looks like this:

    => *text*
    EXEC hymme 4x11/0>>*pattern*

[^3]:

    One exception I can think of is METHOD_HYMMELI/., which uses the New
    Testament of Pastalie dialect of Hymmnos:

    <pre>
    AsAwLYA/. reta lAnYAcLYAaLYE
    INFEL_PHIRA => ADDR:0x121:3461:3270 :: jYEwA KAIRA/.
    </pre>

    Here, the address appears to be octal.

where the pattern uses two digits for each word fragment, such that `00` refers
to the first line, `01` refers to the second line, `10` refers to the third
line, and `11` refers to the fourth line.  If we were to extend this to
Octasphere, we would then get something like

    EXEC hymme 8x111/0>>000001010011100101110111

and Sextasphere:

    EXEC hymme 6x100/0>>000001010011100

Before I wrap this up with an example of Quatrasphere in action, I should point
out that within the Ar Tonelico world, Hymmnos songs are programs that are
executed by a server and require processing power.  Thus, extending Binasphere
to Quatrasphere and beyond would be extremely resource-demanding, and it is
possible that they cannot be run by any of the servers or Reyvateils[^5] in the
series.

[^5]:

    In the Ar Tonelico universe, Reyvateils are artificial lifeforms that serve
    as "clients" to the Song Servers and are thus able to sing songs to execute
    on those servers.  Sometimes, they also take part in the execution of the
    songs that they sing.

And now I present to you an epic linguistic treat; I hope you enjoy it.

    => WAS HEMx CEZx LYE WAx S LYEMx GRANx YEA AR
    ME RA AEx SPHAEx LA ERRx A JE CHS HYMx AG CEx ME
    MATx ARx HOU CET MEA YAx EN AR YLYAx GRANx
    CIx YA ALE MEA DUS AR DOOx CIEx EL DU L
    EXEC hymme 4x11/0>>0010111101011001001001

    Chorus 1: Was yea ra hymme ar ciel.
    Chorus 2: Was granme erra chs cecet mea en grandus ar ciel.
    Chorus 3: hEmLYEmAr Aeje ag mAtYAyLYAyA doodu.
    Chorus 4: cEzLYE sphaela arhou ale mea.

    Chorus 1: I happily sing to create our planet.
    Chorus 2: I shall become a shield and protect the planet.
    Chorus 3: I happily sing my feeling to create the earth.
    Chorus 4: I am glad to become the sound of hope for the world.
