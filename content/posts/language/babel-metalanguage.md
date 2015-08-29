---
title: The Babel meta-language
date: 2015-01-01
---

Inspired by the passion of the creators behind the EXA_PICO universe[^1], I
tried experimenting with a small constructed language (conlang) myself.  The
result is Babel, a compact meta-language.

[^1]:

    The EXA_PICO universe comprises two video games series of three games and
    two games, respectively.  For the setting of these five games, six complete
    conlangs were made: two dialects of Hymmnos, Central Standard Note and New
    Testament of Pastalie, Carmena Foreluna, Ar Ciela, REON-4213, and Emotional
    Song Pact.  There are in fact many more languages that exist in the
    universe, but these six appear extensively in official materials and exhibit
    a reasonably complete grammar and lexicon.  After seeing the effort its
    creators have put into the EXA_PICO universe, it becomes hard to take any
    fictional work seriously if it does not come with its own complete conlang.

Babel possesses a Lisp-like grammar, but otherwise strictly deals in semantic
values and semantic forms.  Also, Babel does not contain any meaning itself,
but relies on binding semantic forms from other languages.  Thus, it is a
meta-language in the truest sense of the word.

First, Babel presents a way to import other languages as a language object:

    (import english)
    (import japanese)
    (import hymmnos)

(I'll be borrowing some [Hymmnos] to demonstrate some interesting constructions
later using Babel.)

[Hymmnos]: http://conlang.wikia.com/wiki/Hymmnos

(Presumably, were one to "implement" Babel, these languages can be implemented
as separate packages.)

These language objects are used to bind semantic values, semantic forms, and
semantic classes, which comprise the essence of Babel.

A semantic value is any individual unit of meaning.  Grammatically, semantic
values generally correspond to words or complete phrases.  The main contrast is
with semantic forms which we'll see next.

    (val english "apple")
    (val japanese "弁当")

Every time we want to use the semantic value for the English word "apple", we
have to use `(val english "apple")`, which will get tedious fast as we move on
to semantic forms and compositions of multiple values and forms, so we can bind
objects to symbols in a straightforward manner:

    (def apple (val english "apple"))
    (def bentou (val japanese "弁当"))

Next are semantic forms, which define compositions of semantic values.  If
semantic values are objects, then semantic forms are functions; values are to
nouns as forms are to verbs, although keep in mind that verbs can be bound as
either values or forms.

    (def subject (class english "subject"))
    (def predicate (class english "predicate"))
    ; (sentence x y) -> x y .
    ; e.g. (sentence <I> <eat apple>) -> I eat apple.
    (def sentence
    (form english
        ((subject x)
        (predicate y))
        x y (val english ".")))

Here we also see classes being used to help define a semantic form.  We can
then apply that form to values that belong to the given classes to complete the
form, in this case, a sentence, like so:

    (def me (val english "I"))
    (def object (class english "object"))
    (def eat
    ((object x))
    (val english "eat") x)
    (sentence me (eat apple))  ; I eat (a/the) apple.

Here we use "eat" by binding it as a semantic form, but we can use it as a
value as well:

    (def eat (val english "eat"))
    (def verb1 (class english "verb1"))  ; verb taking one direct object
    (def do-verb1
    ((verb1 x)
    (object y))
    x y)
    (sentence me (do-verb1 eat apple))

That's all you need from Babel to say anything in any language you want.  But
as you can see, it is quite tedious.  We can take advantage of the Lisp grammar
to add macros to Babel to abstract away all of the boilerplate from defining
lots of classes, values, and forms, but the question one may ask is, why go
through all this trouble in the first place?  The answer is, using Babel allows
you to freely mix semantic forms and semantic values from any language you want.

For example, Hymmnos has the interesting semantic form of preceding each
sentence with three emotion words to describe the emotion behind the sentence:

> Was yea ra chs hymmnos mea.

where the meanings of the individual words are:

<dl>
  <dt>Was</dt>
  <dd>very much</dd>

  <dt>yea</dt>
  <dd>happiness</dd>

  <dt>ra</dt>
  <dd>I want this to last</dd>

  <dt>chs</dt>
  <dd>change</dd>

  <dt>hymmnos</dt>
  <dd>song</dd>

  <dt>mea</dt>
  <dd>me</dd>
</dl>

Hence, the meanings of the emotion preface and the declarative part of the
sentence are:

<dl>
  <dt>Was yea ra</dt>
  <dd>I am feeling very happy and I want this happiness to last for a long time.</dd>

  <dt>chs hymmnos mea</dt>
  <dd>(I) change myself into a song.</dd>
</dl>

which we can translate literally into ugly English:

> I am feeling very happy that I now change myself into a song, and I want this
> feeling to last for a long time.

or more fluently, but losing some of the original meaning:

> I am happy to change myself into a song.

Using Babel, we can use this semantic form in English:

    (def level1 (class hymmnos "emotion level 1"))
    (def level2 (class hymmnos "emotion level 2"))
    (def level3 (class hymmnos "emotion level 3"))
    (def predicate (class hymmnos "predicate"))
    (def hymmnos-emote
        (form hymmnos
        ((level1 a)
            (level2 b)
            (level3 c)
            (predicate p))
        a b c p))
    (def was (val hymmnos "was"))  ; very much
    (def ki (val hymmnos "ki"))  ; concentration
    (def ga (val hymmnos "ga"))  ; I want this to end soon
    (hymmnos-emote was ki ga (sentence me (eat bentou)))

We might render this directly as

> Was ki ga I eat 弁当

meaning

> I am concentrating very much as I eat the (Japanese) bentou, and I want this
> feeling to end soon.

This is a semantic idea that simply cannot be expressed very cleanly, or very
well at all in English, but by binding it through Babel, we can not only use
this Hymmnos semantic form with English, but also use the Japanese semantic
value 弁当 with English.  In this case, we might have used "bento" or "bentou"
as an English loan-word, but there are other conceptual values that are harder
to bring to English, such as 青, which means blue, green, black, or pale.

Ultimately Babel is a toy language, as most conlangs tend to be, but it is an
interesting study of semantics as well, and possibly a key first step in
bringing about strong AI (also known as general AI, or real AI, as opposed to
the fake AI of the oft-touted dumb algorithms of machine learning and neural
networks).
