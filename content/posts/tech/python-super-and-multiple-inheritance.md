---
title: Python, super(), and multiple inheritance
date: 2013-05-02
---

## The Situation

There's been a lot of discussion back when Python first introduced the
MRO and `super()` about how it works and how to use it while avoiding
some potentially unexpected behavior.  At this point, the general
consensus seems to be that `super()` has some advantages and some
disadvantages, and whether or not to use it is up to the user and/or the
problem that needs to be solved.  <a
href="https://fuhm.net/super-harmful/">Python's Super Considered
Harmful</a> and <a
href="https://rhettinger.wordpress.com/2011/05/26/super-considered-super/">Python's super() considered super!</a> are the two articles that come up most
often in such discussions.  But it seems to me that Python is really
doing doing something potentially revolutionary in object-oriented
programming that no one has realized yet.

First, I'll assume the reader is familiar with what Python's MRO (method
resolution order) is and how it works, as well as how `super()` works,
but I'll summarize it here because, what the heck.  The order in which
`super()` (which naturally delegates calls to superclasses) resolves
method calls is decided by the MRO.  The MRO for single inheritance
classes is straightforward, but it gets a little messy with multiple
inheritance; Python uses C3 Linearization to calculate the MRO.  For
simple scenarios, it's fine to assume a depth-first left-to-right
expansions, with duplicate classes being removed save for the last
instance.

## Proposed Solutions

The two commonly agreed-upon rules (from the second article above) is
that if you use `super()`, you need to

* make sure to use `super()` consistently, and
* use keyword arguments along with `**kwargs` in functions that will be
  extended.

This allows multiple inheritance to be used somewhat like pluggable
classes (not a technical term) in the OOP; superclasses provide a single
extra functionality and passes it on to the next, along with a hunk of
keyword arguments.  You can certainly get by taking this approach when
using `super()`, but I think it is ignoring a large part of `super()`'s
versatility.

## The Problem

Allow me to illustrate using the classic ABCD diamond.

      A
    /  \
    B   C
    \  /
      D

Here we have the MRO's

    A: [A, object]
    # A inherits from object (object is Python's base class)
    B(A): [B, A, object]
    # B inherits from A, A inherits from object
    C(A): [C, A, object]
    # C inherits from A, A inherits from object
    D(B, C): [D, B, C, A, object]
    # D inherits from B, B inherits from C, C inherits from A, A inherits from object

Anyone with some degree of familiarity with OOP should immediately see
an issue with that last one.

    D(B, C): [D, B, C, A, object]
    # D inherits from B, *B inherits from C*, C inherits from A, A inherits from object
    D(C, B): [D, C, B, A, object]
    # D inherits from C, *C inherits from B*, C inherits from A, A inherits from object

Oh, god.  What's happening?

The problem is that we're approaching this with the mindset of multiple
inheritance OOP, but in fact, in this scheme, there isn't actually any
multiple inheritance going on.  Both D(B, C) and D(C, B) in fact have
simple single inheritance paths.  What's going on is "dynamic class
composition".  That is, Python allows the programmer to compose classes
however they like into a inheritance path for any given class.  The
flexibility and versatility this offers is huge, but, you say after
picking up your jaw, how can we structure this into something we can use
without losing our sanity?  After all, if B inherits from/delegates
`super()` calls to both A and C, depending on which subclass we're
using, how to we keep all everything consistent?  We need some usage
standard.  Raymond's advice above is very well and good, but I propose
the following scheme to maximize the potential of Python's `super()`.

## Another Solution

Classes can be split into roughly three types: base classes, open
transitive classes, and closed transitive classes.

(For this article I will be using only `__init__()` as an example, but
the classes I describe here can be determined independently for each
method/attribute, e.g. A can be base class for `__init__()` but open
transitive for `do_something()`.  Usually we will define abstract
classes, e.g. BaseA has `__init__()` and `do_something()`, and we say A
is base class for BaseA)

Base classes are classes that do not call `super()`.  They serve as a
cutoff in the MRO; subsequent classes do not matter, as the methods of
subsequent classes will not get called.  Generally, these will be found
at the end of the MRO, just before object.

    Notation:
    A()  # A is a base class

Closed transitive classes resemble traditional subclasses; they may only
subclass a certain class or abstract class/interface type.

    Notation:
    A(B)  # A is closed transitive B

Open transitive classes resemble what Raymond describes in his article,
classes which take their arguments and pass everything else along to the
next class in the MRO.  These can generally be put anywhere in the MRO
before base classes and closed transitive classes, although it's
possible to have closed transitive classes that accept an open
transitive class after them.

    Notation:
    A(*)  # A is open transitive

Take, for example


    from abc import *

    class Rect:
        def __init__(self, w, h):
            self.w = w
            self.h = h

    class SpecialRect(Rect, metaclass=ABCMeta):
        @abstractmethod
        def __init__(self, s):
            pass

    class Square(Rect):
        def __init__(self, s):
            super().__init__(s, s)
    SpecialRect.register(Square)

    class Long(Rect):
        def __init__(self, s):
            super().__init__(s, 2s)
    SpecialRect.register(Long)

    class Foo(Square, Long):
        def __init__(self):
            super().__init__(15)

    class Position:
        def __init__(self, x, y, *args, **kwargs):
            super().__init__(*args, **kwargs)
            self.x = x
            self.y = y

    class PosRect(Position, Rect):
        def __init__(self, x, y, w, h):
            super().__init__(x, y, w, h)

    class PosSquare(Position, Square):
        def __init__(self, x, y, s):
            super().__init__(x, y, s)

    class PosSquareA(Square, PosRect):
        def __init__(self, x, y, s):
            super().__init__(x, y, s)  # TypeError

    class PosSquareB(Position, Square, PosRect):
        def __init__(self, x, y, s):
            super().__init__(x, y, s)

The class diagrams:

    Rect := [Rect(), object()]  # Rect is a base class, so we ignore object's __init__
    Square := [Square(Rect), Rect(), object()]  # okay
    Long := [Long(Rect), Rect(), object()]  # okay
    Foo := [Foo(SpecialRect), Square(Rect), Long(Rect), Rect(), object()]
    #                       ^ okay        ^ NOT okay
    Position := [Position(*), object()]  # okay
    PosRect := [Position(*), Rect(), object()]  # okay
    PosSquare := [Position(*), Square(Rect), Rect(), object()]  # okay
    PosSquareA := [Square(Rect), PosRect(Rect), Position(*), Rect(), object()]
    #                          ^ NOT okay
    PosSquareB := [Position(*), Square(Rect), PosRect(Rect), Rect(), object()]
    #                                       ^ still NOT okay


So we can clearly see the chain of delegation and functionality.  Using
this model, we can build a different class structure, but it requires a
slightly different mentality from traditional OOP.

Phew, I've run out of steam.  Hopefully the above example gives an idea
of what is possible with Python's "multiple inheritance"; it becomes
*very* useful when working with large, convoluted class
trees, such as in video games, but I'm too tired to come up with an
example at the moment.
