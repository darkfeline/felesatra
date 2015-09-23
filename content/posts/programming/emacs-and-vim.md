---
title: Emacs and Vim
date: 2014-12-13
publishdate: 2014-12-13
subjects:
  - "programming"
contents:
  - "article"
---

If you're reading this, you're probably here because of the title, and if so,
you probably have some preconceived notions about what this post will be
talking about.  Let me start, then, by dispelling some of those thoughts: I am
a long-time Vim user who, after repeated attempts in the past to grok the power
of Emacs, has recently begun to include Emacs in his workflow.  The motivation
for this piece is primarily to share some insights I have gained about Emacs,
and about Vim, and about tools in general.

(To dispel potential ambiguity and for the sake of accuracy, I am using "Emacs"
to refer solely to GNU Emacs.  There are different Emaxen, and they are not all
equivalent!  Most of them do not even use Elisp, a vital component of GNU
Emacs.)

## The Emacs and Vim conclusion

Here's a short summary for people unconcerned with subtleties and narrative:
Vim is currently the best text editor in existence.  It has a learning curve.
If you need to edit a good amount of text, you should invest in learning and
then use Vim.

Emacs is an environment.  You can run lots of stuff in it, like a web browser,
or a Python IDE, or Tetris.  It is very flexible, but it is not for everyone.
If you use it, e.g., as an IDE, you will also end up editing a lot of text in
it, in which case I recommend installing a proper text editor, such as Evil,
which emulates Vim.

And now, the article proper:

## Vim is a text editor

I started out as a Vim user.  It is certainly possible to edit text in Notepad,
Emacs, nano, Gedit, or even a TEXTAREA in any web browser, but anyone who does
not immediately experience revulsion and start looking for a better solutions
does not, in my opinion, have any business doing any sort of work identifying
and solving problems, since they cannot even identify the problem of using an
extremely inefficient tool for editing text.

That last paragraph will have offended some persons.  I am confident that I can
prove objectively that Vim is the best text editor in general at the present,
but that would belong in a separate post, so I am content to let the claim
stand unsupported for now.

In any case, I started out as a Vim user.  Specifically, I had looked for a
text editor when I started programming.  I identified a number of candidates,
all of them clearly unsatisfactory.  I found Vim and found it at the very least
not unsatisfactory; it was clear that its creators had considered the task of
editing text and designed an editor for that task, without letting itself be
hampered by any misconceived preconceptions.  Here was a hammer with a hammer
end for hitting nails, and a claw end for pulling out nails, not a smartphone,
because hey, everyone carries a smartphone right? and you can perfectly well
hammer in a nail with a smartphone and people are familiar with smartphones, so
why invent a better, but unfamiliar tool for the task?  So, I started reading
some of the documentation, some of the myriad guides available online, and the
built-in tutor program, and I learned to use Vim.  I liked it very much, and I
still do.

However, at some point, I started extending and customizing Vim, as many (but
not all) Vim users do.  An extra package here, and extra package there.  The
story the follows from here is one many people would be familiar with: as I
extended Vim, the thought that Vim isn't the best tool for this task started to
creep up on me.  How blasphemous, to doubt the Vim that has served me so well
in the past!  Yet the fact was that I found Vim lacking, and so I searched for
a better solution rather than miring myself in denial and self-affirmation (a
lesson many people should learn).  There are a handful of alternatives, and
almost all of them are strictly inferior, and so a disillusioned Vim user like
myself will wander for a bit and almost inevitably, as though fated by the
powers that be, end up in front of the door to Emacs.

What follows is a trial by fire, as the Vim user begins to experiment with
Emacs.  It will be an extremely trying experience, struggling with key
bindings, "what the hell is going on?", "how the fuck do I do this?".  Some
make it, some do not.  I myself braved it many times, going back to the
comforting arms of Vim afterward each time, which I imagine many Vim users do.
Eventually everything clicked for me, and I decided to move some of the things
I would do with Vim onto my Emacs workflow, gradually.  These are two separate
events, and while it is perfectly fine for Vim users to decide not to make the
transition, I think it is a shame that the reason for the choice is that they
never understood Emacs, rather than consciously deciding that the Emacs
workflow is not for them, and partly why I decided to write this article.

I think that there are two key conceptual points that are crucial to grasp to
understand the power of Emacs and how best to use it in one's work flow.  Emacs
is an environment, and Emacs lives and dies by its extensibility and
scriptability.

## Emacs is an environment

Most people who know of Emacs identify it as a text editor.  It is often
compared to vi or Vim, as in the unholy editor wars.  Even Emacs itself refers to
it as a text editor.  Yet calling it a text editor is shooting both your and
Emacs's foot, because Emacs is an environment, both extremely useful and
extremely powerful.  This is more or less an open secret for anyone in the know
("Emacs is an operating system", "Emacs is a way of life" are commonly said),
but it's hard to really grasp the concept, especially when so many sources all
call it a text editor.

To help explain this fact, I will compare Emacs with two other environments:
the traditional UNIX shell environment (or any UNIX-like environment, such as
GNU/Linux or busybox/Linux) and the KDE graphical environment.

Generally the first thing that one notices are the programs that come installed
by default in the environment.  For UNIX, these are the nostalgic and esoteric
names such as `cd`, `ls`, `cp`, `dd`, `sed`, `awk`, and `ed`.  UNIX is
rather frugal in this regard, and generally only provides the minimum required
programs to work on a computer as per the UNIX philosophy, but are often
extended with extra programs such as `w3m` for web browsing, `vim` for editing
text, `irssi` for IRC, and Imagemagick for image manipulation.  Unfortunately
the lack of any graphics outside of curses makes some kinds of programs
impractical, at least without running a different environment on top.

KDE comes with a more familiar set of programs: Konqueror for web browsing,
Amarok for playing music, Krita for painting and image editing, Kate for
editing text, and so on.  (These may not be installed by default on Linux
distributions, but they are all native to KDE.)

Emacs also comes with a set of programs (called modes): Gnus for news and mail
reading, Dired for browsing and managing files, EWW for web browsing.  There
are also games like Tetris (KDE also has games, as does Windows; UNIX does not,
though one can install things like nethack and fortune).  Emacs also comes with
"IDE"s for many programming languages, integrating both with the language and
external tools.  Emacs's library is a little meager compared to KDE, but like
KDE and UNIX, one is free to install third-party packages, so there's no real
shortage.

The tools are one defining aspect of an environment; the other is how those
tools are designed and how they interact.  After all, an environment provides
an environment (surpise!) in which one works, using those tools the environment
provides.  Thus, how everything fits together is important.

All of UNIX's programs are carefully designed and standardized.  They are
designed around one another and the underlying system, such that composing them
is easy and recommended.  The pipeline is a signature UNIX design (`cat foo bar
| sort | uniq | grep 'baz' | awk '{print($1 $3)}' | sed 's/apple/banana'`).
Everything runs on text streams and files.  There are sockets, which are really
just special files.  Documentation is centralized using `man`, and all of the
programs have a consistent design, such as passing options like `-a` (there are
some exceptions, which UNIX haters are more than happy to point out).  UNIX is
extremely powerful, but with a high learning curve.  The design also
inconveniences graphical programs: it's much harder to pipeline from `w3m` than
from `wget`, for example.

In contrast, KDE is graphical by design and takes advantage of it.  Everything
which is done in the shell in UNIX is done using a graphical program: text
editing and processing, search, file management, and configuration.  KDE
programs have a uniform interface through Qt and themes.  Help is provided
through a uniform interface (opening a web page through the default web
browsers).  A way to set default applications is provided, and KDE programs use it
to seamlessly open files and links.  Communication between programs
is limited to drag-and-drop and the clipboard, somewhat lacking in the face of
UNIX.  However, KDE does provide services like Nepomuk that centralize
information and resources for all KDE programs to share.

Emacs, like UNIX, is purely textual.  However, instead of text streams, Emacs
uses buffers.  Instead of small utilities tied together with scripting, Emacs
uses Elisp tied together with more Elisp.  Emacs modes share a unified
interface and standardized key bindings.  Documentation is provided through the
unified `C-h`.  Communication is free-for-all: you can as easily open a link to
a website in EWW as send it to a friend using Gnus, or download the HTML source
and edit it in HTML mode.  That link may have originated in EWW, or Gnus, or
Org mode.  Consider it a bonus from building everything on a standard scripting
language.

So we see that Emacs is an environment and that it's different from other
environments.  Now what?  For one, we can see that Emacs has some pretty neat
apps, and if there's a killer app in there for you (Org mode, Gnus, or a
dedicated LaTeX IDE), you will need to install the Emacs environment to use
it.  Like installing all of KDE's libraries and dependencies just to run Amarok
or Krita, it's a little wasteful, but perfectly reasonable.  If you don't see a
need to move from your custom Ruby development work flow using Vim and tmux,
don't let that stop you from running Org mode on Emacs, but there's no need to
be pressured into using Emacs for Ruby work, either.  Use the best tool for the
job, as they say.

This also allows us to re-frame the question any prospective Emacs user has,
from "How does Emacs the text editor help me?" to "How does Emacs the
environment help me?"

# The power of Emacs

As we saw before, Emacs the environment has a few differences from UNIX-like
command line environments and the KDE-like graphical environments.  It also
means that Emacs has certain advantages and disadvantages, like everything else
in life.  I'll start by listing the main disadvantage: Emacs has a relatively
small community, smaller than most other computing environments.  I think the
TI-88 Linux community or Game Boy Advance Linux community may be smaller, but
otherwise, Emacs has the smallest community of any environment currently in
use.  This does mean less documentation, fewer guides, less mindshare, fewer
third-party packages, and less support.  Emacs also has a higher learning curve
than even UNIX, mainly because there's so much stuff that comes with it by
default.  Together with the smaller community, it means that an Emacs user is
going to have to invest a lot of time into Emacs.  Of course, one invests a lot
of time in any environment, but the problem will feel particularly acute with
Emacs, especially due to the character of Emacs's main advantage.

So, what advantage does one gain sinking all this time into Emacs?
Essentially, extensibility and scriptability using a Lisp dialect, and as a
side effect, extremely flexible interoperability with everything running in the
Emacs environment.  As an example, say you really want to be able to select a
song in your music player, upload it to YouTube, then open an email to your
parents or friends with the link to the uploaded video pasted in the message.
You can do this in UNIX, though your music player will have to comply to the
UNIX design philosophy (`musicplayer play`, `musicplayer status`, `musicplayer
next`, `musicplayer pause`, `musicplayer vol up` anyone?).  It's not terribly
pleasant, but doable.  In a graphical environment, you should give up.  I'm
sure a sufficiently motivated hacker could whip up a kludge of some sort, but
for practical purposes such a task is outside the realm of possibility.

In Emacs, this is a piece of cake.  You will have to implement it yourself, as
it is highly unlikely someone has already done it for you, but if you can
program, it is perfectly doable and will quite possibly be a pleasant
experience.  This is the power of Emacs and the reason why one would want to
use the Emacs environment.

## The conclusion

That was rather long-winded, and I've quite forgotten the intended tack of this
discussion.  So what's the point?

If you're looking for a text editor, you'll want to use Vim.  If you're looking
for an IDE, Vim is still more than sufficient, but now we're looking for an
environment, so Emacs should definitely be considered.  If you're looking for
an environment, you should take a look at Emacs.

Emacs is a unique environment.  It offers unparalleled integration and
extensibility, at the cost of less popularity, being restricted to Emacs, and
having to do many things yourself.  If you can accept those costs, Emacs can
improve your workflow.

Looking back, I didn't do a really good job explaining everything and I'm out
of steam now, but I hope you enjoyed reading this and learned something.

(Note to self: you really do need to outline long writings.)
