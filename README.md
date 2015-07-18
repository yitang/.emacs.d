[![Documentation Status](<https://readthedocs.org/projects/voca-builder/badge/?version=latest>)](http://voca-builder.readthedocs.org/en/latest/)
# About This Document     :README:ExportFile:



The road to Emacs is not easy: I have tried to use Emacs for many
years, and started using on daily basis from Jun 2014. The transition
is difficulty, and full of tears and bloody, and every day I feel like
being doomed in the dark. About Jan 2015, I start to the light. and
conquered Emacs, and it now becomes a symbol of me, and I use it do
most of productive work.

As the configuration grows bigger and bigger, a single *init.el* is
not suitable for organising, testing, and expanding anymore.
Previously, I have about 7 `.el` files, for example, `setup-org.el`,
`setup-email.el`, and I document on each file. Inpisraed by Sachua's
new posts, I think it would be an brilliant idea to org Emacs
configuration code into one single org file, letting along the
conviencen of orgnising and share my seetting, the precious thing I
would appreciate is it provide an way I could start with a long
comments, thoughts or workflow. in this way, the code becomes less
important as it should be.

my configration file is initally seaprated by differnt purpose or
mode, but as it gorwths, it bcaem inconicient in tracking, and, As for
other Emacs user, my configration is keep growthing, and This
documents is first combined by 5 configration files, and it keep
expanding, I use liteature programming to includes all the notes, and
keep a log of how I use themn

This Emacs configuration is free of bug and fully tested on Ubuntu and
OS X.

Normally you could tangle a org file to extract all the source code
into one file, that you could use. But I would like to push liteartpr
programming furture in two aspects: 1) the source code takes input
from this org file, I.e. table. 2) it facility Babel Library to
integrate not only Emacs Lisp, but also sh and R functions that could
be run in Emacs, and I found it particullary useful.
