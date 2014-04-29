---
title: My Road to Haskell
description: How I went from layman to expert
tags: haskell, life
---

# My road to Haskell
Recently on Reddit [there was](http://www.reddit.com/r/haskell/comments/23pipv/job_offer_at_createnet_italy/) a
discussion about a job offer posted but
that received few attentions, which triggered my interested, being myself
very involved in that context. I have also asked if people would have been
interested in my "Road to Haskell" (trademark by [Chris Forno](https://www.youtube.com/watch?v=mpby_hz8lFc)),
and due to the fact, surprisingly, they were, here I am.
I wanted to write such a blog post for a while, but never found the
inspiration I was looking for. So (long breath), what follows is my story,
who I am and how I went from being an Haskell novice to a professional.

## Fall 2007
Every great story begins with "Once upon a time" or "Fall YYYY", so for me
we need to rewind time back in 2007. At the time, I was deep into my
second year of university, and for the first time I took a look to my
curricula's full program and with my surprise I saw a mandatory course called
"Functional Programming". Before, I never heard about functional programming,
being our university quite fundamentalist in Java and OOP. Between general
skepticism and mocking from my colleagues, we attended the course (as said,
it was mandatory). We were roughly 150 students, but I think probably just
10 or so were really interested. The course used OCaml and unfortunately lacked
a bit of real world pragmatism. It was the type of course which really makes you
appreciate the succinctness of algebraic data types and recursive functions on
data, but alas my colleagues (me include, I'll be honest) failed to see any
real world application with that. In other terms, it was a nice way to program,
but in a "toy environment", or in academia, which is, I guess, one of the false
myths we are fighting hard to bust.
Anyway, my first foray into functional programming concluded with a full score
in that exam, but I rapidly forgot about OCaml and went back into OOP and Java land.

## Python-it.org
Fast-forward to Autumn 2009. I graduated with full marks, getting my Bachelor in
Computer Science with a thesis regarding a geo-tagging web application, with some
flavours of artificial intelligence. It was programmed using my favourite tool at
the time, Python. I fell in love with Python during the summer, when my professor
suggested it as a nice system language to implement my thesis with (the thesis was
on a different topic that I later changed into the aforementioned webapp). I embraced
Python completely, being truly refreshing after the clunkiness of Java, so I started
blogging about it, and I signed up on [Python-it.org](http://www.python-it.org/),
the de-facto forum for Italian pythonists. There I met a lot of nice people,
and in general started to expand
my horizons, talking and interacting with people outside my university, which developed
the "Java tunnel vision" I told you about.
I don't know exactly how happened, but in one topic in that forum they started to talk
about functional programming languages, and they named a few: Clojure, Scheme and
Haskell. There was a guy I admired a lot which had a crush with Scheme, and being
Clojure very similar I started to explore the two languages almost at the same time.

## The holy Grail search begins
I bet we all had this phase of our CS life where we relentelessy search for the "Holy
Grail" of programming languages, the Satori language if you prefer. The perfect
language which will be able to restore
[our faith into programming](https://www.quora.com/Reviews-of-Haskell/review/Edward-Kmett).
A language which is fast, expressive, with a great library ecosystem, with a strong community.
[Drunk with the sense of power](https://www.youtube.com/watch?v=bzkRVzciAZg)
this search gave me, I really liked to write small
programs (typically a Project Euler challenge), and implement/benchmark it using
different programming languages. My [old blog](alfredodinapoli.wordpress.com) is
full of articles about FP and Clojure (mostly), but
[this relic](http://alfredodinapoli.wordpress.com/2010/11/01/a-minimalistic-implementation-of-the-hangman-game-in-clojure/)
is the first piece of Haskell I ever wrote.
I wrote it, as usual, to compare implementation and quirks of different
functional programming languages. It's quite funny to get back to this code
in retrospective, there were also a lot of grammatical mistakes, and the sentence
"the pattern match magic", makes me smile! Nevertheless, this was my first
exposure to the language, and despite missing completely the notion of "monad", I
was able to whip up that program just playing "type tetris" and searching for some
basic tutorial on internet. Despite the final goal of the blog post
"I’m looking forward to digging deeper in Haskell", I rapidly went back to Clojure.
What didn't work for me at the time was not the learning curve, but the fact that
I couldn't understand Haskell in its foundations. Reading "Real World Haskell" one
of the first topics I was introduced was the "Maybe datatype", but I wasn't able to
understand _why_ I needed the complication of wrapping my values into something I
needed to unwrap later on. "Why not use something like Python's `None` of Clojure's
`nil`?" - I thought at time. So "Real World Haskell" rapidly finished accumulating
dust into my bookshelf, so did Haskell in my "to-learn" list.

## Searching for the Holy Grail of the Holy Grail
My search didn't stop to Clojure, though. The thing was that I couldn't put up
with the fact it was running on the JVM, inheriting all its problems. Furthermore,
I really wanted something like Common Lisp and its SBCL compiler, capable of
producing efficient native code. So, once again, I started searching and I can't
remember why, but in the back on my mind I still had this small thing about Haskell:
elegant, capable of compiling to efficient native code, fast if optimized. So I
went to my [local bookshop](http://amazon.co.uk/) and I ordered a copy
of "Learn you a Haskell for Great Good!"

## Reinassance and rebirth
That book is, hands down, the reason why I am an Haskell programmer today and why I
didn't grow up with the false belief that Haskell sucked and was
[sour](http://en.wikipedia.org/wiki/The_Fox_and_the_Grapes). LYAH really
showed me that Haskell was the most elegant programming language I ever saw, built out
from mathematical foundations. And suddently I realised why I needed the Maybe type and
to wrap my value with it! I was so enthusiastic that I started blogging about it
(someone perhaps will remember my "Let's build and Elemental Battle System in Haskell"
series). I was by any means good at it, but I do loved it, and this was the important thing.

## My master degree and my thesis
When the time came for me to undertake the ordeal of writing a master degree thesis, I
wanted to have the professor which taught us "Distributed Programming", which is not
idiosyncratic against functional programming but certainly appreciate more languages like
C++. He despise Java, which creates immediately a strong empathy! I based my thesis project
on a simple truth: things like `map :: (a -> b) -> [a] -> [b]` are parallel by nature, so
why we don't try to implement a nice subset of functional programming functions where the
computations can be run on the GPU? I used OpenCL to write a C++ wrapper around them, with
sugar on top. My final plan was to bind this C++ framework to a FP language. I do wanted
to choose Haskell, but I wasn't proficient enough at the time, and I didn't want to get
stuck too much trying to make it work. My professor was a bit more accustomed in Erlang,
and it offered a sort of "process-based" way to integrate with basically everything, so
we choose Erlang. I never programmed in Erlang after my thesis (so no love at first sight!),
but I did recognize that C++ was a nice and powerful imperative language, much more
widespread than Haskell in Italy, so after graduation my mission was: let's get proficient
with C++ and let's find a decent job in Italy. Haskell was still there, occupying a small
part of my hearth, but I was simply trying to be realistic, maximizing my chances to
get a job.

## My first job (well, kinda of)
On June, 2012, I started my first job. It was an intership for a company which operates
in the defense field. I was really excited, being this place a C++ shop. I said to myself
"It's done! I'm in Rome (my city), doing awesome C++, and if I work my ass hard after
the intership I might be offered a full time job, and then I'll be on the top of
the world!". But life had different projects for me. To start with, the intership was
absolutely menial; my job was to investigate whether JavaRT was a suitable candidate
to supersed C++ in a system critical environment (short answer: NO).
So I basically worked my ass hard and finished the intership in 1 month, despite the
6 they allocated with it. This means that while they tried to feed me with different
tasks (which was good), I found myself having a bit of spare time, especially in the
morning. It was clear that the job I dreamt about wasn't exactly the reality I was
living in. So in my "idle time", I used to study Haskell,
[Okasaki's book](http://www.cs.cmu.edu/~rwh/theses/okasaki.pdf) and
[algorithms and data structures](http://sist.sysu.edu.cn/~isslxm/DSA/textbook/Skiena.-.TheAlgorithmDesignManual.pdf),
to keep my mind sharp. It was during my intership
I [wrote that small Shelly tool](http://www.reddit.com/r/haskell/comments/w86gu/my_current_job_task_is_boring_so_i_wrote_a_simple/)
for glueing different tools together. During my
internship I also explored streaming IO and I
[asked help to the Haskell community](http://permalink.gmane.org/gmane.comp.lang.haskell.cafe/100402)
to land a job in functional programming. I always said to myself, for everything in life
"For what I lack in [Skill X], I will make it up with perseverance", which might be
read as "Even if you are not Edward Kmett, you should try to get an Haskell job anyway".
So I did.

## My path to glory
First thing was to realise what I wanted, so I started by [blogging](http://www.alfredodinapoli.com/posts/2012-09-13-getting-serious-about-haskell.html)
that "I wanted to earn a living with FP".
Ok, so that was out of the way, but how to achieve it? When I
asked help the the Haskell community, Doug Beardsley, one of the lead developer of Snap,
suggest me to contribute to a large Haskell project to expand my knowledge of the
language, and he kindly offered me help to get my foot wet with Snap, in order to be
able to contribute to it. I remember with a bittersweet longing those days: I worked
9 to 5, I came back at home, I had a snappy chat with my mom consuming my evening snack
and then I was on IRC chatting with Doug about Snaplets, Handlers and other Snap internal
concepts. Thanks to him I was able to implement the `Snap.Snaplet.Test` module which came
out in Snap 0.10. It was announced in [this changelog](http://snapframework.com/blog/2012/12/10/snap-0.10-released)
and I was damn proud to know that people were going to use something I wrote. It was an
awesome sensation (after that, I contributed to dozens of other Haskell libs). But when
I read those changelog I was not in my small bedroom in Rome, but in my personal flat
in Manchester, because in the meantime I landed a job as Scala Programmer.

## The ordeal to my first FP Job
Let's get back on track on the main plot-line of this story: it's end of June/July, and I
knew for sure that my current job was not what I would have expected, and I wanted
to earn a living
with FP. As said, I started to study FP seriously and looking for jobs outside Italy, as
I knew was impossible to find something FP-related in Rome or even in Italy. So I said
"why aim low?" and I applied for [Jane Street](https://www.janestreet.com/),
which you certainly heard, unless you
have been living under a rock. If was end of August, my parents were away for holidays and
I had plugged my Mac to my dad's LCD screen in the living room. I was browsing email with
Mutt when I decided to apply for JS (unfortunate acronym, I know). So I prepared a
cover letter and I sent it. With my surprise, less then 24 hours later they went
back to me: I had a call scheduled with New York city three days later!
You could image this was kind of a life changer for me; I was going to have a full blown
job interview, for a dream job, completely in a foreign language, and I was scared as fuck!
Details are not important here, but I remember myself refreshing OCaml (we meet again!)
at 3 o clock in the morning, with sweat pearls dropping from my head, in a really hot
Rome's night. Short story short, with my personal satisfaction I was able to pass 2 phone
interviews. I failed the third, and the 11th September (I remember it as it's an unfortunate
date of a tragic event) I received a polite rejection letter. I was sad but at the same time
euphoric; I was able, with nothing but my own strength and will, to pass two notoriously-tough
job interviews! I still remember the facial expressions of my friends when I told them about
my interview questions - "What?" - they said - "binary heaps? Context free grammars?". The
surprise stemmed from the fact rarely in Italy, for entry job positions, you are asked to code.
Bad attitude, I know. Anyway,
I was happy, I was doing a menial job but inside me I wanted to yell people "I know what a
monad is and I passed two phone interviews with Jane Street! And you keep me here writing
Word documents!". I was fully determined, and I applied to different shops until
[Cake Solutions](www.cakesolutions.net) gave me a chance, even though I had just minor Scala
experience, and, in general, no job experience at all.
I was scared by the perspective of going in a foreign country, alone, to work
with a language I wasn't fully proficient with! Lots of uncertainties, but I leaped into
this new adventure nevertheless! That was, in retrospective, the hardest choice I had to
undertake in my life, also complicated by the fact that, when I resigned from the internship,
they offered me a full time contract. Basically, my resignations acted as a catalyst for
what they had prepared for me. Well, I **turned it down**. Not for smugness, but just because
I was certain that if I wasn't going to leap into the dark at that time (namely moving
to the UK), probably now nothing of this would have never happened. So I went straight and
on the 11th of November, 2012, I flew with a single, no return ticket to Manchester.

## The Manchester era
Manchester is a particular city; Can't express myself, but has a nice contract between the
old and the new. It's not uncommon to find very recent building next to second industrial
revolution mills. In fact, Cake Solutions' office is hosted within this mill:

<div align="center">
<img width=400 src="../img/2014/cake_mill.jpg"/>
</div>

Once arrived I was obviously overwhelmed by the new job, and I asked for suggestions pretty
much everywhere and pretty much to everyone
(even to Edward Kmett, which I cold-emailed, and he was kind enough to reply.
I wrote again to Edward a year later once I got my Haskell job).
Scala was a good start but
obviously was not the language I wanted to program with, because **I wanted to program in
Haskell**. So again, I spent my gloomy Mancunian evenings hacking some Haskell and
developing further my knowledge. It was mainly on personal projects, which sometimes
elicited bugs or limitations of existing libraries, which I promptly fixed with PR. This was
another important step, as it became natural with time to simply go in, modify a library
to fix a bug and yield a PR. Anyway, I was _happier_ in Manchester than I was in Rome
(jobwise, I missed the rest), but I knew my journey was not complete. Once in Manchester,
I said to myself "wait, now I am in UK, so I can attend one of the Skills Matter courses
on Haskell!", and being Cake Solutions a SM's partner, I asked if they had access to some
sort of discount for these courses, which are notoriously very expensive. I wasn't even
sure if that was worth asking, as I didn't even have all the money to afford it (my savings
were exactly the ones of a guy just started working), but with my immense surprise
**Cake offered to pay for it in full!**. Not just the course, but the train and a one night
stay in London! If there is a public space to thank Cake Solutions for this, I think I'll
use this one: thanks!
Needless to say, the course was amazing (tiny detail I missed, it's hosted at the SM's HQ
but Well Typed is responsible for the teaching and the material provided). During those
two fantastic days I met another bunch of eager haskellers willing to expand their
knowledge, as well as two Well Typed gurus like Edsko de Vries and Andres Loh.
Following Andres' advice I tried to stay in touch after the course, asking Haskell
question that Andres, always patiently, answered. Back in Manchester though, everything
was the same: I had to put in the cloakroom my Haskell hat, to put back the JVM one.
To complicate things, Cake was also moving to a slightly different business plan, where
Java and bigger corporate projects were going to be preferred to Scala. How ironic! The
fate I tried so hard to escape from came back to haunt me miles away from home!
It was early July. It was time for me to change.

## Summer 2013
I was lurking in the shadows during my even-gloomier Mancunian evenings for a job post
or a pseudo-haskelly job. For a nice coincidence
[Well Typed was hiring](http://www.well-typed.com/blog/80/), and I said "I know to not
have enough skills to be considered an expert, but I want to accept this challenge
and apply anyway. What do I have to lose?". So I did. I also did a couple of interviews
for full time Haskell shops which ultimately yield a failure, but I was happy
anyway, because for the first time in my life I was able to **start and end a full
job interview using nothing but Haskell**. This was another important milestone,
realizing that Haskell started as an hobby language and now it was a language that,
potentially, was as suitable as Python for a job interview.
Then I was off for my summer holidays to Italy, with a pending application to WT
(which I obviously gave for granted as a failure) and a couple of failed more, but
I was confident I was finally proficient enough with the language.

## Vieste

<div align="center">
<img width=400 src="../img/2014/vieste.jpg"/>
</div>

I remember it pretty vividly. I was sitting with my girlfriend in a restaurant in
Vieste, Puglia, Italy, having a quite eventful dinner. While waiting for our
main courses, I lazily checked my email and with my surprise I found a reply to
my WT job application of more than 1 month ago from Duncan and Andres! Obviously,
being WT so focused in hiring experts I didn't even pass the pre-screening.
Another company could have just said "Ok, you didn't pass, best luck for your
search". But Well Typed was different! Duncan said that one of WT's client was
looking to hire for a web developer with Haskell and Ruby knowledge. Despite the
word "Ruby" made me cringe, I thought to myself "Haskell and web development, I
can do this!". And obviously I said this strong of my contribution to Snap! So I
asked Duncan politely to put me in touch with this client. To be honest I wasn't
betting on it too much: previous experiences taught me that usually these things
die by natural causes for different reasons: the company stop hiring, they find
another candidate before reaching you, the person meant to introduce you simply
forgot, etc. Luckily for me, this was not the case. On the 29th of August, when
I was back from a long day of consulting in Wales, I received a mail from
Duncan with the aforementioned job offer, from the client which would have become
my employer, [Iris Connect](http://www.irisconnect.co.uk/).

## (Almost) present days
I took an eventful journey to meet Chris Dornan and the rest of Iris' team, and
spent a Saturday morning with him going through my personal project, a Snap
RESTful server really close in terms of business goal to the one Iris is (still)
developing. At the end of the day, I had a job offer!

## Takehome lessons
So, looking back in retrospective, I was able to go from Haskell novice to
professionally employed in less than 2 years, despite I have been tinkering
with Haskell from 2010. Was I lucky? smart? stubborn? determined? Probably a bit
of everything. This is what I learned and what I think might be an interesting life lesson
(sorry for the following, it's full of conditionals, a nightmare for a non-native writer!):

* **Don't be afraid to take leaps into the dark**: I turned down a job offer in
  the safe harbor of my home city for something totally new and scary. If I didn't do
  that, today I probably wouldn't be an Haskell programmer.

* **Life is about opportunities, seize them**: Think about what would have happened if
  I was too shy to ask Cake Solutions about Skills Matter's courses. They would have
  never payed for the course, I would have never met Andres and probably never applied
  to WT. Duncan would have probably not even considered referring me to Iris.

* **Try to contribute to a "famous" Haskell OSS**: I was able to land this job also
  because I had experience with web dev in Haskell. But I had experience mostly because
  I contributed to Snap. There is a substantial difference to say "I have used Snap", as
  opposed as "I used Snap and I have implemented feature X".

* **Constantly sharpen your saw**: If I felt "realised", today I would still be working
  in Manchester. The burning desire I had to work as a professional Haskell dev caused
  me to spend my spare time programming and studying.

* **Be receptive, do networking**: Having a strong network is vital. Try to actively
  contribute to the community, let other Haskeller know you. Let them think "I have
  already heard about John Doe". Even if just an handfull will do, you won't be a
  total stranger but someone into the community. I think this is the best thing
  which can happen to an Haskeller.

I hope this wasn't too long. It probably is, and also full of mistakes. Bear with me,
I found myself to be much more comfortable with code! And remember, nothing is free
in this life, and come with a price, unless is unconditional love from your parent and
certain type of monads:

```
data F f a = R a | F (f (F f a))
```

Alfredo
