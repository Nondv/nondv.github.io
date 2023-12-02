---
title: Microservices aren't the problem. Incompetent people are
categories: ['Software Engineering', 'Productivity']
tags: [microservices, programming, architecture, incompetence]
img: /img/posts/monkeys.jpg
img_caption: ''
description: >-
  There's been a lot of (deserved) criticism towards microservice architecture
  and how it's usually hurtful to the companies. However, I believe SOA isn't
  bad for the companies. Incompetence is.
---

Have you read the awesome essay ["Death by a thousand microservices"](https://renegadeotter.com/2023/09/10/death-by-a-thousand-microservices.html)?
I agree with everything said in it. However, I'd like to emphasise that
microservices aren't a problem in itself. Also, I don't think monoliths are
necessarily a better fit most of the time even for smaller products. In fact,
*personally*, I prefer services (intentionally avoiding the "micro") to a huge
monolith in the work setting.

The real problem, I think, is the lack of engineering competence and the lack of
"giving a shit" in companies. That's what creates complex systems nobody can
efficiently work on.

<!--more-->

# Incompetence

The software engineering world is incredibly hype-driven. Microservices are the
current hot thing (although I think people are now ready for the next trend).

I, actually, love the fact that we're so quick to adopt new tech. It keeps the
profession interesting. If I didn't like exploring stuff, I wouldn't have had so
much experience with completely different technology and would've been a much
worse programmer (probably working with the same shitty russian PHP framework I
started my professional path with).

However, adopting a new (groundbreaking) technology means that we turn from
seniors to juniors. Yes, we apply the knowledge and techniques we've learned so
far but sometimes it's not very transferable. Remember when Scala had gained
some popularity and people noticed that the code in it is either overly "clever"
math gibberish or simply Java with a different syntax?

Companies often assume that if they hire "smart people" they'll simply figure
stuff out. That's not how it works (also, see the "smart-ass complex" below).
Being smart doesn't guarantee that you can solve any problem. Experience is
probably the most valuable asset (and the experience itself needs to be of the
high quality which is a rare gem too) and we give it up when adopting
new technology. We *willingly* make ourselves incompetent.

Of course, some people are quite knowledgeable in that stuff. But do they manage
to educate and drive other people into leveraging the tech properly? Nope.


### Example: Apache Kafka and event-driven systems

Nowadays there's [a surge](https://trends.google.com/trends/explore?date=all&q=apache%20kafka&hl=en-US) in Google searches for "apache kafka". Lots of
companies, especially startups are using it (just search for it on LinkedIn or
some job boards). Many people I've met get outraged (and have that condescending
pity look on their faces) when I call it an event bus. "Nooo, it's not an event
bus, it's an event streaming and storage platform", "Sooo it's an event bus that
doesn't get rid of the messages?", "Nooo, you don't understand, let me explain
again". The truth is, many companies don't even use it for event streaming but
instead use it as a message queue that they can replay if someone fucks up. In
my current company it's even used with Avro schemas literally as an alternative
to GRPC with protobuff. What's the point? Might as well use HTTP.

Now, people who introduce it often (but not always) have a pretty compelling
pitch and do sound like they understand why it's useful and how to use it
efficiently. The problem is, even if that's true, nobody they hire or manage
do. Kafka wasn't really a thing 10 years ago so where would I get the experience
with it?

If someone's used hammers and nails all their life and you give them screws and
a screwdriver, they'll simply say "oh that's a pretty crappy hammer but the job
is a job" and proceed to smashing the screw into the wood with the handle. If
you want them to use it properly, invest in their education or at least handhold
them until proficient. Do people do that? Not really unless it's a very small
engineering team where everyone works closely together (and even then it's not
necessarily a thing). And don't forget teams where the tech leads have no idea
what they're doing either.

I actually love kafka-based architectures. To me it's like a better AWS SNS
(if I'm not the one setting it up and maintaining it). However, if I see it in a
job description, the chances are, the company is using it wrong so it'd be a
nuisance in your day-to-day job rather than a tool that helps you.


# Not giving a shit

Following my example with a hammer and a screwdriver, you may ask, why not find
some time *yourself* to learn more? After all, you can spend an hour or two a
day reading something and still get paid (and it's still useful to the company).

Well, because most people don't give enough shit to improve. Yes, programmers
are supposed to learn all the time blah-blah and all that crap people say to the
newbies on Reddit. The truth is, most don't.

There's also another category of people. Good (sometimes brilliant, even)
engineers who know exactly what they're doing but who simply don't care enough
to do the right thing, to guide others into doing right thing. As far as they're
concerned, they have a task, they solve it, they get paid. No more extras. The
company may burn in hell for all they care. I've met a few of those. They're
incredibly insightful when having a casual conversation but very quiet and bored
in meetings.

Having a "no fucks given" culture is a disease. It poisons everything around it.
At some point everyone stops trying to improve things. They simply go with the
flow. Just like shit in the sewers (as my teacher used to say). That's one of
the reasons why I always recommend junior people to change jobs at least once a
year. It's better to learn bits and pieces from different places rather than
getting stuck at one.

This also translates into code reviews. And decision making. And hiring.
Incompetent people hire other incompetent people who then get involved in
hiring. It's exponential. I sometimes feel like we need to enforce a lockdown
and develop a vaccine for this.


# The "smart-ass complex"

Let's face it, engineers, most of us aren't smart. We've just been led to
believe we are. Knowing how to code or open a terminal and use `git grep`
doesn't make you smart nor educated. At best, it makes you tech-savvy adjacent.

In quite a few countries people in STEM are even considered more smarter than
people studying social studies (get it? more smarter haha I sometimes crack
myself up). The truth is, people are idiots no matter what their interests,
hobbies, and work are. Being an engineer (even an experienced one) doesn't make
you nor your decisions smart.

I'm bringing this up because I think that having the, let's call it "smart-ass
complex", oftentimes will make us to:

-   **Overcomplicate things**. Let's build a slack bot with 10 different Haskell
    container Lambdas on AWS talking to each other through SNS continuously
    deployed through Circle CI with the usage data sinking to Athena. Now we don't
    have to pick the standup facilitator ourselves, it'll be done automatically
    every morning at 8am!
-   **Attach different names to the same thing**. Adapter? Translator? Facade?
    Delegator? Decorator? Bridge? Ah, you mean the thing we can call simply a
    *wrapper*? Yes, you could argue the important part here is the context but how
    about you simply explain the problem you're trying to solve first instead of
    sounding like a douchebag nerd with your fancy-ass terminology? Also think
    about this next time you call your kafka-based system "event-driven" (is it
    really?).
-   **Seeing problems that don't exist**. Yeah, we need to scale a lot, let's follow
    Twitter's lead and use Java and Scala because we'll definitely have the same
    scaling issues in the future in our niche-ass application only a few thousands
    professionals use. And when that future comes, we'll be like "psych! we're
    ready for you". Actually, Java is too slow and old, let's use Elixir. We've
    got a bunch of Ruby devs, they'll definitely know how to build systems in it,
    it's pretty much Ruby anyway just more faster and more functional-programmier.
-   **And so on and on**. I'm getting off topic here

Not being smart is totally fine (I certainly am not one). We just need to avoid
being stupid. And we need to be humble and not let our egos blind us. If we
recognise we aren't smart, we can start going back to basics by keeping it
simple. If we aren't smart enough to predict future problems, we shouldn't even
try. Instead, let's solve the problems at hand as they come and do our best at
keeping things clean and flexible (see [two-way door decisions](https://www.inc.com/jeff-haden/amazon-founder-jeff-bezos-this-is-how-successful-people-make-such-smart-decisions.html)). *That* would be
a smart decision.

P.S. Coincidentally, when writing this section I saw a [question on Reddit](https://www.reddit.com/r/ExperiencedDevs/s/Qp8InA9tCo) about
toxicity in the software engineering culture (unfortunately, the context was
deleted by the mods). One of the points mentioned was the culture of trying to
be the smartest guy in the room. That happens a lot (I have sinned so myself).


# Competence

So how do we navigate this hype-driven incompetent world of software
engineering? I wish I knew the answer. I'll try to provide some thoughts tho.


### SOLID systems

We've all heard of the [SOLID](https://en.wikipedia.org/wiki/SOLID) principles. However, I notice that people almost
always associate it with OOP specifically and completely dismiss it when talked
about in the context of other languages or system design. In fact, you wouldn't
believe how many times I was *corrected* (i.e. put in my place lol) at meetings
whenever I mentioned SOLID outside object-oriented context. The reality is, if
you think really hard about it, every principle applies as much to the whole
systems as to a particular codebase.

As we often say, programming is all about splitting a problem into a smaller
problems and solving them one by one. However, it's not only about problems. The
same goes for systems and organisations (see [Conway's law](https://en.wikipedia.org/wiki/Conway%27s_law)). System at **every**
level (including organisational) needs to be sanely split into smaller *units*.
And at every level a well split system units will have the characteristics
defined by SOLID principles.

-   **Single responsibility principle**. A unit should have a clear responsibility
    and keep on top of it at all times.
    -   At the code level it means your library/module/class/function/etc should
        only have one purpose.
    -   At the system level it means the same thing for microservices. However, in
        practice it doesn't matter if a service have multiple responsibilities as
        long as they're provided as separate independent APIs. Basically, a service
        is simply an infrastructural unit/junction for APIs which are above in
        abstraction level.
    -   At organisational level that would mean the team should have a clear role.
        In tech teams that would mean looking after specific projects, ideally, in a
        specific domain (this one's hard to achieve).
-   **Open-closed principle**. The unit must be open to extension but closed to
    modification. Personally, I also include "composition over modification" here.
    -   Your data structures should be backward-compatible (at both code and system
        level).
    -   Your services/APIs should allow to build on top of but closed to any
        modifications. This is related to the single responsibility principle.
        Example: a payment service should only provide money movement features.
        And particular product-related business logic should exist as a separate
        loosely-coupled API utilising the original payment service.
    -   At the organisational level the role of the team should not change over
        time. If at some point the role is deprecated, the team needs to be
        disbanded and a new one created with a new clear purpose. At tech teams it's
        also closely related to code ownership.
-   **Liskov substitution principle**. All subclasses should be usable instead of
    their parents. This one sounds very specific to OOP. But no.
    -   At the code level it means that when you have two units implementing the
        same interface, you need to make sure both are usable in any situations the
        interface is required. Simply providing the same structure/methods/etc is
        not always enough, keep that in mind when designing, implementing, and using
        interfaces.
    -   At the system level it's exactly the same. I mean API literally stands for
        "application programming *interface*"
    -   At the org level teams should not be relying on other teams but rather the
        work they do. Sounds pretty obvious but the communication processes are very
        inefficient at times. How do you normally receive work from other teams? Do
        they send a JIRA ticket? Do they talk to your manager or tech lead first?
        How does it get prioritised? How are deadlines/estimates/updates
        communicated?
-   **Interface segregation principle**. Units should not depend on other units they
    don't use.
    -   At the code level it basically means that A using B should not have to use C
        directly just because B requires it. Simple example: I should not have to
        access database credentials and establish connection to it simply because I
        want to fetch a specific user. This should be done somewhere else (where and
        how exactly will depend on the exact codebase).
    -   At the system level, it's pretty much the same.
    -   At the org level it's again all about communication. I shouldn't have to
        deal with irrelevant people and jump hoops to get something from another
        team.
-   **Dependency inversion principle**. Units shouldn't depend on each other,
    instead they should depend on abstractions (interfaces, contracts)
    -   At the code level you basically don't want to use other units directly. It's
        not always practical tho e.g. in Ruby it's usually more convenient and
        doesn't really have any downsides. For instance, in a language like Java
        using other units directly may lead to awkward/bad tests. I guess I would
        reword the principle as "interface first, implementation second"; it's hard
        to establish specific coding rules on this one.
    -   At the system level, again, the communication between services should rely
        on contracts because internally services tend to change. Sometimes services
        even get replaced.
    -   At the org level it's exactly the same. Contracts over specific people.

Wow this section turned out to be much larger than I intended and still doesn't
feel thorough. Oh well.


### Monolith vs services vs code

What's the difference between having a monolith and multiple services?
Communication. Consider these two related units:

```ruby
    module Authorisation
      def allowed?(user, action)
        check_database
      end
    end

    module Users
      def freeze(user)
        if !can_freeze_users?(current_user)
          return auth_error
        end

        do_it_mf
      end

      def can_freeze_users?(user)
        check_permission(user, :freeze_users)
      end
    end
```

Pretty normal monolithic code, right? What if I told you the units aren't from
the same codebase? The first module is actually a separate service. What
changed?

The real difference between monolithic codebases and service-oriented ones is
how you call the APIs.

```ruby
    # Monolithic
    Authorisation.allowed?(user, :freeze_users)

    # Service-oriented
    # makes HTTP/GRPC/etc request
    AuthorsationApiClient.allowed?(user.id, :freeze_users)
```

In an *async* service-oriented system the communication will be less direct and
the exact implementation will depend on the context. You'll probably need to
keep a copy of user permissions locally by subscribing to some changelog.

Coming back to incompetence, one of the reasons I prefer service-oriented
architectures is the fact that I'd rather own a few small services nobody
outside my own team can touch so we'll have complete control over them. I don't
trust other people so I'd rather not let them touch my stuff. When everyone owns
everything (and that's what often happens in monolithic codebases), nothing gets
done.

Also, monolith supporters often say that a properly written monolith can always
have stuff taken out of it easily. That is true. However, what are the chances
your organisation was competent enough to write such monolith?


### Think about Developer Experience

We always talk about UI/UX, customer obsession, working backwards - many
companies steal those principles from Amazon. What people don't always think
about is that the customer is not always the same as business customer.

When you write code or create a project, your colleagues (and yourself) become
your users too. Because they'll work on the code you just wrote. They'll read
it; they'll modify it. Think of ergonomics for your users.

Will it be easy to test your system/service? Is it easy to run locally? Can we
spot and solve production problems easily? Do we need documentation? Is there a
clear way to solve specific problems (existing code should encourage writing
good code).


### KISS

Keep it simple. Just because the rest of your organisation overcomplicates
things, doesn't mean you have to. When it affects system level, you should
probably talk to your tech lead so they can deal with any push back. They are
your best friend (or at least should be) when dealing with outsiders. sdaf

Exercise **common sense** at *all times*.
And do learn to recognise and act accordingly on the [one/two-way door decisions](https://www.inc.com/jeff-haden/amazon-founder-jeff-bezos-this-is-how-successful-people-make-such-smart-decisions.html),
please.


### Doubling down and going all-in is bad

Just because services or monolith seem like a good idea doesn't mean you need to
go to extremities. You can always mix and much. I think people often assume "all
or nothing" stance. But that's impractical, often dangerous. We need to always
exercise common sense. Also, you may have noticed how many startups have
suddenly decided to switch to services. When they do so drastically, it gets out
of control. It's better to do so gradually and the initiative should be overseen
by someone *competent*.

There's no shame in adding extra features to your "microservice". Just make sure
the features don't get entangled with the existing stuff. You can always extract
them later if you ever need to do so (spoiler alert: YAGNI).


### Ownership

Single responsibility and open-closed principles - take ownership of a few
services and don't let anyone touch them. Ideally, don't even touch them
yourself. All the change/feature requests must come through a specific
communication channel into your backlog. If there're multiple urgencies, well,
tough break, tell them to fuck off. No, we don't care about your KPIs. We have
our own.

If you always let them dump "urgent" stuff on you, they'll keep doing that. Take
a stand from time to time. Making compromises all the time will simply slow down
the work in the future and the company won't be able to scale (seen plenty
examples of that).

Ownership is my biggest gripe in my current workplace. There're 3 different
teams working on my repos. What the actual fuck? Recently I took a look at the
code and didn't recognise some parts of it. Some changes even screwed our team
over (not too much, luckily).

This creates a lot of different problems on different levels.

-   **Motivation**. You're more likely to give shits about stuff only your team
    works on. You want to keep it neat and clean. It's yours. This goes away when
    the ownership is shared. An no, calling a service yours but letting other
    people working on it is *not ownership*. As I said above (although pretty sure
    Aristotle said it first), if everyone owns everything, nothing gets taken care
    of.
-   **Lack of communication**. When there's no proper ownership, relevant people are
    often not involved in the decision making enough. Their attention span is also
    reduced due them being involved in lots of things (sometimes irrelevant
    things).
-   **Excess of communication**. Yep. Some relevant people don't get involved enough
    but some completely irrelevant people get involved more often. This reduces
    the quality of meetings and enforces the need for more. Hence, lack of
    communication also leads to having to communicate too much.
-   **Knowledge deprecation speed**. If things people work on change too often by
    different people, their overall understanding of the system gets affected. All
    the knowledge you accumulate deteriorates faster. What you know today is not
    true tomorrow. This isn't true when you only need to know your parts of the
    system and all the changes go through you and your team.
-   **Inability to estimate and tight deadlines**. Because everyone can make changes
    everywhere, the management puts deadlines and expects estimates in the context
    of a single team. And if the estimate date is too far in the future, they just
    throw more people into the problem (2 women can deliver a baby in 4.5 months).
    The problem here is, by working on the same stuff, people get on each others
    toes so it creates extra blocks and unnecessarily conversations. Also, due to
    the deprecated knowledge, the team can't even make a proper estimate how much
    time work in a specific service would take. When there's a clear ownership,
    you contact the relevant teams and they give you more or less precise
    estimates on their own work and potential blockers.
-   **Shit code snowballs**. Related to motivation. Have you ever found yourself
    having to add more tests or an extra function to a file full of shitty code
    but you can't really write yours properly because you have to follow the same
    pattern? Yep. Not owning stuff creates bigger snowballs. People don't care and
    sometimes people are unable to confirm crappy code isn't shitty for a reason.
    This, obviously, slows down future product features delivery. Also it makes it
    even less possible to transition to your precious "proper" microservices so
    you end up in the half-assed state where you tried to go all in but failed.
    Crappy code leads to more crappy code. Exponentially.
-   **Human resource costs**. All that stuff above leads to having to hire more
    people, and more people leaving (and having to replace these). Properly
    organised teams will work much more efficient. It won't matter if you hire
    brilliant developers or not, if you can't manage them properly.

I now realise that because it has a butterfly/domino effect on the whole
organisation and products, I could write this list for a long time. So I'll
stop.


### Governance

I've mentioned previously that you may have brilliant architects working for
you but they're gonna be absolutely worthless if there's nobody to bring
their plans to life.

It's not enough to tell people "we're gonna be event-driven now". You need to
make sure they understand what that means and actually follow that. How do we
accomplish this? I don't know.

I think maybe a stricter hierarchy could help: architects govern team leads and
team leads govern their own teams and report back to the architects. Engineering
managers should be driving the hierarchy enforcement. Yes, sounds restrictive.
But we are an organisation not a bunch of individuals, we need to work
efficiently together not by ourselves.


### If you don't care about the company, care about yourself. And speak up

Honestly, I understand the "no fucks given" attitude. After all, it's not your
business at stake. But surely you don't want to just get through the day.
Surely, you don't want to work on shitty code and doing shitty tasks for some
random idiot manager who wants to hit some KPIs.

Don't worry about the company's success. Think about your own comfort. What
could we change so **you** felt a bit better? Identify those problems and speak
up.

Yes, it's not easy to speak up. Sometimes you want to avoid confrontation. In
such cases, find a person you can talk to about this. The person, who doesn't
avoid confrontation. Who can try and make this happen. It may be your manager.
Or the team lead. Or even someone irrelevant to you or your team. But you need
to talk to *someone* about this. If you don't, everything will stay the same.

As they say on the London underground: see it, say it, sorted.


# Conclusion

I understand it's a harsh title and at times a harsh essay. Such is life. All
opinions are my own.

I also feel like I made it sound like all the problems come from stupid
engineers. Not at all. On average engineers are, well, average. So all those
people on LinkedIn quitting their jobs and saying how smart the people they had
worked with are make me cringe.

No, by incompetence I meant incompetence across organisations. And the biggest
damage is done when incompetent people are leading them. Inefficient management
leads to inefficient workers. Software engineering is easy (tell all the elitist
gatekeepers to fuck right off). It's a simple craft not much different from
locksmithing. There's no romance in it. You don't need to be a genius to be
proficient in it. Hell, you don't even need education. But if your organisation
is fucked up, your work will be too. A fish rots from the head. So we need to do
fix problems from the top not the bottom.
