---
title: Implementing OOP in FP
categories: ['Weird Programming', 'Software Engineering', 'OOP', 'Functional Programming']
tags: [programming, oop, fp, actors, clojure, elixir, erlang]
img: /img/posts/implementing-oop-in-fp.jpg
img_caption: Weird DALL-E 2 generation
is_featured: true
description: >-
  Trying to "implement" OOP in a functional language. What is OOP? Is it really
  different?
---

Some years back I wrote the essay ["Functional programming, meet OOP"](https://nondv.wtf/blog/posts/functional-programming-meet-oop.html). A few months ago I gave a talk loosely based on it but with some major improvements. It didn't feel right to update the original post so instead I decided to rewrite it to go a *little* bit more in detail of the higher level concepts and to make it less Clojure-specific.

<!--more-->

# Intro

I love exploring and experimenting with programming concepts. I find it quite
interesting overall and useful for deeper understanding of the ideas brilliant
people have came up with. It makes us better engineers. For instance, the main
reason I quit my first job was I wanted to explore OOP (also I hated my first
job).

For years I've been toying around with functional programming, lambda calculus, etc. At some point I started wondering about how "compatible" OOP and FP are. So I decided to see if I can "implement" OOP in a functional language.


## The language of choice

My first thought was to use the purest language I could think of - Haskell.
However, after some deliberation I settled on Clojure since I had *actual*
experience with it and it's really well suited for experimenting and prototyping
due to interactive programming capabilities.

However, I had to establish some ground rules. Clojure, being a practical language, has lots of "impure" stuff e.g. Java interop. One thing to avoid was state (so no atoms, agents, etc). Obviously, no hacks to generate java classes and objects. Overall, the rule of thumb was to ask yourself "is this a Clojure thing or an FP thing?".

For the sake of this essay, I'll be using pseudocode so it's a bit easier to read for wider audience (although maybe I made it worse haha).


# What is OOP?

When coming up with ideas how to do this, I used [Alan Kay's quote](https://www.purl.org/stefan_ram/pub/doc_kay_oop_en) as a source of inspiration:

> OOP to me means only messaging, local retention and protection and hiding of state-process, and extreme late-binding of all things.

If we look closely, we can actually highlight three things here:

1.  Messaging
2.  Local retention and protection and hiding of state-process
3.  Late-binding

Let's try to define what they mean.


## Messaging

According to Dr. Kay, it's the most important idea behind OOP. In fact, he [regrets](http://lists.squeakfoundation.org/pipermail/squeak-dev/1998-October/017019.html) using the term "object" because it makes people "focus on the lesser idea".

The concept is simple in its nature: you divide your program into small units (objects) and the only way you can do something with them is to *send* them a message. You don't actually execute anything, you send a message (kind of like sliding into Instagram dm's) and hope it's gonna be understood.

And that's it.

Encapsulation and polymorphism which are the focus of most OOP languages are really just benefits (almost a side-effect) that come with the messaging system.


## Local retention, protection, and hiding of state-process

Behaviour and state are contained within objects and not exposed to the outside world.

Let's compare coding to storytelling. In procedural programming the story is told by a narrator that says exactly what's going on and what's going to happen next. In OOP, the story is told from the first person. All we know is what one character is thinking. As the other characters go, we can only see their actions and hear their words without actually knowing what's going on inside their heads.

The way I see it, it's pretty much a rule to enforce using messaging for everything. If we expose internals of the object, there's gonna be a temptation to use them directly instead of sending a message which will lead to lower level abstractions (which isn't good btw).


## Late binding

Conveniently, there's a [wiki page](https://en.wikipedia.org/wiki/Late_binding)!

The way I define it is: specific implementation (types and behaviour) is only known at runtime.

Take a look at this example:

```ruby
# pseudocode
sendMessage(logger, "describeUser", user)
# smalltalk-like syntax
logger describeUser: user
# most common
logger.describeUser(user)
```

By looking at this code, we can't tell what `logger` technically is or its output format. For all we know, it may not even be logging to stdout but making external device calls instead. But do we care here? No. There's a contract that objects used in this scenario will understand "describeUser" message and which particular implementation it is, we don't care. In runtime, the program will find the particular implementation (likely, based on the object's class) and execute it. As the object user, I don't really care what that is (the black box abstraction).

This is how polymorphism is achieved.


# Implementation


## State

Before diving into the messaging system, we need to somehow represent objects themselves. That means adding state. And we can't use Clojure's atoms.

How is state managed in procedural programming? We assign values to variables and change them over time (as the algorithm's being executed).

In functional programming variables are different. We can't really change their values once assigned. So how do we go around it?

Well, we can either overshadow them (boooo), create intermediate variables for each step (which brings `let` spaghetti hell) or we can execute another (or the same) function with updated values.

Here's a simple example:

```ruby
# Procedural
def listSum(list)
  result = 0
  while !empty?(list)
    result = result + head(list)
    list = tail(list)

  return result

# Functional
def listSum(list, result)
  if empty?(list)
    return result
  else
    return listSum(tail(list), result + head(list))

total = listSum(numbersList, 0)
```

The two are absolutely identical (I'm pretty sure some compilers would produce identical machine code too): we have two variables (`list` and `result`), result is being added to on each step, the list is being shortened one element at a time, and there's the emptiness check. The difference is how exactly we reassign the variables.

So with this in mind I came up with the idea of "object loop":

```ruby
def objectLoop(state)
  nextState = someUpdate(state)
  return objectLoop(nextState)
```

This already achieves **local retention, protection, and hiding of state-process**. Once we spin up an object, there's no way for us to know what's going on inside of it, the state is completely hidden as it's merely temporarily bound to the variable called `state` which then gets discarded (with tail-call elimination).


## Messaging

Since the updates will come only as a reaction to messages, let's update the object loop:

```ruby
def objectLoop(state)
  message = receiveMessage()
  nextState = processMessage(message, state)
  return objectLoop(nextState)
```

How do we receive messages though? In my Clojure implementation I opted for using [channels](https://clojuredocs.org/clojure.core.async/chan) since they're widely present in different languages. But the reality is, any means of communication would do: unix sockets, internet, text files, etc. Channels are simply very convenient.

Objects are now represented by 2 things: a channel and a message handler. On top of that, it just needs an object loop running.

```ruby
# Object struct
# { channel: Channel, messageHandler: Fn }

def sendMessage(obj, msg)
  appendToChannel(obj[:channel], msg)

def receiveMessage(obj)
  return fetchFromChannel(obj[:channel])

def objectLoop(obj, state)
  message = receiveMessage(obj)
  nextState = obj[:messageHandler](obj, state, message)
  return objectLoop(obj, nextState)

def init(initialState, messageHandler)
  channel = createChannel()
  obj = { channel: channel,
          messageHandler: messageHandler}

  objectLoop(obj, initialState)
  return obj

```

Looks good, however, there's a bug. Once `init` gets to `objectLoop`, it'll get stuck because, well, it's an infinite loop. How do we go around it? We run the object in a separate thread! Or process, whatever.

```ruby
def init(initialState, messageHandler)
  channel = createChannel()
  obj = { channel: channel,
          messageHandler: messageHandler}

  newThread(() => objectLoop(obj, initialState))
  return obj
```

Here. Now we're good. Let's create an object:

```ruby
counter = init(
  0,
  (obj, n, msg) => if msg == 'dec' then n - 1 else n + 1
)
sendMessage(counter, "increment plz")
```


## More practical example: StringBuilder

```ruby
def stringBuilderMessageHandler(self, strings, msg)
  case msg[:method]
  when :add
    return strings + [msg[:str]]
  when :addTwice
    # This is crappy (and buggy)
    # but it showcases sending messages to self
    sendMessage(self, { method: :add, str: msg[:str] })
    sendMessage(self, { method: :add, str: msg[:str] })
    return strings
  when :build
    result = concatAll(strings)
    sendMessage(msg[:resultReceiver],
                { method: msg[:receiverMethod],
                  str: result })
    return strings

stringBuilder = init([], stringBuilderMessageHandler)
sendMessage(stringBuilder, { method: :add, str: "hello"})
sendMessage(stringBuilder, { method: :add, str: " world"})
sendMessage(stringBuilder, { method: :build,
                             resultReceiver: logger,
                             receiverMethod: :info })
```


## What's next?

We pretty much created our own "OOP" "language". We can build it further and introduce DSL for defining message handlers via methods (similar to the case statement above).

We can also introduce classes (in the original post I actually implemented them as object factories, which is weird but also fun IMO).

If you're interested, go ahead and take a look at the Clojure code with a todo list example and some tests:

<https://github.com/Nondv/experiments/tree/master/functional_oop>


# Response to the original essay

When I published the original post on different platforms (e.g. Reddit), I received some feedback. It was mentioned multiple times that what I came up with looked a lot like [Actor Model](https://en.wikipedia.org/wiki/Actor_model). At the time I wasn't familiar with it but I thought it was an amusing coincidence. Since then I had an opportunity to learn more and even had some experience with the it (via Elixir which piggy backs off Erlang's OTP). So I'd like to share some thoughts.


# Actor model


## What is it?

It's a way of organising programs as a collection of independent actors that communicate with each other by sending messages. In response to a message, actors can:

-   Make changes to its state
-   Create new actors
-   Send more messages

Sounds familiar? That's a lot like OOP described by Dr Kay, don't you think?

It was introduced in a paper in 1973 by Carl Hewitt. Around the same time as Smalltalk was released (and I'm sure Alan Kay was playing around with the concept long before that). So there's a chance that Hewitt was partially inspired by Kay's work or even the other way around. Unfortunately, Dr Hewitt's died recently so I don't think we'll ever be able to find out.

Nowadays the main two systems that use actor model are Erlang/Elixir OTP and Akka (JVM based). I don't have experience with the latter but OTP is freaking awesome.


## Some (not necessarily good) examples of actors usage

Actors can be used for:

-   Lightweight in-memory storage. Since the communication is handled by the platform natively (e.g. OTP) it's likely more efficient than third parties. You can literally create an actor that responds to "set" and "get" and use it instead of, say, Redis in some cases. And don't forget, there's no overhead, it's all done *natively* in the language/VM you're using.
-   Modelling of business entities (e.g. an IoT device or an NPC in a game). That's pretty much the same as you'd use objects in OOP. You may be interested in [Eero case study](https://www.lightbend.com/case-studies/how-eero-disrupts-consumer-wifi-with-highly-reliable-systems-powered-by-akka-and-reactive).
-   Distributed systems. That's pretty much what it's made for. Actors don't have to live on the same machine. Erlang provides native ways to send messages to actors on a different node just like you'd send them locally (almost). See Erlang's [docs](https://www.erlang.org/doc/reference_manual/distributed.html).


## StringBuilder again

I'd like to implement StringBuilder from the above in Erlang and Elixir

Elixir:

```elixir
defmodule StringBuilder do
  def object_loop(strings \\ []) do
    receive do
      {:add, str} ->
        object_loop(strings ++ [str])
      {:add_twice, str} ->
        # still crappy
        send(self(), {:add, str})
        send(self(), {:add, str})
        object_loop(strings)
      {:build, receiver, method} ->
        send(receiver, {method, Enum.join(strings, "")})
        object_loop(strings)
    end
  end
end

string_builder = spawn(fn -> StringBuilder.object_loop() end)
send(string_builder, {:add, "Hello"})
send(string_builder, {:add, " world"})
send(string_builder, {:build, self(), :print_string})

receive do
  {:print_string, str} ->
    IO.puts(str)
end
```

It works (and looks) pretty the same as my pseudocode (and Clojure) example. Receiver in this case is the current actor. Notice that `send` and `receive` are native constructs to the language.

Erlang example:

```erlang
string_builder() -> string_builder([]).
string_builder(Strings) ->
    receive
        {add, Str} ->
            string_builder(Strings ++ [Str]);
        {add_twice, Str} ->
            self() ! {add, Str},
            self() ! {add, Str},
            string_builder(Strings);
        {build, Obj, Method} ->
            Obj ! {Method, string:join(Strings, "")},
            string_builder(Strings)
    end.

main(_) ->
    StringBuilder = spawn(fun () -> string_builder() end),
    StringBuilder ! {add, "Hello"},
    StringBuilder ! {add, " world"},
    StringBuilder ! {build, self(), print_string},
    receive
        {print_string, Str} ->
            io:format("~s~n", [Str])
    end.
```

It's absolutely the same, just in different language. I added it because I thought `send` syntax in Erlang (the exclamation mark) is funny because if you replace it with a period, it'll look like an OOP language.


# Conclusion

There's none, really. But it's interesting how different ideas all come down the same "split into black-box components" design.

Looking at the OOP vs FP, the only difference I see between the two is how state is managed: internal variables vs closures. Other than that it's all about the way we perceive them. First order functions? Create a "callable" object. Immutable data structures? Who says you have to mutate anything? Type abstractions vs primitives? Have you seen ML-languages? Nobody will tell you what constitutes an FP language and the way I see it, most OOP languages aren't really OOP either.

Language shapes the way its speakers think. But also language is shaped by its speakers.


# Some reading links

-   My original essay ["Functional programming, meet OOP"](https://nondv.wtf/blog/posts/functional-programming-meet-oop.html)
-   [Dr Alan Kay on the Meaning of "Object-Oriented Programming"](https://www.purl.org/stefan_ram/pub/doc_kay_oop_en)
-   [Dr Alan Kay on messaging](http://lists.squeakfoundation.org/pipermail/squeak-dev/1998-October/017019.html)
-   [Pharo](https://pharo.org) - a modern Smalltalk system (Smalltalk is awesome)
-   [Eero case study](https://www.lightbend.com/case-studies/how-eero-disrupts-consumer-wifi-with-highly-reliable-systems-powered-by-akka-and-reactive) on Akka's website
