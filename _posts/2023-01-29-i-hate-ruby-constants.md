---
title: "I hate Ruby constants"
categories: ['Ruby']
img: /img/posts/ruby.png
tags: ['ruby', 'constants', 'lookup', 'const_missing']
description: >-
  Short explanation of how constant lookup works in Ruby
---

*This is a loose translation of [my post](https://habr.com/en/post/347272/) from 2018.*

Ruby is a very complex programming language. It's incredibly beautiful and
expressive.  However, it also has lots of traits and implementation details that
even some of the experienced Ruby programmers may not know. One of these is
constant lookup.

I'm not gonna try to explain the lookup algorithm works. I just want to raise
some awareness of the topic. Mostly, it's just a rant.

<!--more-->

# Example

Let's make a little demonstration. Let's start with a few constants:

{% highlight ruby %}
module M
  A = 'A from M'
end

module Namespace
  A = 'A from Namespace'

  class C
    include M
  end
end
{% endhighlight %}

We have a mixin module `M` and a module `Namespace` with a subclass `C`. Each
module defines a constant `A` which we are going to try and access.

A quiz. What will the code below print out? I'll put the answers a bit further
so they don't spoil the fun.

{% highlight ruby %}
puts Namespace::C::A

module Namespace
  class C
    puts A
  end
end
{% endhighlight %}

Let's also define a few methods:

{% highlight ruby %}
module M
  def m
    A
  end
end

module Namespace
  class C
    def f
      A
    end
  end
end

class Namespace::C
  def g
    A
  end
end


x = Namespace::C.new
puts x.f
puts x.g
puts x.m
{% endhighlight %}

What do you think? Are they different?


# Answers

Here's the full code from our demo with answers in it:

{% highlight ruby %}
module M
  A = 'A from M'
end

module Namespace
  A = 'A from Namespace'
  class C
    include M
  end
end

puts Namespace::C::A # A from M

module Namespace
  class C
    puts A # A from Namespace
  end
end

module M
  def m
    A
  end
end

module Namespace
  class C
    def f
      A
    end
  end
end

class Namespace::C
  def g
    A
  end
end

x = Namespace::C.new
puts x.f # A from Namespace
puts x.g # A from M
puts x.m # A from M
{% endhighlight %}

So the output will be:

    A from M
    A from Namespace
    A from Namespace
    A from M
    A from M


# Short explanation

Simply speaking, constant lookup includes a few steps:

1.  Lexical scope search. I.e. the lookup context will depend on the place where
    the line of code is. For example, in the first `puts` the execution context
    is the top-level so it uses the constant `Namespace::C::A`. During the second
    `puts`, it first changes the context to `Namespace` module, then the `C`
    class, and only then looks `A` up. You can find more about that if you read
    about nesting and `Module.nesting` method.
2.  If the first step wasn't successful, the interpreter starts going through
    mixins and parent classes for *every* module from the first step.
3.  If even that didn't help, the top-level gets checked. Technically, it's
    included in the step 2 because top-level is an `Object` instance.
4.  At this stage the constant is considered missing and the `const_missing`
    method is called (similarly to `method_missing`).

So:

{% highlight ruby %}
# We are at the top-level
# ruby checks C and accesses A from its mixin M.
puts Namespace::C::A # A from M

module Namespace
  class C
    # We are at the Namespace -> C
    # At the first step the constant is found within the lexical scope (in Namespace)
    puts A # A from Namespace
  end
end

module M
  def m
    # We are in the context of M. There's an A within the lexical scope
    A # A from M
  end
end

module Namespace
  class C
    def f
      # We are in the context of Namespace -> C
      A # A from Namespace
    end
  end
end

class Namespace::C
  def g
    # We are in the context of Namespace::C (notice that we didn't enter Namespace)
    # First step will fail as there's no A in the lexical scope
    # At the second step A is found in the mixin.
    A # A from M
  end
end
{% endhighlight %}


# Outro

Можно сказать, Ruby заставляет нас при написании в коде констант вычислять их
значение относительно написанного кода, а не относительно контекста выполнения
(очень странно звучит, простите).

Ruby Style Guide has a good [rule of thumb](https://github.com/bbatsov/ruby-style-guide#namespace-definition): define and extend classes/modules
using explicit nesting, i.e. never write \`class A::B\`. Following this simple
rule is, practically, enough to avoid surprises and to be oblivious of constant
lookup completely.

What else you can read on the subject:

-   Chapter 7.9 of "The Ruby Programming Language" - book written by Matz.
-   [Autoloading and Reloading Constants](https://guides.rubyonrails.org/autoloading_and_reloading_constants.html) in Rails guides
-   [Ruby Style Guide](https://github.com/bbatsov/ruby-style-guide)
-   Play around with [Module.nesting](http://ruby-doc.org/core-2.2.0/Module.html#method-c-nesting)


# Update

User @DsideSPb left a useful [comment](https://habrahabr.ru/post/347272/#comment_10629706) about an additional feature of constant
lookup. However, it was deleted in the last ruby release (2.5.0).

I don't know all the details but in some cases if you specify wrong way to the
constant, interpreter may replace it with one from top-level.

{% highlight ruby %}
# 1.rb
class A; end
class B; end
A::B # returns B with a warning

# 2.rb
class A; end
module M; end
A::M # ==> M with a warning
M::A # ==> NameError
{% endhighlight %}
