---
title: "ruby -e for better console experience"
categories: ['Ruby', 'Tooling', 'Productivity']
tags: ['ruby', 'bash', 'one-liners', 'console', 'terminal']
img: /img/posts/desktop.jpg
description: >-
  Using ruby one-liners in terminal
---

*This is a loose translation of [my post](https://habr.com/en/post/339544/) from 2017.*

Ruby was influenced by Perl and can substitute it in its niche of "practical
extraction and reporting". In this small post, I'll talk about using ruby for
simple text processing in the terminal.

<!--more-->


# TL;DR

Remember these two forms:

{% highlight nil %}
... | ruby -lne '<CODE>'
ruby -lne '<CODE>' file1, file2, ...
{% endhighlight %}

In the former, your input will come from some previous command output (via
pipe). In the latter, it'll come from the concatenation of multiple files.

CODE will be executed for every **line** of the input which will be available via
`$_` variable. Important: lines won't have `\n` at the end (which is enabled via
`-l` flag).


## Examples

This is how we could implement `tac` (reverse `cat`):

{% highlight bash %}
ruby -lne 'puts $_.reverse' file-to-show.txt
{% endhighlight %}

Display the first 10 characters from each filename in the current directory:

{% highlight bash %}
ls | ruby -lne 'puts $_[0..9]'
{% endhighlight %}


# Some details


## Pre-defined variables

Ruby has many pre-defined variables which we may have never even heard
of. Some of them coming from Perl, e.g. `$_` and `$<` (to be precise, Perl
doesn't have `$<` but it's got input operator `<>` reading from files/stdin and
setting `$_`).

You can see the list of those variables [here](https://ruby-doc.org/core-2.4.2/doc/globals_rdoc.html).

In the context of this post, I recommend you take a look at the aforementioned
`$_` and `$<` and also `$~`, `$1`, `$stdin`, `ARGV`, `$stdout`, `STDIN` и
`STDOUT`.


## `gets`' terrible secret

Maybe you've used `gets` before to request some user input from the keyboard.
However, what you may not know is that it also checks `ARGV` (command line
arguments).

When `ARGV` isn't empty `gets` assumes that it has a list of files, takes the
first one and tries to read a line from it.

Consider this script:

{% highlight ruby %}
# myscript.rb
puts gets
{% endhighlight %}

Let's run it with `ruby myscript.rb`. As expected, the program is waiting for
our input.

Let's try running it with `ruby myscript.rb no-such-file.txt`

We'll get a "no such file" error. Let's change the script:

{% highlight ruby %}
# myscript.rb
ARGV.pop
puts gets
{% endhighlight %}

Now the program is waiting for our input once again because we removed the item
from `ARGV`.


## Interpreter options

This is my template for one-liners:

{% highlight nil %}
ruby -lne <CODE> [file1, file2, ...]
{% endhighlight %}

(often without `-l`)

Here's a description of used and some helpful options:

-   `-e <CODE>` - makes ruby not assume that a script is provided in the arguments
    and runs `CODE` instead.
-   `-n` - wraps your program (whether from a file or `-e`) in a `while gets`
    loop. `gets` implicitly stores its result in `$_` variable (hello, Perl!),
    which we can use. **Important:** the string will end with `\n`
-   `-l` - For simplicity, we can say that it just removes newline (`\n`)
    character from `$_`.
-   `-p` - works the same way as `-n` **but** after each iteration it prints `$_`.
    So instead of printing stuff out yourself, you can just change/mutate `$_`.
    Try this: `ls | ruby -pe '$_.upcase!'`.
-   `-C <DIR>` - sets your workdir.
-   `-a` (from "auto-split") - can be used with `-n` или `-p` and sets `$F` to
    `$_.split`.
-   `-F` - sets `$;` which is a default delimiter of `String#split`. In theory,
    can be useful along with `-a`. For instance, we could process simple `.csv`
    files (although I think it's easier to just do `.split` yourself).


# In conclusion

The first time I thought of using `ruby-e` because of the program `cut`. I had
to use it a few times and *every* time I had to open the manual which my had was
taking a long time to understand *every* time.

In the end, I thought: "Yes, I don't know how to use `cut=/=awk=/=sed`. But I
know quite a bit of Ruby so why can't I just use it for small tasks?".

So now, instead of remembering how to use numerous of \*\*nix tools I can just
remember this form:

{% highlight bash %}
ruby -lne CODE
{% endhighlight %}

And that's it!

There's nothing new about this approach. Bearded system administrators have been
using perl for this for a long time.

P.S. Here's an example of how I lately used this:

I downloaded a TV show but it had really ugly filenames like "s01e01 - super
release by super-mega-macho.mkv". I wanted to rename them to keep it neat. Here
you go:

{% highlight bash %}
ls | ruby -lne 'File.rename($_, "#{$_[0..5]}.mkv")'
{% endhighlight %}
