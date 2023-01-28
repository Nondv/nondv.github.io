---
title: How I use git
categories: [productivity]
tags: [git, bash]
img: /img/posts/git-logo.svg
description: >-
    Everyday tips & tricks I use in git
---


This is a loose translation of my [post](https://habr.com/en/post/336708/) from
2017 + minor edits.

<!--more-->

<a id="org7774e9e"></a>

# Intro

I had to learn git basics at my first job (back in 2014). Since then I thought
that in order to be a proficient git user you only need a few commands:

-   `git add <path>`
-   `git commit`
-   `git checkout <path/branch>`
-   `git checkout -b <new branch>`

And additionally:

-   `git push/pull`
-   `git merge <branch>`
-   `git rebase master` (huh? you guys can rebase on other branches too? :O)

For the most part, I'm still thinking that but an old dog always learns new
tricks (eventually).

**Generally speaking, it's useful to know about git fundamentals. It's worth
learning what a commit is, what a branch is, what a tag is, etc.**


<a id="orgec49899"></a>

# Some handy settings


<a id="orgd6db37a"></a>

## Auto completion

I was surprised to find out that not everyone has it enabled. [Here you go](https://letmegooglethat.com/?q=git+completion).


<a id="org6e370de"></a>

## Adding current branch to your bash prompt

Add this code to your `.bashrc`. I've been using it since starting that first job of mine.

```bash
function git-current-branch {
    git branch --no-color 2> /dev/null | grep \* | colrm 1 2
}

function set_prompt_line {
    local        BLUE="\[\033[0;34m\]"

    # OPTIONAL - if you want to use any of these other colors:
    local         RED="\[\033[0;31m\]"
    local   LIGHT_RED="\[\033[1;31m\]"
    local       GREEN="\[\033[0;32m\]"
    local LIGHT_GREEN="\[\033[1;32m\]"
    local       WHITE="\[\033[1;37m\]"
    local  LIGHT_GRAY="\[\033[0;37m\]"
    # END OPTIONAL
    local     DEFAULT="\[\033[0m\]"
    export PS1="$BLUE\w $LIGHT_RED[\$(git-current-branch)]$DEFAULT \$ "
}

set_prompt_line
```

Pro tip: your prompt configuration is stored in the variable `PS1`. Have fun.


<a id="org2320109"></a>

## Aliases

Git has its own aliases but I don't like them.

```bash
#
# Git
#
alias gst='git status'
alias glog='git log'
alias gcheck='git checkout'
alias gamend='git commit --amend'
__git_complete gcheck _git_checkout
alias gcom='git commit'
__git_complete gcom _git_commit
alias gdiff='git diff'
__git_complete gdiff _git_diff
alias gadd='git add'
__git_complete gadd _git_add
```

Note: `__git_complete` enables completion for an alias.


<a id="org73ac64a"></a>

## Edit commit messages in your favourite text editor

Let's start with a scary story based on real events:

> Once upon a time, a young inexperienced programmer wanted to commit his code but git opened vim!

Yep, it's my true story. After a few hours I managed to close it and since then I committed exclusively via `git commit -m` with one-line messages.

Git, similar to some other programs (e.g. crontab) check the `EDITOR` variable. Add this to your bash config (~/.bashrc):

```bash
export EDITOR=<your editor's executable>
```

For me it's `emacsclient`. Before that, it was `subl` (Sublime Text). I haven't checked but I'm assuming it's important that the command locks the terminal until the text file's closed.


<a id="org333fede"></a>

# Tips & Tricks


<a id="orgcc30f4b"></a>

## Switch branch without losing your uncommited changes

Sometimes you can just switch the branch but sometimes you'll get conflicts. I know two ways around them:

1.  Create a temporary commit
2.  `git stash`, switch branch, &#x2026;, switch back, `git stash pop`

The former is more reliable. The latter is more convenient (IMHO).


<a id="org29b18a4"></a>

## Show the changes I've made

```bash
git diff
```

This command shows your changes relative to the current commit + stage (this is important). Note: new files aren't included.


<a id="orgfd0002e"></a>

## Show the changes in the stage

```bash
git diff --cached
```

Note: this *does* show new files.


<a id="orge2ef1ec"></a>

## Delete untracked files

I.e. files that aren't committed to the repo

```bash
git clean -df
```

-   `-d` — also deletes directories
-   `-f` — required flag. Without it, git won't do anything.


<a id="org694bc89"></a>

## Cancel last commit

I've got a bash alias for that:

```bash
alias git-uncommit='git reset --soft HEAD~
```

У меня на это дело есть alias в баше:

Explanation:

`git reset --soft <commit/branch/tag>` sets the branch to the specified commit but doesn't checkout the code. The difference is kept in the stage.

`HEAD~<N>` points to N commits before the current one. `HEAD~` is equivalent to `HEAD~1`.

Basically, the whole command just sets the branch to the previous commit and saves the difference in the stage.


<a id="org67ac5b9"></a>

## Join multiple commits

When I work on my own branch, sometimes I create a few commits that make no sense in isolation (I do that to commit as much as possible without losing my train of thought). So it makes sense to join them before publishing the branch.

Solution: interactive `rebase`!

```bash
git rebase -i master
```

This will open your text editor with a list of commits. You can:

-   Reorder them (pretty useful)
-   "Squash" them - joining multiple commits into one
-   Edit them - git will stop during rebase so you can make changes with `--amend`
-   Changing the commit message - you can do that during edit too.
-   Exclude commit from the branch completely.

For this particular case, all we need to do is to place commits we want to join one after another and mark them with `squash`.

Usually I use interactive rebase every time I'm done working with on feature to see which commits I wanna join, which commits I want to rearrange (for aesthetic reasons), which commits I want to edit. This helps to keep versioning at a higher quality.


<a id="org7de6610"></a>

## Add something to a previous commit

```bash
git add <forgotten changes>
git commit --amend
```

See also:

```bash
git commit --amend --no-edit # keeping the message without changes
git commit --amend -m 'my commit message' # this one's obvious
```


<a id="orgdd47dda"></a>

## Add changes to an older commit (when it's too late for `--amend`)

Use case:

3 commits ago I've made a typo and I don't want anyone to see a dedicated commit fixing that (because it creates useless noise and is generally embarrassing).

Solution:

Interactive rebase!

```bash
git rebase -i HEAD~3
```

Usually in a situation like that (happens to me a lot) I commit the fix with `[to_squash]` in the message. After that whenever I'm ready to use interactive rebase, it'll be easy to identify commits that need joining.


<a id="orgf50a834"></a>

## Commit only a part of a file changes

It's best to keep commits simple. It's quite common to see a huge diff that includes a new feature along with a bunch of related (or even unrelated) changes like refactoring. How on earth are we supposed to read those diffs? It'd be much better if refactoring was committed separately.

For example, consider this code:

```ruby
def sqare(x)
  x * x
end

puts sqare(n)
```

Let's say I want to add a new feature: printing `2*n`. Easy-peasy! And while I'm at it, I fix the naming typo.

The result:

```ruby
def square(x)
  x * x
end

def double(x)
  x + x
end

puts square(n)
puts double(n)
```

But now I have two unrelated changes in one file. How do I commit? I could bring the old name back, commit the new feature, and *then* fix the naming. But this is not always appropriate as the changes may be quite large.

Git has a great command:

```bash
git add -p
```

It's interactive. It grabs parts of the diff (hunks) and asks you what to do with it: ignore, add, or edit+add. The third option is quite powerful; you can commit even a *part* of a line (`sqare(x) + cub(x)` -> `square(x) + cub(x)` in two commits).

I recommend trying it out for yourself. It's better to try once than to hear thrice.


<a id="org15a50f6"></a>

# Notable mentions

-   `git reflog` — saved my butt when I accidentally deleted a branch without pushing it.
-   `git rebase -i` — I only mentioned a couple of use-cases but there're more.
-   `git log --graph` — It's quite cool. Not sure if it's practical though
-   `git cherry-pick <commit>` — tries to apply a commit to the current branch.


<a id="org0bbd0a0"></a>

# Outro

It's only a few git tricks but I use them *every* day.

I'd like to emphasise: **know your tools!**

Git (and many other tools you use or may be using) has lots of features that can make your life easier. For instance, there're repos that use commit validators: you can't commit unless some tests don't pass or there're tests missing for new files.
