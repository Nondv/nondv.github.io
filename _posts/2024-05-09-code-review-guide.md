---
title: Code review guide
categories: ['Software Engineering', 'Productivity']
tags: ['github', 'pr', 'cr', 'code review', 'codereview', 'communication', 'pr comments']
img: /img/posts/inspectocat.jpg
img_caption: >-
  <a href="https://octodex.github.com/inspectocat/">Inspectocat</a> on octodex.github.com
description: >-
  Recommendations on how to make code review process efficient and useful
---

For months (even years) I wanted to compile a list of advice I have on code reviewing. Initially, for myself but recently I thought it'd be a good idea for a blog post. Some of the advice is generic and some is quite opinionated.

I'll try to keep this updated (all my other posts are one-offs).

<!--more-->

# Table of Contents

- [Pay attention to tests](#org47256c6)
  - [Prefer specs over tests](#orgf91d6df)
  - [Before reviewing tests, think to yourself, what needs to be tested](#org971e9e3)
  - [Consider writing "scenario" tests](#org0f5270f)
- [Consider pairing or review with the author present](#org4503bd6)
- [Introduce a checklist/questionnaire to your process](#orgfc5d442)
- [Communication](#orge5245bb)
  - [Use tags to set the tone and convey importance of proposed changes](#org585ca85)
  - [Avoid using "request changes" feature](#orge4ef7a8)
  - [Cultural diversity - always assume good intentions](#org07826b4)
- [As the PR author](#org58801b9)
  - [Don't take anything personally](#orgd0d6873)
  - [Don't ignore comments](#org4a82d36)
  - [Make sure your PR description and commits are easy to read](#org1addbc6)
  - [Review your own work before asking someone else for review](#org908d22a)
- [Code review questionnaire (PR template)](#org2ec31c7)


<a id="org47256c6"></a>

# Pay attention to tests

Tests can be very useful. They provide some guarantees that something works as expected; they can prevent you from introducing breaking changes; and they can act as documentation. It makes a lot of sense to put an emphasis on testing. Especially, as a reviewer, because your job is to make sure that proposed changes match the product requirements


<a id="orgf91d6df"></a>

## Prefer specs over tests

I truly believe that written intention/purpose of tests is more important that the tests (assertions) themselves. And that's what specs are in the nutshell: they describe the *purpose* behind some function/module/feature/etc. If there's no purpose, why are you even building it?

Well-written specs also help you write better tests because it becomes clearer what exactly needs to be tested. They also help the reviewers to come to an informed conclusion whether a test is good or not.

Specs for higher level features also make it easier to reason about with other people e.g. other teams or even non-technical people like your product manager. Such specs pretty much come directly word-to-word from business requirements.

When writing specs, make sure they're human-readable and easy to understand. Literally, ignore the code and read them to yourself out loud as English prose. If it doesn't sound like proper English, you're doing it wrong. I often see tests like "Feature X, when user not authorised" and then there's code with assertions. The sentence describes the situation but doesn't actually say what *should* happen and forces you to read the code. I don't want to read your shitty code, just tell me what you're trying to do first.

As a coder, when I get to testing, before I write any assertions or setup I first jot down a high-level specification for the thing I'm testing. It doesn't have to be TDD but it works very well with it.

Ideally, your testing framework already has some sort of spec-ing DSL similar to Ruby's RSpec or Clojure's `testing` macro:

```ruby
RSpec.describe JobScheduler do
  it 'takes job type and payload, schedules it and returns job id' do
    code_goes_here
  end
  it 'throws if job type doesnt exist'
  it 'optionally, runs the job synchronously'
  context 'when exact time specified' do
    it 'sets the job time'
    it 'throws if time is in the past'
  end
end
```

```clojure
(deftest schedule-job
  (testing "schedule-job"
    (testing "takes job type and payload, schedules it and returns job id"
      (code-goes-here))
    (testing "throws if job doesn't exist")
    (testing "optionally, runs the job synchronously")
    (testing "when exact time specified"
      (testing "sets the job time")
      (testing "throws if time is in the past"))))
```

If not, you can at least use comments instead


<a id="org971e9e3"></a>

## Before reviewing tests, think to yourself, what needs to be tested

When we don't think of the requirements *before* reading the code, what often happens is we start going through it almost following the chain of thought of the author and everything simply makes sense. If something's missing, we may not notice it.

Just because something makes sense, it doesn't mean it's correct.


<a id="org0f5270f"></a>

## Consider writing "scenario" tests

Depending on the context, I also call them "flow tests". Basically, instead of describing what your feature does, it may be helpful to describe a scenario (a script/play) of its usage. For example:

```
- Scenario: user with expired auth token tries to access Product API
  - list endpoint returns 404
  - the body is json with "error": "unauthorised"
```

as opposed to

```
- Product API
  - When there's no token
    - list endpoint returns 404
    - the body is json with "error": "unauthorised"
  - When the token expired
    - list endpoint returns 404
    - the body is json with "error": "unauthorised"
```

The latter is more generic and has more coverage. The tests in it are very similar (which isn't a problem, to be clear). However, the first one, arguably is a bit higher level and may be set up closer to integration tests. The former also can't really serve as documentation BUT a few of those can serve as a kind of a playbook/FAQ (i.e. as a dev I may have a good idea how the API behaves already but I may be interested in some specific edge cases or just the happy path).

Scenario tests are often easier to write as they are quite specific so the set up is more straightforward

I, personally, write those in certain cases:

-   The scenario is clearly outlined in the requirements. Main specs probably already cover it but often implicitly. A scenario test makes it explicit (even if introduces a bit of duplication)
-   The thing I'm working on is a PoC and I don't want to invest too much time in testing. Specific scenarios (e.g. "Scenario: happy path") are often easier to write while providing some important coverage
-   Bugs. TDD recommends writing a red test first when working on a bug. Scenario is a clear set up for it and is often faster to write than modifying existing tests (e.g. when they're a legacy mess). Be careful though, the test may look awkward (kinda random) in the future ("why was this significant?") so make sure the commit history provides context or maybe even leave a comment.


<a id="org4503bd6"></a>

# Consider pairing or review with the author present

Pair programming often reduces time needed for code review as you have a second pair of eyes working on the issue with you. However, not everyone likes pair programming. Many people are simply more productive on their own.

Consider pair reviewing. You just get in touch with the PR author and ask them to walk you through the changes.

But be careful though, you may fall into the same fallacy I mentioned earlier where everything you hear makes sense and you miss some problems.


<a id="orgfc5d442"></a>

# Introduce a checklist/questionnaire to your process

It may be beneficial to have a checklist/questionnaire for reviewers and authors to fill out that'll cover the most important properties of the changes. Different projects will have a different checklist, e.g. some will be specific to your system design (architecture), some will be industry-specific (e.g. compliance in Finance, GDPR, etc), some will be team-specific (e.g. JIRA ticket created, PM approved the UI changes, etc).

Some general advice:

-   Identify things that are often overlooked and lead to bugs. For example, the company I'm working for uses Kafka for interservice communication. However, not many people in general have experience with async processing so the mistake I see quite often is not making sure your code is idempotent which makes things happen multiple times and break. Things like that should be checked every time and are good candidates for the checklist/questionnaire.
-   Try to write the questionnaire in a way that every question *has* to have at least one checkbox ticked. Consider this:

    ```
    - Manual testing
      - [ ] Locally
      - [ ] Staging
    ```

    You're reading through someone's answers and encounter this, both unchecked. What does it mean? Has the person not tested it? Have they forgotten to answer? This can be resolved by simply adding the third option "Nope, not tested".
-   Try to encourage people to provide more context when answering it wherever relevant. For instance:

    ```
    - Automated tests
      - [ ] Unit
      - [ ] Integration
      - [ ] E2e
      - [ ] Nope. *Explain why*
    ```
-   Try to make the extra comments more visible by asking people to use **bold** style on them. Otherwise the comments will go unnoticed after people get used to skimming through the whole thing.
-   Keep in mind, it does feel very bureaucratic and daunting to fill out a questionnaire. So try to make it as brief as possible, covering only important points (that's a tall order, I know. I'm still struggling myself). You may also want to introduce it as a formal process within your team (some bureaucracy may be useful sometimes).

I'll leave an example of the questionnaire I introduced to my team recently at the end of the essay. It's far from perfect and specific to our project but hopefully you'll find it helpful to get you started.


<a id="orge5245bb"></a>

# Communication

There're many posts on the internet about this. Most of them go along the lines of "say *'What do you think of X'* instead of *'it is conventional to do X here'*". It's almost like reading a "corporate emails for dummies" article.

It's generally a good advice in my opinion, however, many people feel like it's too bureaucratic and formal. Also, it forces you to change your style of communication which isn't necessarily a bad thing but is quite challenging.

Ever since moving to England from Russia communication has always been an issue for me (eastern europeans are infamous for being very straightforward and even straight up rude in some cases). While I'm actively working on it and have come a long way (I think), I'm still not perfect and it'd be unwise to not take that into account.

The main problem of communication (not only during code reviews but in general) is that people may misunderstand you or misinterpret your intention or tone (imagine trying to guess tone over the internet).

So I figured in addition to working on my communication style, it's better to set the intent and tone of a message explicitly and straight away. What I came up with is *tags*.


<a id="org585ca85"></a>

## Use tags to set the tone and convey importance of proposed changes

To leave less to interpretation, you can prepend your comments with tags setting up the tone/importance of the comment.

Obviously, you need try and communicate as clear as possible but let's be honest, that's really hard. A tag makes it less likely for you to be misunderstood.

Here's some of the tags I use:

-   `[strong]` - means you feel really strong about this piece and it has to be discussed. Use in combination with other tags
-   `[bug]` - there's an actual problem and it needs to be resolved.
-   `[suggestion]` - is for proposals that you think would improve the code. In most cases, the author will likely just follow them. If you feel like may be subjective, you should make it obvious by either saying it explicitly or adding another tag like `[subjective]`. More often than not, some conventions one uses are based on personal experience and are hard to reason about. However, when proposed by a senior person, they have more weight simply because that person has seen some shit so don't be shy to propose them (but see the `[alternative]` tag which may be more appropriate).
-   `[alternative]` - you don't necessarily think it's better or unsure. You may also use this as a way to share some knowledge but you don't think it's necessarily worth redoing, i.e. "your implementation is good to go but have you considered X? It has benefits of Y and Z".
-   `[question]` - literally a question. Without the tag those can often be perceived as a suggestion or passive aggression.
-   `[nitpick]` - for things that aren't important or maybe even style suggestions (could be combined with `[style]` tag). Can be safely ignored but normally are fixed together with other stuff. If your review has only nitpicks, you should always approve the PR to make sure the person can deploy it without addressing them if needed (sometimes it's better to address those separately post-deploy).
-   `[observation]` - to add a general comment about something. Or maybe when you see a problem but don't think it should be addressed within this PR. For instance "We keep using the same pattern everywhere. We should probably come up with a way to unify it".
-   `[blocking]` and `[non-blocking]`. Not something I actually use myself (I usually communicate this with `[strong]` tag or in the comment itself by saying something like "not actionable" or "feel free to ignore this") but it was suggested by some people and honestly I think it's much better than what I do.

Consider these examples:

```
Have you considered using Redis instead?
```

```
[strong]
Have you considered using Redis instead?
```

```
[suggestion]
Have you considered using Redis instead?
```

```
[alternative]
Have you considered using Redis instead?
```

```
[question]
Have you considered using Redis instead?
```

The tag here communicates your intention. It can also make the tone bit clearer:

```
Why didn't you cover this in tests?
```

```
[question]
Why didn't you cover this in tests?
```

While both variants are really blunt, the former *can* be perceived as an attack whereas the latter looks more like an enquiry for some clarification.

Of course, ideally, you *also* should word it a bit less blunt. Communication style is covered by various guides on the internet even outside software engineering context so please look them up.

A colleague also told me about "shit sandwich" technique where you mix negative feedback with the positive so they balance each other out. I think it's important to give credit where it's due however I feel like it's wrong to provide positive feedback for the sake of it as it may look insincere. In fact, personally, I hate receiving praise (I need therapy haha) so even positive things may be perceived in a negative light. Can't please everyone

It was also pointed out to me that there's a system similar to my tags called ["Conventional Comments"](https://conventionalcomments.org). It's pretty much the same but there's a fancy website with really good examples and explanations. I'm sure it'll be easier to convince your team to follow that rather than some system from a random chump's blog.

And you don't actually have to set up a formal process around it. You can use it just by yourself (after all, it's about you and your communication style, not your teammates).


<a id="orge4ef7a8"></a>

## Avoid using "request changes" feature

While I think it's silly but many people feel that it's quite aggressive when someone "requests" changes. Regardless of what you think about it, it's safer to avoid using it whatsoever.

Nowadays I only use it if I think it'll be helpful to the author. For instance "blocking this because we're waiting for team X to deploy their part of the feature".


<a id="org07826b4"></a>

## Cultural diversity - always assume good intentions

People come from all sorts of backgrounds. Even within the same small town two people may have completely different upbringing meaning that they'll have a very different worldview and perception. Something that's clear to me may not be clear to you.

We should not forget that. We should also never assume that we 100% understand what the other person says or means. So if you are going to make an assumption, always assume good intentions.

Just try.


<a id="org58801b9"></a>

# As the PR author

Code review is at least two-sided process. Both author and the reviewer are working together on solving business problems. The author is as important in code review as the reviewers. Here's some advice on being a good PR author


<a id="orgd0d6873"></a>

## Don't take anything personally

Your code is not your child. It's not even your property (it belongs to the company that hired you). You shouldn't be afraid to throw it away when there's a reason for it. And if you make a mistake or there's a much better solution proposed, it's not a big deal. Shit happens. It's not a reflection of you as a professional or a person.

A real-life example. A developer spends a whole day building an over-engineered solution for a simple internal feature and then it turns out they could've achieved pretty much the same by a simple one-line change (not even tests needed) to solve the problem. When it gets pointed out, the person gets defensive and starts advocating for the solution they wrote: "maybe we'll need to change it in the future", "it gives more human-friendly messages in case of errors". It turns into a huge argument and then their solution just gets deployed because it's already written and it's not worth fighting over. A few months later turns out it was bugged. Roll credits.

Don't grow attached to code.


<a id="org4a82d36"></a>

## Don't ignore comments

Don't ignore comments. Answer everything. If something sounds like it's not even worth discussing, just honestly say you'd don't want to discuss this to save time and the person should follow up on that separately (e.g. bring to the team chat or even create a ticket, use your own judgement) if they truly feel strong about this.

Also, github has a feature "Require conversation resolution before merging" in branch protection settings. I think it's quite nice.


<a id="org1addbc6"></a>

## Make sure your PR description and commits are easy to read

Don't you love a commit history that looks like:

1.  Implement <FEATURE>
2.  fix
3.  fix
4.  address comments
5.  linter

Commit history should be a high level description of the changes in its own right. Each of them should contain a meaningful change that can be reviewed on its own. Commit often and make use of `git rebase -i`, `--amend`, and partial committing (your git UI should have that feature or you can use `git add -p`). See my old ["How I use git"](https://nondv.wtf/blog/posts/how-i-use-git.html) post (nowadays I use Magit so some of the advice there may be a bit dated).

Your PR description should provide an overview of the changes as a whole. List any notable changes. The more background you provide, the better it'll be both now (for the reviewers) and in the future. When looking at history, you'll be able to get to pull requests from specific commits they contained.

Your future self and peers will be grateful for a clean commit history.


<a id="org908d22a"></a>

## Review your own work before asking someone else for review

Before notifying anyone that there's a PR ready, open it and review as if it was someone else's work. Maybe even create it as a draft first. Waiting for CI to finish first is a good idea too.

It's really annoying to invest your time looking at half-baked code. Sometimes you would like some feedback on the solution before it's finalised which is fine. But in such cases you should reach out to people directly and walk them through your idea (maybe pair program) instead of creating a PR and telling them to read through everything on their own lonesome.

"Talk is cheap, show me the code" is **not** a quote you should live by. Business requirements and intent should come first and then based on that, you should see if the code satisfies them.


<a id="org2ec31c7"></a>

# Code review questionnaire (PR template)

As promised, here's the PR [template](https://docs.github.com/en/communities/using-templates-to-encourage-useful-issues-and-pull-requests/creating-a-pull-request-template-for-your-repository) I recently introduced within my team. It's quite specific to us and is not polished enough yet. So use it merely as an example.

```
Acts as an overview of the changes and a checklist for reviewers and the author
to make sure important bits (e.g. idempotency) are covered.

Add commentary in **bold**.

- *Are you using any potentially stale data?* It's important to think of race
  conditions, implications of processing existing data (e.g. new consumers for
  existing topics), etc.
  - [ ] Sinks (DB tables).
  - [ ] Consumer accessing historical/existing data.
  - [ ] Nope.
- *Are you updating any public interfaces/API?* E.g. introduce new parameters or
  change the return result. It's important to make sure any users of those
  interfaces are ready for the change (e.g. updated or not affected at all)
  - [ ] Yes
    - [ ] The API is contained within the codebase and usages are checked.
    - [ ] The API is exposed to external apps (via HTTP/Kafka/etc). The apps
      are ready/unaffected by the changes
    - [ ] It's a new public API and has no active users yet.
  - [ ] No public interfaces are affected.
- *Is the logic idempotent?*. This is really important for reactive services
  - [ ] Yes.
  - [ ] No. Explain why
  - [ ] N/A.
- *Side effects*
  - [ ] Kafka messages produced.
  - [ ] DB writes.
  - [ ] Side effects are transactional (i.e. if one step fails, everything
    gets cancelled).
  - [ ] Logging.
  - [ ] Alerts
    - [ ] Honeybadger.
    - [ ] Slack.
    - [ ] Nope.
  - [ ] Nope.
- *Automated tests*
  - [ ] Unit.
  - [ ] Integration (using kafka).
  - [ ] Flow tests.
  - [ ] None of the above (explain why)
- *Manual testing*
  - [ ] Tested locally.
  - [ ] Tested on staging.
  - [ ] Stakeholder (e.g. PM) involved.
  - [ ] Nope.
- *Infrastructure*
  - [ ] DB migrations.
    - [ ] Indexes added/not needed.
    - [ ] Adequate migration performance (no long locks).
    - [ ] No data regression (e.g. if you delete a column, will you be able to
      restore its data easily?). If there is, explain why
    - [ ] Obvious rollback plan. If not, explain why
    - [ ] Backfill required. Add link to follow-up ticket if applicable
  - [ ] New service (pod) added.
  - [ ] New external resource added/used (usually, terraform changes).
    - [ ] Kafka topics.
      - [ ] ACLs set up. Add link to PR
      - [ ] Needs to be in datalake. Add link to follow-up ticket/PR
    - [ ] AWS resource (e.g. Lambda, database, etc). Add details
  - [ ] Nope.

```
