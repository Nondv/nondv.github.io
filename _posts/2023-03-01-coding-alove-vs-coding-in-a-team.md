---
title: "Coding alone vs coding in a team"
categories: ['Weird Programming', 'Functional Programming', 'Clojure']
tags: ['lisp', 'common lisp', 'functional programming', 'fp', 'programming', 'clojure']
img: /img/posts/alone.jpg
img_caption: >-
  <a href="https://www.freepik.com/free-vector/lonely-girl-suffering-from-depression_7732643.htm">Image by pch.vector</a> on Freepik

description: >-
  The excitement of coding just for yourself and by yourself as a professional
  software engineer
---

Recently I've been working on a personal project (a Tinder bot integrated with
ChatGPT and using Telegram Bot API as UI). I started out with a small prototype
in Ruby (like 40 lines script) however then I switched to Common Lisp.

The reason for it was that I wanted to use the language for a while but couldn't
bring myself to (because I **hate** LISP-2 and I also thought and still do that CL
is a language equivalent of a dumpster).

My opinion of the language hasn't changed but I did love the experience. And now
I'm gonna try to explain why.

<!--more-->

# Why is Common Lisp a dumpster and why do I want to learn it?

It was created a long time ago and it was purposed to be a very practical
language you could use for anything. It, basically, *supports* **every**
programming paradigm. It's important to stress that because other languages
usually *let* you code in a different style. For instance, Ruby lets you code
functionally and Clojure lets you code imperatively, however neither actually
supports that (Ruby doesn't even have functions and Clojure makes it hard to
write stateful code).

Common Lisp, on the other hand, has a pretty much imperative, almost low-level,
core with stateful data structures (lots of functions actually modify data in
place) and has a lot of FP stuff on top of it (like `mapcar` and `reduce`). It
also supports typing and OOP (via CLOS, which is pretty interesting by
itself). Interactive programming, duh. It even mixes lexical and dynamic
binding.

Being relatively low-level in its core AND being a Lisp allows CL to be extended
indefinitely. For instance, there's a 3rd-party **library** that introduces
annotations:

{% highlight lisp %}
@export
(defun my-public-fn (x) (+ x 123))
{% endhighlight %}

That sort of flexibility is why I am drawn to CL even though I dislike a lot of
things about it.

Also, this sort of flexibility is the reason why I don't think it's a good
language to use nowadays.


## Clojure - a lisp on the other end of the spectrum

Compared to Common Lisp, Clojure is incredibly opinionated. It pushes you
towards solving a problem in a particular way. It makes it harder for you to
solve problem some other way.

And I think it's one of its greatest strengths. In the modern software
engineering we have to work on large projects with lots of people from different
backgrounds (experience, language, gender, ethnicity, nationality, etc etc etc).
Having some sort of predictability and consistency is great.

Not only many popular languages are opinionated (think of Python vs Perl, they
literally have opposite philosophy: "Explicit is better than implicit" vs
"There's more than one way to do it") but also the industry itself is
opinionated nowadays. The technology used across companies is often similar,
system designs are often the same (how many apps in the "process of splitting
into services" have you seen in the past 5 years?). It's very practical to
follow the same patterns.

I don't like quite a few things about Clojure but I also think it's a much
better language choice for a modern project. I love using it at work. However, I
noticed that I hated using it at home. At home, I don't need (nor want) to write
perfect production-grade code. I want to write code fast and dirty and play
around with different ideas. Clojure made me feel suffocated.


# So what sort of unholy nsfw stuff did I do while coding just for myself?


## Code the way I want and be as inconsistent as I want

One thing I don't like about Clojure is that it's a bit inconsistent with its
function signatures. Some of them are data-first and others are
data-last (personally, I prefer data-last, similar to ML-languages, lodash/fn,
Ramda.js, etc). Because of that, there're two pipelining operators (threading
macros): thread-first (`->`) which inserts intermediate results as a first
argument and thread-last (`->>`) which inserts them as the last
argument. Sometimes (not often but still) it's a bit annoying when I need to use
a data-first function with the thread-last operator.
There's a library called [swiss-arrows](https://github.com/rplevy/swiss-arrows) which has a clever macro that sort of
addresses this issue:

{% highlight clojure %}
(-<> '(1 2 3 2 4)
     ;; by default it's just like thread-last
     (map inc)
     frequencies
     ;; but we can also specify the exact place
     (assoc <> :meta "some description"))
{% endhighlight %}

I really like this but I never actually got to use it in a real project because
people are quite reluctant about bringing language extensions into projects
without a specific benefit (this is merely a coding utility).

Well, in my own project when I discovered that Common Lisp lacks any pipe
operator, I implemented the one from Clojure:

{% highlight lisp %}
@export
(defmacro ->> (x &rest forms)
  (reduce
   (lambda (a e)
     (let ((sexp (if (symbolp e) (list e) e)))
       ;; OPTIMIZE: this is pretty slow
       (reverse (cons a (reverse sexp)))))
   forms
   :initial-value x))
{% endhighlight %}

So far it seems to me that CL mostly follows data-last style. However, I quickly
realised that it's still not helpful sometimes because many functions have
keyword arguments (e.g. `:initial-value` in `reduce` in the example above).

So I decided to just implement this swiss-arrow macro!

{% highlight lisp %}
(defmacro -<> (x &rest forms)
  ;; <Lots of shitty unoptimised code>
  ;; BUT NOBODY CARES!
  )
{% endhighlight %}

Now I use it everywhere throughout my codebase. And nobody can stop me!

I even used dynamic binding:

{% highlight lisp %}
(defvar *connection*)

(defmacro within-same-connection (&rest forms)
  `(dbi:with-connection (*connection* :sqlite3 :database-name +db-filename+)
     ,(cons 'progn forms)))

(defun sql-fetch (query &rest params)
  ;; Not gonna lie, this is shitty because it opens a new connection
  ;; even if it's not gonna need it.
  ;; But this was easier to write at the time so sue me
  (dbi:with-connection (conn :sqlite3 :database-name +db-filename+)
    (let ((connection (if (boundp '*connection*) *connection* conn)))
      (-<> (dbi:prepare connection query)
           (dbi:execute <> params)
           dbi:fetch-all))))
{% endhighlight %}


## Stick to one coding style. But only when it's convenient

In the recent years I developed a taste for functional style of coding. I love
OOP but I prefer writing functional code as I think it's more laconic and
simpler than OO-style equivalent. With experience in Clojure, Elm and functional
JS and Ruby I started appreciating immutable data structures and variables. So
this is the way I mostly code:

-   I avoid changing state
-   I don't use mutating functions (CL has a pretty convenient naming convention
    where functions that mutate data in-place end with `f` suffix, e.g. `setf`,
    `incf`)
-   I use only simple data structures (lists and cons cells)
-   Etc etc

However, for myself, I don't feel bad about "breaking" the rules and being a bit
naughty:

{% highlight lisp %}
(let ((last-msg-sent-at 0))
  (loop
    (dolist (user (db:get-all-active-users))
      (let ((messages-sent (answer-messages user)))
        (when (> messages-sent 0)
          ;; Yes, this changes the var declared in `let`!
          (setf last-msg-sent-at (get-universal-time)))))

    (let ((seconds-passed (- (get-universal-time) last-msg-sent-at)))
      (if (< 900 seconds-since-last-msg)
          (progn
            (format t "No activity. Taking a break~%")
            (sleep 300))
          (sleep 10)))))
{% endhighlight %}

I'm absolutely sure there's a way to write this code in a more elegant way that
would fit into my usual coding style. However! I didn't come up with it off the
top of my head and I was focused on prototyping. Right now, I don't care about
this piece of code. I have more interesting stuff to do. And there's nobody else
whom this code could screw over.

Sometimes I'd use objects over raw data:

{% highlight lisp %}
@export-accessors
(defclass <user> ()
  ((id :reader id :type string :initarg :id)
   (name :reader name :type string :initarg :name)
   (bio :reader bio :type string :initarg :bio)
   (photos :reader photos :type list :initarg :photos)
   (distance-mi :reader distance-mi :type integer :initarg :distance-mi)
   (verified :reader verified :type boolean :initarg :verified)
   (in-travel-mode :reader in-travel-mode :type boolean :initarg :in-travel-mode)
   (_original-data :initarg :_original-data)))

(defun recommendation->user (rec)
  (make-instance
   '<user>
   :id (alist-dig '("user" "_id") rec)
   :name (alist-dig '("user" "name") rec)
   :bio (alist-dig '("user" "bio") rec)
   :photos (->> rec
                (alist-dig '("user" "photos"))
                (mapcar (lambda (x) (alist-get "url" x))))
   :distance-mi (alist-get "distance_mi" rec)
   :verified (-<> rec
                  (alist-dig '("user" "badges"))
                  (find-if (lambda (x) (string= "selfie_verified" (alist-get "type" x))))
                  (when <> t))
   :in-travel-mode (alist-dig '("user" "is_traveling") rec)
   :_original-data rec))

{% endhighlight %}

This was done for three reasons:

1.  I wanted to simplify the response from Tinder API. So instead of querying the
    raw data in every function I just created one mapper that'll pack it all in a
    convenient structure.
2.  I didn't want to deal with large data blobs because they don't printout
    well.
3.  I kinda felt like using objects. In this particular case this is more of a
    struct though.


## Don't follow community/industry standards

At some point I needed some persistent data. My first thought was "oh that's
easy, I'm just gonna start running my app in a Docker container and setup
PostreSQL in another one". But while I was trying to remember how to write
dockerfiles, it hit me "why the hell do I need docker and postgres?". I just
found a library that supported SQLite and used that. When I wasn't happy with
the recommended library (it was a ROM lib), I just used a different, simpler
one. And it was enough. I don't wanna spend time setting up docker. I wanna
write my app. And by the way, I didn't write any tests. Why? Because fuck you,
that's why. I don't need to set a high bar. I *just* wanna swim in my own crap.

Later, I needed to extend the data. How do we do that usually? We bring a
migration library (or use the one from the framework used, if there's one).Then
we setup a deployment pipeline. Then we just write a bunch of migration files.

Well, no, to hell with that. I'm not doing that shit for my personal small
app. But I did need migrations and I wanted them to be more or less safe.

So I just wrote *my own* migration solution:

{% highlight lisp %}
(defun init ()
  (sql-exec "CREATE TABLE IF NOT EXISTS migrations (id TEXT PRIMARY KEY)")

  (labels ((migrated? (migration-id)
             (sql-fetch "SELECT * FROM migrations WHERE id=?" migration-id))
           (migrate (migration-id &rest queries)
             (when (not (migrated? migration-id))
               (within-same-connection
                (sql-exec "BEGIN TRANSACTION")
                (dolist (quer queries)
                  (apply #'sql-exec (if (listp quer) quer (list quer))))
                (sql-exec "INSERT INTO migrations(id) VALUES(?)" migration-id)
                (sql-exec "COMMIT")))))

    (sql-exec "CREATE TABLE IF NOT EXISTS users (
               tg_chat_id TEXT PRIMARY KEY,
               tg_chat_state TEXT NOT NULL,
               tg_username TEXT NOT NULL,
               tinder_token TEXT
             )")

    (migrate "001"
             "ALTER TABLE users ADD turned_on INTEGER DEFAULT 0"
             "UPDATE users SET turned_on = 1")

    (migrate "002"
             "ALTER TABLE users ADD tinder_msg_limit INTEGER DEFAULT 3"
             "UPDATE users SET tinder_msg_limit = 3")
    ;; ...

    (migrate "007"
             "ALTER TABLE users ADD chatgpt_identity TEXT DEFAULT 'human'"
             "UPDATE users SET chatgpt_identity = 'human'")))
{% endhighlight %}

And it works! I don't even know what the recommended migration library in CL
is. And I don't fucking care!


# In conclusion

I haven't coded anything for myself in years. And I must say, it feels so
bloody liberating. I've been coding professionally for over like 10 years
now and all this time I was honing my skills to make myself a professional
software engineer. And it meant doing things in a certain way to make sure the
code quality is up to standard. Somewhere along the line I lost the passion I
once had for coding. Back then it was a loved hobby and now it's just a tool for
me to keep being paid. A very rigid tool at that.

P.S. noticed how I started swearing towards the end of this essay? It's because
I actually felt exhilaration going through all this wonderful experience in my
head. I finally feel like a programmer again, not a Senior Software Engineer.
