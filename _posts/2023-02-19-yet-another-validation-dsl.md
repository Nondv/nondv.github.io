---
title: "Yet another validation DSL"
categories: ['Ruby', 'Clojure', 'Weird Programming']
tags: ['DSL', 'validation']
img: /img/posts/gear-lightbulb.jpg
description: >-
  A validation DSL generator based on pure functions and data.
---


I've been reading my old essays and found [this one](https://habr.com/en/post/479600/) written in Russian back
in 2019.

Essentially, it describes a Ruby [gem](https://github.com/Nondv/st_validation.rb) (library) I had written that allows you to
create your own validation DSL as a bunch of functions and data. It emphises
simplicity and flexibility. I thought it had a really interesting idea behind it
so I'm going to rehash it here.

The below can be applied to any language (admittedly, it'll be less convenient
in some). I wrote examples in Ruby and Clojure.

<!--more-->


# Why I thought yet another validation DSL was a good idea

Modern validation libraries usually involve a lot of stuff. A **lot** of
stuff. You need to learn a bunch of library functions, read docs, sometimes even
a whole DSL. Also, they can be quite opinionated.

Now, to be completely honest with you, nowadays I don't think it's a bad
thing. But it's always nice to play around with ideas.


# Fundamentals

The goals I was aiming for with that library were:

-   **Simplicity**
-   No magic
-   Easy to learn
-   Easy customisation with as little limitations as possible

Basically, I wanted to make it as simple as possible.


# Architecture

I based the library on 4 concepts:

-   **Validator** - object (could be a function) that validates data. Is built
    from a *blueprint* and *transformations*.
-   **Blueprint** - data that describes data we want to validate. For instance,
    `:int` could represent an integer and `{name: :string, email: :email, age:
      :positive}` could represent a user.
-   **Transformation** - a function `t(b, f)` where `b` is a blueprint and `f` is
    a validator factory (details below) that returns either a validator or a
    blueprint. This is the core concept that I find interesting. Basically, a list
    of transformations describes your DSL.
-   **Factory** is built from a list of transformations. It's a function (well, an
    object in the library) that takes a *blueprint* and passes it through all
    transformations (continuously!) until a validator is created. Factory gets
    stuck in an infinite loop, the schema is invalid (well, or the
    transformations). The gem isn't handling this very well though - it merely
    checks if the blueprint stays the same after a full transformation cycle.

All that is enough to *build your own* validation DSL.


# Factory implementation


## Ruby

{% highlight ruby %}
def build_factory(transformations)
  factory = lambda do |blueprint|
    result = blueprint
    log = [blueprint]
    until result.is_a?(Proc)
      result = transformations.reduce(result) { |r, t| t.call(r, factory) }
      # ensure the transformation didn't loop
      raise "Can't process blueprint " if log.any? { |bp| bp == result }
      log << result
    end

    result
  end

  factory
end
{% endhighlight %}


## Clojure

{% highlight clojure %}
(defn build-factory [transformations]
  (letfn [(factory [blueprint]
            (loop [result blueprint, processed #{blueprint}]
              (if (fn? result)
                result
                (let [next-result (reduce #(%2 %1 factory) result transformations)]
                  (when (contains? processed next-result)
                    (throw (Error. "Can't process blueprint")))
                  (recur next-result (conj processed next-result))))))]
    factory))
{% endhighlight %}


# DSL example


## Ruby

{% highlight ruby %}
my_dsl_transformations = [
  ->(bp, f) { bp.is_a?(Class) ? ->(x) { x.is_a?(bp) } : bp },
  lambda do |bp, f|
    return bp unless bp.is_a?(Array) && bp.size == 1
    element_validator = f.call(bp[0])
    ->(x) { x.is_a?(Array) && x.all?(&element_validator) }
  end,
  lambda do |bp, f|
    return bp unless bp.is_a?(Set) && !bp.empty?
    validators = bp.map(&f)
    ->(x) { validators.any? { |v| v.call(x) } }
  end,
  lambda do |bp, f|
    return bp unless bp.is_a?(Hash)
    validators = bp.map { |k, v| [k, f.call(v)] }
                   .map { |k, v| ->(x) { v.call(x[k]) } }
    ->(x) { x.is_a?(Hash) && validators.all? { |v| v.call(x) } }
  end
]
my_dsl = build_factory(my_dsl_transformations)
user_validator = my_dsl.call(
  name: String,
  age: ->(x) { x.is_a?(Integer) && x >= 18 && x < 150 },
  favourite_food: [String],
  phone: Set[NilClass, String]
)

user_validator.call(
  name: "John Doe",
  age: ->(x) { x.is_a?(Integer) && x >= 18 && x < 150 },
  favourite_food: ["Lasagna", "Borsch"]
)
# ==> true
{% endhighlight %}


## Clojure

{% highlight clojure %}
(defn hashmap-subset-transformation [blueprint factory]
  (if-not (map? blueprint)
    blueprint
    (let [validators (->> blueprint
                          (map (fn [[k bp]] [k (factory bp)]))
                          (map (fn [[k v]] #(v (get % k)))))]
      (fn [x] (and (map? x) (every? #(% x) validators))))))

(def my-dsl-transformations
  [(fn [bp f] (if-not (class? bp)
                bp
                #(instance? bp %)))
   (fn [bp f] (if-not (and (vector? bp) (= 1 (count bp)))
                bp
                #(and (vector? %) (every? (f (first bp)) %))))
   (fn [bp f] (if-not (and (set? bp) (not (empty? bp)))
                bp
                (let [validators (map f bp)]
                  (fn [x] (boolean (some #(% x) validators))))))
   hashmap-subset-transformation])

(def my-dsl (build-factory my-dsl-transformations))

(def validate-user
  (my-dsl
    {:name String
     :age #(and (int? %) (>= % 18) (< % 150))
     :favourite-food [String]
     :phone #{nil? String}}))

(validate-user {:name "John Doe"
                :age #(and (int %) (>= % 18) (< % 150))
                :favourite-food ["Lasagna" "Borsch"]})
;; ==> true
{% endhighlight %}


# Conclusion

This is just a simple example (a prototype, if you will). Some benefits of
approach like this:

-   The core is minimal so it's easy to learn
-   Because each DSL is created with pure functions, they can easily be re-used
-   It's very portable as no special language features are required. Any
    functional or OOP language can have this.

My particular example simply returns `true/false` but with some tuning it can
return explanations for validation failures.
