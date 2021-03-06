---

title:  'Используем Proc как хеш'
categories: development
tags: [ruby, proc, notes]
---

## Intro

Полгода назад придумал "хак" и сегодня воспользовался им вновь.

Что такое хеш? В ruby `Hash` - это структура данных, которая позволяет хранить
пары ключ-значение. Но что, если мы хотим, чтобы ключ "совпадал" по нескольким
условиям? В моем случае, это попадание в диапазон значений (в т.ч. дробных).

<!--more-->

Полагаю, что можно заморочиться и сделать свой `class MyHash < Hash` или создать
класс ключей с перегруженным `#==`. Но есть способ проще: использовать `Proc`.

## Способ

У `Proc` есть разный сахар для `#call`, среди прочих - `#[]`.

```ruby
sqr = ->(x) { x * x }
sqr[4] # ==> 16
```

А давайте переименуем `sqr` в `squares`:

```ruby
squares = sqr
squares[4] # ==> 16
```

Если не знать, что такое `sqr`, то вполне можно подумать, что это хеш.

## Мой сегодняшний кейс

Задача (упрощенная): Сопоставить цену доставки с весом посылки и
"зоной тарификации".

В итоге вместо написания метода, я решил загнать все это в большую константу.
Как-то так:

```ruby
PRICES = {
  zone1: lambda do |weight|
           case weight
           when 0..1000    then 123
           when 1001..1500 then 321
           when 1501..2000 then 456
           end
         end,
  zone2: lambda do |weight|
           case weight
           when 0..1000    then 123
           when 1001..1500 then 321
           when 1501..2000 then 456
           end
         end,
   zone51: ...
}.freeze
```

Итого, зная зону тарификации и вес мы получаем стоимость вот так:

```ruby
tariff_zone = :zone2
weight = 458.5
PRICES[tariff_zone][weight]
```

## Проблемы

Все это просто для удобного доступа.

Очень серьезной проблемой это может стать, если пользователь кода
действительно решит, что это хеш и начнет пытаться обходить ключи и пр.

Смысл этого "хака" в том, чтобы не плодить множество методов,
а использовать структуры данных (если proc можно к ним отнести) языка.

Мне кажется, что определение константы `PRICES` выглядит очень наглядно,
т.к. в ней почти нет никакой императивной логики, а `case` с диапазонами
воспринимается как нечто декларативное, ИМХО.
