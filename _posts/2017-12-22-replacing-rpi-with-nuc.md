---

title:  'Как я Raspberry Pi на Intel NUC заменил'
categories: development
tags: [linux, mpd, rpi, nuc, notes]
---

## Intro

Достаточно долгое время я использовал Raspberry Pi в качестве домашней
"песочницы" (думаю, наши англоязычные коллеги называют это "home lab").

На ней крутился MPD, тянущий музыку с NAS, и мои всякие мелкие проектики,
например, веб-морда и [телеграм-бот](https://github.com/Nondv/mpc_telegram_bot)
для этого самого MPD.

Вчера я заменил малину на Intel NUC 6-го поколения с Celeron.

<!--more-->

## Зачем?

### Архитектура процессора

Малина (rpi) использует ARM процессор. Это порождает некоторые проблемы с
совместимостью софта. Какие-то пакеты приходилось искать специально
скомпилированные под arm/rpi, либо компилировать самостоятельно, что было очень
длительным и неприятным процессом.

Даже в докере не все образы доступны. Например, Alpine на rpi не пашет, и это
очень печально, т.к. alpine-образы намного легче того же slim. В общем,
использование интеловской i386 архитектуры заметно упрощает жизнь.

### Производительность и кастомизация

Здесь я не уверен и тесты не проводил, но я полагаю, что даже нищебродский
Celeron гораздо производительнее ARM с близкой к нему частотой. Однако общая
производительность в любом случае заметно улучшилась не только из-за процессора.

Я купил Intel NUC BOXNUC6CAYH, одну планку оперативы на 4 гигабайта и SSD на 120
гигабайт. Не уверен насчет оперативной памяти, но SSD явно быстрее чем сраная
карта памяти на 8/16 гигов.

В сухом остатке я все еще могу добавить вторую планку оперативы и планку памяти
M.2 (например, Intel Optane), которая должна еще сильнее ускорить работу
системы, а SSD превратить просто в свалку данных (например, можно монтировать
его как /var/log). Кстати, на малине я месяца 2 назад уперся в нехватку
памяти. Приходилось руками удалять всякие архивы логов (так и не смог заставить
systemd удалять логи), старые контейнеры, образы, исходники и пр.

Идеально было бы конечно купить нюк последнего (7-го) поколения с i5/i7, но я
нищеброд.

### Разделение полномочий

Хоть я ее и заменил, но Raspberry Pi все еще в моем распоряжении. Подумываю о
том, чтобы приспособить ее для голосовых команд.

## Процесс настройки


### Операционная система

Чтобы не париться, решил установить Xubuntu 16.04 (вообще-то, хотел Ubuntu
17.10, но у них накануне произошел [коллапс](https://geektimes.ru/post/296645/)
и образы были недоступны).

Почему не Ubuntu Server? Мне, в общем-то, не нужен серверный софт (кроме ssh),
хотелось иметь доступ к GUI и не хотелось заморачиваться с процессом настройки
(хотя мож у сервера тоже он простой).

После установки можем обновиться:

```bash
sudo apt upgrade
```

### Сервер ssh

```bash
sudo apt install openssh-server
```

### Отключение графики


```
sudo systemctl set-default multi-user.target
```

У меня при этом возникает проблема:
после перезагрузки у меня просто черный экран, нет запроса логина. Это решается
переключением на какую-нибудь консоль (`ctr + alt + F1`).
[Тут](https://askubuntu.com/questions/800239/how-to-disable-lightdmdisplay-manager-on-ubuntu-16-0-4-lts)
пишут, что еще нужно поправить конфиг GRUB. Скорее всего, это решит проблему.

Чтобы запустить xfce достаточно набрать `startx`. Согласно ссылке выше unity
запускается по-другому (но в жопу юнити)

### Interlude

Основная настройка завершена (тремя командами, хе-хе).
Отключаем монитор, мышь, клавиатуру, перезагружаемся и мы "all set". Wi-fi
подключится автоматически к тем сетям, что были подключены еще  в графической
среде. Кстати, следует не забыть зафиксировать IP нюки в роутере.

Дальше можно не читать.

### Всякие полезности

```bash
sudo apt install emacs tmux
```

### MPD

```
sudo apt install mpd
```

Далее открываем конфиг `/etc/mpd.conf` и правим все нужное.

#### Звук

Звук работает, но MPD не может его настроить. Логи выдают проблемы с dbus и
pulseaudio. Как я понял, связаны они с тем, что мы работаем без графики.

Погуглив, я не нашел простого решения, поэтому сделал такую настройку на аудио:

```
audio_output {
  type        "alsa"
  name        "audio output"
  mixer_control "PCH"
  mixer_type  "software"
}
```

Таким образом MPD будет программно менять звук (а не через альсу).

Далее просто настроил звук в альсе, чтобы он был с нулевым усилением:

```bash
sudo alsamixer
sudo alsactl store # сохранит настройки в конфиг и будет применять их после перезагрузки
```

### Docker

```bash
sudo apt install docker docker-compose
sudo usermod -aG docker $USER
```

Добавление текущего юзера в группу docker позволит ему управлять докером без sudo
([док](https://docs.docker.com/engine/installation/linux/linux-postinstall/)).

#### Запуск контейнеров

Раньше я создавал юниты для systemd, но оказалось, что докер сам может запускать
все нужное после рестарта - достаточно использовать `--restart always`.

Так же я для своих сервисов написал скриптики, которые обновляют и их до
последней версии образа. Вот пример одного из них:

```ruby
#!/usr/bin/env ruby

IMAGE_NAME = "nondv/mpc_bot:latest"
CONTAINER_NAME = 'mpc_bot'

puts 'updating image...'
pull_output = `docker pull #{IMAGE_NAME}`

def exec(sh_command)
  puts "Exec: #{sh_command}"
  system sh_command
end

def start_new_container
  exec "docker run -e TOKEN=<SECRET> -itd --network host --restart always --name #{CONTAINER_NAME} #{IMAGE_NAME}"
end

if pull_output =~ /up to date/
  puts "Image #{IMAGE_NAME} is already up to date"
  started = exec "docker start #{CONTAINER_NAME}"
  start_new_container unless started
else
  exec "docker stop #{CONTAINER_NAME}"
  exec "docker rm #{CONTAINER_NAME}"
  start_new_container
end

```

Можно даже повесить их на крон.

## Outro

Вот так я и мигрировал с Raspberry Pi на Intel NUC.
Пока что всем доволен - быстродействие, отсутствие проблем из-за архитектуры, да
и звук стал чище и громче.
