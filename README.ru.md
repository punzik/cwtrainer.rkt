# CW (Morse code) Trainer. Koch method

*Read this in other languages: [English](README.md), [Русский](README.ru.md).*

Программа предназначена для помощи в изучении азбуки Морзе методом Коха.

Описание писать лень (может быть позже), если кому-то будет интересно,
просто запустите и попробуйте.

## Требования

* Linux
* Racket
* PulseAudio

## Запуск

Программа консольная, по этому запускается из терминала:

    $ racket ./cwtrainer.rkt

Если хочется просто послушать текст или случайный набор букв, то:

    $ racket ./cwtrainer.rkt listen

или

    $ racket ./cwtrainer.rkt listen "Privet ot staryh stiblet"

## Настройки

Конфигурационные настройки программы хранятся в файле `cwtrainer.conf`.
