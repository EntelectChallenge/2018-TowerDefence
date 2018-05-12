(load "bot.asd")

(ql:quickload :bot)

(save-lisp-and-die "bot" :toplevel #'bot:take-turn :executable t)
