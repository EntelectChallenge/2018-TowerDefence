(load "bot.asd")

(ql:quickload :bot)

(save-lisp-and-die "bot.exe" :toplevel #'bot:take-turn :executable t)
