(in-package :cl-user)

(defpackage :bot.sys
  (:use :asdf :cl))

(in-package :bot.sys)

(defsystem :bot
  :name "Bot"
  :author "Henry Steere"
  :version "0.0.1"
  :maintainer "henry.steere@gmail.com"
  :license "BSD"
  :description "Entellect challenge sample bot"
  :long-description "Sample bot for the Entellect Challenge using a random strategy."
  :depends-on (:yason :alexandria :cl-ppcre)
  :components ((:file "package")
               (:file "parsing" :depends-on ("package"))
               (:file "state" :depends-on ("package" "parsing"))
               (:file "bot" :depends-on ("package" "parsing" "state"))))
