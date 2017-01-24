(ql:quickload "alexandria")
(ql:quickload "cl-glu")
(ql:quickload "cl-glfw3")

(defpackage :js-game
  (:use :cl))
(in-package :js-game)

(load "main.lisp")
