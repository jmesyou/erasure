(ns clj-whitespace.programs
    (:gen-class))

(def hello-world "   \t      \t \n   \t\t\t \t\t   \t\t  \t \n    \n\t\t    \t  \t   \n\t\n     \t\t  \t \t\n\t\n     \t\t \t\t  \n \n \t\n  \t\n     \t\t \t\t\t\t\n\t\n     \t     \n\t\n     \t \t \t\t\t\n\t\n     \t\t \t\t\t\t\n\t\n     \t\t\t  \t \n\t\n     \t\t \t\t  \n\t\n     \t\t  \t  \n\t\n     \t    \t\n\t\n     \t \t \n\t\n  \n\n\n\n    ")

(def count '([:push 1] [:label 0] :dup :printi [:push 10] :printc [:push 1] :add :dup [:push 11] :sub [:jz 1] [:jmp 0] [:label 1] :pop :end))