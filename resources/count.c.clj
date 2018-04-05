'([:push 1]
 [:label 67]
 :dup
 :printi
 [:push 10]
 :printc
 [:push 1]
 :add
 :dup
 [:push 11]
 :sub
 [:jz 69]
 [:jmp 67]
 [:label 69]
 :pop
 :end)
