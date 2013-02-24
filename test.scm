;; A quick test to detect typos

(use gauche.test)

(define-macro (test-file-binding name)
  `(begin
     (define-module ,name)
     (load ,#`"./,|name|.scm" :environment (find-module ',name))
     (test-module ',name)))

(test-start "Checking bindings")

(test-file-binding guess)
(test-file-binding wizards_game)
(test-file-binding graph-util)
(test-file-binding wumpus)
(test-file-binding orc-battle)
(test-file-binding evolution)
(test-file-binding robots)
(test-file-binding webserver)
(test-file-binding dice_of_doom_v1)
(test-file-binding svg)
(test-file-binding wizard_special_actions)
(test-file-binding dice_of_doom_v2)
(test-file-binding dice_of_doom_v3)
(test-file-binding dice_of_doom_v4)

(test-end)

