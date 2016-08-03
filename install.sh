#! /bin/bash -e

git fetch && git merge --ff-only && git submodule update --init --rebase --recursive --remote

(
    cd typescript-tools
    npm install --prefix .
    make
)

BOOTSTRAP=";;;;;;;;;;;;;;;;;;;
;; SETUP FINMACS ;;
;;;;;;;;;;;;;;;;;;;
(load-file \"~/.emacs.d/lisp/fin-macs.el\")
"

if ! grep -i "finmacs" ~/.emacs
then
    echo "${BOOTSTRAP}" >> ~/.emacs
fi
