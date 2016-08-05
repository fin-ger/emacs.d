#! /bin/bash -e

echo "Updating git repository"

(
    git fetch && git merge --ff-only && git submodule update --init --rebase --recursive --remote
) > /dev/null 2>&1 || echo "Failed to update git repository"

echo "Installing typescript-tools locally"

(
    cd typescript-tools
    npm install --prefix .
    make
) > /dev/null 2>&1 || echo "Failed to install typescript tools"

BOOTSTRAP=";;;;;;;;;;;;;;;;;;;
;; SETUP FINMACS ;;
;;;;;;;;;;;;;;;;;;;
(setq custom-file \"~/.emacs.d/lisp/fin-macs.el\")
(load custom-file)
"

if ! grep -i "finmacs" ~/.emacs > /dev/null 2>&1
then
    echo "Bootstrapping emacs"
    echo "${BOOTSTRAP}" >> ~/.emacs
fi

echo "Installing FinMacs desktop file"
mkdir -p ~/.local/share/applications/
cp finmacs.desktop ~/.local/share/applications/

echo
echo "You may add the following line to your local shell rc file:"
echo "    alias emacs='emacs -fh -g 153x74'"
echo

echo "For typescript-tools to work properly please write the following lines to /usr/bin/tss"
echo
echo "#! /bin/bash"
echo '/home/<your-user>/.emacs.d/typescript-tools/bin/tss --module amd $@'
