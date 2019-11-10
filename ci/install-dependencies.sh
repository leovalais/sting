#!/usr/bin/env bash
set -e

make_quickloads() {
    for dep in "$@"; do
        echo "(ql:quickload \"$dep\")"
    done
}

script=$(mktemp)
cat >$script <<EOF
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
#+sbcl (setf sb-impl::*default-external-format* :utf-8
             sb-alien::*default-c-string-external-format* :utf-8)
$(make_quickloads "$@")
EOF
$LISP_EXEC $script
