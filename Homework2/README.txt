                        ━━━━━━━━━━━━━━━━━━━━━━━━
                                 README


                        ━━━━━━━━━━━━━━━━━━━━━━━━


Table of Contents
─────────────────

1 How to run the code


1 How to run the code
═════════════════════

  • GET the SBCL compiler
  • Load quicklisp
    ⁃ _Instructions are as follows_
      ‣ curl -O [https://beta.quicklisp.org/quicklisp.lisp]
      ‣ curl -O [https://beta.quicklisp.org/quicklisp.lisp.asc]
      ‣ gpg –verify quicklisp.lisp.asc quicklisp.lisp
      ‣ sbcl –load quicklisp.lisp
      ‣ (quicklisp-quickstart:install)
    ⁃ And for future reference, just run this, and you won't ever need
      to load quicklisp.lisp again
      ‣ (ql:add-to-init-file)
    ⁃ Further instructions can be gotten here
      [https://www.quicklisp.org/beta/]
  • Load my file twice
    ⁃ This can be done with (load #P"homework2.lisp"), the first time
      should signal some warnings due to the order of some of my
      functions, for readability sake, I order it nicely, so just run
      that command twice
  • You are all set!
