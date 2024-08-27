#lang racket
(require "parenthec.rkt")
(require racket/trace)
#| Assignment 9: ParentheC Interpreter |#

;; Code should run as fast as necessary, but no faster; something
;; important is always traded away to increase speed.
;;
;; Richard Pattis

;; This, but un-ironically:
;; https://www.reddit.com/media?url=https%3A%2F%2Fi.redd.it%2Ffe96ionbykj01.jpg

#| ===== Assignment Guidelines ===== |#


;; This assignment relies on your successful completion of hw7. If you
;; haven't successfully completed hw7 and maintained the versions of
;; your code along the way, please completely correct that before
;; starting this assignment. You will be much happier.

;; Your assignment is to complete the transformation of your
;; interpreter from hw7 to a version we can translate to C.

;; When your interpreter is complete, turn it into C programs using
;; pc2c, and run the test program provided.

;; Save a new copy of your interpreter after you finish every
;; step. **We will expect you to have all of these intermediate files
;; available during your demonstration**. Also, you will likely need
;; at some point to go back to an older version of your interpreter to
;; correct a mistake; having copies of all previous steps will save a
;; lot of time.

;; You should turn in the `interp.pc` file that contains the exact
;; code you used to generate your C programs.

;; Once you've done the assignment, you must meet for an ~15m time
;; slot with the instructor or one of the TAs to demonstrate your
;; knowledge of your code. We will schedule meeting times, likely
;; during office hours, the week of April 5th to April 9th.

;; **You cannot receive a grade for this course until you complete this
;; assignment and demonstrate your understanding with one of the
;; instructors.**

;; Since you must successfully complete this assignment and the
;; corresponding code review in order to receive a passing grade for
;; this course (which is to say, this being a must-pass assignment)
;; this is the one assignment we /will/ take late. For assignments
;; handed in after the due date, credit is reduced proportionally to
;; lateness (dropping ~2 points/week).

;; If you haven't done so, consider reading the ParentheC paper,
;; "Using ParentheC to Transform Scheme Programs to C or How to Write
;; Interesting Recursive Programs in a Spartan Host". It is slightly
;; out of date viz. registerization, but can still prove a useful
;; resource.

;; Download pc2c.rkt and parenthec.rkt

;; You will also need to use the following define-union for
;; expressions and main program:

(define-union expression
  (const cexp)
  (var n)
  (if test conseq alt)
  (mult nexp1 nexp2)
  (sub1 nexp)
  (zero nexp)
  (letcc body)
  (throw kexp vexp)
  (let exp body)
  (lambda body)
  (app rator rand))
  
;; (let ((f (lambda (f)
;;            (lambda (n)
;;          (if (zero? n)
;;          1
;;              (* n ((f f) (sub1 n))))))))
;;   (* (letcc k ((f f) (throw k ((f f) 4)))) 5))

;; The above program, translated
;; Notice that this test program is not quoted data.

;; (define main
;;   (lambda ()
;;     (value-of-cps
;;      (expression_let
;;       (expression_lambda
;;        (expression_lambda
;;         (expression_if
;;          (expression_zero (expression_var 0))
;;          (expression_const 1)
;;          (expression_mult (expression_var 0) (expression_app (expression_app (expression_var 1) (expression_var 1)) (expression_sub1 (expression_var 0)))))))
;;       (expression_mult
;;        (expression_letcc
;;         (expression_app
;;          (expression_app (expression_var 1) (expression_var 1))
;;          (expression_throw (expression_var 0) (expression_app (expression_app (expression_var 1) (expression_var 1)) (expression_const 4)))))
;;        (expression_const 5)))
;;      (envr_empty-env)
;;      (kt_empty-k))))

(define main
  (lambda ()
    (value-of-cps
     (expression_app
      (expression_lambda
       (expression_app (expression_var 0)
                       (expression_lambda (expression_mult (expression_var 0) (expression_const 2)))))
      (expression_lambda (expression_app (expression_var 0) (expression_const 3))))
     (envr_empty-env)
     (kt_empty-k))))


#| ===== Project Part II ===== |#

#|

1. Here are the steps you will need to accomplish:

  1. Copy the relevant parts (Part II) of your fully correct version
     of your final product from assignment 7 into this file where
     indicated. Do not copy over the tests, however. Copy `parenthec.rkt`
     into the same directory as this file. Change the match-expression in
     value-of-cps to instead be a union-case-expression. Consult the
     ParentheC paper or the example from class to see how to do this. Make
     sure to remove the backquotes and commas in the patterns of what was
     your match expression. Ensure `main` is below your interpreter, and
     make sure it returns 120 when you invoke it.
|#



#|
  2. Transform your closure constructor to a define-union, change the
     match in apply-closure to instead use union-case, and ensure that your
     constructor invocations are preceeded with clos_, or something other
     than clos if you use a different name for your union. Make sure to
     remove the backquotes and commas in the patterns in what was your
     match expression.
|#

(define-union clos
  (make-closure body env-cps k^))



  
#|
  3. Transform your environment constructors to a define-union, change
     the match in apply-env to instead use union-case, and ensure all
     constructor invocations are preceeded with envr_, or something other
     than envr if you use a different name for your union. Make sure to
     remove the backquotes and commas in the patterns in what was your
     match expression.
 |#

(define-union envr
  (empty-env)
  (extend-env a^ env-cps^))



#|
  4. Transform your continuation constructors to a define-union,
     change the match in apply-k to instead use union-case, and ensure all
     constructor invocations are preceeded with kt_, or something other
     than kt if you use a different name for your union. Make sure to
     remove the backquotes and commas in the patterns in what was your
     match expression.
 |#

(define-union kt
  (empty-k)
  (mult-outer-k v^ k^)
  (mult-inner-k nexp2^ env-cps^ k^)
  (sub1-k k^)
  (zero?-k k^)
  (if-k conseq^ alt^ env-cps^ k^)
  (throw-k v-exp^ env-cps^)
  (let-k body^ env-cps^ k^)
  (app-outer-k a^ k^)
  (app-inner-k rator^ env-cps^ k^))



#|
  5. Transform all your serious function calls to our A-normal form
     style, by adding let* above your serious calls, and ensuring that the
     names of the actual parameters to the serious calls are *exactly* the
     names of the formal parameters in the definition.
 |#

(trace-define (apply-env env-cps y k)
  (union-case env-cps envr
              [(extend-env a^ env-cps^)
               (if (zero? y)
                   (let* ((v a^))
                     (apply-k k v))
                   (let* ((env-cps env-cps^)
                          (y (sub1 y)))
                     (apply-env env-cps y k)))]
              [(empty-env)(error 'value-of "unbound identifier")]))

(define (apply-closure c-cps a k)
  (union-case c-cps clos
              [(make-closure body env-cps^ k^)
               (let* ((expr body)
                      (env-cps (envr_extend-env a env-cps^)))
                 (value-of-cps expr env-cps k))]))

(define (apply-k k v)
  (union-case k kt
              [(empty-k) v]
              [(mult-outer-k v^ k^)
               (let* ((k k^)
                      (v (* v^ v)))
                 (apply-k k v))]
              [(mult-inner-k nexp2^ env-cps^ k^)
               (let* ((expr nexp2^)
                      (env-cps env-cps^)
                      (k (kt_mult-outer-k v k^)))
                 (value-of-cps expr env-cps k))]
              [(sub1-k k^)
               (let* ((k k^)
                      (v (sub1 v)))
                 (apply-k k v))]
              [(zero?-k k^)
               (let* ((k k^)
                      (v (zero? v)))
                 (apply-k k v))]
              [(if-k conseq^ alt^ env-cps^ k^)
               (if v
                   (let* ((expr conseq^)
                          (env-cps env-cps^)
                          (k k^))
                     (value-of-cps expr env-cps k))
                   (let* ((expr alt^)
                          (env-cps env-cps^)
                          (k k^))
                     (value-of-cps expr env-cps k)))]
              [(throw-k v-exp^ env-cps^)
               (let* ((expr v-exp^)
                      (env-cps env-cps^)
                      (k v))
                 (value-of-cps expr env-cps k))]
              [(let-k body^ env-cps^ k^)
               (let* ((expr body^)
                      (env-cps (envr_extend-env v env-cps^))
                      (k k^))
                 (value-of-cps expr env-cps k))]
              [(app-outer-k a^ k^)
               (let* ((c-cps v)
                      (a a^)
                      (k k^))
                 (apply-closure c-cps a k))]
              [(app-inner-k rator^ env-cps^ k^)
               (let* ((expr rator^)
                      (env-cps env-cps^)
                      (k (kt_app-outer-k v k^)))
                 (value-of-cps expr env-cps k))]))

(define value-of-cps
  (lambda (expr env-cps k)
    (union-case expr expression
                [(const cexpr)
                 (let* ((v cexpr))
                   (apply-k k v))]
                [(mult nexp1 nexp2)
                 (let* ((expr nexp1)
                        (k (kt_mult-inner-k nexp2 env-cps k)))
                   (value-of-cps expr env-cps k))]
                [(sub1 nexp)
                 (let* ((expr nexp)
                        (k (kt_sub1-k k)))
                   (value-of-cps expr env-cps k))]
                [(zero nexp)
                 (let* ((expr nexp)
                        (k (kt_zero?-k k)))
                   (value-of-cps expr env-cps k))]
                [(if test conseq alt)
                 (let* ((expr test)
                        (k (kt_if-k conseq alt env-cps k)))
                   (value-of-cps expr env-cps k))]
                [(letcc body)
                 (let* ((expr body)
                        (env-cps (envr_extend-env k env-cps)))
                   (value-of-cps expr env-cps k))]
                [(throw k-exp v-exp)
                 (let* ((expr k-exp)
                        (k (kt_throw-k v-exp env-cps)))
                   (value-of-cps expr env-cps k))]
                [(let e body)
                 (let* ((expr e)
                        (k (kt_let-k body env-cps k)))
                   (value-of-cps expr env-cps k))]
                [(var y)
                 (apply-env env-cps y k)]
                [(lambda body)
                 (let* ((v (clos_make-closure body env-cps k)))
                   (apply-k k v))]
                [(app rator rand)
                 (let* ((expr rand)
                        (k (kt_app-inner-k rator env-cps k)))
                   (value-of-cps expr env-cps k))])))


#|
  6. Registerize the interpreter. Turn each let* expression to a begin
     block: the former let* bindings will become set! expressions, and the
     body becomes the invocation of a function of no arguments. Change all
     serious functions to be functions of no arguments. Define your global
     registers using define-registers at the top of the program.
 |# 
#|
  7. Change all of your (define name (lambda () ...)) statements to
     instead use define-label. Define your program counter at the top of
     the program using define-program-counter.
 |# 
#|
  8. Convert all label invocations into assignments to the program
     counter, and then add calls to mount-trampoline and
     dismount-trampoline. Note this will require modifying empty-k in your
     kt union, and the empty-k clause in the union-case inside apply-k. On
     the last line of main, print the register containing the final value
     of the program, e.g. (printf "Fact 5: ~s\n" v) See the parentheC
     document for notes on these steps.
 |# 
#|
  9. Comment out the lines `#lang racket`,
     `(require "parentheC.rkt")`. If you added it to your file, also
     comment your invocation of main (that is `(main)` but leave the
     definition of main intact). And save a copy of this file named
     interp.pc.
 |# 
#|
   After you have completed all of these steps, download `pc2c.rkt` to
   the same directory as this file, if you have not already done
   so. Using the DrRacket "open" dialog, open and run pc2c.rkt. This
   should load without errors. In the associated Racket REPL associated
   with your copy of pc2c.rkt, with **no** other files loaded,
   type `(pc2c "interp.pc" "a9.c" "a9.h")` which will generate C code
   from your interpreter. Compile the C program with the C compiler of
   your choice. The `login` linux machines have gcc installed, and you
   can find [binaries](http://gcc.gnu.org/install/binaries.html) for many
   different systems. This should look like `gcc a9.c`. Note, if you see a
   jmp_buf failure, this likely means you accidentally also tried to
   compile `a9.h`. Don't do that. Alternately, you could use an [online C
   compiler](http://tutorialspoint.com/compile_c_online.php). This should
   generate an ELF executable, probably named `a.out` unless you
   specified otherwise. Run the generated executable; if you see the
   correct output, you are finished with this assignment/project.

   You should turn in the `interp.pc` file that contains the exact
   code you used to generate your C programs.

|#


(main)