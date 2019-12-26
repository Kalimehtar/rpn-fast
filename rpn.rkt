#lang typed/racket
(require x64asm (prefix-in r: typed/racket))
(define args (current-command-line-arguments))
(define arg1 (if (= (vector-length args) 0) "f32" (vector-ref args 0)))
(define feature
  (cond
    [(string=? arg1 "f32") 'f32]
    [(string=? arg1 "f64") 'f64]
    [(string=? arg1 "u32") 'u32]
    [(string=? arg1 "u64") 'u64]
    [else 'f32]))

(module settings racket/base
  (require ffi/unsafe)
  (provide get-settings get-file read bytes-ptr)
  (define (get-settings feature)
    (case feature
      [(f32) (values (cast +inf.f _float _uint)
                     (cast -inf.f _float _uint)
                     (cast +nan.f _float _uint)
                     'float
                     4)]
      [(f64) (values (cast +inf.0 _double _uint64)
                     (cast -inf.0 _double _uint64)
                     (cast +nan.0 _double _uint64)
                     'float
                     8)]
      [(u32) (values (cast -1 _int _uint)
                     (cast -2 _int _uint)
                     (cast -3 _int _uint)
                     'int
                     4)]
      [(u64) (values (cast -1 _int64 _uint64)
                     (cast -2 _int64 _uint64)
                     (cast -3 _int64 _uint64)
                     'int
                     8)]))
  (define open (get-ffi-obj 'open #f (_fun _string _int -> _int)))
  (define read (cast (ffi-obj-ref 'read #f) _pointer _uint64))
  (define (bytes-ptr b) (cast b _pointer _uint64))
  (define (get-file f)
    (define s (file-size f))
    (values (open f 0) s)))

(require/typed 'settings
               [get-settings (-> Symbol (Values Integer Integer
                                                Integer Symbol Integer))]
               [get-file (-> String (Values Integer Integer))]
               [bytes-ptr (-> Bytes Integer)]
               [read Integer])

(define-values (plus minus mult type width) (get-settings feature))
(define bits (cast (* 8 width) Size))

(define-cast ->float #:type (-> Float) #:ctype (_fun -> _float))
(define-cast ->double #:type (-> Float) #:ctype (_fun -> _double))
(define-cast ->int #:type (-> Integer) #:ctype (_fun -> _int))
(define-cast ->int64 #:type (-> Integer) #:ctype (_fun -> _int64))

(define buf-size (* 32 1024))
(define buf (bytes-ptr (make-bytes buf-size)))

(define-values (fd size) (get-file "input.txt"))

(define (movf (a : Reg) (b : Reg))
  (when (eq? type 'float) (movq a b)))

(define calc-type
  (cond
    [(r:and (eq? type 'float) (= bits 32)) ->float]
    [(r:and (eq? type 'float) (= bits 64)) ->double]
    [(r:and (eq? type 'int) (= bits 32)) ->int]
    [(r:and (eq? type 'int) (= bits 64)) ->int64]
    [else ->float]))

(define-λ! calc calc-type #:labels (item lplus lminus lmult next read-loop not-last)
  (push rbx)
  (push r12)
  (push r13)
  (push r14)
  (push r15)
  (xor r15 r15)
  (mov r12 (imm64 plus))
  (mov r13 (imm64 minus))
  (mov r14 (imm64 mult))
  (xor rax rax)
  (:! read-loop)
  (mov rdi (imm64 fd))
  (mov rsi (imm64 buf))
  (mov rbx (imm64 buf-size))
  (mov rdx rbx)
  (mov rax (imm64 read))
  (call rax)
  (cmp rax rbx)
  (je (rel8 not-last))
  (mov r15 (imm64 1))
  (:! not-last)
  (shr rax (imm8 (if (= width 4) 2 3)))
  (mov rcx rax)
  (mov rsi (imm64 buf))
  (:! item)
  (mov (if (= bits 32) eax rax) (mref bits rsi))
  (cmp rax r12)
  (je (rel8 lplus))
  (cmp rax r13)
  (je (rel8 lminus))
  (cmp rax r14)
  (je (rel8 lmult))
  (push rax)
  (jmp (rel8 next))
  (:! lplus)
  (pop rbx)
  (movf xmm1 rbx)
  (pop rax)
  (movf xmm0 rax)
  (if (eq? type 'float)
      ((if (eq? width 4) addss addsd) xmm0 xmm1)
      (add rax rbx))
  (movf rax xmm0)
  (push rax)
  (jmp (rel8 next))
  (:! lminus)
  (pop rbx)
  (movf xmm1 rbx)
  (pop rax)
  (movf xmm0 rax)
  (if (eq? type 'float)
      ((if (eq? width 4) subss subsd) xmm0 xmm1)
      (sub rax rbx))
  (movf rax xmm0)
  (push rax)
  (jmp (rel8 next))
  (:! lmult)
  (pop rbx)
  (movf xmm1 rbx)
  (pop rax)
  (movf xmm0 rax)
  (if (eq? type 'float)
      ((if (eq? width 4) mulss mulsd) xmm0 xmm1)
      (mul rbx))
  (movf rax xmm0)
  (push rax)
  (:! next)
  (add rsi (imm8 width))
  (loop (rel8 item))
  (cmp r15 (imm32 0))
  (je (rel32 read-loop))
  (pop rax)
  (movf xmm0 rax)
  (pop r15)
  (pop r14)
  (pop r13)
  (pop r12)
  (pop rbx)
  (ret))

(displayln (calc))
