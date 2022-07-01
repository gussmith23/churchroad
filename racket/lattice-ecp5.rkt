#lang errortrace racket

(require rosette
         rosette/lib/synthax
         "comp-json.rkt")

(provide interpret-lattice-ecp5
         lattice-ecp5-logical-to-physical-inputs
         compile-lattice-pfu
         compile-lattice-ccu2c
         compile-lattice-ripple-pfu
         get-lattice-logical-inputs
         make-lattice-pfu-expr
         make-lattice-ccu2c-expr
         make-lattice-ripple-pfu-expr)

; The output of a LUT is simply the bit at the entry pointed to by `inputs`,
; when interpreted as an integer.
;
; LUTs must return only one bit.
(define (lut memory inputs)
  (let* ([len (length (bitvector->bits memory))] [inputs (zero-extend inputs (bitvector len))])
    (bit 0 (bvlshr memory inputs))))

(module+ test
  (require rackunit)
  (check-equal? (lut (bv #b0110 4) (bv 0 1)) (bv #b0 1))
  (check-equal? (lut (bv #b0110 4) (bv 1 1)) (bv #b1 1))
  (check-equal? (lut (bv #b0110 4) (bv 2 2)) (bv #b1 1))
  (check-equal? (lut (bv #b0110 4) (bv 3 2)) (bv #b0 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;        INTERPRETING LAKEROAD EXPRESSIONS         ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Top level for the lakeroad interpreter. This is used to interpret Lakeroad
;;; expressions:
;;; + (lattice-ecp-pfu lut-a lut-b ... lut-h inputs)
;;; + (lattice-ecp5-ccu2c INIT0 INIT1 INJECT1_0 INJECT1_1 inputs)
(define (interpret-lattice-ecp5 interpreter expr)
  (match expr
    [`(lattice-ecp5-pfu ,lut-a ,lut-b ,lut-c ,lut-d ,lut-e ,lut-f ,lut-g ,lut-h ,inputs)
     (let* ([inputs (interpreter inputs)]
            [pfu (list lut-a lut-b lut-c lut-d lut-e lut-f lut-g lut-h)])
       (interpret-ecp5-pfu-impl pfu inputs))]
    [`(lattice-ecp5-ccu2c ,INIT0 ,INIT1 ,INJECT1_0 ,INJECT1_1 ,CIN ,inputs)
     (interpret-ecp5-ccu2c-impl INIT0 INIT1 INJECT1_0 INJECT1_1 CIN (interpreter inputs))]
    [`(lattice-ecp5-ripple-pfu ,INIT0
                               ,INIT1
                               ,INIT2
                               ,INIT3
                               ,INIT4
                               ,INIT5
                               ,INIT6
                               ,INIT7
                               ,INJECT1_0
                               ,INJECT1_1
                               ,INJECT1_2
                               ,INJECT1_3
                               ,INJECT1_4
                               ,INJECT1_5
                               ,INJECT1_6
                               ,INJECT1_7
                               ,CIN
                               ,inputs)
     (interpret-ecp5-ripple-pfu-impl INIT0
                                     INIT1
                                     INIT2
                                     INIT3
                                     INIT4
                                     INIT5
                                     INIT6
                                     INIT7
                                     INJECT1_0
                                     INJECT1_1
                                     INJECT1_2
                                     INJECT1_3
                                     INJECT1_4
                                     INJECT1_5
                                     INJECT1_6
                                     INJECT1_7
                                     CIN
                                     (interpreter inputs))]
    [_ (error (format "Could not match expression ~a in interpret-lattice-ecp5" expr))]))

(define (interpret-ecp5-ripple-pfu-impl INIT0
                                        INIT1
                                        INIT2
                                        INIT3
                                        INIT4
                                        INIT5
                                        INIT6
                                        INIT7
                                        INJECT1_0
                                        INJECT1_1
                                        INJECT1_2
                                        INJECT1_3
                                        INJECT1_4
                                        INJECT1_5
                                        INJECT1_6
                                        INJECT1_7
                                        CIN
                                        inputs)
  (when (not (= (length inputs) 8))
    (error (format "expected inputs to be length 8, found length ~a: ~a" (length inputs) inputs)))
  (for ([input inputs])
    (when (not ((bitvector 4) input))
      (error (format "expected input to be a (bitvector 4), found ~a" input))))
  (match-let*
   ([`(,i0 ,i1 ,i2 ,i3 ,i4 ,i5 ,i6 ,i7) inputs]
    ;; Slice 0
    [result-0 (interpret-ecp5-ccu2c-impl INIT0 INIT1 INJECT1_0 INJECT1_1 CIN (list i0 i1))]
    [`(,S0 ,S1 ,carry-bit-0) result-0]
    ;; Slice 1
    [result-1 (interpret-ecp5-ccu2c-impl INIT2 INIT3 INJECT1_2 INJECT1_3 carry-bit-0 (list i2 i3))]
    [`(,S2 ,S3 ,carry-bit-1) result-1]
    ;; Slice 2
    [result-2 (interpret-ecp5-ccu2c-impl INIT4 INIT5 INJECT1_4 INJECT1_5 carry-bit-1 (list i4 i5))]
    [`(,S4 ,S5 ,carry-bit-2) result-2]
    ;; Slice 3
    [result-3 (interpret-ecp5-ccu2c-impl INIT6 INIT7 INJECT1_6 INJECT1_7 carry-bit-2 (list i6 i7))]
    [`(,S6 ,S7 ,COUT) result-3])
   (list S0 S1 S2 S3 S4 S5 S6 S7)))
; (let* ([ccu2c_1 (make-lattice-ccu2c-expr )])
;   inputs))
;;; Interpret a CCU2C
;;; INPUTS: (CIN A0 A1 B0 B1 C0 C1 D0 D1)
;;; OUTPUTS: (S0 S1 COUT)
(define (interpret-ecp5-ccu2c-impl INIT0 INIT1 INJECT1_0 INJECT1_1 CIN inputs)
  (match-let* ([`(,INPUTS0 ,INPUTS1) inputs])
              (let* (;;; // First Half
                     ;;; wire LUT4_0, LUT2_0;
                     ;;; LUT4 #(.INIT(INIT0)) lut4_0(.A(A0), .B(B0), .C(C0), .D(D0), .Z(LUT4_0));
                     [LUT4_0 (interpret-lut4-impl INIT0 INPUTS0)]
                     ;;; LUT2 #(.INIT(INIT0[3:0])) lut2_0(.A(A0), .B(B0), .Z(LUT2_0));
                     ; TODO: is this extract correct?
                     [LUT2_0 (interpret-lut2-impl (extract 3 0 INIT0) (extract 1 0 INPUTS0))]
                     ;;; wire gated_cin_0 = (INJECT1_0 == "YES") ? 1'b0 : CIN;
                     [gated_cin_0 (if (bveq INJECT1_0 (bv 1 1)) (bv 0 1) CIN)]
                     ;;; assign S0 = LUT4_0 ^ gated_cin_0;
                     [S0 (bvxor LUT4_0 gated_cin_0)]

                     ;;; wire gated_lut2_0 = (INJECT1_0 == "YES") ? 1'b0 : LUT2_0;
                     [gated_lut2_0 (if (bveq INJECT1_0 (bv 1 1)) (bv 0 1) LUT2_0)]
                     ;;; wire cout_0 = (~LUT4_0 & gated_lut2_0) | (LUT4_0 & CIN);
                     [cout_0 (bvor (bvand (bvnot LUT4_0) gated_lut2_0) (bvand LUT4_0 CIN))]

                     ;;; // Second half
                     ;;; wire LUT4_1, LUT2_1;
                     ;;; LUT4 #(.INIT(INIT1)) lut4_1(.A(A1), .B(B1), .C(C1), .D(D1), .Z(LUT4_1));
                     [LUT4_1 (interpret-lut4-impl INIT1 INPUTS1)]
                     ;;; LUT2 #(.INIT(INIT1[3:0])) lut2_1(.A(A1), .B(B1), .Z(LUT2_1));
                     ; TODO: is this extract correct?
                     [LUT2_1 (interpret-lut2-impl (extract 3 0 INIT1) (extract 1 0 INPUTS1))]
                     ;;;
                     ;;; wire gated_cin_1 = (INJECT1_1 == "YES") ? 1'b0 : cout_0;
                     [gated_cin_1 (if (bveq INJECT1_1 (bv 1 1)) (bv 0 1) cout_0)]
                     ;;; assign S1 = LUT4_1 ^ gated_cin_1;
                     [S1 (bvxor LUT4_1 gated_cin_1)]
                     ;;;
                     ;;; wire gated_lut2_1 = (INJECT1_1 == "YES") ? 1'b0 : LUT2_1;
                     [gated_lut2_1 (if (bveq INJECT1_1 (bv 1 1)) (bv 0 1) LUT2_1)]
                     ;;; assign COUT = (~LUT4_1 & gated_lut2_1) | (LUT4_1 & cout_0);
                     [COUT (bvor (bvand (bvnot LUT4_1) gated_lut2_1) (bvand LUT4_1 cout_0))])
                (list S0 S1 COUT))))

; Returns the physical outputs of the PFU as a list of bits
;
; OUTPUT: (A.Z B.Z C.Z D.Z E.Z F.Z G.Z H.Z)
;   outputs a length-8 list of (bitvector 1)s
(define (interpret-ecp5-pfu-impl pfu lut-inputs)
  (for/list ([l pfu] [i lut-inputs])
    (interpret-lut4-impl l i)))

(module+ test
  (require rackunit)
  (let* ([a (bv #x0003 16)]
         [b (bv #x000c 16)]
         [c (bv #x0030 16)]
         [d (bv #x00c0 16)]
         [e (bv #x0300 16)]
         [f (bv #x0c00 16)]
         [g (bv #x3000 16)]
         [h (bv #xc000 16)]
         [pfu (list a b c d e f g h)]
         [inputs-0s (make-list 8 (bv #x0 4))]
         [inputs-1s (make-list 8 (bv #x1 4))]
         [inputs-2s (make-list 8 (bv #x2 4))]
         [inputs-3s (make-list 8 (bv #x3 4))]
         [inputs-4s (make-list 8 (bv #x4 4))]
         [inputs-5s (make-list 8 (bv #x5 4))]
         [inputs-6s (make-list 8 (bv #x6 4))]
         [inputs-7s (make-list 8 (bv #x7 4))]
         [inputs-8s (make-list 8 (bv #x8 4))]
         [inputs-9s (make-list 8 (bv #x9 4))]
         [inputs-as (make-list 8 (bv #xa 4))]
         [inputs-bs (make-list 8 (bv #xb 4))]
         [inputs-cs (make-list 8 (bv #xc 4))]
         [inputs-ds (make-list 8 (bv #xd 4))]
         [inputs-es (make-list 8 (bv #xe 4))]
         [inputs-fs (make-list 8 (bv #xf 4))])
    (check-equal? (interpret-ecp5-pfu-impl pfu inputs-0s) (bitvector->bits (bv #b00000001 8)))
    (check-equal? (interpret-ecp5-pfu-impl pfu inputs-1s) (bitvector->bits (bv #b00000001 8)))
    (check-equal? (interpret-ecp5-pfu-impl pfu inputs-2s) (bitvector->bits (bv #b00000010 8)))
    (check-equal? (interpret-ecp5-pfu-impl pfu inputs-3s) (bitvector->bits (bv #b00000010 8)))
    (check-equal? (interpret-ecp5-pfu-impl pfu inputs-4s) (bitvector->bits (bv #b00000100 8)))
    (check-equal? (interpret-ecp5-pfu-impl pfu inputs-5s) (bitvector->bits (bv #b00000100 8)))
    (check-equal? (interpret-ecp5-pfu-impl pfu inputs-6s) (bitvector->bits (bv #b00001000 8)))
    (check-equal? (interpret-ecp5-pfu-impl pfu inputs-7s) (bitvector->bits (bv #b00001000 8)))
    (check-equal? (interpret-ecp5-pfu-impl pfu inputs-8s) (bitvector->bits (bv #b00010000 8)))
    (check-equal? (interpret-ecp5-pfu-impl pfu inputs-9s) (bitvector->bits (bv #b00010000 8)))
    (check-equal? (interpret-ecp5-pfu-impl pfu inputs-as) (bitvector->bits (bv #b00100000 8)))
    (check-equal? (interpret-ecp5-pfu-impl pfu inputs-bs) (bitvector->bits (bv #b00100000 8)))
    (check-equal? (interpret-ecp5-pfu-impl pfu inputs-cs) (bitvector->bits (bv #b01000000 8)))
    (check-equal? (interpret-ecp5-pfu-impl pfu inputs-ds) (bitvector->bits (bv #b01000000 8)))
    (check-equal? (interpret-ecp5-pfu-impl pfu inputs-es) (bitvector->bits (bv #b10000000 8)))
    (check-equal? (interpret-ecp5-pfu-impl pfu inputs-fs) (bitvector->bits (bv #b10000000 8)))))

(define (interpret-lut4-impl l inputs)
  (lut l (extract 3 0 inputs)))

(define (interpret-lut2-impl l inputs)
  (lut l (extract 1 0 inputs)))

(module+ test
  (require rackunit)
  (let* ([l (bv #b0110 16)])
    (check-equal? (interpret-lut4-impl l (bv 0 4)) (bv 0 1))
    (check-equal? (interpret-lut4-impl l (bv 1 4)) (bv 1 1))
    (check-equal? (interpret-lut4-impl l (bv 2 4)) (bv 1 1))
    (check-equal? (interpret-lut4-impl l (bv 3 4)) (bv 0 1))))

; Implements the basic arity-4 routing template for bv8 inputs.
;
; + We choose arity-4 because we are working with LUT4s
; + We default to bv8s because there are 8 LUT4s
; + Thus there are 4*8 = 32 input wires.
;
; Logical Inputs:
;
;   Each 'logical input' represents an input of a high-level operation that we
;   are trying to translate to a low-level LUT operation. For instance, if we
;   are trying to synthesize arity-2 addition for bv8s, the logical inputs
;   would be two bv8s.
;
; Physical Inputs:
;
;   Each 'physical input' represents an input pin on a LUT4.
;
;
; Diagramatically, this routing template looks like:
;
;       in3-0 --> A +-------+     # LSB of input 3 goes to pin 0 of LUT A
;       in2-0 --> B | LUT A |
;       in1-0 --> C |       |
;       in0-0 --> D +-------+
;
;       in3-1 --> A +-------+
;       in2-1 --> B | LUT B |
;       in1-1 --> C |       |
;       in0-1 --> D +-------+
;
;       in3-2 --> A +-------+
;       in2-2 --> B | LUT C |
;       in1-2 --> C |       |
;       in0-2 --> D +-------+
;
;       in3-3 --> A +-------+
;       in2-3 --> B | LUT D |
;       in1-3 --> C |       |
;       in0-3 --> D +-------+
;
;       in3-4 --> A +-------+
;       in2-4 --> B | LUT E |
;       in1-4 --> C |       |
;       in0-4 --> D +-------+
;
;       in3-5 --> A +-------+
;       in2-5 --> B | LUT F |
;       in1-5 --> C |       |
;       in0-5 --> D +-------+
;
;       in3-6 --> A +-------+
;       in2-6 --> B | LUT G |
;       in1-6 --> C |       |
;       in0-6 --> D +-------+
;
;       in3-7 --> A +-------+
;       in2-7 --> B | LUT H |
;       in1-7 --> C |       |
;       in0-7 --> D +-------+
; TODO: we don't really need this anymore since interpreter handles this. Is it
; useful for testing? Seems redundant and will likely lead to code syncing errors
; if anything changes down the line.
(define (lattice-ecp5-logical-to-physical-inputs logical-input-0
                                                 logical-input-1
                                                 logical-input-2
                                                 logical-input-3)
  (let ([helper (lambda (idx)
                  (concat (extract idx idx logical-input-3)
                          (extract idx idx logical-input-2)
                          (extract idx idx logical-input-1)
                          (extract idx idx logical-input-0)))])
    (list (helper 0) (helper 1) (helper 2) (helper 3) (helper 4) (helper 5) (helper 6) (helper 7))))

(module+ test
  (require rackunit)
  (let* ([li0 (bv #b00000000 8)]
         [li1 (bv #b11110000 8)]
         [li2 (bv #b11001100 8)]
         [li3 (bv #b10101010 8)]
         [out (lattice-ecp5-logical-to-physical-inputs li0 li1 li2 li3)])

    (check-equal? (list-ref out 7) (bv #b1110 4))
    (check-equal? (list-ref out 6) (bv #b0110 4))
    (check-equal? (list-ref out 5) (bv #b1010 4))
    (check-equal? (list-ref out 4) (bv #b0010 4))
    (check-equal? (list-ref out 3) (bv #b1100 4))
    (check-equal? (list-ref out 2) (bv #b0100 4))
    (check-equal? (list-ref out 1) (bv #b1000 4))
    (check-equal? (list-ref out 0) (bv #b0000 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;      Compile to JSON       ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-lattice-lut4-cell init-mem A B C D Z #:attrs [attrs (hasheq)])
  (make-cell "LUT4"
             (make-cell-port-directions (list 'A 'B 'C 'D) (list 'Z))
             (make-cell-connections 'A A 'B B 'C C 'D D 'Z Z)
             #:params (hasheq 'INIT init-mem)))

(define (make-lattice-ccu2c-cell INIT0
                                 INIT1
                                 INJECT1_0
                                 INJECT1_1
                                 A0
                                 B0
                                 C0
                                 D0
                                 A1
                                 B1
                                 C1
                                 D1
                                 CIN
                                 S0
                                 S1
                                 COUT
                                 #:attrs [attrs (hasheq)])
  (make-cell
   "CCU2C"
   (make-cell-port-directions (list 'A0 'B0 'C0 'D0 'A1 'B1 'C1 'D1 'CIN) (list 'S0 'S1 'COUT))
   (make-cell-connections 'A0
                          A0
                          'B0
                          B0
                          'C0
                          C0
                          'D0
                          D0
                          'A1
                          A1
                          'B1
                          B1
                          'C1
                          C1
                          'D1
                          D1
                          'CIN
                          CIN
                          'S0
                          S0
                          'S1
                          S1
                          'COUT
                          COUT)
   #:params (hasheq 'INIT0 INIT0 'INIT1 INIT1 'INJECT1_0 INJECT1_0 'INJECT1_1 INJECT1_1)))

(define (compile-lattice-ripple-pfu compiler
                                    get-unique-bit-ids
                                    add-cell-to-module
                                    add-netname-to-module
                                    add-parameter-default-value
                                    expr)
  (match-let* ([`(lattice-ecp5-ripple-pfu ,INIT0
                                          ,INIT1
                                          ,INIT2
                                          ,INIT3
                                          ,INIT4
                                          ,INIT5
                                          ,INIT6
                                          ,INIT7
                                          ,INJECT1_0
                                          ,INJECT1_1
                                          ,INJECT1_2
                                          ,INJECT1_3
                                          ,INJECT1_4
                                          ,INJECT1_5
                                          ,INJECT1_6
                                          ,INJECT1_7
                                          ,CIN
                                          ,inputs)
                expr]
               [(list i0 i1 i2 i3 i4 i5 i6 i7) (compiler inputs)]
               [sum-bits (get-unique-bit-ids 8)]
               [(list cout-bit) (get-unique-bit-ids 1)]
               ;;; CCU2C 0
               [ccu2c-0
                (make-lattice-ccu2c-expr #:inputs (list i0 i1)
                                         #:CIN CIN
                                         #:INIT0 INIT0
                                         #:INIT1 INIT1
                                         #:INJECT1_0 INJECT1_0
                                         #:INJECT1_1 INJECT1_1
                                         #:wrapper? #f)]
               [`(,S0 ,S1 (,COUT-0))
                (compile-lattice-ccu2c compiler
                                       get-unique-bit-ids
                                       add-cell-to-module
                                       add-netname-to-module
                                       add-parameter-default-value
                                       ccu2c-0)]
               ;;; CCU2C 1
               [ccu2c-1
                (make-lattice-ccu2c-expr #:inputs (list i2 i3)
                                         #:CIN COUT-0
                                         #:INIT0 INIT2
                                         #:INIT1 INIT3
                                         #:INJECT1_0 INJECT1_2
                                         #:INJECT1_1 INJECT1_3
                                         #:wrapper? #f)]
               [`(,S2 ,S3 (,COUT-1))
                (compile-lattice-ccu2c compiler
                                       get-unique-bit-ids
                                       add-cell-to-module
                                       add-netname-to-module
                                       add-parameter-default-value
                                       ccu2c-1)]
               ;;; CCU2C 2
               [ccu2c-2
                (make-lattice-ccu2c-expr #:inputs (list i4 i5)
                                         #:CIN COUT-1
                                         #:INIT0 INIT4
                                         #:INIT1 INIT5
                                         #:INJECT1_0 INJECT1_4
                                         #:INJECT1_1 INJECT1_5
                                         #:wrapper? #f)]
               [`(,S4 ,S5 (,COUT-2))
                (compile-lattice-ccu2c compiler
                                       get-unique-bit-ids
                                       add-cell-to-module
                                       add-netname-to-module
                                       add-parameter-default-value
                                       ccu2c-2)]
               ;;; CCU2C 3
               [ccu2c-3
                (make-lattice-ccu2c-expr #:inputs (list i6 i7)
                                         #:CIN COUT-2
                                         #:INIT0 INIT6
                                         #:INIT1 INIT7
                                         #:INJECT1_0 INJECT1_6
                                         #:INJECT1_1 INJECT1_7
                                         #:wrapper? #f)]
               [`(,S6 ,S7 ,COUT)
                (compile-lattice-ccu2c compiler
                                       get-unique-bit-ids
                                       add-cell-to-module
                                       add-netname-to-module
                                       add-parameter-default-value
                                       ccu2c-3)])
              (list S0 S1 S2 S3 S4 S5 S6 S7)))

(define (compile-lattice-pfu compiler
                             get-unique-bit-ids
                             add-cell-to-module
                             add-netname-to-module
                             add-parameter-default-value
                             expr)
  (match-let*
   ([`(lattice-ecp5-pfu ,a ,b ,c ,d ,e ,f ,g ,h ,inputs) expr]
    [inputs (compiler inputs)]
    ; Reserve ds for output pins for 8 luts in the PFU
    [luts-z (get-unique-bit-ids 8)]
    [lut-mems (list a b c d e f g h)]
    [LUTS
     (for/list ([lut lut-mems] [lut-input inputs] [z luts-z])
       (apply make-lattice-lut4-cell (make-literal-value-from-bv lut) (append lut-input (list z))))])
   ;; Add LUT cells
   (for ([lut LUTS] [c "ABCDEFGH"])
     (add-cell-to-module (as-symbol (format "~a_LUT" c)) lut))
   (for/list ([z luts-z])
     (list z))))

;;; Compile a Lakeroad CCU2C expression
;;; Return list of bits ((s0) (s1) (cout))
(define (compile-lattice-ccu2c compiler
                               get-unique-bit-ids
                               add-cell-to-module
                               add-netname-to-module
                               add-parameter-default-value
                               expr)
  (match-let* ([`(lattice-ecp5-ccu2c ,INIT0 ,INIT1 ,INJECT1_0 ,INJECT1_1 ,CIN ,inputs) expr]
               [compiled-inputs (compiler inputs)]
               [(list (list A0 B0 C0 D0) (list A1 B1 C1 D1)) compiled-inputs]
               [(list s0 s1 cout) (get-unique-bit-ids 3)]
               [ccu2c
                (make-lattice-ccu2c-cell (if (bv? INIT0) (make-literal-value-from-bv INIT0) INIT0)
                                         (if (bv? INIT1) (make-literal-value-from-bv INIT1) INIT1)
                                         (if (bvzero? INJECT1_0) "NO" "YES")
                                         (if (bvzero? INJECT1_1) "NO" "YES")
                                         A0
                                         B0
                                         C0
                                         D0
                                         A1
                                         B1
                                         C1
                                         D1
                                         (if (bv? CIN) (make-literal-value-from-bv CIN) CIN)
                                         s0
                                         s1
                                         cout)])
              (add-cell-to-module 'CCU2C ccu2c)
              (list (list s0) (list s1) (list cout))))

(define test-pfu
  `(lattice-ecp5-pfu ,(bv 0 16)
                     ,(bv 1 16)
                     ,(bv 2 16)
                     ,(bv 4 16)
                     ,(bv 8 16)
                     ,(bv 16 16)
                     ,(bv 32 16)
                     ,(bv 64 16)
                     ,(list (bv 0 4) (bv 0 4) (bv 0 4) (bv 0 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;     Create Lakeroad Expressions      ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Get logical inputs for an expression
(define (get-lattice-logical-inputs bv-expr #:num-inputs [num-inputs 4])
  (let ([symbs (symbolics bv-expr)])
    (append symbs (make-list (- num-inputs (length symbs)) (bv #xff 8)))))

;;; Create a lakeroad expression for a pfu
(define (make-lattice-pfu-expr logical-inputs)
  `(first (physical-to-logical-mapping
           (bitwise)
           (lattice-ecp5-pfu ,(?? (bitvector 16))
                             ,(?? (bitvector 16))
                             ,(?? (bitvector 16))
                             ,(?? (bitvector 16))
                             ,(?? (bitvector 16))
                             ,(?? (bitvector 16))
                             ,(?? (bitvector 16))
                             ,(?? (bitvector 16))
                             (logical-to-physical-mapping (bitwise) ,logical-inputs)))))

;;; Create a Lakeroad expression for a CCU2C. This can be used to specify a
;;; 2-bit add, etc
;;;
;;; Output: (S0 S1 COUT)
(define (make-lattice-ccu2c-expr #:inputs inputs
                                 #:CIN [CIN #f]
                                 #:INIT0 [INIT0 #f]
                                 #:INIT1 [INIT1 #f]
                                 #:INJECT1_0 [INJECT1_0 #f]
                                 #:INJECT1_1 [INJECT1_1 #f]
                                 #:wrapper? [wrapper? #t])
  (let ([inputs (append inputs (make-list (- 4 (length inputs)) (bv 3 2)))]
        [base-expr `(lattice-ecp5-ccu2c ,(or INIT0 (?? (bitvector 16))) ; INIT0
                                        ,(or INIT1 (?? (bitvector 16))) ; INIT1
                                        ,(or INJECT1_0 (?? (bitvector 1))) ; INJECT1_0
                                        ,(or INJECT1_1 (?? (bitvector 1))) ; INJECT1_1
                                        ,(or CIN (?? (bitvector 1))) ; CIN
                                        ,inputs)])
    (if wrapper? `(first (physical-to-logical-mapping (bitwise) ,base-expr)) base-expr)))

;;; Create a Lakeroad expression for a Ripple PFU. This can be used to specify
;;; an 8-bit add, etc.
;;;
;;; Optional Inputs
;;; ===============
;;;
;;; + #:inputs: the logical inputs to pass to the Ripple PFU. This should
;;;   be a list of length 4 or less, and each element should be a (bitvector 8).
;;;   This value defaults to an empty list. Provided input lists are padded with
;;;   (bv -1 8) to make the list length 8. If the list has length > 4 an error is
;;;   thrown.
;;;
;;; + #:CIN: the carry in bit. By default it is synthesizable as a single bit.
;;;   If specified it must bit a (bitvector 1)
;;;
;;; + #:INITn: Memory of the nth lut. Default is a hole to be synthesized.
;;;   Expects a (bitvector 16).
;;;
;;; + #:INJECT1_n: INJECT1 parameter for nth LUT4
;;;
;;;
;;; Interpretting the output of this function will result in a list
;;; `(list S0 S1 S2 S3 S4 S5 S6 S7 COUT)` of 8 sum-bits and a carry-bit
;;;
;;; The structure of this PFU is such that CIN feeds into the LUT0 (w/ memory
;;; INIT0), and the carry bit from LUT0 feeds into LUT1, etc.
(define (make-lattice-ripple-pfu-expr #:inputs [inputs '()]
                                      #:CIN [CIN #f]
                                      #:INIT0 [INIT0 #f]
                                      #:INIT1 [INIT1 #f]
                                      #:INIT2 [INIT2 #f]
                                      #:INIT3 [INIT3 #f]
                                      #:INIT4 [INIT4 #f]
                                      #:INIT5 [INIT5 #f]
                                      #:INIT6 [INIT6 #f]
                                      #:INIT7 [INIT7 #f]
                                      #:INJECT1_0 [INJECT1_0 #f]
                                      #:INJECT1_1 [INJECT1_1 #f]
                                      #:INJECT1_2 [INJECT1_2 #f]
                                      #:INJECT1_3 [INJECT1_3 #f]
                                      #:INJECT1_4 [INJECT1_4 #f]
                                      #:INJECT1_5 [INJECT1_5 #f]
                                      #:INJECT1_6 [INJECT1_6 #f]
                                      #:INJECT1_7 [INJECT1_7 #f])

  (define fn-name "make-latticeripple-pfu-expr")
  (when (> (length inputs) 4)
    (error (format "~a: inputs must be length 4 or less: ~a" fn-name inputs)))

  (for ([input inputs])
    (when (not ((bitvector 8) input))
      (error (format "~a: all inputs must satisfy (bitvector 8): ~a" fn-name input))))

  (let ([inputs (append inputs (make-list (- 4 (length inputs)) (bv -1 8)))])
    `(first (physical-to-logical-mapping
             (bitwise)
             (lattice-ecp5-ripple-pfu ,(or INIT0 (?? (bitvector 16)))
                                      ,(or INIT1 (?? (bitvector 16)))
                                      ,(or INIT2 (?? (bitvector 16)))
                                      ,(or INIT3 (?? (bitvector 16)))
                                      ,(or INIT4 (?? (bitvector 16)))
                                      ,(or INIT5 (?? (bitvector 16)))
                                      ,(or INIT6 (?? (bitvector 16)))
                                      ,(or INIT7 (?? (bitvector 16)))
                                      ,(or INJECT1_0 (?? (bitvector 1)))
                                      ,(or INJECT1_1 (?? (bitvector 1)))
                                      ,(or INJECT1_2 (?? (bitvector 1)))
                                      ,(or INJECT1_3 (?? (bitvector 1)))
                                      ,(or INJECT1_4 (?? (bitvector 1)))
                                      ,(or INJECT1_5 (?? (bitvector 1)))
                                      ,(or INJECT1_6 (?? (bitvector 1)))
                                      ,(or INJECT1_7 (?? (bitvector 1)))
                                      ,(or CIN (?? (bitvector 1)))
                                      (logical-to-physical-mapping (bitwise) ,inputs))))))