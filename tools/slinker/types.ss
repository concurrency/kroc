#|
slinker - types.ss
A Scheme linker for Transputer bytecodes targeting the Transterpreter
Copyright (C) 2004-2008 Matthew C. Jadud, Christian L. Jacobsen

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
|#
#cs
(module types mzscheme
  (require "version.ss")
  (provide (all-defined))
  
  ;; Add this files revision to the version information
  (put-revision "$Revision: 3119 $")

  (define-struct pass (meta bytes) (make-inspector))
  (define-struct Instruction (meta) (make-inspector))
  ;;(define-struct Primary () (make-inspector))
  ;;(define-struct Secondary () (make-inspector))
  ;;(define-struct Special () (make-inspector))

  (define-struct (inst Instruction) (fn op) (make-inspector))
  
  (define-struct (spec Instruction) (op data) (make-inspector))

  ;;Does a special check equally well on 16 and 32 bit machines.
  (define low2
    (lambda (op)
      (bitwise-and op #xFFFF)))
  
  (define special?
    (lambda (fn op)
      (and (= fn 15)
           (>= (low2 op) (string->number "#xFF00" 16))
           (<= (low2 op) (string->number "#xFF30" 16)))))
  
  (define special:0->15?
    (lambda (fn op)
      (and (= fn 15)
           (>= (low2 op) (string->number "#xFF00" 16))
           (<= (low2 op) (string->number "#xFF0F" 16)))))
  
  (define special:string?
    (lambda (fn op)
      (and (= fn 15)
           (>= (low2 op) (string->number "#xFF11" 16))
           (<= (low2 op) (string->number "#xFF1C" 16)))))
  
  (define special:jump?
    (lambda (fn op)
      (and (= fn 15)
           (= (low2 op) (string->number "#xFF20" 16)))))
  
  
  (define special:mobilespace?
    (lambda (fn op)
      (and (= fn 15)
           (= (low2 op) (string->number "#xFF24" 16)))))
  
  (define special:lend?
    (lambda (fn op)
      (and (= fn 15)
           (= (low2 op) (string->number "#xFF21" 16)))))

  (define special:lend3?
    (lambda (fn op)
      (and (= fn 15)
           (= (low2 op) (string->number "#xFF22" 16)))))

  (define special:lendbw?
    (lambda (fn op)
      (and (= fn 15)
           (= (low2 op) (string->number "#xFF23" 16)))))
  
  (define special:loadwsmap?
    (lambda (fn op)
      (and (= fn 15)
           (= (low2 op) (string->number "#xFF27" 16)))))
  
  (define special:unloadwsmap?
    (lambda (fn op)
      (and (= fn 15)
           (= (low2 op) (string->number "#xFF28" 16)))))


  (define special:mobileinit?
    (lambda (fn op)
      (and (= fn 15)
           (= (low2 op) (string->number "#xFF25" 16)))))

  (define-struct (tsdepth Instruction) (value) (make-inspector))
  
  (define-struct (funcresults Instruction) (value) (make-inspector))
  
  (define-struct (funcreturn Instruction) (value) (make-inspector))
  
  (define-struct (endws Instruction) (value) (make-inspector))
  
  (define-struct (realresult Instruction) (value) (make-inspector))
  
  (define-struct (setlab Instruction) (value) (make-inspector))
  
  (define-struct (sectionlab Instruction) (value) (make-inspector))
  
  (define-struct (align Instruction) (value) (make-inspector))
  
  (define-struct (linenum Instruction) (value) (make-inspector))
  
  (define-struct (debugline Instruction) (value) (make-inspector))
  
  (define-struct (setws Instruction) (value) (make-inspector))
  
  (define-struct (setvs Instruction) (value) (make-inspector))
  
  (define-struct (sllimm Instruction) (value) (make-inspector))
  
  (define-struct (slrimm Instruction) (value) (make-inspector))
  
  (define-struct (loopheadtop Instruction) (value) (make-inspector))
  
  (define-struct (stubname Instruction) (value) (make-inspector))
  (define-struct (ffi-stubname Instruction) (value) (make-inspector))
  
  (define-struct (globalname Instruction) (string) (make-inspector))
  
  (define-struct (jentry Instruction) (string) (make-inspector))
  
  (define-struct (procentry Instruction) (string) (make-inspector))
  
  (define-struct (codemap Instruction) (string) (make-inspector))
  
  (define-struct (source-filename Instruction) (string) (make-inspector))
  
  (define-struct (compiler-comment Instruction) (string) (make-inspector))
  
  (define-struct (data-bytes Instruction) (string) (make-inspector))
  
  (define-struct (message-bytes Instruction) (string) (make-inspector))
  
  (define-struct (mobilespace-usage Instruction) (value) (make-inspector))
  
  (define-struct (boolinvert Instruction) ())
  
  (define-struct (starttable Instruction) ())
  
  (define-struct (widenshort Instruction) ())

  (define-struct (loopheadbot Instruction) ())
  
  (define-struct (contrsplit Instruction) ())
  
  (define-struct (contrjoin Instruction) ())
  
  (define-struct (i64toreal Instruction) ())
  
  (define-struct (notprocess Instruction) ())
  
  (define-struct (fppop Instruction) ())
  
  (define-struct (checknotnull Instruction) ())
  
  (define-struct (semclaim Instruction) ())
  
  (define-struct (semrelease Instruction) ())
  
  (define-struct (seminit Instruction) ())
  
  (define-struct (etc0_reschedule Instruction) ())
  
  ;;Barrier support
  (define-struct (barinit Instruction) ())
    
  (define-struct (barsync Instruction) ())
  
  (define-struct (barresign Instruction) ())
  
  (define-struct (barenroll Instruction) ())                         
  
  (define-struct (j Instruction) (value) (make-inspector))
  
  (define-struct (cj Instruction) (value) (make-inspector))
  
  (define-struct (call Instruction) (value) (make-inspector))
  
  (define-struct (load-label Instruction) (value) (make-inspector))
  
  (define-struct (load-label-difference Instruction) (start end) (make-inspector))
  
  (define-struct (loopend Instruction) (ctrl-block-addr end start) (make-inspector))

  (define-struct (loopend3 Instruction) (ctrl-block-addr end start) (make-inspector))
  ;;(define-struct (loopend3 loopend) () (make-inspector))

  (define-struct (loopend-backwards Instruction) (ctrl-block-addr end start) (make-inspector))
  ;;(define-struct (loopend-backwards loopend) () (make-inspector))
  
  ;; FIXME: I have no clue what this does, but I am going to pick it up
  ;; for now and discard it. I have only seen it used with occam-pi stuff
  ;; (the occampi.occ file in occam8.lib)
  (define-struct (loadwsmap Instruction) (something a-label) (make-inspector))
  ;; As above
  (define-struct (unloadwsmap Instruction) (something a-label) (make-inspector))
  
  (define-struct (binary Instruction) (value) (make-inspector))
 
  (define-struct (ujump Instruction) (value fn) (make-inspector))

  ;; This is a fixed (length) jump, which needs special handling for calculating
  ;; its distance/length. It is going to be used for stubnames and case
  ;; jumptables and that sort of thing where we need to ensure that optimal
  ;; prefixing does not shrink the jump as that would be bad!!!
  ;; The dist is the distance which the jump needs to have...
  (define-struct (fjump ujump) (len) (make-inspector))
  
  (define-struct (ulabel Instruction) (value) (make-inspector))
 
  ;; Mobilethingy
  (define-struct (mobileinit Instruction) (msp-offset count  pairs) (make-inspector))
  )
