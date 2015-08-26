#lang racket
(require gigls/unsafe)
; gigls/tree.rkt
;   Procedures to work with trees

(provide (all-defined-out))

; +-------+-----------------------------------------------------------
; | Notes |
; +-------+

; This library is currently under development while I think through
; the issues.

; While there are procedures to set left and right subtrees, we may
; not reveal those procedures to students.

; +-----------+-------------------------------------------------------
; | Questions |
; +-----------+

; Should I call these bintrees, 2trees, bi-trees or something similar to 
; indicate that they are binary trees?

; Should I generalize nodes so that they are not ?

; +-------------+-----------------------------------------------------
; | Empty Trees |
; +-------------+

;;; Name:
;;;   nil
;;; Type:
;;;   tree
;;; Value:
;;;   The empty tree
(define nil 'nil)

;;; Procedure:
;;;   nil?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine if val represents an empty tree.
;;; Produces:
;;;   is-nil?, a Boolean 
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   is-nil? is true (#t) if and only if val can be interpreted as
;;;   the empty tree.
(define nil? 
  (lambda (val)
    (eq? val nil)))

; +-------+-----------------------------------------------------------
; | Nodes |
; +-------+

;;; Procedure:
;;;   node
;;; Parameters:
;;;   val, a value
;;;   left, a tree
;;;   right, a tree
;;; Purpose:
;;;   Create a node in a binary tree.
;;; Produces:
;;;   tree, a tree
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (node? tree) holds.
;;;   (node-left tree) = left.
;;;   (node-right tree) = right.
;;;   (node-value tree) = val.
(define node
  (lambda (val left right)
    (vector 'node val left right)))

;;; Procedure:
;;;   node?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine if val can be used as a tree node.
;;; Produces:
;;;   is-tree?, a Boolean
(define node?
  (lambda (val)
    (and (vector? val)
         (= (vector-length val) 4)
         (eq? (vector-ref val 0) 'node))))

;;; Procedure:
;;;   leaf?
;;; Parameters:
;;;   nod, a binary tree node
;;; Purpose:
;;;   Determine if node is a leaf
;;; Produces:
;;;   is-leaf?, a Boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   is-leaf? is true iff the left subtree and the right subtree are nil.
(define leaf?
  (lambda (nod)
    (and (nil? (node-left nod))
         (nil? (node-right nod)))))

;;; Procedure:
;;;   node-left
;;; Parameters:
;;;   nod, a binary tree node
;;; Purpose:
;;;   Extract the left subtree of nod.
;;; Produces:
;;;   val, a Scheme value
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (node-left (node val l r)) = l
(define node-left
  (lambda (nod)
    (when (not (node? nod))
      (error "node-left: expected a node, received " nod))
    (vector-ref nod 2)))

;;; Procedure:
;;;   node-right
;;; Parameters:
;;;   nod, a binary tree node
;;; Purpose:
;;;   Extract the right subtree of nod.
;;; Produces:
;;;   val, a Scheme value
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (node-right (node val l r)) = r 
(define node-right
  (lambda (nod)
    (when (not (node? nod))
      (error "node-right: expected a node, received " nod))
    (vector-ref nod 3)))

;;; Procedure:
;;;   node-value
;;; Parameters:
;;;   nod, a binary tree node
;;; Purpose:
;;;   Extract the value of nod.
;;; Produces:
;;;   val, a Scheme value
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (node-value (node val left right)) = val
(define node-value
  (lambda (nod)
    (when (not (node? nod))
      (error "node-value: expected a node, received " nod))
    (vector-ref nod 1)))

;;; Procedure:
;;;   tree?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determines if val represents a tree
;;; Produces:
;;;   is-tree?, a Boolean value
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If val is either nil or a node whose left and right children
;;;     are trees, then is-tree? is #t.
;;;   Otherwise, is-tree? is #f.
(define tree?
  (lambda (val)
    (or (nil? val)
        (and (node? val)
             (tree? (node-left val))
             (tree? (node-right val))))))

; +----------+--------------------------------------------------------
; | Mutators |
; +----------+

; +------------------------------+------------------------------------
; | Miscellaneous Tree Utilities |
; +------------------------------+

;;; Procedure:
;;;   tree-depth
;;; Parameters:
;;;   tree, a tree
;;; Purpose:
;;;   Determine the depth of tree
;;; Produces:
;;;   depth, an integer
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   depth represents the number of nodes on the path from the node-value to 
;;;   the furthest leaf.
(define tree-depth
  (lambda (tree)
    (cond
      [(nil? tree)
       0]
      [(not (node? tree))
       (error "Cannot find the depth of a non-tree")]
      [else
       (+ 1 (max (tree-depth (node-left tree))
                 (tree-depth (node-right tree))))])))

;;; Procedure:
;;;   tree-size
;;; Parameters:
;;;   tree, a tree
;;; Purpose:
;;;   Determine the number of node sin a binary tree
;;; Produces:
;;;   size, an integer
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   size >= 0
;;;   tree contains size nodes
(define tree-size
  (lambda (tree)
    (cond
      [(nil? tree)
       0]
      [(not (node? tree))
       (error "tree-size: cannot find the size of a non-tree")]
      [else
       (+ 1 (tree-size (node-left tree))
          (tree-size (node-right tree)))])))

;;; Procedure:
;;;   tree->code
;;; Parameters:
;;;   tree, a tree
;;; Purpose:
;;;   Generate Scheme code to make a tree.
;;; Produces:
;;;   code, a Scheme value
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   code, when evaluated, can give something like tree
;;; Problems:
;;;   Won't work for all kinds of tree values
(define tree->code
  (lambda (tree)
    (if (nil? tree)
        'nil
        (list 'node 
              (node-value tree) 
              (tree->code (node-left tree))
              (tree->code (node-right tree))))))

;;; Procedure:
;;;   visualize-tree
;;; Parameters:
;;;   tree, a binary tree
;;;   width, a positive integer
;;;   height, a positive integer
;;; Purpose:
;;;   Create a simple image to visualize a tree
;;; Produces:
;;;   image, an image
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   image contains an "appropriate" representation of the tree.
;;;   The context may have been updated.
;;; Problems:
;;;   Doesn't do well with especially bushy trees - these may overlap.
(define visualize-tree
  (let ([; (draw-nil image x y)
         ;   Draw the nil tree on image, with horizontal center at x and top at y.
         draw-nil!
         (lambda (image x y)
           (context-set-fgcolor! "grey")
           (image-select-ellipse! image REPLACE (- x 5) (- y 10) 10 10)
           (image-draw-line! image (- x 5) (- y 10) (+ x 5) y)
           (image-stroke! image)
           (image-select-nothing! image))])
    (lambda (tree width height)
      ; Set the font
      (context-set-font-name! "Monospace")
      (context-set-font-size! 12)
      ; Set the brush
      (context-set-brush! "2. Hardness 100" 1)
      (let (; The resulting image
            [result (image-show (image-new width height))]
            ; The height of each level
            [level-height (if (nil? tree)
                              height
                              (/ (- height 20) (tree-depth tree)))])
        (let kernel ([tree tree]
                     [left 0]
                     [top 0]
                     [width width])
          (let ([center-x (+ left (/ width 2))]
                [center-y (+ top 15)])
            (cond
              [(nil? tree)
               (draw-nil! result center-x center-y)]
              [else
               ; Display the node's value
               (context-set-fgcolor! "black")
               (image-display-text! result
                                    (value->string (node-value tree))
                                    center-x center-y
                                    ALIGN-CENTER ALIGN-BOTTOM)
               (let ([left-subtree (node-left tree)]
                     [right-subtree (node-right tree)]
                     [half-width (/ width 2)]
                     [quarter-width (/ width 4)]
                     [next-top (+ top level-height)])
                 ; Display the links
                 (context-set-fgcolor! "grey")
                 (image-draw-arrow! result
                                    'filled
                                    center-x center-y
                                    (- center-x quarter-width) next-top
                                    5 5)
                 (image-draw-arrow! result
                                    'filled
                                    center-x center-y
                                    (+ center-x quarter-width) next-top
                                    5 5)
                 ; Draw the left subtree
                 (kernel left-subtree left next-top half-width)
                 ; Draw the right subtree
                 (kernel right-subtree center-x next-top half-width))])))))))

;;; Procedure:
;;;   random-tree
;;; Parameters:
;;;   size, a non-negative integer
;;;   value!, a zeroary procedure
;;; Purpose:
;;;   Build a tree of the given size whose structure is unpredictable.
;;; Produces:
;;;   tree, a tree
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (tree? tree)
;;;   (tree-size tree) == size.
;;;   All values in the tree were produced by separate calls to (value!)
(define random-tree
  (lambda (size value!)
    (cond
      [(zero? size)
       nil]
      [(= size 1)
       (node (value!) nil nil)]
      [else
       (let ([left-size (random (- size 1))])
         (node (value!) 
               (random-tree left-size value!)
               (random-tree (- size left-size 1) value!)))])))
