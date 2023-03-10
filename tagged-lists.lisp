(in-package :sbgp)

;;; Tagged lists
(defun TL-get-name (obj)  "-> symbol" (car obj))
(defun TL-typep (obj) (and (consp obj) (car obj) (symbolp (car obj))))

;;; Tagged vectors
(defmacro TV-get-name (obj) "-> symbol" `(svref ,obj 0))
(defun TV-typep (obj) (and (vectorp obj) (> (length obj) 0) (symbolp (svref obj 0))))

;;; WALK / FILTER TREE OF TAGGED LISTS

(defun TL-find-in-tree (tag-symbol tl-tree)
  "Walks tree TL-TREE and returns first list with CAR that is EQ to TAG-SYMBOL"
  (block TREE-WALK-BLOCK
    (labels ((tree-walk (node)
	       (if (consp (car node))
		   (tree-walk (car node)))
	       (if (eq (car node) tag-symbol)
		   (return-from TREE-WALK-BLOCK node)
		   (if (consp (cdr node))
		       (tree-walk (cdr node))))))
      (tree-walk tl-tree))))

(defun TL-tree-extract-all-records (tl-tree)
  "Walks tree TL-TREE and returns list of all sublists that have a symbol as first element"
  (let ((return-list nil))
    (labels ((tree-walk (node)
	       (if (consp (car node))
		   (tree-walk (car node)))

	       (if (and (car node)
			(symbolp (car node)))
		   (push node return-list))
	       
	       (if (consp (cdr node))
		   (tree-walk (cdr node)))))
      (tree-walk tl-tree)
      return-list)))

(defun TL-tree-extract-if-member (tl-tree &optional member-list)
  "Walks tree TL-TREE and returns list of all sublists that have a symbol as first element.
If MEMBER-LIST is supplied, sublist will only be collected if (member (car 'sublist') member-list)."
  (let ((return-list nil))
    (labels ((tree-walk (node)
	       (if (consp (car node))
		   (tree-walk (car node)))

	       (if (and (car node)                  ;; don't collect nil
			(symbolp (car node))        ;; 
			(if member-list 
			    (member (car node) member-list)
			    t))
		   (push node return-list))
	       
	       (if (consp (cdr node))
		   (tree-walk (cdr node)))))
      (tree-walk tl-tree)
      return-list)))


(defun tl-tree-filter-collect1 (tags-member-list tl-tree)
  "Walks tree TL-TREE and returns list that IDs match TAGS-MEMBER-LIST"
  (let ((return-list nil))
    (labels ((tree-walk (node)
	       (if (consp (car node))
		   (tree-walk (car node)))

	       (if (member (car node) tags-member-list)
		   (push node return-list)
		   (if (consp (cdr node))
		       (tree-walk (cdr node))))))
      (tree-walk tl-tree)
      return-list)))

(defun tl-greater-than-p (tl-1 tl-2)
  "Greater than function for tagged-lists (suitable for sorting path-attributes only).
Function assumes both arguments are lists comprised of only integers, symbols, and other tagged-lists. 
The lists are compared cell by cell and assumes: all symbols are equal; all integers > all symbols and sublists; longer lists are > shorter lists.
Note: this function isn't suitable for comparing two lists that contain only a single symbol each or NLRI with NLRI-WITHDRAWL"
  (cond ((eq tl-1 tl-2)                                  ; run out of elements in both lists, or both lists are identical
	 nil)
	((null tl-1)                                     ; head of tl-2 is same as tl-1, but tl-2 is longer
	 nil)
	((null tl-2)                                     ; head of tl-1 is same as tl-2, but tl-1 is longer
	 t)
	(t
	 (let ((e1 (car tl-1))
	       (e2 (car tl-2)))
	   (cond ((or (eq e1 e2)                         ; if both cells that are equal, or are both symbols
		      (and (symbolp e1) (symbolp e2)))    
		  (tl-greater-than-p (cdr tl-1)          ;  move to next element in tl-1 and tl-2
				    (cdr tl-2)))
		 
		 ((and (integerp e1) (integerp e2))      ; if both elements are fixnums
		  (cond ((> e1 e2)                       ;  compare integers
			 t)
			((= e1 e2)
			 (tl-greater-than-p (cdr tl-1)
					   (cdr tl-2)))
			(t
			 nil)))
		  
		 ((and (consp e1) (consp e2))            ; if both elements are sublists
		  (if (equal e1 e2)                      ;  if equal
		      (tl-greater-than-p (cdr tl-1)      ;  move to next element in tl-1 and tl-2
					(cdr tl-2)))
		      (tl-greater-than-p e1 e2))         ;  otherwise, return value from comparing sublists
			
		 ((integerp e1)                          ; assume all integers > symbols and lists
		  t)
		 (t
		  nil))))))


(defun list-of-tl-greater-than-p (tl-list-1 tl-list-2)
  "Greater than function to be used for sorting lists of path attributes, or rib-entries (keyed by path-attribute). To be passed as predicate to 'sort' function for ordering rib entries with shared path attributes together"
  (cond ((eq tl-list-1 tl-list-2)                               ; run out of elements in both lists, or both lists are identical
	 nil)
	((null tl-list-1)                                       ; head of tl-list-2 is same as tl-list-1, but tl-list-2 is longer
	 nil)
	((null tl-list-2)                                       ; head of tl-list-1 is same as tl-list-2, but tl-list-1 is longer
	 t)
	(t
	 (let ((tl-1 (car tl-list-1))                           ; compare sublists
	       (tl-2 (car tl-list-2)))
	   (cond ((equal tl-1 tl-2)                             ; if equal, move to next sublists
		  (list-of-tl-greater-than-p (cdr tl-list-1)
					   (cdr tl-list-2)))
		 (t
		  (tl-greater-than-p tl-1 tl-2)))))))           ; otherwise, compare sublists
	

