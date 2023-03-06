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
