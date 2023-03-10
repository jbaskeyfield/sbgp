(in-package :sbgp)

(defun insert-in-order (element list-in predicate)
  "Function takes sorted list LIST-IN (previously sorted by PREDICATE) and returns copy with ELEMENT inserted in ordered position"
  (labels ((recurse (lst)
	     (cond ((null lst)
		    (cons element nil))
		   ((funcall predicate element (car lst))
		    (cons element lst))
		   (t
		    (cons (car lst) (recurse (cdr lst)))))))
    (recurse list-in)))
			  
  

;;; destructive version of remove-if and a version of remove-if that works on lists of lists

(defun remove-if! (predicate lst)
  "Destructive version of remove-if. Returns the modified list and the number of elements removed from the original list"
  (let ((removed-count 0))
    (labels ((find-head-of-list (lst)                                        
	       (cond ((null lst)
		      nil)  
		     ((funcall predicate (car lst))
		      (incf removed-count)
		      (find-head-of-list (cdr lst)))
		     (t
		      lst))))
      (let ((head-of-list (find-head-of-list lst)))
	(labels ((relink-list! (previous-cons this-cons)
		   (cond ((null this-cons)
			  (cond ((null (cdr previous-cons))
				 head-of-list)
				(t
				 (setf (cdr previous-cons) nil)
				 head-of-list)))
			 ((funcall predicate (car this-cons))              ; element matches predicate
			  (incf removed-count)                             ;  increment 'removed-count'
			  (relink-list! previous-cons (cdr this-cons)))    ;  move to next element in list ('previous-cons' unchanged)
			 (t                                                ; element doesnt match predicate
			  (if (not (eq (cdr previous-cons) this-cons))     ;  if 'previous-cons' and 'this-cons' are not linked
			      (setf (cdr previous-cons) this-cons))        ;   link 'previous-cons' to 'this-cons'
			  (relink-list! this-cons (cdr this-cons))))))     ;  move to next element in list 

	  (values (relink-list! head-of-list (cdr head-of-list))
		  removed-count))))))


(defun remove-sublist-elements-if! (predicate list-of-lists)
  "Destructive remove-if that works on lists of lists. Returns a list with all elemets of the sublists removed that satisfy PREDICATE. Sublists will be removed entirely if all elements satisfy PREDICATE." 
  (let ((removed-count 0))
    (labels ((find-head-of-list (list-of-lists)
	       (if (null list-of-lists)
		   nil
		   (multiple-value-bind (lst count) (remove-if! predicate (car list-of-lists))
		     (if (> count 0) (incf removed-count count))
		     (if (null lst)
			 (find-head-of-list (cdr list-of-lists))
			 list-of-lists)))))
      (let ((head-of-list (find-head-of-list list-of-lists)))
	(labels ((relink-list! (previous-cons this-cons)
		   (cond ((null this-cons)
			  (cond ((null (cdr previous-cons))
				 head-of-list)
				(t
				 (setf (cdr previous-cons) nil)
				 head-of-list)))
			 (t
			  (multiple-value-bind (sublist count) (remove-if! predicate (car this-cons))
			    (if (> count 0)
				(incf removed-count count))
			    (cond ((null sublist)
				   (relink-list! previous-cons (cdr this-cons)))
				  (t
				   (setf (car this-cons) sublist)
			           (if (not (eq (cdr previous-cons) this-cons))
				       (setf (cdr previous-cons) this-cons))
				   (relink-list! this-cons (cdr this-cons)))))))))

	  (values (relink-list! head-of-list (cdr head-of-list))
		  removed-count))))))
