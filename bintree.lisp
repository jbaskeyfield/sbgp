 (in-package :sbgp)

;;; binary tree routing table functions rewritten to accept NLRI object instead of integer subnet and prefix arguments


(defun NODE-make ()
  (make-list 3 :initial-element nil))

(defun NODE-get-left-child (node)  "-> [ nil | NODE ]"  (car node))
(defun NODE-get-right-child (node) "-> [ nil | NODE ]"  (cadr node))
(defun NODE-get-data (node)        "-> object"          (caddr node))


(defmacro NODE-set-left-child (node value)  `(setf (car ,node) ,value))
(defmacro NODE-set-right-child (node value) `(setf (cadr ,node) ,value))
(defmacro NODE-set-data (node value)        `(setf (caddr ,node) ,value))

(defmacro NODE-clear-left-child (node)      `(setf (car ,node) nil))
(defmacro NODE-clear-right-child (node)     `(setf (cadr ,node) nil))
(defmacro NODE-clear-data (node)            `(setf (caddr ,node) nil))

(defmacro NODE-clear (node)
  `(setf (car ,node) nil
	 (cadr ,node) nil
	 (caddr ,node) nil))

(defmacro NODE-leaf-nodep (node)
  `(and (not (car ,node))
	(not (cadr ,node))))

(defmacro NODE-emptyp (node)
  `(and (not (NODE-get-left-child ,node))
	(not (NODE-get-right-child ,node))
	(not (NODE-get-data ,node))))

;;; 'generic' tree traversal functions (pre-order, in-order & post-order)
;;;  processes entire subtree from passed root node

;;; PRE-ORDER

(defun BINTREE-process-preorder (node fn)
  "Walks tree from NODE and applies passed function FN to NODE and each node in subtree.
(funcall fn node) run at each visited node. Processing order is 'pre-order'." 

  (funcall fn node)

  (if (NODE-get-left-child node)
      (BINTREE-process-preorder (NODE-get-left-child node) fn))
  
  (if (NODE-get-right-child node)
      (BINTREE-process-preorder (NODE-get-right-child node) fn)))

;;; IN-ORDER

(defun BINTREE-process-inorder (node fn)
  "Walks tree from NODE and applies passed function FN to NODE and each node in subtree.
(funcall fn node) run at each visited node. Processing order is 'in-order'." 

  (if (NODE-get-left-child node)
      (BINTREE-process-inorder (NODE-get-left-child node) fn))
  
  (funcall fn node)

  (if (NODE-get-right-child node)
      (BINTREE-process-inorder (NODE-get-right-child node) fn)))

;;; POST-ORDER

(defun BINTREE-process-postorder (node fn)
  "Walks tree from NODE and applies passed function FN to NODE and each node in subtree.
(funcall fn node) run at each visited node. Processing order is 'post-order'." 

  (if (NODE-get-left-child node)
      (BINTREE-process-postorder (NODE-get-left-child node) fn))
  
  (if (NODE-get-right-child node)
      (BINTREE-process-postorder (NODE-get-right-child node) fn))

  (funcall fn node))

;; process tree to 'max-depth'      
(defun BINTREE-process-inorder-to-depth (max-depth node fn)
  "Walks tree from NODE and applies passed function FN to NODE and each node in subtree - up to MAX-DEPTH links from NODE.
(funcall fn node) run at each visited node. Processing order is 'in-order'." 
  (labels ((rec (node current-depth)

	     (if (and (< current-depth max-depth)
		      (NODE-get-left-child node))
		 (rec (NODE-get-left-child node) (1+ current-depth)))
	     
	     (funcall fn node)

	     (if (and (< current-depth max-depth)
		      (NODE-get-right-child node))
		 (rec (NODE-get-right-child node) (1+ current-depth)))))
    
    (rec node 0)))

;; process tree at 'depth' from root node      
(defun BINTREE-process-inorder-at-depth (depth node fn)
  "Walks tree from NODE and applies passed function FN to each node in subtree at DEPTH links from NODE.
(funcall fn node) run at each visited node at DEPTH. Processing order is 'in-order'."
  (labels ((rec (node current-depth)

	     (if (and (< current-depth depth)
		      (NODE-get-left-child node))
		 (rec (NODE-get-left-child node) (1+ current-depth)))
	     
	     (if (= current-depth depth)
		 (funcall fn node))

	     (if (and (< current-depth depth)
		      (NODE-get-right-child node))
		 (rec (NODE-get-right-child node) (1+ current-depth)))))
    
    (rec node 0)))

(defun BINTREE-clear-subtree (node)
  "Clear and unlink all child nodes below NODE"
  (if (NODE-get-left-child node)
      (BINTREE-clear-subtree (NODE-get-left-child node)))

  (if (NODE-get-right-child node)
      (BINTREE-clear-subtree (NODE-get-right-child node)))

  ;; we are now at a leaf node
  ;; clear node entries before returning up to parent
  (NODE-clear node))


(defun BINTREE-count-nodes (root-node)
  "Walks entire subtree and returns multiple integer values (count of): 
1. nodes in tree 
2. routes. i.e. nodes for which NODE-get-data is not nil
3. leaf nodes i.e. nodes for which NODE-leaf-nodep is true
4. subnetted routes i.e nodes for which (and (NODE-get-data) (not (NODE-leaf-nodep))) / a non leaf-node that has one or more more specific subnets below it
5. empty internal nodes i.e. nodes for which (and (not (NODE-get-data)) (not (NODE-leaf-nodep))) / a non leaf node without attached data
6. empty nodes i.e nodes for which NODE-emptyp is true / a leaf node with no attached data"
  (let ((node-count 0)
	(route-count 0)
	(leaf-node-count 0)
	(subnetted-routes-count 0)
	(empty-internal-nodes 0)
	(empty-node-count 0))
    
    (BINTREE-process-inorder root-node
			     #'(lambda (node)
				 (incf node-count)                           ; nodes in tree
				 (if (NODE-get-data node)                    ; nodes with route data
				     (incf route-count))
				 (if (NODE-leaf-nodep node)                  ; leaf nodes
				     (incf leaf-node-count))
				 (if (and (NODE-get-data node)            
					  (not (NODE-leaf-nodep node)))      ; subnetted routes / non leaf node with data and one or more child nodes
				     (incf subnetted-routes-count))
				 (if (and (not (NODE-get-data node))
					  (not (NODE-leaf-nodep node)))      ; empty internal nodes / non leaf node without data
				     (incf empty-internal-nodes))
				 (if (NODE-emptyp node)                      ; leaf node without data
				     (incf empty-node-count))))
    (values node-count
	    route-count
	    leaf-node-count
	    subnetted-routes-count
	    empty-internal-nodes
	    empty-node-count)))


(defun BINTREE-add-route (root-node nlri data)
  (let ((prefix-length (NLRI-get-prefix-length-bits nlri))
	(prefix (NLRI-get-prefix nlri)))
    (labels ((rec (node current-depth)
	       (cond ((= current-depth prefix-length)                                                      ; if current-depth equals prefix-length,
		      (NODE-set-data node data)                                                            ;   set data on node,
		      node)                                                                                ;   return link to node.
		     (t                                                                                    ; otherwise, descend the tree
		      (cond ((list-uN-bit-set-p u56 prefix current-depth)                                  ; if subnet-id is to right of current node:
			     (if (not (NODE-get-right-child node))                                         ;   if no right-child
				 (NODE-set-right-child node (NODE-make)))                                  ;     make new empty child node
			     (rec (NODE-get-right-child node) (1+ current-depth)))                         ;   descend to right child
			    (t                                                                             ; otherwise, subnet-id is to left of current node:
			     (if (not (NODE-get-left-child node))                                          ;   if no left-child
				 (NODE-set-left-child node (NODE-make)))                                   ;     make new empty child node
			     (rec (NODE-get-left-child node) (1+ current-depth))))))))                     ;   descend to left child                          	  
      (rec root-node 0))))


(defun BINTREE-find-node (root-node nlri)
  ;; return link to node representing the prefix (node may or may not have attached data)
  ;; if node corresponding to prefix does not exist, returns nil
  (let ((prefix-length (NLRI-get-prefix-length-bits nlri))
	(prefix (NLRI-get-prefix nlri)))
    (labels ((rec (node current-depth)
	       (cond ((= current-depth prefix-length)                             ; if we have reached the 'prefix-length' tree depth,
		      node)                                                       ;  return current node
		     ((NODE-leaf-nodep node)
		      nil)
		     ((list-uN-bit-set-p u56 prefix current-depth)                ; bit clear => left child; bit set => right child
		      (if (NODE-get-right-child node)                             ; if right-child exists
			  (rec (NODE-get-right-child node) (1+ current-depth))    ;  descend to right child.
			  nil))                                                   ; otherwise, return nil
		     (t
		      (if (NODE-get-left-child node)                              ; if left-child exists
			  (rec (NODE-get-left-child node) (1+ current-depth))     ; descend to left child
			  nil)))))                                                ; otherwise, return nil
      (rec root-node 0))))   


(defun BINTREE-collect-nodes-on-path (root-node nlri)
  "Descends tree collecting nodes from ROOT-NODE to passed prefix NLRI.
Returns list of nodes traversed from ordered with the ROOT-NODE at the tail of the list.
Returns a partial node path if the target node is not found.
Second value returned indicates the status of the search - if returned path is a complete or a partial list.
 t => node corresponding to NLRI was found and is head of the returned path.
 nil => node correspondinf to NLRI was not found, incomplete path returned."
  (let ((prefix-length (NLRI-get-prefix-length-bits nlri))
	(prefix (NLRI-get-prefix nlri)))
    (labels ((rec (node current-depth node-path)
	       (cond ((= current-depth prefix-length)                        ; have found node mathcing the passed NLRI
		      (values (cons node node-path) t))                      
		     ((NODE-leaf-nodep node)                                 ; search truncated before current-depth = prefix-length 
		      (values node-path nil))
		     ((list-uN-bit-set-p u56 prefix current-depth)           ; bit clear => left child; bit set => right child
		      (if (NODE-get-right-child node)                        ; if right-child exists
			  (rec (NODE-get-right-child node)
			       (1+ current-depth)
			       (cons node node-path))
			  (values node-path nil)))
		     (t
		      (if (NODE-get-left-child node)                              ; if left-child exists
			  (rec (NODE-get-left-child node)
			       (1+ current-depth)
			       (cons node node-path))
			  (values node-path nil))))))
      (rec root-node 0 nil))))


(defun BINTREE-collect-data-matches-ipv4 (root-node ipv4-address)
   "Returns a list of matching data values on nodes (if present) from ROOT-NODE to the most specific match to IPV4-ADDRESS.
Most specific match is at head of list / ROOT-NODE will be at tail if default route exists"
  (let ((ip-address-u32 (IPV4-get-value ipv4-address)))
    (labels ((rec (node route-matches-list single-bit-mask)
	       (if (NODE-get-data node)                                                             ; if current node has a route entry
		   (push (NODE-get-data node) route-matches-list))                                  ; push current node onto matches list

	       (cond ((= 0 (logand ip-address-u32 single-bit-mask))                                 ; then descend the tree
		      (if (NODE-get-left-child node)                                                ; bit clear => left child
			  (rec (NODE-get-left-child node)
			       route-matches-list
			       (ash single-bit-mask -1))                                            ;  descend to left child node
			  route-matches-list))                                                      ;  or return node list
		     (t
		      (if (NODE-get-right-child node)                                               ; bit set => right child
			  (rec (NODE-get-right-child node)
			       route-matches-list
			       (ash single-bit-mask -1))                                            ;  descend to right child node
			  route-matches-list)))))                                                   ;  or return node list
      (rec root-node nil (ash 1 31)))))


#|
(defun delete-route (root-node nlri)
;; find node corresponding to passed prefix, and clear the data in node
;; prune tree back to parent node with data

(multiple-value-bind (node-path route-found)
(find-node-path root-node nlri)
(cond (route-found

t)
(t
nil))))
|#
