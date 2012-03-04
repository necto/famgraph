(in-package #:kin-package)


(defun get-page (is-firefox)
  (kin-templates:tree-page `(:agent ,(if is-firefox
									   "firefox"
									   "other"))))

(defun graphic-vec (vec)
  `(:x ,(vec-x vec) :y ,(vec-y vec)))

(defun graphic-marriage (marr)
  `(:id ,(id marr)
	:date ,(item-date marr)
	:date-unknown ,(item-date-unknown marr)
	:photo ,(if (m-photo marr)
			  (m-photo marr)
			  "rings.jpg")
    :surname ,(m-surname marr)))

(defun graphic-person (pers)
  `(:id ,(id pers)
	:name ,(p-name pers)
	:middle-name ,(p-middle-name pers)
	:surname ,(p-surname pers)
	:age ,(safe-age-to-height (safe-p-age pers))
	:sex ,(symbol-name (p-sex pers))
	:photo ,(if (p-photo pers)
			  (p-photo pers)
			  "nophoto.jpg")
	:date-unknown ,(item-date-unknown pers)
	:death-date-unknown ,(p-death-date-unknown pers)
	:birth ,(item-date pers)
	:death ,(p-death-date pers)))

(defun simple-graphic-person (pers)
  (append (graphic-person pers) '(:simple t)))

(defun graphic-node (node)
  `(:pos ,(graphic-vec (node-pos node))
	:type ,(symbol-name (type-of (node-data node)))
	,@(case (type-of (node-data node))
			(marriage (graphic-marriage (node-data node)))
			(person (graphic-person (node-data node))))))

(defun graphic-edge (edge)
  `(:from ,(graphic-vec (edge-start edge))
	:to   ,(graphic-vec (edge-finish edge))
	:color ,(if (edge-color edge)
			  (edge-color edge)
			  "#0b3296")))

(defun transform-node-list (nodes)
  (mapcar #'graphic-node nodes))

(defun transform-edge-list (edges)
  (mapcar #'graphic-edge edges))

(defun nearest-round (val divisor)
  (* (ceiling (/ val divisor)) divisor))

(defun generate-scale (begin end width)
  (list
	:marks (iter (for i from (nearest-round begin 100) to end by 100) ;TODO: choose step automatically
				 (collect `(:y ,(age-to-height i :absolute t)
							   :length ,width
							   :label ,i)))
	:ticks (iter (for i from (nearest-round begin 10) to end by 10)
				 (unless (= (mod i 100) 0)
				   (collect `(:y ,(age-to-height i :absolute t)
								 :label ,(mod i 100)))))
	:now `(:y ,(age-to-height *now* :absolute t)
		   :length ,width
		   :label ,(format nil "~a - сейчас" *now*))))

(defun graphic-tree (nodes)
  (let ((size (get-tree-size nodes)))
	`(:nodes ,(transform-node-list nodes)
	  :edges ,(transform-edge-list (build-edges nodes))
	  :size ,(graphic-vec size)
	  :background ,(generate-scale
					 *start-year*
					 (+ *start-year*
						(height-to-age (vec-y size)))
							(vec-x size)))))


(defun draw-tree ()
  (kin-templates:tree (graphic-tree (place-nodes (build-tree)))))

(defun draw-view-tree ()
  (kin-templates:view-tree 
	`(:tree ,(graphic-tree (place-nodes (build-tree))))))

(defun person-change (pers)
  (kin-templates:person-changer
	(append (graphic-person pers) `(:names ,(get-fields 'person)))))

(defun wedding-change (marr man wife children)
  (kin-templates:wedding-changer
	`(,@(graphic-marriage marr)
	  :man ,(if man (simple-graphic-person man))
	  :man-id ,(m-man marr)
	  :wife ,(if wife (simple-graphic-person wife))
	  :wife-id ,(m-wife marr)
	  :children ,(mapcar #'simple-graphic-person children)
	  :child-ids ,(m-children marr)
	  :names ,(get-fields 'marriage))))

(defun upload-finished (file)
  (kin-templates:upload-finished `(:file ,file)))

(defun refresh-tree ()
  (kin-templates:refresh-tree))

(defun refresh-node (pers)
  (kin-templates:refresh-node 
	`(:id ,(id pers)
		  ,(kin-templates:card (append (graphic-person pers)
									   '(:body-only t))))))

(defun person-card (pers)
  (kin-templates:card (simple-graphic-person pers)))

(defun draw-new-items ()
  (kin-templates:new-items))

