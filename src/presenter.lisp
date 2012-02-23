(in-package #:kin-package)


(defun get-page ()
  (kin-templates:tree-page))

(defun graphic-vec (vec)
  `(:x ,(vec-x vec) :y ,(vec-y vec)))

(defun graphic-marriage (marr)
  `(:id ,(marriage-id marr)
	:date ,(marriage-date marr)
	:date-unknown ,(item-date-unknown marr)
	:photo ,(if (marriage-photo marr)
			  (marriage-photo marr)
			  "rings.jpg")
    :surname ,(marriage-surname marr)))

(defun graphic-person (pers)
  `(:id ,(person-id pers)
	:name ,(person-name pers)
	:middle-name ,(person-middle-name pers)
	:surname ,(person-surname pers)
	:age ,(safe-age-to-height (safe-person-age pers))
	:sex ,(symbol-name (person-sex pers))
	:photo ,(if (person-photo pers)
			  (person-photo pers)
			  "nophoto.jpg")
	:date-unknown ,(item-date-unknown pers)
	:death-date-unknown ,(person-death-date-unknown pers)
	:birth ,(person-date pers)
	:death ,(person-death-date pers)))

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


(defun draw-tree ()
; (format nil "~a" `(:nodes ,(transform-node-list (arrange-nodes (build-tree))))))
  (let* ((nodes (place-nodes (build-tree)))
		 (size (get-tree-size nodes)))
	(kin-templates:tree `(:nodes ,(transform-node-list nodes)
						  :edges ,(transform-edge-list (build-edges nodes))
						  :size ,(graphic-vec size)
						  :background ,(generate-scale
										 *start-year*
										 (+ *start-year*
											(height-to-age (vec-y size)))
													   (vec-x size))))))

(defun person-change (pers &key new)
  (kin-templates:person-changer `(,@(graphic-person pers)
								  :new ,new)))

(defun wedding-change (marr man wife children &key new)
  (kin-templates:wedding-changer
	`(,@(graphic-marriage marr)
	  :man ,(if man (simple-graphic-person man))
	  :man-id ,(marriage-man marr)
	  :wife ,(if wife (simple-graphic-person wife))
	  :wife-id ,(marriage-wife marr)
	  :children ,(mapcar #'simple-graphic-person children)
	  :child-ids ,(marriage-children marr)
	  :new ,new)))

(defun upload-finished (file)
  (kin-templates:upload-finished `(:file ,file)))

(defun refresh-tree ()
  (kin-templates:refresh-tree))

(defun refresh-node (pers)
  (kin-templates:refresh-node 
	`(:id ,(person-id pers)
		  ,(kin-templates:card (append (graphic-person pers)
									   '(:body-only t))))))

(defun person-card (pers)
  (kin-templates:card (simple-graphic-person pers)))

(defun draw-new-items ()
  (kin-templates:new-items))
