;;; -*- Mode: Lisp -*-
;Edoardo 856450
;;;; mst-lisp

(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *vertex-keys* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *neib* (make-hash-table :test #'equal))
(defparameter *figli* (make-hash-table :test #'equal))

(defun memb_bool_old (elem list)
  (if (null list)
      NIL
    (if (not (null (member elem list)))
      T
    NIL)))

(defun memb_bool (elem list)
  (if (null list)
      NIL
    (if (not (null (member elem list :test #'equal)))
      T
    NIL)))

(defun clear_for_graph (graph-id)
  (progn
    (maphash 
     (lambda (k v)
       (progn
         (null v)
         (if
             (memb_bool (first k) (list graph-id))
             (remhash k *vertex-keys*)
           )
         )
       ) 
     *vertex-keys*)

    (maphash 
     (lambda (k v)
       (progn
         (null v)
         (if
             (memb_bool (first k) (list graph-id))
             (remhash k *previous*)
           )
         )
       ) 
     *previous*)

    (maphash 
     (lambda (k v)
       (progn
         (null v)
         (if
             (memb_bool k (list graph-id))
             (remhash k *visited*)
           )
         )
       ) 
     *visited*)
    )
  )

(defun print_hash (myHash)
  (maphash 
   (lambda (k v) 
     (format t "key: ~S, value: ~S ~%" k v)) 
   myHash))

(defun stampa_lista_debug (lis)
  (progn
    (mapcar (lambda (v) (format t "~S~%" v)) lis)
    T))

(defun is-graph (graph-id)
  (cond
   ((null graph-id)
    (error "Input NIL nella is-graph"))
   ((not (null (gethash graph-id *graphs*)))
    graph-id)
   (T NIL)))

(defun new-graph (graph-id)
  (cond
   ((null graph-id)
    (error "Input NIL nella new-graph"))
   (T
    (setf (gethash graph-id *graphs*) graph-id))))

(defun delete-graph (graph-id)
  (progn
    (clear_for_graph graph-id)
    (maphash 
     (lambda (k v)
       (progn
         (null v)
         (if 
             (memb_bool k (list graph-id)) 
             (remhash k *graphs*))
         )
       )
     *graphs*)
    (maphash 
     (lambda (k v)
       (progn
         (null v)
         (if
             (memb_bool graph-id (list (second k)))
             (remhash k *vertices*))
         )
       )
     *vertices*)
    (maphash 
     (lambda (k v)
       (progn
         (null v)
         (if
             (memb_bool graph-id (list (second k)))
             (remhash k *arcs*))
         )
       ) 
     *arcs*)
    NIL))

(defun new-vertex (graph-id vertex-id)
  (progn
    (new-graph graph-id)
    (if
        (or (not (atom vertex-id)) (null vertex-id))
        (error "VERTEX-ID in input INVALIDO")
      (setf 
       (gethash 
        (list 'vertex graph-id vertex-id) 
        *vertices*) 
       (list 'vertex graph-id vertex-id)))))

(defun graph-vertices (graph-id)
    (let ((kvs ()))
      (maphash 
       (lambda (k v) 
         (if 
             (memb_bool (second k) (list graph-id)) 
             (push v kvs))) 
       *vertices*) 
      kvs))

(defun new-arc (graph-id vtid1 vtid2 &optional (weight 1))
  (if
      (memb_bool vtid1 (list vtid2))
      NIL
    (progn
      (new-graph graph-id)
      (new-vertex graph-id vtid1)
      (new-vertex graph-id vtid2)
      (if
          (not (numberp weight))
          (error "weight deve essere un numero")
        )
      (if
          (not 
           (null 
            (gethash 
             (list 'arc graph-id vtid2 vtid1) 
             *arcs*)))
          (remhash (list 'arc graph-id vtid2 vtid1) *arcs*)
        )
      (setf 
       (gethash (list 'arc graph-id vtid1 vtid2) *arcs*) 
       (list 'arc graph-id vtid1 vtid2 weight)))))

(defun graph-arcs (graph-id)
  (let ((kvs ()))
      (maphash 
       (lambda (k v)  
         (if 
             (memb_bool (second k) (list graph-id)) 
             (push v kvs))) 
       *arcs*) kvs))

(defun graph-vertex-neighbors (graph-id vertex-id)
  (remove
   NIL
   (append
    (mapcar
     (lambda (v)
       (if
           (memb_bool (third v) (list vertex-id))
           v))
     (graph-arcs graph-id))
    (mapcar
     (lambda (v)
       (if (memb_bool 
            (fourth v) 
            (list vertex-id)) 
           (list 
            'arc 
            graph-id 
            vertex-id 
            (third v) 
            (fifth v))))
     (graph-arcs graph-id)))))

(defun graph-vertex-adjacent (graph-id vertex-id)
  (mapcar
   (lambda (v)
     (if
         (memb_bool vertex-id (list (third v)))
         (list 'vertex graph-id (fourth v))
       (list 'vertex graph-id (third v))
         )
     )
   (graph-vertex-neighbors graph-id vertex-id)))

(defun graph-print (graph-id)
  (progn
    (mapcar 
     (lambda (v) 
       (format t "~S~%" v)) 
     (graph-vertices graph-id))
    (mapcar 
     (lambda (v) 
       (format t "~S~%" v)) 
     (graph-arcs graph-id))
    T))

;ARRAY

(defun add_to_array (elem array-input pos)
  (progn
    (if
      (>= pos (array-total-size array-input)) 
      (add_to_array
       elem 
       (adjust-array 
        array-input 
        (+ 20 (array-total-size array-input))) 
       pos)
    (setf (aref array-input pos) elem))
    NIL))

(defun leggi_pos_array (array-input pos)
  (if
      (>= pos (array-total-size array-input))
      (error "LETTURA OLTRE LA DIMENSIONE DELL'ARRAY")
    (aref array-input pos)))

(defun scambio_array (array-input pos1 pos2)
  (progn
    (if 
        (>= pos1 (array-total-size array-input)) 
        (error "POS1 INVALIDA"))
    (if 
        (>= pos2 (array-total-size array-input)) 
        (error "POS2 INVALIDA"))
    (setf 
     (aref array-input 0) 
     (leggi_pos_array array-input pos1)))
    (setf 
     (aref array-input pos1) 
     (leggi_pos_array array-input pos2))
    (setf 
     (aref array-input pos2) 
     (leggi_pos_array array-input 0))
    (setf (aref array-input 0) NIL)
    T)

;HEAP

(defun new-heap (heap-id &optional (capacity 42))
  (setf 
   (gethash heap-id *heaps*) 
   (list 
    'heap 
    heap-id 
    0 
    (make-array capacity :ADJUSTABLE T) 
    (make-hash-table :test #'equal))))

(defun heap-id (heap-id)
  (second (gethash heap-id *heaps*)))

(defun heap-size (heap-id)
  (third (gethash heap-id *heaps*)))

(defun heap-actual-heap (heap-id)
  (fourth (gethash heap-id *heaps*)))

(defun heap-actual-hash (heap-id)
  (fifth (gethash heap-id *heaps*)))

(defun heap-delete (heap-id)
  (progn
    (remhash heap-id *heaps*) T))

(defun inc_hs (heap-id)
  (progn
    (setf (gethash heap-id *heaps*)
          (list
           'heap
           (second (gethash heap-id *heaps*))
           (+ (heap-size heap-id) 1)
           (heap-actual-heap heap-id)
           (heap-actual-hash heap-id)))
    NIL))

(defun dec_hs (heap-id)
  (progn
    (if 
        (= (heap-size heap-id) 0)
        (error 
         "HEAP è GIA A 0, IMPOSS DECREMENTARLO"))

    (setf (gethash heap-id *heaps*)
          (list
           'heap
           (second (gethash heap-id *heaps*))
           (- (heap-size heap-id) 1)
           (heap-actual-heap heap-id)
           (heap-actual-hash heap-id)))NIL))

(defun heap-empty (heap-id)
  (if
      (= 0 (heap-size heap-id))
      T NIL))

(defun heap-not-empty (heap-id)
  (if
      (> (heap-size heap-id) 0)
      T NIL))

(defun heap-head (heap-id)
  (leggi_pos_array 
   (heap-actual-heap heap-id) 1))

(defun pos_ref_same_value (heap-id pos1 pos2)
  (if
      (memb_bool
       (second 
        (leggi_pos_array 
         (heap-actual-heap heap-id) 
         pos1))
       (list 
        (second 
         (leggi_pos_array 
          (heap-actual-heap heap-id) 
          pos2)))
       ) T NIL))

(defun scambio_entry_hashtable (heap-id pos1 pos2) 
  (if
      (pos_ref_same_value heap-id pos1 pos2)
      T
    (progn
      (setf 
       (gethash 
        (second 
         (leggi_pos_array 
          (heap-actual-heap heap-id) 
          pos1)) 
        (heap-actual-hash heap-id))
       (append 
        (list pos2)
        (remove 
         pos1
         (gethash 
          (second 
           (leggi_pos_array 
            (heap-actual-heap heap-id) 
            pos1)) 
          (heap-actual-hash heap-id)))))
      (setf 
       (gethash 
        (second 
         (leggi_pos_array 
          (heap-actual-heap heap-id) 
          pos2)) 
        (heap-actual-hash heap-id))
       (append 
        (list pos1)
        (remove 
         pos2
         (gethash 
          (second 
           (leggi_pos_array 
            (heap-actual-heap heap-id) 
            pos2)) 
          (heap-actual-hash heap-id)))))
      T)))

(defun scambio_entry_heap (heap-id pos1 pos2)
  (progn
    (if
        (> pos1 (heap-size heap-id))
        (error "POS1 INVALIDA")
        )
    (if
        (> pos2 (heap-size heap-id))
        (error "POS2 INVALIDA")
        )
    (scambio_entry_hashtable heap-id pos1 pos2)
    (scambio_array 
     (heap-actual-heap heap-id) 
     pos1 pos2)))

(defun rev_heapify (heap-id pos)
  (if
      (<= pos 1)
      T
    (if
        (<
         (first 
          (leggi_pos_array 
           (heap-actual-heap heap-id) 
           pos))
         (first 
          (leggi_pos_array 
           (heap-actual-heap heap-id) 
           (+ (floor pos 2) 0)))
         )
        (progn
          (scambio_entry_heap 
           heap-id pos 
           (+ (floor pos 2) 0))
          (rev_heapify 
           heap-id 
           (+ (floor pos 2) 0))
          T
          )
      T)))

(defun heap-insert (heap-id k v)
  (progn
    (if
        (not (numberp k))
        (error "K DEVE ESSERE UN NUMERO")
        )
    (if
        (< k 0)
        (error "K DEVE ESSERE UN NUMERO POSITIVO")
        )
    (if
        (memb_bool 
         'EEE 
         (mapcar 
          (lambda (p) 
            (if
                (= 
                 k 
                 (first 
                  (leggi_pos_array 
                   (heap-actual-heap heap-id) 
                   p)))
                'EEE
              NIL))
          (gethash v (heap-actual-hash heap-id))))
        NIL
      (progn
        (inc_hs heap-id)
        (add_to_array 
         (list k v) 
         (heap-actual-heap heap-id) 
         (heap-size heap-id))
        (if
            (null 
             (gethash 
              v 
              (heap-actual-hash heap-id)))
            (setf 
             (gethash v (heap-actual-hash heap-id)) 
             (list (heap-size heap-id)))
          (setf 
           (gethash v (heap-actual-hash heap-id)) 
           (append 
            (list (heap-size heap-id))
            (gethash v (heap-actual-hash heap-id)))))
        (rev_heapify heap-id (heap-size heap-id))
        T))))

(defun minore_tra_due (num1 num2)
  (if
      (< num1 num2)
      num1
    num2))

(defun min_list (listt)
  (progn
    (if
        (null listt)
        (error 
         "MIN-LIST IMPOSSIBILE PER LISTA VUOTA O NIL"))
    (if
        (= 1 (length listt))
        (first listt)
      (minore_tra_due
       (car listt)
       (min_list (cdr listt))))))

(defun pos_piu_piccolo (heap-id pos_p)
  (cond
   (
    (> pos_p (heap-size heap-id))
    (error "POSZIONE IN INPUT OLTRE LA DIM DELL'HEAP")
    )
   (
    (<= (+ 1 (* pos_p 2)) (heap-size heap-id))
    (cond
     (
      (= 
       (first 
        (leggi_pos_array (heap-actual-heap heap-id) pos_p))
       (min_list 
        (list 
         (first 
          (leggi_pos_array 
           (heap-actual-heap heap-id) pos_p))
         (first 
          (leggi_pos_array 
           (heap-actual-heap heap-id) 
           (* pos_p 2)))
         (first 
          (leggi_pos_array 
           (heap-actual-heap heap-id) 
           (+ 1 (* pos_p 2))))
         )
        )
       )
      pos_p
      )
     (
      (= 
       (first 
        (leggi_pos_array 
         (heap-actual-heap heap-id) 
         (* pos_p 2)))
       (min_list 
        (list 
         (first 
          (leggi_pos_array 
           (heap-actual-heap heap-id) 
           pos_p))
         (first 
          (leggi_pos_array 
           (heap-actual-heap heap-id) 
           (* pos_p 2)))
         (first 
          (leggi_pos_array 
           (heap-actual-heap heap-id) 
           (+ 1 (* pos_p 2))))
         )
        )
       )
      (* pos_p 2)
      )
     (
      T
      (+ 1 (* pos_p 2))
      )
     )
   )
   (
    (<= (* pos_p 2) (heap-size heap-id))
    (if
        (<=
         (first 
          (leggi_pos_array 
           (heap-actual-heap heap-id) 
           pos_p))
         (first 
          (leggi_pos_array 
           (heap-actual-heap heap-id) 
           (* pos_p 2)))
         )
        pos_p
      (* pos_p 2)
      )
    )
   (T pos_p)))

(defun heapify (heap-id pos)
  (let ((kvs ()))
    (if
        (= pos (pos_piu_piccolo heap-id pos))
        T
      (progn
        (push (pos_piu_piccolo heap-id pos) kvs)
        (scambio_entry_heap 
         heap-id 
         pos 
         (pos_piu_piccolo heap-id pos))
        (heapify heap-id (pop kvs))
        T))))

(defun remove_hash_table (heap-id pos_to_rem elem)
  (if
      (= 
       1 
       (length 
        (gethash 
         elem 
         (heap-actual-hash heap-id))))
      (remhash elem (heap-actual-hash heap-id))
    (setf 
     (gethash elem (heap-actual-hash heap-id)) 
     (remove 
      pos_to_rem
      (gethash elem (heap-actual-hash heap-id))))))

(defun add_hash_table (heap-id pos_to_add v)
  (if
      (null (gethash v (heap-actual-hash heap-id)))
      (setf 
       (gethash v (heap-actual-hash heap-id)) 
       (list pos_to_add))
    (setf
     (gethash v (heap-actual-hash heap-id)) 
     (append 
      (list pos_to_add)
      (gethash v (heap-actual-hash heap-id))))))

(defun heap-extract (heap-id)
  (cond
   (
    (= 0 (heap-size heap-id))
    NIL
    )
   (
    (= 1 (heap-size heap-id))
    (let ((kvs ()))
    (progn
      (remove_hash_table
       heap-id
       1
       (second (heap-head heap-id))
       )
      (push (heap-head heap-id) kvs)
      (
       add_to_array
       NIL
       (heap-actual-heap heap-id)
       1
       )
      (dec_hs heap-id)
      T
      )
    (first kvs)
     )
    )
   (
    (< 1 (heap-size heap-id))
    (let ((kvs ()))
      (progn
        (remove_hash_table
         heap-id
         1
         (second (heap-head heap-id))
         )
        (push (heap-head heap-id) kvs)
        (
         add_to_array 
         (leggi_pos_array 
          (heap-actual-heap heap-id) 
          (heap-size heap-id)) 
         (heap-actual-heap heap-id) 
         1
         )
        (
         add_to_array
         NIL
         (heap-actual-heap heap-id)
         (heap-size heap-id)
         )
        (remove_hash_table
         heap-id
         (heap-size heap-id)
         (second (heap-head heap-id))
         )
        (add_hash_table
         heap-id
         1
         (second (heap-head heap-id))
         )
        
        (dec_hs heap-id)
        (heapify heap-id 1)
        T
        )
      (first kvs)))))

(defun pos_coppia_kv (heap-id key value)
  (let ((kvs ()))
    (progn
      (mapcar
       (lambda (posx)
         (if
             (= 
              key 
              (first 
               (leggi_pos_array 
                (heap-actual-heap heap-id) 
                posx)))
             (push posx kvs)))
       (gethash value (heap-actual-hash heap-id))
       )
      (if
          (= 0 (length kvs))
          (error 
           "MODIFY KEY_ELEMENTO_INESIST")
        )
      (if
          (> (length kvs) 1)
          (error "AMBIGUITA HASH TABLE")
        )
      (pop kvs))))

(defun pos_coppia_kv_noerror (heap-id key value)
  (let ((kvs ()))
    (progn
      (mapcar
       (lambda (posx)
         (if
             (= 
              key 
              (first 
               (leggi_pos_array 
                (heap-actual-heap heap-id) 
                posx)))
             (push posx kvs)
           )
         )
       (gethash value (heap-actual-hash heap-id))
       )
      kvs)))

(defun heap-modify-key (heap-id new-key old-key value)
  (cond
   (
    (not (numberp new-key))
    (error "NEW-KEY NON è UN NUMERO")
    )
   (
    (not (numberp old-key))
    (error "OLD-KEY NON è UN NUMERO")
    )
   (
    (not 
     (null 
      (pos_coppia_kv_noerror 
       heap-id 
       new-key 
       value)))
    (error 
     "MOD-KEY CREA COPPIE K,V UGUALI NELL'HEAP")
    )
   (
    (= new-key old-key)
    T
    )
   (
    (< new-key old-key)
    (progn
      (add_to_array 
       (list
        new-key
        value
        )
       (heap-actual-heap heap-id)
       (pos_coppia_kv heap-id old-key value)
       )
      (
       rev_heapify
       heap-id
       (pos_coppia_kv heap-id new-key value)
       )
      )
    )
   (
    (> new-key old-key)
    (progn
      (add_to_array 
       (list
        new-key
        value
        )
       (heap-actual-heap heap-id)
       (pos_coppia_kv heap-id old-key value)
       )
      (
       heapify
       heap-id
       (pos_coppia_kv heap-id new-key value)
       ))))
  T)


(defun rude_print (array numb heap-size)
    (if
        (> numb heap-size)
        'FINE_HEAP
      (progn
        (format t "~S - ~S ~%" numb 
                (leggi_pos_array array numb))
        (rude_print array (+ 1 numb) heap-size))))

(defun heap-print(heap-id)
  (if
      (null (heap-id heap-id))
      NIL
    (progn
      (rude_print 
       (heap-actual-heap heap-id) 
       1 
       (heap-size heap-id))
      T)))

;PRIM

(defun add_to_neib (graph-id vtid1 vtid2 weight)
  (progn
    (if
        (null 
         (gethash (list graph-id vtid1 'd) *neib*))
        (setf 
         (gethash (list graph-id vtid1 'd) *neib*) 
         (list (list weight vtid2)))
      (setf
       (gethash (list graph-id vtid1 'd) *neib*)
       (append
        (gethash (list graph-id vtid1 'd) *neib*)
        (list (list weight vtid2)))))
    (if
        (null 
         (gethash (list graph-id vtid2 's) *neib*))
        (setf 
         (gethash (list graph-id vtid2 's) *neib*) 
         (list (list weight vtid1)))
      (setf
       (gethash (list graph-id vtid2 's) *neib*)
       (append
        (gethash (list graph-id vtid2 's) *neib*)
        (list (list weight vtid1)))))
    NIL))

(defun create_neib (graph-id)
  (maphash
   (lambda (k v)
     (if
         (memb_bool (second k) (list graph-id))
         (add_to_neib 
          (second v) 
          (third v) 
          (fourth v) 
          (fifth v))
         ))
   *arcs*))

(defun delete_neib (aaa)
  (progn
    (null aaa)
    (maphash
     (lambda (k v)
       (progn
         (null v)
         (remhash k *neib*)
         )
       )
     *neib*)))

(defun all_ver_of_graph (graph-id)
    (let ((kvs ()))
      (maphash 
       (lambda (k v)  
         (if 
             (memb_bool (second k) (list graph-id)) 
             (push (third v) kvs)
           )
         ) 
       *vertices*
       ) 
      kvs))

(defun insert_list_MPN (heap-id listt)
  (if
      (null listt)
      T
    (progn
      (heap-insert 
       heap-id 
       MOST-POSITIVE-DOUBLE-FLOAT 
       (car listt))
      (insert_list_MPN heap-id (cdr listt))
      T)))

(defun initialize_heap (graph-id nodo-sorg heap-id)
  (progn
    (heap-insert heap-id 0 nodo-sorg)
    (insert_list_MPN
     heap-id
     (remove 
      nodo-sorg 
      (all_ver_of_graph graph-id)
      )
     )
    (mapcar
     (lambda (v)
       (setf
        (gethash (list graph-id v) *vertex-keys*)
        MOST-POSITIVE-DOUBLE-FLOAT
        )
      )
     (remove 
      nodo-sorg 
      (all_ver_of_graph graph-id)
      )
     )
    (setf
        (gethash (list graph-id nodo-sorg) *vertex-keys*)
        0
        )
    T))

(defun arc_list_for_vertex (graph-id vertex-input)
  (append
   (gethash (list graph-id vertex-input 'd) *neib*)
   (gethash (list graph-id vertex-input 's) *neib*)))

(defun agg_heap_nuovi_archi (graph-id heap-id nuovo_vertice)
  (mapcar
   (lambda (v)
     (if
         (null 
          (memb_bool 
           (second v) 
           (gethash graph-id *visited*)))
         (if
             (< 
              (first v) 
              (gethash 
               (list graph-id (second v)) 
               *vertex-keys*))
             (progn
               (heap-modify-key
                heap-id
                (first v)
                (gethash 
                 (list graph-id (second v)) 
                 *vertex-keys*)
                (second v)
                )
               (setf 
                (gethash 
                 (list graph-id (second v)) 
                 *vertex-keys*) 
                (first v))
               (setf 
                (gethash 
                 (list graph-id (second v)) 
                 *previous*) 
                nuovo_vertice)
               T
               )
          )
         )
    )
   (arc_list_for_vertex graph-id nuovo_vertice)))

(defun ciclo_heap (graph-id heap-id)
  (let ((kvs ()))
  (cond
   (
    (and
     (heap-not-empty heap-id)
     (> 
      MOST-POSITIVE-DOUBLE-FLOAT 
      (first (heap-head heap-id)))
     )
    (progn
      (setf 
       (gethash 
        (list 
         graph-id 
         (second (heap-head heap-id))) 
        *vertex-keys*)
       (first (heap-head heap-id))
       )
      (if
          (null (gethash graph-id *visited*))
          (setf 
           (gethash graph-id *visited*)
           (list (second (heap-head heap-id)))
           )
        (setf 
         (gethash graph-id *visited*)
         (append
          (list (second (heap-head heap-id)))
          (gethash graph-id *visited*)
          )
         )
        )
      (push (second (heap-head heap-id)) kvs)
      (heap-extract heap-id)
      (agg_heap_nuovi_archi graph-id heap-id (pop kvs))
      (ciclo_heap graph-id heap-id)
      )
    )
   (
    (and
     (heap-not-empty heap-id)
     (= 
      MOST-POSITIVE-DOUBLE-FLOAT 
      (first (heap-head heap-id)))
     )
    (progn
      (remhash 
       (list 
        graph-id 
        (second (heap-head heap-id))) 
       *previous*)
      (setf 
       (gethash 
        (list 
         graph-id 
         (second (heap-head heap-id))) 
        *vertex-keys*)
       (first (heap-head heap-id))
       )
      (heap-extract heap-id)
      (ciclo_heap graph-id heap-id)
      T
     )
    )
   (
    (heap-empty heap-id)
    T
    )
   (
    T
   (error 
    "CICLO HEAP NON è ENTRATO IN NESSUNA DELLE 3 COND")
    ))))

(defun mst-prim (graph-id nodo-sorg)
  (progn
    (clear_for_graph graph-id)
    (if
        (not 
         (memb_bool 
          nodo-sorg 
          (all_ver_of_graph graph-id)))
        (error 
         "nodo-sorg non è nodo del grafo inserito")
      )
    (create_neib graph-id)
    (new-heap 'h111)
    (initialize_heap graph-id nodo-sorg 'h111)
    (ciclo_heap graph-id 'h111)
    (delete_neib 'aaa)))

(defun mst-vertex-key (graph-id vertex-id)
  (let ((kvs ()))
    (push 
     (gethash 
      (list graph-id vertex-id) 
      *vertex-keys*) 
     kvs)
    (pop kvs)))

(defun mst-previous (graph-id vertex-id)
  (let ((kvs ()))
    (push 
     (gethash 
      (list graph-id vertex-id) 
      *previous*) 
     kvs)
    (pop kvs)))

;GET

(defun create_figli (graph-id)
  (maphash 
   (lambda (k v)
     (if
         (memb_bool graph-id (list (first k)))
         (if
             (null (gethash (list graph-id v) *figli*))
             (setf 
              (gethash (list graph-id v) *figli*)
              (list (second k))
              )
           (setf 
            (gethash (list graph-id v) *figli*)
            (append
             (list (second k))
             (gethash (list graph-id v) *figli*)
             )
            )
           )
       )
     )
   *previous*))

(defun delete_figli (aaa)
  (progn
    (null aaa)
    (maphash
     (lambda (k v)
       (progn
         (null v)
         (remhash k *figli*)
         )
       )
     *figli*)))

(defun create_arc_to_figli (graph-id nodo-padre)
  (mapcar
   (lambda (v)
     (list 
      'arc
      graph-id
      nodo-padre
      v
      (gethash (list graph-id v) *vertex-keys*))
     ) 
   (gethash (list graph-id nodo-padre) *figli*)
   )
  )

(defun cmp< (x y)
  (cond
   (
    (and
     (memb_bool 'fixnum (list (type-of x)))
     (memb_bool 'fixnum (list (type-of y)))
     )
    (< x y)
    )
   (
    (and
     (memb_bool 'SYMBOL (list (type-of x)))
     (memb_bool 'SYMBOL (list (type-of y)))
     )
    (string< x y)
    )
   (
    (and
     (memb_bool 'fixnum (list (type-of x)))
     (memb_bool 'SYMBOL (list (type-of y)))
     )
    T
    )
   (
    (and
     (memb_bool 'SYMBOL (list (type-of x)))
     (memb_bool 'fixnum (list (type-of y)))
     )
    NIL
    )
   )
  )

(defun sort_list (listt key_to_order)
  (sort 
   (copy-list listt)  
   #'cmp< :key key_to_order)
)

(defun stable_sort_list (listt key_to_order)
  (stable-sort 
   (copy-list listt)  
   #'cmp< :key key_to_order)
)

(defun sorted_figli_of_node (graph-id nodo-padre)
  (stable_sort_list 
   (sort_list
    (create_arc_to_figli graph-id nodo-padre)
    'fourth)
   'fifth))

(defun procedura (graph-id list-archi-figli)
  (if
      (not (null list-archi-figli))
      (append
       (list (first list-archi-figli))
       (procedura 
        graph-id 
        (sorted_figli_of_node 
         graph-id 
         (fourth 
          (first list-archi-figli)))
        )
       (procedura 
        graph-id 
        (cdr list-archi-figli)))))

(defun mst-get (graph-id nodo-source)
  (let ((kvs ()))
    (progn
      (create_figli graph-id)
      (push 
       (procedura
        graph-id
        (sorted_figli_of_node 
         graph-id 
         nodo-source)) 
       kvs)
      (delete_figli 'aaa)
      (pop kvs))))

;;;; end of file mst-lisp