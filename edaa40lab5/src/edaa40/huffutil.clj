(ns edaa40.huffutil
    (:require [clojure.java.io :as io])
)


;;
;;  building tree nodes and testing their types
;;

(defn make-leaf
    "Create a leaf node with the specified value and (optional) frequency."
    (
        [value]
    
        {:kind :leaf :value value}
    )
    (
        [value freq]
    
        {:kind :leaf :value value :frequency freq}
    )
)

(defn make-branch
    "Create a branch node with the specified left and right subtrees, and optional frequency."
    (
        [L R]
        
        (if (and (:frequency L) (:frequency R))
            (make-branch L R (+ (:frequency L) (:frequency R)))
            {:kind :branch :left L :right R}
        )
    )
    (
        [L R freq]
        
        {:kind :branch :left L :right R :frequency freq}
    )
)

(defn isleaf? 
    "Is the tree a leaf?"
    [T]
    
    (= (:kind T) :leaf)
)

(defn isbranch? 
    "Is the tree a branch?"
    [T]
    
    (= (:kind T) :branch)
)

;;
;;  serialize and unserialize a tree from a sequence of bytes
;;

(defn serialize-tree
    "Simple serialization of the tree. I Leaf is represented by a 0 followed by the value.
    A branch is represented by a 1 followed by the left subtree followed by the right subtree."
    [T]
    
    (if (isleaf? T)
        (list 0 (:value T))
        (concat (list 1) (serialize-tree (:left T)) (serialize-tree (:right T)))
    )
)

(defn build-tree-from-bytes
    "Parses a sequence of values to construct a tree. Returns a pair "
    [B]
    
    (if (= 0 (first B))
        {:tree (make-leaf (second B)) :remaining-bytes (drop 2 B)}
        (let
            [ 
                {T1 :tree R1 :remaining-bytes}     (build-tree-from-bytes (rest B))
                {T2 :tree R2 :remaining-bytes}     (build-tree-from-bytes R1)
            ]
            
            {:tree (make-branch T1 T2) :remaining-bytes R2}
        )
    )
)

;;
;; conversions from and into bytes
;;

(defn int-to-digits
    "Converts an integer K into a sequence of n 'digits' (represented as integers from 0 to base-1).
    Least significant digit first."
    [K base n]

    (if (<= n 0)
        '()
        (cons (mod K base) (int-to-digits (int (/ K base)) base (dec n)))
    )
)

(defn digits-to-int
    "Converts a sequence of 'digits' (integers from 0 to base-1) into an integer.
    Least significant digit first."
    [Ds base]
    
    (if (empty? Ds)
        0
        (+ (first Ds) (* base (digits-to-int (rest Ds) base)))
    )
)


(defn byte-to-bits
    "Converts a byte (0 to 255) into the eight bits (0 or 1) that represent it,
    least significant bit first."
    [B]
    
    (int-to-digits B 2 8)
)


(defn bytes-to-bits
    "Converts a sequence of bytes (0 to 255) into the bits (0 or 1) that represent it."
    [Bs]
    
    (mapcat byte-to-bits Bs)
)

(defn bits-to-byte
    "Converts a sequence of bits (LSB first) into a byte (0 to 255)."
    [bits]
    
    (digits-to-int bits 2)
)


(defn- bits-to-bytes'
    "Converts a sequence of bits into a vector of bytes."
    [bits Bs]

    (if (empty? bits)
        Bs
        (recur (drop 8 bits) (conj Bs (bits-to-byte (take 8 bits))) )
    )
)


(defn bits-to-bytes
    "Converts a sequence of bits into a sequence of bytes."
    [bits]
    
    (bits-to-bytes' bits [])
)


    
(defn int-to-bytes 
    "Converts an integers into n bytes representing it, LSB first."
    [K n] 

    (int-to-digits K 256 n)
)

(defn bytes-to-int 
    "Converts a sequence of bytes (LSB first) into an integer."
    [Bs]
    
    (digits-to-int Bs 256)
)

(defn byte-s2u
    "Converts a signed byte (-128 to 127) into an unsigned byte (0 to 255)."
    [B]
    
    (if (>= B 0)
        B
        (+ B 256)
    )
)

(defn byte-u2s
    "Converts an unsigned byte (0 to 255) into an signed byte (-128 to 127)."
    [B]
    
    (if (< B 128)
        B
        (- B 256)
    )
)

;;
;;  Huffman codes to and from byte sequence
;;

(defn create-huffman-bytes
    "Serializes a Huffman code (comprising the tree, the length of the original file in bytes, and the bits encoding it) into a sequence of bytes."
    (
        [T N bits]
        
        (concat
            (int-to-bytes N 4)
            (serialize-tree T)
            (bits-to-bytes bits)
        )
    )
    (
        [H]
        
        (create-huffman-bytes (:tree H) (:length H) (:bits H))
    )
)

(defn parse-huffman-bytes
    "Reconstructs a Huffman code (Huffman tree, a file length, and bits) from a serialized Huffman code."
    [Bs]
    
    (let
        [
            N                               (bytes-to-int (take 4 Bs))
            {T :tree R :remaining-bytes}    (build-tree-from-bytes (drop 4 Bs))
        ]
        
        {:tree T :length N :bits (bytes-to-bits R)}
    )
)



;;
;;  simple binary file I/O
;;


(defn slurp-bytes 
    "Returns a byte array containing the content of the specified file."
    [file]
    
    (with-open 
        [
            xin     (io/input-stream file)
            xout    (java.io.ByteArrayOutputStream.)
        ]

        (io/copy xin xout)
        (map byte-s2u (.toByteArray xout))
    )
)

(defn spit-bytes 
    "Stores the specified bytes (sequence of values from 0 to 255) in the specified file."
    [file bytes]
    
    (with-open 
        [
            xin     (java.io.ByteArrayInputStream. (byte-array (map byte-u2s bytes)))
            xout    (io/output-stream file)
        ]

        (io/copy xin xout)
    )
)


