(ns a2.treasure
	(:gen-class))

(require '[clojure.string :as str])

; // code to add breakpoint starts here ======================
(defn contextual-eval [ctx expr]
    (eval                                           
        `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)] 
             ~expr)))
(defmacro local-context []
    (let [symbols (keys &env)]
        (zipmap (map (fn [sym] `(quote ~sym)) symbols) symbols)))
(defn readr [prompt exit-code]
    (let [input (clojure.main/repl-read prompt exit-code)]
        (if (= input ::tl)
            exit-code
             input)))
;;make a break point
(defmacro break []
  `(clojure.main/repl
    :prompt #(print "debug=> ")
    :read readr
    :eval (partial contextual-eval (local-context))))
; // code to add breakpoint ends here ========================


(def treasureMap [])

(defn printDataArray
	[items]
	(if(empty? items)
		[]
		(do
			(println(str/join "" (first items)))
			(printDataArray (rest items)))))

(defn printData
	[firstLine data]
	(println firstLine)
	(printDataArray data)
	(println "\n"))

(defn getElementAtIndex
	[mapData, indices]
	(nth (nth mapData (first indices)) (last indices)))

(defn updateTreasureMap
	[mapData, indices, character]
	(assoc mapData (first indices) (assoc (nth mapData (first indices)) (last indices) character))
	)

(defn traceSolution
	[mapData, solutionList]
	(if(empty? solutionList)
		mapData
		(do
			(traceSolution (updateTreasureMap mapData (first solutionList) "+") (rest solutionList))))
	)

(defn getNeighboursIndex
	[mapData, rowPos, colPos]
		(cond
			(and(= rowPos 0)(= colPos 0))(do
				; top left elemenet
				[
					[0 1]
					[1 0]
				])
			(and(= rowPos 0)(= colPos (- (count (nth mapData 0)) 1)))(do
				; top right element
				[
					[0 (- (count (nth mapData 0)) 2)] 
					[ 1 (- (count (nth mapData 0)) 1)]
				])
			(and(= rowPos (- (count mapData) 1))(= colPos 0))(do
				; bottom left element
				[
					[(- (count mapData) 2) 0] 
					[(- (count mapData) 1) 1]
				])
			(and(= rowPos (- (count mapData) 1))(= colPos (- (count (nth mapData 0)) 1)))(do
				; bottom right element
				[
					[(- (count mapData) 1) (- (count (nth mapData 0)) 2)] 
					[(- (count mapData) 2) (- (count (nth mapData 0)) 1)]
				])
			(= rowPos 0)(do
				; first row
				[
					[0 (- colPos 1)]
					[(+ rowPos 1) colPos] 
					[0 (+ colPos 1)] 
				])
			(= colPos 0)(do
				; first col
				[
					[(- rowPos 1) colPos]
					[rowPos (+ colPos 1)]
					[(+ rowPos 1) colPos] 
				])
			(= rowPos (- (count mapData) 1))(do
				; last row
				[
					[rowPos (- colPos 1)] 
					[(- rowPos 1) colPos]
					[rowPos (+ colPos 1)] 
				])
			(= colPos (- (count (nth mapData 0)) 1))(do
				; last col
				[	
					[(- rowPos 1) colPos] 
					[rowPos (- colPos 1)]
					[(+ rowPos 1) colPos] 
				])
			:else (do
				; cells which are in between
				[
					[rowPos (- colPos 1)] 
					[(- rowPos 1) colPos] 
					[rowPos (+ colPos 1)] 
					[(+ rowPos 1) colPos]
				])
			)
	)

(defn traverseData
	([mapData, rowPos, colPos]
	(def openList (getNeighboursIndex mapData rowPos colPos))
	(traverseData mapData (first openList) (rest openList) "!" [[rowPos colPos]] 0 [])
	)
	([mapData, currIndex, openList, character, solutionList, branchingCount, branchingStack]
		(if(empty? openList)
			(do
				(def neighbours (
					filter(
						fn [x] (or(=(getElementAtIndex mapData x) "-")(=(getElementAtIndex mapData x) "@"))
						)
					(getNeighboursIndex mapData (first currIndex) (last currIndex))
				))
				(if(>(count neighbours) 0)
					(do
						(traverseData
							; update map only if element at currindex is '-'
							(if(=(getElementAtIndex mapData currIndex) "-")
								(updateTreasureMap mapData currIndex character)
								mapData
								)
							(last 
								(vec
									(distinct
										(concat 
											openList 
											neighbours
											)
										)
									)
								)
							(vec
								(butlast 
									(vec (distinct
										(concat 
											openList 
											neighbours
											)
										))
									)
								)
							character
							(if(=(count neighbours) 0)
								(vec(drop-last branchingCount solutionList))
								(conj solutionList currIndex)
								)
							(cond
								(=(count neighbours) 0)
									(last branchingStack)
								(>(count neighbours) 1)
									0
								:else
									(+ branchingCount 1)
								)
							(cond
								(=(count neighbours) 0)
									(conj (vec (drop-last branchingStack)) 0)
								(>(count neighbours) 1)
									(conj (vec (drop-last branchingStack)) 0)
								:else
									(conj (vec (drop-last branchingStack)) (+ branchingCount 1))
								)
							)
						)
					(do
						(if(=(getElementAtIndex mapData currIndex) "@")
							(do
								(printData "===== Found the solution: The result is: ======\n" (traceSolution mapData solutionList))
								)
							(printData "===== Couldn't find the solution: The result is: ======\n" (updateTreasureMap mapData currIndex "!"))
							)
						)
					)
				)
			(do
				(cond
					(=(getElementAtIndex mapData currIndex) "@")(do
						(printData "===== Found the solution: The result is: ======\n" (traceSolution mapData solutionList))
						)
					:else (do
						(if(=(getElementAtIndex mapData currIndex) "-")
							; can go to this index, add its neighbours to openlist and update map
							(do
								(def neighbours (
									filter(
										fn [x] (or(=(getElementAtIndex mapData x) "-")(=(getElementAtIndex mapData x) "@"))
										)
									(getNeighboursIndex mapData (first currIndex) (last currIndex))
								))
								(traverseData 
									(updateTreasureMap mapData currIndex character)
									(last 
										(vec
											(distinct
												(concat 
													openList 
													neighbours
													)
												)
											)
										)
									(vec
										(butlast 
											(vec (distinct
												(concat 
													openList 
													neighbours
													)
												))
											)
										)
									character
									(if(=(count neighbours) 0)
										(vec(drop-last branchingCount solutionList))
										(conj solutionList currIndex)
										)
									(cond
										(=(count neighbours) 0)
											(last branchingStack)
										(>(count neighbours) 1)
											0
										:else
											(+ branchingCount 1)
										)
									(cond
										(=(count neighbours) 0)
											(conj (vec (drop-last branchingStack)) 0)
										(>(count neighbours) 1)
											(conj (vec (drop-last branchingStack)) 0)
										:else
											(conj (vec (drop-last branchingStack)) (+ branchingCount 1))
										)
									)
								)
							)
						)
					)
				)
			)
		)
	)
			

(defn buildTreasureMap
	[data, treasureMap, currIndex]
	(if(empty? data)
		treasureMap
		(do
			(def rowEl(str/split (first data) #""))
			(buildTreasureMap (rest data) (assoc treasureMap currIndex rowEl) (+ currIndex 1)))))

(defn isValid? 
	([treasureMap]
		(isValid? (rest treasureMap) (count (first treasureMap)))
		)
	([treasureMap, size]
		(if(empty? treasureMap)
			true
			(do
				(if(= size (count(first treasureMap)))
					(isValid? (rest treasureMap) (count (first treasureMap)))
					false
				)
			)
		)
	)
)

(defn main
	[]
	(def dataArray(with-open [rdr (clojure.java.io/reader "./map.txt")]
		(reduce conj [] (line-seq rdr))))
	(def data (slurp "./map.txt"))
	(printData "This is my challenge:\n" dataArray)
	(def treasureMap(buildTreasureMap dataArray treasureMap 0))
	(if(isValid? treasureMap)
		(do
			(traverseData (updateTreasureMap treasureMap [0 0] "!") 0 0)
			)
		(print "Invalid Map\n"))
)

(main)