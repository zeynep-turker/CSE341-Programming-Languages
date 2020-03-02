; *********************************************
; *  341 Programming Languages                *
; *  Fall 2019                                *
; *  Author: Yakup Genc                       *
; *  Student: Zeynep Nazire YUKSEL            *
; *  No: 161044068                            *
; *********************************************

;; utility functions 
(load "include.lisp") ;; "c2i and "i2c"

(defun read-as-list (filename)
	; Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters).
	(setq llist '())
	(let ((in (open filename :if-does-not-exist nil)))
		(when in
			(loop for line = (read-line in nil)
				while line do
				(setq llist (add_list (splitLine (string-downcase line)) llist)) ;file dan alıdıgım cümleyi boşluklarına ayırıp listeledim.
			)
			(close in)
		)
	)
	(setq temp '())
	(setq temp (addListOfList llist temp 0))

	temp
)
;; -----------------------------------------------------
;; HELPERS
;; *** PLACE YOUR HELPER FUNCTIONS BELOW ***
(defun splitLine(str) ;txt den okunulan line ı boşluklara göre kelimelere ayırır.
	(setq i 0)
	(setq j 0)
	(setq myList '())
	(loop 		
	   (cond	   	
	   		((char= (char str j) #\Space)
	   			(setq myList (append myList (list (subseq str i j))))
	   			(setq i (+ j 1))
	   			(setq j (+ j 1))
   			)
   			((char/= (char str j) #\Space) (setq j (+ j 1)))
	   	)
   	(when (= j (length str)) (setq myList (append myList (list (subseq str i j)))) (return myList) )
	)
)
(defun add_list(line ll) ;iki line ı birbirine ekler.
	(setq newlist (append ll line))
	newlist

)
(defun splitWord(word myList index) ;bir word u karakterlerine ayırır.
	(cond 
		( (= index (length word)) myList)
		( (< index (length word))
			(setq myList (append myList (list (char word index))))
			(splitWord word myList (+ index 1))
		)
	)
)
(defun addListOfList(list temp i) ;kelimelerden oluşan listin içindeki kelimeleri karakterlerine ayırarak
	(cond 							;list in listi yapar.
		((= i (length list)) temp)
		((< i (length list))
			(setq a '())
			(setq list1 (list(splitWord (nth i list) a 0)))
			(setq temp (append temp list1))
			(addListOfList list temp (+ i 1)))

	)
)
(defun createHashMap() ;dictionary2 deki kelimelerden hash map oluşturulur.
	(setq myHash (make-hash-table))
	 	(setq in (open "dictionary2.txt" :if-does-not-exist nil))
	 	(setq i 0)
			(when in
				(loop for line = (read-line in nil)
					while line do
				 (setf (gethash i myHash) (string-downcase line))
				 (setq i (+ i 1))
				)
				(close in)
			)
		myHash
)
(defun encoder (paragraph index encodedList alp) ;paragrafı random bir alfabeyle şifreler.
	(cond
		((< index (length paragraph))
			(setq a '())
			(setq sublist (encodeSubList (nth index paragraph) 0 a alp))
			(setq encodedList (append encodedList (list sublist)))
			(encoder paragraph (+ index 1) encodedList alp))
		((= index (length paragraph)) encodedList)
	)
)
(defun encodeSubList (sublist index myList alp) ;paragrafın alt listlerini şifrelemede yardımcı olur.
	(setq newAlphabet alp)
	(cond 
		((< index (length sublist))
			(setq encodeChar (findİndex (nth index sublist) 0 newAlphabet))
			(setq myList (append myList (list encodeChar)))
			(encodeSubList sublist (+ index 1) myList alp))
		((= index (length sublist)) myList))
)
(defun findİndex (letter index alp) ;şifrelencek kelimenin alfadeki yer indexini bulup şifrelencek alfabedeki char ı return eder.
	(setq alphabet '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
		(cond ((equal (nth index alphabet) letter) (nth index alp))
			((< index 25) (findİndex letter (+ index 1) alp))
		)
)
(defun newalp(myList i) ;random yeni bir alfabe oluşturur.
	(setq alphabet '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
	(cond 
		((< i 26)
			(setq myList (tryRandom myList (nth (random (length alphabet)) alphabet) 0))
			(newalp myList (+ i 1))

		)
		((= i 26) myList)
	)
)
(defun tryRandom (myList rndm i) ;yeni bir alfabe oluşturmak için yardımcı fonksiyon.
	(setq alphabet '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
	(cond
		((= i (length myList)) (setq myList (append myList (list rndm))) myList)
		((equal rndm (nth i myList))
			(setq rndm (nth (random (length alphabet)) alphabet))
			(tryRandom myList rndm 0))
		((< i (length myList)) (tryRandom myList rndm (+ i 1)))
		
	)
)
(defun createDecodeWord(word decodeWord i alp);kelimenin şifresini çözmek için ihtimaller yapar.
	(cond 
		((= i (length word)) decodeWord)
		((/= i (length word))
			(setq c (findİndex (nth i word) 0 alp))
			(setq decodeWord (append  decodeWord (list c)))
			(createDecodeWord word decodeWord (+ i 1) alp)
			)
	)
)
(defun spell-checker-0 (word)
	(setq temp nil)
  	(setq in (open "dictionary2.txt" :if-does-not-exist nil))
		(when in
			(loop for line = (read-line in nil)
				while line do
			    (cond ( (equal (splitWord (string-downcase line) '() 0) word )
			    
			    	(setq temp t)))

			)
			(close in)
		)
		temp	
)
(defun spell-checker-1 (word)
 	(setq myHash (createHashMap))
 	(setq i 0)
 	(loop 
  		(cond 
  			((equal (splitWord (gethash i myHash) '() 0) word)
  				(return t))
  			)
  		(setq i (+ i 1))
   	(when (string= (gethash i myHash) nil) (return nil))
	)

)
;; -----------------------------------------------------
;; DECODE FUNCTIONS

(defun Gen-Decoder-A (paragraph)
	(setq alp (newalp '() 0))
	(decoder-a paragraph 0 '() alp)
)
(defun  decoder-a (paragraph i decodedList alp) ;şifrelenmiş kelimeye random alfabedeki harflerden yerleştirip sözlükte kontrol ettim.
	(setq decode (createDecodeWord (nth i paragraph) '() 0 alp));bu işlem kelimeye bulana kadar devam eder.
	(setq temp (spell-checker-0 decode))
	(setq temp2 (and temp t))
	(cond
		((and (equal temp2 t) (= (+ i 1) (length paragraph))) (setq decodedList (append decodedList (list decode))) decodedList)
		((and (equal temp2 t) (/= i (length paragraph))) (setq decodedList (append decodedList (list decode)))
				( decoder-a paragraph (+ i 1) decodedList alp))
		((equal temp2 nil) (setq alp (newalp '() 0)) (decoder-a paragraph 0 '() alp))

	 )
)
(defun Gen-Decoder-B-0 (paragraph)
  	;you should implement this function
)
(defun Gen-Decoder-B-1 (paragraph)
  	;you should implement this function
)
(defun Code-Breaker (document decoder)
	(print "Encoded Paragraph : ") (write document)
	(print "Decoded Paragraph : ") (write decoder)
)
;; -----------------------------------------------------
;; Test code...

(defun test_on_test_data ()
	(print "....................................................")
	(print "Testing ....")
	(print "....................................................")

	(setq alp (newalp '() 0))
	(print "read-as-list Test for document2.txt")
	(setq doc (read-as-list "document2.txt"))
	(print doc)
	(print "Encoder Test for document2.txt")
	(setq encoderDoc (encoder doc 0 '() alp))
	(print encoderDoc)
	(print "Spell-checker-0 Test for word in test1.txt")
	(setq word1 (read-as-list "test1.txt"))
	(print (spell-checker-0 (nth 0 word1)))
	(print "Spell-checker-1 Test for word in test2.txt")
	(setq word2 (read-as-list "test2.txt"))
	(print (spell-checker-1 (nth 0 word2)))
	(print "Decode Test test3.txt")
	(setq doc (read-as-list "test3.txt"))
	(Code-Breaker doc (Gen-Decoder-A doc)) ;decoder A 4 harf ya da daha az harften oluşan şifreli kelimelerin şifresini çözebilir. 
	;diğer olasılıklar uzun sürer.
)


(test_on_test_data)