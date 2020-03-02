(setq keywords 	'("and" "or" "not" "equal" "less" "nil" "list"
"append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp"
"true" "false"))
(setq kw_keywords '("KW_AND" "KW_OR" "KW_ NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST"
"KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF"
"KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE"))
(setq operatorsForLexer '("+" "-" "/" "*"  "(" ")" "**" "\"" "\"" ","))
(setq operatorsForParser '("+" "-" "/" "*" "**" "'" "\"" "\"" ","))
(setq kw_operators '("OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_OP" "OP_CP"
"OP_DBLMULT" "OP_OC" "OP_CC" "OP_COMMA"))
(setq EXPB '("and" "or" "not" "equal" "true" "false"))

(setq EXPI '("set"  "+" "-" "/" "*" "(" ")" "**" "if"   
					"deffun"  "for"  "defvar"   "while"  "exit"))
(setq EXPLISTI '("concat" "append" "null" "'(" "'()" "list"))

(setq indent 0)

(setq idListCount 0)

(setq parseList '())

(setq comments '(";;"))

(defun readFile (fileName)
	;str ile gelen dosyayı açar ve okuyup liste halinde geri döndürür.
	(setq tokenList '())
	(let ((in (open fileName :if-does-not-exist nil)))
	   (when in
	      (loop for line = (read-line in nil)
	      	while line do
	      	(setq line (subseq line 0 (split line 0 #\;)));file daki line da ;; varsa ;; dan sonrasını line dan çıkarırız.
	      	(setq tokenList (cons line tokenList)))
	      (close in)
	   )
	)
	(reverse tokenList)
)
(defun split(str i chr) ;gelen stringde seçilmiş chara kadar olan index i return eder.
	(cond
		((= i (length str)) i)
		((char= (char str i) chr)
			(cond((= i 0) (setq i (+ i 1))))
			(cond((char= chr #\;) (setq i (+ i 2))))
		i)
		((char/= (char str i) chr) (split str (+ i 1) chr)))
)
(defun is_digit (chr) ;Gelen karakterin rakam olup olmadığını kontrol edip t veya nil döndürür.
	(setq ascii (char-code chr))
	(setq isDigit nil)
	(if (and (> ascii 47) (< ascii 58))
		(setq isDigit t))
	isDigit
)

(defun is_alpha (chr) ;Gelen karakterin harf olup olmadığını kontrol edip t veya nil döndürür.
	(setq ascii (char-code chr))	
	(setq isAlpha nil)

	(if (or (and (> ascii 64) (< ascii 91)) (and (> ascii 96) (< ascii 123)))
		(setq isAlpha t))
	isAlpha
)

(defun combineList (nested)	;içiçe (1 (2 3 (4))) gibi olan listeleri tek liste haline getiren fonksiyon.
	(cond
		((null nested) nil)
		((listp (car nested)) (append (combineList (car nested)) (combineList (cdr nested))))
		(t (cons (car nested) (combineList (cdr nested))))
	)
)

(defun getWord (lisst) ;lisst ile gelen satır stringinde whitespace veya ( veya ) karakterleri görene kadar olan string i döndürür.
	(cond 
		((null lisst) nil)

	((not (equal (find  (car lisst) '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout #\( #\) #\") :test #'char=) nil))
		nil)
		(t (append (list (car lisst)) (getWord (cdr lisst))))
	)
)

(defun tokenizer (line) ;gelen satır stringindeki tokenleri tek tek ayrıştırıp liste halinde geri döndürür.
	(setq line (coerce line 'list))
	(cond 
		((null line)
			nil)
		(t 	(setq word (getWord line)) ;whitespace karakterlere veya ( ) operatörlerine kadar olan string i recursive olarak bulup word e  atıyoruz.
			(setq wordSize (list-length word)) ;word un size ını alıyoruz ki token lara eklersek size kadar satırda ilerleyerek devam edelim.
			(setq word (coerce word 'string)) ;word char listesi olarak geliyor ve burda string e çeviriyoruz.
		)
	)
	(cond 
		((null line) nil)
		((equal (car line) nil) nil)
		((not (equal (find (string (car line)) operatorsForLexer :test #'string=) nil))
			(cond
				((and (equal (car line) #\*) (equal (car (cdr line)) #\*))
	; * ve ** operatorlerini kontrol ederek recursive call yapıyoruz. Çünkü harf harf baktığımızdan dolayı bu kontrolü
	; yapmassak eğer ** olduğu durumda ilk * ı bir operatör ve 2. * ı bir operatör olarak alacaktı. Burda bunu engelliyoruz.
			 		(append '("**") (list (tokenizer (cdr (cdr line))))))
				((and (equal (car line) #\-) (> wordSize 1))
	;Negatif sayıları Integer olarak alabilmek ve integer olması için gerekli regular expression u sağlaması için gerekli if kontrolü
					(if (equal (every #'is_digit (string-left-trim "-" word)) t) ; - den sonra gelenler tamamen sayı olmalı harf içermemeli
						(append (list word) (list (tokenizer (nthcdr wordSize line))))
						(append (list (format nil "~A  [Syntax Error !]" word)) (list (tokenizer (nthcdr wordSize line)))) 
					)
				)
				((> wordSize 1)  ; *ab /tmp gibi identifier tanımlamalarına izin vermemek için kontrol yapıyoruz.
					(append (list (format nil "~A  [Syntax Error !]" word)) (list (tokenizer (nthcdr wordSize line)))))

			 	(t (append (list (string (car line))) (list (tokenizer (cdr line)))))
			 	
			)

		)
		
		((equal wordSize 0)  ; tab newline bosluk gibi karakterler olduğunda size 0  hesaplanacaktır ve atlayarak recursive call lara devam ediyoruz.
		; ( ve ) operatörlerinde de  size 0 olacaktır fakat yukarıdaki if kontrolüne gireceği için buraya girmeyecektir.
			(tokenizer (cdr line))
		)
		;*******************
		;comment
		((not (equal (find (string (car line)) comments :test #'string=) nil))
			(append (list word) (list (tokenizer (nthcdr wordSize line))))
		)
		;*******************
		;value
		((equal (every #'is_digit word) nil)   ; kelimenin her karakteri harf ise identifier olabilir. Aksi halde olamaz.
			(cond
				((equal (is_digit (char word 0)) nil) (append (list word) (list (tokenizer (nthcdr wordSize line)))))
				((equal (is_digit (char word 0)) t) (append (list (format nil "~A  [Syntax Error !]" word)) (list (tokenizer (nthcdr wordSize line)))))
			)
		)
		;********************
		;value
		((equal (every #'is_digit word) t)  ; her karakter rakam ise integer sayi olabilir. (Negatif sayı kontrolü operatör condition unda yapılıyor.)
			(cond
				((char/= (char word 0) #\0) (append (list word) (list (tokenizer (nthcdr wordSize line)))))
				((char= (char word 0) #\0)
					(cond 
					((> (length word) 1) (append (list (format nil "~A  [Syntax Error !]" word)) (list (tokenizer (nthcdr wordSize line)))))
					((= (length word) 1)(append (list word) (list (tokenizer (nthcdr wordSize line)))))))
			)
		)
		;*********************
		((and (equal wordSize 1) (or (is_alpha (coerce word 'character)) (is_digit (coerce word 'character))))
		; x veya 5 gibi tek harfli sayı veya identifier ları ekliyoruz. Bu kontrolü ? : gibi identifier tanımlanmasını engellemek için yapıyoruz.
			(append (list (string (car line))) (list (tokenizer (cdr line))))
		)
		;*********************
		;keyword
		((not (equal (find word keywords :test #'string=) nil))
	    ; bosluga kadar okunan kelime keyword lerden biriyse listeye ekliyoruz.
			(append (list word) (list (tokenizer (nthcdr wordSize line))))
		)
		;**********************
		;default
		; Hatalı durumlarda token listesine hatalı olduğu belirtilerek eklenip devam ediliyor.
		(t (append (list (format nil "~A  [Syntax Error !]" word)) (list (tokenizer (nthcdr wordSize line))))) 
	)
)

(defun lexer (filename)
	;lexer işlemlerini yapar.Dosyayı okuyup listeyi alır ve satır satır tokenizer fonksiyonuna göndererek token listesini oluşturur.
	(setq listoffile (readFile filename))
	(setq size (list-length listoffile))
	(setq tokenList (combineList (mapcar #'tokenizer listoffile)))

	tokenList
)
(defun findIndex (lisst token i)
	(cond
		( (equal lisst nil) i)
		( (equal (car lisst) token) i)
		(t (findIndex (cdr lisst) token (+ i 1))))
)
(defun findIndex2 (temp)
	(cond
		 ((equal temp 0) 7)
		 ((equal temp 1) 8)))
	
(defun gppinterpreterlex (filename) ;Token ları tipleri ile birlikte ekrana sıralı şekilde basan print fonksiyonudur.
	(setq tokenList (lexer filename))
	(setq temp 1) ;" operatorü için OP_OC ve OP_CC yi belirlemek için kullanılır.
	(setq output '())
	(setq used '())
	(mapcar (lambda (token)

			(cond
				((not (equal (find token operatorsForLexer :test #'string=) nil))
					(cond ((equal token "\"")
					(cond ((equal temp 0) (setq temp 1))
							((equal temp 1) (setq temp 0)))))
					(cond
					((equal token "\"")
					(setq used (append (list (nth (findIndex2 temp) kw_operators)) (list token)))
					(setq output (append output (list used)))
					)
					((equal (equal token "\"") nil)
					(setq used (append (list (nth (findIndex operatorsForLexer token 0) kw_operators)) (list token)))
					(setq output (append output (list used)))
					)))
				((not (equal (find token comments :test #'string=) nil))
					(setq used (append (list "COMMENT") (list token)))
					(setq output (append output (list used)))
				)
				((not (equal (find token keywords :test #'string=) nil))
					(setq used (append (list (nth (findIndex keywords token 0) kw_keywords)) (list token)))
					(setq output (append output (list used)))
				)
				((and (equal (length token) 1) (equal (is_digit (character token)) t))
					(setq used (append (list "VALUE") (list token)))
					(setq output (append output (list used)))
				)
				((equal (every #'is_digit token) t)
					(setq used (append (list "VALUE") (list token)))
					(setq output (append output (list used)))
				)
				((equal (search "Syntax Error" token) nil)
					(setq used (append (list "IDENTIFIER") (list token)))
					(setq output (append output (list used)))
				)
				(t (setq output (list "ERROR")))
			)
		)
	tokenList)
	output
)
(defun writeToFile (stream)
	"liste halinde sıralanmış bir grup parse edilmiş elemanları dosyaya yazan fonksiyon"
	(cond
		((null parseList) nil)
		((equal (car parseList) nil) nil)
		((equal (car parseList) "(") ; ilk eleman ( ise tab sayısını arttırarak yazacağımız için bu duruma özel şart yazdım.
			(incf indent)
			(format stream "~v@{~A~:*~}" (+ indent idListCount) #\tab) ; çoklu şekilde print yapabilmek için yazılan format komutu (arka arkaya verilen sayı kadar tab yazıyor)
	   		(format stream "(") ; gerekli sayıda  tab yapıldıktan sonra ( dosyaya yazılır.
	   		(terpri stream)
	   		(setq parseList (cdr parseList)) 
	   			; recursive şekilde parseList gezilir. Parametre olarak verildiğinde nedense nil olarak okuyordu.
				; Ben de çözüm olarak global yaptım ve recursive de azaltarak base case i sağladım.
	   		(writeToFile stream)
		)
		((equal (car parseList) ")") ; ) görünce de  dosyaya  ) yazıp tab sayısını azaltılır.
			(format stream "~v@{~A~:*~}" indent #\tab)
	   		(format stream ")")
	   		(terpri stream)
	   		(decf indent)
	   		(setq idListCount 0)
	   		(setq parseList (cdr parseList))
	   		(writeToFile stream)
		)
		((equal (car parseList) "IDLIST")
			(cond
				((equal (car (cdr parseList)) "(")  ; IDLIST den sonra ( geldiği durumda  IDLIST -> (IDLIST)  kuralı için kontrol
					(format stream "~v@{~A~:*~}" (+ indent idListCount) #\tab)
			   		(format stream "IDLIST")
			   		(terpri stream)
			   		(setq parseList (cdr parseList))
	   				(writeToFile stream)
				)
				(t ; diğer durumlarda içiçe ID ler basılacağı için her seferinde ID lere özel tab arttırması yapılacak. 
					;Bu yüzden idListCount diye ayrı bir count tutuldu.
					(format stream "~v@{~A~:*~}" (+ indent idListCount) #\tab)
			   		(format stream "IDLIST")
			   		(terpri stream)
			   		(format stream "~v@{~A~:*~}" (+ indent idListCount 1) #\tab)
			   		(format stream "ID")
			   		(terpri stream)
			   		(format stream "~v@{~A~:*~}" (+ indent idListCount 2) #\tab)
			   		(format stream (car (cdr (cdr parseList))))
			   		(terpri stream)
			   		(incf idListCount)
			   		(setq parseList (cdr (cdr (cdr parseList))))
	   				(writeToFile stream)
				)
			)
		)
		((equal (car parseList) "ID")  ; ID görünce gerekli tab sayısı kadar +  içiçe olan ID ler için belirlenen tab sayısı kadar tab bırakılarak dosyaya yazılır.
			(format stream "~v@{~A~:*~}" (+ indent idListCount) #\tab)
	   		(format stream "ID")
	   		(terpri stream)
	   		(format stream "~v@{~A~:*~}" (+ indent idListCount 1) #\tab)
	   		(format stream (car (cdr parseList)))
	   		(terpri stream)
	   		(setq parseList (cdr (cdr parseList)))
			(writeToFile stream)
		)
		((equal (car parseList) "null")
			(format stream "~v@{~A~:*~}" (+ indent idListCount) #\tab)
	   		(format stream "null")
	   		(terpri stream)
	   		(setq parseList (cdr parseList))
	   		(writeToFile stream)
		)
		((equal (car parseList) "VALUES") ; sayı görünce  içiçe tablar yaparak sayı değerini dosyaya yazar.
			(format stream "~v@{~A~:*~}" (+ indent idListCount) #\tab)
	   		(format stream "VALUES")
	   		(terpri stream)
	   		(format stream "~v@{~A~:*~}" (+ indent idListCount 1) #\tab)
	   		(format stream "IntegerValue")
	   		(terpri stream)
	   		(format stream "~v@{~A~:*~}" (+ indent idListCount 2) #\tab)
	   		(format stream (car (cdr (cdr parseList))))
	   		(terpri stream)
	   		(setq parseList (cdr (cdr (cdr parseList))))
	   		(writeToFile stream)
		)
		(t  ; diğer durumlar için o anki tab sayısı kadar tab yapılıp  yazma işlemi yapılır.
			(format stream "~v@{~A~:*~}" (+ indent idListCount) #\tab)
	   		(format stream (car parseList))
	   		(terpri stream)
	   		(setq parseList (cdr parseList))
	   		(writeToFile stream)
		)
	)
)


(defun helper (tokens stream)
	" recursive olarak tüm tokenleri sırayla gezerek parseList i doldurur ve gerekli durumlarda dosyaya da recursive olarak yazması için writeToFile fonksiyonunu çağırır."
	(cond ((null tokens) nil)
		(t (setq token (car tokens))) ; işlem kolaylığı açısından o anki token token değişkeninde tutuldu.
	)

	(cond 
		((null tokens) nil)
		((null token) nil)
		((equal (isThere (car token) kw_operators) t) ; token in tipine göre şartlar kontrol edilir ve gerekli rule için işlemler yapılır.
			(cond
				((not (equal (find (car (cdr token)) operatorsForParser :test #'string=) nil)) ; ( veya ) haricindeki operatörler'den biri ise EXPI dir.
					(setq parseList (cons "EXPI" parseList))
					(setq parseList (append parseList (list (car (cdr token)))))
				)
				((equal (car (cdr token)) "(") ; ( görüldüğünde o ana kadar yapılmış parse işlemini dosyaya yazar. 
											;	Çünkü parse tree'de ( den sonra başka bir rule a geçilir ve 1 tab içeri girilir.
					(writeToFile stream)
					(setq parseList '("("))
				)
				((equal (car (cdr token)) ")") ; ) görüldüğünde de o operasyon biter ve bir tab geri gidilir. Bu yüzden parseList boşaltılmak için dosyaya yazılır ve sıfırlanır.
					(setq parseList (append parseList (list ")")))
					(writeToFile stream)
					(setq parseList '())
				)
			)
			(helper (cdr tokens) stream)
		)
		((equal (isThere (car token) kw_keywords) t)
			(cond
				((not (equal (find (car (cdr token)) EXPI :test #'string=) nil)) ; gelen keyword EXPI EXPB veya EXPLISTI keyword lerinden herhangi biri olabilir. Kontrol ediyoruz.
					(setq parseList (cons "EXPI" parseList))
					(setq parseList (append parseList (list (car (cdr token)))))
				)
				((not (equal (find (car (cdr token)) EXPB :test #'string=) nil))
					(setq parseList (cons "EXPB" parseList))
					(setq parseList (append parseList (list (car (cdr token)))))
				)
				((not (equal (find (car (cdr token)) EXPLISTI :test #'string=) nil))
					(setq parseList (cons "EXPLISTI" parseList))
					(setq parseList (append parseList (list (car (cdr token)))))
				)
			)
			(helper (cdr tokens) stream)
		)
		((equal (car token) "IDENTIFIER")
			(cond
				((and (equal (car parseList) "(") (equal (car (car (cdr tokens))) "IDENTIFIER")) 
				; ID den önce ( varsa ve sonra da ID geliyorsa IDLIST 'dir yani (x y) gibi devam etmektedir.
					(setq parseList (cons "IDLIST" parseList))
					(setq parseList (append parseList '("IDLIST") '("ID") (list (car (cdr token)))))
					(writeToFile stream)
					(setq parseList '())
				)
				((or (equal (car (car (cdr tokens))) "IDENTIFIER") (> idListCount 0))
					; bir sonraki  token de ID ise veya bir IDLIST 'in içinde isek bu şarta girer. (x y z t)
					(setq parseList (cons "IDLIST" parseList))
					(setq parseList (append parseList '("ID") (list (car (cdr token)))))
					(writeToFile stream)
					(setq parseList '())
				)

				((and (equal (car parseList) "(") (equal (car (cdr (car (cdr tokens)))) ")")) ; tek ID li bir IDLIST ise -> (x)
					(setq parseList (cons "IDLIST" parseList))
					(setq parseList (append parseList '("IDLIST") '("ID") (list (car (cdr token)))))
				)
				(t
					(setq parseList (append parseList '("ID") (list (car (cdr token)))))
				)
			)
			(helper (cdr tokens) stream)
		)
		((equal (car token) "VALUE") ; sayı ise
			(setq parseList (append parseList '("VALUES") '("IntegerValue") (list (car (cdr token)))))
			(helper (cdr tokens) stream)
		)
		(t (helper (cdr tokens) stream))
	)
)
(defun isThere(value listt);bir liste de bir elemanın olup olmadıgını kontrol eder.
      (cond
       ((null listt) nil)
       ((equal value (car listt)) t)
       ((consp (car listt)) (or (isThere value (car listt))
                            (isThere value (cdr listt))))
       (t (isThere value (cdr listt)))))
(defun parser (tokenList)
	(with-open-file (stream "161044068.tree" :direction :output :if-does-not-exist :create :if-exists :supersede)
   		(format stream "; DIRECTIVE: parse tree")
   		(terpri stream)

   		(format stream "START")
   		(terpri stream)
   		(incf indent)
   		(format stream "~v@{~A~:*~}" indent #\tab)
   		(format stream "INPUT")
   		(terpri stream)
   		(incf indent)
   		(helper tokenList stream)
	)
	(setq idListCount 0)
	(setq indent 0)
	(setq parseList '())
)
(defun gppinterpreter(file)
	(setq lexerlist (gppinterpreterlex file))
	(cond
		((equal (car lexerlist) "ERROR")
			(print "SYNTAX_ERROR Expression not recognized"))
		(t (print "Syntax OK.")(parser lexerlist))
	)
)


(gppinterpreter "helloworld.g++")