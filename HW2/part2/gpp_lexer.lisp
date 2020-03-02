(defvar keywords 	'("and" "or" "not" "equal" "less" "nil" "list"
"append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp"
"true" "false"))
(defvar kw_keywords '("KW_AND" "KW_OR" "KW_ NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST"
"KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF"
"KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE"))

(setq operators '("+" "-" "/" "*"  "(" ")" "**" "\"" "\"" ","))

(defvar kw_operators '("OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_OP" "OP_CP"
"OP_DBLMULT" "OP_OC" "OP_CC" "OP_COMMA"))

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
		((not (equal (find (string (car line)) operators :test #'string=) nil))
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
	
(defun gppinterpreter (filename) ;Token ları tipleri ile birlikte ekrana sıralı şekilde basan print fonksiyonudur.
	(setq tokenList (lexer filename))
	(setq temp 1) ;" operatorü için OP_OC ve OP_CC yi belirlemek için kullanılır.
	(mapcar (lambda (token)
			(cond
				((not (equal (find token operators :test #'string=) nil))
					(cond ((equal token "\"")
					(cond ((equal temp 0) (setq temp 1))
							((equal temp 1) (setq temp 0)))))
					(cond
						((equal token "\"")
					(format t "Token -> ~A ->> [~A]~%" 
						token (nth (findIndex2 temp) kw_operators)))
						((equal (equal token "\"") nil) 
					(format t "Token -> ~A ->> [~A]~%" token (nth (findIndex operators token 0) kw_operators))))
				)
				((not (equal (find token comments :test #'string=) nil))
					(format t "Token -> ~A ->> [COMMENT]~%" token))

				((not (equal (find token keywords :test #'string=) nil))
					(format t "Token -> ~A ->> [~A]~%" token (nth (findIndex keywords token 0) kw_keywords)))

				((and (equal (length token) 1) (equal (is_digit (character token)) t))
					(format t "Token -> ~A ->> [VALUE]~%" token))
				((equal (every #'is_digit token) t) 
					(format t "Token -> ~A ->> [VALUE]~%" token))
				((equal (search "Syntax Error" token) nil)
					(format t "Token -> ~A ->> [IDENTIFIER]~%" token))
				(t (format t "Token -> ~A ->> [SYNTAX ERROR IN THIS TOKEN !!]~%" (subseq token 0 (split token 0 #\Space))))
			)
		)
	tokenList)
	tokenList
)
(gppinterpreter "helloworld.g++")

