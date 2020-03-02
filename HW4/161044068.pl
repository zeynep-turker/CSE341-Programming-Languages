flight(istanbul,ankara).
flight(ankara,istanbul).

flight(istanbul,izmir).
flight(izmir,istanbul).

flight(istanbul,antalya).
flight(antalya,istanbul).

flight(istanbul,gaziantep).
flight(gaziantep,istanbul).

flight(istanbul,van).
flight(van,istanbul).

flight(istanbul,rize).
flight(rize,istanbul).

flight(izmir,isparta).
flight(isparta,izmir).

flight(isparta,burdur).
flight(burdur,isparta).

flight(edirne,edremit).
flight(edremit,edirne).

flight(edremit,erzincan).
flight(erzincan,edremit).

flight(antalya,konya).
flight(konya,antalya).

flight(antalya,gaziantep).
flight(gaziantep,antalya).

flight(ankara,konya).
flight(konya,ankara).

flight(ankara,van).
flight(van,ankara).

flight(van,rize).
flight(rize,van).

distance(istanbul,ankara,351).
distance(ankara,istanbul,351).

distance(istanbul,izmir,328).
distance(izmir,istanbul,328).

distance(istanbul,antalya,481).
distance(antalya,istanbul,481).

distance(istanbul,gaziantep,847).
distance(gaziantep,istanbul,847).

distance(istanbul,van,1262).
distance(van,istanbul,1262).

distance(istanbul,rize,968).
distance(rize,istanbul,968).

distance(izmir,isparta,309).
distance(isparta,izmir,309).

distance(isparta,burdur,25).
distance(burdur,isparta,25).

distance(edirne,edremit,915).
distance(edremit,edirne,915).

distance(edremit,erzincan,736).
distance(erzincan,edremit,736).

distance(antalya,konya,192).
distance(konya,antalya,192).

distance(antalya,gaziantep,592).
distance(gaziantep,antalya,592).

distance(ankara,konya,227).
distance(konya,ankara,227).

distance(ankara,van,920).
distance(van,ankara,920).

distance(van,rize,373).
distance(rize,van,373).


%PART1%
route(X,Y) :- flight(X,Y).
route(X , Y) :- findRoute(X , Y , []).
findRoute(X , Y , _) :- flight(X , Y).
findRoute(X , Y , L) :- \+ member(X , L),flight(X , Z),findRoute(Z , Y , [X|L]).


%PART2%
:-dynamic (minDistance/2).      % dynamic variable.

sroute(X,Y,Distance) :- findMinimumDistance(X,Y,Distance).

%X den başlayıp Y yi bulana kadar bütün kombinasyonlar bulunur.Daha sonra en kısa mesafe return edilir.
findMinimumDistance(X, Y,Distance):-walkAround(X),minDistance([Y|RPath], Dist)->reverse([Y|RPath], _),Distance is round(Dist).

% Xden başlayarak bütün mümkün şehirleri gezer.
%Xin ziyaret edilmemiş komşu şehirleri ziyaret eder ve en kısa yolu günceller.
walkAround(X, Path, Distance) :- distance(X, Y, D),not(member(Y, Path)),shorterDistance([Y,X|Path], Distance+D),walkAround(Y,[X|Path],Distance+D).	   
%Oluşan bütün kombinasyonları sıfırlar ve tekrar Xden başlayarak gezintiye başlar.
walkAround(X) :- retractall(minDistance(_,_)),walkAround(X,[],0).              
walkAround(_).

%yeni distance depolanmış olandan daha küçükse minDistance güncellenir.

shorterDistance([P|Path], Distance) :- minDistance([P|Path], D), !, Distance < D,retract(minDistance([P|_],_)),assert(minDistance([P|Path], Distance)).
shorterDistance(Path, Distance) :- assert(minDistance(Path,Distance)).

%PART3%
% knowledge base
enroll(a,102).
enroll(a,108).
enroll(b,102).
enroll(c,108).
enroll(d,341).
enroll(e,455).

when(102,10).
when(108,12).
when(341,14).
when(455,16).
when(452,17).

where(102,z23).
where(108,z11).
where(341,z06).
where(455,207).
where(452,207).
% rules
%S nin enroll deki denk gelen sınıf lecture olur ve bu classın yeri ve zamanı where ve %when ile bulunur. 
schedule(S,P,T) :- enroll(S,X) , where(X,P) , when(X,T).
%P de işlenen classı bulup bu class ın verildiği zaman %when ile bulunur.
usage(P,T) :- where(X, P) , when(X, T).
%X ve %Y class larinin zamanları ve yerleri bulunur ve eşitlik olup olmadıgına bakılır.
conflict(X,Y) :- when(X,T1) , when(Y,T2) , where(X,P1) , where(Y,P2) , (=(P1,P2) ; =(T1,T2)),!.
%X ve %Y öğrencilerinin dersleri bulunur ve bu derslerin yerleri ve zamanları aynı ise true %return edilir.
meet(X,Y) :- enroll(X,A), enroll(Y,B), when(A,T1), when(B,T2), where(A,P1), where(B,P2), =(T1,T2), =(P1,P2),!.


				%PART4%
%%%%%%%%%%%%%element(%E,%S)%%%%%%%%%%%%%%%

%Array boş ise %X bulunmaz ve false %return edilir.
element(_,[]):-false,!.
%Recursive ile kümenin baştan her elemanının %X ile aynı olup olmadıgı kontrol edilir.
%Aynı ise true %return edilir.
%Farklı ise arrayin bir sonraki elemanına bakmak için tekrar fonksiyon çağrılır.
element(X,[X|_]):-true,!. 
element(X,[Y|Tail]):- not(=(X,Y)) ,element(X,Tail).


%%%%%%%%%%%%%union(%S1,%S2,%S3)%%%%%%%%%%%%%

union(S1,S2,S3) :- uninn(S1,S2,S3).
%Eğer iki arrayde boş ise birleşimleri boş kümedir.
uninn([] , [] , []) :- true,!.  	
%Eğer sadece ilk küme boş ise birleşimleri ikinci kümedir.
uninn([] ,S , S) :- true,!.		
%Eğer sadece ikinci küme boş ise birleşimleri birinci kümedir.
uninn(L , [] , L) :- true,!.
%findUnion ile S1 ve S2 nin birleşim kümesi bulunur ve equivalent ile birleşim kümesiyle S3 ün eşit olup olmadıgı kontrol edilir.
uninn(S1,S2,S3) :- 	findUnion(S1,S2,C), equivalent(C,S3),!.		
%Eğer birinci kümenin ilk elemanı ikinci kümenin bir elemanı değilse bu birleşim kümesine eklenir.
%Bu işlem recursive ile 1.küme boş kalıncaya kadar yapılır.
findUnion([H|T] , S , [H|U]) :- \+member(H , S) ,union(T , S , U) . 
%Eğer birinci kümenin elamını ikinci kümenin bir elemanıysa bu eleman birleşime eklenmez.
%Bu işlem recursive ile birinci küme boş kalıncaya kadar devam eder.
findUnion([H|T] , S , U) :- member(H , S) ,union(T , S , U) .

%%%%%%%%%%%%%%intersect(%S1,%S2,%S3)%%%%%%%%%%
intersect(S1,S2,S3) :- submethod(S1,S2,S3).
%Eğer iki arrayde boş ise kesişimleri boş kümedir
submethod([],[],[]) :- true,!.
%Birinci küme boş ise kesişimleri boş kümedir
submethod([],_,[]) :- true,!.	  
%İkinci küme boş ise kesişimleri boş kümedir
submethod(_,[],[]) :-true,!.
%intersct ile intersect kümesi bulunur ve equivalent ile kümelerin aynı olup olmadıgı kontrol edilir.
submethod(S1,S2,S3) :- intersct(S1,S2,C),equivalent(C,S3),!.

%Birinci kümenin elemanı ikinci kümenin içinde bulunuyorsa bu eleman kesişim kümesine eklenir
%ve recursive ile birinci kümenin diğer elemanlarını kontrol eder
%Bu işlem birinci küme boş kalana kadar devam eder
intersct([H|T],L2,[H|I]) :- member(H , L2), intersect(T,L2,I),!.
%Eğer birinci kümenin elemanı ikinci kümede bulunmuyorsa bu eleman kesişim kümesine eklenmez
%Ve recursive ile birinci kümenin diğer elemanlarını kontrol eder
%Bu işlem birinci küme boş kalana kadar devam eder
intersct([H|T],L2,I) :- \+member(H , L2), intersect(T,L2,I).

%%%%%%%%%%%%%%equivalent(%S1,%S2)%%%%%%%%%%%%


equivalent(X,Y) :- a(X,X,Y).
%Eğer iki kümede boş ise true %return edilir
a([],[],[]) :- true.
%Eğer kümelerden biri boş diğeri boş değilse bu kümeler aynı değildir ve false %return edilir
a([],[],_) :- false.
a(_,_,[]) :- false.
%Eğer kümelerin elemanları aynı değilse false %return edilir
%Eğer elemanları aynıysa recursive ile kontrol etmeye devam edilir
a([],_,_) :- true.
a([X|Y],Z,T) :- length(Z,C1),length(T,C2),=(C1,C2),member(X,T),a(Y,Z,T),!.


