/**
Yonathan Bettan 302279138 yonibettan@gmail.com Alon Kwart 201025228 kwart@campus.technion.ac.il
*/

/***
@descr Family rules.
@author Tomer
@date Long time ago
*/

/**
@form male(Name).
@constraints
  @unrestricted Name.
@descr Person with Name is a male.
*/
male(terah).
male(haran).
male(lot).
male(abraham).
male(ishmael).
male(isaac).
male(bethuel).
male(laban).
male(jacob).
male(iscah).
male(esau).
male(reuben).
male(simeon).
male(levi).
male(judah).
male(dan).
male(naphtali).
male(gad).
male(asher).
male(issachar).
male(zebulun).
male(joseph).
male(benjamin).
male(tomer).
male(nahor).


/**
@form female(Name).
@constraints
  @unrestricted Name.
@descr Person with Name is a female.
*/
female(sarah).
female(hagar).
female(milcah).
female(rebecca).
female(leah).
female(rachel).
female(bilhah).
female(zilpah).
female(dinah).

/**
@form parent(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is the parent of person with Name2.
*/
parent(terah, abraham).
parent(terah, sarah).
parent(terah, haran).
parent(terah, nahor).
parent(nahor, bethuel).
parent(haran, lot).
parent(haran, milcah).
parent(haran, iscah).
parent(milcah, bethuel).
parent(abraham, isaac).
parent(abraham, ishmael).
parent(bethuel,rebecca).
parent(bethuel,laban).
parent(isaac, jacob).
parent(isaac, esau).
parent(laban, rachel).
parent(laban, leah).
parent(sarah, isaac).
parent(hagar, ishmael).
parent(rebecca, jacob).
parent(rebecca, esau).
parent(jacob, dinah).
parent(jacob, reuben).
parent(jacob, simeon).
parent(jacob, levi).
parent(jacob, judah).
parent(jacob, dan).
parent(jacob, naphtali).
parent(jacob, gad).
parent(jacob, asher).
parent(jacob, issachar).
parent(jacob, zebulun).
parent(jacob, joseph).
parent(jacob, benjamin).
parent(leah, dinah).
parent(leah, reuben).
parent(leah, simeon).
parent(leah, levi).
parent(leah, judah).
parent(bilhah, dan).
parent(bilhah, naphtali).
parent(zilpah, gad).
parent(zilpah, asher).
parent(leah, issachar).
parent(leah, zebulun).
parent(rachel, joseph).
parent(rachel, benjamin).

/**
@form mother(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is the mother of person with Name2.
*/
mother(X,Y):- 
	parent(X,Y),
	female(X).

/**
@form father(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is the father of person with Name2.
*/
father(X,Y):- 
	parent(X,Y), 
	male(X).

/**
@form son(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is the son of person with Name2.
*/
son(X,Y):- 
	parent(Y,X), 
	male(X).

/**
@form daughter(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is the daughter of person with Name2.
*/
daughter(X,Y):- 
	parent(Y,X), 
	female(X).

/**
@form spouse(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is a spouse (has a child with)  of person with Name2.
*/
mutual_kid(X,Y,Z):-
	parent(X,Z),
	parent(Y,Z),
	X\=Y.
	
spouse(X,Y):- 
	setof(K,mutual_kid(X,Y,K),_).

/**
@form siblings(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is a siblings of person with Name2.
*/

mutual_parent(X,Y,Z):-
	parent(Z,X),
	parent(Z,Y),
	X\=Y.
siblings(X,Y):- 
	setof(Z,mutual_parent(X,Y,Z),_).

/**
@form brother(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is a brother of person with Name2.
*/

brother(X,Y):- 
	male(X), 
	siblings(X,Y).

/**
@form sister(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is a sister of person with Name2.
*/

sister(X,Y):- 
	female(X), 
	siblings(X,Y).

/**
@form grandfather(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is a grandparent of person with Name2.
*/
grandfather_h(X,Y):- 
	father(X,Z), 
	parent(Z,Y).
grandfather(X,Y):-
	setof(Y,grandfather_h(X,Y),S),
	member(Y,S).

/**
@form grandmother(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is a grandmother of person with Name2.
*/
grandmother_h(X,Y):- 
	mother(X,Z), 
	parent(Z,Y).
grandmother(X,Y):-
	setof(Y,grandmother_h(X,Y),S),
	member(Y,S).
/**
@form uncle(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is an uncle of person with Name2. Uncle is a male family relationship or kinship within an extended or immediate family. An uncle is the brother, half-brother, step-brother, or brother-in-law of one's parent, or the husband of one's aunt.
*/
uncle_h(X,Y):- 
	brother(X,Z),
	parent(Z,Y).
uncle_h(X,Y):-
	male(X),
	spouse(X,Z),
	siblings(Z,U),
	parent(U,Y).
uncle(X,Y):-
	setof(Y,uncle_h(X,Y),S),
	member(Y,S).
	
/**
@form nephew(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is a newphew of person with Name2.
*/
nephew_h(X,Y):- 
	siblings(Y,Z),
	son(X,Z).
nephew(X,Y):-
	setof(Y,nephew_h(X,Y),S),
	member(Y,S).
	
/**
@form cousin(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is a cousin of person with Name2.
*/

cousin_h(X,Y):- 
	parent(Z,X),
	parent(U,Y),
	siblings(Z,U).
cousin(X,Y):-
	setof(Y,cousin_h(X,Y),S),
	member(Y,S).
	
/**
@form successor(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is a successor of person with Name2.
*/
successor_h(X,Y):- 
	parent(Y,X).
successor_h(X,Y):- 
	parent(Z,X),
	successor(Z,Y).
successor(X,Y):-
	setof(Y,successor_h(X,Y),S),
	member(Y,S).
	
/**
@form ancestor(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is an ancestor of person with Name2.
*/
ancestor_h(X,Y):- 
	parent(X,Y).
ancestor_h(X,Y):- 
	parent(X,Z), 
	ancestor(Z,Y).
ancestor(X,Y):-
	setof(Y,ancestor_h(X,Y),S),
	member(Y,S).
/**
@form family(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is family (has any conaction of family tree) of person with Name2.
*/

family_h(X,Y,_):- 
	parent(X,Y); 
	parent(Y,X).
family_h(X,Y,L):-
	(parent(X,Z); parent(Z,X)),
	not(member(Z,L)),
	family_h(Z,Y,[Z|L]).
family(X,Y):-
	setof(Y,family_h(X,Y,[]),S),
	member(Y,S),
	X\=Y.
	
/**
@form bad_spouse(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is a spouse of person with Name2 and they have the same precessor.
*/

mutual_ancestor(X,Y,Z):-
	successor(X,Z), 
	successor(Y,Z).
	
bad_spouse(X,Y):- 
	spouse(X,Y), 
	setof(Z,mutual_ancestor(X,Y,Z),_).
