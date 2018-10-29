:- module(path_finding,
	  [
	    buscar_plan_desplazamiento/3
	  ]).


:- dynamic visitado/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%buscar_plan_desplazamiento(+Metas, -Plan, -Destino)
buscar_plan_desplazamiento(Metas, Plan, Destino):-
	retractall(visitado(_)),
	at([agent,me],IdNodo),
	h(IdNodo,Metas,H),
	Frontera=[[IdNodo,[],0,H]],
	buscar(Frontera,Metas,Solucion),
	Solucion=[Destino|_Camino],
	armarPlan(Solucion,Plan).

%armarPlan(+Solucion,-Plan)
%armardel nodo actual:
armarPlan([_NodoActual|[]],[]).
armarPlan([IdNodo|CamInverso],Plan):-
	armarPlan(CamInverso,Plan1),
	append(Plan1,[move(IdNodo)],Plan).

%h(+IdNodo,+Metas,-H)
h(IdNodo,[Meta],H):-
	node(IdNodo,PosNodo,_Conections),
	node(Meta,PosMeta,_Conections2),
	distance(PosNodo,PosMeta,H).
h(IdNodo,[Meta|Ms],H):-
	node(IdNodo,PosNodo,_Conections),
	node(Meta,PosMeta,_Conections2),
	distance(PosNodo,PosMeta,H1),
	h(IdNodo,Ms,H2),
	menorH(H1,H2,H).

%menorH(+H1,+H2,-Menor)
menorH(H1,H2,H1):-H1<H2.
menorH(_H1,H2,H2).


%buscar(Frontera,+Metas,-Solucion)
buscar(Frontera,Metas,[IdNodo|Camino]):-	
	seleccionar(Nodo,Frontera,_FronteraSinNodo),
	Nodo=[IdNodo,Camino,_G,_F],
	member(IdNodo,Metas).
	
buscar(Frontera,Metas,Solucion):-
	seleccionar(Nodo,Frontera,FronteraSinNodo),
	assert(visitado(Nodo)),
	vecinosV(Nodo,Metas,Vecinos),
	agregar_frontera(Vecinos,FronteraSinNodo,NewFront),
	eliminarRepetidos(NewFront,FrontSinRep),
	buscar(FrontSinRep,Metas,Solucion).


%seleccionar(+Nodo,+Frontera,-FronteraSinNodo)
seleccionar(Nodo,[Nodo|FrontSinNodo],FrontSinNodo).

%vecinosV(+Nodo,+Metas,-Vecinos)
vecinosV(Nodo,Metas,Vecinos):-
	Nodo=[IdNodo,Camino,G,_F],
	node(IdNodo,_Pos,Conections),
	findall([IdNodoSuc,[IdNodo|Camino],GSuc,FSuc],
		(member([IdNodoSuc,Costo],Conections),
		GSuc is G +Costo,
		noCaminoOVisitadoMayor(IdNodoSuc,GSuc,Camino),
		h(IdNodoSuc,Metas,H),
		FSuc is GSuc +H),
		Vecinos).

%noCaminoOVisitadoMayor(+IdNodoSuc,+GSuc,+Camino)
noCaminoOVisitadoMayor(IdNodoSuc,GSuc,Camino):-
		not(member(IdNodoSuc,Camino)),
		visitado([IdNodoSuc,_,G2,_]),!,
		GSuc<G2,
		retractall(visitado([IdNodoSuc,_,_,_])).
		
noCaminoOVisitadoMayor(IdNodoSuc,_GSuc,Camino):-
		not(member(IdNodoSuc,Camino)).

%agregar_frontera(Vecinos,Frontera,NewFront)
agregar_frontera([],Frontera,Frontera).
agregar_frontera([Vec|Vecinos],Frontera,FronteraOrdenada):-
	insertarOrdenado(Vec,Frontera,FrontConVec),
	agregar_frontera(Vecinos,FrontConVec,FronteraOrdenada).

%insertarOrdenado(+Nodo,+Lista,-ListaConNodordenado)
insertarOrdenado(Nodo,[],[Nodo]):-!.
insertarOrdenado(Nodo,[F|Fs],[Nodo|[F|Fs]]):-
	Nodo=[_IdNodo1,_Cam1,_G1,F1],
	F=[_IdNodo2,_Cam2,_G2,F2],
	F1<F2,!.
insertarOrdenado(Nodo,[F|Fs],[F|FrontOrd]):-
	insertarOrdenado(Nodo,Fs,FrontOrd).

%eliminarRepetidos(+Frontera,-FronteraSinRepetidos).
eliminarRepetidos([],[]).
eliminarRepetidos([Nodo|Fs],[Nodo|FAux]):-
	Nodo=[IdNodo,_Cam1,_G1,_F],
	sacar(IdNodo,Fs,FrontSinId),
	eliminarRepetidos(FrontSinId,FAux).

%sacar(+IdNodo, +ListaConNodo,  -ListaSinNodo).	
sacar(_IdNodo,[],[]).
sacar(IdNodo,[[IdNodo,_,_,_]|Fs],Front):-
	!,sacar(IdNodo,Fs,Front).
sacar(IdNodo,[N|Fs],[N|Front]):-
	sacar(IdNodo,Fs,Front).

