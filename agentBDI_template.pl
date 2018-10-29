%% Player-Agent BDI
%

:- [disable_writes].

:- [ag_primitives, module_beliefs_update, module_actions_rep_and_projection, module_strips, module_path_finding, extras_for_agents].

:- consult(extras_meta_preds).

:- dynamic intention/1, plan/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%           AGENT	   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%       EXECUTION CYCLE	   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


run:-
      get_percept(Percept),

      ag_name(_AgName),

      %tell('log.txt'),
      update_beliefs(Percept),

      display_ag, nl,!,
      deliberate,  % FUE IMPLEMENTADO DE MANERA QUE SI POSTERIORMENTE FALLA LA OBTENCIÓN DE UN PLAN PARA LA INTENCIÓN
		   % EN PRINCIPIO SELECCIONADA, VUELVE A RECONSIDERAR INTENCIÓN.
		   
      planning_and_execution(Action),
      do_action(Action),
      run,!.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%   1. UPDATING BELIEFS	   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%     FROM PERCEPTIONS	   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% update_beliefs(+Perc)
%
% Se encuentra definido en el módulo 'module_beliefs_update'.

% << DESARROLLADO EN ETAPA 1 >>




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%     2. DELIBERATION      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% deliberate
%
% El agente analiza si continuará con su intención actual, considerando
% deseos de alta prioridad, la factibilidad del plan
% para la intencion actual, si la intención actual fue lograda, etc.
% En caso de no continuar con la intención corriente, establece cual
% será la nueva intención analizando los deseos existentes y
% seleccionando uno de ellos.

deliberate:-
	high_priority(HPDesire, Explanation),	 % Si existe un deseo HPDesire de alta prioridad:
						 % ( <<<CHOICE POINT>>> -- Posibilidad de backtracking )

	not(intention(HPDesire)),        % y no es el caso que coincide con la intención actual,

	write('High-priority Desire: '), write(HPDesire), write(', since '), writeln(Explanation), nl,

	retractall(intention(_)),     % (Estratégicamente colocado luego del "choice point", para que,
	retractall(plan(_)),	      %	 ante la búsqueda de una intención alternativa (por no haberse encontrado un plan
	                              %  para la anterior), la intención anterior se elimina y se hace assert de la nueva).

	assert(intention(HPDesire)),     % se establece HPDesire como intención actual.
	assert(plan([HPDesire])).

deliberate:-
												% Si

	(   not(intention(_)),                     % actualmente no hay intención
	    writeln('There is no intention '), nl
	;                                          % o
	    intention(Int),
	    achieved(Int),                         % la intención corriente fue lograda
	    write('Intention '), write(Int), writeln(' achieved.')
	;					   % o

	    plan([]),                              % el plan para para la intención actual fue consumido
	    writeln('Plan consumed.')
	;                                          % o
	    (
			
	        plan(Plan),
			Plan \= [], 
			not(feasible(Plan))								% el plan corriente se tornó no factible, o
		;

	        not(plan(_))                                  % no hay plan. Esto ocurre si se descubre que el plan actual es no
	                                                      % factible al intentar obtener, sin éxito, el (sub) plan para la
	                                                      % siguiente acción de alto nivel (falla el next_primitive_action).
	    ),
	    writeln('Plan actual no factible.'), nl
	),

	!,
	
	findall(Desire, desire(Desire, _Explanation), Desires),  % Calcular el conjunto de deseos
	write('Deseos: '), writeln(Desires),nl,
	select_intention(NewInt, NewExpl, Desires),   % Seleccionar una intención
	                                              % (deseo que el agente se compromete a lograr)
	                                              % <<<CHOICE POINT>>> (Posibilidad de backtracking)

	write('New Intention: '), write(NewInt), write(', since '), writeln(NewExpl), nl,

	retractall(intention(_)),  % (Estrategicamente colocado luego del "choice point", para que,
	retractall(plan(_)),	   % ante la búsqueda de una intención alternativa (por no haberse encontrado un plan
	                           % para la anterior), la intención anterior se elimina y se asserta la nueva.)

	assert(intention(NewInt)),                    % Recordar la nueva intención seleccionada.
	assert(plan([NewInt])).


deliberate:-
	intention(Int),
	write('Current Intention: '), writeln(Int), nl.
	% Caso contrario, se continúa con la intención y plan corrientes


deliberate:-            % Si llega acá significa que falló el next_primitive_action al planificar la siguiente acción
	                % de alto nivel de un plan factible. Ejecuto nuevamente el delibarate.
	deliberate.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%  2.1. COMPUTING DESIRES  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%% Metas / Deseos %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% desire(-Desire, -Explanation)
%
% Retorna un deseo Desire (actualmente activo de acuerdo a las creencias
% del agente). Asociado al deseo se retorna una explicación,
% especificando las razones por las cuales Desire se considera
% actualmente un deseo. En su forma más básica Explanation puede ser un
% string con una descripción narrada (breve) de dichas razones,
% que luego puede ser impresa por pantalla.

%_____________________________________________________________________
%
% Get treasure at position
%
% Si recuerdo que un tesoro dado se encuentra tirado en el piso, tener
% ese tesoro es una meta.

desire(get([gold, TrName]), 'quiero apoderarme de muchos tesoros!'):-
	at([gold, TrName], _PosTr),
	not(intention(get([gold, TrName]))).

%_____________________________________________________________________
%
% Get Potion at position
%
% Si recuerdo que una potion dada se encuentra tirado en el piso, tener
% ese tesoro es una meta.

desire(get([potion, TrName]), 'quiero apoderarme de muchas pociones!'):-
			at([potion, TrName], _PosTr),			
			not(intention(get([potion, TrName]))).


%_____________________________________________________________________
%
% Liberar oro de tumba
%
%

desire(get([grave, TrName]), 'quiero apoderarme de un oro que esta en una tumba!'):-
	at([grave, TrName], _PosTr),
	not(intention(get([grave, TrName]))),
	tieneOroyYoPotion([grave, TrName]).


%_____________________________________________________________________
%
% Atacar Enemigo
%
% Si recuerdo que un agente enemigo y este esta en rango deseo atacarlo.

desire(atacarEnemigoLejano([agent, TrName]), 'quiero atacar a un enemigo lejano!!!'):-
	at([agent, TrName], _PosTr),
	property([agent,me],home,Hme),
	property([agent,TrName],home,HAgent),
	TrName\=me,
	Hme\=HAgent,
	not(ataqueenDesventaja([agent,me],[agent,TrName])).

%_____________________________________________________________________
%
% Vaciar Home Enemigo
%
% Si recuerdo que un agente enemigo deseo atacarlo.

desire(vaciarHomeEnemigo([home, HAgent]), 'quiero vaciar el home contrario!'):-
	at([home, HAgent], _PosTr),
	property([agent,me],home,Hme),
	Hme\=HAgent,
	has([home,HAgent],[gold,_N]),
	has([agent,me],[potion,_P]).

	
%_____________________________________________________________________
%
% Rest


desire(rest, 'quiero estar descansado'):-
	property([agent, me], life, St),
	St < 100.



%_____________________________________________________________________
%
% Move at Random
%

desire(move_at_random, 'quiero estar siempre en movimiento!').


%_____________________________________________________________________
%
% DejarOroHome
%

desire(get([home,MyHome]), 'tengo muchos oros, deseo dejarlos en mi home'):-
	property([agent,me],home,MyHome),
	at([home,MyHome],_NodoHome),
	findall(Oro,has([agent,me],[gold,Oro]),Oros),
	tieneAlmenos(Oros,s(s(s(0)))).
	
tieneAlmenos(_L,0).
tieneAlmenos([_L1|Ls],s(X)):-
	tieneAlmenos(Ls,X).

ataqueenDesventaja(Me,Enemigo):-
		has(Enemigo,[potion,_]), 
		not(has(Me,[potion,_])),
		has(Me,[gold,_]). %no atacara si el agente tiene al menos una potion y nuestro agente no. 

%
% ACLARACIÓN: Pueden modificarse los deseos considerados, y la
% implementación actual de desire/2, si se lo considera apropiado.


%me interesa solo si tengo una pocion y la tumba un oro no cual es ese oro o pocion
tieneOroyYoPotion(Tumba):-
	has(Tumba,[gold,_N]),!,		
	has([agent,me],[potion,_P]),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% high_priority(-HPDesire, Explanation).
%
% Determina si existe un deseo HPDesire de alta prioridad, es
% decir, tal que implica abandonar la intención actual. En caso de
% existir tal deseo de alta prioridad, lo retorna.
%
% Análogamente al predicado desire/2, asociado al deseo HPDesire de alta
% prioridad se retorna una explicación, especificando las
% razones por las cuales HPDesire se considera un deseo que
% debe adoptarse inmediatamente como intención.
% En su forma más básica Explanation puede ser un string con una
% descripción narrada (breve) de dichas razones, que luego puede ser
% impresa por pantalla.
:- dynamic high_priority/2.


high_priority(rest, 'necesito descansar'):-  % runs low of stamina

	property([agent, me], life, St),
	St < 40, % running low of stamina...

	once(at([inn, _HName], _Pos)),!. % se conoce al menos una posada

high_priority(huir([agent,Bad]), 'Tengo un enemigo cerca y no tengo vida suficiente para defenderme,deseo huir'):-  % runs low of stamina
	property([agent, me], life, St),
	has([agent,me],[gold,_Gold]),
	St < 60, % running low of stamina...
	atPos([agent,me],MyPos),
	atPos([agent,Bad],TargetPos),
	property([agent,me],home,Hme),
	property([agent,Bad],home,HAgent),
	Hme\=HAgent,
	pos_in_attack_range(MyPos, TargetPos),	% si estoy en rango de ataque y no tengo vida tengo que huir.
	once(at([inn, _HName], _Pos)),!.

high_priority(dejarOrosEnHome, 'tengo demasiados oros, tengo que dejarlos en mi home'):-
	has([agent,me],[gold,_]),
	property([agent,me],home,MyHome),
	at([home,MyHome],_NodoHome),
	(
		findall(Oro1,(has([agent,me],[gold,Oro1])),Oros),
		tieneAlmenos(Oros,s(s(s(s(s(s(s(s(s(s(0)))))))))))
	;
		findall(Oro2,(has([agent,AgenteAliado],[gold,Oro2]),property([agent,AgenteAliado],home,MyHome); has([home,MyHome],[gold,Oro2])),OrosAliados),
		tieneAlmenos(OrosAliados,s(s(s(s(s(s(s(s(s(s(s(s(0)))))))))))) ) 
	;
		findall(Oro3,(has([home,HomeEnemigo],[gold,Oro3]),HomeEnemigo\=MyHome),OrosEnemigos),
		tieneAlmenos(OrosEnemigos,s(s(s(s(s(s(s(s(s(s(s(s(0)))))))))))) ), 
		findall(P,(has([agent,AgenteAliado],[potion,P]),property([agent,AgenteAliado],home,MyHome)),Posiones),
		Posiones=[]

		
	).%si tengo 10 o si entre los oros que tengo y los que tienen mis aliados  y el home tenemos 12(mayoria)
	
high_priority(atacarEnemigo([agent,Bad]), 'Tengo un enemigo en rango de ataque ,tengo que defenderme'):-  % runs low of stamina
	atPos([agent,me],MyPos),
	atPos([agent,Bad],TargetPos),
	property([agent,me],home,Hme),
	property([agent,Bad],home,HAgent),
	property([agent,Bad],life,LifeEnemigo),
	LifeEnemigo>0,
	Hme\=HAgent,
	pos_in_attack_range(MyPos, TargetPos),!.

% << TODO: DEFINIR >>
%
% ACLARACIÓN: Puede modificarse la implementación actual de
% high_priority/2, si se lo considera apropiado.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                             %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%  2.2. SELECTING INTENTIONS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                             %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% select_intention(-Intention, -Explanation, +CurrentDesires).
%
% Selecciona uno de los deseos corrientes del agente como intención.
%
% En su forma más básica, define un orden de prioridad entre los deseos
% del agente, seleccionando como intención al primero en este orden
% de prioridad.
%
% Tiene múltiples soluciones. Esto es adrede: si
% posteriormente a la selección de una intención (por parte de este
% predicado) no se encuentra plan para alcanzarla, entonces se obtendrá
% la siguiente intención (mediante backtracking), hasta encontrar una
% viable, o agotarlas a todas.



%_____________________________________________________________________
%
% Rest before commiting to any other desire
%
% Dado que el nivel de stamina es relativamente bajo, se decide ir
% descansar antes de abordar otro deseo.

select_intention(rest, 'voy a recargar antes de encarar otro deseo', Desires):-
	member(rest, Desires),
	property([agent, me], life, St),
	St < 70,!.


%_____________________________________________________________________
%
% Conseguir un objeto que se halla tirado en el suelo
%
% De todos los posibles objetos tirados en el suelo que el agente desea tener,
% selecciono como intención obtener aquel que se encuentra más cerca.

select_intention(get(Obj), 'es el oro o posion más cercano de los que deseo obtener', Desires):-
	findall(ObjPos, (member(get(Obj), Desires),
			 at(Obj, ObjPos)),
		Metas), % Obtengo posiciones de todos los objetos meta tirados en el suelo.
	buscar_plan_desplazamiento(Metas, _Plan, CloserObjPos),
	member(get(Obj), Desires),
        at(Obj, CloserObjPos),!.
%_____________________________________________________________________
%
% Defender El Home Propio
%
% Si no tengo mas objetos libres para levantar y un enemigo intenta atacar mi home, 
% decido ir a atacarlo

select_intention(atacarEnemigoLejano(Enemigo), 'No conozco ningun objeto y mi home esta siendo atacada, deseo ir a defenderla', Desires):-
	member(atacarEnemigoLejano(Enemigo), Desires),
	property([agent,me],home,HomeMe),
	atPos([home,HomeMe],PosHome),
	atPos(Enemigo,TargetPos),
	pos_in_attack_range(PosHome, TargetPos),!.

%_____________________________________________________________________
%
%Liberar un oro que se encuentre en una tumba
%

%select_intention(getOroTumba(Obj), 'es tumba con oro más cercana de las que deseo obtener', Desires):-
%	findall(ObjPos, (member(getOroTumba(Obj), Desires),
%			 at(Obj, ObjPos)),
%		Metas), % Obtengo posiciones de todos las tumbas con oro.
%	buscar_plan_desplazamiento(Metas, _Plan, CloserObjPos),
%	member(getOroTumba(Obj), Desires),
 %       at(Obj, CloserObjPos).		

%_____________________________________________________________________

%Atacar home Enemigo
% Si conosco el home Enemigo y el home enemigo tiene oro
% Selecciono como intencion vaciar el home contrario


select_intention(vaciarHomeEnemigo(HomeEnemigo), 'Si no conosco objetos libres, atacare el home enemigo', Desires):-
	member(vaciarHomeEnemigo(HomeEnemigo), Desires),!.



%_____________________________________________________________________
%
%Atacar un enemigo lejano que esta en rango de ataque de un aliado
%

select_intention(atacarEnemigoLejano(Enemigo), 'no tengo nada mas que hacer por lo que deseo atacar al enemigo que esta en rango de ataque de mi aliado', Desires):-			
	atPos([agent,Aliado],Pos),
	atPos(Enemigo,TargetPos),
	property([agent,me],home,Hme),
	property([agent,Aliado],home,Hme),
	Aliado\=me,
	pos_in_attack_range(Pos, TargetPos),
	member(atacarEnemigoLejano(Enemigo), Desires),!.		
%_____________________________________________________________________
%
%Atacar un enemigo lejano
%

select_intention(atacarEnemigoLejano(Obj), 'no tengo nada mas que hacer por lo que deseo atacar al enemigo mas cercano que vi', Desires):-			
	findall(ObjPos, (member(atacarEnemigoLejano(Obj), Desires),
			 at(Obj, ObjPos), has(Obj,[gold,_G])),
		Metas), 
	buscar_plan_desplazamiento(Metas, _Plan, CloserObjPos),
	member(atacarEnemigoLejano(Obj), Desires),
        at(Obj, CloserObjPos),!.		

%_____________________________________________________________________
%
% Rest
%
% Si no existen objetos que deseen obtenerse, y existe el deseo de
% descansar (stamina por debajo de 100), se decide ir a descansar.

select_intention(rest, 'no tengo otra cosa más interesante que hacer', Desires):-
	member(rest, Desires),!.


%_____________________________________________________________________
%
% Move at random

select_intention(move_at_random, 'no tengo otra cosa más interesante que hacer', Desires):-
	member(move_at_random, Desires).



%
% ACLARACIÓN: Puede modificarse la implementación actual de
% select_intention/3, si se lo considera apropiado.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% achieved(+Intention).
%
% Determina si la intención Intention fue alcanzada. Esto es, si
% se verifica de acuerdo a las creencias del agente.


achieved(rest):-
	property([agent, me], life, St),
	property([agent, me], lifeTotal, MaxSt),
	AlmostMaxSt is MaxSt - 10,
	St > AlmostMaxSt.

achieved(get(Obj)):-
	has([agent, me], Obj).

achieved(get([grave,Tumba])):-
	not(has([grave,Tumba],[gold,_N])).
	
achieved(goto(Pos)):-
	at([agent, me], Pos).

achieved(getOroTumba(Tumba)):-
	not(has(Tumba,[gold,_N])).

achieved(atacarEnemigo(Enemigo)):-
	property(Enemigo, life,0).
	
achieved(atacarEnemigoLejano(Enemigo)):-
	property(Enemigo, life,0).
	
achieved(vaciarHomeEnemigo(HomeEnemigo)):-
		not(has(HomeEnemigo,[gold,_F])).
	
achieved(dejarOrosEnHome):-
		not(has([agent,me],[gold,_P])),
		property([agent,me],lastAction,drop([gold,G])),
		property([agent,me],home,MyHome),
		has(MyHome,G).
achieved(huir(_Enemigo)):-
		achieved(dejarOrosEnHome).
achieved(huir(_Enemigo)):-
		at([agent,me],NodoInn),
		at([inn,_Inn],NodoInn),
		achieved(rest).
achieved(huir(Enemigo)):-
		property(Enemigo,life,0).
		
achieved(dejarTodoElOro):-
		not(has([agent,me],[gold,_P])).
		
%achieved(move_at_random):- 
%	desire(Deseo,_),
%	Deseo\=move_at_random.

		
% << TODO: COMPLETAR DEFINICIóN >>
%
% ACLARACIÓN: Puede modificarse la implementación actual de
% achieved/1, si se lo considera apropiado.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%      3. PLANNING         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%       & EXECUTION        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% planning_and_execution(-Action)
%
% Obtiene la siguiente acción primitiva Action correspondiente al plan
% actual, removiéndola del plan. Nótese que la siguiente acción
% del plan podría ser de alto nivel (no primitiva), en cuyo caso debe
% planificarse hasta llegar al nivel de acciones primitivas.
% (Ver next_primitive_action/3).

planning_and_execution(Action):-

	retract(plan(Plan)),      % Ejecutar siguiente acción del plan.

	write('Following plan: '), writeln(Plan), nl,
	next_primitive_action(Plan, Action, RestOfPlan),
	write('Next action: '), writeln(Action),
	assert(plan(RestOfPlan)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% planify(+HLAction, -Plan)
%
% Define una librería de Planes, es decir, un mapeo de acciones de alto
% nivel (en particular, intenciones) a planes involucrando acciones de
% menor nivel.
%
% Dada una acción HLAction de alto nivel, retorna un plan (involucrando
% acciones de menor nivel) cuya ejecución equivalga al efecto de la
% acción HLAction.
%
% Debe definirse una regla de este predicado por cada acción de alto
% nivel considerada por el agente (incluída las intenciones, que
% constituyen las acciones de más alto nivel).
%
% La planificación de HLAction puede realizarse,
% según el tipo de la acción, de las siguientes maneras:
%
%   a) simplemente especificando en forma "estática" la secuencia
%      (lista) de acciones de menor nivel cuyo efecto equivale al de
%       HLAction.
%
%   b) empleando un algoritmo de búsqueda (por ejemplo el implementado
%      para la etapa 2, permitiendo planificar el desplazamiento a una
%      posición determinada)
%
%   c) empleando el algoritmo de planeamiento STRIPS (cuya
%      implementación es provista por la cátedra), adecuado para
%      realizar planificaciones de metas más complejas, como obtener un
%      tesoro que se encuentra en una tumba.
%
%
% La opción a admite la especificación de planes recursivos, donde en
% particular, la última acción del plan para HLAction es la propia
% HLAction. Esto permite, por ejemplo, especificar que la
% planificación de HLAction es [Action, HLAction], donde Action es
% una acción que acerca al agente al cumplimiento de HLAction. Cuando
% esa siguiente acción sea ejecutada y consumida, el agente vuelve a
% considerar y planificar HLAction, obteniendo la siguiente acción, y
% así siguiendo.
%
% Esta estrategia es ideal para intenciones/acciones que no pueden
% planificarse por completo en un dado instante, resultando apropiado
% establecer en cada turno cual es la siguiente acción para lograrlas
% (por ejemplo, perseguir a un agente para saquearlo).
%
% Este plan recursivo se corta cuando los efectos de HLAction se logran
% (achieved(HLAction)), o cuando falla la planificación de HLAction,
% reflejando que ya no existe plan para la misma.
%
% IMPORTANTE: Este predicado entregará soluciones alternativas (por
% demanda), correspondientes a planes alternativos para la meta
% considerada. Analizar según la acción que se esté planificando,
% si es deseable brindar soluciones alternativas.



planify(get([grave,G]), [getOroTumba([grave,G])]):- !.
planify(get([home,_H]), [dejarOrosEnHome]):- !.

planify(get(Obj), Plan):- % Planificación para obtener de un objeto que yace en el suelo
	at(Obj, Pos),
	Plan = [goto(Pos), pickup(Obj)].


planify(goto(PosDest), Plan):- % Planificación para desplazarse a un destino dado
	buscar_plan_desplazamiento([PosDest], Plan, _MetaLograda),
	!. % Evita la búsqueda de soluciones alternativas para un plan de desplazamiento.


planify(rest, Plan):- % Planificación para desplazarse a un destino dado

	findall(IdNodoInn,(at([inn,_Inn],IdNodoInn),noBaneado(IdNodoInn)),Inns),
	buscar_plan_desplazamiento(Inns,PlanBusqueda,_NodoInn),!,
	append(PlanBusqueda,[stay],Plan).
	
planify(stay, [noop , stay]).                     % Planificación recursiva. En este caso permite repetir indefinidamente
                                                  % una acción (noop) hasta que la intención de alto nivel corriente
                                                  % (rest) sea lograda (achieved/1). Esto se hizo así dado que resulta
                                                  % más simple que predecir de antemano cuantos turnos exactamente debe
                                                  % permanecer el agente para recargarse por completo (nótese que el agente
						  % podría incluso sufrir ataques mientras está en la posada, siendo imposible
						  % planificar de antemano cuantos turnos debe permanecer en la posada para
						  % reponerse por completo)

planify(getOroTumba(Tumba),[goto(NodoTumba),cast_spell(open(Tumba,Potion))]):-
		at(Tumba,NodoTumba),
		has([agent,me],Potion),
		Potion=[potion,_P].

planify(vaciarHomeEnemigo(Home),[goto(NodoHome),cast_spell(open(Home,Potion))]):-
		at(Home,NodoHome),
		has([agent,me],Potion).
%si estabamos atacando a un enemigoen rango de ataque y se nos fue de rango, hay qe seguirlo para completar el ataque
planify(atacarEnemigo(Enemigo),[atacarEnemigoLejano(Enemigo)]):-
	atPos([agent,me],MyPos),
	atPos(Enemigo,TargetPos),
	not(pos_in_attack_range(MyPos, TargetPos)),!.

planify(atacarEnemigo(Enemigo),[cast_spell(sleep(Enemigo,Potion))]):-
		has([agent,me],Potion),
		Potion=[potion,_P],
		(
			has(Enemigo,[potion,_Potion])
		;
			property(Enemigo,life,LyfeEnem),
			property([agent,me],life,MyLyfe),
			LyfeEnem> MyLyfe
		),
		!.
planify(atacarEnemigo(Enemigo),[attack(Enemigo)]).

planify(atacarEnemigoLejano(Enemigo),[atacarEnemigo(Enemigo)]):-
		atPos([agent,me],MyPos),
		atPos(Enemigo,TargetPos),
		pos_in_attack_range(MyPos, TargetPos),!.
planify(atacarEnemigoLejano(Enemigo),[move(Nodo),atacarEnemigoLejano(Enemigo)]):-
		obtenerMejorNodo(Enemigo,Nodo),!.
%nos escontramos en un borde todos los nodos cercanos tienen mayor distancia(caso del rio)
planify(atacarEnemigoLejano(Enemigo),Plan):-
	at(Enemigo,NodoEnemigo),
	buscar_plan_desplazamiento([NodoEnemigo],Plan1,_Dest),!,
	obtenerPrimeros(Plan1,5,SubPlan),
	append(SubPlan,[atacarEnemigoLejano(Enemigo)],Plan).
		

%Si conocemos la posada y nuestra home. Vamos a la posada que esta mas cerca que el home.	
planify(huir(_Enemigo),[goto(Dest),stay]):-
	property([agent,me],home,MyHome),
	at([home,MyHome],NodoHome),
	findall(IdNodoInn,(at([inn,Inn],IdNodoInn),noBaneado(Inn)),Inns),
	buscarSinBack([NodoHome|Inns],_Plan,Dest),
	NodoHome\=Dest.
	
%Decidimos dejar los oros en el home.
planify(huir(_Enemigo),[dejarOrosEnHome]).
	
planify(dejarOrosEnHome,[goto(NodoHome),dejarTodoElOro]):-		
	property([agent,me],home,MyHome),
	at([home,MyHome],NodoHome).
	
planify(dejarTodoElOro,[drop([gold,G]),dejarTodoElOro]):-
	has([agent,me],[gold,G]).

						  
planify(move_at_random, Plan):- % Planificación para moverse aleatoriamente
	findall(Node, node(Node, _, _), PossibleDestPos),
	random_member(DestPos, PossibleDestPos), % Selecciona aleatoriamente una posición destino.
				                 % <<<CHOICE POINT>>> (Posibilidad de backtracking)
	Plan = [goto(DestPos)].


noBaneado(IdNodoInn):-
	time(T),
	property([inn,IdNodoInn],forbidden_entry,[me,T1]),!,
	atPos([agent,me],MyPos),
	atPos([inn,IdNodoInn],PosInn),
	distance(MyPos, PosInn, DistanciaAInn),
	T2 is T + DistanciaAInn,
	T1<T2.
noBaneado(_IdNodoInn).

buscarSinBack(Metas,Plan,Dest):- 	buscar_plan_desplazamiento(Metas,Plan,Dest),!.
	
% << TODO: COMPLETAR DEFINICIóN >>
%
% ACLARACIÓN: Puede modificarse la implementación actual de
% planify/2, si se lo considera apropiado.

obtenerPrimeros([],_,[]).
obtenerPrimeros(_Plan,0,[]).
obtenerPrimeros([Act|Plan],N,[Act|SubPlan]):-
	N1 is N-1,
	obtenerPrimeros(Plan,N1,SubPlan).

obtenerMejorNodo(Enemigo,NodoMenor):-
	atPos(Enemigo,NodoEnemigo),
	at([agent,me],MiNodito),
	node(MiNodito,_Pos,Conections),
	menorNodo(Conections,NodoEnemigo,NodoMenor,Dist),
	atPos([agent,me],MyPos),
	distance(MyPos, NodoEnemigo, MiDistancia),
	Dist<MiDistancia.

menorNodo([Nodo],Enemigo,Nodo,Distancia):-
	node(Nodo,Pos,_C),
	distance(Pos, Enemigo, Distancia),!.

menorNodo([Nodo|Nodos],Enemigo,Nodo2,Dist2):-
	node(Nodo,Pos,_C),
	distance(Pos, Enemigo, Dist1),
	menorNodo(Nodos,Enemigo,Nodo2,Dist2),
	Dist2<Dist1,!.

menorNodo([Nodo|_Nodos],Enemigo,Nodo,Dist1):-
	node(Nodo,Pos,_C),
	distance(Pos, Enemigo, Dist1).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% next_primitive_action(+Plan, -NextAction, -RemainingPlan)
%
% Selecciona y retorna la siguiente acción primitiva del plan de alto
% nivel, además del devolver el plan restante.
%
% Planteo Recursivo:
%
% Sea Plan = [A_1, A_2, ..., A_n]
%
% CB: Si A_1 es primitiva, entonces NextAction = A_1 y RemainingPlan =
% [A_2, ..., A_n].
%
% CR: Si A_1 es de alto nivel, se hallará mediante planify/2 una
% secuencia de acciones de menor nivel [A_1.1, A_1.2, ..., A_1.k], o
% (sub)plan, para A_1, dando lugar a una versión refinada de Plan:
%
%          PlanRef = [A_1.1, A_1.2, ..., A_1.k, A_2, ..., A_n]
%
% Luego se obtiene recursivamente la siguinte acción, pero esta vez para
% PlanRef.
%
% CR 2: Los efectos de A_1 se cumplen en el estado actual del mundo.
% Luego se obtiene recursivamente la siguinte acción, pero esta vez para
% [A_2, ..., A_n].
%
% Observación: A modo de mantener registro de la descomposición de
% acciones de alto nivel en subplanes, la estructura empleada por este
% predicado para representar planes incolucra el uso de "marcadores".
% Concretamente, en CR, PranRef = [A_1.1, A_1.2, ..., A_1.k, [A_1], A_2,
% ..., A_n] donde [A_1] es un marcador indiciando que las acciones que
% se encuentran a la izquierda corresponden al sub-plan para lograr A_1.
% Luego, el propósito del predicado remove_executed_ancestors/2 empleado
% en CB y CR 2 es eliminar los marcadores de acciones de alto nivel
% cuyo sub-plan asociado fue ejecutado por completo.


% CR 2:

next_primitive_action([Action | RestOfPlan], NextAction, RemainingPlan):-
	% Este caso permite, por ejemplo, terminar exitosamente un programa para una HLAction
	% cuando ésta ya fue lograda (debe especificarse achieved/1 para HLAction).

	clause(achieved(Action), _), % Existe especificación de cuándo Action se considera lograda.

	achieved(Action), % Action fue lograda.
	!,
	write('Action '), write(Action), write(' achieved.'),nl,
	remove_executed_ancestors(RestOfPlan, CleanRestOfPlan),
	next_primitive_action(CleanRestOfPlan, NextAction, RemainingPlan).


% CB:

next_primitive_action([Action | RemainingPlan], Action, CleanRemainingPlan):-
	primitive(Action),
	remove_executed_ancestors(RemainingPlan, CleanRemainingPlan),
	!.

% CR:

next_primitive_action([HLAction | RestOfPlan], Action, RemainingPlan):-


        if_fails_do(

	clause(planify(HLAction, _SubPlan), _), % Planificación definida para HLAction.

		    throw_exception((
			  write(HLAction),
			  write(' is undefined. Declare it as primitive or planify it.')
			 ))
		   ),
        !,

	(

	     planify(HLAction, SubPlan)	 % <<<CHOICE POINT>>> (Posibilidad de backtracking)

	     ;

	     (write('Planning for '), write(HLAction), write(' failed.'), nl, fail)
	                                 % Si definitivamente no encuentra plan para la intención seleccionada,
	                                 % luego de buscar planes alternativos (backtracking sobre planify(HLAction, SubPlan)),
				         % selecciona otra intención mediante backtracking en deliberate/0.

	),

	(   last_element(HLAction, SubPlan),
	    append(SubPlan, RestOfPlan, LowerLevelPlan) % Se evita introducir el marcador de super-accion
							% si la acción de alto nivel coincide con la última del subplan.
	;
	    append(SubPlan, [[HLAction]|RestOfPlan], LowerLevelPlan)
	),

	%'High-level action ' HLAction ' expanded into '
	%write(HLAction), write(' -- expanded into -> '), write(SubPlan),nl,
	writeln('          -- expanded into -> '), nl,
	write(LowerLevelPlan), nl, nl,

	next_primitive_action(LowerLevelPlan, Action, RemainingPlan).

% Observación: si en particular Subplan es [] (esto puede
% ocurrir cuando los efectos de HLAction ya valen en
% el estado actual del mundo) entonces ejecuta la siguiente acción de
% RestOfPlan.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% remove_executed_ancestors(+Plan, -CleanPlan)
%
%

remove_executed_ancestors([[_]| Rest], Clean):-
	!,
	remove_executed_ancestors(Rest, Clean).

remove_executed_ancestors(Clean, Clean).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% primitive(ActionName).
%
% Especifica las acciones primitivas del agente, es decir, aquellas que
% no pueden descomponerse en acciones más básicas.

primitive(move(_)).
primitive(pickup(_)).
primitive(drop(_)).
primitive(attack(_)).
primitive(cast_spell(_)).
primitive(noop).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% feasible(+Plan).
%
% Determina si el plan jerárquico Plan es factible de acuerdo a las
% creencias actuales del agente.

feasible(Plan):-
	dynamic_state_rels(Init),
	project(Plan, Init, _Finish).
	% El plan puede ejecutarse con éxito a partir del estado actual. Si alguna de las precondiciones de las
        % acciones del plan ya no se satisfacen (por ejemplo, el tesoro que voy a juntar ya no se encuentra más
        % en la posición que recordaba), entonces project/3 fallará, reflejando que el plan no es factible.

%feasible([Acciones|_Ls]):-
%	property([agent,me],lastAction,Action),
%	Action=[Accion,_],
%	Acciones\=Accion.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%        AGENT SETUP       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%      & REGISTRATION      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- dynamic name/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% start_ag
%
% Solicita la registración al juego, y recuerda su nombre.


start_ag:- AgName = agentBDI,
           agent_init(AgName),
           assert(ag_name(AgName)),
	   agent_reset,
           connect,
           run,
           disconnect.

s:- start_ag.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% start_ag_instance(+InstanceID)
%
% Solicita la registración al juego de una instancia, y recuerda su
% nombre, que será el de la versión original seguido del InstanceID
% entre paréntesis.


start_ag_instance(InstanceID):-
                    AgClassName = agentBDI,
                    AgInstanceName =.. [AgClassName, InstanceID],
		    agent_init(AgInstanceName),
		    assert(ag_name(AgInstanceName)),
		    agent_reset,
		    connect,
		    run,
		    disconnect.

si(InstanceID):- start_ag_instance(InstanceID).













































































