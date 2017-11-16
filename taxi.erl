-module(taxi). 
-export([crear_taxi/3, inicio_lista_centrales/0]).
-import('matriz', [genera_nodo/1]).

% La funcion genera_nodo genera la matriz a donde se conectara, con todo y su hostname
% En este caso, se genera servidor_taxi@HOSTNAME
% nodo_servidor() -> genera_nodo("central_taxis").
% nodo_cliente() -> genera_nodo("cliente").

crear_taxi(Tipo, Placas, NombreCentral)->
    lista_centrales ! {crear_taxi, {Tipo,Placas,NombreCentral}}.

% taxi
servicio_taxi(Tipo,Placas,PID_Central) ->
		receive
			{PID_Cliente, _} ->
				Espera = wait(),
				io:fwrite("El taxi con placas ~s esta esperando por ~p ~n", [Placas,Espera]),
				receive 
					{PID_Cliente, cancelar} ->
                    io:fwrite("El taxi con placas ~s fue cancelado por cliente con PID ~p ~n", [Placas,PID_Cliente]),
					PID_Central ! {respuesta_taxi, servicio_cancelado},
					PID_Central ! {nuevo_taxi,self(), Tipo, Placas},
					servicio_taxi(Tipo,Placas,PID_Central)
				after Espera ->
					io:fwrite("El taxi con placas ~s hace servicio ~n", [Placas]),			
					PID_Cliente ! {taxi,llega},
					PID_Central ! {respuesta_taxi, servicio_ofrecido}
                end			
		end.
		
lista_centrales(ListaCentrales) ->
    receive
        {crear_taxi, {Tipo, Placas, NombreCentral}} ->
			case busca(NombreCentral, ListaCentrales) of
				indefinido ->
					io:fwrite("La central no existe ~n"),
					lista_centrales(ListaCentrales);
				PID_Central ->
					PID_Taxi = spawn(fun() -> servicio_taxi(Tipo, Placas, PID_Central)end),
					PID_Central ! {nuevo_taxi, {PID_Taxi, Tipo, Placas}},
					lista_centrales(ListaCentrales)
			end;
		{respuesta_central, Nombre, {X,Y}, PID_Central} ->
                io:fwrite("Se recibio solicitud para crear central ~s ~n", [Nombre]),
				lista_centrales(ListaCentrales ++ [{Nombre, {X,Y}, PID_Central}]);
        lista_centrales -> io:fwrite("Las centrales son ~p ~n", [ListaCentrales]),
                           lista_centrales(ListaCentrales)
    end.

wait() -> rand:uniform(5000).

busca(Nombre, [{Nombre, _, PID_Central}|_]) ->
    PID_Central;
busca(Quien, [_|T]) ->
    busca(Quien, T);
busca(_, _) ->
    indefinido.

inicio_lista_centrales() ->
    register(lista_centrales, spawn(fun() -> lista_centrales([])end)).

