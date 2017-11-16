-module(taxi). 
-export([crear_taxi/3, inicio_lista_centrales/0]).
-import('matriz', [genera_nodo/1]).

crear_taxi(Tipo, Placas, NombreCentral)->
    lista_centrales ! {crear_taxi, {Tipo,Placas,NombreCentral}}.

% taxi
servicio_taxi(Tipo,Placas,PID_Central) ->
		receive
			{PID_Cliente, {_,_}} ->
				Espera = wait(),
				io:fwrite("El taxi con placas ~s esta esperando por ~p ~n", [Placas,Espera]),
				receive
					{PID_Cliente, cancelar} ->
						io:fwrite("El taxi con placas ~s fue cancelado por cliente con PID ~p ~n", [Placas,PID_Cliente]),
						PID_Central ! {respuesta_taxi, servicio_cancelado},
						PID_Central ! {nuevo_taxi, {self(), Tipo, Placas}},
						servicio_taxi(Tipo,Placas,PID_Central)
                    io:fwrite("El taxi con placas ~s fue cancelado por cliente con PID ~p ~n", [Placas,PID_Cliente]),
				after Espera ->
					io:fwrite("El taxi con placas ~s hace servicio ~n", [Placas]),			
					PID_Cliente ! {taxi,llega},
					PID_Central ! {respuesta_taxi, servicio_ofrecido}
				end
		end.

lista_centrales(ListaCentrales) ->
    receive
        {crear_taxi, {Tipo, Placas, NombreCentral}} ->
			case buscar(NombreCentral, ListaCentrales) of
				indefinido ->
					io:fwrite("La central no existe ~n"),
					lista_centrales(ListaCentrales);
				PID_Central ->
					PID_Taxi = spawn(fun() -> servicio_taxi(Tipo, Placas, PID_Central) end),
					PID_Central ! {nuevo_taxi, {PID_Taxi, Tipo, Placas}},
					lista_centrales(ListaCentrales)
			end;
		{respuesta_central, NombreCentral, {X,Y}, PID_Central} ->
			lista_centrales(ListaCentrales ++ [{NombreCentral, {X,Y}, PID_Central}])
    end.

wait() -> rand:uniform(5000).

buscar(Nombre, [{Nombre, _, PID_Central}|_]) ->
    PID_Central;
buscar(Quien, [_|T]) ->
    buscar(Quien, T);
buscar(_, _) ->
    indefinido.

inicio_lista_centrales() ->
    register(lista_centrales, spawn(fun() -> lista_centrales([]) end)).