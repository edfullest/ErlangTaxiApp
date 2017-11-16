-module(taxi). 
-export([crear_taxi/3, inicio_lista_centrales/0]).
-import('matriz', [genera_nodo/1]).

crear_taxi(Tipo, Placas, NombreCentral)->
    lista_centrales ! {crear_taxi, {Tipo,Placas,NombreCentral}}.

% taxi
servicio_taxi(Tipo,Placas,PID_Central) ->
        process_flag(trap_exit, true),
        link(PID_Central),
		receive
			{PID_Cliente, {_,_}} ->
				Espera = wait(),
				io:fwrite("~ts El taxi con placas ~s se tardará ~p en llegar al cliente con PID ~p ~n", ["◻︎",Placas,Espera, PID_Cliente]),
				receive
					{PID_Cliente, cancelar} ->
						io:fwrite("~ts El taxi con placas ~s fue cancelado por cliente con PID ~p ~n", ["✖︎",Placas,PID_Cliente]),
						PID_Central ! {respuesta_taxi, servicio_cancelado},
						PID_Central ! {nuevo_taxi, {self(), Tipo, Placas}},
						servicio_taxi(Tipo,Placas,PID_Central)
				after Espera ->
					io:fwrite("~ts El taxi con placas ~s hizo el servicio con exito ~n", ["◼︎",Placas]),			
					PID_Cliente ! {taxi,llega},
					PID_Central ! {respuesta_taxi, servicio_ofrecido}
				end;
            {'EXIT', PID_Central, _} ->
                io:format("~ts La central con PID ~p ya no está funcionando. Taxi con placas ~p va a terminar~n", ["▷",PID_Central, Placas])
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