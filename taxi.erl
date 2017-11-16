-module(cliente). 
-export([crear_taxi/3, inicio_lista_centrales/0]).
-import('matriz', [genera_nodo/1]).

% La funcion genera_nodo genera la matriz a donde se conectara, con todo y su hostname
% En este caso, se genera servidor_taxi@HOSTNAME
nodo_servidor() -> genera_nodo("central_taxis").
nodo_cliente() -> genera_nodo("cliente").

crear_taxi(Tipo, Placas, NombreCentral)->
    lista_centrales ! {crear_taxi, {Tipo,Placas,NombreCentral}};

% taxi
servicio_taxi(Tipo,Placas,PID_Central) ->
		receive
			{PID_Cliente, {X,Y}} ->
				Espera = wait(),
				io:fwrite("El taxi con placas ~s esta esperando por ~p ~n", [Placas,Espera]),
				receive 
					{De , cancelar} ->
					{PID_Central, nodo_servidor()} ! {respuesta_taxi, servicio_cancelado},
					PID_Central ! {nuevo_taxi,self(), Tipo, Placas},
					servicio_taxi(Tipo,Placas,PID_Central)
				after Espera ->
					io:fwrite("El taxi con placas ~s hace servicio ~n", [Placas]),			
					{pide_taxi,nodo_servidor()} ! {taxi,llega},
					{PID_Central, servicio_central}! {respuesta_taxi, servicio_ofrecido}
			
		end;
		
lista_centrales(ListaCentrales) ->
    receive
        {crear_taxi, {Tipo, Placas, NombreCentral}} ->
			case buscar(Nombre, ListaCentrales) of
				indefinido ->
					io:fwrite("La central no existe ~n"),
					lista_centrales(ListaCentrales);
				PID_Central ->
					PID_Taxi = spawn(fun() -> servicio_taxi(Tipo, Placas, PID_Central)end),
					PID_Central ! {nuevo_taxi, {PID_Taxi, Tipo, Placas}},
					lista_centrales(ListaCentrales)
			end;
		{respuesta_central, Nombre, {X,Y}, PID_Central} ->
				lista_centrales(ListaCentrales ++ {Nombre, {X,Y}, PID_Central})
    end;

wait() -> random:uniform(5000);

busca(Nombre, [{Nombre, Location, PID_Central}|_]) ->
    PID_Central; 
busca(Quien, [_|T]) ->
    busca(Quien, T);
busca(_, _) ->
    indefinido;

inicio_lista_centrales() ->
    register(lista_centrales, spawn(fun() -> lista_centrales([])end));

