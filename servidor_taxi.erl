-module(servidor_taxi).
-export([servidor/3, inicio/0, paro/0, desplegarRegistroClientes/0, desplegarCentrales/0]).

servidor(Centrales, RegistroClientes, NumServicio) ->
	process_flag(trap_exit, true),
	receive
		{respuesta_central, Nombre_Central, {X, Y}, PID_Central} ->
			io:format("~ts Se recibió una solicitud de una central con PID ~p que quiere registrarse.~n", ["☐",PID_Central]),
			case centralYaExiste(Centrales, Nombre_Central) of
				no_existe ->
					link(PID_Central),
					PID_Central ! {respuesta_servidor, si},
					io:format("~ts La central con PID ~p ha sido registrada exitosamente con el nombre ~p.~n", ["◼︎",PID_Central, Nombre_Central]),
					servidor(lists:append(Centrales, [{Nombre_Central, {X, Y}, PID_Central}]), RegistroClientes, NumServicio);
				si_existe ->
					PID_Central ! repetido,
					io:format("~ts No se pudo registrar a la central con PID ~p, pues ya existía una central con el nombre ~p.~n", ["✖︎",PID_Central, Nombre_Central]),
					servidor(Centrales, RegistroClientes, NumServicio)
			end;
		{pedir_cliente, Nombre_Cliente, {X, Y}, PID_Cliente} ->
			io:format("~ts Se recibió una solicitud de taxi de ~p con localización ~p.~n", ["☐",Nombre_Cliente, {X, Y}]),
			CentralesOrdenadas = lists:sort(fun({_, Dist1, _}, {_, Dist2, _}) -> Dist1 =< Dist2 end, listaDeCentralesConDistancia(Centrales, {X, Y})),
			case solicitarTaxi(CentralesOrdenadas) of
				{Nombre_Central, PID_Taxi, Tipo, Placas} ->
					PID_Cliente ! {NumServicio, PID_Taxi, Tipo, Placas},
					PID_Taxi ! {PID_Cliente, {X, Y}},
					io:format("~ts Se le asignó un taxi de tipo ~p con placas ~p de la central ~p al cliente ~p.~n", ["◼︎",Tipo, Placas, Nombre_Central, Nombre_Cliente]),
					servidor(Centrales, agregarRegistro(RegistroClientes, {Nombre_Cliente, Nombre_Central, Tipo, Placas}), NumServicio + 1);
				no_taxis_disponibles ->
					PID_Cliente ! no_taxis_disponibles,
					io:format("~ts No hay taxis disponibles para asignarle al cliente ~p.~n", ["✖︎",Nombre_Cliente]),
					servidor(Centrales, agregarRegistro(RegistroClientes, {Nombre_Cliente, no_taxis_disponibles}), NumServicio)
			end;
		{'EXIT', PID_Central, _} ->
			io:format("~ts La central con PID ~p ya no está funcionando.~n", ["▷",PID_Central]),
			servidor(eliminarCentral(Centrales, PID_Central), RegistroClientes, NumServicio);
		paro ->
			paro;
		obtener_registro_clientes ->
			io:format("~p~n", [RegistroClientes]),
			servidor(Centrales, RegistroClientes, NumServicio);
		obtener_centrales ->
			io:format("~p~n", [Centrales]),
			servidor(Centrales, RegistroClientes, NumServicio)
	end.

centralYaExiste([], _) -> no_existe;
centralYaExiste([{Nombre, _, _} | _], Nombre) -> si_existe;
centralYaExiste([_ | Resto], Nombre) -> centralYaExiste(Resto, Nombre).

listaDeCentralesConDistancia([], _) -> [];
listaDeCentralesConDistancia([{Nombre, {X1, Y1}, PID} | Resto], {X2, Y2}) ->
	Dist = math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)),
	[{Nombre, Dist, PID} | listaDeCentralesConDistancia(Resto, {X2, Y2})].

eliminarCentral([{_, _, PID_Central} | Resto], PID_Central) -> Resto;
eliminarCentral([PrimerElem | Resto], PID_Central) -> [PrimerElem | eliminarCentral(Resto, PID_Central)].

solicitarTaxi([]) -> no_taxis_disponibles;
solicitarTaxi([{Nombre_Central, _, PID_Central} | Resto]) ->
	PID_Central ! solicitar_taxi,
	receive
		{si, {PID_Taxi, Tipo, Placas}} -> {Nombre_Central, PID_Taxi, Tipo, Placas};
		no -> solicitarTaxi(Resto)
	end.

agregarRegistro(RegistroClientes, {Nombre_Cliente, Nombre_Central, Tipo, Placas}) -> lists:append(RegistroClientes, [{Nombre_Cliente, Nombre_Central, Tipo, Placas}]);
agregarRegistro(RegistroClientes, {Nombre_Cliente, no_taxis_disponibles}) -> lists:append(RegistroClientes, [{Nombre_Cliente, no_taxis_disponibles}]).

% FUNCIONES DE INTERFACE.

% Crea el proceso del servidor de taxis y lo asocia al nombre "servidor_taxi". Esta función es ejecutada por el administrador del servidor de taxis.
inicio() -> register(servidor_taxi, spawn(servidor_taxi, servidor, [[], [], 0])).

% Termina el proceso del servidor de taxis. Esta función es ejecutada por el administrador del servidor de taxis.
paro() -> servidor_taxi ! paro.

% Despliega una lista que contiene información de los servicios que se han solicitado. Esta función es ejecutada por el administrador del servidor de taxis.
desplegarRegistroClientes() -> servidor_taxi ! obtener_registro_clientes.

% Despliega una lista que contiene información sobre las centrales de taxis activas. Esta función es ejecutada por el administrador del servidor de taxis.
desplegarCentrales() -> servidor_taxi ! obtener_centrales.