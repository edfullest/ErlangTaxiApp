-module(central_de_taxis). 
-export([crear_central/2, central_taxis_test/0, listar_taxis/1, listar_servicios/1, inicio/0, parar_central/1]).
-import('matriz', [genera_nodo/1]).

% La funcion genera_nodo genera la matriz a donde se conectara, con todo y su hostname
% En este caso, se genera servidor_taxi@HOSTNAME
nodo_servidor() -> genera_nodo("servidor_taxi").
nodo_taxi() -> genera_nodo("taxi").

% FUNCION INTERFAZ: crear_central
% La funcion crear central toma un nombre y unas coordenadas para mandar esto al servidor_taxi
crear_central(Nombre, {X,Y}) ->
    % Esta central tendra su propio servicio central, con su propia lista de taxis
    PID_Central = spawn(fun () -> servicio_central(Nombre,{X,Y},[], 0, 0) end),
    NodoServidor = nodo_servidor(),
    % Se manda al servidor_taxi la nueva central que se esta creando, para que el servidor
    % taxi tenga registro de ésta
    {servidor_taxi, NodoServidor} ! {respuesta_central, Nombre, {X,Y}, PID_Central},
    io:fwrite("~ts Se hizo la solicitud al servidor de taxis para crear central ~s con PID: ~p ~n", ["☐",Nombre, PID_Central]).

%FUNCION INTERFAZ: lista taxis
listar_taxis(NombreCentral) ->
    centrales ! {listar_taxis, NombreCentral}.

%FUNCION INTERFAZ: lista servicios
listar_servicios(NombreCentral) ->
    centrales ! {listar_servicios, NombreCentral}.

%FUNCION INTERFAZ: para central
parar_central(NombreCentral) ->
    centrales ! {parar, NombreCentral}.

servicio_central(Nombre,{X,Y},ListaTaxis, NumServicios, NumCancelaciones) ->
    NodoServidor = nodo_servidor(),
    monitor_node(NodoServidor, true),
    receive
        %%% MENSAJES RECIBIDOS DE SERVIDOR TAXI %%%

        % Este mensaje sera recibido del servidor de taxis cuando éste ocupe un taxi.
        % Si tiene, consigue el primero y se lo manda al servidor, y si no, le dice que no tiene.
        solicitar_taxi -> case length(ListaTaxis) > 0 of
                                true -> {PID_Taxi, Tipo, Placas} = hd(ListaTaxis),
                                        io:fwrite("~ts EXITO: Servidor Taxi pidió un taxi a la central ~s. La central despachó el taxi con placas ~s .~n", ["✔", Nombre, Placas]),
                                        {servidor_taxi, NodoServidor} ! {si, {PID_Taxi, Tipo, Placas}},
                                        servicio_central(Nombre,{X,Y},tl(ListaTaxis), NumServicios, NumCancelaciones);
                                false -> {servidor_taxi, NodoServidor} ! no,
                                        io:fwrite("~ts FRACASO: Servidor Taxi pidió un taxi a la central ~s, pero esta central no tiene taxis.~n", ["✖︎", Nombre]),
                                        servicio_central(Nombre,{X,Y},[], NumServicios, NumCancelaciones)
                           end;

        % Si el servidor taxi determina que la central que se quiso crear tiene un nombre repetido, entonces
        % se manda el mensaje de error correspondiente
        repetido -> io:fwrite("~ts ERROR: Central ~s con proceso ~p no fue creado, pues ya existe una central de taxis registrada con ese nombre. ~n", ["✖︎✖︎✖︎",Nombre, self()]);
        parar -> io:fwrite("Central ~s con PID ~p va a terminar! ~n", [Nombre, self()]);

        % Servidor taxi manda SI si y solo si se pudo crear la central, es decir, no hay repetidos
        {respuesta_servidor, si} ->   
            % Se manda esta central al nodo taxi para que tenga registro de la central que se acaba de crear
            % Esto ayuda en Taxi, pues asi cualquier Taxi puede crear un nuevo taxi en esa central
            NodoTaxi = nodo_taxi(),
            io:fwrite("~ts Servidor Taxi aceptó la creación de central ~s. ~n", ["◼︎", Nombre]),
            {lista_centrales, NodoTaxi} ! {respuesta_central, Nombre, {X,Y} , self()},
            centrales ! {nueva_central, Nombre,{X,Y}, self()},
            servicio_central(Nombre,{X,Y},ListaTaxis, NumServicios, NumCancelaciones);

        %%% MENSAJES RECIBIDOS DE ALGUN TAXI %%%

        % El nuevo proceso de taxi creado se registra a esta central de taxi en específico. Le manda el taxi y se le agrega a la lista actual
        {nuevo_taxi, {PID_Taxi, Tipo, Placas}} -> 
                io:fwrite("~ts Taxi de placas ~s con PID ~p llegó a la central ~s ~n", ["🚕",Placas, PID_Taxi, Nombre]),
                servicio_central(Nombre,{X,Y},ListaTaxis ++ [{PID_Taxi, Tipo, Placas}], NumServicios, NumCancelaciones);
        % Si el taxi pudo ofrecer el servicio, entonces se aumenta en uno el numero de servicios
        {respuesta_taxi, servicio_ofrecido} -> servicio_central(Nombre,{X,Y},ListaTaxis, NumServicios + 1, NumCancelaciones);
        % Si el taxi NO pudo ofrecer el servicio, entonces se aumenta en uno el numero de cancelaciones
        {respuesta_taxi, servicio_cancelado} -> servicio_central(Nombre,{X,Y},ListaTaxis, NumServicios, NumCancelaciones + 1);
        
        % Estos mensajes TAMBIEN los mandar'a taxi!
        listar_taxis ->  io:fwrite("Central ~s tiene los siguientes Taxis ~p ~n", [Nombre,ListaTaxis]), 
                         servicio_central(Nombre,{X,Y},ListaTaxis, NumServicios, NumCancelaciones);
        listar_servicios -> io:fwrite("Central ~s ha ofrecido ~p servicios ~n", [Nombre,NumServicios]),
                            io:fwrite("A Central ~s le han cancelado ~p servicios ~n", [Nombre,NumCancelaciones]),
                            servicio_central(Nombre,{X,Y},ListaTaxis, NumServicios, NumCancelaciones);

        {nodedown, NodoServidor} -> io:fwrite("~ts El servidor de taxis murió. La central también lo hará ~n", ["☹︎"])
    end.

% Esta funcion va a tener todas las centrales que se han registrado exitosamente
% con servidor taxi
lista_centrales(ListaCentrales) ->
    receive
        {nueva_central, NombreCentral, {X,Y}, PID_Central} ->
            lista_centrales(ListaCentrales ++ [{NombreCentral, {X,Y}, PID_Central}]);
        {listar_servicios, NombreCentral} ->
            case buscar(NombreCentral, ListaCentrales) of
                indefinido ->
                    io:fwrite("La central no existe ~n"),
                    lista_centrales(ListaCentrales);
                PID_Central ->
                    PID_Central ! listar_servicios,
                    lista_centrales(ListaCentrales)
            end;
        {listar_taxis, NombreCentral} ->
            case buscar(NombreCentral, ListaCentrales) of
                indefinido ->
                    io:fwrite("La central no existe ~n"),
                    lista_centrales(ListaCentrales);
                PID_Central ->
                    PID_Central ! listar_taxis,
                    lista_centrales(ListaCentrales)
            end;
        {parar, NombreCentral} ->
            case buscar(NombreCentral, ListaCentrales) of
                indefinido ->
                    io:fwrite("La central no existe ~n"),
                    lista_centrales(ListaCentrales);
                PID_Central ->
                    PID_Central ! parar,
                    lista_centrales(ListaCentrales)
            end
    end.

inicio() ->
    register(centrales, spawn(fun() -> lista_centrales([]) end)).

central_taxis_test() ->
    PID_Central = spawn(fun () -> servicio_central("Nombre", {20,20},[], 0, 0) end),
    PID_Central ! {nuevo_taxi, {self(), 'sedan', 'SRJ 10 04'}},
    PID_Central ! listar_taxis,
    PID_Central ! solicitar_taxi.

buscar(Nombre, [{Nombre, _, PID_Central}|_]) ->
    PID_Central;
buscar(Quien, [_|T]) ->
    buscar(Quien, T);
buscar(_, _) ->
    indefinido.