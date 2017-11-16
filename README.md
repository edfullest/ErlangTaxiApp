#PASOS PARA EJECUTAR ACTIVIDAD

#Para comenzar el sistema, se deben iniciar 4 terminales que actuaran como los elementos del sistema.

##1. EN CONSOLA
    erl -sname servidor_taxi -setcookie demo
##1.1 EN ERLANG
    c(servidor_taxi).
    servidor_taxi:inicio().

##2. EN CONSOLA
    erl -sname central_de_taxis -setcookie demo
##2.1 EN ERLANG
    c(central_de_taxis).
    central_de_taxis:inicio().

##3. EN CONSOLA
    erl -sname taxi -setcookie demo
##3.1 EN ERLANG
    c(taxi).
    taxi:inicio().

##4 EN CONSOLA
    erl -sname cliente -setcookie demo
#$4.1 EN ERLANG
    c(cliente).

#SEEDS

##5. EN ERLANG | Terminal central_de_taxis
    central_de_taxis:crear_central("Central 1", {20,20}).
    central_de_taxis:crear_central("Central 2", {0,0}).
    central_de_taxis:crear_central("Central 3", {10,10}).

##6. ERLANG | Terminal taxi
    taxi:crear_taxi(sedan, 'srj 10 04', "Central 1").
    taxi:crear_taxi(sedan, 'srj 10 05', "Central 2").
    taxi:crear_taxi(sedan, 'srj 10 06', "Central 3").

##7. EN ERLANG | Terminal cliente
    cliente:pedir_taxi("Santiago", {2,3}).

###NOTA: El hecho de si se cancela o no el viaje depende del azar, ya que el cliente puede llamar a cancelar antes de que el taxi haya notificado de su llegada. Esto se define por una variable aleatoria.