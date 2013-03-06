-module(looper).

-compile(export_all).

loop() ->
  receive 
    abc -> 
      io:format("Receive abc. ~n "),
      loop();
    stop-> 
      io:format("stop"),
      stop
  end.    

loop2() -> 
  receive 
    abc -> 
      io:format("Receive abc. ~n "),
      loop2();
    stop-> 
      io:format("stop"),
      stop
  after 15000 -> 
      receive 
        Any ->
          io:format("Receive ~p ~n ",[ Any])
      end,                 
      io:format("clear . ~n "),
      loop2()
  end. 
