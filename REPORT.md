# FAF.PTR16.1 -- Project 1

> **Performed by:** Viorel Noroc, group FAF-203

> **Verified by:** asist. univ. Alexandru Osadcenco

## General Requirements

Compared to the previous Project, all weeks for this one aim to build upon the same application.

The goal is to finish the Project with a more or less functional stream processing system.

Since you will be working on a complex application, each presentation will now require
you to present 2 diagrams: a Message Flow Diagram and a Supervision Tree Diagram. The
Message Flow Diagram describes the message exchange between actors of your system whereas
the Supervision Tree Diagram analyzes the monitor structures of your application.

Every task you work on should be easily verifiable. Your system should provide logs about
starting / stopping actors, auto-scaling / load balancing workers and printing processed tweets
on screen.

## Week 1

Supervision Tree Diagram:

![Supervision Tree Diagram](diagrams/week1_sup.png)

Message Flow Diagram:

![Message Flow Diagram](diagrams/week1_msg.png)

### Task 1 -- **Minimal Task**

Initialize a VCS repository for your project.

[https://github.com/darkcat013/ptr-lab2](https://github.com/darkcat013/ptr-lab2)

### Task 2 -- **Minimal Task**

Write an actor that would read SSE streams. The SSE streams for this lab are available on [Docker Hub](https://hub.docker.com/) at `alexburlacu/rtp-server`, courtesy of our beloved FAFer Alex Burlacu.

```erlang
-module(reader).

-export([start/1]).

start(Endpoint) ->
    {Pid, _} = spawn_monitor(fun() -> read_sse(Endpoint) end),
    {ok, Pid}.

read_sse(Endpoint) ->
    {ok, Conn} = shotgun:open("localhost", 8080),
    Options =
        #{async => true,
          async_mode => sse,
          handle_event =>
              fun(_, _State, BinMsg) -> printer_sup:send_msg(BinMsg) end},
    {ok, _Ref} = shotgun:get(Conn, Endpoint, #{}, Options).
```

This is a simple actor that opens a connection to localhost:8080 using the Shotgun library and reads the Server Side Events. When an event is received, it is sent to the printer supervisor.

### Task 3 -- **Minimal Task**

Create an actor that would print on the screen the tweets it receives from
the SSE Reader. You can only print the text of the tweet to save on screen space.

```erlang
-module(printer).

-export([start/0]).

start() ->
    {Pid, _} = spawn_monitor(fun() -> receive_loop() end),
    {ok, Pid}.

receive_loop() ->
    receive
        Message when <<"event: \"message\"">> /= Message ->
            [_ | [Json]] = string:split(Message, ": "),
            TweetMap = jsx:decode(Json),
            #{<<"message">> := #{<<"tweet">> := #{<<"text">> := TweetText}}} = TweetMap,
            io:format("~s~n", [binary_to_list(TweetText)]),
            timer:sleep(rand:uniform(45)+5)
    end,
    receive_loop().
```

This is a simple actor that decodes a json using the JSX library and prints the tweet text.

### Task 4 -- **Main Task**

Create a second Reader actor that will consume the second stream provided by
the Docker image. Send the tweets to the same Printer actor.

```erlang
-module(reader_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    MaxRestarts = 1000,
    MaxTime = 10,
    SupFlags =
        #{strategy => one_for_one,
          intensity => MaxRestarts,
          period => MaxTime},

    Endpoint1 = "/tweets/1",
    Endpoint2 = "/tweets/2",

    Reader1 =
        #{id => reader1,
          start => {reader, start, [Endpoint1]},
          restart => permanent,
          shutdown => 2000,
          type => worker,
          modules => [reader]},

    Reader2 =
        #{id => reader2,
          start => {reader, start, [Endpoint2]},
          restart => permanent,
          shutdown => 2000,
          type => worker,
          modules => [reader]},

    ChildSpecs = [Reader1, Reader2],
    {ok, {SupFlags, ChildSpecs}}.
```

This reader supervisor starts 2 reader actors with 2 different endpoints, they both send the message to the printer supervisor which has only 1 child.

### Task 5 -- **Main Task**

Continue your Printer actor. Simulate some load on the actor by sleeping every
time a tweet is received.
Suggested time of sleep – 5ms to 50ms.
Consider using Poisson
distribution. Sleep values / distribution parameters need to be parameterizable.

```erlang
timer:sleep(rand:uniform(45)+5)
```

I simulated the load by using a random time for sleep from 5 to 50 ms.

## Week 2

Supervision Tree Diagram:

![Supervision Tree Diagram](diagrams/week2_sup.png)

Message Flow Diagram:

![Message Flow Diagram](diagrams/week2_msg.png)

### Task 1 -- **Minimal Task**

Create a Worker Pool to substitute the Printer actor from previous week. The
pool will contain 3 copies of the Printer actor which will be supervised by a Pool Supervisor.
Use the one-for-one restart policy.

```erlang
-module(worker_pool_sup).

-behaviour(supervisor).

-export([start_link/1, send_msg/2]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(WorkersNr) ->
    {ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    start_workers(WorkersNr),
    {ok, Pid}.

init([]) ->
    MaxRestarts = 1000,
    MaxTime = 10,
    SupFlags =
        #{strategy => simple_one_for_one,
          intensity => MaxRestarts,
          period => MaxTime},

    ChildSpecs =
        [#{id => worker_printer,
           start => {worker_printer, start, []},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [worker_printer]}],
    {ok, {SupFlags, ChildSpecs}}.

send_msg(BinMsg, WorkerNr) ->
    {_, ChildPid, _, _} = lists:nth(WorkerNr, supervisor:which_children(?MODULE)),
    ChildPid ! BinMsg.

new_child() ->
    supervisor:start_child(?MODULE, []).

start_workers(0) -> ok;
start_workers(WorkersNr) ->
    new_child(),
    start_workers(WorkersNr-1).

```

The printer_sup from previous week was renamed in worker_pool_sup and it starts 3 printer workers.

### Task 2 -- **Minimal Task**

Create an actor that would mediate the tasks being sent to the Worker Pool.
Any tweet that this actor receives will be sent to the Worker Pool in a Round Robin fashion.
Direct the Reader actor to sent it’s tweets to this actor.

```erlang
-module(load_balancer).

-export([start/1]).

start(WorkersNr) ->
    Pid = spawn_link(fun() -> balancer_loop(WorkersNr, 1) end),
    register(balancer, Pid),
    {ok, Pid}.

%% round robin
balancer_loop(WorkersNr, CurrentNr) ->
    receive
        BinMsg ->
            worker_pool_sup:send_msg(BinMsg, CurrentNr)
    end,
    balancer_loop(WorkersNr, CurrentNr rem 3 + 1).

```

This is a simple round robin balancer which is registered and both readers sent their messages to it.

### Task 3 -- **Main Task**

Continue your Worker actor. Occasionally, the SSE events will contain a “kill
message”. Change the actor to crash when such a message is received. Of course, this should
trigger the supervisor to restart the crashed actor.

```erlang
-module(worker_printer).

-export([start/0]).

start() ->
    Pid = spawn_link(fun() -> receive_loop() end),
    {ok, Pid}.

receive_loop() ->
    receive
        Message when <<"event: \"message\"">> /= Message ->
            case string:find(Message, "panic") of
                nomatch ->
                    [_, Json] = string:split(Message, ": "),
                    TweetMap = jsx:decode(Json),
                    #{<<"message">> := #{<<"tweet">> := #{<<"text">> := TweetText}}} = TweetMap,
                    io:format("Worker: ~p, Text: ~s~n", [self(), binary_to_list(TweetText)]),
                    timer:sleep(rand:uniform(45) + 5),
                    receive_loop();
                _ ->
                    io:format("Worker: ~p, Kill message received (panic) ~n", [self()])
            end
    end.
```

The kill message is panic and the worker stops when receives it. The supervisor restarts the worker after this.



## Bibliography

[https://www.erlang.org/doc/design_principles/sup_princ.html](https://www.erlang.org/doc/design_principles/sup_princ.html)

[https://github.com/inaka/shotgun](https://github.com/inaka/shotgun)

[https://hexdocs.pm/shotgun/shotgun.html](https://hexdocs.pm/shotgun/shotgun.html)

[http://www.plantuml.com/](http://www.plantuml.com/)