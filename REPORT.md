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

Message Flow Diagram:

### Task 1 -- **Minimal Task**

Create a Worker Pool to substitute the Printer actor from previous week. The
pool will contain 3 copies of the Printer actor which will be supervised by a Pool Supervisor.
Use the one-for-one restart policy.

```erlang
hello_world() ->
    io:format("Hello PTR\n").
```

### Task 2 -- **Minimal Task**

Create an actor that would mediate the tasks being sent to the Worker Pool.
Any tweet that this actor receives will be sent to the Worker Pool in a Round Robin fashion.
Direct the Reader actor to sent it’s tweets to this actor.

```erlang
hello_world() ->
    io:format("Hello PTR\n").
```

## Week 3

Supervision Tree Diagram:

Message Flow Diagram:

### Task 1 -- **Minimal Task**

Continue your Worker actor. Any bad words that a tweet might contain
mustn’t be printed. Instead, a set of stars should appear, the number of which corresponds to
the bad word’s length. Consult the Internet for a list of bad words.

```erlang
hello_world() ->
    io:format("Hello PTR\n").
```

## Week 4

Supervision Tree Diagram:

Message Flow Diagram:

### Task 1 -- **Minimal Task**

Continue your Worker actor. Besides printing out the redacted tweet text,
the Worker actor must also calculate two values: the Sentiment Score and the Engagement
Ratio of the tweet. To compute the Sentiment Score per tweet you should calculate the mean
of emotional scores of each word in the tweet text. A map that links words with their scores is
provided as an endpoint in the Docker container. If a word cannot be found in the map, it’s
emotional score is equal to 0. The Engagement Ratio should be calculated as follows:

$$
engagement\_ratio = \frac{\#favourites + \#retweets}{\#followers}
$$

```erlang
hello_world() ->
    io:format("Hello PTR\n").
```

## Week 5

Supervision Tree Diagram:

Message Flow Diagram:

### Task 1 -- **Minimal Task**

Create an actor that would collect the redacted tweets from Workers and
would print them in batches. Instead of printing the tweets, the Worker should now send them
to the Batcher, which then prints them. The batch size should be parametrizable.

```erlang
hello_world() ->
    io:format("Hello PTR\n").
```

## Week 6

Supervision Tree Diagram:

Message Flow Diagram:

### Task 1 -- **Minimal Task**

Create a database that would store the tweets processed by your system.

```erlang
hello_world() ->
    io:format("Hello PTR\n").
```

### Task 2 -- **Minimal Task**

Continue your Batcher actor. Instead of printing the batches of tweets, the
actor should now send them to the database, which will persist them.

```erlang
hello_world() ->
    io:format("Hello PTR\n").
```

## Bibliography

[https://www.erlang.org/doc/design_principles/sup_princ.html](https://www.erlang.org/doc/design_principles/sup_princ.html)

[https://github.com/inaka/shotgun](https://github.com/inaka/shotgun)

[https://hexdocs.pm/shotgun/shotgun.html](https://hexdocs.pm/shotgun/shotgun.html)