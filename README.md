# Queue Server
**Derek Brown**  
**February 2016**

## Overview
This project implements a remote queue, accessible via TCP connection, with concurrent clients supported.

## Use
The project is implemented as a rebar3 release, and assumes `rebar3` is in your `PATH`.



Example usage:



```
$ telnet localhost 2223
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
foo
[2016-02-21 21:23:46] Illegal command. Usage: in {value} | out | quit
out
[2016-02-21 21:23:53] Queue is empty
in aa
[2016-02-21 21:23:57] Added to end of queue: aa
in bb
[2016-02-21 21:23:59] Added to end of queue: bb
in cc
[2016-02-21 21:24:00] Added to end of queue: cc
out
[2016-02-21 21:24:01] Removed from front of queue: aa
in dd
[2016-02-21 21:24:04] Added to end of queue: dd
out
[2016-02-21 21:24:06] Removed from front of queue: bb
out
[2016-02-21 21:24:09] Removed from front of queue: cc
out
[2016-02-21 21:24:11] Removed from front of queue: dd
out
[2016-02-21 21:24:12] Queue is empty
quit
Connection closed by foreign host.
```

## Building and Running

### Shell

```
$ make shell
[...]
1>
```

The server may now be accessed as seen in the example usage above.

### Release

To build the server as a standalone embedded Erlang node and run:

```
$ make release
[...]
$ _build/prod/rel/cue/bin/cue start
```

The server may now be accessed as seen in the example usage above, and stopped as follows:

```
$ _build/prod/rel/cue/bin/cue stop
```

## Tests

EUnit tests may be run as follows:

```
$ make test
```

## Implementation and Flow

This is an OTP application consisting of a supervisor (*cue_sup*) with two workers (*cue_listener_server* and *cue_handler_server*)

### Startup
*cue_sup*'s definition results in the following steps at application start:

* *cue_handler_server* Start, and ready to receive accepted socket connections from *cue_listener_server*
* *cue_listener_server* Start, and ready to receive new socket connections

Both servers spawn_link worker processes, so any failures will result in server failure and supervisor action.

## Example Concurrent Access

Terminal 1:

```
$ make shell
[...]
1>
```

Terminal 2:

```
$ telnet localhost 2223
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
in a1
[2016-02-21 22:37:39] Added to end of queue: a1
in a2
[2016-02-21 22:37:42] Added to end of queue: a2
in a3
[2016-02-21 22:37:52] Added to end of queue: a3
out
[2016-02-21 22:38:04] Removed from front of queue: a1
out
[2016-02-21 22:38:12] Removed from front of queue: a2
out
[2016-02-21 22:38:14] Removed from front of queue: a3
out
[2016-02-21 22:38:15] Queue is empty
quit
Connection closed by foreign host.
```

Terminal 3:

```
$ telnet localhost 2223
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
in b1
[2016-02-21 22:37:48] Added to end of queue: b1
in b2
[2016-02-21 22:37:56] Added to end of queue: b2
out
[2016-02-21 22:38:07] Removed from front of queue: b1
out
[2016-02-21 22:38:09] Removed from front of queue: b2
out
[2016-02-21 22:38:10] Queue is empty
quit
Connection closed by foreign host.
```


## Future Improvements

* Persisting queues across connections. Rather than keeping the queue local to a given connection, it could be kept in ETS, with a client token used to access from later connections.

* Additional type annotations

* Hiding the data structure used by *cue_queue* with *-opaque*, to rely less on its implementation as a list (or any other form).

* Possibly making the *spawn_link*'ed processes proper special processes, for more thorough integration into the runtime ([1])

## References

[1] http://erlang.org/doc/design_principles/spec_proc.html






