## Performance comparison of Erlang socket option `{packet, 0}` and `{packet, 4}`

Here, we compare peformance of **receiving** from a TCP socket using 
1. `{packet, 0}` and parsing the packets in the application code vs.
2. `{packet, 4}` having the Erlang runtime parse the packet for us.

In both cases, we want to end up with a single binary per packet.

### Usage
Start server with only a single scheduler:
```
erlc packet.erl && erl +S 1 +SDcpu 1 +SDio 1
1> packet:server0(). %% compare against packet:server4()
```

In a second Erlang shell, start the client.
Give it more schedulers since we do not want the client to be the bottleneck.
```
erl +S 4 +SDcpu 4 +SDio 10
1> packet:client().
```

### Results
Executed on an Intel NUC 11 with 8 cores and 32GB of RAM running Ubuntu 22.04 and Erlang 25.0.1.

The columns show number of messages received across 2 different runs of 30 seconds each.
(For example `26,800,000 - 27,800,000` means in one of the 30 second runs, the server received 26,800,000 messages in the other 30 second run it received 27,800,000 messages.)

| MSG_BYTES           | [{packet, 0}, {active, once}] | [{packet, 4}, {active, once}] | [{packet, 4}, {active, 40}] | [{packet, 4}, {active, true}]|
| ------------------- | ----------------------------- | ----------------------------- | ----------------------------| -----------------------------|
| 10                  | 26,800,000 - 27,800,000 | 27,000,000 - 28,700,000 | 34,330,000 - 36,730,000 | 29,330,000 - 31,270,000 |
| 1,000               | 26,390,000 - 27,100,000 | 25,100,000 - 26,300,000 | | |
| 1,400               | 24,280,000 - 25,520,000 | 23,170,000 - 25,030,000 | 8,950,000 - 9,100,000 -  | 15,670,000 - 16,690,000 |
| 1,500               | 23,290,000 - 24,890,000 | 4,290,000 - 4,300,000 | 6,230,000 - 6,380,000 | 9,470,000 - 9,260,000 |
| 2,000               | 19,190,000 - 19,360,000 | 4,110,000 - 4,200,000 | | |
| 3,000               | 14,130,000 - 14,370,000 | 3,910,000 - 3,940,000 | | |
| 5,000               | 8,770,000 - 9,070,000   | 3,990,000 - 4,110,000 | | |
| 10,000              | 4,480,000 - 4,690,000   | 3,630,000 - 3,660,000 | | |
| 100,000             | 488,000 - 493,000       | 892,000 - 892,000 | | |
| 1,000,000,000 (1GB) | 16 - 16                 | 70 - 72 | 73 - 74 | 77 - 77 |
