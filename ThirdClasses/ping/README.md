# Ex 2.1 Ping

In this exercise we will try to handle spawning new processes and handling the communication between them.
We want to have the _coordinator_ process which will spawn _worker_, a separate process for the task.
We want to ping all IP addresses in the given set.

## Worker

This process should be started by a _coordinator_ process, with following arguments passed:
 
 - Coordinator's PID - to send back the result
 - IP address that needs to be pinged
 - Number of ICMP requests (this one is optional, it can be hard coded to eg 4)

This process should do the following:

1) If needed transform IP address to the string.
2) Assemble the string command, use [ping](https://linux.die.net/man/8/ping).
3) Run the command using [os:cmd/1](http://erlang.org/doc/man/os.html#cmd-1).
4) Parse the result and extract the result.
5) Send the result back to the _coordinator_.

## Coordinator

This process is started by a user. It accepts:

 - IP address parts ranges, four of them as we use IPv4
 - Number of ICMP requests (this one is optional, it can be hard coded to eg 4)

This process should do the following:

1) Generate IP addresses.
2) For each IP address spawn a _worker_ process.
3) Enter a loop receiving results from the _workers_.
4) Print the result when no more reports appear.

## Tips

### Worker

If you struggle with assembling the command try running it in the console.
The command should look something like `ping -c 4 192.168.0.10`.


### Coordinator

Use `receive` with `after` so you will not wait forever.