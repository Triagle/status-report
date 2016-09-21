What is it?
===========

Status Report is a simple program that connects to a server, who
performs a few checks to ensure that everything is okay and returns a
report of how the system is doing currently.

Why is it?
==========

Status report was made to create a simple way of getting a report of the
current status of my raspberry pi, without having to ssh in and check
myself.

Where is status report useful
=============================

Status report is useful for the most basic of status checks, it should
not be used if you rely on the data (server farm for instance), or if
you plan on having large numbers of clients connecting to this server
for status checks as the io is blocking.

Status report is very useful for Raspberry Pi machines running in a
headless mode, it can check if services are running without the overhead
of more professional tools that a hobbyist has no need for.

Usage
=====

To use this program, first have the program setup on your server, and
the client installed on whatever other computer you want to get the
status report from. Next on the server create a json file that holds the
various service names and associated commands used to check they're
still working.

For instance here is an example of such a json file, that runs a check
to ensure syncthing is still operational.

``` {.javascript}
{
    "Syncthing" : "if [ \"$(ps aux | grep syncthing | tail -n +2)\" ]; then echo \"UP\"; else echo \"DOWN\"; fi"
}
```

The client is looking for an UP if the status is operational, and a DOWN
is the status failed.

Finally start the server, pointing it at the configuration file for
status results

``` {.shell}
status-report-server -c /path/to/config.json
```

From the client side, all you need to do is run the client, pointing it
at the ip of the server.

``` {.shell}
status-report $IP
```

It will run, and give a nice output of the processes, and their
statuses. As an example, if you were to run the report pointed at a
server config like above you'll get an output like so

``` {.example}
     _        _                                         _   
 ___| |_ __ _| |_ _   _ ___   _ __ ___ _ __   ___  _ __| |_ 
/ __| __/ _` | __| | | / __| | '__/ _ \ '_ \ / _ \| '__| __|
\__ \ || (_| | |_| |_| \__ \ | | |  __/ |_) | (_) | |  | |_ 
|___/\__\__,_|\__|\__,_|___/ |_|  \___| .__/ \___/|_|   \__|
                                      |_|                   

- Syncthing :: Ok

Looks like all checks have passed!
```
