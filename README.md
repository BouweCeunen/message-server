# Message server in Haskell (UNMAINTAINED)

This messageserver can be used to connect two tcp connections to form a passage. When a third wants to connect, it gets rejected to ensure safe communication between two hosts. It also sends UDP messages when something is wrong, like the protocol that is not being followed.

Feel free to let me know if something isn't working as it should at bouwe.ceunen@gmail.com

### Get it running

```sh
$ ./messageserver
```
This will give an error args, arguments are needed.
Append the arguments so that the first argument is the UDP send port, the second argument the UDP send IP and the third port the actual port used for TCP communication.

```sh
$ ./messageserver 1012 127.0.0.1 5678
```
### Protocol 

In order to get into the messageserver when connecting to it, a protocol is needed. The init and pass can be customized. An example is 'connect' 'name' 'password'.
-   init
-   'name'
-   pass

### Features

- send 'info' to get information about the other connected side.
- send 'quit' to quit
- UDP messages are send when the protocol is not followed by the host, this prevents unwanted intrusions

### Example usage

```sh
$ ./messageserver 1012 127.0.0.1 5678
```

```sh
$ telnet 127.0.0.1 5678
>> connect
>> user1
>> passw
```

```sh
$ telnet 127.0.0.1 5678
>> connect
>> user2
>> passw
```
