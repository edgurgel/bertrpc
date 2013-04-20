# BERT-RPC Erlang client

A pool of BERT-RPC clients

## Implementation

bertrpc uses poolboy to have pool of workers that connect/encode/decode BERT-RPC.

The configuration is done directly on each pool, this is part of the .app (as example):

```erlang
{env, [
    {pools, [
      {ext, [
        {size, 10},
        {max_overflow, 20}
        ], [
        {hostname, {127,0,0,1}},
        {port, 8000}
        ]},
      {messages_controller, [
        {size, 5},
        {max_overflow, 10}
        ], [
        {hostname, {127,0,0,1}},
        {port, 9999}
        ]}
      ]}
    ]}
```

This configuration means:

* Module ext is hosted at 127.0.0.1 at 8000 on a pool of size 10 and max_overflow 20
* Module messages_controller is hosted at 127.0.0.1 at 9999 on a pool of size 5 and max overflow 10

Usage:

Let's suppose that ext has a binary method named... binary.

```erlang
Response = bertrpc_app:call(ext, binary, [1,2]).
```

This will pick a worker, execute the call and return the expected response.

That's it.

## TODO

Write tests!

## Contributing

If you'd like to hack on bertrpc, start by forking my repo on Github.

You need [Erlang](http://www.erlang.org) and [rebar](https://github.com/basho/rebar). We are using Erlang R15B03, but you may try use it with other versions.

Dependencies can be fetched running:

```
$ rebar get-deps
```

To compile:

```
$ rebar compile
```

Pull requests are greatly appreciated.

