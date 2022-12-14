# mamml
A caching database with first class JSON support, static typing, and a network interface.

## Usage

### Installation
```shell
git clone https://github.com/rawleyfowler/mamml.git
cd mamml
eval $(opam env) # optional
make install
mamml # starts the mamml server on port 5555
```

### Testing Connection
```
telnet localhost 5555 # Substitute localhost for your server's IP
PUT `Hello World` AS TEXT NAMED test; -> test
GET test; -> Hello World
```

### Development
```shell
git clone https://github.com/rawleyfowler/mamml.git
cd mamml
eval $(opam env) # optional
dune exec mamml.exe
```

Note: We use backticks \` for strings.

##### Example valid mamml instructions:
```
PUT 123 AS Int; -> Returns the UUID for this node.
PUT `{ "hello": "world" }` AS Json; -> Returns the unique ID for this node.
PUT `{ "hello": "world" }` AS Json NAMED myJson; -> Returns the label 'myJson'.
PUT `{ "json": { "some_num": 123 } }` AS Json NAMED myDeeperJson;
GET myJson; -> `{ "hello": "world" }`
GET myJson.hello; -> 'world'
GET myJson.hello AS Json; -> '{ "hello": "world" }'
GET myDeeperJson.json.some_num; -> 123
```
##### We can also just store primitives:
```
PUT 12345678 AS Int; -> Returns ID pointing to this node containing the number.
PUT 12345678 AS Int NAMED 'myAge'; -> Returns the label 'myAge'.
UPDATE myAge TO 12393844893; -> Update a given node by name. 
CLEAR myAge; -> Sets a node to Null.
DELETE myAge; -> Deletes a given node by name. (this does not null the value, but rather remove it entirely from the cache)
```
##### Null can be achieved by:
```
PUT Null AS Int; -> Id of new Null, number node.
```

## Why
Many systems rely on caching for fast lookup times, however a lot of the systems currently
in use don't support JSON or, are not open source. So I set out on making mamml.

## License
Mamml is provided under the ISC license, please read the `LICENSE` file at the root of the
project for more information.
