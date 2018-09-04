# `XML-RPC`

My notes about the `XML RPC` Specification, which determined this implementation (i.e. `xmlrpc`).

## Links

<http://xmlrpc.scripting.com/spec.html>

## Specification

## Summary

An `XML-RPC` message is a `POST`-method `HTTP` request. The body of the request is in `XML. 
`
The `ResponseBody` is also in `XML` (formatted from the value returned by the procedure that executes on the server).

Procedure parameters can be: numbers, strings, dates, booleans, (but not null); or, a record or a list thereof.

### Example `Request`

e.g. an `XML-RPC` Request:

    POST /RPC2 HTTP/1.0
    User-Agent: Frontier/5.1.2 (WinNT)
    Host: betty.userland.com
    Content-Type: text/xml
    Content-length: 181
    
    
    <?xml version="1.0"?>
    <methodCall>
       <methodName>examples.getStateName</methodName>
       <params>
          <param>
             <value><i4>41</i4></value>
             </param>
          </params>
       </methodCall>

`NOTE`: As any `HTTP_Request`, there's a double `"\n\r"` between headers and body.

### The `Request`

#### The `RequestHeaders`

##### `URI:` 

The format of the URI in the first line of the header may-or-may-not specified. For example, it could be empty, a single slash, if the server is only handling `XML-RPC` calls.

However, if the server is handling a mix of incoming `HTTP` requests, we allow the URI to help route the request to the code that handles `XML-RPC` requests. (In the example, the `URI` is `/RPC2`, telling the server to route the request to the `"RPC2"` method-handler.)

##### `User-Agent:` and `Host:`

Both the host and a user-agent are required.

##### `Content-Type:`

the content-type is `text/xml`.

##### `Content-Length:`

the content-length must be specified and must be correct.

#### The `RequestBody`'s Payload

The payload is in `XML`, a single `<methodCall>` structure.

The `<methodCall>` must contain a `<methodName>` sub-item, a string, containing the name of the method to be called. The string may only contain identifier characters, upper and lower-case A-Z, the numeric characters, 0-9, underscore, dot, colon and slash. 
It has no intrinsic interpretation (It's entirely up to the server to decide how to interpret the characters in a methodName. For example, the methodName could be the name of a file containing a script that executes on an incoming request. It could be the name of a cell in a database table. Or it could be a path to a file contained within a hierarchy of folders and files.)

### `XML-RPC` Values

An `XML-RPC` value is like a `JSON` value. It has:

- several primitive (a.k.a. "scalar") values, including a `datetime`, but excluding `null`.
- two aggregate (a.k.a. "vector") values, records and tuples.

#### "Scalar" `<value>`s

if a type tag is absent, the type defaults to string (i.e. `<string>`s may be implicit).

| `XML` Tag            | `XML-RPC` Type                                 | Example                        |
| ---------            | :--------------                                | :-------                       |
| `<i4>` or `<int>`    | four-byte signed integer.                      | `-12`                          |
| `<boolean>`          | `0` (`false`) or `1` (`true`).                 | `1`                            |
| `<string>`           | string.                                        | `hello world`                  |
| `<double>`           | double-precision signed floating-point number. | `-12.214`                      |
| `<dateTime.iso8601>` | date & time.                                   | `19980717T14:08:55`            |
| `<base64>`           | base64-encoded binary.                         | `eW91IGNhbid0IHJlYWQgdGhpcyE=` |

#### `<struct>`s

A `<struct>` is a "record" (a.k.a. `dict`, a.k.a. `attrset`).

A `<struct>` contains `<member>`s, and each `<member>` contains a `<name>` and a `<value>`.

`<struct>`s are recursive, i.e. any `<member>` may be any `<value>` or `<struct>` or `<array>`.

e.g. a two-element `<struct>`:

      <struct>
    
       <member>
          <name>lowerBound</name>
          <value><i4>18</i4></value>
        </member>
        
       <member>
          <name>upperBound</name>
          <value><i4>139</i4></value>
        </member>
    
       </struct>

i.e. (in a pseudo-syntax for records):

    { lowerBound = 18, upperBound = 139 }
    
The syntax for a `<member>` being:
    
       <member>
          <name>...</name>
          ...
        </member>

#### `<array>`s

An `<array>` is a "tuple" (a.k.a. a heterogeneous list).

An `<array>` contains a single `<data>` element, and the `<data>` contains any number of `<value>`s.

`<array>`s are recursive, i.e. any datum in  `<data>` may be any `<value>` or `<struct>` or `<array>`.

e.g. a four-element `<array>`:

    <array>
      <data>

        <value><i4>12</i4></value>

        <value><string>Egypt</string></value>

        <value><boolean>0</boolean></value>

        <value><i4>-31</i4></value>

      </data>
    </array>

i.e. (in a pseudo-syntax for heterogeneous lists):

    [ 12      :: Int
    , "Egypt" :: String
    , False   :: Bool
    , -31     :: Int
    ]

### Example `Response` (success)

e.g. a successful `XML-RPC` Response:

    HTTP/1.1 200 OK
    Connection: close
    Content-Length: 158
    Content-Type: text/xml
    Date: Fri, 17 Jul 1998 19:55:08 GMT
    Server: UserLand Frontier/5.1.2-WinNT
    
    
    <?xml version="1.0"?>
    <methodResponse>
       <params>
          <param>
             <value><string>South Dakota</string></value>
             </param>
          </params>
       </methodResponse>

### Example `Response` (failure)

e.g. a failed `XML-RPC` Response:

    HTTP/1.1 200 OK
    Connection: close
    Content-Length: 426
    Content-Type: text/xml
    Date: Fri, 17 Jul 1998 19:55:02 GMT
    Server: UserLand Frontier/5.1.2-WinNT
    
    
    <?xml version="1.0"?>
    <methodResponse>
       <fault>
          <value>
             <struct>
                <member>
                   <name>faultCode</name>
                   <value><int>4</int></value>
                   </member>
                <member>
                   <name>faultString</name>
                   <value><string>Too many parameters.</string></value>
                   </member>
                </struct>
             </value>
          </fault>
       </methodResponse>

`NOTE`: As any `HTTP_Request`, there's a double `"\n\r"` between headers and body.

### The `Response`

Return `200 OK` unless there's a lower-level error.

The `Content-Type:` is `text/xml`.

The `Content-Length:` must be present and correct.

The `RequestBody` is a `<methodResponse>`.

#### `<methodResponse>`

- `<params>`:

The body of the response is a single XML structure, a `<methodResponse>`. it can contain a single `<params>`, which contains a single `<param>`, which contains a single `<value>`.

- `<fault>`:

The `<methodResponse>` could also contain a `<fault>` which contains a `<value>` which is a `<struct>` containing two elements, one named `<faultCode>`, an `<int>` and one named `<faultString>`, a `<string>`.

`NOTE`: `<fault>` and `<params>` are mutually exclusive; `<methodResponse>` can not contain both a `<fault>` and a `<params>`; but `<methodResponse>` must contain *either* a `<fault>` or a `<params>`.

### `null`?

No, the `null` value is undefined by the standard (unsupported).

Different implementations may reject it, or may implement it as:

- `<nil />` 
- `<ex:nil />`
- `<array />`
- `<array><data>...<data/><array/>`
- etc.

### `Unicode`?

Yes, `Unicode` is supported. Because the `XML` format itself supports `Unicode`.

### FAQ

#### `<int>` Syntax/Range?

`Q:` the legal syntax (and range) for integers? How to deal with leading zeros? Is a leading plus sign allowed? How to deal with whitespace?

`A:` An integer is a 32-bit signed number. You can include a plus or minus at the beginning of a string of numeric characters. Leading zeros are collapsed. Whitespace is not permitted. Just numeric characters preceeded by a plus or minus.

#### `<double>` Syntax/Range?

`Q:` What is the legal syntax (and range) for floating point values (doubles)? How is the exponent represented? How to deal with whitespace? Can infinity and "not a number" be represented?

`A:` There is no representation for infinity or negative infinity or "not a number". At this time, only decimal point notation is allowed, a plus or a minus, followed by any number of numeric characters, followed by a period and any number of numeric characters. Whitespace is not allowed. The range of allowable values is implementation-dependent, is not specified.

#### Valid Characters of `<string>`?

`Q:` What characters are allowed in strings? Non-printable characters? Null characters? Can a "string" be used to hold an arbitrary chunk of binary data?

`A:` Any characters are allowed in a string except `<` and `&`, which are encoded as `&lt;` and `&amp;`. A string can be used to encode binary data.

#### Is `<struct>` Ordered?

`Q:` Does the "struct" element keep the order of keys. Or in other words, is the struct "foo=1, bar=2" equivalent to "bar=2, foo=1" or not?

`A:` The struct element does not preserve the order of the keys. The two structs are equivalent.

#### Is `<fault>` Open?

`Q:` Can the `<fault>` struct contain other members than `<faultCode>` and `<faultString>`? Is there a global list of faultCodes? (so they can be mapped to distinct exceptions for languages like Python and Java)?

`A:` No, a `<fault>` struct may not contain members other than those specified. This is true for all other structures. We believe the specification is flexible enough so that all reasonable data-transfer needs can be accomodated within the specified structures. If you believe strongly that this is not true, please post a message on the discussion group.
There is no global list of fault codes. It is up to the server implementer, or higher-level standards to specify fault codes.

#### Timezone?

`Q:` What timezone should be assumed for the dateTime.iso8601 type? UTC? localtime?

`A:` Don't assume a timezone. It should be specified by the server in its documentation what assumptions it makes about timezones.

#### `

`Q:` how to represent a null parameter value?

`A:` `XML-RPC` doesn't strictly have a NULL or None value: xmlrpc.com Some times it is represented by using an empty list, but this is implementation specific. Particular libraries also may have extensions that allow sending and receiving NULL values, but again, it is implementation specific and might not work between arbitrary pairs of client/server libraries.
– Adam Vandenberg

`A:` Needs a vendor extension. Python’s extension uses `<nil />`. .NET's xml rpc extension also uses `<nil />`. However, Apache’s xml-rpc doesn't, it uses `ex: nil />`.

### Conclusion

