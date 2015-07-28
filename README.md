**AWS Lib** is minimalist, **object-oriented**, **interface-based** and **immutable** for Amazon Web Services (S3).

The code have some principles:
 1. all classes are sealed
 2. all methods return an interface or primitive type
 3. all public methods are implementations to methods of an interface
 4. all instances are immutable
 5. memory is released automatically 

## A "Bigger" Example

Bellow you see a complete example to create a new Bucket and send a file on it.

``` pascal
program s3;
{$mode objfpc}{$H+}
uses
  aws_client,
  aws_s3;
var
  Rgn: IS3Region;
begin
  Rgn := TS3Region.Create(
    TAWSClient.Create(
      TAWSCredentials.Create('YOUR_access_key', 'YOUR_secret_key', True)
    )
  );
  Rgn.Buckets.Put('mys3examplebucket', '/').Objects.Put('foo.txt', 'plain', 'foo.txt', '');
end.
```

First a Region object was created -- this is your connection to the Amazon services.

Second, using just one line, the code creates a new Bucket and put a new file on it.

No need to release memory!

To get this file that was sent, use the code:

``` pascal
var
  Rgn: IS3Region;
  Obj: IS3Object;
begin
  Rgn := TS3Region.Create(
    TAWSClient.Create(
      TAWSCredentials.Create('YOUR_access_key', 'YOUR_secret_key', True)
    )
  );
  Obj := Rgn.Buckets.Get('mys3examplebucket', '/').Objects.Get('foo.txt', '/');
  Obj.Stream.SaveToFile('foo.txt');
end.
```

You can use this syntax too, without an Obj variable:

``` pascal
Rgn.Buckets.Get('mys3examplebucket', '/')
  .Objects.Get('foo.txt', '/')
  .Stream.SaveToFile('foo.txt');
```

To delete this file on server, use the code:

``` pascal
Rgn.Buckets.Get('mys3examplebucket', '/').Objects.Delete('foo.txt');
```

If you needs to know if the region is online, just use:
``` pascal
if Rgn.Online then
  writeln('Server is online!');
```

## Dependencies 

There is only one dependency: [Synapse](http://synapse.ararat.cz/doku.php/download)

Synapse is used as HTTP client.  You can customize or create a new client using another lib like lNet, fpHttp, whatever.

## Got questions?

If you have questions or general suggestions, don't hesitate to submit
a new [Github issue](https://github.com/mdbs99/AWS/issues/new).

## Amazon S3 Documentation
* [REST API](http://docs.aws.amazon.com/AmazonS3/latest/API/APIRest.html)
* [Error responses](http://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html)
