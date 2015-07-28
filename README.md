**AWS Lib** is minimalist, **object-oriented**, **interface-based** and **immutable** for Amazon Web Services (S3).

The code have some principles:
 1. all classes are sealed
 2. all methods return an interface or primitive type
 3. all public methods are implementations to methods of an interface
 4. memory is released automatically 

## A "Bigger" Example

Bellow you see a complete example to create a new Bucket and send a file on it.

``` pascal
program s3;
{$mode objfpc}{$H+}
uses
  aws_client,
  aws_s3;
var
  Rg: IS3Region;
begin
  Rg := TS3Region.Create(
    TAWSClient.Create(
      TAWSCredentials.Create('YOUR_access_key', 'YOUR_secret_key', True)
    )
  );
  Rg.Buckets.Put('mys3examplebucket', '/').Objects.Put('foo.txt', 'plain', 'foo.txt', '');
end.
```

First a Region object was created -- this is your connection to the Amazon services.

Second, using just one line, the code creates a new Bucket and put a new file on it.

No need to release memory!

You can get/put/delete Buckets and Objects using this API.

## Dependencies 

There is only one dependency: [Synapse](http://synapse.ararat.cz/doku.php/download)

Synapse is used as HTTP client.  You can customize or create a new client using another lib like lNet, fpHttp, whatever.

## Got questions?

If you have questions or general suggestions, don't hesitate to submit
a new [Github issue](https://github.com/mdbs99/AWS/issues/new).

## Amazon S3 Documentation
* [REST API](http://docs.aws.amazon.com/AmazonS3/latest/API/APIRest.html)
* [Error responses](http://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html)
