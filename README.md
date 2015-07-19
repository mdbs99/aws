#AWS Lib

It's a minimalist implementation for Amazon Web Services.

Fully **object-oriented**, **interface-based** and all objects are **immutable**.

## Amazon S3 Documentation
* [REST API](http://docs.aws.amazon.com/AmazonS3/latest/API/APIRest.html)
* [Error responses](http://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html)

## A "Bigger" Example

First you need create a Region object -- this is your connection to the Amazon services.

Second, using just one line, the app creates a new Bucket and put a new file on it.

Because all return types are interfaces, the final return to an variable is necessary to release the memory automatically.

``` pascal
program s3;
{$mode objfpc}{$H+}
uses
  aws_client,
  aws_s3;
var
  Rg: IS3Region;
  Ob: IS3Object;
begin
  Rg := TS3Region.Create(
    TAWSClient.Create(
      TAWSCredentials.Create('YOUR_access_key', 'YOUR_secret_key', True)
    )
  );
  Ob := Rg.Buckets.Put('mys3examplebucket', '/').Objects.Put('foo.txt', 'plain', 'foo.txt', '');
end.
```

That's all.

### Explanation step-by-step
- you create a Region object, that need a Client object;
- so R.Buckets.Put(...) will creates a new Bucket on the server and the return is an instance of Bucket;
- the Bucket created have the method Objects, so Objects.Put(...) will put a new file on the server.

That is the same:
``` pascal
var
  Rg: IS3Region;
  Bk: IS3Bucket;
  Ob: IS3Object;
begin
  Rg := TS3Region.Create(...);
  Bk := Rg.Buckets.Put('mys3examplebucket', '/');
  Ob := Bk.Objects.Put('foo.txt', 'plain', 'foo.txt', '');
end;
```

You can get/put/delete Buckets and Objects using this API.

No need to release memory!

## Dependencies 

There is only one dependency: [Synapse](http://synapse.ararat.cz/doku.php/download)

Synapse is used as HTTP client.  You can customize or create a new client using another lib like lNet, fpHttp, whatever.

## Got questions?

If you have questions or general suggestions, don't hesitate to submit
a new [Github issue](https://github.com/mdbs99/AWS/issues/new).

