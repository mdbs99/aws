#AWS Lib

AWS Lib is a minimalistic Object Pascal implementation for the Amazon Web Services.

This project is fully **object-oriented**, **interface-based** and all objects are **immutable objects**.

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
  R: IS3Region;
  O: IS3Object;
begin
  R := TS3Region.Create(
    TAWSClient.Create(
      TAWSCredentials.Create('YOUR_access_key', 'YOUR_secret_key', True)
    )
  );
  O := R.Buckets.Put('mys3examplebucket', '/').Objects.Put('foo.txt', 'plain', 'foo.txt', '');
end.
```

That's all.

You can get/put/delete Buckets and Objects using this API.

No need to release memory!
