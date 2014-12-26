#AWS Lib

AWS Lib is a minimalistic Pascal implementation of Amazon REST API for [Free Pascal](http://freepascal.org/).

This project is fully **object-oriented**, **interface-based** and all objects are **immutable objects**.

###Amazon S3
* [REST API](http://docs.aws.amazon.com/AmazonS3/latest/API/APIRest.html)
* [Error responses](http://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html)

``` pascal
program awstest;
{$mode objfpc}{$H+}
uses
  sysutils,
  aws_auth,
  aws_client,  
  aws_s3;
var
  Cred: IAWSCredentials;
  Client: IAWSClient;
  Region: IS3Region;
begin
  Cred := TAWSCredentials.Create('access_key', 'secret_key', True);
  Client := TAWSClient.Create(Cred);
  Region := TS3Region.Create(Client);
  if Region.IsOnline then
  begin
    // create a new bucket
    Region.Buckets.Put('colorpictures', '/');
    // delete the new bucket
    Region.Buckets.Delete('colorpictures', '/');
  end;
end.  
```
