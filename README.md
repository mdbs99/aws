#AWS Lib

AWS Lib is a Pascal implementation of Amazon REST API for [Free Pascal](http://freepascal.org/).

Until now we have the implementation of Amazon S3.
* S3 REST API: http://docs.aws.amazon.com/AmazonS3/latest/API/APIRest.html
* S3 Error responses: http://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html

It is minimalistic, clean and simple.

``` pascal
program awstest;
{$mode objfpc}{$H+}
uses
  sysutils,
  aws_auth,
  aws_http,  
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
