#AWS Lib

AWS Lib is a minimalistic Object Pascal implementation for the Amazon Web Services.

This project is fully **object-oriented**, **interface-based** and all objects are **immutable objects**.

###Amazon S3
* [REST API](http://docs.aws.amazon.com/AmazonS3/latest/API/APIRest.html)
* [Error responses](http://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html)

``` pascal
program awstest;
{$mode objfpc}{$H+}
uses
  sysutils,
  aws_client,  
  aws_s3;
var
  Region: IS3Region;
begin
  Region := TS3Region.Create(
    TAWSClient.Create(
	  TAWSCredentials.Create('access_key', 'secret_key', True)
	)
  );
  if Region.IsOnline then
  begin
    // create a new bucket
    Region.Buckets.Put('colorpictures', '/');
    // delete the new bucket
    Region.Buckets.Delete('colorpictures', '/');
  end;
end.  
```
