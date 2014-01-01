#AWS Lib

AWS Lib is a Pascal implementation of Amazon S3 REST API for [Free Pascal](http://freepascal.org/).

It is minimalistic, clean and simple.

Amazon S3 REST API:
http://docs.aws.amazon.com/AmazonS3/latest/API/APIRest.html

``` pascal
program awstest;
{$mode objfpc}{$H+}
uses
  SysUtils, AwsS3;
const
  ACCESS_KEY = 'YOUR ACCESS KEY ID';
  SECRET_KEY = 'YOUR SECRET KEY';
  BUCKET_NAME = 'YOUR BUCKET';
  NEW_BUCKET_NAME = 'awspascal_s3_0001';
var
  s3: TAwsS3Client;
begin
  s3 := TAwsS3Client.Create;
  try
    s3.AccessKeyId := ACCESS_KEY;
    s3.SecretKey := SECRET_KEY;
    s3.UseSSL := True;
    try
      // check access
      if s3.GETService <> 200 then
	    raise Exception.Create('Access denied.');

      // bucket check
      if s3.GETBucket(BUCKET_NAME, '/') <> 200 then
        raise Exception.Create('Bucket do not exists or access denied.');

      // create a new bucket
      if s3.PUTBucket(NEW_BUCKET_NAME, '/') <> 200 then
        raise Exception.Create('Bucket was not created.');

      // delete the new bucket
      if s3.DELETEBucket(NEW_BUCKET_NAME, '/') <> 204 then  // No Content response
        raise Exception.Create('Bucket was not deleted.');
    except
      on E: Exception do
	    writeln(E.Message + #13 + Format('ERROR: #%d %s', [s3.HTTP.ResultCode, s3.HTTP.ResultString]));
    end;
  finally
    s3.Free;
  end;

  writeln('Everything works!');
  writeln;
end.  
```