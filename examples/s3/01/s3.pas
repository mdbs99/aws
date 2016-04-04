program s3;
{$mode objfpc}{$H+}
uses
  aws_credentials,
  aws_client,
  aws_s3;

begin
  TS3Region.New(
    TAWSClient.New(
      TAWSSignatureVersion1.New(
        TAWSCredentials.New('access_key', 'secret_key', True)
      )
    )
  )
  .Buckets
  .Put('mys3examplebucket', '/')
  .Objects
  .Put('foo.txt', 'plain', 'foo.txt', '');
end.

