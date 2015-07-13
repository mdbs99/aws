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
      TAWSCredentials.Create('access_key', 'secret_key', True)
    )
  );
  O := R.Buckets.Put('mys3examplebucket', '/').Objects.Put('foo.txt', 'plain', 'foo.txt', '');
end.

