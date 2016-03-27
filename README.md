**AWS Lib** is minimalist implementation, truly **object-oriented** and **interface-based** with **immutable** objects, 
for Amazon Web Services.

The code have some principles:
 1. all classes are sealed
 2. all methods return an interface or primitive type
 3. all public methods are implementations of interface methods
 4. all instances are immutable
 5. memory is released automatically 

## A "Bigger" Example using **Amazon S3**

Bellow you see a complete example to create a new Bucket and send a file on it.

``` pascal
program s3;
{$mode objfpc}{$H+}
uses
  aws_client,
  aws_s3;

begin
  TS3Region.New(
    TAWSClient.New(
      TAWSCredentials.New('YOUR_access_key', 'YOUR_secret_key', True)
    )
  )
  .Buckets
  .Put('mys3examplebucket', '/')
  .Objects
  .Put('foo.txt', 'plain', './foo.txt', '');
end.
```

First a Region object was created -- this is your connection to the Amazon services.

Second, using just one line, the code creates a new Bucket and put a new file on it.

No need to release memory!

To get this file that was sent, use the code:

``` pascal
  TS3Region.New(
    TAWSClient.New(
      TAWSCredentials.New('YOUR_access_key', 'YOUR_secret_key', True)
    )
  )
  .Buckets
  .Get('mys3examplebucket', '/')
  .Objects
  .Get('foo.txt', '/');
  .Stream
  .SaveToFile('./foo.txt');
```

To delete this file on server, use the code:

``` pascal
  TS3Region.New(
    TAWSClient.New(
      TAWSCredentials.New('YOUR_access_key', 'YOUR_secret_key', True)
    )
  )
  .Buckets
  .Get('mys3examplebucket', '/')
  .Objects
  .Delete('foo.txt');
```

## Dependencies 

There is only one dependency: [Synapse](http://synapse.ararat.cz/doku.php/download)

Synapse is used as HTTP client.  You can customize or create a new client using another lib like lNet, fpHttpClient, whatever.

## Got questions?

If you have questions or general suggestions, don't hesitate to submit
a new [Github issue](https://github.com/mdbs99/AWS/issues/new).

## Amazon S3 Documentation
* [REST API](http://docs.aws.amazon.com/AmazonS3/latest/API/APIRest.html)
* [Error responses](http://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html)
