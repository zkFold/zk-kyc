# zkKYC server

Working with GHC 9.6.7 and Cabal 3.12.1.0

## Running the server

Replace XXXX with the port number (default is 3000) and timeout (default is 300)

```bash
cabal run kyc-server
# or
cabal run kyc-server -- --port XXXX --timeout XXX
```

When the server is running, you can query it via curl (assuming that the port is set to 3000):

Prove request:
```bash
curl -X POST -H "Content-Type: application/json" --connect-timeout 300 -d @example-json/kyc-data.json localhost:3000/prove
```

Verify request:
```bash
curl -X POST -H "Content-Type: application/json" --connect-timeout 300 -d @example-json/prover-output.json localhost:3000/verify
```

## Docs
To view the documentation, open the file `docs/index.html` using your browser