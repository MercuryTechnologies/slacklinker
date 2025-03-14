## Linear GraphQL integration

This uses `graphql-client` and its `graphql-codegen` code generator to create the `.API` module in here.

To generate the code, run `graphql-codegen` (provided via Nix shell) in the root of the repo.

Sadly we cannot automatically check that the generated code is up to date since generating the code requires hitting the World Wide Web for Linear's GraphQL schema.
