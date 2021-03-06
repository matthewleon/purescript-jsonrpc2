'use strict';

const Ajv = require('ajv');
const ajv = new Ajv({
  schemaId: 'id',
  meta: false,
});

const metaSchema = require('ajv/lib/refs/json-schema-draft-04.json');
ajv.addMetaSchema(metaSchema);
ajv._opts.defaultMeta = metaSchema.id;

const schema = {
  "$schema": "http://json-schema.org/draft-04/schema#",
  "description": "A JSON RPC 2.0 request",
  "oneOf": [
    {
      "description": "An individual request",
      "$ref": "#/definitions/request"
    },
    {
      "description": "An array of requests",
      "type": "array",
      "items": { "$ref": "#/definitions/request" }
    }
  ],
  "definitions": {
    "request": {
      "type": "object",
      "required": [ "jsonrpc", "method" ],
      "properties": {
        "jsonrpc": { "enum": [ "2.0" ] },
        "method": {
          "type": "string"
        },
        "id": {
          "type": [ "string", "number", "null" ],
          "note": [
            "While allowed, null should be avoided: http://www.jsonrpc.org/specification#id1",
            "While allowed, a number with a fractional part should be avoided: http://www.jsonrpc.org/specification#id2"
          ]
        },
        "params": {
          "type": [ "array", "object" ]
        }
      }
    }
  }
}

exports.validateRequest = ajv.compile(schema);
