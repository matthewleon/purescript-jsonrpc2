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
  "description": "A JSON RPC 2.0 response",
  "oneOf": [
    { "$ref": "#/definitions/success" },
    { "$ref": "#/definitions/error" },
    {
      "type": "array",
      "items": {
        "oneOf": [
          { "$ref": "#/definitions/success" },
          { "$ref": "#/definitions/error" }
        ]
      }
    }
  ],
  "definitions": {
    "common": {
      "required": [ "id", "jsonrpc" ],
      "not": {
        "description": "cannot have result and error at the same time",
        "required": [ "result", "error" ]
      },
      "type": "object",
      "properties": {
        "id": {
          "type": [ "string", "integer", "null" ],
          "note": [
            "spec says a number which should not contain a fractional part",
            "We choose integer here, but this is unenforceable with some languages"
          ]
        },
        "jsonrpc": { "enum": [ "2.0" ] }
      }
    },
    "success": {
      "description": "A success. The result member is then required and can be anything.",
      "allOf": [
        { "$ref": "#/definitions/common" },
        { "required": [ "result" ] }
      ]
    },
    "error": {
      "allOf" : [
        { "$ref": "#/definitions/common" },
        {
          "required": [ "error" ],
          "properties": {
            "error": {
              "type": "object",
              "required": [ "code", "message" ],
              "properties": {
                "code": {
                  "type": "integer",
                  "note": [ "unenforceable in some languages" ]
                },
                "message": { "type": "string" },
                "data": {
                  "description": "optional, can be anything"
                }
              }
            }
          }
        }
      ]
    }
  }
};

exports.validateResponse = ajv.compile(schema);
