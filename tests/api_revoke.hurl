# Verify that a revoked API key is rejected and a valid key still works.
# Requires:
#   --variable revoked_key=<key>   (key that was revoked via CLI before this runs)
#   --variable valid_key=<key>     (key that is still active)


# Revoked key gets 401
POST http://{{host}}/twirp/benchpress_api.BenchpressApi/GetJobStatus
Content-Type: application/json
Authorization: Bearer {{revoked_key}}
{"jobId": "some-id"}
HTTP 401
[Asserts]
jsonpath "$.code" == "unauthenticated"

# Valid key still works (job not found, but not 401)
POST http://{{host}}/twirp/benchpress_api.BenchpressApi/GetJobStatus
Content-Type: application/json
Authorization: Bearer {{valid_key}}
{"jobId": "some-id"}
HTTP 404
