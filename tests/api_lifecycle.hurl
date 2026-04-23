# Full job lifecycle tests for benchpress-server.
# Requires two API keys for different users:
#   --variable api_key1=<key>    (user 1)
#   --variable api_key2=<key>    (user 2, for isolation checks)
# The server config must define provers "always-true" and "slow",
# and a dir at /tmp/bench.


# ── Happy-path: start job, poll until complete, browse results ────────────────

# Start a fast job; capture the job_id
POST http://{{host}}/twirp/benchpress_api.BenchpressApi/NewJob
Content-Type: application/json
Authorization: Bearer {{api_key1}}
{"provers": ["always-true"], "paths": ["/tmp/bench"]}
HTTP 200
[Captures]
job_id: jsonpath "$.jobId"

# Poll until COMPLETED (retries every 500 ms, up to 15 s)
POST http://{{host}}/twirp/benchpress_api.BenchpressApi/GetJobStatus
Content-Type: application/json
Authorization: Bearer {{api_key1}}
[Options]
retry: 30
retry-interval: 500
{"jobId": "{{job_id}}"}
HTTP 200
[Asserts]
jsonpath "$.jobId" == "{{job_id}}"
jsonpath "$.status" == "COMPLETED"

# Cross-user isolation: user2 cannot see user1's job
POST http://{{host}}/twirp/benchpress_api.BenchpressApi/GetJobStatus
Content-Type: application/json
Authorization: Bearer {{api_key2}}
{"jobId": "{{job_id}}"}
HTTP 404
[Asserts]
jsonpath "$.code" == "not_found"

# Results are browsable publicly (no auth required)
GET http://{{host}}/list-benchs
HTTP 200


# ── Cancellation lifecycle ────────────────────────────────────────────────────

# Start a slow job (sleep 30 per file) so we can cancel it
POST http://{{host}}/twirp/benchpress_api.BenchpressApi/NewJob
Content-Type: application/json
Authorization: Bearer {{api_key1}}
{"provers": ["slow"], "paths": ["/tmp/bench"]}
HTTP 200
[Captures]
slow_job_id: jsonpath "$.jobId"

# Job exists and belongs to user1
POST http://{{host}}/twirp/benchpress_api.BenchpressApi/GetJobStatus
Content-Type: application/json
Authorization: Bearer {{api_key1}}
{"jobId": "{{slow_job_id}}"}
HTTP 200
[Asserts]
jsonpath "$.jobId" == "{{slow_job_id}}"

# Cross-user isolation: user2 cannot cancel user1's job
POST http://{{host}}/twirp/benchpress_api.BenchpressApi/CancelJob
Content-Type: application/json
Authorization: Bearer {{api_key2}}
{"jobId": "{{slow_job_id}}"}
HTTP 404
[Asserts]
jsonpath "$.code" == "not_found"

# User1 cancels their own job
POST http://{{host}}/twirp/benchpress_api.BenchpressApi/CancelJob
Content-Type: application/json
Authorization: Bearer {{api_key1}}
{"jobId": "{{slow_job_id}}"}
HTTP 200

# Status is now CANCELLED (retry: worker may take a moment to act on interrupt)
POST http://{{host}}/twirp/benchpress_api.BenchpressApi/GetJobStatus
Content-Type: application/json
Authorization: Bearer {{api_key1}}
[Options]
retry: 20
retry-interval: 500
{"jobId": "{{slow_job_id}}"}
HTTP 200
[Asserts]
jsonpath "$.status" == "CANCELLED"
