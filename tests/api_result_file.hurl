# Test that GetJobStatus returns a non-empty result_file once a job completes,
# and that repeated calls after completion are idempotent.
# Requires: --variable host=... --variable api_key=...
# Server config must define prover "always-true" and dir "/tmp/bench".


# ── Submit a fast job and wait for it to complete ─────────────────────────────

POST http://{{host}}/twirp/benchpress_api.BenchpressApi/NewJob
Content-Type: application/json
Authorization: Bearer {{api_key}}
{"provers": ["always-true"], "paths": ["/tmp/bench"]}
HTTP 200
[Captures]
job_id: jsonpath "$.jobId"

# Poll until status == COMPLETED
POST http://{{host}}/twirp/benchpress_api.BenchpressApi/GetJobStatus
Content-Type: application/json
Authorization: Bearer {{api_key}}
[Options]
retry: 30
retry-interval: 500
{"jobId": "{{job_id}}"}
HTTP 200
[Asserts]
jsonpath "$.status" == "COMPLETED"
[Captures]
result_file: jsonpath "$.resultFile"

# result_file must be non-empty once completed
POST http://{{host}}/twirp/benchpress_api.BenchpressApi/GetJobStatus
Content-Type: application/json
Authorization: Bearer {{api_key}}
{"jobId": "{{job_id}}"}
HTTP 200
[Asserts]
jsonpath "$.status" == "COMPLETED"
jsonpath "$.resultFile" != ""

# Repeated poll returns COMPLETED with the same result_file (idempotent)
POST http://{{host}}/twirp/benchpress_api.BenchpressApi/GetJobStatus
Content-Type: application/json
Authorization: Bearer {{api_key}}
{"jobId": "{{job_id}}"}
HTTP 200
[Asserts]
jsonpath "$.status" == "COMPLETED"
jsonpath "$.resultFile" == "{{result_file}}"


# ── A cancelled job does not set result_file ──────────────────────────────────

POST http://{{host}}/twirp/benchpress_api.BenchpressApi/NewJob
Content-Type: application/json
Authorization: Bearer {{api_key}}
{"provers": ["slow"], "paths": ["/tmp/bench"]}
HTTP 200
[Captures]
slow_id: jsonpath "$.jobId"

POST http://{{host}}/twirp/benchpress_api.BenchpressApi/CancelJob
Content-Type: application/json
Authorization: Bearer {{api_key}}
{"jobId": "{{slow_id}}"}
HTTP 200

POST http://{{host}}/twirp/benchpress_api.BenchpressApi/GetJobStatus
Content-Type: application/json
Authorization: Bearer {{api_key}}
[Options]
retry: 10
retry-interval: 500
{"jobId": "{{slow_id}}"}
HTTP 200
[Asserts]
jsonpath "$.status" == "CANCELLED"
