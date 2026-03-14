# Test suite for benchpress-server auth gating.
# Run with:
#   hurl --variable host=localhost:8083 tests/api_auth.hurl --test
#
# Requires a running server and a valid API key in the environment:
#   hurl --variable host=localhost:8083 --variable api_key=<key> tests/api_auth.hurl --test


# ── Read-only routes are public ───────────────────────────────────────────────

GET http://{{host}}/
HTTP 200

GET http://{{host}}/provers
HTTP 200

GET http://{{host}}/tasks
HTTP 200


# ── Unauthenticated write requests get 401 ───────────────────────────────────

POST http://{{host}}/run/sometask
HTTP 401
[Asserts]
jsonpath "$.code" == "unauthenticated"

POST http://{{host}}/interrupt/fake-uuid
HTTP 401
[Asserts]
jsonpath "$.code" == "unauthenticated"


# ── Unauthenticated Twirp calls get 401 ──────────────────────────────────────

POST http://{{host}}/twirp/benchpress_api.BenchpressApi/NewJob
Content-Type: application/json
{"provers": ["z3"], "paths": ["/tmp"]}
HTTP 401
[Asserts]
jsonpath "$.code" == "unauthenticated"

POST http://{{host}}/twirp/benchpress_api.BenchpressApi/GetJobStatus
Content-Type: application/json
{"job_id": "fake-id"}
HTTP 401
[Asserts]
jsonpath "$.code" == "unauthenticated"

POST http://{{host}}/twirp/benchpress_api.BenchpressApi/CancelJob
Content-Type: application/json
{"job_id": "fake-id"}
HTTP 401
[Asserts]
jsonpath "$.code" == "unauthenticated"


# ── Authenticated Twirp: bad input → domain error, not 401 ───────────────────

POST http://{{host}}/twirp/benchpress_api.BenchpressApi/NewJob
Content-Type: application/json
Authorization: Bearer {{api_key}}
{"provers": ["z3"], "paths": ["/tmp"]}
HTTP 400
[Asserts]
jsonpath "$.code" == "invalid_argument"

POST http://{{host}}/twirp/benchpress_api.BenchpressApi/GetJobStatus
Content-Type: application/json
Authorization: Bearer {{api_key}}
{"job_id": "fake-id"}
HTTP 404
[Asserts]
jsonpath "$.code" == "not_found"

POST http://{{host}}/twirp/benchpress_api.BenchpressApi/CancelJob
Content-Type: application/json
Authorization: Bearer {{api_key}}
{"job_id": "fake-id"}
HTTP 404
[Asserts]
jsonpath "$.code" == "not_found"


# ── Invalid Bearer token gets 401 ────────────────────────────────────────────

POST http://{{host}}/twirp/benchpress_api.BenchpressApi/GetJobStatus
Content-Type: application/json
Authorization: Bearer notavalidkey
{"job_id": "fake-id"}
HTTP 401
[Asserts]
jsonpath "$.code" == "unauthenticated"
