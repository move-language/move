/// This module defines a set of canonical error codes which can be used as abort codes in Move `assert!` and `abort`
/// statements. The move standard library uses those codes.
///
/// Those codes have been adopted from Google's canonical error codes, which in turn are derived from Unix error codes
/// [see here](https://cloud.google.com/apis/design/errors#handling_errors). Each code has an associated HTTP
/// error code which can be used in REST apis. The mapping from error code to http code is not 1:1; error codes
/// here are a bit richer than HTTP codes.
///
/// NOTE: Move currently does not support public constants, so we use functions. This may change in
/// the future.
module std::errors {

  /// Caller specified an invalid argument (http: 400)
  const INVALID_ARGUMENT: u64 = 2001;

  /// An input or result of a computation is out of range (http: 400)
  const OUT_OF_RANGE: u64 = 2002;

  /// The system is not in a state where the operation can be performed (http: 400)
  const INVALID_STATE: u64 = 2003;

  /// Request not authenticated due to missing, invalid, or expired auth token (http: 401)
  const UNAUTHENTICATED: u64 = 2004;

  /// client does not have sufficient permission (http: 403)
  const PERMISSION_DENIED: u64 = 2005;

  /// A specified resource is not found (http: 404)
  const NOT_FOUND: u64 = 2006;

  /// Concurrency conflict, such as read-modify-write conflict (http: 409)
  const ABORTED: u64 = 2007;

  /// The resource that a client tried to create already exists (http: 409)
  const ALREADY_EXISTS: u64 = 2008;

  /// Out of gas or other forms of quota (http: 429)
  const RESOURCE_EXHAUSTED: u64 = 2009;

  /// Request cancelled by the client (http: 499)
  const CANCELLED: u64 = 2010;

  /// Internal error (http: 500)
  const INTERNAL: u64 = 2011;

  /// Feature not implemented (http: 501)
  const NOT_IMPLEMENTED: u64 = 2012;

  /// The service is currently unavailable. Indicates that a retry could solve the issue (http: 503)
  const UNAVAILABLE: u64 = 2013;

  public fun invalid_argument(): u64 {  INVALID_ARGUMENT }
  public fun out_of_range(): u64 {  OUT_OF_RANGE }
  public fun invalid_state(): u64 {  INVALID_STATE }
  public fun unauthenticated(): u64 { UNAUTHENTICATED }
  public fun permission_denied(): u64 { PERMISSION_DENIED }
  public fun not_found(): u64 { NOT_FOUND }
  public fun aborted(): u64 { ABORTED }
  public fun already_exists(): u64 { ALREADY_EXISTS }
  public fun resource_exhausted(): u64 {  RESOURCE_EXHAUSTED }
  public fun internal(): u64 {  INTERNAL }
  public fun not_implemented(): u64 {  NOT_IMPLEMENTED }
  public fun unavailable(): u64 { UNAVAILABLE }
}

