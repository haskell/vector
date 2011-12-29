#define PHASE_STREAM [1]
#define PHASE_INNER  [0]

#define INLINE_STREAM INLINE PHASE_STREAM
#define INLINE_INNER  INLINE PHASE_INNER

#ifndef NOT_VECTOR_MODULE
import qualified Data.Vector.Internal.Check as Ck
#endif

#define ERROR(f)  (Ck.f __FILE__ __LINE__)
#define CHECK(f) (Ck.f __FILE__ __LINE__)

#define BOUNDS_ERROR(f) (ERROR(f) Ck.Bounds)
#define BOUNDS_CHECK(f) (CHECK(f) Ck.Bounds)

#define UNSAFE_ERROR(f) (ERROR(f) Ck.Unsafe)
#define UNSAFE_CHECK(f) (CHECK(f) Ck.Unsafe)

#define INTERNAL_ERROR(f) (ERROR(f) Ck.Internal)
#define INTERNAL_CHECK(f) (CHECK(f) Ck.Internal)


