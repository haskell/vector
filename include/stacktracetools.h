#if MIN_VERSION_base(4,9,0)
import GHC.Stack (HasCallStack)
#define CHECK(f) (withFrozenCallStack Ck.f __FILE__ __LINE__)
#else
#define HasCallStack (Eq ())
#endif
