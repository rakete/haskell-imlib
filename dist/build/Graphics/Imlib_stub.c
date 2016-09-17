#define IN_STG_CODE 0
#include "Rts.h"
#include "Stg.h"
#ifdef __cplusplus
extern "C" {
#endif
 
void GraphicsziImlib_d4y8(StgStablePtr the_stableptr, HsPtr a1, HsPtr a2)
{
Capability *cap;
HaskellObj ret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,rts_apply(cap,(StgClosure*)deRefStablePtr(the_stableptr),rts_mkPtr(cap,a1)),rts_mkPtr(cap,a2))) ,&ret);
rts_checkSchedStatus("GraphicsziImlib_d4y8",cap);
rts_unlock(cap);
}
 
HsInt32 GraphicsziImlib_d4yr(StgStablePtr the_stableptr, HsPtr a1, HsWord8 a2, HsInt32 a3, HsInt32 a4, HsInt32 a5, HsInt32 a6, void* original_return_addr)
{
Capability *cap;
HaskellObj ret;
HsInt32 cret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,rts_apply(cap,rts_apply(cap,rts_apply(cap,rts_apply(cap,rts_apply(cap,(StgClosure*)deRefStablePtr(the_stableptr),rts_mkPtr(cap,a1)),rts_mkWord8(cap,a2)),rts_mkInt32(cap,a3)),rts_mkInt32(cap,a4)),rts_mkInt32(cap,a5)),rts_mkInt32(cap,a6))) ,&ret);
rts_checkSchedStatus("GraphicsziImlib_d4yr",cap);
cret=rts_getInt32(ret);
rts_unlock(cap);
return cret;
}
#ifdef __cplusplus
}
#endif

