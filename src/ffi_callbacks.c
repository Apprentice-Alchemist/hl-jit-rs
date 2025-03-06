#include <ffi.h>
#include <hl.h>

static ffi_type *to_ffi(hl_type *t) {
  if (hl_is_ptr(t)) {
    return &ffi_type_pointer;
  } else {
    switch (t->kind) {
    case HVOID:
      return &ffi_type_void;
    case HUI8:
      return &ffi_type_uint8;
    case HUI16:
      return &ffi_type_uint16;
    case HI32:
      return &ffi_type_sint32;
    case HI64:
      return &ffi_type_sint64;
    case HF32:
      return &ffi_type_float;
    case HF64:
      return &ffi_type_double;
    case HBOOL:
      return &ffi_type_uint8;
    default:
      abort();
      break;
    }
  }
}

HL_API void *ffi_static_call(void *fun, hl_type *ft, void **args,
                             vdynamic *out) {
  ffi_cif cif = {0};
  ffi_type **atypes = alloca(sizeof(ffi_type *) * ft->fun->nargs);
  void **avalues = alloca(sizeof(ffi_raw) * ft->fun->nargs);
  for (int i = 0; i < ft->fun->nargs; i++) {
    hl_type *t = ft->fun->args[i];
    atypes[i] = to_ffi(t);
    if (hl_is_ptr(t)) {
      avalues[i] = &args[i];
    } else {
      avalues[i] = args[i];
    }
  }
  ffi_prep_cif(&cif, FFI_DEFAULT_ABI, ft->fun->nargs, to_ffi(ft->fun->ret),
               atypes);
  ffi_call(&cif, fun, &out->v.ptr, avalues);
  if (hl_is_ptr(ft->fun->ret)) {
    return out->v.ptr;
  } else {
    return &out->v.ptr;
  }
}

static void call_wrapper(ffi_cif *cif, void *ret, void **args,
                         void *user_data) {
  (void)user_data;
  for (unsigned int i = 0; i < cif->nargs; i++) {
    if (cif->arg_types[i] == &ffi_type_pointer) {
      args[i] = *(void **)args[i];
    }
  }
  vdynamic dyn_ret = {0};
  void *pret = hl_wrapper_call(args[0], args + 1, &dyn_ret);
  if (cif->rtype->type == FFI_TYPE_VOID) {
    return;
  } else if (cif->rtype->type == FFI_TYPE_POINTER) {
    *((void **)ret) = pret;
  } else {
    memcpy(ret, &dyn_ret.v.i64, sizeof(ffi_arg));
  }
}

HL_API void *ffi_get_wrapper(hl_type *t) {
  void *codeloc;
  ffi_closure *closure = ffi_closure_alloc(sizeof(ffi_closure), &codeloc);
  ffi_cif *cif = malloc(sizeof(ffi_cif));
  int nargs = t->fun->nargs;
  ffi_type **atypes = malloc((sizeof *atypes) * (nargs + 1));
  atypes[0] = &ffi_type_pointer;
  for (int i = 0; i < t->fun->nargs; i++) {
    atypes[i + 1] = to_ffi(t->fun->args[i]);
  }
  ffi_prep_cif(cif, FFI_DEFAULT_ABI, nargs + 1, to_ffi(t->fun->ret), atypes);
  ffi_prep_closure_loc(closure, cif, call_wrapper, NULL, codeloc);
  return codeloc;
}