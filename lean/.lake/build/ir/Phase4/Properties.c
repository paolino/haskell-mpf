// Lean compiler output
// Module: Phase4.Properties
// Imports: public import Init public import Phase4.Domain
#include <lean/lean.h>
#if defined(__clang__)
#pragma clang diagnostic ignored "-Wunused-parameter"
#pragma clang diagnostic ignored "-Wunused-label"
#elif defined(__GNUC__) && !defined(__CLANG__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wunused-label"
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
#endif
#ifdef __cplusplus
extern "C" {
#endif
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyInv___redArg(lean_object*, lean_object*, lean_object*, lean_object*);
lean_object* l_List_mapTR_loop___redArg(lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__0___boxed(lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__4___boxed(lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyEvents___redArg(lean_object*, lean_object*, lean_object*, lean_object*);
lean_object* l_List_filterTR_loop___redArg(lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_inverseOf___redArg___lam__1___boxed(lean_object*, lean_object*, lean_object*);
LEAN_EXPORT uint8_t lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__1(lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__2(lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__1___boxed(lean_object*, lean_object*, lean_object*);
uint8_t l_List_all___redArg(lean_object*, lean_object*);
lean_object* l_List_find_x3f___redArg(lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyInvs(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
lean_object* l_List_foldl___redArg(lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyInv(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyEvent___redArg(lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyInvs___redArg(lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyEvents(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__3___boxed(lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_inverseOf___redArg(lean_object*, lean_object*, lean_object*);
LEAN_EXPORT uint8_t lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__4(lean_object*, lean_object*, lean_object*);
lean_object* l_List_appendTR___redArg(lean_object*, lean_object*);
LEAN_EXPORT uint8_t lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__3(lean_object*, lean_object*, lean_object*);
static lean_object* lp_phase4_x2dinvariants_Phase4_inverseOf___redArg___closed__0;
LEAN_EXPORT uint8_t lp_phase4_x2dinvariants_Phase4_inverseOf___redArg___lam__1(lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyEvent(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_inverseOf___redArg___lam__0(lean_object*);
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_inverseOf(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT uint8_t lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__0(lean_object*, lean_object*, lean_object*);
LEAN_EXPORT uint8_t lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__0(lean_object* x_1, lean_object* x_2, lean_object* x_3) {
_start:
{
lean_object* x_4; uint8_t x_5; 
x_4 = lean_apply_2(x_1, x_2, x_3);
x_5 = lean_unbox(x_4);
if (x_5 == 0)
{
uint8_t x_6; 
x_6 = 1;
return x_6;
}
else
{
uint8_t x_7; 
x_7 = 0;
return x_7;
}
}
}
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__0___boxed(lean_object* x_1, lean_object* x_2, lean_object* x_3) {
_start:
{
uint8_t x_4; lean_object* x_5; 
x_4 = lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__0(x_1, x_2, x_3);
x_5 = lean_box(x_4);
return x_5;
}
}
LEAN_EXPORT uint8_t lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__1(lean_object* x_1, lean_object* x_2, lean_object* x_3) {
_start:
{
lean_object* x_4; uint8_t x_5; 
x_4 = lean_alloc_closure((void*)(lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__0___boxed), 3, 2);
lean_closure_set(x_4, 0, x_1);
lean_closure_set(x_4, 1, x_3);
x_5 = l_List_all___redArg(x_2, x_4);
return x_5;
}
}
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__1___boxed(lean_object* x_1, lean_object* x_2, lean_object* x_3) {
_start:
{
uint8_t x_4; lean_object* x_5; 
x_4 = lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__1(x_1, x_2, x_3);
x_5 = lean_box(x_4);
return x_5;
}
}
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__2(lean_object* x_1, lean_object* x_2, lean_object* x_3, lean_object* x_4) {
_start:
{
lean_object* x_5; lean_object* x_6; uint8_t x_7; 
x_5 = lean_ctor_get(x_4, 0);
lean_inc(x_5);
x_6 = lean_apply_2(x_1, x_5, x_2);
x_7 = lean_unbox(x_6);
if (x_7 == 0)
{
lean_dec(x_3);
return x_4;
}
else
{
uint8_t x_8; 
lean_inc(x_5);
x_8 = !lean_is_exclusive(x_4);
if (x_8 == 0)
{
lean_object* x_9; lean_object* x_10; 
x_9 = lean_ctor_get(x_4, 1);
lean_dec(x_9);
x_10 = lean_ctor_get(x_4, 0);
lean_dec(x_10);
lean_ctor_set(x_4, 1, x_3);
return x_4;
}
else
{
lean_object* x_11; 
lean_dec(x_4);
x_11 = lean_alloc_ctor(0, 2, 0);
lean_ctor_set(x_11, 0, x_5);
lean_ctor_set(x_11, 1, x_3);
return x_11;
}
}
}
}
LEAN_EXPORT uint8_t lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__3(lean_object* x_1, lean_object* x_2, lean_object* x_3) {
_start:
{
lean_object* x_4; uint8_t x_5; 
x_4 = lean_apply_2(x_1, x_3, x_2);
x_5 = lean_unbox(x_4);
if (x_5 == 0)
{
uint8_t x_6; 
x_6 = 1;
return x_6;
}
else
{
uint8_t x_7; 
x_7 = 0;
return x_7;
}
}
}
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__3___boxed(lean_object* x_1, lean_object* x_2, lean_object* x_3) {
_start:
{
uint8_t x_4; lean_object* x_5; 
x_4 = lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__3(x_1, x_2, x_3);
x_5 = lean_box(x_4);
return x_5;
}
}
LEAN_EXPORT uint8_t lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__4(lean_object* x_1, lean_object* x_2, lean_object* x_3) {
_start:
{
lean_object* x_4; lean_object* x_5; uint8_t x_6; 
x_4 = lean_ctor_get(x_3, 0);
lean_inc(x_4);
lean_dec_ref(x_3);
x_5 = lean_apply_2(x_1, x_4, x_2);
x_6 = lean_unbox(x_5);
if (x_6 == 0)
{
uint8_t x_7; 
x_7 = 1;
return x_7;
}
else
{
uint8_t x_8; 
x_8 = 0;
return x_8;
}
}
}
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__4___boxed(lean_object* x_1, lean_object* x_2, lean_object* x_3) {
_start:
{
uint8_t x_4; lean_object* x_5; 
x_4 = lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__4(x_1, x_2, x_3);
x_5 = lean_box(x_4);
return x_5;
}
}
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyEvent___redArg(lean_object* x_1, lean_object* x_2, lean_object* x_3, lean_object* x_4) {
_start:
{
switch (lean_obj_tag(x_4)) {
case 0:
{
uint8_t x_5; 
lean_dec_ref(x_2);
lean_dec_ref(x_1);
x_5 = !lean_is_exclusive(x_4);
if (x_5 == 0)
{
uint8_t x_6; 
x_6 = !lean_is_exclusive(x_3);
if (x_6 == 0)
{
lean_object* x_7; lean_object* x_8; 
x_7 = lean_ctor_get(x_3, 0);
x_8 = lean_alloc_ctor(1, 2, 0);
lean_ctor_set(x_8, 0, x_4);
lean_ctor_set(x_8, 1, x_7);
lean_ctor_set(x_3, 0, x_8);
return x_3;
}
else
{
lean_object* x_9; lean_object* x_10; lean_object* x_11; lean_object* x_12; 
x_9 = lean_ctor_get(x_3, 0);
x_10 = lean_ctor_get(x_3, 1);
lean_inc(x_10);
lean_inc(x_9);
lean_dec(x_3);
x_11 = lean_alloc_ctor(1, 2, 0);
lean_ctor_set(x_11, 0, x_4);
lean_ctor_set(x_11, 1, x_9);
x_12 = lean_alloc_ctor(0, 2, 0);
lean_ctor_set(x_12, 0, x_11);
lean_ctor_set(x_12, 1, x_10);
return x_12;
}
}
else
{
lean_object* x_13; lean_object* x_14; lean_object* x_15; lean_object* x_16; lean_object* x_17; lean_object* x_18; lean_object* x_19; lean_object* x_20; 
x_13 = lean_ctor_get(x_4, 0);
x_14 = lean_ctor_get(x_4, 1);
lean_inc(x_14);
lean_inc(x_13);
lean_dec(x_4);
x_15 = lean_ctor_get(x_3, 0);
lean_inc(x_15);
x_16 = lean_ctor_get(x_3, 1);
lean_inc(x_16);
if (lean_is_exclusive(x_3)) {
 lean_ctor_release(x_3, 0);
 lean_ctor_release(x_3, 1);
 x_17 = x_3;
} else {
 lean_dec_ref(x_3);
 x_17 = lean_box(0);
}
x_18 = lean_alloc_ctor(0, 2, 0);
lean_ctor_set(x_18, 0, x_13);
lean_ctor_set(x_18, 1, x_14);
x_19 = lean_alloc_ctor(1, 2, 0);
lean_ctor_set(x_19, 0, x_18);
lean_ctor_set(x_19, 1, x_15);
if (lean_is_scalar(x_17)) {
 x_20 = lean_alloc_ctor(0, 2, 0);
} else {
 x_20 = x_17;
}
lean_ctor_set(x_20, 0, x_19);
lean_ctor_set(x_20, 1, x_16);
return x_20;
}
}
case 1:
{
lean_object* x_21; uint8_t x_22; 
lean_dec_ref(x_2);
lean_dec_ref(x_1);
x_21 = lean_ctor_get(x_4, 0);
lean_inc(x_21);
lean_dec_ref(x_4);
x_22 = !lean_is_exclusive(x_3);
if (x_22 == 0)
{
lean_object* x_23; lean_object* x_24; 
x_23 = lean_ctor_get(x_3, 1);
x_24 = lean_alloc_ctor(1, 2, 0);
lean_ctor_set(x_24, 0, x_21);
lean_ctor_set(x_24, 1, x_23);
lean_ctor_set(x_3, 1, x_24);
return x_3;
}
else
{
lean_object* x_25; lean_object* x_26; lean_object* x_27; lean_object* x_28; 
x_25 = lean_ctor_get(x_3, 0);
x_26 = lean_ctor_get(x_3, 1);
lean_inc(x_26);
lean_inc(x_25);
lean_dec(x_3);
x_27 = lean_alloc_ctor(1, 2, 0);
lean_ctor_set(x_27, 0, x_21);
lean_ctor_set(x_27, 1, x_26);
x_28 = lean_alloc_ctor(0, 2, 0);
lean_ctor_set(x_28, 0, x_25);
lean_ctor_set(x_28, 1, x_27);
return x_28;
}
}
case 2:
{
lean_object* x_29; lean_object* x_30; lean_object* x_31; uint8_t x_32; 
x_29 = lean_ctor_get(x_4, 0);
lean_inc(x_29);
x_30 = lean_ctor_get(x_4, 1);
lean_inc(x_30);
x_31 = lean_ctor_get(x_4, 2);
lean_inc(x_31);
lean_dec_ref(x_4);
x_32 = !lean_is_exclusive(x_3);
if (x_32 == 0)
{
lean_object* x_33; lean_object* x_34; lean_object* x_35; lean_object* x_36; lean_object* x_37; lean_object* x_38; lean_object* x_39; 
x_33 = lean_ctor_get(x_3, 0);
x_34 = lean_ctor_get(x_3, 1);
x_35 = lean_alloc_closure((void*)(lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__1___boxed), 3, 2);
lean_closure_set(x_35, 0, x_2);
lean_closure_set(x_35, 1, x_31);
x_36 = lean_alloc_closure((void*)(lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__2), 4, 3);
lean_closure_set(x_36, 0, x_1);
lean_closure_set(x_36, 1, x_29);
lean_closure_set(x_36, 2, x_30);
x_37 = lean_box(0);
x_38 = l_List_mapTR_loop___redArg(x_36, x_33, x_37);
x_39 = l_List_filterTR_loop___redArg(x_35, x_34, x_37);
lean_ctor_set(x_3, 1, x_39);
lean_ctor_set(x_3, 0, x_38);
return x_3;
}
else
{
lean_object* x_40; lean_object* x_41; lean_object* x_42; lean_object* x_43; lean_object* x_44; lean_object* x_45; lean_object* x_46; lean_object* x_47; 
x_40 = lean_ctor_get(x_3, 0);
x_41 = lean_ctor_get(x_3, 1);
lean_inc(x_41);
lean_inc(x_40);
lean_dec(x_3);
x_42 = lean_alloc_closure((void*)(lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__1___boxed), 3, 2);
lean_closure_set(x_42, 0, x_2);
lean_closure_set(x_42, 1, x_31);
x_43 = lean_alloc_closure((void*)(lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__2), 4, 3);
lean_closure_set(x_43, 0, x_1);
lean_closure_set(x_43, 1, x_29);
lean_closure_set(x_43, 2, x_30);
x_44 = lean_box(0);
x_45 = l_List_mapTR_loop___redArg(x_43, x_40, x_44);
x_46 = l_List_filterTR_loop___redArg(x_42, x_41, x_44);
x_47 = lean_alloc_ctor(0, 2, 0);
lean_ctor_set(x_47, 0, x_45);
lean_ctor_set(x_47, 1, x_46);
return x_47;
}
}
case 3:
{
lean_object* x_48; uint8_t x_49; 
lean_dec_ref(x_1);
x_48 = lean_ctor_get(x_4, 0);
lean_inc(x_48);
lean_dec_ref(x_4);
x_49 = !lean_is_exclusive(x_3);
if (x_49 == 0)
{
lean_object* x_50; lean_object* x_51; lean_object* x_52; lean_object* x_53; 
x_50 = lean_ctor_get(x_3, 1);
x_51 = lean_alloc_closure((void*)(lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__3___boxed), 3, 2);
lean_closure_set(x_51, 0, x_2);
lean_closure_set(x_51, 1, x_48);
x_52 = lean_box(0);
x_53 = l_List_filterTR_loop___redArg(x_51, x_50, x_52);
lean_ctor_set(x_3, 1, x_53);
return x_3;
}
else
{
lean_object* x_54; lean_object* x_55; lean_object* x_56; lean_object* x_57; lean_object* x_58; lean_object* x_59; 
x_54 = lean_ctor_get(x_3, 0);
x_55 = lean_ctor_get(x_3, 1);
lean_inc(x_55);
lean_inc(x_54);
lean_dec(x_3);
x_56 = lean_alloc_closure((void*)(lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__3___boxed), 3, 2);
lean_closure_set(x_56, 0, x_2);
lean_closure_set(x_56, 1, x_48);
x_57 = lean_box(0);
x_58 = l_List_filterTR_loop___redArg(x_56, x_55, x_57);
x_59 = lean_alloc_ctor(0, 2, 0);
lean_ctor_set(x_59, 0, x_54);
lean_ctor_set(x_59, 1, x_58);
return x_59;
}
}
default: 
{
lean_object* x_60; uint8_t x_61; 
lean_dec_ref(x_2);
x_60 = lean_ctor_get(x_4, 0);
lean_inc(x_60);
lean_dec_ref(x_4);
x_61 = !lean_is_exclusive(x_3);
if (x_61 == 0)
{
lean_object* x_62; lean_object* x_63; lean_object* x_64; lean_object* x_65; 
x_62 = lean_ctor_get(x_3, 0);
x_63 = lean_alloc_closure((void*)(lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__4___boxed), 3, 2);
lean_closure_set(x_63, 0, x_1);
lean_closure_set(x_63, 1, x_60);
x_64 = lean_box(0);
x_65 = l_List_filterTR_loop___redArg(x_63, x_62, x_64);
lean_ctor_set(x_3, 0, x_65);
return x_3;
}
else
{
lean_object* x_66; lean_object* x_67; lean_object* x_68; lean_object* x_69; lean_object* x_70; lean_object* x_71; 
x_66 = lean_ctor_get(x_3, 0);
x_67 = lean_ctor_get(x_3, 1);
lean_inc(x_67);
lean_inc(x_66);
lean_dec(x_3);
x_68 = lean_alloc_closure((void*)(lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__4___boxed), 3, 2);
lean_closure_set(x_68, 0, x_1);
lean_closure_set(x_68, 1, x_60);
x_69 = lean_box(0);
x_70 = l_List_filterTR_loop___redArg(x_68, x_66, x_69);
x_71 = lean_alloc_ctor(0, 2, 0);
lean_ctor_set(x_71, 0, x_70);
lean_ctor_set(x_71, 1, x_67);
return x_71;
}
}
}
}
}
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyEvent(lean_object* x_1, lean_object* x_2, lean_object* x_3, lean_object* x_4, lean_object* x_5, lean_object* x_6, lean_object* x_7) {
_start:
{
lean_object* x_8; 
x_8 = lp_phase4_x2dinvariants_Phase4_applyEvent___redArg(x_4, x_5, x_6, x_7);
return x_8;
}
}
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyEvents___redArg(lean_object* x_1, lean_object* x_2, lean_object* x_3, lean_object* x_4) {
_start:
{
lean_object* x_5; lean_object* x_6; 
x_5 = lean_alloc_closure((void*)(lp_phase4_x2dinvariants_Phase4_applyEvent), 7, 5);
lean_closure_set(x_5, 0, lean_box(0));
lean_closure_set(x_5, 1, lean_box(0));
lean_closure_set(x_5, 2, lean_box(0));
lean_closure_set(x_5, 3, x_1);
lean_closure_set(x_5, 4, x_2);
x_6 = l_List_foldl___redArg(x_5, x_3, x_4);
return x_6;
}
}
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyEvents(lean_object* x_1, lean_object* x_2, lean_object* x_3, lean_object* x_4, lean_object* x_5, lean_object* x_6, lean_object* x_7) {
_start:
{
lean_object* x_8; 
x_8 = lp_phase4_x2dinvariants_Phase4_applyEvents___redArg(x_4, x_5, x_6, x_7);
return x_8;
}
}
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_inverseOf___redArg___lam__0(lean_object* x_1) {
_start:
{
lean_object* x_2; 
x_2 = lean_alloc_ctor(2, 1, 0);
lean_ctor_set(x_2, 0, x_1);
return x_2;
}
}
LEAN_EXPORT uint8_t lp_phase4_x2dinvariants_Phase4_inverseOf___redArg___lam__1(lean_object* x_1, lean_object* x_2, lean_object* x_3) {
_start:
{
lean_object* x_4; lean_object* x_5; uint8_t x_6; 
x_4 = lean_ctor_get(x_3, 0);
lean_inc(x_4);
lean_dec_ref(x_3);
x_5 = lean_apply_2(x_1, x_4, x_2);
x_6 = lean_unbox(x_5);
return x_6;
}
}
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_inverseOf___redArg___lam__1___boxed(lean_object* x_1, lean_object* x_2, lean_object* x_3) {
_start:
{
uint8_t x_4; lean_object* x_5; 
x_4 = lp_phase4_x2dinvariants_Phase4_inverseOf___redArg___lam__1(x_1, x_2, x_3);
x_5 = lean_box(x_4);
return x_5;
}
}
static lean_object* _init_lp_phase4_x2dinvariants_Phase4_inverseOf___redArg___closed__0() {
_start:
{
lean_object* x_1; 
x_1 = lean_alloc_closure((void*)(lp_phase4_x2dinvariants_Phase4_inverseOf___redArg___lam__0), 1, 0);
return x_1;
}
}
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_inverseOf___redArg(lean_object* x_1, lean_object* x_2, lean_object* x_3) {
_start:
{
switch (lean_obj_tag(x_3)) {
case 0:
{
uint8_t x_4; 
lean_dec_ref(x_2);
lean_dec_ref(x_1);
x_4 = !lean_is_exclusive(x_3);
if (x_4 == 0)
{
lean_object* x_5; lean_object* x_6; lean_object* x_7; lean_object* x_8; 
x_5 = lean_ctor_get(x_3, 0);
x_6 = lean_ctor_get(x_3, 1);
lean_dec(x_6);
x_7 = lean_alloc_ctor(1, 1, 0);
lean_ctor_set(x_7, 0, x_5);
x_8 = lean_box(0);
lean_ctor_set_tag(x_3, 1);
lean_ctor_set(x_3, 1, x_8);
lean_ctor_set(x_3, 0, x_7);
return x_3;
}
else
{
lean_object* x_9; lean_object* x_10; lean_object* x_11; lean_object* x_12; 
x_9 = lean_ctor_get(x_3, 0);
lean_inc(x_9);
lean_dec(x_3);
x_10 = lean_alloc_ctor(1, 1, 0);
lean_ctor_set(x_10, 0, x_9);
x_11 = lean_box(0);
x_12 = lean_alloc_ctor(1, 2, 0);
lean_ctor_set(x_12, 0, x_10);
lean_ctor_set(x_12, 1, x_11);
return x_12;
}
}
case 1:
{
uint8_t x_13; 
lean_dec_ref(x_2);
lean_dec_ref(x_1);
x_13 = !lean_is_exclusive(x_3);
if (x_13 == 0)
{
lean_object* x_14; lean_object* x_15; 
lean_ctor_set_tag(x_3, 3);
x_14 = lean_box(0);
x_15 = lean_alloc_ctor(1, 2, 0);
lean_ctor_set(x_15, 0, x_3);
lean_ctor_set(x_15, 1, x_14);
return x_15;
}
else
{
lean_object* x_16; lean_object* x_17; lean_object* x_18; lean_object* x_19; 
x_16 = lean_ctor_get(x_3, 0);
lean_inc(x_16);
lean_dec(x_3);
x_17 = lean_alloc_ctor(3, 1, 0);
lean_ctor_set(x_17, 0, x_16);
x_18 = lean_box(0);
x_19 = lean_alloc_ctor(1, 2, 0);
lean_ctor_set(x_19, 0, x_17);
lean_ctor_set(x_19, 1, x_18);
return x_19;
}
}
case 2:
{
lean_object* x_20; lean_object* x_21; uint8_t x_22; 
x_20 = lean_ctor_get(x_3, 0);
lean_inc(x_20);
x_21 = lean_ctor_get(x_3, 2);
lean_inc(x_21);
lean_dec_ref(x_3);
x_22 = !lean_is_exclusive(x_2);
if (x_22 == 0)
{
lean_object* x_23; lean_object* x_24; lean_object* x_25; lean_object* x_26; lean_object* x_31; lean_object* x_32; 
x_23 = lean_ctor_get(x_2, 0);
x_24 = lean_ctor_get(x_2, 1);
lean_dec(x_24);
x_25 = lp_phase4_x2dinvariants_Phase4_inverseOf___redArg___closed__0;
lean_inc(x_20);
x_31 = lean_alloc_closure((void*)(lp_phase4_x2dinvariants_Phase4_inverseOf___redArg___lam__1___boxed), 3, 2);
lean_closure_set(x_31, 0, x_1);
lean_closure_set(x_31, 1, x_20);
x_32 = l_List_find_x3f___redArg(x_31, x_23);
if (lean_obj_tag(x_32) == 0)
{
lean_object* x_33; 
lean_free_object(x_2);
lean_dec(x_20);
x_33 = lean_box(0);
x_26 = x_33;
goto block_30;
}
else
{
lean_object* x_34; uint8_t x_35; 
x_34 = lean_ctor_get(x_32, 0);
lean_inc(x_34);
lean_dec_ref(x_32);
x_35 = !lean_is_exclusive(x_34);
if (x_35 == 0)
{
lean_object* x_36; lean_object* x_37; 
x_36 = lean_ctor_get(x_34, 0);
lean_dec(x_36);
lean_ctor_set(x_34, 0, x_20);
x_37 = lean_box(0);
lean_ctor_set_tag(x_2, 1);
lean_ctor_set(x_2, 1, x_37);
lean_ctor_set(x_2, 0, x_34);
x_26 = x_2;
goto block_30;
}
else
{
lean_object* x_38; lean_object* x_39; lean_object* x_40; 
x_38 = lean_ctor_get(x_34, 1);
lean_inc(x_38);
lean_dec(x_34);
x_39 = lean_alloc_ctor(0, 2, 0);
lean_ctor_set(x_39, 0, x_20);
lean_ctor_set(x_39, 1, x_38);
x_40 = lean_box(0);
lean_ctor_set_tag(x_2, 1);
lean_ctor_set(x_2, 1, x_40);
lean_ctor_set(x_2, 0, x_39);
x_26 = x_2;
goto block_30;
}
}
block_30:
{
lean_object* x_27; lean_object* x_28; lean_object* x_29; 
x_27 = lean_box(0);
x_28 = l_List_mapTR_loop___redArg(x_25, x_21, x_27);
x_29 = l_List_appendTR___redArg(x_26, x_28);
return x_29;
}
}
else
{
lean_object* x_41; lean_object* x_42; lean_object* x_43; lean_object* x_48; lean_object* x_49; 
x_41 = lean_ctor_get(x_2, 0);
lean_inc(x_41);
lean_dec(x_2);
x_42 = lp_phase4_x2dinvariants_Phase4_inverseOf___redArg___closed__0;
lean_inc(x_20);
x_48 = lean_alloc_closure((void*)(lp_phase4_x2dinvariants_Phase4_inverseOf___redArg___lam__1___boxed), 3, 2);
lean_closure_set(x_48, 0, x_1);
lean_closure_set(x_48, 1, x_20);
x_49 = l_List_find_x3f___redArg(x_48, x_41);
if (lean_obj_tag(x_49) == 0)
{
lean_object* x_50; 
lean_dec(x_20);
x_50 = lean_box(0);
x_43 = x_50;
goto block_47;
}
else
{
lean_object* x_51; lean_object* x_52; lean_object* x_53; lean_object* x_54; lean_object* x_55; lean_object* x_56; 
x_51 = lean_ctor_get(x_49, 0);
lean_inc(x_51);
lean_dec_ref(x_49);
x_52 = lean_ctor_get(x_51, 1);
lean_inc(x_52);
if (lean_is_exclusive(x_51)) {
 lean_ctor_release(x_51, 0);
 lean_ctor_release(x_51, 1);
 x_53 = x_51;
} else {
 lean_dec_ref(x_51);
 x_53 = lean_box(0);
}
if (lean_is_scalar(x_53)) {
 x_54 = lean_alloc_ctor(0, 2, 0);
} else {
 x_54 = x_53;
}
lean_ctor_set(x_54, 0, x_20);
lean_ctor_set(x_54, 1, x_52);
x_55 = lean_box(0);
x_56 = lean_alloc_ctor(1, 2, 0);
lean_ctor_set(x_56, 0, x_54);
lean_ctor_set(x_56, 1, x_55);
x_43 = x_56;
goto block_47;
}
block_47:
{
lean_object* x_44; lean_object* x_45; lean_object* x_46; 
x_44 = lean_box(0);
x_45 = l_List_mapTR_loop___redArg(x_42, x_21, x_44);
x_46 = l_List_appendTR___redArg(x_43, x_45);
return x_46;
}
}
}
case 3:
{
uint8_t x_57; 
lean_dec_ref(x_2);
lean_dec_ref(x_1);
x_57 = !lean_is_exclusive(x_3);
if (x_57 == 0)
{
lean_object* x_58; lean_object* x_59; 
lean_ctor_set_tag(x_3, 2);
x_58 = lean_box(0);
x_59 = lean_alloc_ctor(1, 2, 0);
lean_ctor_set(x_59, 0, x_3);
lean_ctor_set(x_59, 1, x_58);
return x_59;
}
else
{
lean_object* x_60; lean_object* x_61; lean_object* x_62; lean_object* x_63; 
x_60 = lean_ctor_get(x_3, 0);
lean_inc(x_60);
lean_dec(x_3);
x_61 = lean_alloc_ctor(2, 1, 0);
lean_ctor_set(x_61, 0, x_60);
x_62 = lean_box(0);
x_63 = lean_alloc_ctor(1, 2, 0);
lean_ctor_set(x_63, 0, x_61);
lean_ctor_set(x_63, 1, x_62);
return x_63;
}
}
default: 
{
lean_object* x_64; uint8_t x_65; 
x_64 = lean_ctor_get(x_3, 0);
lean_inc(x_64);
lean_dec_ref(x_3);
x_65 = !lean_is_exclusive(x_2);
if (x_65 == 0)
{
lean_object* x_66; lean_object* x_67; lean_object* x_68; lean_object* x_69; 
x_66 = lean_ctor_get(x_2, 0);
x_67 = lean_ctor_get(x_2, 1);
lean_dec(x_67);
lean_inc(x_64);
x_68 = lean_alloc_closure((void*)(lp_phase4_x2dinvariants_Phase4_inverseOf___redArg___lam__1___boxed), 3, 2);
lean_closure_set(x_68, 0, x_1);
lean_closure_set(x_68, 1, x_64);
x_69 = l_List_find_x3f___redArg(x_68, x_66);
if (lean_obj_tag(x_69) == 0)
{
lean_object* x_70; 
lean_free_object(x_2);
lean_dec(x_64);
x_70 = lean_box(0);
return x_70;
}
else
{
lean_object* x_71; uint8_t x_72; 
x_71 = lean_ctor_get(x_69, 0);
lean_inc(x_71);
lean_dec_ref(x_69);
x_72 = !lean_is_exclusive(x_71);
if (x_72 == 0)
{
lean_object* x_73; lean_object* x_74; 
x_73 = lean_ctor_get(x_71, 0);
lean_dec(x_73);
lean_ctor_set(x_71, 0, x_64);
x_74 = lean_box(0);
lean_ctor_set_tag(x_2, 1);
lean_ctor_set(x_2, 1, x_74);
lean_ctor_set(x_2, 0, x_71);
return x_2;
}
else
{
lean_object* x_75; lean_object* x_76; lean_object* x_77; 
x_75 = lean_ctor_get(x_71, 1);
lean_inc(x_75);
lean_dec(x_71);
x_76 = lean_alloc_ctor(0, 2, 0);
lean_ctor_set(x_76, 0, x_64);
lean_ctor_set(x_76, 1, x_75);
x_77 = lean_box(0);
lean_ctor_set_tag(x_2, 1);
lean_ctor_set(x_2, 1, x_77);
lean_ctor_set(x_2, 0, x_76);
return x_2;
}
}
}
else
{
lean_object* x_78; lean_object* x_79; lean_object* x_80; 
x_78 = lean_ctor_get(x_2, 0);
lean_inc(x_78);
lean_dec(x_2);
lean_inc(x_64);
x_79 = lean_alloc_closure((void*)(lp_phase4_x2dinvariants_Phase4_inverseOf___redArg___lam__1___boxed), 3, 2);
lean_closure_set(x_79, 0, x_1);
lean_closure_set(x_79, 1, x_64);
x_80 = l_List_find_x3f___redArg(x_79, x_78);
if (lean_obj_tag(x_80) == 0)
{
lean_object* x_81; 
lean_dec(x_64);
x_81 = lean_box(0);
return x_81;
}
else
{
lean_object* x_82; lean_object* x_83; lean_object* x_84; lean_object* x_85; lean_object* x_86; lean_object* x_87; 
x_82 = lean_ctor_get(x_80, 0);
lean_inc(x_82);
lean_dec_ref(x_80);
x_83 = lean_ctor_get(x_82, 1);
lean_inc(x_83);
if (lean_is_exclusive(x_82)) {
 lean_ctor_release(x_82, 0);
 lean_ctor_release(x_82, 1);
 x_84 = x_82;
} else {
 lean_dec_ref(x_82);
 x_84 = lean_box(0);
}
if (lean_is_scalar(x_84)) {
 x_85 = lean_alloc_ctor(0, 2, 0);
} else {
 x_85 = x_84;
}
lean_ctor_set(x_85, 0, x_64);
lean_ctor_set(x_85, 1, x_83);
x_86 = lean_box(0);
x_87 = lean_alloc_ctor(1, 2, 0);
lean_ctor_set(x_87, 0, x_85);
lean_ctor_set(x_87, 1, x_86);
return x_87;
}
}
}
}
}
}
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_inverseOf(lean_object* x_1, lean_object* x_2, lean_object* x_3, lean_object* x_4, lean_object* x_5, lean_object* x_6) {
_start:
{
lean_object* x_7; 
x_7 = lp_phase4_x2dinvariants_Phase4_inverseOf___redArg(x_4, x_5, x_6);
return x_7;
}
}
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyInv___redArg(lean_object* x_1, lean_object* x_2, lean_object* x_3, lean_object* x_4) {
_start:
{
switch (lean_obj_tag(x_4)) {
case 0:
{
uint8_t x_5; 
lean_dec_ref(x_2);
lean_dec_ref(x_1);
x_5 = !lean_is_exclusive(x_4);
if (x_5 == 0)
{
uint8_t x_6; 
x_6 = !lean_is_exclusive(x_3);
if (x_6 == 0)
{
lean_object* x_7; lean_object* x_8; 
x_7 = lean_ctor_get(x_3, 0);
x_8 = lean_alloc_ctor(1, 2, 0);
lean_ctor_set(x_8, 0, x_4);
lean_ctor_set(x_8, 1, x_7);
lean_ctor_set(x_3, 0, x_8);
return x_3;
}
else
{
lean_object* x_9; lean_object* x_10; lean_object* x_11; lean_object* x_12; 
x_9 = lean_ctor_get(x_3, 0);
x_10 = lean_ctor_get(x_3, 1);
lean_inc(x_10);
lean_inc(x_9);
lean_dec(x_3);
x_11 = lean_alloc_ctor(1, 2, 0);
lean_ctor_set(x_11, 0, x_4);
lean_ctor_set(x_11, 1, x_9);
x_12 = lean_alloc_ctor(0, 2, 0);
lean_ctor_set(x_12, 0, x_11);
lean_ctor_set(x_12, 1, x_10);
return x_12;
}
}
else
{
lean_object* x_13; lean_object* x_14; lean_object* x_15; lean_object* x_16; lean_object* x_17; lean_object* x_18; lean_object* x_19; lean_object* x_20; 
x_13 = lean_ctor_get(x_4, 0);
x_14 = lean_ctor_get(x_4, 1);
lean_inc(x_14);
lean_inc(x_13);
lean_dec(x_4);
x_15 = lean_ctor_get(x_3, 0);
lean_inc(x_15);
x_16 = lean_ctor_get(x_3, 1);
lean_inc(x_16);
if (lean_is_exclusive(x_3)) {
 lean_ctor_release(x_3, 0);
 lean_ctor_release(x_3, 1);
 x_17 = x_3;
} else {
 lean_dec_ref(x_3);
 x_17 = lean_box(0);
}
x_18 = lean_alloc_ctor(0, 2, 0);
lean_ctor_set(x_18, 0, x_13);
lean_ctor_set(x_18, 1, x_14);
x_19 = lean_alloc_ctor(1, 2, 0);
lean_ctor_set(x_19, 0, x_18);
lean_ctor_set(x_19, 1, x_15);
if (lean_is_scalar(x_17)) {
 x_20 = lean_alloc_ctor(0, 2, 0);
} else {
 x_20 = x_17;
}
lean_ctor_set(x_20, 0, x_19);
lean_ctor_set(x_20, 1, x_16);
return x_20;
}
}
case 1:
{
lean_object* x_21; uint8_t x_22; 
lean_dec_ref(x_2);
x_21 = lean_ctor_get(x_4, 0);
lean_inc(x_21);
lean_dec_ref(x_4);
x_22 = !lean_is_exclusive(x_3);
if (x_22 == 0)
{
lean_object* x_23; lean_object* x_24; lean_object* x_25; lean_object* x_26; 
x_23 = lean_ctor_get(x_3, 0);
x_24 = lean_alloc_closure((void*)(lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__4___boxed), 3, 2);
lean_closure_set(x_24, 0, x_1);
lean_closure_set(x_24, 1, x_21);
x_25 = lean_box(0);
x_26 = l_List_filterTR_loop___redArg(x_24, x_23, x_25);
lean_ctor_set(x_3, 0, x_26);
return x_3;
}
else
{
lean_object* x_27; lean_object* x_28; lean_object* x_29; lean_object* x_30; lean_object* x_31; lean_object* x_32; 
x_27 = lean_ctor_get(x_3, 0);
x_28 = lean_ctor_get(x_3, 1);
lean_inc(x_28);
lean_inc(x_27);
lean_dec(x_3);
x_29 = lean_alloc_closure((void*)(lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__4___boxed), 3, 2);
lean_closure_set(x_29, 0, x_1);
lean_closure_set(x_29, 1, x_21);
x_30 = lean_box(0);
x_31 = l_List_filterTR_loop___redArg(x_29, x_27, x_30);
x_32 = lean_alloc_ctor(0, 2, 0);
lean_ctor_set(x_32, 0, x_31);
lean_ctor_set(x_32, 1, x_28);
return x_32;
}
}
case 2:
{
lean_object* x_33; uint8_t x_34; 
lean_dec_ref(x_2);
lean_dec_ref(x_1);
x_33 = lean_ctor_get(x_4, 0);
lean_inc(x_33);
lean_dec_ref(x_4);
x_34 = !lean_is_exclusive(x_3);
if (x_34 == 0)
{
lean_object* x_35; lean_object* x_36; 
x_35 = lean_ctor_get(x_3, 1);
x_36 = lean_alloc_ctor(1, 2, 0);
lean_ctor_set(x_36, 0, x_33);
lean_ctor_set(x_36, 1, x_35);
lean_ctor_set(x_3, 1, x_36);
return x_3;
}
else
{
lean_object* x_37; lean_object* x_38; lean_object* x_39; lean_object* x_40; 
x_37 = lean_ctor_get(x_3, 0);
x_38 = lean_ctor_get(x_3, 1);
lean_inc(x_38);
lean_inc(x_37);
lean_dec(x_3);
x_39 = lean_alloc_ctor(1, 2, 0);
lean_ctor_set(x_39, 0, x_33);
lean_ctor_set(x_39, 1, x_38);
x_40 = lean_alloc_ctor(0, 2, 0);
lean_ctor_set(x_40, 0, x_37);
lean_ctor_set(x_40, 1, x_39);
return x_40;
}
}
default: 
{
lean_object* x_41; uint8_t x_42; 
lean_dec_ref(x_1);
x_41 = lean_ctor_get(x_4, 0);
lean_inc(x_41);
lean_dec_ref(x_4);
x_42 = !lean_is_exclusive(x_3);
if (x_42 == 0)
{
lean_object* x_43; lean_object* x_44; lean_object* x_45; lean_object* x_46; 
x_43 = lean_ctor_get(x_3, 1);
x_44 = lean_alloc_closure((void*)(lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__3___boxed), 3, 2);
lean_closure_set(x_44, 0, x_2);
lean_closure_set(x_44, 1, x_41);
x_45 = lean_box(0);
x_46 = l_List_filterTR_loop___redArg(x_44, x_43, x_45);
lean_ctor_set(x_3, 1, x_46);
return x_3;
}
else
{
lean_object* x_47; lean_object* x_48; lean_object* x_49; lean_object* x_50; lean_object* x_51; lean_object* x_52; 
x_47 = lean_ctor_get(x_3, 0);
x_48 = lean_ctor_get(x_3, 1);
lean_inc(x_48);
lean_inc(x_47);
lean_dec(x_3);
x_49 = lean_alloc_closure((void*)(lp_phase4_x2dinvariants_Phase4_applyEvent___redArg___lam__3___boxed), 3, 2);
lean_closure_set(x_49, 0, x_2);
lean_closure_set(x_49, 1, x_41);
x_50 = lean_box(0);
x_51 = l_List_filterTR_loop___redArg(x_49, x_48, x_50);
x_52 = lean_alloc_ctor(0, 2, 0);
lean_ctor_set(x_52, 0, x_47);
lean_ctor_set(x_52, 1, x_51);
return x_52;
}
}
}
}
}
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyInv(lean_object* x_1, lean_object* x_2, lean_object* x_3, lean_object* x_4, lean_object* x_5, lean_object* x_6, lean_object* x_7) {
_start:
{
lean_object* x_8; 
x_8 = lp_phase4_x2dinvariants_Phase4_applyInv___redArg(x_4, x_5, x_6, x_7);
return x_8;
}
}
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyInvs___redArg(lean_object* x_1, lean_object* x_2, lean_object* x_3, lean_object* x_4) {
_start:
{
lean_object* x_5; lean_object* x_6; 
x_5 = lean_alloc_closure((void*)(lp_phase4_x2dinvariants_Phase4_applyInv), 7, 5);
lean_closure_set(x_5, 0, lean_box(0));
lean_closure_set(x_5, 1, lean_box(0));
lean_closure_set(x_5, 2, lean_box(0));
lean_closure_set(x_5, 3, x_1);
lean_closure_set(x_5, 4, x_2);
x_6 = l_List_foldl___redArg(x_5, x_3, x_4);
return x_6;
}
}
LEAN_EXPORT lean_object* lp_phase4_x2dinvariants_Phase4_applyInvs(lean_object* x_1, lean_object* x_2, lean_object* x_3, lean_object* x_4, lean_object* x_5, lean_object* x_6, lean_object* x_7) {
_start:
{
lean_object* x_8; 
x_8 = lp_phase4_x2dinvariants_Phase4_applyInvs___redArg(x_4, x_5, x_6, x_7);
return x_8;
}
}
lean_object* initialize_Init(uint8_t builtin);
lean_object* initialize_phase4_x2dinvariants_Phase4_Domain(uint8_t builtin);
static bool _G_initialized = false;
LEAN_EXPORT lean_object* initialize_phase4_x2dinvariants_Phase4_Properties(uint8_t builtin) {
lean_object * res;
if (_G_initialized) return lean_io_result_mk_ok(lean_box(0));
_G_initialized = true;
res = initialize_Init(builtin);
if (lean_io_result_is_error(res)) return res;
lean_dec_ref(res);
res = initialize_phase4_x2dinvariants_Phase4_Domain(builtin);
if (lean_io_result_is_error(res)) return res;
lean_dec_ref(res);
lp_phase4_x2dinvariants_Phase4_inverseOf___redArg___closed__0 = _init_lp_phase4_x2dinvariants_Phase4_inverseOf___redArg___closed__0();
lean_mark_persistent(lp_phase4_x2dinvariants_Phase4_inverseOf___redArg___closed__0);
return lean_io_result_mk_ok(lean_box(0));
}
#ifdef __cplusplus
}
#endif
