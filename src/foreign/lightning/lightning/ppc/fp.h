/******************************** -*- C -*- ****************************
 *
 *	Run-time assembler & support macros for the Sparc math unit
 *
 ***********************************************************************/


/***********************************************************************
 *
 * Copyright 2000, 2001, 2002 Free Software Foundation, Inc.
 * Written by Paolo Bonzini.
 *
 * This file is part of GNU lightning.
 *
 * GNU lightning is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1, or (at your option)
 * any later version.
 * 
 * GNU lightning is distributed in the hope that it will be useful, but 
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with GNU lightning; see the file COPYING.LESSER; if not, write to the
 * Free Software Foundation, 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA.
 *
 ***********************************************************************/




#ifndef __lightning_asm_fp_h
#define __lightning_asm_fp_h


#define JIT_FPR_NUM	       6
#define JIT_FPR(i)	       (8+(i))

#define JIT_FPFR	       0

/* Make space for 1 or 2 words, store address in REG */
#define jit_data(REG, D1)	        (_FBA	(18, 8, 0, 1),  _jit_L(D1), MFLRr(REG))

#define jit_addr_d(rd,s1,s2)  FADDDrrr((rd),(s1),(s2))
#define jit_subr_d(rd,s1,s2)  FSUBDrrr((rd),(s1),(s2))
#define jit_mulr_d(rd,s1,s2)  FMULDrrr((rd),(s1),(s2))
#define jit_divr_d(rd,s1,s2)  FDIVDrrr((rd),(s1),(s2))

#define jit_addr_f(rd,s1,s2)  FADDSrrr((rd),(s1),(s2))
#define jit_subr_f(rd,s1,s2)  FSUBSrrr((rd),(s1),(s2))
#define jit_mulr_f(rd,s1,s2)  FMULSrrr((rd),(s1),(s2))
#define jit_divr_f(rd,s1,s2)  FDIVSrrr((rd),(s1),(s2))

#define jit_movr_d(rd,rs)     ( (rd) == (rs) ? 0 : FMOVErr((rd),(rs)))
#define jit_movi_d(reg0,d) do {                   \
      double _v = (d);                            \
      _FBA (18, 12, 0, 1); 			  \
      memcpy(_jit.x.uc_pc, &_v, sizeof (double)); \
      _jit.x.uc_pc += sizeof (double);            \
      MFLRr (JIT_AUX);				  \
      jit_ldxi_d((reg0), JIT_AUX, 0);		  \
   } while(0) 


#define jit_movr_f(rd,rs)     ( (rd) == (rs) ? 0 : FMOVErr((rd),(rs)))
#define jit_movi_f(reg0,f) do {                   \
      float _v = (f);                             \
      _FBA (18, 8, 0, 1); 			  \
      memcpy(_jit.x.uc_pc, &_v, sizeof (float));  \
      _jit.x.uc_pc += sizeof (float);             \
      MFLRr (JIT_AUX);				  \
      jit_ldxi_f((reg0), JIT_AUX, 0);		  \
   } while(0) 


#define jit_abs_d(rd,rs)       FABSrr((rd),(rs))
#define jit_negr_d(rd,rs)      FNEGrr((rd),(rs))
#define jit_sqrt_d(rd,rs)      FSQRTDrr((rd),(rs))


#define jit_ldxi_f(reg0, rs, is)    (_siP(16,(is)) ? LFSrri((reg0),(rs),(is)) : (MOVEIri(JIT_AUX,(is)),LFSxrrr((reg0),(rs),JIT_AUX))) 
#define jit_ldxi_d(reg0, rs, is)    (_siP(16,(is)) ? LFDrri((reg0),(rs),(is)) : (MOVEIri(JIT_AUX,(is)),LFDxrrr((reg0),(rs),JIT_AUX)))
#define jit_ldxr_f(reg0, s1, s2)    LFSxrrr((reg0),(s1),(s2))
#define jit_ldxr_d(reg0, s1, s2)    LFDxrrr((reg0),(s1),(s2))
#define jit_ldi_f(reg0, is)          (_siP(16,(is)) ? LFSrri((reg0),0,(is)) : (MOVEIri(JIT_AUX,(is)),LFSrri((reg0),JIT_AUX,0)))
#define jit_ldi_d(reg0, is)          (_siP(16,(is)) ? LFDrri((reg0),0,(is)) : (MOVEIri(JIT_AUX,(is)),LFDrri((reg0),JIT_AUX,0)))
#define jit_ldr_f(reg0, rs)          LFSrri((reg0),(rs),0)
#define jit_ldr_d(reg0, rs)          LFDrri((reg0),(rs),0)
#define jit_stxi_f(id, rd, reg0)     (_siP(16,(id)) ? STFSrri((reg0),(rd),(id)) : (MOVEIri(JIT_AUX,(id)),STFSrri((reg0),(rd),JIT_AUX))) 
#define jit_stxi_d(id, rd, reg0)     (_siP(16,(id)) ? STFDrri((reg0),(rd),(id)) : (MOVEIri(JIT_AUX,(id)),STFDrri((reg0),(rd),JIT_AUX))) 
#define jit_stxr_f(d1, d2, reg0)     STFSxrrr((reg0),(d1),(d2))
#define jit_stxr_d(d1, d2, reg0)     STFDxrrr((reg0),(d1),(d2))
#define jit_sti_f(id, reg0)          (_siP(16,(id)) ? STFSrri((reg0),0,(id)) : (MOVEIri(JIT_AUX,(id)),STFSrri((reg0),JIT_AUX,0)))
#define jit_sti_d(id, reg0)          (_siP(16,(id)) ? STFDrri((reg0),0,(id)) : (MOVEIri(JIT_AUX,(id)),STFDrri((reg0),JIT_AUX,0)))
#define jit_str_f(rd, reg0)          STFSrri((reg0),(rd),0)
#define jit_str_d(rd, reg0)          STFDrri((reg0),(rd),0)

#define jit_fpboolr(d, s1, s2, rcbit) (FCMPOrrr(_cr0,(s1),(s2)),MFCRr((d)), EXTRWIrrii((d), (d), 1, (rcbit)))
#define jit_fpboolr2(d, s1, s2,rcbit) (FCMPOrrr(_cr0,(s1),(s2)),MFCRr((d)), EXTRWIrrii((d), (d), 1, (rcbit)), XORIrri((d), (d), 1))
#define jit_fpboolur(d, s1, s2, rcbit) (FCMPUrrr(_cr0,(s1),(s2)),MFCRr((d)), EXTRWIrrii((d), (d), 1, (rcbit)))
#define jit_fpboolur2(d, s1, s2,rcbit) (FCMPUrrr(_cr0,(s1),(s2)),MFCRr((d)), EXTRWIrrii((d), (d), 1, (rcbit)), XORIrri((d), (d), 1))

#define jit_gtr_d(d, s1, s2)      jit_fpboolr ((d),(s1),(s2),_gt)   
#define jit_ger_d(d, s1, s2)      jit_fpboolr2((d),(s1),(s2),_lt)   
#define jit_ltr_d(d, s1, s2)      jit_fpboolr ((d),(s1),(s2),_lt)         
#define jit_ler_d(d, s1, s2)      jit_fpboolr2((d),(s1),(s2),_gt)         
#define jit_eqr_d(d, s1, s2)      jit_fpboolr ((d),(s1),(s2),_eq)         
#define jit_ner_d(d, s1, s2)      jit_fpboolr2((d),(s1),(s2),_eq)               
#define jit_unler_d(d, s1, s2)    0      
#define jit_unltr_d(d, s1, s2)    0      
#define jit_unger_d(d, s1, s2)    0      
#define jit_ungtr_d(d, s1, s2)    0      
#define jit_ltgtr_d(d, s1, s2)    (jit_fpboolur((d),(s1),(2),_un))
#define jit_uneqr_d(d, s1, s2)    0      
#define jit_ordr_d(d, s1, s2)     0      
#define jit_unordr_d(d, s1, s2)   0      

#define jit_bgtr_d(d, s1, s2)           0
#define jit_bger_d(d, s1, s2)           0
#define jit_bunler_d(d, s1, s2)         0
#define jit_bunltr_d(d, s1, s2)         0
#define jit_bltr_d(d, s1, s2)           0
#define jit_bler_d(d, s1, s2)           0
#define jit_bunger_d(d, s1, s2)         0
#define jit_bungtr_d(d, s1, s2)         0
#define jit_beqr_d(d, s1, s2)           0
#define jit_bner_d(d, s1, s2)           0
#define jit_bltgtr_d(d, s1, s2)         0
#define jit_buneqr_d(d, s1, s2)         0
#define jit_bordr_d(d, s1, s2)          0
#define jit_bunordr_d(d, s1, s2)        0

#define jit_getarg_f(rd, ofs)        jit_movr_f((rd),(ofs))
#define jit_getarg_d(rd, ofs)        jit_movr_d((rd),(ofs))
#define jit_pusharg_d(rs)	     (_jitl.nextarg_putd--,jit_movr_d((_jitl.nextarg_putf+_jitl.nextarg_putd+1), (rs)))
#define jit_pusharg_f(rs)	     (_jitl.nextarg_putf--,jit_movr_f((_jitl.nextarg_putf+_jitl.nextarg_putd+1), (rs)))
#define jit_retval_d(op1)            jit_movr_d(1, (op1))
#define jit_retval_f(op1)            jit_movr_f(1, (op1))


#define jit_floorr_d_i(rd,rs) 0
#define jit_ceilr_d_i(rd,rs) 0
#define jit_roundr_d_i(rd,rs) 0
#define jit_truncr_d_i(rd,rs) 0  

#if 0
/* The rest of the old code */

#define jit_floor(rd, reg0)	0

#define jit_ceil(rd, reg0)	0

#define jit_call_fp(rd, reg0, fn)						\
	jit_fail(#fn " not supported", __FILE__, __LINE__, __FUNCTION__)
/*	pass reg0 as first parameter of rd
	bl	fn
	mr	r3, rd */

#define jit_trunc(rd, reg0)	(jit_data((rd), 0),                     \
                                 FCTIWZrr(JIT_FPFR, (reg0)),		\
                                 STFIWXrrr(JIT_FPFR, 0, (rd)), 		\
                                 LWZrm((rd), 0, (rd)))
          
#define jit_round(rd, reg0)	(jit_data((rd), 0),                     \
				FCTIWrr(JIT_FPFR, (reg0)),		\
				STFIWXrrr(JIT_FPFR, 0, (rd)),		\
				LWZrm((rd), 0, (rd)))
				
#define jit_cmp(le, ge, reg0)	(FCMPOirr(7, JIT_FPFR, (reg0), 0),	   \
				CRORiii(28 + _gt, 28 + _gt, 28 + _eq),	   \
				CRORiii(28 + _lt, 28 + _lt, 28 + _eq),	   \
				MFCRr((ge)), 				   \
				EXTRWIrrii((le), (ge), 1, 28 + _lt),	   \
				EXTRWIrrii((ge), (ge), 1, 28 + _gt))

#endif

#endif /* __lightning_asm_h */
