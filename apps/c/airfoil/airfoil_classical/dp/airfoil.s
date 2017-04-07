# mark_description "Intel(R) C Intel(R) 64 Compiler for applications running on Intel(R) 64, Version 17.0.2.174 Build 20170213";
# mark_description "-S";
	.file "airfoil.cpp"
	.text
..TXTST0:
# -- Begin  main
	.text
# mark_begin;
       .align    16,0x90
	.globl main
# --- main(int, char **)
main:
# parameter 1: %edi
# parameter 2: %rsi
..B1.1:                         # Preds ..B1.0
                                # Execution count [1.00e+00]
	.cfi_startproc
	.cfi_personality 0x3,__gxx_personality_v0
..___tag_value_main.1:
..L2:
                                                          #74.33
        pushq     %rbp                                          #74.33
	.cfi_def_cfa_offset 16
        movq      %rsp, %rbp                                    #74.33
	.cfi_def_cfa 6, 16
	.cfi_offset 6, -16
        andq      $-128, %rsp                                   #74.33
        pushq     %r12                                          #74.33
        pushq     %r13                                          #74.33
        pushq     %r14                                          #74.33
        pushq     %r15                                          #74.33
        pushq     %rbx                                          #74.33
        subq      $4824, %rsp                                   #74.33
	.cfi_escape 0x10, 0x03, 0x0e, 0x38, 0x1c, 0x0d, 0x80, 0xff, 0xff, 0xff, 0x1a, 0x0d, 0xd8, 0xff, 0xff, 0xff, 0x22
	.cfi_escape 0x10, 0x0c, 0x0e, 0x38, 0x1c, 0x0d, 0x80, 0xff, 0xff, 0xff, 0x1a, 0x0d, 0xf8, 0xff, 0xff, 0xff, 0x22
	.cfi_escape 0x10, 0x0d, 0x0e, 0x38, 0x1c, 0x0d, 0x80, 0xff, 0xff, 0xff, 0x1a, 0x0d, 0xf0, 0xff, 0xff, 0xff, 0x22
	.cfi_escape 0x10, 0x0e, 0x0e, 0x38, 0x1c, 0x0d, 0x80, 0xff, 0xff, 0xff, 0x1a, 0x0d, 0xe8, 0xff, 0xff, 0xff, 0x22
	.cfi_escape 0x10, 0x0f, 0x0e, 0x38, 0x1c, 0x0d, 0x80, 0xff, 0xff, 0xff, 0x1a, 0x0d, 0xe0, 0xff, 0xff, 0xff, 0x22
        movq      %rsi, %r12                                    #74.33
        movl      %edi, %ebx                                    #74.33
        xorl      %esi, %esi                                    #74.33
        movl      $3, %edi                                      #74.33
        call      __intel_new_feature_proc_init                 #74.33
                                # LOE r12 ebx
..B1.341:                       # Preds ..B1.1
                                # Execution count [1.00e+00]
        stmxcsr   (%rsp)                                        #74.33
        movl      %ebx, %edi                                    #77.3
        movq      %r12, %rsi                                    #77.3
        orl       $32832, (%rsp)                                #74.33
        movl      $2, %edx                                      #77.3
        ldmxcsr   (%rsp)                                        #74.33
..___tag_value_main.11:
#       op_init(int, char **, int)
        call      op_init                                       #77.3
..___tag_value_main.12:
                                # LOE
..B1.2:                         # Preds ..B1.341
                                # Execution count [1.00e+00]
        movl      $.L_2__STRING.6, %edi                         #86.3
        xorl      %eax, %eax                                    #86.3
..___tag_value_main.13:
#       op_printf(const char *, ...)
        call      op_printf                                     #86.3
..___tag_value_main.14:
                                # LOE
..B1.3:                         # Preds ..B1.2
                                # Execution count [1.00e+00]
        movl      $.L_2__STRING.7, %esi                         #92.18
        lea       304(%rsp), %rdi                               #92.18
        movq      $0x646972675f77656e, %rax                     #88.15
        movq      %rax, (%rdi)                                  #88.15
        movl      $3500078, 8(%rdi)                             #88.15
..___tag_value_main.15:
#       op_decl_set_hdf5(const char *, const char *)
        call      op_decl_set_hdf5                              #92.18
..___tag_value_main.16:
                                # LOE rax
..B1.342:                       # Preds ..B1.3
                                # Execution count [1.00e+00]
        movq      %rax, %r12                                    #92.18
                                # LOE r12
..B1.4:                         # Preds ..B1.342
                                # Execution count [1.00e+00]
        movl      $.L_2__STRING.8, %esi                         #93.18
        lea       304(%rsp), %rdi                               #93.18
..___tag_value_main.17:
#       op_decl_set_hdf5(const char *, const char *)
        call      op_decl_set_hdf5                              #93.18
..___tag_value_main.18:
                                # LOE rax r12
..B1.343:                       # Preds ..B1.4
                                # Execution count [1.00e+00]
        movq      %rax, %rbx                                    #93.18
                                # LOE rbx r12
..B1.5:                         # Preds ..B1.343
                                # Execution count [1.00e+00]
        movl      $.L_2__STRING.9, %esi                         #94.19
        lea       304(%rsp), %rdi                               #94.19
..___tag_value_main.19:
#       op_decl_set_hdf5(const char *, const char *)
        call      op_decl_set_hdf5                              #94.19
..___tag_value_main.20:
                                # LOE rax rbx r12
..B1.344:                       # Preds ..B1.5
                                # Execution count [1.00e+00]
        movq      %rax, 4600(%rsp)                              #94.19[spill]
                                # LOE rbx r12
..B1.6:                         # Preds ..B1.344
                                # Execution count [1.00e+00]
        movl      $.L_2__STRING.10, %esi                        #95.18
        lea       304(%rsp), %rdi                               #95.18
..___tag_value_main.21:
#       op_decl_set_hdf5(const char *, const char *)
        call      op_decl_set_hdf5                              #95.18
..___tag_value_main.22:
                                # LOE rax rbx r12
..B1.345:                       # Preds ..B1.6
                                # Execution count [1.00e+00]
        movq      %rax, %r14                                    #95.18
                                # LOE rbx r12 r14
..B1.7:                         # Preds ..B1.345
                                # Execution count [1.00e+00]
        movq      %rbx, %rdi                                    #97.18
        movq      %r12, %rsi                                    #97.18
        movl      $2, %edx                                      #97.18
        lea       304(%rsp), %rcx                               #97.18
        movl      $.L_2__STRING.11, %r8d                        #97.18
..___tag_value_main.23:
#       op_decl_map_hdf5(op_set, op_set, int, const char *, const char *)
        call      op_decl_map_hdf5                              #97.18
..___tag_value_main.24:
                                # LOE rax rbx r12 r14
..B1.346:                       # Preds ..B1.7
                                # Execution count [1.00e+00]
        movq      %rax, 4592(%rsp)                              #97.18[spill]
                                # LOE rbx r12 r14
..B1.8:                         # Preds ..B1.346
                                # Execution count [1.00e+00]
        movq      %rbx, %rdi                                    #98.19
        movq      %r14, %rsi                                    #98.19
        movl      $2, %edx                                      #98.19
        lea       304(%rsp), %rcx                               #98.19
        movl      $.L_2__STRING.12, %r8d                        #98.19
..___tag_value_main.25:
#       op_decl_map_hdf5(op_set, op_set, int, const char *, const char *)
        call      op_decl_map_hdf5                              #98.19
..___tag_value_main.26:
                                # LOE rax rbx r12 r14
..B1.347:                       # Preds ..B1.8
                                # Execution count [1.00e+00]
        movq      %rax, 4488(%rsp)                              #98.19[spill]
                                # LOE rbx r12 r14
..B1.9:                         # Preds ..B1.347
                                # Execution count [1.00e+00]
        movq      %r12, %rsi                                    #99.19
        movl      $2, %edx                                      #99.19
        movl      $.L_2__STRING.13, %r8d                        #99.19
        lea       304(%rsp), %rcx                               #99.19
        movq      4296(%rcx), %rdi                              #99.19[spill]
..___tag_value_main.27:
#       op_decl_map_hdf5(op_set, op_set, int, const char *, const char *)
        call      op_decl_map_hdf5                              #99.19
..___tag_value_main.28:
                                # LOE rax rbx r12 r14
..B1.348:                       # Preds ..B1.9
                                # Execution count [1.00e+00]
        movq      %rax, 4584(%rsp)                              #99.19[spill]
                                # LOE rbx r12 r14
..B1.10:                        # Preds ..B1.348
                                # Execution count [1.00e+00]
        movq      %r14, %rsi                                    #100.20
        movl      $1, %edx                                      #100.20
        movl      $.L_2__STRING.14, %r8d                        #100.20
        lea       304(%rsp), %rcx                               #100.20
        movq      4296(%rcx), %rdi                              #100.20[spill]
..___tag_value_main.29:
#       op_decl_map_hdf5(op_set, op_set, int, const char *, const char *)
        call      op_decl_map_hdf5                              #100.20
..___tag_value_main.30:
                                # LOE rax rbx r12 r14
..B1.349:                       # Preds ..B1.10
                                # Execution count [1.00e+00]
        movq      %rax, 4576(%rsp)                              #100.20[spill]
                                # LOE rbx r12 r14
..B1.11:                        # Preds ..B1.349
                                # Execution count [1.00e+00]
        movq      %r14, %rdi                                    #101.18
        movq      %r12, %rsi                                    #101.18
        movl      $4, %edx                                      #101.18
        lea       304(%rsp), %rcx                               #101.18
        movl      $.L_2__STRING.15, %r8d                        #101.18
..___tag_value_main.31:
#       op_decl_map_hdf5(op_set, op_set, int, const char *, const char *)
        call      op_decl_map_hdf5                              #101.18
..___tag_value_main.32:
                                # LOE rax rbx r12 r14
..B1.350:                       # Preds ..B1.11
                                # Execution count [1.00e+00]
        movq      %rax, 4568(%rsp)                              #101.18[spill]
                                # LOE rbx r12 r14
..B1.12:                        # Preds ..B1.350
                                # Execution count [1.00e+00]
        movq      %r14, %rdi                                    #103.19
        movq      %r12, %rsi                                    #103.19
        movl      $4, %edx                                      #103.19
        lea       304(%rsp), %rcx                               #103.19
        movl      $.L_2__STRING.16, %r8d                        #103.19
..___tag_value_main.33:
#       op_decl_map_hdf5(op_set, op_set, int, const char *, const char *)
        call      op_decl_map_hdf5                              #103.19
..___tag_value_main.34:
                                # LOE rax rbx r12 r14
..B1.13:                        # Preds ..B1.12
                                # Execution count [1.00e+00]
        testq     %rax, %rax                                    #104.17
        je        ..B1.338      # Prob 3%                       #104.17
                                # LOE rbx r12 r14
..B1.14:                        # Preds ..B1.338 ..B1.13
                                # Execution count [1.00e+00]
        movl      $1, %esi                                      #107.20
        movl      $.L_2__STRING.18, %edx                        #107.20
        movl      $.L_2__STRING.19, %r8d                        #107.20
        lea       304(%rsp), %rcx                               #107.20
        movq      4296(%rcx), %rdi                              #107.20[spill]
..___tag_value_main.35:
#       op_decl_dat_hdf5(op_set, int, const char *, const char *, const char *)
        call      op_decl_dat_hdf5                              #107.20
..___tag_value_main.36:
                                # LOE rax rbx r12 r14
..B1.352:                       # Preds ..B1.14
                                # Execution count [1.00e+00]
        movq      %rax, 4560(%rsp)                              #107.20[spill]
                                # LOE rbx r12 r14
..B1.15:                        # Preds ..B1.352
                                # Execution count [1.00e+00]
        movq      %r12, %rdi                                    #108.16
        movl      $2, %esi                                      #108.16
        movl      $.L_2__STRING.1, %edx                         #108.16
        lea       304(%rsp), %rcx                               #108.16
        movl      $.L_2__STRING.20, %r8d                        #108.16
..___tag_value_main.37:
#       op_decl_dat_hdf5(op_set, int, const char *, const char *, const char *)
        call      op_decl_dat_hdf5                              #108.16
..___tag_value_main.38:
                                # LOE rax rbx r14
..B1.353:                       # Preds ..B1.15
                                # Execution count [1.00e+00]
        movq      %rax, 4480(%rsp)                              #108.16[spill]
                                # LOE rbx r14
..B1.16:                        # Preds ..B1.353
                                # Execution count [1.00e+00]
        movq      %r14, %rdi                                    #109.16
        movl      $4, %esi                                      #109.16
        movl      $.L_2__STRING.1, %edx                         #109.16
        lea       304(%rsp), %rcx                               #109.16
        movl      $.L_2__STRING.21, %r8d                        #109.16
..___tag_value_main.39:
#       op_decl_dat_hdf5(op_set, int, const char *, const char *, const char *)
        call      op_decl_dat_hdf5                              #109.16
..___tag_value_main.40:
                                # LOE rax rbx r14
..B1.354:                       # Preds ..B1.16
                                # Execution count [1.00e+00]
        movq      %rax, %r13                                    #109.16
                                # LOE rbx r13 r14
..B1.17:                        # Preds ..B1.354
                                # Execution count [1.00e+00]
        movq      %r14, %rdi                                    #110.19
        movl      $4, %esi                                      #110.19
        movl      $.L_2__STRING.1, %edx                         #110.19
        lea       304(%rsp), %rcx                               #110.19
        movl      $.L_2__STRING.22, %r8d                        #110.19
..___tag_value_main.41:
#       op_decl_dat_hdf5(op_set, int, const char *, const char *, const char *)
        call      op_decl_dat_hdf5                              #110.19
..___tag_value_main.42:
                                # LOE rax rbx r13 r14
..B1.355:                       # Preds ..B1.17
                                # Execution count [1.00e+00]
        movq      %rax, %r15                                    #110.19
                                # LOE rbx r13 r14 r15
..B1.18:                        # Preds ..B1.355
                                # Execution count [1.00e+00]
        movq      %r14, %rdi                                    #111.18
        movl      $1, %esi                                      #111.18
        movl      $.L_2__STRING.1, %edx                         #111.18
        lea       304(%rsp), %rcx                               #111.18
        movl      $.L_2__STRING.23, %r8d                        #111.18
..___tag_value_main.43:
#       op_decl_dat_hdf5(op_set, int, const char *, const char *, const char *)
        call      op_decl_dat_hdf5                              #111.18
..___tag_value_main.44:
                                # LOE rax rbx r13 r14 r15
..B1.356:                       # Preds ..B1.18
                                # Execution count [1.00e+00]
        movq      %rax, 4456(%rsp)                              #111.18[spill]
                                # LOE rbx r13 r14 r15
..B1.19:                        # Preds ..B1.356
                                # Execution count [1.00e+00]
        movq      %r14, %rdi                                    #112.18
        movl      $4, %esi                                      #112.18
        movl      $.L_2__STRING.1, %edx                         #112.18
        lea       304(%rsp), %rcx                               #112.18
        movl      $.L_2__STRING.24, %r8d                        #112.18
..___tag_value_main.45:
#       op_decl_dat_hdf5(op_set, int, const char *, const char *, const char *)
        call      op_decl_dat_hdf5                              #112.18
..___tag_value_main.46:
                                # LOE rax rbx r13 r14 r15
..B1.357:                       # Preds ..B1.19
                                # Execution count [1.00e+00]
        movq      %rax, 4552(%rsp)                              #112.18[spill]
                                # LOE rbx r13 r14 r15
..B1.20:                        # Preds ..B1.357
                                # Execution count [1.00e+00]
        movq      %r14, %rdi                                    #114.19
        movl      $4, %esi                                      #114.19
        movl      $.L_2__STRING.1, %edx                         #114.19
        lea       304(%rsp), %rcx                               #114.19
        movl      $.L_2__STRING.25, %r8d                        #114.19
..___tag_value_main.47:
#       op_decl_dat_hdf5(op_set, int, const char *, const char *, const char *)
        call      op_decl_dat_hdf5                              #114.19
..___tag_value_main.48:
                                # LOE rax rbx r13 r14 r15
..B1.21:                        # Preds ..B1.20
                                # Execution count [1.00e+00]
        testq     %rax, %rax                                    #115.17
        je        ..B1.337      # Prob 3%                       #115.17
                                # LOE rbx r13 r14 r15
..B1.22:                        # Preds ..B1.337 ..B1.21
                                # Execution count [1.00e+00]
        movl      $.L_2__STRING.27, %edi                        #118.3
        movl      $1, %esi                                      #118.3
        movl      $.L_2__STRING.1, %edx                         #118.3
        movl      $gam, %ecx                                    #118.3
        movl      $.L_2__STRING.28, %r8d                        #118.3
..___tag_value_main.49:
#       op_get_const_hdf5(const char *, int, const char *, char *, const char *)
        call      op_get_const_hdf5                             #118.3
..___tag_value_main.50:
                                # LOE rbx r13 r14 r15
..B1.23:                        # Preds ..B1.22
                                # Execution count [1.00e+00]
        movl      $.L_2__STRING.29, %edi                        #119.3
        movl      $1, %esi                                      #119.3
        movl      $.L_2__STRING.1, %edx                         #119.3
        movl      $gm1, %ecx                                    #119.3
        movl      $.L_2__STRING.28, %r8d                        #119.3
..___tag_value_main.51:
#       op_get_const_hdf5(const char *, int, const char *, char *, const char *)
        call      op_get_const_hdf5                             #119.3
..___tag_value_main.52:
                                # LOE rbx r13 r14 r15
..B1.24:                        # Preds ..B1.23
                                # Execution count [1.00e+00]
        movl      $.L_2__STRING.30, %edi                        #120.3
        movl      $1, %esi                                      #120.3
        movl      $.L_2__STRING.1, %edx                         #120.3
        movl      $cfl, %ecx                                    #120.3
        movl      $.L_2__STRING.28, %r8d                        #120.3
..___tag_value_main.53:
#       op_get_const_hdf5(const char *, int, const char *, char *, const char *)
        call      op_get_const_hdf5                             #120.3
..___tag_value_main.54:
                                # LOE rbx r13 r14 r15
..B1.25:                        # Preds ..B1.24
                                # Execution count [1.00e+00]
        movl      $.L_2__STRING.31, %edi                        #121.3
        movl      $1, %esi                                      #121.3
        movl      $.L_2__STRING.1, %edx                         #121.3
        movl      $eps, %ecx                                    #121.3
        movl      $.L_2__STRING.28, %r8d                        #121.3
..___tag_value_main.55:
#       op_get_const_hdf5(const char *, int, const char *, char *, const char *)
        call      op_get_const_hdf5                             #121.3
..___tag_value_main.56:
                                # LOE rbx r13 r14 r15
..B1.26:                        # Preds ..B1.25
                                # Execution count [1.00e+00]
        movl      $.L_2__STRING.32, %edi                        #122.3
        movl      $1, %esi                                      #122.3
        movl      $.L_2__STRING.1, %edx                         #122.3
        movl      $mach, %ecx                                   #122.3
        movl      $.L_2__STRING.28, %r8d                        #122.3
..___tag_value_main.57:
#       op_get_const_hdf5(const char *, int, const char *, char *, const char *)
        call      op_get_const_hdf5                             #122.3
..___tag_value_main.58:
                                # LOE rbx r13 r14 r15
..B1.27:                        # Preds ..B1.26
                                # Execution count [1.00e+00]
        movl      $.L_2__STRING.33, %edi                        #123.3
        movl      $1, %esi                                      #123.3
        movl      $.L_2__STRING.1, %edx                         #123.3
        movl      $alpha, %ecx                                  #123.3
        movl      $.L_2__STRING.28, %r8d                        #123.3
..___tag_value_main.59:
#       op_get_const_hdf5(const char *, int, const char *, char *, const char *)
        call      op_get_const_hdf5                             #123.3
..___tag_value_main.60:
                                # LOE rbx r13 r14 r15
..B1.28:                        # Preds ..B1.27
                                # Execution count [1.00e+00]
        movl      $.L_2__STRING.34, %edi                        #124.3
        movl      $4, %esi                                      #124.3
        movl      $.L_2__STRING.1, %edx                         #124.3
        movl      $qinf, %ecx                                   #124.3
        movl      $.L_2__STRING.28, %r8d                        #124.3
..___tag_value_main.61:
#       op_get_const_hdf5(const char *, int, const char *, char *, const char *)
        call      op_get_const_hdf5                             #124.3
..___tag_value_main.62:
                                # LOE rbx r13 r14 r15
..B1.29:                        # Preds ..B1.28
                                # Execution count [1.00e+00]
..___tag_value_main.63:
#       op_diagnostic_output()
        call      op_diagnostic_output                          #134.3
..___tag_value_main.64:
                                # LOE rbx r13 r14 r15
..B1.30:                        # Preds ..B1.29
                                # Execution count [1.00e+00]
        movl      $.L_2__STRING.35, %edi                        #139.3
..___tag_value_main.65:
#       op_dump_to_hdf5(const char *)
        call      op_dump_to_hdf5                               #139.3
..___tag_value_main.66:
                                # LOE rbx r13 r14 r15
..B1.31:                        # Preds ..B1.30
                                # Execution count [1.00e+00]
        movl      $.L_2__STRING.27, %edi                        #141.3
        movl      $1, %esi                                      #141.3
        movl      $.L_2__STRING.1, %edx                         #141.3
        movl      $gam, %ecx                                    #141.3
        movl      $.L_2__STRING.35, %r8d                        #141.3
..___tag_value_main.67:
#       op_write_const_hdf5(const char *, int, const char *, char *, const char *)
        call      op_write_const_hdf5                           #141.3
..___tag_value_main.68:
                                # LOE rbx r13 r14 r15
..B1.32:                        # Preds ..B1.31
                                # Execution count [1.00e+00]
        movl      $.L_2__STRING.29, %edi                        #142.3
        movl      $1, %esi                                      #142.3
        movl      $.L_2__STRING.1, %edx                         #142.3
        movl      $gm1, %ecx                                    #142.3
        movl      $.L_2__STRING.35, %r8d                        #142.3
..___tag_value_main.69:
#       op_write_const_hdf5(const char *, int, const char *, char *, const char *)
        call      op_write_const_hdf5                           #142.3
..___tag_value_main.70:
                                # LOE rbx r13 r14 r15
..B1.33:                        # Preds ..B1.32
                                # Execution count [1.00e+00]
        movl      $.L_2__STRING.30, %edi                        #143.3
        movl      $1, %esi                                      #143.3
        movl      $.L_2__STRING.1, %edx                         #143.3
        movl      $cfl, %ecx                                    #143.3
        movl      $.L_2__STRING.35, %r8d                        #143.3
..___tag_value_main.71:
#       op_write_const_hdf5(const char *, int, const char *, char *, const char *)
        call      op_write_const_hdf5                           #143.3
..___tag_value_main.72:
                                # LOE rbx r13 r14 r15
..B1.34:                        # Preds ..B1.33
                                # Execution count [1.00e+00]
        movl      $.L_2__STRING.31, %edi                        #144.3
        movl      $1, %esi                                      #144.3
        movl      $.L_2__STRING.1, %edx                         #144.3
        movl      $eps, %ecx                                    #144.3
        movl      $.L_2__STRING.35, %r8d                        #144.3
..___tag_value_main.73:
#       op_write_const_hdf5(const char *, int, const char *, char *, const char *)
        call      op_write_const_hdf5                           #144.3
..___tag_value_main.74:
                                # LOE rbx r13 r14 r15
..B1.35:                        # Preds ..B1.34
                                # Execution count [1.00e+00]
        movl      $.L_2__STRING.32, %edi                        #145.3
        movl      $1, %esi                                      #145.3
        movl      $.L_2__STRING.1, %edx                         #145.3
        movl      $mach, %ecx                                   #145.3
        movl      $.L_2__STRING.35, %r8d                        #145.3
..___tag_value_main.75:
#       op_write_const_hdf5(const char *, int, const char *, char *, const char *)
        call      op_write_const_hdf5                           #145.3
..___tag_value_main.76:
                                # LOE rbx r13 r14 r15
..B1.36:                        # Preds ..B1.35
                                # Execution count [1.00e+00]
        movl      $.L_2__STRING.33, %edi                        #146.3
        movl      $1, %esi                                      #146.3
        movl      $.L_2__STRING.1, %edx                         #146.3
        movl      $alpha, %ecx                                  #146.3
        movl      $.L_2__STRING.35, %r8d                        #146.3
..___tag_value_main.77:
#       op_write_const_hdf5(const char *, int, const char *, char *, const char *)
        call      op_write_const_hdf5                           #146.3
..___tag_value_main.78:
                                # LOE rbx r13 r14 r15
..B1.37:                        # Preds ..B1.36
                                # Execution count [1.00e+00]
        movl      $.L_2__STRING.34, %edi                        #147.3
        movl      $4, %esi                                      #147.3
        movl      $.L_2__STRING.1, %edx                         #147.3
        movl      $qinf, %ecx                                   #147.3
        movl      $.L_2__STRING.35, %r8d                        #147.3
..___tag_value_main.79:
#       op_write_const_hdf5(const char *, int, const char *, char *, const char *)
        call      op_write_const_hdf5                           #147.3
..___tag_value_main.80:
                                # LOE rbx r13 r14 r15
..B1.38:                        # Preds ..B1.37
                                # Execution count [1.00e+00]
        movl      $.L_2__STRING.36, %edi                        #151.4
        movl      $.L_2__STRING.37, %esi                        #151.4
        movq      %rbx, %rdx                                    #151.4
        movq      4488(%rsp), %rcx                              #151.4[spill]
        movq      4480(%rsp), %r8                               #151.4[spill]
..___tag_value_main.81:
#       op_partition(const char *, const char *, op_set, op_map, op_dat)
        call      op_partition                                  #151.4
..___tag_value_main.82:
                                # LOE rbx r13 r14 r15
..B1.39:                        # Preds ..B1.38
                                # Execution count [1.00e+00]
        movq      %r14, %rdi                                    #153.17
..___tag_value_main.83:
#       op_get_size(op_set)
        call      op_get_size                                   #153.17
..___tag_value_main.84:
                                # LOE rbx r13 r14 r15 eax
..B1.359:                       # Preds ..B1.39
                                # Execution count [1.00e+00]
        movl      %eax, %r12d                                   #153.17
                                # LOE rbx r13 r14 r15 r12d
..B1.40:                        # Preds ..B1.359
                                # Execution count [1.00e+00]
        lea       8(%rsp), %rdi                                 #156.3
        lea       3488(%rsp), %rsi                              #156.3
..___tag_value_main.85:
#       op_timers(double *, double *)
        call      op_timers                                     #156.3
..___tag_value_main.86:
                                # LOE rbx r13 r14 r15 r12d
..B1.41:                        # Preds ..B1.40
                                # Execution count [1.00e+00]
        pxor      %xmm1, %xmm1                                  #217.30
        movl      $1, %esi                                      #161.17
        cvtsi2sd  %r12d, %xmm1                                  #217.30
        movsd     %xmm1, 4408(%rsp)                             #167.60[spill]
        movq      %rbx, 4616(%rsp)                              #167.60[spill]
        xorl      %eax, %eax                                    #205.13
        movl      %esi, %ebx                                    #167.60
                                # LOE r13 r14 r15 ebx r12d
..B1.42:                        # Preds ..B1.317 ..B1.41
                                # Execution count [5.56e+00]
        addq      $-16, %rsp                                    #166.57
        movq      %r13, %rsi                                    #166.57
        movl      $-1, %edx                                     #166.57
        lea       32(%rsp), %rdi                                #166.57
        xorl      %ecx, %ecx                                    #166.57
        movl      $4, %r8d                                      #166.57
        movl      $.L_2__STRING.1, %r9d                         #166.57
        movl      $0, (%rsp)                                    #166.57
..___tag_value_main.87:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #166.57
..___tag_value_main.88:
                                # LOE r13 r14 r15 ebx r12d
..B1.43:                        # Preds ..B1.42
                                # Execution count [5.56e+00]
        movq      %r15, %rsi                                    #167.60
        movl      $-1, %edx                                     #167.60
        lea       128(%rsp), %rdi                               #167.60
        xorl      %ecx, %ecx                                    #167.60
        movl      $4, %r8d                                      #167.60
        movl      $.L_2__STRING.1, %r9d                         #167.60
        movl      $1, (%rsp)                                    #167.60
..___tag_value_main.89:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #167.60
..___tag_value_main.90:
                                # LOE r13 r14 r15 ebx r12d
..B1.361:                       # Preds ..B1.43
                                # Execution count [5.56e+00]
        addq      $16, %rsp                                     #167.60
                                # LOE r13 r14 r15 ebx r12d
..B1.44:                        # Preds ..B1.361
                                # Execution count [5.56e+00]
        movups    112(%rsp), %xmm0                              #165.5
        movups    128(%rsp), %xmm1                              #165.5
        pxor      %xmm6, %xmm6                                  #165.5
        movups    144(%rsp), %xmm2                              #165.5
        movups    160(%rsp), %xmm3                              #165.5
        movups    176(%rsp), %xmm4                              #165.5
        movups    192(%rsp), %xmm5                              #165.5
        movups    %xmm0, 208(%rsp)                              #165.5
        movups    %xmm1, 224(%rsp)                              #165.5
        movups    %xmm2, 240(%rsp)                              #165.5
        movups    %xmm3, 256(%rsp)                              #165.5
        movups    %xmm4, 272(%rsp)                              #165.5
        movups    %xmm5, 288(%rsp)                              #165.5
        movups    %xmm6, 4464(%rsp)                             #165.5
                                # LOE r13 r14 r15 ebx r12d
..B1.45:                        # Preds ..B1.44
                                # Execution count [5.56e+00]
        movups    16(%rsp), %xmm0                               #165.5
        movups    32(%rsp), %xmm1                               #165.5
        movups    48(%rsp), %xmm2                               #165.5
        movups    64(%rsp), %xmm3                               #165.5
        movups    80(%rsp), %xmm4                               #165.5
        movups    96(%rsp), %xmm5                               #165.5
        movups    224(%rsp), %xmm7                              #165.5
        movups    208(%rsp), %xmm6                              #165.5
        movups    240(%rsp), %xmm8                              #165.5
        movups    256(%rsp), %xmm9                              #165.5
        movups    272(%rsp), %xmm10                             #165.5
        movups    288(%rsp), %xmm11                             #165.5
        movups    %xmm0, 3696(%rsp)                             #165.5
        movups    %xmm1, 3712(%rsp)                             #165.5
        movups    %xmm2, 3728(%rsp)                             #165.5
        movups    %xmm3, 3744(%rsp)                             #165.5
        movups    %xmm4, 3760(%rsp)                             #165.5
        movups    %xmm5, 3776(%rsp)                             #165.5
        movups    %xmm6, 3792(%rsp)                             #165.5
        movups    %xmm7, 3808(%rsp)                             #165.5
        movups    %xmm8, 3824(%rsp)                             #165.5
        movups    %xmm9, 3840(%rsp)                             #165.5
        movups    %xmm10, 3856(%rsp)                            #165.5
        movups    %xmm11, 3872(%rsp)                            #165.5
        cmpl      $-1, 44(%rsp)                                 #165.5
        jge       ..B1.48       # Prob 78%                      #165.5
                                # LOE r13 r14 r15 ebx r12d
..B1.46:                        # Preds ..B1.45
                                # Execution count [1.22e+00]
        movl      3724(%rsp), %edi                              #165.5
        negl      %edi                                          #165.5
        movslq    %edi, %rdi                                    #165.5
        shlq      $3, %rdi                                      #165.5
..___tag_value_main.91:
#       op_malloc(size_t)
        call      op_malloc                                     #165.5
..___tag_value_main.92:
                                # LOE rax r13 r14 r15 ebx r12d
..B1.47:                        # Preds ..B1.46
                                # Execution count [1.22e+00]
        movq      %rax, 4464(%rsp)                              #165.5
                                # LOE r13 r14 r15 ebx r12d
..B1.48:                        # Preds ..B1.45 ..B1.47
                                # Execution count [5.56e+00]
        cmpl      $-1, 236(%rsp)                                #165.5
        jge       ..B1.51       # Prob 78%                      #165.5
                                # LOE r13 r14 r15 ebx r12d
..B1.49:                        # Preds ..B1.48
                                # Execution count [1.22e+00]
        movl      3820(%rsp), %edi                              #165.5
        negl      %edi                                          #165.5
        movslq    %edi, %rdi                                    #165.5
        shlq      $3, %rdi                                      #165.5
..___tag_value_main.93:
#       op_malloc(size_t)
        call      op_malloc                                     #165.5
..___tag_value_main.94:
                                # LOE rax r13 r14 r15 ebx r12d
..B1.50:                        # Preds ..B1.49
                                # Execution count [1.22e+00]
        movq      %rax, 4472(%rsp)                              #165.5
                                # LOE r13 r14 r15 ebx r12d
..B1.51:                        # Preds ..B1.48 ..B1.50
                                # Execution count [5.56e+00]
        cmpl      $0, 3780(%rsp)                                #165.5
        jne       ..B1.55       # Prob 50%                      #165.5
                                # LOE r13 r14 r15 ebx r12d
..B1.52:                        # Preds ..B1.51
                                # Execution count [2.78e+00]
        movl      3728(%rsp), %edi                              #165.5
        cmpl      blank_args_size(%rip), %edi                   #165.5
        jle       ..B1.55       # Prob 68%                      #165.5
                                # LOE r13 r14 r15 ebx edi r12d
..B1.53:                        # Preds ..B1.52
                                # Execution count [8.75e-01]
        movl      %edi, blank_args_size(%rip)                   #165.5
        movslq    %edi, %rdi                                    #165.5
..___tag_value_main.95:
#       op_malloc(size_t)
        call      op_malloc                                     #165.5
..___tag_value_main.96:
                                # LOE rax r13 r14 r15 ebx r12d
..B1.54:                        # Preds ..B1.53
                                # Execution count [8.75e-01]
        movq      %rax, blank_args(%rip)                        #165.5
                                # LOE r13 r14 r15 ebx r12d
..B1.55:                        # Preds ..B1.51 ..B1.52 ..B1.54
                                # Execution count [5.56e+00]
        cmpl      $0, 3876(%rsp)                                #165.5
        jne       ..B1.59       # Prob 50%                      #165.5
                                # LOE r13 r14 r15 ebx r12d
..B1.56:                        # Preds ..B1.55
                                # Execution count [2.78e+00]
        movl      3824(%rsp), %edi                              #165.5
        cmpl      blank_args_size(%rip), %edi                   #165.5
        jle       ..B1.59       # Prob 68%                      #165.5
                                # LOE r13 r14 r15 ebx edi r12d
..B1.57:                        # Preds ..B1.56
                                # Execution count [8.75e-01]
        movl      %edi, blank_args_size(%rip)                   #165.5
        movslq    %edi, %rdi                                    #165.5
..___tag_value_main.97:
#       op_malloc(size_t)
        call      op_malloc                                     #165.5
..___tag_value_main.98:
                                # LOE rax r13 r14 r15 ebx r12d
..B1.58:                        # Preds ..B1.57
                                # Execution count [8.75e-01]
        movq      %rax, blank_args(%rip)                        #165.5
                                # LOE r13 r14 r15 ebx r12d
..B1.59:                        # Preds ..B1.55 ..B1.56 ..B1.58
                                # Execution count [5.56e+00]
        movl      $0, 316(%rsp)                                 #165.5
        cmpl      $0, OP_diags(%rip)                            #165.5
        jle       ..B1.67       # Prob 40%                      #165.5
                                # LOE r13 r14 r15 ebx r12d
..B1.60:                        # Preds ..B1.59
                                # Execution count [2.98e+00]
        xorl      %esi, %esi                                    #165.5
        xorl      %eax, %eax                                    #165.5
        movq      %r15, 4608(%rsp)                              #165.5[spill]
        movq      %rax, %r15                                    #165.5
        movq      %r13, 4496(%rsp)                              #165.5[spill]
        movl      %esi, %r13d                                   #165.5
                                # LOE r14 r15 ebx r12d r13d
..B1.61:                        # Preds ..B1.62 ..B1.60
                                # Execution count [1.66e+01]
        addq      $-96, %rsp                                    #165.5
        movq      %r14, %rdi                                    #165.5
        movq      %rsp, %r8                                     #165.5
        movl      %r13d, %esi                                   #165.5
        movups    3792(%rsp,%r15), %xmm0                        #165.5
        lea       412(%rsp), %rdx                               #165.5
        movups    3808(%rsp,%r15), %xmm1                        #165.5
        movl      $.L_2__STRING.38, %ecx                        #165.5
        movups    3824(%rsp,%r15), %xmm2                        #165.5
        movups    3840(%rsp,%r15), %xmm3                        #165.5
        movups    3856(%rsp,%r15), %xmm4                        #165.5
        movups    3872(%rsp,%r15), %xmm5                        #165.5
        movups    %xmm0, (%r8)                                  #165.5
        movups    %xmm1, 16(%r8)                                #165.5
        movups    %xmm2, 32(%r8)                                #165.5
        movups    %xmm3, 48(%r8)                                #165.5
        movups    %xmm4, 64(%r8)                                #165.5
        movups    %xmm5, 80(%r8)                                #165.5
..___tag_value_main.99:
#       op_arg_check(op_set, int, op_arg, int *, const char *)
        call      op_arg_check                                  #165.5
..___tag_value_main.100:
                                # LOE r14 r15 ebx r12d r13d
..B1.366:                       # Preds ..B1.61
                                # Execution count [1.66e+01]
        addq      $96, %rsp                                     #165.5
                                # LOE r14 r15 ebx r12d r13d
..B1.62:                        # Preds ..B1.366
                                # Execution count [1.66e+01]
        incl      %r13d                                         #165.5
        addq      $96, %r15                                     #165.5
        cmpl      $2, %r13d                                     #165.5
        jl        ..B1.61       # Prob 82%                      #165.5
                                # LOE r14 r15 ebx r12d r13d
..B1.63:                        # Preds ..B1.62
                                # Execution count [2.98e+00]
        movq      4608(%rsp), %r15                              #[spill]
        movq      4496(%rsp), %r13                              #[spill]
        cmpl      $2, OP_diags(%rip)                            #165.5
        jle       ..B1.67       # Prob 50%                      #165.5
                                # LOE r13 r14 r15 ebx r12d
..B1.64:                        # Preds ..B1.63
                                # Execution count [2.78e+00]
        cmpl      $0, 316(%rsp)                                 #165.5
        jne       ..B1.66       # Prob 50%                      #165.5
                                # LOE r13 r14 r15 ebx r12d
..B1.65:                        # Preds ..B1.64
                                # Execution count [1.39e+00]
        movl      $.L_2__STRING.3, %edi                         #165.5
        movl      $.L_2__STRING.38, %esi                        #165.5
        xorl      %eax, %eax                                    #165.5
..___tag_value_main.101:
#       printf(const char *, ...)
        call      printf                                        #165.5
..___tag_value_main.102:
        jmp       ..B1.67       # Prob 100%                     #165.5
                                # LOE r13 r14 r15 ebx r12d
..B1.66:                        # Preds ..B1.64
                                # Execution count [1.39e+00]
        movl      $.L_2__STRING.4, %edi                         #165.5
        movl      $.L_2__STRING.38, %esi                        #165.5
        xorl      %eax, %eax                                    #165.5
..___tag_value_main.103:
#       printf(const char *, ...)
        call      printf                                        #165.5
..___tag_value_main.104:
                                # LOE r13 r14 r15 ebx r12d
..B1.67:                        # Preds ..B1.65 ..B1.66 ..B1.59 ..B1.63
                                # Execution count [5.56e+00]
        lea       4368(%rsp), %rdi                              #165.5
        lea       4376(%rsp), %rsi                              #165.5
..___tag_value_main.105:
#       op_timers_core(double *, double *)
        call      op_timers_core                                #165.5
..___tag_value_main.106:
                                # LOE r13 r14 r15 ebx r12d
..B1.68:                        # Preds ..B1.67
                                # Execution count [5.56e+00]
        movq      %r14, %rdi                                    #165.5
        movl      $2, %esi                                      #165.5
        lea       3696(%rsp), %rdx                              #165.5
..___tag_value_main.107:
#       op_mpi_halo_exchanges(op_set, int, op_arg *)
        call      op_mpi_halo_exchanges                         #165.5
..___tag_value_main.108:
                                # LOE r13 r14 r15 eax ebx r12d
..B1.367:                       # Preds ..B1.68
                                # Execution count [5.56e+00]
        movl      %eax, %r8d                                    #165.5
                                # LOE r13 r14 r15 ebx r8d r12d
..B1.69:                        # Preds ..B1.367
                                # Execution count [5.56e+00]
        xorl      %edx, %edx                                    #165.5
        movl      16(%r14), %ecx                                #165.5
        xorl      %eax, %eax                                    #165.5
        testl     %r8d, %r8d                                    #165.5
        jle       ..B1.335      # Prob 10%                      #165.5
                                # LOE r13 r14 r15 eax edx ecx ebx r8d r12d
..B1.70:                        # Preds ..B1.69
                                # Execution count [5.00e+00]
        movl      %ecx, 4656(%rsp)                              #[spill]
        movl      %r8d, 4664(%rsp)                              #[spill]
        movl      %ebx, 3496(%rsp)                              #[spill]
        movl      %edx, %ebx                                    #
        movl      %r12d, 4400(%rsp)                             #[spill]
        movl      %eax, %r12d                                   #
        movq      %r15, 4608(%rsp)                              #[spill]
        movq      %r13, 4496(%rsp)                              #[spill]
                                # LOE r14 ebx r12d
..B1.71:                        # Preds ..B1.111 ..B1.70
                                # Execution count [2.78e+01]
        cmpl      4656(%rsp), %r12d                             #165.5[spill]
        jne       ..B1.74       # Prob 78%                      #165.5
                                # LOE r14 ebx r12d
..B1.72:                        # Preds ..B1.71
                                # Execution count [6.11e+00]
        movl      $2, %edi                                      #165.5
        lea       3696(%rsp), %rsi                              #165.5
..___tag_value_main.109:
#       op_mpi_wait_all(int, op_arg *)
        call      op_mpi_wait_all                               #165.5
..___tag_value_main.110:
                                # LOE r14 ebx r12d
..B1.73:                        # Preds ..B1.72
                                # Execution count [6.11e+00]
        movl      16(%r14), %ecx                                #165.5
        movl      %ecx, 4656(%rsp)                              #165.5[spill]
                                # LOE r14 ebx r12d
..B1.74:                        # Preds ..B1.73 ..B1.71
                                # Execution count [2.78e+01]
        movl      $1, %ecx                                      #165.5
        cmpl      4(%r14), %r12d                                #165.5
        cmove     %ecx, %ebx                                    #165.5
        cmpl      $-1, 3724(%rsp)                               #165.5
        jge       ..B1.82       # Prob 50%                      #165.5
                                # LOE r14 ebx r12d
..B1.75:                        # Preds ..B1.74
                                # Execution count [1.39e+01]
        movl      3724(%rsp), %eax                              #165.5
        movl      %eax, %r8d                                    #165.5
        negl      %r8d                                          #165.5
        movq      3712(%rsp), %r9                               #165.5
        movq      3736(%rsp), %rsi                              #165.5
        movl      3728(%rsp), %ecx                              #165.5
        movq      4464(%rsp), %rdi                              #165.5
        testl     %r8d, %r8d                                    #165.5
        jle       ..B1.92       # Prob 50%                      #165.5
                                # LOE rsi rdi r9 r14 eax ecx ebx r8d r12d
..B1.76:                        # Preds ..B1.75
                                # Execution count [1.39e+01]
        movl      %r8d, %r13d                                   #165.5
        movl      $1, %r10d                                     #165.5
        shrl      $31, %r13d                                    #165.5
        xorl      %r11d, %r11d                                  #165.5
        subl      %eax, %r13d                                   #165.5
        sarl      $1, %r13d                                     #165.5
        movl      24(%r9), %r15d                                #165.5
        testl     %r13d, %r13d                                  #165.5
        jbe       ..B1.80       # Prob 10%                      #165.5
                                # LOE rsi rdi r9 r14 ecx ebx r8d r10d r11d r12d r13d r15d
..B1.77:                        # Preds ..B1.76
                                # Execution count [1.25e+01]
        movl      %r12d, %r10d                                  #165.5
        imull     %r15d, %r10d                                  #165.5
        movq      %r14, 4696(%rsp)                              #165.5[spill]
        .align    16,0x90
                                # LOE rsi rdi r9 ecx ebx r8d r10d r11d r12d r13d r15d
..B1.78:                        # Preds ..B1.78 ..B1.77
                                # Execution count [3.47e+01]
        movq      32(%r9), %r14                                 #165.5
        lea       (%r10,%r11,2), %eax                           #165.5
        movslq    %eax, %rax                                    #165.5
        lea       (%r11,%r11), %edx                             #165.5
        movslq    %edx, %rdx                                    #165.5
        incl      %r11d                                         #165.5
        movl      (%r14,%rax,4), %r14d                          #165.5
        imull     %ecx, %r14d                                   #165.5
        movslq    %r14d, %r14                                   #165.5
        addq      %rsi, %r14                                    #165.5
        movq      %r14, (%rdi,%rdx,8)                           #165.5
        movq      32(%r9), %r14                                 #165.5
        movl      4(%r14,%rax,4), %r14d                         #165.5
        imull     %ecx, %r14d                                   #165.5
        movslq    %r14d, %r14                                   #165.5
        addq      %rsi, %r14                                    #165.5
        movq      %r14, 8(%rdi,%rdx,8)                          #165.5
        cmpl      %r13d, %r11d                                  #165.5
        jb        ..B1.78       # Prob 64%                      #165.5
                                # LOE rsi rdi r9 ecx ebx r8d r10d r11d r12d r13d r15d
..B1.79:                        # Preds ..B1.78
                                # Execution count [1.25e+01]
        movq      4696(%rsp), %r14                              #[spill]
        lea       1(%r11,%r11), %r10d                           #165.5
                                # LOE rsi rdi r9 r14 ecx ebx r8d r10d r12d r15d
..B1.80:                        # Preds ..B1.79 ..B1.76
                                # Execution count [1.39e+01]
        lea       -1(%r10), %r11d                               #165.5
        cmpl      %r8d, %r11d                                   #165.5
        jae       ..B1.92       # Prob 10%                      #165.5
                                # LOE rsi rdi r9 r14 ecx ebx r10d r12d r15d
..B1.81:                        # Preds ..B1.80
                                # Execution count [1.25e+01]
        imull     %r12d, %r15d                                  #165.5
        movslq    %r10d, %r10                                   #165.5
        movslq    %r15d, %r15                                   #165.5
        addq      %r10, %r15                                    #165.5
        movq      32(%r9), %r9                                  #165.5
        movl      -4(%r9,%r15,4), %r8d                          #165.5
        imull     %r8d, %ecx                                    #165.5
        movslq    %ecx, %rcx                                    #165.5
        addq      %rcx, %rsi                                    #165.5
        movq      %rsi, -8(%rdi,%r10,8)                         #165.5
        jmp       ..B1.92       # Prob 100%                     #165.5
                                # LOE rdi r14 ebx r12d
..B1.82:                        # Preds ..B1.74
                                # Execution count [1.39e+01]
        movups    3696(%rsp), %xmm0                             #165.5
        movups    3712(%rsp), %xmm1                             #165.5
        movups    3728(%rsp), %xmm2                             #165.5
        movups    3744(%rsp), %xmm3                             #165.5
        movups    3760(%rsp), %xmm4                             #165.5
        movups    3776(%rsp), %xmm5                             #165.5
        movq      3736(%rsp), %rdi                              #165.5
        movups    %xmm0, 3504(%rsp)                             #165.5
        movups    %xmm1, 3520(%rsp)                             #165.5
        movups    %xmm2, 3536(%rsp)                             #165.5
        movups    %xmm3, 3552(%rsp)                             #165.5
        movups    %xmm4, 3568(%rsp)                             #165.5
        movups    %xmm5, 3584(%rsp)                             #165.5
        cmpl      $0, 3780(%rsp)                                #165.5
        jne       ..B1.88       # Prob 50%                      #165.5
                                # LOE rdi r14 ebx r12d
..B1.83:                        # Preds ..B1.82
                                # Execution count [6.94e+00]
        testl     %ebx, %ebx                                    #165.5
        je        ..B1.86       # Prob 50%                      #165.5
                                # LOE rdi r14 ebx r12d
..B1.84:                        # Preds ..B1.83
                                # Execution count [3.47e+00]
        cmpl      $0, 3776(%rsp)                                #165.5
        je        ..B1.86       # Prob 50%                      #165.5
                                # LOE rdi r14 ebx r12d
..B1.85:                        # Preds ..B1.84
                                # Execution count [1.74e+00]
        movq      blank_args(%rip), %rdi                        #165.5
        movq      %rdi, 4464(%rsp)                              #165.5
        jmp       ..B1.92       # Prob 100%                     #165.5
                                # LOE rdi r14 ebx r12d
..B1.86:                        # Preds ..B1.83 ..B1.84
                                # Execution count [1.74e+00]
        movq      %rdi, 4464(%rsp)                              #165.5
        jmp       ..B1.92       # Prob 100%                     #165.5
                                # LOE rdi r14 ebx r12d
..B1.88:                        # Preds ..B1.82
                                # Execution count [6.94e+00]
        movq      3520(%rsp), %r8                               #165.5
        testq     %r8, %r8                                      #165.5
        je        ..B1.90       # Prob 12%                      #165.5
                                # LOE rdi r8 r14 ebx r12d
..B1.89:                        # Preds ..B1.88
                                # Execution count [6.11e+00]
        cmpl      $0, 3596(%rsp)                                #165.5
        jne       ..B1.91       # Prob 50%                      #165.5
                                # LOE rdi r8 r14 ebx r12d
..B1.90:                        # Preds ..B1.88 ..B1.89
                                # Execution count [3.89e+00]
        movslq    %r12d, %r12                                   #165.5
        movslq    3536(%rsp), %rcx                              #165.5
        imulq     %r12, %rcx                                    #165.5
        addq      %rcx, %rdi                                    #165.5
        movq      %rdi, 4464(%rsp)                              #165.5
        jmp       ..B1.92       # Prob 100%                     #165.5
                                # LOE rdi r14 ebx r12d
..B1.91:                        # Preds ..B1.89
                                # Execution count [3.06e+00]
        movl      %r12d, %ecx                                   #165.5
        imull     24(%r8), %ecx                                 #165.5
        addl      3532(%rsp), %ecx                              #165.5
        movslq    %ecx, %rcx                                    #165.5
        movq      32(%r8), %r8                                  #165.5
        movl      3536(%rsp), %r9d                              #165.5
        imull     (%r8,%rcx,4), %r9d                            #165.5
        movslq    %r9d, %r9                                     #165.5
        addq      %r9, %rdi                                     #165.5
        movq      %rdi, 4464(%rsp)                              #165.5
                                # LOE rdi r14 ebx r12d
..B1.92:                        # Preds ..B1.81 ..B1.80 ..B1.75 ..B1.85 ..B1.86
                                #       ..B1.90 ..B1.91
                                # Execution count [2.78e+01]
        cmpl      $-1, 3820(%rsp)                               #165.5
        jge       ..B1.100      # Prob 50%                      #165.5
                                # LOE rdi r14 ebx r12d
..B1.93:                        # Preds ..B1.92
                                # Execution count [1.39e+01]
        movl      3820(%rsp), %r15d                             #165.5
        movl      %r15d, %r11d                                  #165.5
        negl      %r11d                                         #165.5
        movq      3808(%rsp), %rcx                              #165.5
        movq      3832(%rsp), %r8                               #165.5
        movl      3824(%rsp), %r10d                             #165.5
        movq      4472(%rsp), %rsi                              #165.5
        testl     %r11d, %r11d                                  #165.5
        jle       ..B1.110      # Prob 50%                      #165.5
                                # LOE rcx rsi rdi r8 r14 ebx r10d r11d r12d r15d
..B1.94:                        # Preds ..B1.93
                                # Execution count [1.39e+01]
        movl      %r11d, %edx                                   #165.5
        movl      $1, %eax                                      #165.5
        shrl      $31, %edx                                     #165.5
        xorl      %r9d, %r9d                                    #165.5
        subl      %r15d, %edx                                   #165.5
        sarl      $1, %edx                                      #165.5
        movl      24(%rcx), %r13d                               #165.5
        testl     %edx, %edx                                    #165.5
        jbe       ..B1.98       # Prob 10%                      #165.5
                                # LOE rcx rsi rdi r8 r14 eax edx ebx r9d r10d r11d r12d r13d
..B1.95:                        # Preds ..B1.94
                                # Execution count [1.25e+01]
        movl      %r12d, %eax                                   #165.5
        imull     %r13d, %eax                                   #165.5
        movl      %ebx, 4624(%rsp)                              #165.5[spill]
        movq      %r14, 4696(%rsp)                              #165.5[spill]
        .align    16,0x90
                                # LOE rcx rsi rdi r8 eax edx r9d r10d r11d r12d r13d
..B1.96:                        # Preds ..B1.96 ..B1.95
                                # Execution count [3.47e+01]
        movq      32(%rcx), %r15                                #165.5
        lea       (%rax,%r9,2), %ebx                            #165.5
        movslq    %ebx, %rbx                                    #165.5
        lea       (%r9,%r9), %r14d                              #165.5
        movslq    %r14d, %r14                                   #165.5
        incl      %r9d                                          #165.5
        movl      (%r15,%rbx,4), %r15d                          #165.5
        imull     %r10d, %r15d                                  #165.5
        movslq    %r15d, %r15                                   #165.5
        addq      %r8, %r15                                     #165.5
        movq      %r15, (%rsi,%r14,8)                           #165.5
        movq      32(%rcx), %r15                                #165.5
        movl      4(%r15,%rbx,4), %ebx                          #165.5
        imull     %r10d, %ebx                                   #165.5
        movslq    %ebx, %rbx                                    #165.5
        addq      %r8, %rbx                                     #165.5
        movq      %rbx, 8(%rsi,%r14,8)                          #165.5
        cmpl      %edx, %r9d                                    #165.5
        jb        ..B1.96       # Prob 64%                      #165.5
                                # LOE rcx rsi rdi r8 eax edx r9d r10d r11d r12d r13d
..B1.97:                        # Preds ..B1.96
                                # Execution count [1.25e+01]
        movl      4624(%rsp), %ebx                              #[spill]
        lea       1(%r9,%r9), %eax                              #165.5
        movq      4696(%rsp), %r14                              #[spill]
                                # LOE rcx rsi rdi r8 r14 eax ebx r10d r11d r12d r13d
..B1.98:                        # Preds ..B1.97 ..B1.94
                                # Execution count [1.39e+01]
        lea       -1(%rax), %r9d                                #165.5
        cmpl      %r11d, %r9d                                   #165.5
        jae       ..B1.110      # Prob 10%                      #165.5
                                # LOE rcx rsi rdi r8 r14 eax ebx r10d r12d r13d
..B1.99:                        # Preds ..B1.98
                                # Execution count [1.25e+01]
        imull     %r12d, %r13d                                  #165.5
        movslq    %eax, %rax                                    #165.5
        movslq    %r13d, %r13                                   #165.5
        addq      %rax, %r13                                    #165.5
        movq      32(%rcx), %rcx                                #165.5
        movl      -4(%rcx,%r13,4), %r9d                         #165.5
        imull     %r9d, %r10d                                   #165.5
        movslq    %r10d, %r10                                   #165.5
        addq      %r10, %r8                                     #165.5
        movq      %r8, -8(%rsi,%rax,8)                          #165.5
        jmp       ..B1.110      # Prob 100%                     #165.5
                                # LOE rsi rdi r14 ebx r12d
..B1.100:                       # Preds ..B1.92
                                # Execution count [1.39e+01]
        movups    3792(%rsp), %xmm0                             #165.5
        movups    3808(%rsp), %xmm1                             #165.5
        movups    3824(%rsp), %xmm2                             #165.5
        movups    3840(%rsp), %xmm3                             #165.5
        movups    3856(%rsp), %xmm4                             #165.5
        movups    3872(%rsp), %xmm5                             #165.5
        movq      3832(%rsp), %rsi                              #165.5
        movups    %xmm0, 3600(%rsp)                             #165.5
        movups    %xmm1, 3616(%rsp)                             #165.5
        movups    %xmm2, 3632(%rsp)                             #165.5
        movups    %xmm3, 3648(%rsp)                             #165.5
        movups    %xmm4, 3664(%rsp)                             #165.5
        movups    %xmm5, 3680(%rsp)                             #165.5
        cmpl      $0, 3876(%rsp)                                #165.5
        jne       ..B1.106      # Prob 50%                      #165.5
                                # LOE rsi rdi r14 ebx r12d
..B1.101:                       # Preds ..B1.100
                                # Execution count [6.94e+00]
        testl     %ebx, %ebx                                    #165.5
        je        ..B1.104      # Prob 50%                      #165.5
                                # LOE rsi rdi r14 ebx r12d
..B1.102:                       # Preds ..B1.101
                                # Execution count [3.47e+00]
        cmpl      $0, 3872(%rsp)                                #165.5
        je        ..B1.104      # Prob 50%                      #165.5
                                # LOE rsi rdi r14 ebx r12d
..B1.103:                       # Preds ..B1.102
                                # Execution count [1.74e+00]
        movq      blank_args(%rip), %rsi                        #165.5
        movq      %rsi, 4472(%rsp)                              #165.5
        jmp       ..B1.110      # Prob 100%                     #165.5
                                # LOE rsi rdi r14 ebx r12d
..B1.104:                       # Preds ..B1.101 ..B1.102
                                # Execution count [1.74e+00]
        movq      %rsi, 4472(%rsp)                              #165.5
        jmp       ..B1.110      # Prob 100%                     #165.5
                                # LOE rsi rdi r14 ebx r12d
..B1.106:                       # Preds ..B1.100
                                # Execution count [6.94e+00]
        movq      3616(%rsp), %r8                               #165.5
        testq     %r8, %r8                                      #165.5
        je        ..B1.108      # Prob 12%                      #165.5
                                # LOE rsi rdi r8 r14 ebx r12d
..B1.107:                       # Preds ..B1.106
                                # Execution count [6.11e+00]
        cmpl      $0, 3692(%rsp)                                #165.5
        jne       ..B1.109      # Prob 50%                      #165.5
                                # LOE rsi rdi r8 r14 ebx r12d
..B1.108:                       # Preds ..B1.106 ..B1.107
                                # Execution count [3.89e+00]
        movslq    %r12d, %r12                                   #165.5
        movslq    3632(%rsp), %rcx                              #165.5
        imulq     %r12, %rcx                                    #165.5
        addq      %rcx, %rsi                                    #165.5
        movq      %rsi, 4472(%rsp)                              #165.5
        jmp       ..B1.110      # Prob 100%                     #165.5
                                # LOE rsi rdi r14 ebx r12d
..B1.109:                       # Preds ..B1.107
                                # Execution count [3.06e+00]
        movl      %r12d, %ecx                                   #165.5
        imull     24(%r8), %ecx                                 #165.5
        addl      3628(%rsp), %ecx                              #165.5
        movslq    %ecx, %rcx                                    #165.5
        movq      32(%r8), %r8                                  #165.5
        movl      3632(%rsp), %r9d                              #165.5
        imull     (%r8,%rcx,4), %r9d                            #165.5
        movslq    %r9d, %r9                                     #165.5
        addq      %r9, %rsi                                     #165.5
        movq      %rsi, 4472(%rsp)                              #165.5
                                # LOE rsi rdi r14 ebx r12d
..B1.110:                       # Preds ..B1.99 ..B1.98 ..B1.93 ..B1.103 ..B1.104
                                #       ..B1.108 ..B1.109
                                # Execution count [2.78e+01]
..___tag_value_main.111:
#       save_soln(const double *, double *)
        call      _Z9save_solnPKdPd                             #165.5
..___tag_value_main.112:
                                # LOE r14 ebx r12d
..B1.111:                       # Preds ..B1.110
                                # Execution count [2.78e+01]
        incl      %r12d                                         #165.5
        cmpl      4664(%rsp), %r12d                             #165.5[spill]
        jl        ..B1.71       # Prob 82%                      #165.5
                                # LOE r14 ebx r12d
..B1.112:                       # Preds ..B1.111
                                # Execution count [5.00e+00]
        movl      4656(%rsp), %ecx                              #[spill]
        movl      4664(%rsp), %r8d                              #[spill]
        movl      3496(%rsp), %ebx                              #[spill]
        movl      4400(%rsp), %r12d                             #[spill]
        movq      4608(%rsp), %r15                              #[spill]
        movq      4496(%rsp), %r13                              #[spill]
        cmpl      %ecx, %r8d                                    #165.5
        jne       ..B1.114      # Prob 50%                      #165.5
                                # LOE r13 r14 r15 ebx r12d
..B1.113:                       # Preds ..B1.336 ..B1.112 ..B1.335
                                # Execution count [3.39e+00]
        movl      $2, %edi                                      #165.5
        lea       3696(%rsp), %rsi                              #165.5
..___tag_value_main.113:
#       op_mpi_wait_all(int, op_arg *)
        call      op_mpi_wait_all                               #165.5
..___tag_value_main.114:
                                # LOE r13 r14 r15 ebx r12d
..B1.114:                       # Preds ..B1.113 ..B1.336 ..B1.112
                                # Execution count [5.56e+00]
        movl      $2, %edi                                      #165.5
        lea       3696(%rsp), %rsi                              #165.5
..___tag_value_main.115:
#       op_mpi_set_dirtybit(int, op_arg *)
        call      op_mpi_set_dirtybit                           #165.5
..___tag_value_main.116:
                                # LOE r13 r14 r15 ebx r12d
..B1.115:                       # Preds ..B1.114
                                # Execution count [5.56e+00]
        movq      4472(%rsp), %rsi                              #165.5
        lea       208(%rsp), %rdi                               #165.5
..___tag_value_main.117:
#       op_mpi_reduce_double(op_arg *, double *)
        call      op_mpi_reduce_double                          #165.5
..___tag_value_main.118:
                                # LOE r13 r14 r15 ebx r12d
..B1.116:                       # Preds ..B1.115
                                # Execution count [5.56e+00]
        lea       4384(%rsp), %rdi                              #165.5
        lea       4392(%rsp), %rsi                              #165.5
..___tag_value_main.119:
#       op_timers_core(double *, double *)
        call      op_timers_core                                #165.5
..___tag_value_main.120:
                                # LOE r13 r14 r15 ebx r12d
..B1.117:                       # Preds ..B1.116
                                # Execution count [5.56e+00]
        movsd     4392(%rsp), %xmm0                             #165.5
        movl      $.L_2__STRING.38, %edi                        #165.5
        subsd     4376(%rsp), %xmm0                             #165.5
..___tag_value_main.121:
#       op_mpi_perf_time(const char *, double)
        call      op_mpi_perf_time                              #165.5
..___tag_value_main.122:
                                # LOE r13 r14 r15 ebx r12d
..B1.118:                       # Preds ..B1.117
                                # Execution count [5.56e+00]
        cmpl      $-1, 44(%rsp)                                 #165.5
        jge       ..B1.120      # Prob 78%                      #165.5
                                # LOE r13 r14 r15 ebx r12d
..B1.119:                       # Preds ..B1.118
                                # Execution count [1.22e+00]
        movq      4464(%rsp), %rdi                              #165.5
#       free(void *)
        call      free                                          #165.5
                                # LOE r13 r14 r15 ebx r12d
..B1.120:                       # Preds ..B1.119 ..B1.118
                                # Execution count [5.56e+00]
        cmpl      $-1, 236(%rsp)                                #165.5
        jge       ..B1.122      # Prob 78%                      #165.5
                                # LOE r13 r14 r15 ebx r12d
..B1.121:                       # Preds ..B1.120
                                # Execution count [1.22e+00]
        movq      4472(%rsp), %rdi                              #165.5
#       free(void *)
        call      free                                          #165.5
                                # LOE r13 r14 r15 ebx r12d
..B1.122:                       # Preds ..B1.121 ..B1.120
                                # Execution count [5.56e+00]
        xorb      %al, %al                                      #171.16
        movl      %ebx, 3496(%rsp)                              #171.16[spill]
        movl      %r12d, 4400(%rsp)                             #171.16[spill]
        movq      %r14, 4696(%rsp)                              #171.16[spill]
        movq      %r15, 4608(%rsp)                              #171.16[spill]
        movb      %al, %r15b                                    #171.16
        movq      4456(%rsp), %r14                              #171.16[spill]
        movq      4480(%rsp), %r12                              #171.16[spill]
        movq      4488(%rsp), %rbx                              #171.16[spill]
                                # LOE rbx r12 r13 r14 r15b
..B1.123:                       # Preds ..B1.312 ..B1.122
                                # Execution count [1.11e+01]
        addq      $-16, %rsp                                    #176.58
        movq      %r12, %rsi                                    #176.58
        xorl      %edx, %edx                                    #176.58
        lea       336(%rsp), %rdi                               #176.58
        movl      $2, %r8d                                      #176.58
        movl      $.L_2__STRING.1, %r9d                         #176.58
        movl      $0, (%rsp)                                    #176.58
        movq      4248(%rdi), %rcx                              #176.58[spill]
..___tag_value_main.123:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #176.58
..___tag_value_main.124:
                                # LOE rbx r12 r13 r14 r15b
..B1.124:                       # Preds ..B1.123
                                # Execution count [1.11e+01]
        movq      %r12, %rsi                                    #177.58
        movl      $1, %edx                                      #177.58
        lea       432(%rsp), %rdi                               #177.58
        movl      $2, %r8d                                      #177.58
        movl      $.L_2__STRING.1, %r9d                         #177.58
        movl      $0, (%rsp)                                    #177.58
        movq      4152(%rdi), %rcx                              #177.58[spill]
..___tag_value_main.125:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #177.58
..___tag_value_main.126:
                                # LOE rbx r12 r13 r14 r15b
..B1.125:                       # Preds ..B1.124
                                # Execution count [1.11e+01]
        movl      $2, %edx                                      #178.58
        movq      %r12, %rsi                                    #178.58
        lea       528(%rsp), %rdi                               #178.58
        movl      %edx, %r8d                                    #178.58
        movl      $.L_2__STRING.1, %r9d                         #178.58
        movl      $0, (%rsp)                                    #178.58
        movq      4056(%rdi), %rcx                              #178.58[spill]
..___tag_value_main.127:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #178.58
..___tag_value_main.128:
                                # LOE rbx r12 r13 r14 r15b
..B1.126:                       # Preds ..B1.125
                                # Execution count [1.11e+01]
        movq      %r12, %rsi                                    #179.58
        movl      $3, %edx                                      #179.58
        lea       624(%rsp), %rdi                               #179.58
        movl      $2, %r8d                                      #179.58
        movl      $.L_2__STRING.1, %r9d                         #179.58
        movl      $0, (%rsp)                                    #179.58
        movq      3960(%rdi), %rcx                              #179.58[spill]
..___tag_value_main.129:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #179.58
..___tag_value_main.130:
                                # LOE rbx r12 r13 r14 r15b
..B1.127:                       # Preds ..B1.126
                                # Execution count [1.11e+01]
        movq      %r13, %rsi                                    #180.59
        movl      $-1, %edx                                     #180.59
        lea       720(%rsp), %rdi                               #180.59
        xorl      %ecx, %ecx                                    #180.59
        movl      $4, %r8d                                      #180.59
        movl      $.L_2__STRING.1, %r9d                         #180.59
        movl      $0, (%rsp)                                    #180.59
..___tag_value_main.131:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #180.59
..___tag_value_main.132:
                                # LOE rbx r12 r13 r14 r15b
..B1.128:                       # Preds ..B1.127
                                # Execution count [1.11e+01]
        movq      %r14, %rsi                                    #181.61
        movl      $-1, %edx                                     #181.61
        lea       816(%rsp), %rdi                               #181.61
        xorl      %ecx, %ecx                                    #181.61
        movl      $1, %r8d                                      #181.61
        movl      $.L_2__STRING.1, %r9d                         #181.61
        movl      $1, (%rsp)                                    #181.61
..___tag_value_main.133:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #181.61
..___tag_value_main.134:
                                # LOE rbx r12 r13 r14 r15b
..B1.129:                       # Preds ..B1.128
                                # Execution count [1.11e+01]
        addq      $-560, %rsp                                   #175.7
        movl      $_Z8adt_calcPKdS0_S0_S0_S0_Pd, %edi           #175.7
        movq      %rsp, %r8                                     #175.7
        movl      $.L_2__STRING.39, %esi                        #175.7
        movups    896(%rsp), %xmm0                              #175.7
        movups    912(%rsp), %xmm1                              #175.7
        movups    928(%rsp), %xmm2                              #175.7
        lea       96(%rsp), %r9                                 #175.7
        movups    944(%rsp), %xmm3                              #175.7
        lea       192(%rsp), %r10                               #175.7
        movups    960(%rsp), %xmm4                              #175.7
        lea       288(%rsp), %r11                               #175.7
        movups    976(%rsp), %xmm5                              #175.7
        lea       384(%rsp), %rdx                               #175.7
        movups    1008(%rsp), %xmm7                             #175.7
        movups    992(%rsp), %xmm6                              #175.7
        movups    1024(%rsp), %xmm8                             #175.7
        movups    1040(%rsp), %xmm9                             #175.7
        movups    1056(%rsp), %xmm10                            #175.7
        movups    1072(%rsp), %xmm11                            #175.7
        movups    1104(%rsp), %xmm13                            #175.7
        movups    1088(%rsp), %xmm12                            #175.7
        movups    %xmm0, (%r8)                                  #175.7
        movups    %xmm1, 16(%r8)                                #175.7
        movups    %xmm2, 32(%r8)                                #175.7
        movups    %xmm3, 48(%r8)                                #175.7
        movups    %xmm4, 64(%r8)                                #175.7
        movups    %xmm5, 80(%r8)                                #175.7
        movups    %xmm6, (%r9)                                  #175.7
        movups    %xmm7, 16(%r9)                                #175.7
        movups    %xmm8, 32(%r9)                                #175.7
        movups    %xmm9, 48(%r9)                                #175.7
        movups    %xmm10, 64(%r9)                               #175.7
        movups    %xmm11, 80(%r9)                               #175.7
        movups    %xmm12, (%r10)                                #175.7
        movups    %xmm13, 16(%r10)                              #175.7
        movups    1120(%rsp), %xmm14                            #175.7
        movups    1136(%rsp), %xmm15                            #175.7
        movups    1152(%rsp), %xmm0                             #175.7
        movups    1168(%rsp), %xmm1                             #175.7
        movups    1200(%rsp), %xmm3                             #175.7
        movups    1184(%rsp), %xmm2                             #175.7
        movups    1216(%rsp), %xmm4                             #175.7
        movups    1232(%rsp), %xmm5                             #175.7
        movups    1248(%rsp), %xmm6                             #175.7
        movups    1264(%rsp), %xmm7                             #175.7
        movups    1296(%rsp), %xmm9                             #175.7
        movups    1280(%rsp), %xmm8                             #175.7
        movups    1312(%rsp), %xmm10                            #175.7
        movups    1328(%rsp), %xmm11                            #175.7
        movups    1344(%rsp), %xmm12                            #175.7
        movups    1360(%rsp), %xmm13                            #175.7
        movups    %xmm14, 32(%r10)                              #175.7
        movups    %xmm15, 48(%r10)                              #175.7
        movups    %xmm0, 64(%r10)                               #175.7
        movups    %xmm1, 80(%r10)                               #175.7
        movups    %xmm2, (%r11)                                 #175.7
        movups    %xmm3, 16(%r11)                               #175.7
        movups    %xmm4, 32(%r11)                               #175.7
        movups    %xmm5, 48(%r11)                               #175.7
        movups    %xmm6, 64(%r11)                               #175.7
        movups    %xmm7, 80(%r11)                               #175.7
        movups    %xmm8, (%rdx)                                 #175.7
        movups    %xmm9, 16(%rdx)                               #175.7
        movups    %xmm10, 32(%rdx)                              #175.7
        movups    %xmm11, 48(%rdx)                              #175.7
        movups    %xmm12, 64(%rdx)                              #175.7
        movups    %xmm13, 80(%rdx)                              #175.7
        movups    1376(%rsp), %xmm14                            #175.7
        lea       480(%rsp), %rdx                               #175.7
        movups    1392(%rsp), %xmm15                            #175.7
        movups    1408(%rsp), %xmm0                             #175.7
        movups    1424(%rsp), %xmm1                             #175.7
        movups    1440(%rsp), %xmm2                             #175.7
        movups    1456(%rsp), %xmm3                             #175.7
        movups    %xmm14, (%rdx)                                #175.7
        movups    %xmm15, 16(%rdx)                              #175.7
        movups    %xmm0, 32(%rdx)                               #175.7
        movups    %xmm1, 48(%rdx)                               #175.7
        movups    %xmm2, 64(%rdx)                               #175.7
        movups    %xmm3, 80(%rdx)                               #175.7
        movq      5272(%rsp), %rdx                              #175.7[spill]
..___tag_value_main.135:
#       op_par_loop<const double, const double, const double, const double, const double, double>(void (*)(const double *, const double *, const double *, const double *, const double *, double *), const char *, op_set, op_arg, op_arg, op_arg, op_arg, op_arg, op_arg)
        call      _Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_ #175.7
..___tag_value_main.136:
                                # LOE rbx r12 r13 r14 r15b
..B1.130:                       # Preds ..B1.129
                                # Execution count [1.11e+01]
        addq      $560, %rsp                                    #186.58
        movq      %r12, %rsi                                    #186.58
        xorl      %edx, %edx                                    #186.58
        lea       912(%rsp), %rdi                               #186.58
        movl      $2, %r8d                                      #186.58
        movl      $.L_2__STRING.1, %r9d                         #186.58
        movl      $0, (%rsp)                                    #186.58
        movq      3696(%rdi), %rcx                              #186.58[spill]
..___tag_value_main.137:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #186.58
..___tag_value_main.138:
                                # LOE rbx r12 r13 r14 r15b
..B1.131:                       # Preds ..B1.130
                                # Execution count [1.11e+01]
        movq      %r12, %rsi                                    #187.58
        movl      $1, %edx                                      #187.58
        lea       1008(%rsp), %rdi                              #187.58
        movl      $2, %r8d                                      #187.58
        movl      $.L_2__STRING.1, %r9d                         #187.58
        movl      $0, (%rsp)                                    #187.58
        movq      3600(%rdi), %rcx                              #187.58[spill]
..___tag_value_main.139:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #187.58
..___tag_value_main.140:
                                # LOE rbx r12 r13 r14 r15b
..B1.132:                       # Preds ..B1.131
                                # Execution count [1.11e+01]
        movq      %r13, %rsi                                    #188.59
        xorl      %edx, %edx                                    #188.59
        lea       1104(%rsp), %rdi                              #188.59
        movq      %rbx, %rcx                                    #188.59
        movl      $4, %r8d                                      #188.59
        movl      $.L_2__STRING.1, %r9d                         #188.59
        movl      $0, (%rsp)                                    #188.59
..___tag_value_main.141:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #188.59
..___tag_value_main.142:
                                # LOE rbx r12 r13 r14 r15b
..B1.133:                       # Preds ..B1.132
                                # Execution count [1.11e+01]
        movq      %r13, %rsi                                    #189.59
        movl      $1, %edx                                      #189.59
        lea       1200(%rsp), %rdi                              #189.59
        movq      %rbx, %rcx                                    #189.59
        movl      $4, %r8d                                      #189.59
        movl      $.L_2__STRING.1, %r9d                         #189.59
        movl      $0, (%rsp)                                    #189.59
..___tag_value_main.143:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #189.59
..___tag_value_main.144:
                                # LOE rbx r12 r13 r14 r15b
..B1.134:                       # Preds ..B1.133
                                # Execution count [1.11e+01]
        movq      %r14, %rsi                                    #190.61
        xorl      %edx, %edx                                    #190.61
        lea       1296(%rsp), %rdi                              #190.61
        movq      %rbx, %rcx                                    #190.61
        movl      $1, %r8d                                      #190.61
        movl      $.L_2__STRING.1, %r9d                         #190.61
        movl      $0, (%rsp)                                    #190.61
..___tag_value_main.145:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #190.61
..___tag_value_main.146:
                                # LOE rbx r12 r13 r14 r15b
..B1.135:                       # Preds ..B1.134
                                # Execution count [1.11e+01]
        movl      $1, %edx                                      #191.61
        movq      %r14, %rsi                                    #191.61
        lea       1392(%rsp), %rdi                              #191.61
        movq      %rbx, %rcx                                    #191.61
        movl      %edx, %r8d                                    #191.61
        movl      $.L_2__STRING.1, %r9d                         #191.61
        movl      $0, (%rsp)                                    #191.61
..___tag_value_main.147:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #191.61
..___tag_value_main.148:
                                # LOE rbx r12 r13 r14 r15b
..B1.136:                       # Preds ..B1.135
                                # Execution count [1.11e+01]
        xorl      %edx, %edx                                    #192.61
        movq      %rbx, %rcx                                    #192.61
        lea       1488(%rsp), %rdi                              #192.61
        movl      $4, %r8d                                      #192.61
        movl      $.L_2__STRING.1, %r9d                         #192.61
        movl      $3, (%rsp)                                    #192.61
        movq      3080(%rdi), %rsi                              #192.61[spill]
..___tag_value_main.149:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #192.61
..___tag_value_main.150:
                                # LOE rbx r12 r13 r14 r15b
..B1.137:                       # Preds ..B1.136
                                # Execution count [1.11e+01]
        movl      $1, %edx                                      #193.61
        movq      %rbx, %rcx                                    #193.61
        lea       1584(%rsp), %rdi                              #193.61
        movl      $4, %r8d                                      #193.61
        movl      $.L_2__STRING.1, %r9d                         #193.61
        movl      $3, (%rsp)                                    #193.61
        movq      2984(%rdi), %rsi                              #193.61[spill]
..___tag_value_main.151:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #193.61
..___tag_value_main.152:
                                # LOE rbx r12 r13 r14 r15b
..B1.138:                       # Preds ..B1.137
                                # Execution count [1.11e+01]
        addq      $-752, %rsp                                   #185.7
        movl      $_Z8res_calcPKdS0_S0_S0_S0_S0_PdS1_, %edi     #185.7
        movq      %rsp, %r8                                     #185.7
        movl      $.L_2__STRING.40, %esi                        #185.7
        movups    1664(%rsp), %xmm0                             #185.7
        movups    1680(%rsp), %xmm1                             #185.7
        movups    1696(%rsp), %xmm2                             #185.7
        lea       96(%rsp), %r9                                 #185.7
        movups    1712(%rsp), %xmm3                             #185.7
        lea       192(%rsp), %r10                               #185.7
        movups    1728(%rsp), %xmm4                             #185.7
        lea       288(%rsp), %r11                               #185.7
        movups    1744(%rsp), %xmm5                             #185.7
        lea       384(%rsp), %rdx                               #185.7
        movups    1776(%rsp), %xmm7                             #185.7
        movups    1760(%rsp), %xmm6                             #185.7
        movups    1792(%rsp), %xmm8                             #185.7
        movups    1808(%rsp), %xmm9                             #185.7
        movups    1824(%rsp), %xmm10                            #185.7
        movups    1840(%rsp), %xmm11                            #185.7
        movups    1872(%rsp), %xmm13                            #185.7
        movups    1856(%rsp), %xmm12                            #185.7
        movups    %xmm0, (%r8)                                  #185.7
        movups    %xmm1, 16(%r8)                                #185.7
        movups    %xmm2, 32(%r8)                                #185.7
        movups    %xmm3, 48(%r8)                                #185.7
        movups    %xmm4, 64(%r8)                                #185.7
        movups    %xmm5, 80(%r8)                                #185.7
        movups    %xmm6, (%r9)                                  #185.7
        movups    %xmm7, 16(%r9)                                #185.7
        movups    %xmm8, 32(%r9)                                #185.7
        movups    %xmm9, 48(%r9)                                #185.7
        movups    %xmm10, 64(%r9)                               #185.7
        movups    %xmm11, 80(%r9)                               #185.7
        movups    %xmm12, (%r10)                                #185.7
        movups    %xmm13, 16(%r10)                              #185.7
        movups    1888(%rsp), %xmm14                            #185.7
        movups    1904(%rsp), %xmm15                            #185.7
        movups    1920(%rsp), %xmm0                             #185.7
        movups    1936(%rsp), %xmm1                             #185.7
        movups    1968(%rsp), %xmm3                             #185.7
        movups    1952(%rsp), %xmm2                             #185.7
        movups    1984(%rsp), %xmm4                             #185.7
        movups    2000(%rsp), %xmm5                             #185.7
        movups    2016(%rsp), %xmm6                             #185.7
        movups    2032(%rsp), %xmm7                             #185.7
        movups    2064(%rsp), %xmm9                             #185.7
        movups    2048(%rsp), %xmm8                             #185.7
        movups    2080(%rsp), %xmm10                            #185.7
        movups    2096(%rsp), %xmm11                            #185.7
        movups    2112(%rsp), %xmm12                            #185.7
        movups    2128(%rsp), %xmm13                            #185.7
        movups    %xmm14, 32(%r10)                              #185.7
        movups    %xmm15, 48(%r10)                              #185.7
        movups    %xmm0, 64(%r10)                               #185.7
        movups    %xmm1, 80(%r10)                               #185.7
        movups    %xmm2, (%r11)                                 #185.7
        movups    %xmm3, 16(%r11)                               #185.7
        movups    %xmm4, 32(%r11)                               #185.7
        movups    %xmm5, 48(%r11)                               #185.7
        movups    %xmm6, 64(%r11)                               #185.7
        movups    %xmm7, 80(%r11)                               #185.7
        movups    %xmm8, (%rdx)                                 #185.7
        movups    %xmm9, 16(%rdx)                               #185.7
        movups    %xmm10, 32(%rdx)                              #185.7
        movups    %xmm11, 48(%rdx)                              #185.7
        movups    %xmm12, 64(%rdx)                              #185.7
        movups    %xmm13, 80(%rdx)                              #185.7
        movups    2144(%rsp), %xmm14                            #185.7
        lea       480(%rsp), %rdx                               #185.7
        movups    2160(%rsp), %xmm15                            #185.7
        movups    2176(%rsp), %xmm0                             #185.7
        movups    2192(%rsp), %xmm1                             #185.7
        movups    2208(%rsp), %xmm2                             #185.7
        movups    2224(%rsp), %xmm3                             #185.7
        movups    2256(%rsp), %xmm5                             #185.7
        movups    2240(%rsp), %xmm4                             #185.7
        movups    2272(%rsp), %xmm6                             #185.7
        movups    2288(%rsp), %xmm7                             #185.7
        movups    2304(%rsp), %xmm8                             #185.7
        movups    2320(%rsp), %xmm9                             #185.7
        movups    %xmm14, (%rdx)                                #185.7
        movups    %xmm15, 16(%rdx)                              #185.7
        movups    %xmm0, 32(%rdx)                               #185.7
        movups    %xmm1, 48(%rdx)                               #185.7
        movups    %xmm2, 64(%rdx)                               #185.7
        movups    %xmm3, 80(%rdx)                               #185.7
        movups    2336(%rsp), %xmm10                            #185.7
        lea       576(%rsp), %rdx                               #185.7
        movups    2352(%rsp), %xmm11                            #185.7
        movups    2368(%rsp), %xmm12                            #185.7
        movups    2384(%rsp), %xmm13                            #185.7
        movups    2400(%rsp), %xmm14                            #185.7
        movups    2416(%rsp), %xmm15                            #185.7
        movups    %xmm4, (%rdx)                                 #185.7
        movups    %xmm5, 16(%rdx)                               #185.7
        movups    %xmm6, 32(%rdx)                               #185.7
        movups    %xmm7, 48(%rdx)                               #185.7
        movups    %xmm8, 64(%rdx)                               #185.7
        movups    %xmm9, 80(%rdx)                               #185.7
        lea       672(%rsp), %rdx                               #185.7
        movups    %xmm10, (%rdx)                                #185.7
        movups    %xmm11, 16(%rdx)                              #185.7
        movups    %xmm12, 32(%rdx)                              #185.7
        movups    %xmm13, 48(%rdx)                              #185.7
        movups    %xmm14, 64(%rdx)                              #185.7
        movups    %xmm15, 80(%rdx)                              #185.7
        movq      5384(%rsp), %rdx                              #185.7[spill]
..___tag_value_main.153:
#       op_par_loop<const double, const double, const double, const double, const double, const double, double, double>(void (*)(const double *, const double *, const double *, const double *, const double *, const double *, double *, double *), const char *, op_set, op_arg, op_arg, op_arg, op_arg, op_arg, op_arg, op_arg, op_arg)
        call      _Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_ #185.7
..___tag_value_main.154:
                                # LOE rbx r12 r13 r14 r15b
..B1.139:                       # Preds ..B1.138
                                # Execution count [1.11e+01]
        addq      $752, %rsp                                    #196.59
        movq      %r12, %rsi                                    #196.59
        xorl      %edx, %edx                                    #196.59
        lea       1680(%rsp), %rdi                              #196.59
        movl      $2, %r8d                                      #196.59
        movl      $.L_2__STRING.1, %r9d                         #196.59
        movl      $0, (%rsp)                                    #196.59
        movq      2920(%rdi), %rcx                              #196.59[spill]
..___tag_value_main.155:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #196.59
..___tag_value_main.156:
                                # LOE rbx r12 r13 r14 r15b
..B1.140:                       # Preds ..B1.139
                                # Execution count [1.11e+01]
        movq      %r12, %rsi                                    #197.59
        movl      $1, %edx                                      #197.59
        lea       1776(%rsp), %rdi                              #197.59
        movl      $2, %r8d                                      #197.59
        movl      $.L_2__STRING.1, %r9d                         #197.59
        movl      $0, (%rsp)                                    #197.59
        movq      2824(%rdi), %rcx                              #197.59[spill]
..___tag_value_main.157:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #197.59
..___tag_value_main.158:
                                # LOE rbx r12 r13 r14 r15b
..B1.141:                       # Preds ..B1.140
                                # Execution count [1.11e+01]
        movq      %r13, %rsi                                    #198.60
        xorl      %edx, %edx                                    #198.60
        lea       1872(%rsp), %rdi                              #198.60
        movl      $4, %r8d                                      #198.60
        movl      $.L_2__STRING.1, %r9d                         #198.60
        movl      $0, (%rsp)                                    #198.60
        movq      2720(%rdi), %rcx                              #198.60[spill]
..___tag_value_main.159:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #198.60
..___tag_value_main.160:
                                # LOE rbx r12 r13 r14 r15b
..B1.142:                       # Preds ..B1.141
                                # Execution count [1.11e+01]
        movq      %r14, %rsi                                    #199.62
        xorl      %edx, %edx                                    #199.62
        lea       1968(%rsp), %rdi                              #199.62
        movl      $1, %r8d                                      #199.62
        movl      $.L_2__STRING.1, %r9d                         #199.62
        movl      $0, (%rsp)                                    #199.62
        movq      2624(%rdi), %rcx                              #199.62[spill]
..___tag_value_main.161:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #199.62
..___tag_value_main.162:
                                # LOE rbx r12 r13 r14 r15b
..B1.143:                       # Preds ..B1.142
                                # Execution count [1.11e+01]
        xorl      %edx, %edx                                    #200.62
        movl      $4, %r8d                                      #200.62
        lea       2064(%rsp), %rdi                              #200.62
        movl      $.L_2__STRING.1, %r9d                         #200.62
        movl      $3, (%rsp)                                    #200.62
        movq      2504(%rdi), %rsi                              #200.62[spill]
        movq      2528(%rdi), %rcx                              #200.62[spill]
..___tag_value_main.163:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #200.62
..___tag_value_main.164:
                                # LOE rbx r12 r13 r14 r15b
..B1.144:                       # Preds ..B1.143
                                # Execution count [1.11e+01]
        movl      $-1, %edx                                     #201.60
        xorl      %ecx, %ecx                                    #201.60
        lea       2160(%rsp), %rdi                              #201.60
        movl      $1, %r8d                                      #201.60
        movl      $.L_2__STRING.18, %r9d                        #201.60
        movl      $0, (%rsp)                                    #201.60
        movq      2416(%rdi), %rsi                              #201.60[spill]
..___tag_value_main.165:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #201.60
..___tag_value_main.166:
                                # LOE rbx r12 r13 r14 r15b
..B1.145:                       # Preds ..B1.144
                                # Execution count [1.11e+01]
        addq      $-560, %rsp                                   #195.7
        movl      $_Z9bres_calcPKdS0_S0_S0_PdPKi, %edi          #195.7
        movq      %rsp, %r8                                     #195.7
        movl      $.L_2__STRING.41, %esi                        #195.7
        movups    2240(%rsp), %xmm0                             #195.7
        movups    2256(%rsp), %xmm1                             #195.7
        movups    2272(%rsp), %xmm2                             #195.7
        lea       96(%rsp), %r9                                 #195.7
        movups    2288(%rsp), %xmm3                             #195.7
        lea       192(%rsp), %r10                               #195.7
        movups    2304(%rsp), %xmm4                             #195.7
        lea       288(%rsp), %r11                               #195.7
        movups    2320(%rsp), %xmm5                             #195.7
        lea       384(%rsp), %rdx                               #195.7
        movups    2352(%rsp), %xmm7                             #195.7
        movups    2336(%rsp), %xmm6                             #195.7
        movups    2368(%rsp), %xmm8                             #195.7
        movups    2384(%rsp), %xmm9                             #195.7
        movups    2400(%rsp), %xmm10                            #195.7
        movups    2416(%rsp), %xmm11                            #195.7
        movups    2448(%rsp), %xmm13                            #195.7
        movups    2432(%rsp), %xmm12                            #195.7
        movups    %xmm0, (%r8)                                  #195.7
        movups    %xmm1, 16(%r8)                                #195.7
        movups    %xmm2, 32(%r8)                                #195.7
        movups    %xmm3, 48(%r8)                                #195.7
        movups    %xmm4, 64(%r8)                                #195.7
        movups    %xmm5, 80(%r8)                                #195.7
        movups    %xmm6, (%r9)                                  #195.7
        movups    %xmm7, 16(%r9)                                #195.7
        movups    %xmm8, 32(%r9)                                #195.7
        movups    %xmm9, 48(%r9)                                #195.7
        movups    %xmm10, 64(%r9)                               #195.7
        movups    %xmm11, 80(%r9)                               #195.7
        movups    %xmm12, (%r10)                                #195.7
        movups    %xmm13, 16(%r10)                              #195.7
        movups    2464(%rsp), %xmm14                            #195.7
        movups    2480(%rsp), %xmm15                            #195.7
        movups    2496(%rsp), %xmm0                             #195.7
        movups    2512(%rsp), %xmm1                             #195.7
        movups    2544(%rsp), %xmm3                             #195.7
        movups    2528(%rsp), %xmm2                             #195.7
        movups    2560(%rsp), %xmm4                             #195.7
        movups    2576(%rsp), %xmm5                             #195.7
        movups    2592(%rsp), %xmm6                             #195.7
        movups    2608(%rsp), %xmm7                             #195.7
        movups    2640(%rsp), %xmm9                             #195.7
        movups    2624(%rsp), %xmm8                             #195.7
        movups    2656(%rsp), %xmm10                            #195.7
        movups    2672(%rsp), %xmm11                            #195.7
        movups    2688(%rsp), %xmm12                            #195.7
        movups    2704(%rsp), %xmm13                            #195.7
        movups    %xmm14, 32(%r10)                              #195.7
        movups    %xmm15, 48(%r10)                              #195.7
        movups    %xmm0, 64(%r10)                               #195.7
        movups    %xmm1, 80(%r10)                               #195.7
        movups    %xmm2, (%r11)                                 #195.7
        movups    %xmm3, 16(%r11)                               #195.7
        movups    %xmm4, 32(%r11)                               #195.7
        movups    %xmm5, 48(%r11)                               #195.7
        movups    %xmm6, 64(%r11)                               #195.7
        movups    %xmm7, 80(%r11)                               #195.7
        movups    %xmm8, (%rdx)                                 #195.7
        movups    %xmm9, 16(%rdx)                               #195.7
        movups    %xmm10, 32(%rdx)                              #195.7
        movups    %xmm11, 48(%rdx)                              #195.7
        movups    %xmm12, 64(%rdx)                              #195.7
        movups    %xmm13, 80(%rdx)                              #195.7
        movups    2720(%rsp), %xmm14                            #195.7
        lea       480(%rsp), %rdx                               #195.7
        movups    2736(%rsp), %xmm15                            #195.7
        movups    2752(%rsp), %xmm0                             #195.7
        movups    2768(%rsp), %xmm1                             #195.7
        movups    2784(%rsp), %xmm2                             #195.7
        movups    2800(%rsp), %xmm3                             #195.7
        movups    %xmm14, (%rdx)                                #195.7
        movups    %xmm15, 16(%rdx)                              #195.7
        movups    %xmm0, 32(%rdx)                               #195.7
        movups    %xmm1, 48(%rdx)                               #195.7
        movups    %xmm2, 64(%rdx)                               #195.7
        movups    %xmm3, 80(%rdx)                               #195.7
        movq      5176(%rsp), %rdx                              #195.7[spill]
..___tag_value_main.167:
#       op_par_loop<const double, const double, const double, const double, double, const int>(void (*)(const double *, const double *, const double *, const double *, double *, const int *), const char *, op_set, op_arg, op_arg, op_arg, op_arg, op_arg, op_arg)
        call      _Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_ #195.7
..___tag_value_main.168:
                                # LOE rbx r12 r13 r14 r15b
..B1.390:                       # Preds ..B1.145
                                # Execution count [1.11e+01]
        addq      $576, %rsp                                    #195.7
                                # LOE rbx r12 r13 r14 r15b
..B1.146:                       # Preds ..B1.390
                                # Execution count [1.11e+01]
        movq      $0, 4512(%rsp)                                #205.7
        addq      $-16, %rsp                                    #208.62
        movl      $-1, %edx                                     #208.62
        lea       2256(%rsp), %rdi                              #208.62
        xorl      %ecx, %ecx                                    #208.62
        movl      $4, %r8d                                      #208.62
        movl      $.L_2__STRING.1, %r9d                         #208.62
        movl      $0, (%rsp)                                    #208.62
        movq      2368(%rdi), %rsi                              #208.62[spill]
..___tag_value_main.169:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #208.62
..___tag_value_main.170:
                                # LOE rbx r12 r13 r14 r15b
..B1.147:                       # Preds ..B1.146
                                # Execution count [1.11e+01]
        movq      %r13, %rsi                                    #209.59
        movl      $-1, %edx                                     #209.59
        lea       2352(%rsp), %rdi                              #209.59
        xorl      %ecx, %ecx                                    #209.59
        movl      $4, %r8d                                      #209.59
        movl      $.L_2__STRING.1, %r9d                         #209.59
        movl      $1, (%rsp)                                    #209.59
..___tag_value_main.171:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #209.59
..___tag_value_main.172:
                                # LOE rbx r12 r13 r14 r15b
..B1.148:                       # Preds ..B1.147
                                # Execution count [1.11e+01]
        movl      $-1, %edx                                     #210.61
        xorl      %ecx, %ecx                                    #210.61
        lea       2448(%rsp), %rdi                              #210.61
        movl      $4, %r8d                                      #210.61
        movl      $.L_2__STRING.1, %r9d                         #210.61
        movl      $2, (%rsp)                                    #210.61
        movq      2120(%rdi), %rsi                              #210.61[spill]
..___tag_value_main.173:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #210.61
..___tag_value_main.174:
                                # LOE rbx r12 r13 r14 r15b
..B1.149:                       # Preds ..B1.148
                                # Execution count [1.11e+01]
        movq      %r14, %rsi                                    #211.61
        movl      $-1, %edx                                     #211.61
        lea       2544(%rsp), %rdi                              #211.61
        xorl      %ecx, %ecx                                    #211.61
        movl      $1, %r8d                                      #211.61
        movl      $.L_2__STRING.1, %r9d                         #211.61
        movl      $0, (%rsp)                                    #211.61
..___tag_value_main.175:
#       op_arg_dat(op_dat, int, op_map, int, const char *, op_access)
        call      op_arg_dat                                    #211.61
..___tag_value_main.176:
                                # LOE rbx r12 r13 r14 r15b
..B1.394:                       # Preds ..B1.149
                                # Execution count [1.11e+01]
        addq      $16, %rsp                                     #211.61
                                # LOE rbx r12 r13 r14 r15b
..B1.150:                       # Preds ..B1.394
                                # Execution count [1.11e+01]
        movl      $1, %edx                                      #212.49
        lea       2624(%rsp), %rdi                              #212.49
        movl      $.L_2__STRING.1, %ecx                         #212.49
        lea       4512(%rsp), %rsi                              #212.49
        movl      $8, %r8d                                      #212.49
        movl      $3, %r9d                                      #212.49
..___tag_value_main.177:
#       op_arg_gbl_char(char *, int, const char *, int, op_access)
        call      op_arg_gbl_char                               #212.49
..___tag_value_main.178:
                                # LOE rbx r12 r13 r14 r15b
..B1.151:                       # Preds ..B1.150
                                # Execution count [5.56e+00]
        movups    2336(%rsp), %xmm6                             #207.7
        movups    2352(%rsp), %xmm7                             #207.7
        movups    2640(%rsp), %xmm0                             #212.49
        movups    %xmm6, 2720(%rsp)                             #207.7
        movups    %xmm7, 2736(%rsp)                             #207.7
        movups    2624(%rsp), %xmm5                             #212.49
        movups    2656(%rsp), %xmm1                             #212.49
        movups    2672(%rsp), %xmm2                             #212.49
        movups    2688(%rsp), %xmm3                             #212.49
        movups    2704(%rsp), %xmm4                             #212.49
        movups    2368(%rsp), %xmm8                             #207.7
        movups    2384(%rsp), %xmm9                             #207.7
        movups    2400(%rsp), %xmm10                            #207.7
        movups    2416(%rsp), %xmm11                            #207.7
        movups    2448(%rsp), %xmm13                            #207.7
        movups    2432(%rsp), %xmm12                            #207.7
        movups    2464(%rsp), %xmm14                            #207.7
        movups    2480(%rsp), %xmm15                            #207.7
        movups    2496(%rsp), %xmm6                             #207.7
        movups    2512(%rsp), %xmm7                             #207.7
        movups    %xmm0, 2928(%rsp)                             #207.7
        pxor      %xmm0, %xmm0                                  #207.7
        movups    %xmm8, 2752(%rsp)                             #207.7
        movups    %xmm9, 2768(%rsp)                             #207.7
        movups    %xmm10, 2784(%rsp)                            #207.7
        movups    %xmm11, 2800(%rsp)                            #207.7
        movups    %xmm12, 2816(%rsp)                            #207.7
        movups    %xmm13, 2832(%rsp)                            #207.7
        movups    %xmm14, 2848(%rsp)                            #207.7
        movups    %xmm15, 2864(%rsp)                            #207.7
        movups    %xmm6, 2880(%rsp)                             #207.7
        movups    %xmm7, 2896(%rsp)                             #207.7
        movups    %xmm5, 2912(%rsp)                             #207.7
        movups    %xmm1, 2944(%rsp)                             #207.7
        movups    %xmm2, 2960(%rsp)                             #207.7
        movups    %xmm3, 2976(%rsp)                             #207.7
        movups    %xmm4, 2992(%rsp)                             #207.7
        movups    %xmm0, 4416(%rsp)                             #207.7
        movups    %xmm0, 4432(%rsp)                             #207.7
        movq      %xmm0, 4448(%rsp)                             #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.152:                       # Preds ..B1.151
                                # Execution count [1.11e+01]
        movups    2240(%rsp), %xmm0                             #207.7
        movups    2256(%rsp), %xmm1                             #207.7
        movups    2272(%rsp), %xmm2                             #207.7
        movups    2288(%rsp), %xmm3                             #207.7
        movups    2304(%rsp), %xmm4                             #207.7
        movups    2320(%rsp), %xmm5                             #207.7
        movups    2736(%rsp), %xmm7                             #207.7
        movups    2720(%rsp), %xmm6                             #207.7
        movups    2752(%rsp), %xmm8                             #207.7
        movups    2768(%rsp), %xmm9                             #207.7
        movups    2784(%rsp), %xmm10                            #207.7
        movups    2800(%rsp), %xmm11                            #207.7
        movups    2832(%rsp), %xmm13                            #207.7
        movups    2816(%rsp), %xmm12                            #207.7
        movups    %xmm0, 3008(%rsp)                             #207.7
        movups    %xmm1, 3024(%rsp)                             #207.7
        movups    %xmm2, 3040(%rsp)                             #207.7
        movups    %xmm3, 3056(%rsp)                             #207.7
        movups    %xmm4, 3072(%rsp)                             #207.7
        movups    %xmm5, 3088(%rsp)                             #207.7
        movups    %xmm6, 3104(%rsp)                             #207.7
        movups    %xmm7, 3120(%rsp)                             #207.7
        movups    %xmm8, 3136(%rsp)                             #207.7
        movups    %xmm9, 3152(%rsp)                             #207.7
        movups    %xmm10, 3168(%rsp)                            #207.7
        movups    %xmm11, 3184(%rsp)                            #207.7
        movups    %xmm12, 3200(%rsp)                            #207.7
        movups    %xmm13, 3216(%rsp)                            #207.7
        movups    2848(%rsp), %xmm14                            #207.7
        movups    2864(%rsp), %xmm15                            #207.7
        movups    2880(%rsp), %xmm0                             #207.7
        movups    2896(%rsp), %xmm1                             #207.7
        movups    2544(%rsp), %xmm3                             #207.7
        movups    2528(%rsp), %xmm2                             #207.7
        movups    2560(%rsp), %xmm4                             #207.7
        movups    2576(%rsp), %xmm5                             #207.7
        movups    2592(%rsp), %xmm6                             #207.7
        movups    2608(%rsp), %xmm7                             #207.7
        movups    2928(%rsp), %xmm9                             #207.7
        movups    2912(%rsp), %xmm8                             #207.7
        movups    2944(%rsp), %xmm10                            #207.7
        movups    2960(%rsp), %xmm11                            #207.7
        movups    2976(%rsp), %xmm12                            #207.7
        movups    2992(%rsp), %xmm13                            #207.7
        movups    %xmm14, 3232(%rsp)                            #207.7
        movups    %xmm15, 3248(%rsp)                            #207.7
        movups    %xmm0, 3264(%rsp)                             #207.7
        movups    %xmm1, 3280(%rsp)                             #207.7
        movups    %xmm2, 3296(%rsp)                             #207.7
        movups    %xmm3, 3312(%rsp)                             #207.7
        movups    %xmm4, 3328(%rsp)                             #207.7
        movups    %xmm5, 3344(%rsp)                             #207.7
        movups    %xmm6, 3360(%rsp)                             #207.7
        movups    %xmm7, 3376(%rsp)                             #207.7
        movups    %xmm8, 3392(%rsp)                             #207.7
        movups    %xmm9, 3408(%rsp)                             #207.7
        movups    %xmm10, 3424(%rsp)                            #207.7
        movups    %xmm11, 3440(%rsp)                            #207.7
        movups    %xmm12, 3456(%rsp)                            #207.7
        movups    %xmm13, 3472(%rsp)                            #207.7
        cmpl      $-1, 2268(%rsp)                               #207.7
        jge       ..B1.155      # Prob 78%                      #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.153:                       # Preds ..B1.152
                                # Execution count [2.44e+00]
        movl      3036(%rsp), %edi                              #207.7
        negl      %edi                                          #207.7
        movslq    %edi, %rdi                                    #207.7
        shlq      $3, %rdi                                      #207.7
..___tag_value_main.179:
#       op_malloc(size_t)
        call      op_malloc                                     #207.7
..___tag_value_main.180:
                                # LOE rax rbx r12 r13 r14 r15b
..B1.154:                       # Preds ..B1.153
                                # Execution count [2.44e+00]
        movq      %rax, 4416(%rsp)                              #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.155:                       # Preds ..B1.152 ..B1.154
                                # Execution count [1.11e+01]
        cmpl      $-1, 2748(%rsp)                               #207.7
        jge       ..B1.158      # Prob 78%                      #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.156:                       # Preds ..B1.155
                                # Execution count [2.44e+00]
        movl      3132(%rsp), %edi                              #207.7
        negl      %edi                                          #207.7
        movslq    %edi, %rdi                                    #207.7
        shlq      $3, %rdi                                      #207.7
..___tag_value_main.181:
#       op_malloc(size_t)
        call      op_malloc                                     #207.7
..___tag_value_main.182:
                                # LOE rax rbx r12 r13 r14 r15b
..B1.157:                       # Preds ..B1.156
                                # Execution count [2.44e+00]
        movq      %rax, 4424(%rsp)                              #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.158:                       # Preds ..B1.155 ..B1.157
                                # Execution count [1.11e+01]
        cmpl      $-1, 2844(%rsp)                               #207.7
        jge       ..B1.161      # Prob 78%                      #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.159:                       # Preds ..B1.158
                                # Execution count [2.44e+00]
        movl      3228(%rsp), %edi                              #207.7
        negl      %edi                                          #207.7
        movslq    %edi, %rdi                                    #207.7
        shlq      $3, %rdi                                      #207.7
..___tag_value_main.183:
#       op_malloc(size_t)
        call      op_malloc                                     #207.7
..___tag_value_main.184:
                                # LOE rax rbx r12 r13 r14 r15b
..B1.160:                       # Preds ..B1.159
                                # Execution count [2.44e+00]
        movq      %rax, 4432(%rsp)                              #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.161:                       # Preds ..B1.158 ..B1.160
                                # Execution count [1.11e+01]
        cmpl      $-1, 2556(%rsp)                               #207.7
        jge       ..B1.164      # Prob 78%                      #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.162:                       # Preds ..B1.161
                                # Execution count [2.44e+00]
        movl      3324(%rsp), %edi                              #207.7
        negl      %edi                                          #207.7
        movslq    %edi, %rdi                                    #207.7
        shlq      $3, %rdi                                      #207.7
..___tag_value_main.185:
#       op_malloc(size_t)
        call      op_malloc                                     #207.7
..___tag_value_main.186:
                                # LOE rax rbx r12 r13 r14 r15b
..B1.163:                       # Preds ..B1.162
                                # Execution count [2.44e+00]
        movq      %rax, 4440(%rsp)                              #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.164:                       # Preds ..B1.161 ..B1.163
                                # Execution count [1.11e+01]
        cmpl      $-1, 2940(%rsp)                               #207.7
        jge       ..B1.167      # Prob 78%                      #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.165:                       # Preds ..B1.164
                                # Execution count [2.44e+00]
        movl      3420(%rsp), %edi                              #207.7
        negl      %edi                                          #207.7
        movslq    %edi, %rdi                                    #207.7
        shlq      $3, %rdi                                      #207.7
..___tag_value_main.187:
#       op_malloc(size_t)
        call      op_malloc                                     #207.7
..___tag_value_main.188:
                                # LOE rax rbx r12 r13 r14 r15b
..B1.166:                       # Preds ..B1.165
                                # Execution count [2.44e+00]
        movq      %rax, 4448(%rsp)                              #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.167:                       # Preds ..B1.164 ..B1.166
                                # Execution count [1.11e+01]
        cmpl      $0, 3092(%rsp)                                #207.7
        jne       ..B1.171      # Prob 50%                      #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.168:                       # Preds ..B1.167
                                # Execution count [5.56e+00]
        movl      3040(%rsp), %edi                              #207.7
        cmpl      blank_args_size(%rip), %edi                   #207.7
        jle       ..B1.171      # Prob 68%                      #207.7
                                # LOE rbx r12 r13 r14 edi r15b
..B1.169:                       # Preds ..B1.168
                                # Execution count [1.75e+00]
        movl      %edi, blank_args_size(%rip)                   #207.7
        movslq    %edi, %rdi                                    #207.7
..___tag_value_main.189:
#       op_malloc(size_t)
        call      op_malloc                                     #207.7
..___tag_value_main.190:
                                # LOE rax rbx r12 r13 r14 r15b
..B1.170:                       # Preds ..B1.169
                                # Execution count [1.75e+00]
        movq      %rax, blank_args(%rip)                        #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.171:                       # Preds ..B1.167 ..B1.168 ..B1.170
                                # Execution count [1.11e+01]
        cmpl      $0, 3188(%rsp)                                #207.7
        jne       ..B1.175      # Prob 50%                      #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.172:                       # Preds ..B1.171
                                # Execution count [5.56e+00]
        movl      3136(%rsp), %edi                              #207.7
        cmpl      blank_args_size(%rip), %edi                   #207.7
        jle       ..B1.175      # Prob 68%                      #207.7
                                # LOE rbx r12 r13 r14 edi r15b
..B1.173:                       # Preds ..B1.172
                                # Execution count [1.75e+00]
        movl      %edi, blank_args_size(%rip)                   #207.7
        movslq    %edi, %rdi                                    #207.7
..___tag_value_main.191:
#       op_malloc(size_t)
        call      op_malloc                                     #207.7
..___tag_value_main.192:
                                # LOE rax rbx r12 r13 r14 r15b
..B1.174:                       # Preds ..B1.173
                                # Execution count [1.75e+00]
        movq      %rax, blank_args(%rip)                        #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.175:                       # Preds ..B1.172 ..B1.171 ..B1.174
                                # Execution count [1.11e+01]
        cmpl      $0, 3284(%rsp)                                #207.7
        jne       ..B1.179      # Prob 50%                      #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.176:                       # Preds ..B1.175
                                # Execution count [5.56e+00]
        movl      3232(%rsp), %edi                              #207.7
        cmpl      blank_args_size(%rip), %edi                   #207.7
        jle       ..B1.179      # Prob 68%                      #207.7
                                # LOE rbx r12 r13 r14 edi r15b
..B1.177:                       # Preds ..B1.176
                                # Execution count [1.75e+00]
        movl      %edi, blank_args_size(%rip)                   #207.7
        movslq    %edi, %rdi                                    #207.7
..___tag_value_main.193:
#       op_malloc(size_t)
        call      op_malloc                                     #207.7
..___tag_value_main.194:
                                # LOE rax rbx r12 r13 r14 r15b
..B1.178:                       # Preds ..B1.177
                                # Execution count [1.75e+00]
        movq      %rax, blank_args(%rip)                        #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.179:                       # Preds ..B1.175 ..B1.176 ..B1.178
                                # Execution count [1.11e+01]
        cmpl      $0, 3380(%rsp)                                #207.7
        jne       ..B1.183      # Prob 50%                      #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.180:                       # Preds ..B1.179
                                # Execution count [5.56e+00]
        movl      3328(%rsp), %edi                              #207.7
        cmpl      blank_args_size(%rip), %edi                   #207.7
        jle       ..B1.183      # Prob 68%                      #207.7
                                # LOE rbx r12 r13 r14 edi r15b
..B1.181:                       # Preds ..B1.180
                                # Execution count [1.75e+00]
        movl      %edi, blank_args_size(%rip)                   #207.7
        movslq    %edi, %rdi                                    #207.7
..___tag_value_main.195:
#       op_malloc(size_t)
        call      op_malloc                                     #207.7
..___tag_value_main.196:
                                # LOE rax rbx r12 r13 r14 r15b
..B1.182:                       # Preds ..B1.181
                                # Execution count [1.75e+00]
        movq      %rax, blank_args(%rip)                        #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.183:                       # Preds ..B1.179 ..B1.180 ..B1.182
                                # Execution count [1.11e+01]
        cmpl      $0, 3476(%rsp)                                #207.7
        jne       ..B1.187      # Prob 50%                      #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.184:                       # Preds ..B1.183
                                # Execution count [5.56e+00]
        movl      3424(%rsp), %edi                              #207.7
        cmpl      blank_args_size(%rip), %edi                   #207.7
        jle       ..B1.187      # Prob 68%                      #207.7
                                # LOE rbx r12 r13 r14 edi r15b
..B1.185:                       # Preds ..B1.184
                                # Execution count [1.75e+00]
        movl      %edi, blank_args_size(%rip)                   #207.7
        movslq    %edi, %rdi                                    #207.7
..___tag_value_main.197:
#       op_malloc(size_t)
        call      op_malloc                                     #207.7
..___tag_value_main.198:
                                # LOE rax rbx r12 r13 r14 r15b
..B1.186:                       # Preds ..B1.185
                                # Execution count [1.75e+00]
        movq      %rax, blank_args(%rip)                        #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.187:                       # Preds ..B1.183 ..B1.184 ..B1.186
                                # Execution count [1.11e+01]
        movl      $0, 4720(%rsp)                                #207.7
        cmpl      $0, OP_diags(%rip)                            #207.7
        jle       ..B1.195      # Prob 40%                      #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.188:                       # Preds ..B1.187
                                # Execution count [5.97e+00]
        xorl      %esi, %esi                                    #207.7
        xorl      %eax, %eax                                    #207.7
        movq      %r13, 4496(%rsp)                              #207.7[spill]
        movq      %rax, %rbx                                    #207.7
        movq      4696(%rsp), %r12                              #207.7[spill]
        movl      %esi, %r13d                                   #207.7
                                # LOE rbx r12 r14 r13d r15b
..B1.189:                       # Preds ..B1.190 ..B1.188
                                # Execution count [3.32e+01]
        addq      $-96, %rsp                                    #207.7
        movq      %r12, %rdi                                    #207.7
        movq      %rsp, %r8                                     #207.7
        movl      %r13d, %esi                                   #207.7
        movups    3104(%rsp,%rbx), %xmm0                        #207.7
        lea       4816(%rsp), %rdx                              #207.7
        movups    3120(%rsp,%rbx), %xmm1                        #207.7
        movl      $.L_2__STRING.42, %ecx                        #207.7
        movups    3136(%rsp,%rbx), %xmm2                        #207.7
        movups    3152(%rsp,%rbx), %xmm3                        #207.7
        movups    3168(%rsp,%rbx), %xmm4                        #207.7
        movups    3184(%rsp,%rbx), %xmm5                        #207.7
        movups    %xmm0, (%r8)                                  #207.7
        movups    %xmm1, 16(%r8)                                #207.7
        movups    %xmm2, 32(%r8)                                #207.7
        movups    %xmm3, 48(%r8)                                #207.7
        movups    %xmm4, 64(%r8)                                #207.7
        movups    %xmm5, 80(%r8)                                #207.7
..___tag_value_main.199:
#       op_arg_check(op_set, int, op_arg, int *, const char *)
        call      op_arg_check                                  #207.7
..___tag_value_main.200:
                                # LOE rbx r12 r14 r13d r15b
..B1.405:                       # Preds ..B1.189
                                # Execution count [3.32e+01]
        addq      $96, %rsp                                     #207.7
                                # LOE rbx r12 r14 r13d r15b
..B1.190:                       # Preds ..B1.405
                                # Execution count [3.32e+01]
        incl      %r13d                                         #207.7
        addq      $96, %rbx                                     #207.7
        cmpl      $5, %r13d                                     #207.7
        jl        ..B1.189      # Prob 82%                      #207.7
                                # LOE rbx r12 r14 r13d r15b
..B1.191:                       # Preds ..B1.190
                                # Execution count [5.97e+00]
        movq      4496(%rsp), %r13                              #[spill]
        movq      4480(%rsp), %r12                              #[spill]
        movq      4488(%rsp), %rbx                              #[spill]
        cmpl      $2, OP_diags(%rip)                            #207.7
        jle       ..B1.195      # Prob 50%                      #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.192:                       # Preds ..B1.191
                                # Execution count [5.56e+00]
        cmpl      $0, 4720(%rsp)                                #207.7
        jne       ..B1.194      # Prob 50%                      #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.193:                       # Preds ..B1.192
                                # Execution count [2.78e+00]
        movl      $.L_2__STRING.3, %edi                         #207.7
        movl      $.L_2__STRING.42, %esi                        #207.7
        xorl      %eax, %eax                                    #207.7
..___tag_value_main.201:
#       printf(const char *, ...)
        call      printf                                        #207.7
..___tag_value_main.202:
        jmp       ..B1.195      # Prob 100%                     #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.194:                       # Preds ..B1.192
                                # Execution count [2.78e+00]
        movl      $.L_2__STRING.4, %edi                         #207.7
        movl      $.L_2__STRING.42, %esi                        #207.7
        xorl      %eax, %eax                                    #207.7
..___tag_value_main.203:
#       printf(const char *, ...)
        call      printf                                        #207.7
..___tag_value_main.204:
                                # LOE rbx r12 r13 r14 r15b
..B1.195:                       # Preds ..B1.193 ..B1.194 ..B1.187 ..B1.191
                                # Execution count [1.11e+01]
        lea       4520(%rsp), %rdi                              #207.7
        lea       4528(%rsp), %rsi                              #207.7
..___tag_value_main.205:
#       op_timers_core(double *, double *)
        call      op_timers_core                                #207.7
..___tag_value_main.206:
                                # LOE rbx r12 r13 r14 r15b
..B1.196:                       # Preds ..B1.195
                                # Execution count [1.11e+01]
        movl      $5, %esi                                      #207.7
        lea       3008(%rsp), %rdx                              #207.7
        movq      1688(%rdx), %rdi                              #207.7[spill]
..___tag_value_main.207:
#       op_mpi_halo_exchanges(op_set, int, op_arg *)
        call      op_mpi_halo_exchanges                         #207.7
..___tag_value_main.208:
                                # LOE rbx r12 r13 r14 eax r15b
..B1.406:                       # Preds ..B1.196
                                # Execution count [1.11e+01]
        movl      %eax, %r9d                                    #207.7
                                # LOE rbx r12 r13 r14 r9d r15b
..B1.197:                       # Preds ..B1.406
                                # Execution count [1.11e+01]
        movq      4696(%rsp), %r8                               #207.7[spill]
        xorl      %edx, %edx                                    #207.7
        xorl      %eax, %eax                                    #207.7
        movl      16(%r8), %r8d                                 #207.7
        testl     %r9d, %r9d                                    #207.7
        jle       ..B1.333      # Prob 10%                      #207.7
                                # LOE rbx r12 r13 r14 eax edx r8d r9d r15b
..B1.198:                       # Preds ..B1.197
                                # Execution count [1.00e+01]
        movl      %r8d, 4704(%rsp)                              #[spill]
        movl      %eax, %ebx                                    #
        movl      %r9d, 4712(%rsp)                              #[spill]
        movl      %edx, %r12d                                   #
        movb      %r15b, 4504(%rsp)                             #[spill]
        movq      %r14, 4456(%rsp)                              #[spill]
        movq      %r13, 4496(%rsp)                              #[spill]
                                # LOE ebx r12d
..B1.199:                       # Preds ..B1.293 ..B1.198
                                # Execution count [5.56e+01]
        cmpl      4704(%rsp), %ebx                              #207.7[spill]
        jne       ..B1.202      # Prob 78%                      #207.7
                                # LOE ebx r12d
..B1.200:                       # Preds ..B1.199
                                # Execution count [1.22e+01]
        movl      $5, %edi                                      #207.7
        lea       3008(%rsp), %rsi                              #207.7
..___tag_value_main.209:
#       op_mpi_wait_all(int, op_arg *)
        call      op_mpi_wait_all                               #207.7
..___tag_value_main.210:
                                # LOE ebx r12d
..B1.201:                       # Preds ..B1.200
                                # Execution count [1.22e+01]
        movq      4696(%rsp), %rcx                              #207.7[spill]
        movl      16(%rcx), %esi                                #207.7
        movl      %esi, 4704(%rsp)                              #207.7[spill]
                                # LOE ebx r12d
..B1.202:                       # Preds ..B1.201 ..B1.199
                                # Execution count [5.56e+01]
        movq      4696(%rsp), %rcx                              #207.7[spill]
        movl      $1, %esi                                      #207.7
        cmpl      4(%rcx), %ebx                                 #207.7
        cmove     %esi, %r12d                                   #207.7
        cmpl      $-1, 3036(%rsp)                               #207.7
        jge       ..B1.210      # Prob 50%                      #207.7
                                # LOE ebx r12d
..B1.203:                       # Preds ..B1.202
                                # Execution count [2.78e+01]
        movl      3036(%rsp), %r15d                             #207.7
        movl      %r15d, %esi                                   #207.7
        negl      %esi                                          #207.7
        movq      3024(%rsp), %r9                               #207.7
        movq      3048(%rsp), %r8                               #207.7
        movl      3040(%rsp), %ecx                              #207.7
        movq      4416(%rsp), %rdi                              #207.7
        testl     %esi, %esi                                    #207.7
        jle       ..B1.220      # Prob 50%                      #207.7
                                # LOE rdi r8 r9 ecx ebx esi r12d r15d
..B1.204:                       # Preds ..B1.203
                                # Execution count [2.78e+01]
        movl      %esi, %r11d                                   #207.7
        movl      $1, %r10d                                     #207.7
        shrl      $31, %r11d                                    #207.7
        xorl      %r14d, %r14d                                  #207.7
        subl      %r15d, %r11d                                  #207.7
        sarl      $1, %r11d                                     #207.7
        movl      24(%r9), %r13d                                #207.7
        testl     %r11d, %r11d                                  #207.7
        jbe       ..B1.208      # Prob 10%                      #207.7
                                # LOE rdi r8 r9 ecx ebx esi r10d r11d r12d r13d r14d
..B1.205:                       # Preds ..B1.204
                                # Execution count [2.50e+01]
        movl      %ebx, %r10d                                   #207.7
        imull     %r13d, %r10d                                  #207.7
        .align    16,0x90
                                # LOE rdi r8 r9 ecx ebx esi r10d r11d r12d r13d r14d
..B1.206:                       # Preds ..B1.206 ..B1.205
                                # Execution count [6.94e+01]
        movq      32(%r9), %r15                                 #207.7
        lea       (%r10,%r14,2), %eax                           #207.7
        movslq    %eax, %rax                                    #207.7
        lea       (%r14,%r14), %edx                             #207.7
        movslq    %edx, %rdx                                    #207.7
        incl      %r14d                                         #207.7
        movl      (%r15,%rax,4), %r15d                          #207.7
        imull     %ecx, %r15d                                   #207.7
        movslq    %r15d, %r15                                   #207.7
        addq      %r8, %r15                                     #207.7
        movq      %r15, (%rdi,%rdx,8)                           #207.7
        movq      32(%r9), %r15                                 #207.7
        movl      4(%r15,%rax,4), %r15d                         #207.7
        imull     %ecx, %r15d                                   #207.7
        movslq    %r15d, %r15                                   #207.7
        addq      %r8, %r15                                     #207.7
        movq      %r15, 8(%rdi,%rdx,8)                          #207.7
        cmpl      %r11d, %r14d                                  #207.7
        jb        ..B1.206      # Prob 64%                      #207.7
                                # LOE rdi r8 r9 ecx ebx esi r10d r11d r12d r13d r14d
..B1.207:                       # Preds ..B1.206
                                # Execution count [2.50e+01]
        lea       1(%r14,%r14), %r10d                           #207.7
                                # LOE rdi r8 r9 ecx ebx esi r10d r12d r13d
..B1.208:                       # Preds ..B1.207 ..B1.204
                                # Execution count [2.78e+01]
        lea       -1(%r10), %r11d                               #207.7
        cmpl      %esi, %r11d                                   #207.7
        jae       ..B1.220      # Prob 10%                      #207.7
                                # LOE rdi r8 r9 ecx ebx r10d r12d r13d
..B1.209:                       # Preds ..B1.208
                                # Execution count [2.50e+01]
        imull     %ebx, %r13d                                   #207.7
        movslq    %r10d, %r10                                   #207.7
        movslq    %r13d, %r13                                   #207.7
        addq      %r10, %r13                                    #207.7
        movq      32(%r9), %r9                                  #207.7
        movl      -4(%r9,%r13,4), %esi                          #207.7
        imull     %esi, %ecx                                    #207.7
        movslq    %ecx, %rcx                                    #207.7
        addq      %rcx, %r8                                     #207.7
        movq      %r8, -8(%rdi,%r10,8)                          #207.7
        jmp       ..B1.220      # Prob 100%                     #207.7
                                # LOE rdi ebx r12d
..B1.210:                       # Preds ..B1.202
                                # Execution count [2.78e+01]
        movups    3008(%rsp), %xmm0                             #207.7
        movups    3024(%rsp), %xmm1                             #207.7
        movups    3040(%rsp), %xmm2                             #207.7
        movups    3056(%rsp), %xmm3                             #207.7
        movups    3072(%rsp), %xmm4                             #207.7
        movups    3088(%rsp), %xmm5                             #207.7
        movq      3048(%rsp), %rdi                              #207.7
        movups    %xmm0, 3888(%rsp)                             #207.7
        movups    %xmm1, 3904(%rsp)                             #207.7
        movups    %xmm2, 3920(%rsp)                             #207.7
        movups    %xmm3, 3936(%rsp)                             #207.7
        movups    %xmm4, 3952(%rsp)                             #207.7
        movups    %xmm5, 3968(%rsp)                             #207.7
        cmpl      $0, 3092(%rsp)                                #207.7
        jne       ..B1.216      # Prob 50%                      #207.7
                                # LOE rdi ebx r12d
..B1.211:                       # Preds ..B1.210
                                # Execution count [1.39e+01]
        testl     %r12d, %r12d                                  #207.7
        je        ..B1.214      # Prob 50%                      #207.7
                                # LOE rdi ebx r12d
..B1.212:                       # Preds ..B1.211
                                # Execution count [6.94e+00]
        cmpl      $0, 3088(%rsp)                                #207.7
        je        ..B1.214      # Prob 50%                      #207.7
                                # LOE rdi ebx r12d
..B1.213:                       # Preds ..B1.212
                                # Execution count [3.47e+00]
        movq      blank_args(%rip), %rdi                        #207.7
        movq      %rdi, 4416(%rsp)                              #207.7
        jmp       ..B1.220      # Prob 100%                     #207.7
                                # LOE rdi ebx r12d
..B1.214:                       # Preds ..B1.211 ..B1.212
                                # Execution count [3.47e+00]
        movq      %rdi, 4416(%rsp)                              #207.7
        jmp       ..B1.220      # Prob 100%                     #207.7
                                # LOE rdi ebx r12d
..B1.216:                       # Preds ..B1.210
                                # Execution count [1.39e+01]
        movq      3904(%rsp), %rsi                              #207.7
        testq     %rsi, %rsi                                    #207.7
        je        ..B1.218      # Prob 12%                      #207.7
                                # LOE rsi rdi ebx r12d
..B1.217:                       # Preds ..B1.216
                                # Execution count [1.22e+01]
        cmpl      $0, 3980(%rsp)                                #207.7
        jne       ..B1.219      # Prob 50%                      #207.7
                                # LOE rsi rdi ebx r12d
..B1.218:                       # Preds ..B1.216 ..B1.217
                                # Execution count [7.78e+00]
        movslq    %ebx, %rbx                                    #207.7
        movslq    3920(%rsp), %rcx                              #207.7
        imulq     %rbx, %rcx                                    #207.7
        addq      %rcx, %rdi                                    #207.7
        movq      %rdi, 4416(%rsp)                              #207.7
        jmp       ..B1.220      # Prob 100%                     #207.7
                                # LOE rdi ebx r12d
..B1.219:                       # Preds ..B1.217
                                # Execution count [6.11e+00]
        movl      %ebx, %ecx                                    #207.7
        imull     24(%rsi), %ecx                                #207.7
        addl      3916(%rsp), %ecx                              #207.7
        movslq    %ecx, %rcx                                    #207.7
        movq      32(%rsi), %rsi                                #207.7
        movl      3920(%rsp), %r8d                              #207.7
        imull     (%rsi,%rcx,4), %r8d                           #207.7
        movslq    %r8d, %r8                                     #207.7
        addq      %r8, %rdi                                     #207.7
        movq      %rdi, 4416(%rsp)                              #207.7
                                # LOE rdi ebx r12d
..B1.220:                       # Preds ..B1.209 ..B1.208 ..B1.203 ..B1.213 ..B1.214
                                #       ..B1.218 ..B1.219
                                # Execution count [5.56e+01]
        cmpl      $-1, 3132(%rsp)                               #207.7
        jge       ..B1.228      # Prob 50%                      #207.7
                                # LOE rdi ebx r12d
..B1.221:                       # Preds ..B1.220
                                # Execution count [2.78e+01]
        movl      3132(%rsp), %eax                              #207.7
        movl      %eax, %r8d                                    #207.7
        negl      %r8d                                          #207.7
        movq      3120(%rsp), %r9                               #207.7
        movq      3144(%rsp), %r15                              #207.7
        movl      3136(%rsp), %ecx                              #207.7
        movq      4424(%rsp), %rsi                              #207.7
        testl     %r8d, %r8d                                    #207.7
        jle       ..B1.238      # Prob 50%                      #207.7
                                # LOE rsi rdi r9 r15 eax ecx ebx r8d r12d
..B1.222:                       # Preds ..B1.221
                                # Execution count [2.78e+01]
        movl      %r8d, %r13d                                   #207.7
        movl      $1, %r10d                                     #207.7
        shrl      $31, %r13d                                    #207.7
        xorl      %r11d, %r11d                                  #207.7
        subl      %eax, %r13d                                   #207.7
        sarl      $1, %r13d                                     #207.7
        movl      24(%r9), %r14d                                #207.7
        testl     %r13d, %r13d                                  #207.7
        jbe       ..B1.226      # Prob 10%                      #207.7
                                # LOE rsi rdi r9 r15 ecx ebx r8d r10d r11d r12d r13d r14d
..B1.223:                       # Preds ..B1.222
                                # Execution count [2.50e+01]
        movl      %ebx, %r10d                                   #207.7
        imull     %r14d, %r10d                                  #207.7
        movl      %r12d, 4632(%rsp)                             #207.7[spill]
        .align    16,0x90
                                # LOE rsi rdi r9 r15 ecx ebx r8d r10d r11d r13d r14d
..B1.224:                       # Preds ..B1.224 ..B1.223
                                # Execution count [6.94e+01]
        movq      32(%r9), %r12                                 #207.7
        lea       (%r10,%r11,2), %eax                           #207.7
        movslq    %eax, %rax                                    #207.7
        lea       (%r11,%r11), %edx                             #207.7
        movslq    %edx, %rdx                                    #207.7
        incl      %r11d                                         #207.7
        movl      (%r12,%rax,4), %r12d                          #207.7
        imull     %ecx, %r12d                                   #207.7
        movslq    %r12d, %r12                                   #207.7
        addq      %r15, %r12                                    #207.7
        movq      %r12, (%rsi,%rdx,8)                           #207.7
        movq      32(%r9), %r12                                 #207.7
        movl      4(%r12,%rax,4), %r12d                         #207.7
        imull     %ecx, %r12d                                   #207.7
        movslq    %r12d, %r12                                   #207.7
        addq      %r15, %r12                                    #207.7
        movq      %r12, 8(%rsi,%rdx,8)                          #207.7
        cmpl      %r13d, %r11d                                  #207.7
        jb        ..B1.224      # Prob 64%                      #207.7
                                # LOE rsi rdi r9 r15 ecx ebx r8d r10d r11d r13d r14d
..B1.225:                       # Preds ..B1.224
                                # Execution count [2.50e+01]
        movl      4632(%rsp), %r12d                             #[spill]
        lea       1(%r11,%r11), %r10d                           #207.7
                                # LOE rsi rdi r9 r15 ecx ebx r8d r10d r12d r14d
..B1.226:                       # Preds ..B1.225 ..B1.222
                                # Execution count [2.78e+01]
        lea       -1(%r10), %r11d                               #207.7
        cmpl      %r8d, %r11d                                   #207.7
        jae       ..B1.238      # Prob 10%                      #207.7
                                # LOE rsi rdi r9 r15 ecx ebx r10d r12d r14d
..B1.227:                       # Preds ..B1.226
                                # Execution count [2.50e+01]
        imull     %ebx, %r14d                                   #207.7
        movslq    %r10d, %r10                                   #207.7
        movslq    %r14d, %r14                                   #207.7
        addq      %r10, %r14                                    #207.7
        movq      32(%r9), %r9                                  #207.7
        movl      -4(%r9,%r14,4), %r8d                          #207.7
        imull     %r8d, %ecx                                    #207.7
        movslq    %ecx, %rcx                                    #207.7
        addq      %rcx, %r15                                    #207.7
        movq      %r15, -8(%rsi,%r10,8)                         #207.7
        jmp       ..B1.238      # Prob 100%                     #207.7
                                # LOE rsi rdi ebx r12d
..B1.228:                       # Preds ..B1.220
                                # Execution count [2.78e+01]
        movups    3104(%rsp), %xmm0                             #207.7
        movups    3120(%rsp), %xmm1                             #207.7
        movups    3136(%rsp), %xmm2                             #207.7
        movups    3152(%rsp), %xmm3                             #207.7
        movups    3168(%rsp), %xmm4                             #207.7
        movups    3184(%rsp), %xmm5                             #207.7
        movq      3144(%rsp), %rsi                              #207.7
        movups    %xmm0, 3984(%rsp)                             #207.7
        movups    %xmm1, 4000(%rsp)                             #207.7
        movups    %xmm2, 4016(%rsp)                             #207.7
        movups    %xmm3, 4032(%rsp)                             #207.7
        movups    %xmm4, 4048(%rsp)                             #207.7
        movups    %xmm5, 4064(%rsp)                             #207.7
        cmpl      $0, 3188(%rsp)                                #207.7
        jne       ..B1.234      # Prob 50%                      #207.7
                                # LOE rsi rdi ebx r12d
..B1.229:                       # Preds ..B1.228
                                # Execution count [1.39e+01]
        testl     %r12d, %r12d                                  #207.7
        je        ..B1.232      # Prob 50%                      #207.7
                                # LOE rsi rdi ebx r12d
..B1.230:                       # Preds ..B1.229
                                # Execution count [6.94e+00]
        cmpl      $0, 3184(%rsp)                                #207.7
        je        ..B1.232      # Prob 50%                      #207.7
                                # LOE rsi rdi ebx r12d
..B1.231:                       # Preds ..B1.230
                                # Execution count [3.47e+00]
        movq      blank_args(%rip), %rsi                        #207.7
        movq      %rsi, 4424(%rsp)                              #207.7
        jmp       ..B1.238      # Prob 100%                     #207.7
                                # LOE rsi rdi ebx r12d
..B1.232:                       # Preds ..B1.229 ..B1.230
                                # Execution count [3.47e+00]
        movq      %rsi, 4424(%rsp)                              #207.7
        jmp       ..B1.238      # Prob 100%                     #207.7
                                # LOE rsi rdi ebx r12d
..B1.234:                       # Preds ..B1.228
                                # Execution count [1.39e+01]
        movq      4000(%rsp), %r8                               #207.7
        testq     %r8, %r8                                      #207.7
        je        ..B1.236      # Prob 12%                      #207.7
                                # LOE rsi rdi r8 ebx r12d
..B1.235:                       # Preds ..B1.234
                                # Execution count [1.22e+01]
        cmpl      $0, 4076(%rsp)                                #207.7
        jne       ..B1.237      # Prob 50%                      #207.7
                                # LOE rsi rdi r8 ebx r12d
..B1.236:                       # Preds ..B1.234 ..B1.235
                                # Execution count [7.78e+00]
        movslq    %ebx, %rbx                                    #207.7
        movslq    4016(%rsp), %rcx                              #207.7
        imulq     %rbx, %rcx                                    #207.7
        addq      %rcx, %rsi                                    #207.7
        movq      %rsi, 4424(%rsp)                              #207.7
        jmp       ..B1.238      # Prob 100%                     #207.7
                                # LOE rsi rdi ebx r12d
..B1.237:                       # Preds ..B1.235
                                # Execution count [6.11e+00]
        movl      %ebx, %ecx                                    #207.7
        imull     24(%r8), %ecx                                 #207.7
        addl      4012(%rsp), %ecx                              #207.7
        movslq    %ecx, %rcx                                    #207.7
        movq      32(%r8), %r8                                  #207.7
        movl      4016(%rsp), %r9d                              #207.7
        imull     (%r8,%rcx,4), %r9d                            #207.7
        movslq    %r9d, %r9                                     #207.7
        addq      %r9, %rsi                                     #207.7
        movq      %rsi, 4424(%rsp)                              #207.7
                                # LOE rsi rdi ebx r12d
..B1.238:                       # Preds ..B1.227 ..B1.226 ..B1.221 ..B1.231 ..B1.232
                                #       ..B1.236 ..B1.237
                                # Execution count [5.56e+01]
        cmpl      $-1, 3228(%rsp)                               #207.7
        jge       ..B1.246      # Prob 50%                      #207.7
                                # LOE rsi rdi ebx r12d
..B1.239:                       # Preds ..B1.238
                                # Execution count [2.78e+01]
        movl      3228(%rsp), %r15d                             #207.7
        movl      %r15d, %r13d                                  #207.7
        negl      %r13d                                         #207.7
        movq      3216(%rsp), %r9                               #207.7
        movq      3240(%rsp), %r10                              #207.7
        movl      3232(%rsp), %r11d                             #207.7
        movq      4432(%rsp), %rdx                              #207.7
        testl     %r13d, %r13d                                  #207.7
        jle       ..B1.256      # Prob 50%                      #207.7
                                # LOE rdx rsi rdi r9 r10 ebx r11d r12d r13d r15d
..B1.240:                       # Preds ..B1.239
                                # Execution count [2.78e+01]
        movl      %r13d, %ecx                                   #207.7
        movl      $1, %eax                                      #207.7
        shrl      $31, %ecx                                     #207.7
        xorl      %r8d, %r8d                                    #207.7
        subl      %r15d, %ecx                                   #207.7
        sarl      $1, %ecx                                      #207.7
        movl      24(%r9), %r14d                                #207.7
        testl     %ecx, %ecx                                    #207.7
        jbe       ..B1.244      # Prob 10%                      #207.7
                                # LOE rdx rsi rdi r9 r10 eax ecx ebx r8d r11d r12d r13d r14d
..B1.241:                       # Preds ..B1.240
                                # Execution count [2.50e+01]
        movl      %ebx, %eax                                    #207.7
        imull     %r14d, %eax                                   #207.7
        movl      %ebx, 4640(%rsp)                              #207.7[spill]
        movl      %r12d, 4632(%rsp)                             #207.7[spill]
        .align    16,0x90
                                # LOE rdx rsi rdi r9 r10 eax ecx r8d r11d r13d r14d
..B1.242:                       # Preds ..B1.242 ..B1.241
                                # Execution count [6.94e+01]
        movq      32(%r9), %r15                                 #207.7
        lea       (%rax,%r8,2), %ebx                            #207.7
        movslq    %ebx, %rbx                                    #207.7
        lea       (%r8,%r8), %r12d                              #207.7
        movslq    %r12d, %r12                                   #207.7
        incl      %r8d                                          #207.7
        movl      (%r15,%rbx,4), %r15d                          #207.7
        imull     %r11d, %r15d                                  #207.7
        movslq    %r15d, %r15                                   #207.7
        addq      %r10, %r15                                    #207.7
        movq      %r15, (%rdx,%r12,8)                           #207.7
        movq      32(%r9), %r15                                 #207.7
        movl      4(%r15,%rbx,4), %ebx                          #207.7
        imull     %r11d, %ebx                                   #207.7
        movslq    %ebx, %rbx                                    #207.7
        addq      %r10, %rbx                                    #207.7
        movq      %rbx, 8(%rdx,%r12,8)                          #207.7
        cmpl      %ecx, %r8d                                    #207.7
        jb        ..B1.242      # Prob 64%                      #207.7
                                # LOE rdx rsi rdi r9 r10 eax ecx r8d r11d r13d r14d
..B1.243:                       # Preds ..B1.242
                                # Execution count [2.50e+01]
        movl      4640(%rsp), %ebx                              #[spill]
        lea       1(%r8,%r8), %eax                              #207.7
        movl      4632(%rsp), %r12d                             #[spill]
                                # LOE rdx rsi rdi r9 r10 eax ebx r11d r12d r13d r14d
..B1.244:                       # Preds ..B1.243 ..B1.240
                                # Execution count [2.78e+01]
        lea       -1(%rax), %ecx                                #207.7
        cmpl      %r13d, %ecx                                   #207.7
        jae       ..B1.256      # Prob 10%                      #207.7
                                # LOE rdx rsi rdi r9 r10 eax ebx r11d r12d r14d
..B1.245:                       # Preds ..B1.244
                                # Execution count [2.50e+01]
        imull     %ebx, %r14d                                   #207.7
        movslq    %eax, %rax                                    #207.7
        movslq    %r14d, %r14                                   #207.7
        addq      %rax, %r14                                    #207.7
        movq      32(%r9), %r9                                  #207.7
        movl      -4(%r9,%r14,4), %ecx                          #207.7
        imull     %ecx, %r11d                                   #207.7
        movslq    %r11d, %r11                                   #207.7
        addq      %r11, %r10                                    #207.7
        movq      %r10, -8(%rdx,%rax,8)                         #207.7
        jmp       ..B1.256      # Prob 100%                     #207.7
                                # LOE rdx rsi rdi ebx r12d
..B1.246:                       # Preds ..B1.238
                                # Execution count [2.78e+01]
        movups    3200(%rsp), %xmm0                             #207.7
        movups    3216(%rsp), %xmm1                             #207.7
        movups    3232(%rsp), %xmm2                             #207.7
        movups    3248(%rsp), %xmm3                             #207.7
        movups    3264(%rsp), %xmm4                             #207.7
        movups    3280(%rsp), %xmm5                             #207.7
        movq      3240(%rsp), %rdx                              #207.7
        movups    %xmm0, 4080(%rsp)                             #207.7
        movups    %xmm1, 4096(%rsp)                             #207.7
        movups    %xmm2, 4112(%rsp)                             #207.7
        movups    %xmm3, 4128(%rsp)                             #207.7
        movups    %xmm4, 4144(%rsp)                             #207.7
        movups    %xmm5, 4160(%rsp)                             #207.7
        cmpl      $0, 3284(%rsp)                                #207.7
        jne       ..B1.252      # Prob 50%                      #207.7
                                # LOE rdx rsi rdi ebx r12d
..B1.247:                       # Preds ..B1.246
                                # Execution count [1.39e+01]
        testl     %r12d, %r12d                                  #207.7
        je        ..B1.250      # Prob 50%                      #207.7
                                # LOE rdx rsi rdi ebx r12d
..B1.248:                       # Preds ..B1.247
                                # Execution count [6.94e+00]
        cmpl      $0, 3280(%rsp)                                #207.7
        je        ..B1.250      # Prob 50%                      #207.7
                                # LOE rdx rsi rdi ebx r12d
..B1.249:                       # Preds ..B1.248
                                # Execution count [3.47e+00]
        movq      blank_args(%rip), %rdx                        #207.7
        movq      %rdx, 4432(%rsp)                              #207.7
        jmp       ..B1.256      # Prob 100%                     #207.7
                                # LOE rdx rsi rdi ebx r12d
..B1.250:                       # Preds ..B1.247 ..B1.248
                                # Execution count [3.47e+00]
        movq      %rdx, 4432(%rsp)                              #207.7
        jmp       ..B1.256      # Prob 100%                     #207.7
                                # LOE rdx rsi rdi ebx r12d
..B1.252:                       # Preds ..B1.246
                                # Execution count [1.39e+01]
        movq      4096(%rsp), %r8                               #207.7
        testq     %r8, %r8                                      #207.7
        je        ..B1.254      # Prob 12%                      #207.7
                                # LOE rdx rsi rdi r8 ebx r12d
..B1.253:                       # Preds ..B1.252
                                # Execution count [1.22e+01]
        cmpl      $0, 4172(%rsp)                                #207.7
        jne       ..B1.255      # Prob 50%                      #207.7
                                # LOE rdx rsi rdi r8 ebx r12d
..B1.254:                       # Preds ..B1.252 ..B1.253
                                # Execution count [7.78e+00]
        movslq    %ebx, %rbx                                    #207.7
        movslq    4112(%rsp), %rcx                              #207.7
        imulq     %rbx, %rcx                                    #207.7
        addq      %rcx, %rdx                                    #207.7
        movq      %rdx, 4432(%rsp)                              #207.7
        jmp       ..B1.256      # Prob 100%                     #207.7
                                # LOE rdx rsi rdi ebx r12d
..B1.255:                       # Preds ..B1.253
                                # Execution count [6.11e+00]
        movl      %ebx, %ecx                                    #207.7
        imull     24(%r8), %ecx                                 #207.7
        addl      4108(%rsp), %ecx                              #207.7
        movslq    %ecx, %rcx                                    #207.7
        movq      32(%r8), %r8                                  #207.7
        movl      4112(%rsp), %r9d                              #207.7
        imull     (%r8,%rcx,4), %r9d                            #207.7
        movslq    %r9d, %r9                                     #207.7
        addq      %r9, %rdx                                     #207.7
        movq      %rdx, 4432(%rsp)                              #207.7
                                # LOE rdx rsi rdi ebx r12d
..B1.256:                       # Preds ..B1.245 ..B1.244 ..B1.239 ..B1.249 ..B1.250
                                #       ..B1.254 ..B1.255
                                # Execution count [5.56e+01]
        cmpl      $-1, 3324(%rsp)                               #207.7
        jge       ..B1.264      # Prob 50%                      #207.7
                                # LOE rdx rsi rdi ebx r12d
..B1.257:                       # Preds ..B1.256
                                # Execution count [2.78e+01]
        movl      3324(%rsp), %r8d                              #207.7
        movl      %r8d, 4680(%rsp)                              #207.7[spill]
        negl      %r8d                                          #207.7
        movq      3312(%rsp), %r10                              #207.7
        movq      3336(%rsp), %r11                              #207.7
        movl      3328(%rsp), %r13d                             #207.7
        movq      4440(%rsp), %rcx                              #207.7
        testl     %r8d, %r8d                                    #207.7
        jle       ..B1.274      # Prob 50%                      #207.7
                                # LOE rdx rcx rsi rdi r10 r11 ebx r8d r12d r13d
..B1.258:                       # Preds ..B1.257
                                # Execution count [2.78e+01]
        movl      %r8d, %r15d                                   #207.7
        movl      $1, %eax                                      #207.7
        shrl      $31, %r15d                                    #207.7
        xorl      %r9d, %r9d                                    #207.7
        subl      4680(%rsp), %r15d                             #207.7[spill]
        sarl      $1, %r15d                                     #207.7
        movl      24(%r10), %r14d                               #207.7
        testl     %r15d, %r15d                                  #207.7
        jbe       ..B1.262      # Prob 10%                      #207.7
                                # LOE rdx rcx rsi rdi r10 r11 eax ebx r8d r9d r12d r13d r14d r15d
..B1.259:                       # Preds ..B1.258
                                # Execution count [2.50e+01]
        movl      %ebx, %eax                                    #207.7
        movl      %r12d, 4632(%rsp)                             #207.7[spill]
        imull     %r14d, %eax                                   #207.7
        movq      %rdi, 4648(%rsp)                              #207.7[spill]
        movl      %ebx, 4640(%rsp)                              #207.7[spill]
        movl      %r15d, %r12d                                  #207.7
        .align    16,0x90
                                # LOE rdx rcx rsi r10 r11 eax r8d r9d r12d r13d r14d
..B1.260:                       # Preds ..B1.260 ..B1.259
                                # Execution count [6.94e+01]
        movq      32(%r10), %r15                                #207.7
        lea       (%rax,%r9,2), %ebx                            #207.7
        movslq    %ebx, %rbx                                    #207.7
        lea       (%r9,%r9), %edi                               #207.7
        movslq    %edi, %rdi                                    #207.7
        incl      %r9d                                          #207.7
        movl      (%r15,%rbx,4), %r15d                          #207.7
        imull     %r13d, %r15d                                  #207.7
        movslq    %r15d, %r15                                   #207.7
        addq      %r11, %r15                                    #207.7
        movq      %r15, (%rcx,%rdi,8)                           #207.7
        movq      32(%r10), %r15                                #207.7
        movl      4(%r15,%rbx,4), %ebx                          #207.7
        imull     %r13d, %ebx                                   #207.7
        movslq    %ebx, %rbx                                    #207.7
        addq      %r11, %rbx                                    #207.7
        movq      %rbx, 8(%rcx,%rdi,8)                          #207.7
        cmpl      %r12d, %r9d                                   #207.7
        jb        ..B1.260      # Prob 64%                      #207.7
                                # LOE rdx rcx rsi r10 r11 eax r8d r9d r12d r13d r14d
..B1.261:                       # Preds ..B1.260
                                # Execution count [2.50e+01]
        movq      4648(%rsp), %rdi                              #[spill]
        lea       1(%r9,%r9), %eax                              #207.7
        movl      4640(%rsp), %ebx                              #[spill]
        movl      4632(%rsp), %r12d                             #[spill]
                                # LOE rdx rcx rsi rdi r10 r11 eax ebx r8d r12d r13d r14d
..B1.262:                       # Preds ..B1.261 ..B1.258
                                # Execution count [2.78e+01]
        lea       -1(%rax), %r9d                                #207.7
        cmpl      %r8d, %r9d                                    #207.7
        jae       ..B1.274      # Prob 10%                      #207.7
                                # LOE rdx rcx rsi rdi r10 r11 eax ebx r12d r13d r14d
..B1.263:                       # Preds ..B1.262
                                # Execution count [2.50e+01]
        imull     %ebx, %r14d                                   #207.7
        movslq    %eax, %rax                                    #207.7
        movslq    %r14d, %r14                                   #207.7
        addq      %rax, %r14                                    #207.7
        movq      32(%r10), %r10                                #207.7
        movl      -4(%r10,%r14,4), %r8d                         #207.7
        imull     %r8d, %r13d                                   #207.7
        movslq    %r13d, %r13                                   #207.7
        addq      %r13, %r11                                    #207.7
        movq      %r11, -8(%rcx,%rax,8)                         #207.7
        jmp       ..B1.274      # Prob 100%                     #207.7
                                # LOE rdx rcx rsi rdi ebx r12d
..B1.264:                       # Preds ..B1.256
                                # Execution count [2.78e+01]
        movups    3296(%rsp), %xmm0                             #207.7
        movups    3312(%rsp), %xmm1                             #207.7
        movups    3328(%rsp), %xmm2                             #207.7
        movups    3344(%rsp), %xmm3                             #207.7
        movups    3360(%rsp), %xmm4                             #207.7
        movups    3376(%rsp), %xmm5                             #207.7
        movq      3336(%rsp), %rcx                              #207.7
        movups    %xmm0, 4176(%rsp)                             #207.7
        movups    %xmm1, 4192(%rsp)                             #207.7
        movups    %xmm2, 4208(%rsp)                             #207.7
        movups    %xmm3, 4224(%rsp)                             #207.7
        movups    %xmm4, 4240(%rsp)                             #207.7
        movups    %xmm5, 4256(%rsp)                             #207.7
        cmpl      $0, 3380(%rsp)                                #207.7
        jne       ..B1.270      # Prob 50%                      #207.7
                                # LOE rdx rcx rsi rdi ebx r12d
..B1.265:                       # Preds ..B1.264
                                # Execution count [1.39e+01]
        testl     %r12d, %r12d                                  #207.7
        je        ..B1.268      # Prob 50%                      #207.7
                                # LOE rdx rcx rsi rdi ebx r12d
..B1.266:                       # Preds ..B1.265
                                # Execution count [6.94e+00]
        cmpl      $0, 3376(%rsp)                                #207.7
        je        ..B1.268      # Prob 50%                      #207.7
                                # LOE rdx rcx rsi rdi ebx r12d
..B1.267:                       # Preds ..B1.266
                                # Execution count [3.47e+00]
        movq      blank_args(%rip), %rcx                        #207.7
        movq      %rcx, 4440(%rsp)                              #207.7
        jmp       ..B1.274      # Prob 100%                     #207.7
                                # LOE rdx rcx rsi rdi ebx r12d
..B1.268:                       # Preds ..B1.265 ..B1.266
                                # Execution count [3.47e+00]
        movq      %rcx, 4440(%rsp)                              #207.7
        jmp       ..B1.274      # Prob 100%                     #207.7
                                # LOE rdx rcx rsi rdi ebx r12d
..B1.270:                       # Preds ..B1.264
                                # Execution count [1.39e+01]
        movq      4192(%rsp), %r9                               #207.7
        testq     %r9, %r9                                      #207.7
        je        ..B1.272      # Prob 12%                      #207.7
                                # LOE rdx rcx rsi rdi r9 ebx r12d
..B1.271:                       # Preds ..B1.270
                                # Execution count [1.22e+01]
        cmpl      $0, 4268(%rsp)                                #207.7
        jne       ..B1.273      # Prob 50%                      #207.7
                                # LOE rdx rcx rsi rdi r9 ebx r12d
..B1.272:                       # Preds ..B1.270 ..B1.271
                                # Execution count [7.78e+00]
        movslq    %ebx, %rbx                                    #207.7
        movslq    4208(%rsp), %r8                               #207.7
        imulq     %rbx, %r8                                     #207.7
        addq      %r8, %rcx                                     #207.7
        movq      %rcx, 4440(%rsp)                              #207.7
        jmp       ..B1.274      # Prob 100%                     #207.7
                                # LOE rdx rcx rsi rdi ebx r12d
..B1.273:                       # Preds ..B1.271
                                # Execution count [6.11e+00]
        movl      %ebx, %r8d                                    #207.7
        imull     24(%r9), %r8d                                 #207.7
        addl      4204(%rsp), %r8d                              #207.7
        movslq    %r8d, %r8                                     #207.7
        movq      32(%r9), %r9                                  #207.7
        movl      4208(%rsp), %r10d                             #207.7
        imull     (%r9,%r8,4), %r10d                            #207.7
        movslq    %r10d, %r10                                   #207.7
        addq      %r10, %rcx                                    #207.7
        movq      %rcx, 4440(%rsp)                              #207.7
                                # LOE rdx rcx rsi rdi ebx r12d
..B1.274:                       # Preds ..B1.263 ..B1.262 ..B1.257 ..B1.267 ..B1.268
                                #       ..B1.272 ..B1.273
                                # Execution count [5.56e+01]
        cmpl      $-1, 3420(%rsp)                               #207.7
        jge       ..B1.282      # Prob 50%                      #207.7
                                # LOE rdx rcx rsi rdi ebx r12d
..B1.275:                       # Preds ..B1.274
                                # Execution count [2.78e+01]
        movl      3420(%rsp), %r9d                              #207.7
        movl      %r9d, 4672(%rsp)                              #207.7[spill]
        negl      %r9d                                          #207.7
        movq      3408(%rsp), %r10                              #207.7
        movq      3432(%rsp), %r11                              #207.7
        movl      3424(%rsp), %r13d                             #207.7
        movq      4448(%rsp), %r8                               #207.7
        movl      %r9d, 4688(%rsp)                              #207.7[spill]
        testl     %r9d, %r9d                                    #207.7
        jle       ..B1.292      # Prob 50%                      #207.7
                                # LOE rdx rcx rsi rdi r8 r10 r11 ebx r9d r12d r13d
..B1.276:                       # Preds ..B1.275
                                # Execution count [2.78e+01]
        movl      %r9d, %r15d                                   #207.7
        movl      $1, %eax                                      #207.7
        shrl      $31, %r15d                                    #207.7
        xorl      %r9d, %r9d                                    #207.7
        subl      4672(%rsp), %r15d                             #207.7[spill]
        sarl      $1, %r15d                                     #207.7
        movl      24(%r10), %r14d                               #207.7
        testl     %r15d, %r15d                                  #207.7
        jbe       ..B1.280      # Prob 10%                      #207.7
                                # LOE rdx rcx rsi rdi r8 r10 r11 eax ebx r9d r12d r13d r14d r15d
..B1.277:                       # Preds ..B1.276
                                # Execution count [2.50e+01]
        movl      %ebx, %eax                                    #207.7
        movl      %r12d, 4632(%rsp)                             #207.7[spill]
        imull     %r14d, %eax                                   #207.7
        movq      %rdi, 4648(%rsp)                              #207.7[spill]
        movl      %ebx, 4640(%rsp)                              #207.7[spill]
        movl      %r15d, %r12d                                  #207.7
        .align    16,0x90
                                # LOE rdx rcx rsi r8 r10 r11 eax r9d r12d r13d r14d
..B1.278:                       # Preds ..B1.278 ..B1.277
                                # Execution count [6.94e+01]
        movq      32(%r10), %r15                                #207.7
        lea       (%rax,%r9,2), %edi                            #207.7
        movslq    %edi, %rdi                                    #207.7
        lea       (%r9,%r9), %ebx                               #207.7
        movslq    %ebx, %rbx                                    #207.7
        incl      %r9d                                          #207.7
        movl      (%r15,%rdi,4), %r15d                          #207.7
        imull     %r13d, %r15d                                  #207.7
        movslq    %r15d, %r15                                   #207.7
        addq      %r11, %r15                                    #207.7
        movq      %r15, (%r8,%rbx,8)                            #207.7
        movq      32(%r10), %r15                                #207.7
        movl      4(%r15,%rdi,4), %edi                          #207.7
        imull     %r13d, %edi                                   #207.7
        movslq    %edi, %rdi                                    #207.7
        addq      %r11, %rdi                                    #207.7
        movq      %rdi, 8(%r8,%rbx,8)                           #207.7
        cmpl      %r12d, %r9d                                   #207.7
        jb        ..B1.278      # Prob 64%                      #207.7
                                # LOE rdx rcx rsi r8 r10 r11 eax r9d r12d r13d r14d
..B1.279:                       # Preds ..B1.278
                                # Execution count [2.50e+01]
        movq      4648(%rsp), %rdi                              #[spill]
        lea       1(%r9,%r9), %eax                              #207.7
        movl      4640(%rsp), %ebx                              #[spill]
        movl      4632(%rsp), %r12d                             #[spill]
                                # LOE rdx rcx rsi rdi r8 r10 r11 eax ebx r12d r13d r14d
..B1.280:                       # Preds ..B1.279 ..B1.276
                                # Execution count [2.78e+01]
        lea       -1(%rax), %r9d                                #207.7
        cmpl      4688(%rsp), %r9d                              #207.7[spill]
        jae       ..B1.292      # Prob 10%                      #207.7
                                # LOE rdx rcx rsi rdi r8 r10 r11 eax ebx r12d r13d r14d
..B1.281:                       # Preds ..B1.280
                                # Execution count [2.50e+01]
        imull     %ebx, %r14d                                   #207.7
        movslq    %eax, %rax                                    #207.7
        movslq    %r14d, %r14                                   #207.7
        addq      %rax, %r14                                    #207.7
        movq      32(%r10), %r10                                #207.7
        movl      -4(%r10,%r14,4), %r9d                         #207.7
        imull     %r9d, %r13d                                   #207.7
        movslq    %r13d, %r13                                   #207.7
        addq      %r13, %r11                                    #207.7
        movq      %r11, -8(%r8,%rax,8)                          #207.7
        jmp       ..B1.292      # Prob 100%                     #207.7
                                # LOE rdx rcx rsi rdi r8 ebx r12d
..B1.282:                       # Preds ..B1.274
                                # Execution count [2.78e+01]
        movups    3392(%rsp), %xmm0                             #207.7
        movups    3408(%rsp), %xmm1                             #207.7
        movups    3424(%rsp), %xmm2                             #207.7
        movups    3440(%rsp), %xmm3                             #207.7
        movups    3456(%rsp), %xmm4                             #207.7
        movups    3472(%rsp), %xmm5                             #207.7
        movq      3432(%rsp), %r8                               #207.7
        movups    %xmm0, 4272(%rsp)                             #207.7
        movups    %xmm1, 4288(%rsp)                             #207.7
        movups    %xmm2, 4304(%rsp)                             #207.7
        movups    %xmm3, 4320(%rsp)                             #207.7
        movups    %xmm4, 4336(%rsp)                             #207.7
        movups    %xmm5, 4352(%rsp)                             #207.7
        cmpl      $0, 3476(%rsp)                                #207.7
        jne       ..B1.288      # Prob 50%                      #207.7
                                # LOE rdx rcx rsi rdi r8 ebx r12d
..B1.283:                       # Preds ..B1.282
                                # Execution count [1.39e+01]
        testl     %r12d, %r12d                                  #207.7
        je        ..B1.286      # Prob 50%                      #207.7
                                # LOE rdx rcx rsi rdi r8 ebx r12d
..B1.284:                       # Preds ..B1.283
                                # Execution count [6.94e+00]
        cmpl      $0, 3472(%rsp)                                #207.7
        je        ..B1.286      # Prob 50%                      #207.7
                                # LOE rdx rcx rsi rdi r8 ebx r12d
..B1.285:                       # Preds ..B1.284
                                # Execution count [3.47e+00]
        movq      blank_args(%rip), %r8                         #207.7
        movq      %r8, 4448(%rsp)                               #207.7
        jmp       ..B1.292      # Prob 100%                     #207.7
                                # LOE rdx rcx rsi rdi r8 ebx r12d
..B1.286:                       # Preds ..B1.283 ..B1.284
                                # Execution count [3.47e+00]
        movq      %r8, 4448(%rsp)                               #207.7
        jmp       ..B1.292      # Prob 100%                     #207.7
                                # LOE rdx rcx rsi rdi r8 ebx r12d
..B1.288:                       # Preds ..B1.282
                                # Execution count [1.39e+01]
        movq      4288(%rsp), %r10                              #207.7
        testq     %r10, %r10                                    #207.7
        je        ..B1.290      # Prob 12%                      #207.7
                                # LOE rdx rcx rsi rdi r8 r10 ebx r12d
..B1.289:                       # Preds ..B1.288
                                # Execution count [1.22e+01]
        cmpl      $0, 4364(%rsp)                                #207.7
        jne       ..B1.291      # Prob 50%                      #207.7
                                # LOE rdx rcx rsi rdi r8 r10 ebx r12d
..B1.290:                       # Preds ..B1.288 ..B1.289
                                # Execution count [7.78e+00]
        movslq    %ebx, %rbx                                    #207.7
        movslq    4304(%rsp), %r9                               #207.7
        imulq     %rbx, %r9                                     #207.7
        addq      %r9, %r8                                      #207.7
        movq      %r8, 4448(%rsp)                               #207.7
        jmp       ..B1.292      # Prob 100%                     #207.7
                                # LOE rdx rcx rsi rdi r8 ebx r12d
..B1.291:                       # Preds ..B1.289
                                # Execution count [6.11e+00]
        movl      %ebx, %r9d                                    #207.7
        imull     24(%r10), %r9d                                #207.7
        addl      4300(%rsp), %r9d                              #207.7
        movslq    %r9d, %r9                                     #207.7
        movq      32(%r10), %r10                                #207.7
        movl      4304(%rsp), %r11d                             #207.7
        imull     (%r10,%r9,4), %r11d                           #207.7
        movslq    %r11d, %r11                                   #207.7
        addq      %r11, %r8                                     #207.7
        movq      %r8, 4448(%rsp)                               #207.7
                                # LOE rdx rcx rsi rdi r8 ebx r12d
..B1.292:                       # Preds ..B1.281 ..B1.280 ..B1.275 ..B1.285 ..B1.286
                                #       ..B1.290 ..B1.291
                                # Execution count [5.56e+01]
..___tag_value_main.211:
#       update(const double *, double *, double *, const double *, double *)
        call      _Z6updatePKdPdS1_S0_S1_                       #207.7
..___tag_value_main.212:
                                # LOE ebx r12d
..B1.293:                       # Preds ..B1.292
                                # Execution count [5.56e+01]
        incl      %ebx                                          #207.7
        cmpl      4712(%rsp), %ebx                              #207.7[spill]
        jl        ..B1.199      # Prob 82%                      #207.7
                                # LOE ebx r12d
..B1.294:                       # Preds ..B1.293
                                # Execution count [1.00e+01]
        movl      4704(%rsp), %r8d                              #[spill]
        movl      4712(%rsp), %r9d                              #[spill]
        movb      4504(%rsp), %r15b                             #[spill]
        movq      4456(%rsp), %r14                              #[spill]
        movq      4496(%rsp), %r13                              #[spill]
        movq      4480(%rsp), %r12                              #[spill]
        movq      4488(%rsp), %rbx                              #[spill]
        cmpl      %r8d, %r9d                                    #207.7
        jne       ..B1.296      # Prob 50%                      #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.295:                       # Preds ..B1.333 ..B1.294 ..B1.334
                                # Execution count [6.78e+00]
        movl      $5, %edi                                      #207.7
        lea       3008(%rsp), %rsi                              #207.7
..___tag_value_main.213:
#       op_mpi_wait_all(int, op_arg *)
        call      op_mpi_wait_all                               #207.7
..___tag_value_main.214:
                                # LOE rbx r12 r13 r14 r15b
..B1.296:                       # Preds ..B1.295 ..B1.334 ..B1.294
                                # Execution count [1.11e+01]
        movl      $5, %edi                                      #207.7
        lea       3008(%rsp), %rsi                              #207.7
..___tag_value_main.215:
#       op_mpi_set_dirtybit(int, op_arg *)
        call      op_mpi_set_dirtybit                           #207.7
..___tag_value_main.216:
                                # LOE rbx r12 r13 r14 r15b
..B1.297:                       # Preds ..B1.296
                                # Execution count [1.11e+01]
        movq      4424(%rsp), %rsi                              #207.7
        lea       2720(%rsp), %rdi                              #207.7
..___tag_value_main.217:
#       op_mpi_reduce_double(op_arg *, double *)
        call      op_mpi_reduce_double                          #207.7
..___tag_value_main.218:
                                # LOE rbx r12 r13 r14 r15b
..B1.298:                       # Preds ..B1.297
                                # Execution count [1.11e+01]
        movq      4432(%rsp), %rsi                              #207.7
        lea       2816(%rsp), %rdi                              #207.7
..___tag_value_main.219:
#       op_mpi_reduce_double(op_arg *, double *)
        call      op_mpi_reduce_double                          #207.7
..___tag_value_main.220:
                                # LOE rbx r12 r13 r14 r15b
..B1.299:                       # Preds ..B1.298
                                # Execution count [1.11e+01]
        movq      4448(%rsp), %rsi                              #207.7
        lea       2912(%rsp), %rdi                              #207.7
..___tag_value_main.221:
#       op_mpi_reduce_double(op_arg *, double *)
        call      op_mpi_reduce_double                          #207.7
..___tag_value_main.222:
                                # LOE rbx r12 r13 r14 r15b
..B1.300:                       # Preds ..B1.299
                                # Execution count [1.11e+01]
        lea       4536(%rsp), %rdi                              #207.7
        lea       4544(%rsp), %rsi                              #207.7
..___tag_value_main.223:
#       op_timers_core(double *, double *)
        call      op_timers_core                                #207.7
..___tag_value_main.224:
                                # LOE rbx r12 r13 r14 r15b
..B1.301:                       # Preds ..B1.300
                                # Execution count [1.11e+01]
        movsd     4544(%rsp), %xmm0                             #207.7
        movl      $.L_2__STRING.42, %edi                        #207.7
        subsd     4528(%rsp), %xmm0                             #207.7
..___tag_value_main.225:
#       op_mpi_perf_time(const char *, double)
        call      op_mpi_perf_time                              #207.7
..___tag_value_main.226:
                                # LOE rbx r12 r13 r14 r15b
..B1.302:                       # Preds ..B1.301
                                # Execution count [1.11e+01]
        cmpl      $-1, 2268(%rsp)                               #207.7
        jge       ..B1.304      # Prob 78%                      #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.303:                       # Preds ..B1.302
                                # Execution count [2.44e+00]
        movq      4416(%rsp), %rdi                              #207.7
#       free(void *)
        call      free                                          #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.304:                       # Preds ..B1.303 ..B1.302
                                # Execution count [1.11e+01]
        cmpl      $-1, 2748(%rsp)                               #207.7
        jge       ..B1.306      # Prob 78%                      #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.305:                       # Preds ..B1.304
                                # Execution count [2.44e+00]
        movq      4424(%rsp), %rdi                              #207.7
#       free(void *)
        call      free                                          #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.306:                       # Preds ..B1.305 ..B1.304
                                # Execution count [1.11e+01]
        cmpl      $-1, 2844(%rsp)                               #207.7
        jge       ..B1.308      # Prob 78%                      #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.307:                       # Preds ..B1.306
                                # Execution count [2.44e+00]
        movq      4432(%rsp), %rdi                              #207.7
#       free(void *)
        call      free                                          #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.308:                       # Preds ..B1.307 ..B1.306
                                # Execution count [1.11e+01]
        cmpl      $-1, 2556(%rsp)                               #207.7
        jge       ..B1.310      # Prob 78%                      #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.309:                       # Preds ..B1.308
                                # Execution count [2.44e+00]
        movq      4440(%rsp), %rdi                              #207.7
#       free(void *)
        call      free                                          #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.310:                       # Preds ..B1.309 ..B1.308
                                # Execution count [1.11e+01]
        cmpl      $-1, 2940(%rsp)                               #207.7
        jge       ..B1.312      # Prob 78%                      #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.311:                       # Preds ..B1.310
                                # Execution count [2.44e+00]
        movq      4448(%rsp), %rdi                              #207.7
#       free(void *)
        call      free                                          #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.312:                       # Preds ..B1.311 ..B1.310
                                # Execution count [1.11e+01]
        incb      %r15b                                         #171.28
        cmpb      $2, %r15b                                     #171.25
        jl        ..B1.123      # Prob 50%                      #171.25
                                # LOE rbx r12 r13 r14 r15b
..B1.313:                       # Preds ..B1.312
                                # Execution count [5.56e+00]
        movsd     4512(%rsp), %xmm0                             #217.16
        movl      $1374389535, %eax                             #219.16
        divsd     4408(%rsp), %xmm0                             #217.30[spill]
        movl      3496(%rsp), %ebx                              #[spill]
        imull     %ebx                                          #219.16
        sqrtsd    %xmm0, %xmm0                                  #217.11
        sarl      $5, %edx                                      #219.16
        imull     $-100, %edx, %ecx                             #219.16
        movsd     %xmm0, 4512(%rsp)                             #217.5
        movq      %r14, 4456(%rsp)                              #[spill]
        movl      4400(%rsp), %r12d                             #[spill]
        movq      4608(%rsp), %r15                              #[spill]
        movq      4696(%rsp), %r14                              #[spill]
        addl      %ebx, %ecx                                    #219.16
        jne       ..B1.315      # Prob 78%                      #219.23
                                # LOE r13 r14 r15 ebx r12d xmm0
..B1.314:                       # Preds ..B1.313
                                # Execution count [1.22e+00]
        movl      $.L_2__STRING.43, %edi                        #220.7
        movl      %ebx, %esi                                    #220.7
        movl      $1, %eax                                      #220.7
..___tag_value_main.227:
#       op_printf(const char *, ...)
        call      op_printf                                     #220.7
..___tag_value_main.228:
                                # LOE r13 r14 r15 ebx r12d
..B1.315:                       # Preds ..B1.314 ..B1.313
                                # Execution count [5.56e+00]
        movl      $274877907, %eax                              #222.16
        imull     %ebx                                          #222.16
        sarl      $6, %edx                                      #222.16
        imull     $-1000, %edx, %ecx                            #222.16
        addl      %ebx, %ecx                                    #222.16
        jne       ..B1.317      # Prob 50%                      #222.24
                                # LOE r13 r14 r15 ebx r12d
..B1.316:                       # Preds ..B1.315
                                # Execution count [2.78e+00]
        cmpl      $720000, %r12d                                #223.20
        je        ..B1.329      # Prob 5%                       #223.20
                                # LOE r13 r14 r15 ebx r12d
..B1.317:                       # Preds ..B1.331 ..B1.332 ..B1.316 ..B1.315
                                # Execution count [5.56e+00]
        incl      %ebx                                          #161.37
        cmpl      $1000, %ebx                                   #161.30
        jle       ..B1.42       # Prob 82%                      #161.30
                                # LOE r13 r14 r15 ebx r12d
..B1.318:                       # Preds ..B1.317
                                # Execution count [1.00e+00]
        lea       (%rsp), %rdi                                  #237.3
        lea       3496(%rsp), %rsi                              #237.3
..___tag_value_main.229:
#       op_timers(double *, double *)
        call      op_timers                                     #237.3
..___tag_value_main.230:
                                # LOE r13 r14
..B1.319:                       # Preds ..B1.318
                                # Execution count [1.00e+00]
        movq      %r14, %rdi                                    #242.52
..___tag_value_main.231:
#       op_get_size(op_set)
        call      op_get_size                                   #242.52
..___tag_value_main.232:
                                # LOE r13 r14 eax
..B1.320:                       # Preds ..B1.319
                                # Execution count [1.00e+00]
        movslq    %eax, %rax                                    #242.25
        shlq      $5, %rax                                      #242.25
        movq      %rax, %rdi                                    #242.25
..___tag_value_main.233:
#       op_malloc(size_t)
        call      op_malloc                                     #242.25
..___tag_value_main.234:
                                # LOE rax r13 r14
..B1.408:                       # Preds ..B1.320
                                # Execution count [1.00e+00]
        movq      %rax, %rbx                                    #242.25
                                # LOE rbx r13 r14
..B1.321:                       # Preds ..B1.408
                                # Execution count [1.00e+00]
        movq      %r14, %rdi                                    #243.32
..___tag_value_main.235:
#       op_get_size(op_set)
        call      op_get_size                                   #243.32
..___tag_value_main.236:
                                # LOE rbx r13 eax
..B1.322:                       # Preds ..B1.321
                                # Execution count [1.00e+00]
        decl      %eax                                          #243.3
        movq      %r13, %rdi                                    #243.3
        movq      %rbx, %rsi                                    #243.3
        xorl      %edx, %edx                                    #243.3
        movl      %eax, %ecx                                    #243.3
..___tag_value_main.237:
#       op_fetch_data_idx_char(op_dat, char *, int, int)
        call      op_fetch_data_idx_char                        #243.3
..___tag_value_main.238:
                                # LOE rbx r13
..B1.323:                       # Preds ..B1.322
                                # Execution count [1.00e+00]
        movq      %rbx, %rdi                                    #244.3
#       free(void *)
        call      free                                          #244.3
                                # LOE r13
..B1.324:                       # Preds ..B1.323
                                # Execution count [1.00e+00]
        movq      %r13, %rdi                                    #248.3
        movl      $.L_2__STRING.47, %esi                        #248.3
..___tag_value_main.239:
#       op_fetch_data_hdf5_file(op_dat, const char *)
        call      op_fetch_data_hdf5_file                       #248.3
..___tag_value_main.240:
                                # LOE
..B1.325:                       # Preds ..B1.324
                                # Execution count [1.00e+00]
..___tag_value_main.241:
#       op_timing_output()
        call      op_timing_output                              #259.3
..___tag_value_main.242:
                                # LOE
..B1.326:                       # Preds ..B1.325
                                # Execution count [1.00e+00]
        movsd     3496(%rsp), %xmm0                             #260.3
        movl      $.L_2__STRING.48, %edi                        #260.3
        movl      $1, %eax                                      #260.3
        subsd     3488(%rsp), %xmm0                             #260.3
..___tag_value_main.243:
#       op_printf(const char *, ...)
        call      op_printf                                     #260.3
..___tag_value_main.244:
                                # LOE
..B1.327:                       # Preds ..B1.326
                                # Execution count [1.00e+00]
..___tag_value_main.245:
#       op_exit()
        call      op_exit                                       #261.3
..___tag_value_main.246:
                                # LOE
..B1.328:                       # Preds ..B1.327
                                # Execution count [1.00e+00]
        xorl      %eax, %eax                                    #262.1
        addq      $4824, %rsp                                   #262.1
	.cfi_restore 3
        popq      %rbx                                          #262.1
	.cfi_restore 15
        popq      %r15                                          #262.1
	.cfi_restore 14
        popq      %r14                                          #262.1
	.cfi_restore 13
        popq      %r13                                          #262.1
	.cfi_restore 12
        popq      %r12                                          #262.1
        movq      %rbp, %rsp                                    #262.1
        popq      %rbp                                          #262.1
	.cfi_def_cfa 7, 8
	.cfi_restore 6
        ret                                                     #262.1
	.cfi_def_cfa 6, 16
	.cfi_escape 0x10, 0x03, 0x0e, 0x38, 0x1c, 0x0d, 0x80, 0xff, 0xff, 0xff, 0x1a, 0x0d, 0xd8, 0xff, 0xff, 0xff, 0x22
	.cfi_offset 6, -16
	.cfi_escape 0x10, 0x0c, 0x0e, 0x38, 0x1c, 0x0d, 0x80, 0xff, 0xff, 0xff, 0x1a, 0x0d, 0xf8, 0xff, 0xff, 0xff, 0x22
	.cfi_escape 0x10, 0x0d, 0x0e, 0x38, 0x1c, 0x0d, 0x80, 0xff, 0xff, 0xff, 0x1a, 0x0d, 0xf0, 0xff, 0xff, 0xff, 0x22
	.cfi_escape 0x10, 0x0e, 0x0e, 0x38, 0x1c, 0x0d, 0x80, 0xff, 0xff, 0xff, 0x1a, 0x0d, 0xe8, 0xff, 0xff, 0xff, 0x22
	.cfi_escape 0x10, 0x0f, 0x0e, 0x38, 0x1c, 0x0d, 0x80, 0xff, 0xff, 0xff, 0x1a, 0x0d, 0xe0, 0xff, 0xff, 0xff, 0x22
                                # LOE
..B1.329:                       # Preds ..B1.316
                                # Execution count [1.42e-01]: Infreq
        movsd     4512(%rsp), %xmm0                             #225.36
        movl      $.L_2__STRING.44, %edi                        #226.7
        divsd     .L_2il0floatpacket.63(%rip), %xmm0            #225.42
        mulsd     .L_2il0floatpacket.62(%rip), %xmm0            #225.42
        movl      $720000, %esi                                 #226.7
        movl      $1, %eax                                      #226.7
        subsd     .L_2il0floatpacket.62(%rip), %xmm0            #225.65
        andps     .L_2il0floatpacket.65(%rip), %xmm0            #225.21
        movsd     %xmm0, (%rsp)                                 #225.21[spill]
..___tag_value_main.261:
#       op_printf(const char *, ...)
        call      op_printf                                     #226.7
..___tag_value_main.262:
                                # LOE r13 r14 r15 ebx r12d
..B1.330:                       # Preds ..B1.329
                                # Execution count [1.42e-01]: Infreq
        movsd     .L_2il0floatpacket.64(%rip), %xmm0            #229.18
        comisd    (%rsp), %xmm0                                 #229.18[spill]
        jbe       ..B1.332      # Prob 50%                      #229.18
                                # LOE r13 r14 r15 ebx r12d
..B1.331:                       # Preds ..B1.330
                                # Execution count [7.08e-02]: Infreq
        movl      $.L_2__STRING.45, %edi                        #230.9
        xorl      %eax, %eax                                    #230.9
..___tag_value_main.263:
#       op_printf(const char *, ...)
        call      op_printf                                     #230.9
..___tag_value_main.264:
        jmp       ..B1.317      # Prob 100%                     #230.9
                                # LOE r13 r14 r15 ebx r12d
..B1.332:                       # Preds ..B1.330
                                # Execution count [7.08e-02]: Infreq
        movl      $.L_2__STRING.46, %edi                        #232.9
        xorl      %eax, %eax                                    #232.9
..___tag_value_main.265:
#       op_printf(const char *, ...)
        call      op_printf                                     #232.9
..___tag_value_main.266:
        jmp       ..B1.317      # Prob 100%                     #232.9
                                # LOE r13 r14 r15 ebx r12d
..B1.333:                       # Preds ..B1.197
                                # Execution count [1.11e+00]: Infreq
        cmpl      %r8d, %r9d                                    #207.7
        je        ..B1.295      # Prob 50%                      #207.7
                                # LOE rbx r12 r13 r14 r9d r15b
..B1.334:                       # Preds ..B1.333
                                # Execution count [5.56e-01]: Infreq
        testl     %r9d, %r9d                                    #207.7
        je        ..B1.295      # Prob 22%                      #207.7
        jmp       ..B1.296      # Prob 100%                     #207.7
                                # LOE rbx r12 r13 r14 r15b
..B1.335:                       # Preds ..B1.69
                                # Execution count [5.56e-01]: Infreq
        cmpl      %ecx, %r8d                                    #165.5
        je        ..B1.113      # Prob 50%                      #165.5
                                # LOE r13 r14 r15 ebx r8d r12d
..B1.336:                       # Preds ..B1.335
                                # Execution count [2.78e-01]: Infreq
        testl     %r8d, %r8d                                    #165.5
        je        ..B1.113      # Prob 22%                      #165.5
        jmp       ..B1.114      # Prob 100%                     #165.5
                                # LOE r13 r14 r15 ebx r12d
..B1.337:                       # Preds ..B1.21
                                # Execution count [3.70e-02]: Infreq
        movl      $il0_peep_printf_format_1, %edi               #116.5
        call      puts                                          #116.5
        jmp       ..B1.22       # Prob 100%                     #116.5
                                # LOE rbx r13 r14 r15
..B1.338:                       # Preds ..B1.13
                                # Execution count [3.70e-02]: Infreq
        movl      $il0_peep_printf_format_0, %edi               #105.5
        call      puts                                          #105.5
        jmp       ..B1.14       # Prob 100%                     #105.5
        .align    16,0x90
                                # LOE rbx r12 r14
	.cfi_endproc
# mark_end;
	.type	main,@function
	.size	main,.-main
	.section .rodata.str1.4, "aMS",@progbits,1
	.align 4
	.align 4
il0_peep_printf_format_1:
	.long	1702125424
	.long	1847620723
	.long	1713402991
	.long	1684960623
	.byte	0
	.space 3, 0x00 	# pad
	.align 4
il0_peep_printf_format_0:
	.long	1702125421
	.long	1847620723
	.long	1713402991
	.long	1684960623
	.byte	0
	.data
# -- End  main
	.section .text._Z9save_solnPKdPd, "xaG",@progbits,_Z9save_solnPKdPd,comdat
..TXTST1:
# -- Begin  _Z9save_solnPKdPd
	.section .text._Z9save_solnPKdPd, "xaG",@progbits,_Z9save_solnPKdPd,comdat
# mark_begin;
       .align    16,0x90
	.weak _Z9save_solnPKdPd
# --- save_soln(const double *, double *)
_Z9save_solnPKdPd:
# parameter 1: %rdi
# parameter 2: %rsi
..B2.1:                         # Preds ..B2.0
                                # Execution count [1.00e+00]
	.cfi_startproc
	.cfi_personality 0x3,__gxx_personality_v0
..___tag_value__Z9save_solnPKdPd.268:
..L269:
                                                        #1.54
        movq      (%rdi), %rax                                  #3.15
        movq      %rax, (%rsi)                                  #3.5
        movq      8(%rdi), %rdx                                 #3.15
        movq      %rdx, 8(%rsi)                                 #3.5
        movq      16(%rdi), %rcx                                #3.15
        movq      %rcx, 16(%rsi)                                #3.5
        movq      24(%rdi), %r8                                 #3.15
        movq      %r8, 24(%rsi)                                 #3.5
        ret                                                     #4.1
        .align    16,0x90
                                # LOE
	.cfi_endproc
# mark_end;
	.type	_Z9save_solnPKdPd,@function
	.size	_Z9save_solnPKdPd,.-_Z9save_solnPKdPd
	.data
# -- End  _Z9save_solnPKdPd
	.section .text._Z6updatePKdPdS1_S0_S1_, "xaG",@progbits,_Z6updatePKdPdS1_S0_S1_,comdat
..TXTST2:
# -- Begin  _Z6updatePKdPdS1_S0_S1_
	.section .text._Z6updatePKdPdS1_S0_S1_, "xaG",@progbits,_Z6updatePKdPdS1_S0_S1_,comdat
# mark_begin;
       .align    16,0x90
	.weak _Z6updatePKdPdS1_S0_S1_
# --- update(const double *, double *, double *, const double *, double *)
_Z6updatePKdPdS1_S0_S1_:
# parameter 1: %rdi
# parameter 2: %rsi
# parameter 3: %rdx
# parameter 4: %rcx
# parameter 5: %r8
..B3.1:                         # Preds ..B3.0
                                # Execution count [1.00e+00]
	.cfi_startproc
	.cfi_personality 0x3,__gxx_personality_v0
..___tag_value__Z6updatePKdPdS1_S0_S1_.271:
..L272:
                                                        #2.52
        xorl      %eax, %eax                                    #11.5
        movsd     .L_2il0floatpacket.66(%rip), %xmm7            #6.3
        divsd     (%rcx), %xmm7                                 #6.19
        movsd     (%rdx), %xmm8                                 #9.18
        mulsd     %xmm7, %xmm8                                  #9.18
        movsd     (%rdi), %xmm0                                 #10.12
        subsd     %xmm8, %xmm0                                  #10.22
        mulsd     %xmm8, %xmm8                                  #12.19
        movsd     %xmm0, (%rsi)                                 #10.5
        movsd     8(%rdx), %xmm2                                #9.18
        mulsd     %xmm7, %xmm2                                  #9.18
        movq      %rax, (%rdx)                                  #11.5
        movsd     8(%rdi), %xmm1                                #10.12
        subsd     %xmm2, %xmm1                                  #10.22
        mulsd     %xmm2, %xmm2                                  #12.19
        movsd     %xmm1, 8(%rsi)                                #10.5
        addsd     %xmm2, %xmm8                                  #12.5
        movsd     16(%rdx), %xmm4                               #9.18
        mulsd     %xmm7, %xmm4                                  #9.18
        movq      %rax, 8(%rdx)                                 #11.5
        movsd     16(%rdi), %xmm3                               #10.12
        subsd     %xmm4, %xmm3                                  #10.22
        mulsd     %xmm4, %xmm4                                  #12.19
        movsd     %xmm3, 16(%rsi)                               #10.5
        addsd     %xmm4, %xmm8                                  #12.5
        movsd     24(%rdx), %xmm5                               #9.18
        mulsd     %xmm5, %xmm7                                  #9.18
        movq      %rax, 16(%rdx)                                #11.5
        movsd     24(%rdi), %xmm6                               #10.12
        subsd     %xmm7, %xmm6                                  #10.22
        mulsd     %xmm7, %xmm7                                  #12.19
        movsd     %xmm6, 24(%rsi)                               #10.5
        addsd     %xmm7, %xmm8                                  #12.5
        movq      %rax, 24(%rdx)                                #11.5
        addsd     (%r8), %xmm8                                  #14.4
        movsd     %xmm8, (%r8)                                  #14.4
        ret                                                     #15.1
        .align    16,0x90
                                # LOE
	.cfi_endproc
# mark_end;
	.type	_Z6updatePKdPdS1_S0_S1_,@function
	.size	_Z6updatePKdPdS1_S0_S1_,.-_Z6updatePKdPdS1_S0_S1_
	.data
# -- End  _Z6updatePKdPdS1_S0_S1_
	.section .text._Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_, "xaG",@progbits,_Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_,comdat
..TXTST3:
# -- Begin  _Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_
	.section .text._Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_, "xaG",@progbits,_Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_,comdat
# mark_begin;
       .align    16,0x90
	.weak _Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_
# --- op_par_loop<const double, const double, const double, const double, double, const int>(void (*)(const double *, const double *, const double *, const double *, double *, const int *), const char *, op_set, op_arg, op_arg, op_arg, op_arg, op_arg, op_arg)
_Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_:
# parameter 1: %rdi
# parameter 2: %rsi
# parameter 3: %rdx
# parameter 4: 1360 + %rsp
# parameter 5: 1456 + %rsp
# parameter 6: 1552 + %rsp
# parameter 7: 1648 + %rsp
# parameter 8: 1744 + %rsp
# parameter 9: 1840 + %rsp
..B4.1:                         # Preds ..B4.0
                                # Execution count [1.00e+00]
	.cfi_startproc
	.cfi_personality 0x3,__gxx_personality_v0
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.274:
..L275:
                                                        #534.70
        pushq     %r13                                          #534.70
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
        pushq     %r14                                          #534.70
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
        pushq     %r15                                          #534.70
	.cfi_def_cfa_offset 32
	.cfi_offset 15, -32
        pushq     %rbx                                          #534.70
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
        pushq     %rbp                                          #534.70
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
        subq      $1312, %rsp                                   #534.70
	.cfi_def_cfa_offset 1360
        movq      %rdx, %rbx                                    #534.70
        pxor      %xmm0, %xmm0                                  #536.16
        movq      %rsi, %rbp                                    #534.70
        movups    %xmm0, 1152(%rsp)                             #536.16
        movq      %rdi, %r15                                    #534.70
        movups    %xmm0, 1168(%rsp)                             #536.16
        movups    %xmm0, 1184(%rsp)                             #536.16
                                # LOE rbx rbp r12 r13 r14 r15
..B4.2:                         # Preds ..B4.1
                                # Execution count [1.00e+00]
        movups    1360(%rsp), %xmm0                             #537.21
        movups    1376(%rsp), %xmm1                             #537.21
        movups    1392(%rsp), %xmm2                             #537.21
        movups    1408(%rsp), %xmm3                             #537.21
        movups    1424(%rsp), %xmm4                             #537.21
        movups    1440(%rsp), %xmm5                             #537.21
        movups    1456(%rsp), %xmm6                             #537.27
        movups    1472(%rsp), %xmm7                             #537.27
        movups    1488(%rsp), %xmm8                             #537.27
        movups    1504(%rsp), %xmm9                             #537.27
        movups    1520(%rsp), %xmm10                            #537.27
        movups    1536(%rsp), %xmm11                            #537.27
        movups    1552(%rsp), %xmm12                            #537.33
        movups    1568(%rsp), %xmm13                            #537.33
        movups    1584(%rsp), %xmm14                            #537.33
        movups    1600(%rsp), %xmm15                            #537.33
        movups    %xmm0, (%rsp)                                 #537.21
        movups    %xmm1, 16(%rsp)                               #537.21
        movups    %xmm2, 32(%rsp)                               #537.21
        movups    %xmm3, 48(%rsp)                               #537.21
        movups    1616(%rsp), %xmm0                             #537.33
        movups    1632(%rsp), %xmm1                             #537.33
        movups    1648(%rsp), %xmm2                             #537.39
        movups    1664(%rsp), %xmm3                             #537.39
        movups    %xmm4, 64(%rsp)                               #537.21
        movups    %xmm5, 80(%rsp)                               #537.21
        movups    %xmm6, 96(%rsp)                               #537.27
        movups    %xmm7, 112(%rsp)                              #537.27
        movups    %xmm8, 128(%rsp)                              #537.27
        movups    %xmm9, 144(%rsp)                              #537.27
        movups    %xmm10, 160(%rsp)                             #537.27
        movups    %xmm11, 176(%rsp)                             #537.27
        movups    %xmm12, 192(%rsp)                             #537.33
        movups    %xmm13, 208(%rsp)                             #537.33
        movups    %xmm14, 224(%rsp)                             #537.33
        movups    %xmm15, 240(%rsp)                             #537.33
        movups    %xmm0, 256(%rsp)                              #537.33
        movups    %xmm1, 272(%rsp)                              #537.33
        movups    %xmm2, 288(%rsp)                              #537.39
        movups    %xmm3, 304(%rsp)                              #537.39
        movups    1680(%rsp), %xmm4                             #537.39
        movups    1696(%rsp), %xmm5                             #537.39
        movups    1712(%rsp), %xmm6                             #537.39
        movups    1728(%rsp), %xmm7                             #537.39
        movups    1744(%rsp), %xmm8                             #537.45
        movups    1760(%rsp), %xmm9                             #537.45
        movups    1776(%rsp), %xmm10                            #537.45
        movups    1792(%rsp), %xmm11                            #537.45
        movups    1808(%rsp), %xmm12                            #537.45
        movups    1824(%rsp), %xmm13                            #537.45
        movups    1840(%rsp), %xmm14                            #537.51
        movups    1856(%rsp), %xmm15                            #537.51
        movups    1872(%rsp), %xmm0                             #537.51
        movups    1888(%rsp), %xmm1                             #537.51
        movups    1904(%rsp), %xmm2                             #537.51
        movups    1920(%rsp), %xmm3                             #537.51
        movups    %xmm4, 320(%rsp)                              #537.39
        movups    %xmm5, 336(%rsp)                              #537.39
        movups    %xmm6, 352(%rsp)                              #537.39
        movups    %xmm7, 368(%rsp)                              #537.39
        movups    %xmm8, 384(%rsp)                              #537.45
        movups    %xmm9, 400(%rsp)                              #537.45
        movups    %xmm10, 416(%rsp)                             #537.45
        movups    %xmm11, 432(%rsp)                             #537.45
        movups    %xmm12, 448(%rsp)                             #537.45
        movups    %xmm13, 464(%rsp)                             #537.45
        movups    %xmm14, 480(%rsp)                             #537.51
        movups    %xmm15, 496(%rsp)                             #537.51
        movups    %xmm0, 512(%rsp)                              #537.51
        movups    %xmm1, 528(%rsp)                              #537.51
        movups    %xmm2, 544(%rsp)                              #537.51
        movups    %xmm3, 560(%rsp)                              #537.51
        cmpl      $-1, 1388(%rsp)                               #538.18
        jge       ..B4.5        # Prob 78%                      #538.18
                                # LOE rbx rbp r12 r13 r14 r15
..B4.3:                         # Preds ..B4.2
                                # Execution count [2.20e-01]
        movl      28(%rsp), %edi                                #539.22
        negl      %edi                                          #539.22
        movslq    %edi, %rdi                                    #539.22
        shlq      $3, %rdi                                      #539.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.287:
#       op_malloc(size_t)
        call      op_malloc                                     #539.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.288:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B4.4:                         # Preds ..B4.3
                                # Execution count [2.20e-01]
        movq      %rax, 1152(%rsp)                              #539.5
                                # LOE rbx rbp r12 r13 r14 r15
..B4.5:                         # Preds ..B4.2 ..B4.4
                                # Execution count [1.00e+00]
        cmpl      $-1, 1484(%rsp)                               #541.18
        jge       ..B4.8        # Prob 78%                      #541.18
                                # LOE rbx rbp r12 r13 r14 r15
..B4.6:                         # Preds ..B4.5
                                # Execution count [2.20e-01]
        movl      124(%rsp), %edi                               #542.22
        negl      %edi                                          #542.22
        movslq    %edi, %rdi                                    #542.22
        shlq      $3, %rdi                                      #542.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.289:
#       op_malloc(size_t)
        call      op_malloc                                     #542.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.290:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B4.7:                         # Preds ..B4.6
                                # Execution count [2.20e-01]
        movq      %rax, 1160(%rsp)                              #542.5
                                # LOE rbx rbp r12 r13 r14 r15
..B4.8:                         # Preds ..B4.5 ..B4.7
                                # Execution count [1.00e+00]
        cmpl      $-1, 1580(%rsp)                               #544.18
        jge       ..B4.11       # Prob 78%                      #544.18
                                # LOE rbx rbp r12 r13 r14 r15
..B4.9:                         # Preds ..B4.8
                                # Execution count [2.20e-01]
        movl      220(%rsp), %edi                               #545.22
        negl      %edi                                          #545.22
        movslq    %edi, %rdi                                    #545.22
        shlq      $3, %rdi                                      #545.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.291:
#       op_malloc(size_t)
        call      op_malloc                                     #545.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.292:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B4.10:                        # Preds ..B4.9
                                # Execution count [2.20e-01]
        movq      %rax, 1168(%rsp)                              #545.5
                                # LOE rbx rbp r12 r13 r14 r15
..B4.11:                        # Preds ..B4.8 ..B4.10
                                # Execution count [1.00e+00]
        cmpl      $-1, 1676(%rsp)                               #547.18
        jge       ..B4.14       # Prob 78%                      #547.18
                                # LOE rbx rbp r12 r13 r14 r15
..B4.12:                        # Preds ..B4.11
                                # Execution count [2.20e-01]
        movl      316(%rsp), %edi                               #548.22
        negl      %edi                                          #548.22
        movslq    %edi, %rdi                                    #548.22
        shlq      $3, %rdi                                      #548.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.293:
#       op_malloc(size_t)
        call      op_malloc                                     #548.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.294:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B4.13:                        # Preds ..B4.12
                                # Execution count [2.20e-01]
        movq      %rax, 1176(%rsp)                              #548.5
                                # LOE rbx rbp r12 r13 r14 r15
..B4.14:                        # Preds ..B4.11 ..B4.13
                                # Execution count [1.00e+00]
        cmpl      $-1, 1772(%rsp)                               #550.18
        jge       ..B4.17       # Prob 78%                      #550.18
                                # LOE rbx rbp r12 r13 r14 r15
..B4.15:                        # Preds ..B4.14
                                # Execution count [2.20e-01]
        movl      412(%rsp), %edi                               #551.22
        negl      %edi                                          #551.22
        movslq    %edi, %rdi                                    #551.22
        shlq      $3, %rdi                                      #551.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.295:
#       op_malloc(size_t)
        call      op_malloc                                     #551.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.296:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B4.16:                        # Preds ..B4.15
                                # Execution count [2.20e-01]
        movq      %rax, 1184(%rsp)                              #551.5
                                # LOE rbx rbp r12 r13 r14 r15
..B4.17:                        # Preds ..B4.14 ..B4.16
                                # Execution count [1.00e+00]
        cmpl      $-1, 1868(%rsp)                               #553.18
        jge       ..B4.20       # Prob 78%                      #553.18
                                # LOE rbx rbp r12 r13 r14 r15
..B4.18:                        # Preds ..B4.17
                                # Execution count [2.20e-01]
        movl      508(%rsp), %edi                               #554.22
        negl      %edi                                          #554.22
        movslq    %edi, %rdi                                    #554.22
        shlq      $2, %rdi                                      #554.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.297:
#       op_malloc(size_t)
        call      op_malloc                                     #554.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.298:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B4.19:                        # Preds ..B4.18
                                # Execution count [2.20e-01]
        movq      %rax, 1192(%rsp)                              #554.5
                                # LOE rbx rbp r12 r13 r14 r15
..B4.20:                        # Preds ..B4.17 ..B4.19
                                # Execution count [1.00e+00]
        cmpl      $0, 84(%rsp)                                  #559.28
        jne       ..B4.24       # Prob 50%                      #559.28
                                # LOE rbx rbp r12 r13 r14 r15
..B4.21:                        # Preds ..B4.20
                                # Execution count [5.00e-01]
        movl      32(%rsp), %edi                                #559.42
        cmpl      blank_args_size(%rip), %edi                   #559.57
        jle       ..B4.24       # Prob 68%                      #559.57
                                # LOE rbx rbp r12 r13 r14 r15 edi
..B4.22:                        # Preds ..B4.21
                                # Execution count [1.58e-01]
        movl      %edi, blank_args_size(%rip)                   #560.7
        movslq    %edi, %rdi                                    #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.299:
#       op_malloc(size_t)
        call      op_malloc                                     #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.300:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B4.23:                        # Preds ..B4.22
                                # Execution count [1.58e-01]
        movq      %rax, blank_args(%rip)                        #561.7
                                # LOE rbx rbp r12 r13 r14 r15
..B4.24:                        # Preds ..B4.21 ..B4.20 ..B4.23
                                # Execution count [1.00e+00]
        cmpl      $0, 180(%rsp)                                 #559.28
        jne       ..B4.28       # Prob 50%                      #559.28
                                # LOE rbx rbp r12 r13 r14 r15
..B4.25:                        # Preds ..B4.24
                                # Execution count [5.00e-01]
        movl      128(%rsp), %edi                               #559.42
        cmpl      blank_args_size(%rip), %edi                   #559.57
        jle       ..B4.28       # Prob 68%                      #559.57
                                # LOE rbx rbp r12 r13 r14 r15 edi
..B4.26:                        # Preds ..B4.25
                                # Execution count [1.58e-01]
        movl      %edi, blank_args_size(%rip)                   #560.7
        movslq    %edi, %rdi                                    #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.301:
#       op_malloc(size_t)
        call      op_malloc                                     #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.302:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B4.27:                        # Preds ..B4.26
                                # Execution count [1.58e-01]
        movq      %rax, blank_args(%rip)                        #561.7
                                # LOE rbx rbp r12 r13 r14 r15
..B4.28:                        # Preds ..B4.24 ..B4.25 ..B4.27
                                # Execution count [1.00e+00]
        cmpl      $0, 276(%rsp)                                 #559.28
        jne       ..B4.32       # Prob 50%                      #559.28
                                # LOE rbx rbp r12 r13 r14 r15
..B4.29:                        # Preds ..B4.28
                                # Execution count [5.00e-01]
        movl      224(%rsp), %edi                               #559.42
        cmpl      blank_args_size(%rip), %edi                   #559.57
        jle       ..B4.32       # Prob 68%                      #559.57
                                # LOE rbx rbp r12 r13 r14 r15 edi
..B4.30:                        # Preds ..B4.29
                                # Execution count [1.58e-01]
        movl      %edi, blank_args_size(%rip)                   #560.7
        movslq    %edi, %rdi                                    #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.303:
#       op_malloc(size_t)
        call      op_malloc                                     #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.304:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B4.31:                        # Preds ..B4.30
                                # Execution count [1.58e-01]
        movq      %rax, blank_args(%rip)                        #561.7
                                # LOE rbx rbp r12 r13 r14 r15
..B4.32:                        # Preds ..B4.28 ..B4.29 ..B4.31
                                # Execution count [1.00e+00]
        cmpl      $0, 372(%rsp)                                 #559.28
        jne       ..B4.36       # Prob 50%                      #559.28
                                # LOE rbx rbp r12 r13 r14 r15
..B4.33:                        # Preds ..B4.32
                                # Execution count [5.00e-01]
        movl      320(%rsp), %edi                               #559.42
        cmpl      blank_args_size(%rip), %edi                   #559.57
        jle       ..B4.36       # Prob 68%                      #559.57
                                # LOE rbx rbp r12 r13 r14 r15 edi
..B4.34:                        # Preds ..B4.33
                                # Execution count [1.58e-01]
        movl      %edi, blank_args_size(%rip)                   #560.7
        movslq    %edi, %rdi                                    #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.305:
#       op_malloc(size_t)
        call      op_malloc                                     #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.306:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B4.35:                        # Preds ..B4.34
                                # Execution count [1.58e-01]
        movq      %rax, blank_args(%rip)                        #561.7
                                # LOE rbx rbp r12 r13 r14 r15
..B4.36:                        # Preds ..B4.32 ..B4.33 ..B4.35
                                # Execution count [1.00e+00]
        cmpl      $0, 468(%rsp)                                 #559.28
        jne       ..B4.40       # Prob 50%                      #559.28
                                # LOE rbx rbp r12 r13 r14 r15
..B4.37:                        # Preds ..B4.36
                                # Execution count [5.00e-01]
        movl      416(%rsp), %edi                               #559.42
        cmpl      blank_args_size(%rip), %edi                   #559.57
        jle       ..B4.40       # Prob 68%                      #559.57
                                # LOE rbx rbp r12 r13 r14 r15 edi
..B4.38:                        # Preds ..B4.37
                                # Execution count [1.58e-01]
        movl      %edi, blank_args_size(%rip)                   #560.7
        movslq    %edi, %rdi                                    #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.307:
#       op_malloc(size_t)
        call      op_malloc                                     #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.308:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B4.39:                        # Preds ..B4.38
                                # Execution count [1.58e-01]
        movq      %rax, blank_args(%rip)                        #561.7
                                # LOE rbx rbp r12 r13 r14 r15
..B4.40:                        # Preds ..B4.36 ..B4.37 ..B4.39
                                # Execution count [1.00e+00]
        cmpl      $0, 564(%rsp)                                 #559.28
        jne       ..B4.44       # Prob 50%                      #559.28
                                # LOE rbx rbp r12 r13 r14 r15
..B4.41:                        # Preds ..B4.40
                                # Execution count [5.00e-01]
        movl      512(%rsp), %edi                               #559.42
        cmpl      blank_args_size(%rip), %edi                   #559.57
        jle       ..B4.44       # Prob 68%                      #559.57
                                # LOE rbx rbp r12 r13 r14 r15 edi
..B4.42:                        # Preds ..B4.41
                                # Execution count [1.58e-01]
        movl      %edi, blank_args_size(%rip)                   #560.7
        movslq    %edi, %rdi                                    #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.309:
#       op_malloc(size_t)
        call      op_malloc                                     #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.310:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B4.43:                        # Preds ..B4.42
                                # Execution count [1.58e-01]
        movq      %rax, blank_args(%rip)                        #561.7
                                # LOE rbx rbp r12 r13 r14 r15
..B4.44:                        # Preds ..B4.40 ..B4.41 ..B4.43
                                # Execution count [1.00e+00]
        movl      $0, 1304(%rsp)                                #564.13
        cmpl      $0, OP_diags(%rip)                            #565.18
        jle       ..B4.52       # Prob 40%                      #565.18
                                # LOE rbx rbp r12 r13 r14 r15
..B4.45:                        # Preds ..B4.44
                                # Execution count [5.37e-01]
        xorl      %esi, %esi                                    #566.5
        xorl      %eax, %eax                                    #566.5
        movq      %rax, %r14                                    #566.5
        movl      %esi, %r13d                                   #566.5
                                # LOE rbx rbp r12 r14 r15 r13d
..B4.46:                        # Preds ..B4.47 ..B4.45
                                # Execution count [2.98e+00]
        addq      $-96, %rsp                                    #566.5
	.cfi_def_cfa_offset 1456
        movq      %rbx, %rdi                                    #566.5
        movq      %rsp, %r8                                     #566.5
        movl      %r13d, %esi                                   #566.5
        movups    96(%rsp,%r14), %xmm0                          #566.5
        lea       1400(%rsp), %rdx                              #566.5
        movups    112(%rsp,%r14), %xmm1                         #566.5
        movq      %rbp, %rcx                                    #566.5
        movups    128(%rsp,%r14), %xmm2                         #566.5
        movups    144(%rsp,%r14), %xmm3                         #566.5
        movups    160(%rsp,%r14), %xmm4                         #566.5
        movups    176(%rsp,%r14), %xmm5                         #566.5
        movups    %xmm0, (%r8)                                  #566.5
        movups    %xmm1, 16(%r8)                                #566.5
        movups    %xmm2, 32(%r8)                                #566.5
        movups    %xmm3, 48(%r8)                                #566.5
        movups    %xmm4, 64(%r8)                                #566.5
        movups    %xmm5, 80(%r8)                                #566.5
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.312:
#       op_arg_check(op_set, int, op_arg, int *, const char *)
        call      op_arg_check                                  #566.5
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.313:
                                # LOE rbx rbp r12 r14 r15 r13d
..B4.203:                       # Preds ..B4.46
                                # Execution count [2.98e+00]
        addq      $96, %rsp                                     #566.5
	.cfi_def_cfa_offset 1360
                                # LOE rbx rbp r12 r14 r15 r13d
..B4.47:                        # Preds ..B4.203
                                # Execution count [2.98e+00]
        incl      %r13d                                         #566.5
        addq      $96, %r14                                     #566.5
        cmpl      $6, %r13d                                     #566.5
        jl        ..B4.46       # Prob 82%                      #566.5
                                # LOE rbx rbp r12 r14 r15 r13d
..B4.48:                        # Preds ..B4.47
                                # Execution count [5.37e-01]
        cmpl      $2, OP_diags(%rip)                            #568.18
        jle       ..B4.52       # Prob 50%                      #568.18
                                # LOE rbx rbp r12 r13 r14 r15
..B4.49:                        # Preds ..B4.48
                                # Execution count [5.00e-01]
        cmpl      $0, 1304(%rsp)                                #569.18
        jne       ..B4.51       # Prob 50%                      #569.18
                                # LOE rbx rbp r12 r13 r14 r15
..B4.50:                        # Preds ..B4.49
                                # Execution count [2.50e-01]
        movl      $.L_2__STRING.3, %edi                         #570.7
        movq      %rbp, %rsi                                    #570.7
        xorl      %eax, %eax                                    #570.7
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.315:
#       printf(const char *, ...)
        call      printf                                        #570.7
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.316:
        jmp       ..B4.52       # Prob 100%                     #570.7
                                # LOE rbx rbp r12 r13 r14 r15
..B4.51:                        # Preds ..B4.49
                                # Execution count [2.50e-01]
        movl      $.L_2__STRING.4, %edi                         #572.7
        movq      %rbp, %rsi                                    #572.7
        xorl      %eax, %eax                                    #572.7
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.317:
#       printf(const char *, ...)
        call      printf                                        #572.7
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.318:
                                # LOE rbx rbp r12 r13 r14 r15
..B4.52:                        # Preds ..B4.44 ..B4.50 ..B4.51 ..B4.48
                                # Execution count [1.00e+00]
        lea       1216(%rsp), %rdi                              #576.3
        lea       1224(%rsp), %rsi                              #576.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.319:
#       op_timers_core(double *, double *)
        call      op_timers_core                                #576.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.320:
                                # LOE rbx rbp r12 r13 r14 r15
..B4.53:                        # Preds ..B4.52
                                # Execution count [1.00e+00]
        movq      %rbx, %rdi                                    #579.17
        movl      $6, %esi                                      #579.17
        lea       (%rsp), %rdx                                  #579.17
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.321:
#       op_mpi_halo_exchanges(op_set, int, op_arg *)
        call      op_mpi_halo_exchanges                         #579.17
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.322:
                                # LOE rbx rbp r12 r13 r14 r15 eax
..B4.204:                       # Preds ..B4.53
                                # Execution count [1.00e+00]
        movl      %eax, %r9d                                    #579.17
                                # LOE rbx rbp r12 r13 r14 r15 r9d
..B4.54:                        # Preds ..B4.204
                                # Execution count [1.00e+00]
        xorl      %edx, %edx                                    #582.12
        xorl      %eax, %eax                                    #584.14
        testl     %r9d, %r9d                                    #584.23
        jle       ..B4.188      # Prob 10%                      #584.23
                                # LOE rbx rbp r12 r13 r14 r15 eax edx r9d
..B4.55:                        # Preds ..B4.54
                                # Execution count [9.00e-01]
        movl      %r9d, 1280(%rsp)                              #588.7[spill]
        movq      %r15, 1288(%rsp)                              #588.7[spill]
        movq      %rbp, 1208(%rsp)                              #588.7[spill]
        movl      %edx, %ebp                                    #588.7
        movq      %rbx, 1296(%rsp)                              #588.7[spill]
        movl      %eax, %ebx                                    #588.7
        movq      %r12, 1200(%rsp)                              #588.7[spill]
	.cfi_offset 12, -160
                                # LOE ebx ebp
..B4.56:                        # Preds ..B4.167 ..B4.55
                                # Execution count [5.00e+00]
        movq      1296(%rsp), %rcx                              #585.14[spill]
        cmpl      16(%rcx), %ebx                                #585.14
        jne       ..B4.58       # Prob 78%                      #585.14
                                # LOE ebx ebp
..B4.57:                        # Preds ..B4.56
                                # Execution count [1.10e+00]
        movl      $6, %edi                                      #586.7
        lea       (%rsp), %rsi                                  #586.7
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.324:
#       op_mpi_wait_all(int, op_arg *)
        call      op_mpi_wait_all                               #586.7
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.325:
                                # LOE ebx ebp
..B4.58:                        # Preds ..B4.57 ..B4.56
                                # Execution count [5.00e+00]
        movq      1296(%rsp), %rcx                              #588.7[spill]
        movl      $1, %esi                                      #588.7
        cmpl      4(%rcx), %ebx                                 #588.7
        cmove     %esi, %ebp                                    #588.7
        cmpl      $-1, 28(%rsp)                                 #589.23
        jge       ..B4.66       # Prob 50%                      #589.23
                                # LOE ebx ebp
..B4.59:                        # Preds ..B4.58
                                # Execution count [2.50e+00]
        movl      28(%rsp), %r14d                               #590.7
        movl      %r14d, %ecx                                   #590.7
        negl      %ecx                                          #590.7
        movq      16(%rsp), %r9                                 #590.7
        movq      40(%rsp), %r8                                 #590.7
        movl      32(%rsp), %r10d                               #590.7
        movq      1152(%rsp), %rdi                              #590.43
        testl     %ecx, %ecx                                    #590.7
        jle       ..B4.76       # Prob 50%                      #590.7
                                # LOE rdi r8 r9 ecx ebx ebp r10d r14d
..B4.60:                        # Preds ..B4.59
                                # Execution count [2.50e+00]
        movl      %ecx, %r11d                                   #590.7
        movl      $1, %r13d                                     #590.7
        shrl      $31, %r11d                                    #590.7
        xorl      %esi, %esi                                    #590.7
        subl      %r14d, %r11d                                  #590.7
        sarl      $1, %r11d                                     #590.7
        movl      24(%r9), %r12d                                #590.7
        testl     %r11d, %r11d                                  #590.7
        jbe       ..B4.64       # Prob 10%                      #590.7
                                # LOE rdi r8 r9 ecx ebx ebp esi r10d r11d r12d r13d
..B4.61:                        # Preds ..B4.60
                                # Execution count [2.25e+00]
        movl      %ebx, %r13d                                   #590.7
        imull     %r12d, %r13d                                  #590.7
        .align    16,0x90
                                # LOE rdi r8 r9 ecx ebx ebp esi r10d r11d r12d r13d
..B4.62:                        # Preds ..B4.62 ..B4.61
                                # Execution count [6.25e+00]
        movq      32(%r9), %r14                                 #590.7
        lea       (%r13,%rsi,2), %eax                           #590.7
        movslq    %eax, %rax                                    #590.7
        lea       (%rsi,%rsi), %edx                             #590.7
        movslq    %edx, %rdx                                    #590.7
        incl      %esi                                          #590.7
        movl      (%r14,%rax,4), %r15d                          #590.7
        imull     %r10d, %r15d                                  #590.7
        movslq    %r15d, %r15                                   #590.7
        addq      %r8, %r15                                     #590.7
        movq      %r15, (%rdi,%rdx,8)                           #590.7
        movq      32(%r9), %r14                                 #590.7
        movl      4(%r14,%rax,4), %r14d                         #590.7
        imull     %r10d, %r14d                                  #590.7
        movslq    %r14d, %r14                                   #590.7
        addq      %r8, %r14                                     #590.7
        movq      %r14, 8(%rdi,%rdx,8)                          #590.7
        cmpl      %r11d, %esi                                   #590.7
        jb        ..B4.62       # Prob 64%                      #590.7
                                # LOE rdi r8 r9 ecx ebx ebp esi r10d r11d r12d r13d
..B4.63:                        # Preds ..B4.62
                                # Execution count [2.25e+00]
        lea       1(%rsi,%rsi), %r13d                           #590.7
                                # LOE rdi r8 r9 ecx ebx ebp r10d r12d r13d
..B4.64:                        # Preds ..B4.63 ..B4.60
                                # Execution count [2.50e+00]
        lea       -1(%r13), %esi                                #590.7
        cmpl      %ecx, %esi                                    #590.7
        jae       ..B4.76       # Prob 10%                      #590.7
                                # LOE rdi r8 r9 ebx ebp r10d r12d r13d
..B4.65:                        # Preds ..B4.64
                                # Execution count [2.25e+00]
        imull     %ebx, %r12d                                   #590.7
        movslq    %r13d, %r13                                   #590.7
        movslq    %r12d, %r12                                   #590.7
        addq      %r13, %r12                                    #590.7
        movq      32(%r9), %r9                                  #590.7
        movl      -4(%r9,%r12,4), %ecx                          #590.7
        imull     %ecx, %r10d                                   #590.7
        movslq    %r10d, %r10                                   #590.7
        addq      %r10, %r8                                     #590.7
        movq      %r8, -8(%rdi,%r13,8)                          #590.7
        jmp       ..B4.76       # Prob 100%                     #590.7
                                # LOE rdi ebx ebp
..B4.66:                        # Preds ..B4.58
                                # Execution count [2.50e+00]
        movups    (%rsp), %xmm0                                 #592.7
        movups    16(%rsp), %xmm1                               #592.7
        movups    32(%rsp), %xmm2                               #592.7
        movups    48(%rsp), %xmm3                               #592.7
        movups    64(%rsp), %xmm4                               #592.7
        movups    80(%rsp), %xmm5                               #592.7
        movq      40(%rsp), %rdi                                #592.7
        movups    %xmm0, 576(%rsp)                              #592.7
        movups    %xmm1, 592(%rsp)                              #592.7
        movups    %xmm2, 608(%rsp)                              #592.7
        movups    %xmm3, 624(%rsp)                              #592.7
        movups    %xmm4, 640(%rsp)                              #592.7
        movups    %xmm5, 656(%rsp)                              #592.7
        cmpl      $0, 84(%rsp)                                  #592.7
        jne       ..B4.72       # Prob 50%                      #592.7
                                # LOE rdi ebx ebp
..B4.67:                        # Preds ..B4.66
                                # Execution count [1.25e+00]
        testl     %ebp, %ebp                                    #592.7
        je        ..B4.70       # Prob 50%                      #592.7
                                # LOE rdi ebx ebp
..B4.68:                        # Preds ..B4.67
                                # Execution count [6.25e-01]
        cmpl      $0, 80(%rsp)                                  #592.7
        je        ..B4.70       # Prob 50%                      #592.7
                                # LOE rdi ebx ebp
..B4.69:                        # Preds ..B4.68
                                # Execution count [3.12e-01]
        movq      blank_args(%rip), %rdi                        #592.7
        movq      %rdi, 1152(%rsp)                              #592.31
        jmp       ..B4.76       # Prob 100%                     #592.31
                                # LOE rdi ebx ebp
..B4.70:                        # Preds ..B4.67 ..B4.68
                                # Execution count [3.12e-01]
        movq      %rdi, 1152(%rsp)                              #592.31
        jmp       ..B4.76       # Prob 100%                     #592.31
                                # LOE rdi ebx ebp
..B4.72:                        # Preds ..B4.66
                                # Execution count [1.25e+00]
        movq      592(%rsp), %rsi                               #592.7
        testq     %rsi, %rsi                                    #592.7
        je        ..B4.74       # Prob 12%                      #592.7
                                # LOE rsi rdi ebx ebp
..B4.73:                        # Preds ..B4.72
                                # Execution count [1.10e+00]
        cmpl      $0, 668(%rsp)                                 #592.7
        jne       ..B4.75       # Prob 50%                      #592.7
                                # LOE rsi rdi ebx ebp
..B4.74:                        # Preds ..B4.72 ..B4.73
                                # Execution count [7.00e-01]
        movslq    %ebx, %rbx                                    #592.7
        movslq    608(%rsp), %rcx                               #592.7
        imulq     %rbx, %rcx                                    #592.7
        addq      %rcx, %rdi                                    #592.7
        movq      %rdi, 1152(%rsp)                              #592.31
        jmp       ..B4.76       # Prob 100%                     #592.31
                                # LOE rdi ebx ebp
..B4.75:                        # Preds ..B4.73
                                # Execution count [5.50e-01]
        movl      %ebx, %ecx                                    #592.7
        imull     24(%rsi), %ecx                                #592.7
        addl      604(%rsp), %ecx                               #592.7
        movslq    %ecx, %rcx                                    #592.7
        movq      32(%rsi), %rsi                                #592.7
        movl      608(%rsp), %r8d                               #592.7
        imull     (%rsi,%rcx,4), %r8d                           #592.7
        movslq    %r8d, %r8                                     #592.7
        addq      %r8, %rdi                                     #592.7
        movq      %rdi, 1152(%rsp)                              #592.31
                                # LOE rdi ebx ebp
..B4.76:                        # Preds ..B4.65 ..B4.64 ..B4.59 ..B4.69 ..B4.70
                                #       ..B4.74 ..B4.75
                                # Execution count [5.00e+00]
        cmpl      $-1, 124(%rsp)                                #593.23
        jge       ..B4.84       # Prob 50%                      #593.23
                                # LOE rdi ebx ebp
..B4.77:                        # Preds ..B4.76
                                # Execution count [2.50e+00]
        movl      124(%rsp), %r15d                              #594.7
        movl      %r15d, %r8d                                   #594.7
        negl      %r8d                                          #594.7
        movq      112(%rsp), %r9                                #594.7
        movq      136(%rsp), %r14                               #594.7
        movl      128(%rsp), %ecx                               #594.7
        movq      1160(%rsp), %rsi                              #594.43
        testl     %r8d, %r8d                                    #594.7
        jle       ..B4.94       # Prob 50%                      #594.7
                                # LOE rsi rdi r9 r14 ecx ebx ebp r8d r15d
..B4.78:                        # Preds ..B4.77
                                # Execution count [2.50e+00]
        movl      %r8d, %r12d                                   #594.7
        movl      $1, %r10d                                     #594.7
        shrl      $31, %r12d                                    #594.7
        xorl      %r11d, %r11d                                  #594.7
        subl      %r15d, %r12d                                  #594.7
        sarl      $1, %r12d                                     #594.7
        movl      24(%r9), %r13d                                #594.7
        testl     %r12d, %r12d                                  #594.7
        jbe       ..B4.82       # Prob 10%                      #594.7
                                # LOE rsi rdi r9 r14 ecx ebx ebp r8d r10d r11d r12d r13d
..B4.79:                        # Preds ..B4.78
                                # Execution count [2.25e+00]
        movl      %ebx, %r10d                                   #594.7
        imull     %r13d, %r10d                                  #594.7
        .align    16,0x90
                                # LOE rsi rdi r9 r14 ecx ebx ebp r8d r10d r11d r12d r13d
..B4.80:                        # Preds ..B4.80 ..B4.79
                                # Execution count [6.25e+00]
        movq      32(%r9), %r15                                 #594.7
        lea       (%r10,%r11,2), %eax                           #594.7
        movslq    %eax, %rax                                    #594.7
        lea       (%r11,%r11), %edx                             #594.7
        movslq    %edx, %rdx                                    #594.7
        incl      %r11d                                         #594.7
        movl      (%r15,%rax,4), %r15d                          #594.7
        imull     %ecx, %r15d                                   #594.7
        movslq    %r15d, %r15                                   #594.7
        addq      %r14, %r15                                    #594.7
        movq      %r15, (%rsi,%rdx,8)                           #594.7
        movq      32(%r9), %r15                                 #594.7
        movl      4(%r15,%rax,4), %r15d                         #594.7
        imull     %ecx, %r15d                                   #594.7
        movslq    %r15d, %r15                                   #594.7
        addq      %r14, %r15                                    #594.7
        movq      %r15, 8(%rsi,%rdx,8)                          #594.7
        cmpl      %r12d, %r11d                                  #594.7
        jb        ..B4.80       # Prob 64%                      #594.7
                                # LOE rsi rdi r9 r14 ecx ebx ebp r8d r10d r11d r12d r13d
..B4.81:                        # Preds ..B4.80
                                # Execution count [2.25e+00]
        lea       1(%r11,%r11), %r10d                           #594.7
                                # LOE rsi rdi r9 r14 ecx ebx ebp r8d r10d r13d
..B4.82:                        # Preds ..B4.81 ..B4.78
                                # Execution count [2.50e+00]
        lea       -1(%r10), %r11d                               #594.7
        cmpl      %r8d, %r11d                                   #594.7
        jae       ..B4.94       # Prob 10%                      #594.7
                                # LOE rsi rdi r9 r14 ecx ebx ebp r10d r13d
..B4.83:                        # Preds ..B4.82
                                # Execution count [2.25e+00]
        imull     %ebx, %r13d                                   #594.7
        movslq    %r10d, %r10                                   #594.7
        movslq    %r13d, %r13                                   #594.7
        addq      %r10, %r13                                    #594.7
        movq      32(%r9), %r9                                  #594.7
        movl      -4(%r9,%r13,4), %r8d                          #594.7
        imull     %r8d, %ecx                                    #594.7
        movslq    %ecx, %rcx                                    #594.7
        addq      %rcx, %r14                                    #594.7
        movq      %r14, -8(%rsi,%r10,8)                         #594.7
        jmp       ..B4.94       # Prob 100%                     #594.7
                                # LOE rsi rdi ebx ebp
..B4.84:                        # Preds ..B4.76
                                # Execution count [2.50e+00]
        movups    96(%rsp), %xmm0                               #596.7
        movups    112(%rsp), %xmm1                              #596.7
        movups    128(%rsp), %xmm2                              #596.7
        movups    144(%rsp), %xmm3                              #596.7
        movups    160(%rsp), %xmm4                              #596.7
        movups    176(%rsp), %xmm5                              #596.7
        movq      136(%rsp), %rsi                               #596.7
        movups    %xmm0, 672(%rsp)                              #596.7
        movups    %xmm1, 688(%rsp)                              #596.7
        movups    %xmm2, 704(%rsp)                              #596.7
        movups    %xmm3, 720(%rsp)                              #596.7
        movups    %xmm4, 736(%rsp)                              #596.7
        movups    %xmm5, 752(%rsp)                              #596.7
        cmpl      $0, 180(%rsp)                                 #596.7
        jne       ..B4.90       # Prob 50%                      #596.7
                                # LOE rsi rdi ebx ebp
..B4.85:                        # Preds ..B4.84
                                # Execution count [1.25e+00]
        testl     %ebp, %ebp                                    #596.7
        je        ..B4.88       # Prob 50%                      #596.7
                                # LOE rsi rdi ebx ebp
..B4.86:                        # Preds ..B4.85
                                # Execution count [6.25e-01]
        cmpl      $0, 176(%rsp)                                 #596.7
        je        ..B4.88       # Prob 50%                      #596.7
                                # LOE rsi rdi ebx ebp
..B4.87:                        # Preds ..B4.86
                                # Execution count [3.12e-01]
        movq      blank_args(%rip), %rsi                        #596.7
        movq      %rsi, 1160(%rsp)                              #596.7
        jmp       ..B4.94       # Prob 100%                     #596.7
                                # LOE rsi rdi ebx ebp
..B4.88:                        # Preds ..B4.85 ..B4.86
                                # Execution count [3.12e-01]
        movq      %rsi, 1160(%rsp)                              #596.7
        jmp       ..B4.94       # Prob 100%                     #596.7
                                # LOE rsi rdi ebx ebp
..B4.90:                        # Preds ..B4.84
                                # Execution count [1.25e+00]
        movq      688(%rsp), %r8                                #596.7
        testq     %r8, %r8                                      #596.7
        je        ..B4.92       # Prob 12%                      #596.7
                                # LOE rsi rdi r8 ebx ebp
..B4.91:                        # Preds ..B4.90
                                # Execution count [1.10e+00]
        cmpl      $0, 764(%rsp)                                 #596.7
        jne       ..B4.93       # Prob 50%                      #596.7
                                # LOE rsi rdi r8 ebx ebp
..B4.92:                        # Preds ..B4.91 ..B4.90
                                # Execution count [7.00e-01]
        movslq    %ebx, %rbx                                    #596.7
        movslq    704(%rsp), %rcx                               #596.7
        imulq     %rbx, %rcx                                    #596.7
        addq      %rcx, %rsi                                    #596.7
        movq      %rsi, 1160(%rsp)                              #596.7
        jmp       ..B4.94       # Prob 100%                     #596.7
                                # LOE rsi rdi ebx ebp
..B4.93:                        # Preds ..B4.91
                                # Execution count [5.50e-01]
        movl      %ebx, %ecx                                    #596.7
        imull     24(%r8), %ecx                                 #596.7
        addl      700(%rsp), %ecx                               #596.7
        movslq    %ecx, %rcx                                    #596.7
        movq      32(%r8), %r8                                  #596.7
        movl      704(%rsp), %r9d                               #596.7
        imull     (%r8,%rcx,4), %r9d                            #596.7
        movslq    %r9d, %r9                                     #596.7
        addq      %r9, %rsi                                     #596.7
        movq      %rsi, 1160(%rsp)                              #596.7
                                # LOE rsi rdi ebx ebp
..B4.94:                        # Preds ..B4.83 ..B4.82 ..B4.77 ..B4.87 ..B4.88
                                #       ..B4.92 ..B4.93
                                # Execution count [5.00e+00]
        cmpl      $-1, 220(%rsp)                                #597.23
        jge       ..B4.102      # Prob 50%                      #597.23
                                # LOE rsi rdi ebx ebp
..B4.95:                        # Preds ..B4.94
                                # Execution count [2.50e+00]
        movl      220(%rsp), %r10d                              #598.7
        movl      %r10d, %r9d                                   #598.7
        negl      %r9d                                          #598.7
        movq      208(%rsp), %r15                               #598.7
        movq      232(%rsp), %r11                               #598.7
        movl      224(%rsp), %r8d                               #598.7
        movq      1168(%rsp), %rdx                              #598.43
        testl     %r9d, %r9d                                    #598.7
        jle       ..B4.112      # Prob 50%                      #598.7
                                # LOE rdx rsi rdi r11 r15 ebx ebp r8d r9d r10d
..B4.96:                        # Preds ..B4.95
                                # Execution count [2.50e+00]
        movl      %r9d, %r13d                                   #598.7
        movl      $1, %ecx                                      #598.7
        shrl      $31, %r13d                                    #598.7
        xorl      %r12d, %r12d                                  #598.7
        subl      %r10d, %r13d                                  #598.7
        sarl      $1, %r13d                                     #598.7
        movl      24(%r15), %r14d                               #598.7
        testl     %r13d, %r13d                                  #598.7
        jbe       ..B4.100      # Prob 10%                      #598.7
                                # LOE rdx rsi rdi r11 r15 ecx ebx ebp r8d r9d r12d r13d r14d
..B4.97:                        # Preds ..B4.96
                                # Execution count [2.25e+00]
        movl      %ebx, %r10d                                   #598.7
        imull     %r14d, %r10d                                  #598.7
        movl      %ebp, 1232(%rsp)                              #598.7[spill]
        .align    16,0x90
                                # LOE rdx rsi rdi r11 r15 ebx r8d r9d r10d r12d r13d r14d
..B4.98:                        # Preds ..B4.98 ..B4.97
                                # Execution count [6.25e+00]
        movq      32(%r15), %rbp                                #598.7
        lea       (%r10,%r12,2), %eax                           #598.7
        movslq    %eax, %rax                                    #598.7
        lea       (%r12,%r12), %ecx                             #598.7
        movslq    %ecx, %rcx                                    #598.7
        incl      %r12d                                         #598.7
        movl      (%rbp,%rax,4), %ebp                           #598.7
        imull     %r8d, %ebp                                    #598.7
        movslq    %ebp, %rbp                                    #598.7
        addq      %r11, %rbp                                    #598.7
        movq      %rbp, (%rdx,%rcx,8)                           #598.7
        movq      32(%r15), %rbp                                #598.7
        movl      4(%rbp,%rax,4), %ebp                          #598.7
        imull     %r8d, %ebp                                    #598.7
        movslq    %ebp, %rbp                                    #598.7
        addq      %r11, %rbp                                    #598.7
        movq      %rbp, 8(%rdx,%rcx,8)                          #598.7
        cmpl      %r13d, %r12d                                  #598.7
        jb        ..B4.98       # Prob 64%                      #598.7
                                # LOE rdx rsi rdi r11 r15 ebx r8d r9d r10d r12d r13d r14d
..B4.99:                        # Preds ..B4.98
                                # Execution count [2.25e+00]
        movl      1232(%rsp), %ebp                              #[spill]
        lea       1(%r12,%r12), %ecx                            #598.7
                                # LOE rdx rsi rdi r11 r15 ecx ebx ebp r8d r9d r14d
..B4.100:                       # Preds ..B4.99 ..B4.96
                                # Execution count [2.50e+00]
        lea       -1(%rcx), %r10d                               #598.7
        cmpl      %r9d, %r10d                                   #598.7
        jae       ..B4.112      # Prob 10%                      #598.7
                                # LOE rdx rsi rdi r11 r15 ecx ebx ebp r8d r14d
..B4.101:                       # Preds ..B4.100
                                # Execution count [2.25e+00]
        imull     %ebx, %r14d                                   #598.7
        movslq    %ecx, %rcx                                    #598.7
        movslq    %r14d, %r14                                   #598.7
        addq      %rcx, %r14                                    #598.7
        movq      32(%r15), %r15                                #598.7
        movl      -4(%r15,%r14,4), %r9d                         #598.7
        imull     %r9d, %r8d                                    #598.7
        movslq    %r8d, %r8                                     #598.7
        addq      %r8, %r11                                     #598.7
        movq      %r11, -8(%rdx,%rcx,8)                         #598.7
        jmp       ..B4.112      # Prob 100%                     #598.7
                                # LOE rdx rsi rdi ebx ebp
..B4.102:                       # Preds ..B4.94
                                # Execution count [2.50e+00]
        movups    192(%rsp), %xmm0                              #600.7
        movups    208(%rsp), %xmm1                              #600.7
        movups    224(%rsp), %xmm2                              #600.7
        movups    240(%rsp), %xmm3                              #600.7
        movups    256(%rsp), %xmm4                              #600.7
        movups    272(%rsp), %xmm5                              #600.7
        movq      232(%rsp), %rdx                               #600.7
        movups    %xmm0, 768(%rsp)                              #600.7
        movups    %xmm1, 784(%rsp)                              #600.7
        movups    %xmm2, 800(%rsp)                              #600.7
        movups    %xmm3, 816(%rsp)                              #600.7
        movups    %xmm4, 832(%rsp)                              #600.7
        movups    %xmm5, 848(%rsp)                              #600.7
        cmpl      $0, 276(%rsp)                                 #600.7
        jne       ..B4.108      # Prob 50%                      #600.7
                                # LOE rdx rsi rdi ebx ebp
..B4.103:                       # Preds ..B4.102
                                # Execution count [1.25e+00]
        testl     %ebp, %ebp                                    #600.7
        je        ..B4.106      # Prob 50%                      #600.7
                                # LOE rdx rsi rdi ebx ebp
..B4.104:                       # Preds ..B4.103
                                # Execution count [6.25e-01]
        cmpl      $0, 272(%rsp)                                 #600.7
        je        ..B4.106      # Prob 50%                      #600.7
                                # LOE rdx rsi rdi ebx ebp
..B4.105:                       # Preds ..B4.104
                                # Execution count [3.12e-01]
        movq      blank_args(%rip), %rdx                        #600.7
        movq      %rdx, 1168(%rsp)                              #600.7
        jmp       ..B4.112      # Prob 100%                     #600.7
                                # LOE rdx rsi rdi ebx ebp
..B4.106:                       # Preds ..B4.103 ..B4.104
                                # Execution count [3.12e-01]
        movq      %rdx, 1168(%rsp)                              #600.7
        jmp       ..B4.112      # Prob 100%                     #600.7
                                # LOE rdx rsi rdi ebx ebp
..B4.108:                       # Preds ..B4.102
                                # Execution count [1.25e+00]
        movq      784(%rsp), %r8                                #600.7
        testq     %r8, %r8                                      #600.7
        je        ..B4.110      # Prob 12%                      #600.7
                                # LOE rdx rsi rdi r8 ebx ebp
..B4.109:                       # Preds ..B4.108
                                # Execution count [1.10e+00]
        cmpl      $0, 860(%rsp)                                 #600.7
        jne       ..B4.111      # Prob 50%                      #600.7
                                # LOE rdx rsi rdi r8 ebx ebp
..B4.110:                       # Preds ..B4.109 ..B4.108
                                # Execution count [7.00e-01]
        movslq    %ebx, %rbx                                    #600.7
        movslq    800(%rsp), %rcx                               #600.7
        imulq     %rbx, %rcx                                    #600.7
        addq      %rcx, %rdx                                    #600.7
        movq      %rdx, 1168(%rsp)                              #600.7
        jmp       ..B4.112      # Prob 100%                     #600.7
                                # LOE rdx rsi rdi ebx ebp
..B4.111:                       # Preds ..B4.109
                                # Execution count [5.50e-01]
        movl      %ebx, %ecx                                    #600.7
        imull     24(%r8), %ecx                                 #600.7
        addl      796(%rsp), %ecx                               #600.7
        movslq    %ecx, %rcx                                    #600.7
        movq      32(%r8), %r8                                  #600.7
        movl      800(%rsp), %r9d                               #600.7
        imull     (%r8,%rcx,4), %r9d                            #600.7
        movslq    %r9d, %r9                                     #600.7
        addq      %r9, %rdx                                     #600.7
        movq      %rdx, 1168(%rsp)                              #600.7
                                # LOE rdx rsi rdi ebx ebp
..B4.112:                       # Preds ..B4.101 ..B4.100 ..B4.95 ..B4.105 ..B4.106
                                #       ..B4.110 ..B4.111
                                # Execution count [5.00e+00]
        cmpl      $-1, 316(%rsp)                                #601.23
        jge       ..B4.120      # Prob 50%                      #601.23
                                # LOE rdx rsi rdi ebx ebp
..B4.113:                       # Preds ..B4.112
                                # Execution count [2.50e+00]
        movl      316(%rsp), %r15d                              #602.7
        movl      %r15d, %r13d                                  #602.7
        negl      %r13d                                         #602.7
        movq      304(%rsp), %r10                               #602.7
        movq      328(%rsp), %r11                               #602.7
        movl      320(%rsp), %r12d                              #602.7
        movq      1176(%rsp), %rcx                              #602.43
        testl     %r13d, %r13d                                  #602.7
        jle       ..B4.130      # Prob 50%                      #602.7
                                # LOE rdx rcx rsi rdi r10 r11 ebx ebp r12d r13d r15d
..B4.114:                       # Preds ..B4.113
                                # Execution count [2.50e+00]
        movl      %r13d, %r8d                                   #602.7
        movl      $1, %eax                                      #602.7
        shrl      $31, %r8d                                     #602.7
        xorl      %r9d, %r9d                                    #602.7
        subl      %r15d, %r8d                                   #602.7
        sarl      $1, %r8d                                      #602.7
        movl      24(%r10), %r14d                               #602.7
        testl     %r8d, %r8d                                    #602.7
        jbe       ..B4.118      # Prob 10%                      #602.7
                                # LOE rdx rcx rsi rdi r10 r11 eax ebx ebp r8d r9d r12d r13d r14d
..B4.115:                       # Preds ..B4.114
                                # Execution count [2.25e+00]
        movl      %ebx, %eax                                    #602.7
        imull     %r14d, %eax                                   #602.7
        movl      %ebx, 1240(%rsp)                              #602.7[spill]
        movl      %ebp, 1232(%rsp)                              #602.7[spill]
        .align    16,0x90
                                # LOE rdx rcx rsi rdi r10 r11 eax r8d r9d r12d r13d r14d
..B4.116:                       # Preds ..B4.116 ..B4.115
                                # Execution count [6.25e+00]
        movq      32(%r10), %r15                                #602.7
        lea       (%rax,%r9,2), %ebx                            #602.7
        movslq    %ebx, %rbx                                    #602.7
        lea       (%r9,%r9), %ebp                               #602.7
        movslq    %ebp, %rbp                                    #602.7
        incl      %r9d                                          #602.7
        movl      (%r15,%rbx,4), %r15d                          #602.7
        imull     %r12d, %r15d                                  #602.7
        movslq    %r15d, %r15                                   #602.7
        addq      %r11, %r15                                    #602.7
        movq      %r15, (%rcx,%rbp,8)                           #602.7
        movq      32(%r10), %r15                                #602.7
        movl      4(%r15,%rbx,4), %ebx                          #602.7
        imull     %r12d, %ebx                                   #602.7
        movslq    %ebx, %rbx                                    #602.7
        addq      %r11, %rbx                                    #602.7
        movq      %rbx, 8(%rcx,%rbp,8)                          #602.7
        cmpl      %r8d, %r9d                                    #602.7
        jb        ..B4.116      # Prob 64%                      #602.7
                                # LOE rdx rcx rsi rdi r10 r11 eax r8d r9d r12d r13d r14d
..B4.117:                       # Preds ..B4.116
                                # Execution count [2.25e+00]
        movl      1240(%rsp), %ebx                              #[spill]
        lea       1(%r9,%r9), %eax                              #602.7
        movl      1232(%rsp), %ebp                              #[spill]
                                # LOE rdx rcx rsi rdi r10 r11 eax ebx ebp r12d r13d r14d
..B4.118:                       # Preds ..B4.117 ..B4.114
                                # Execution count [2.50e+00]
        lea       -1(%rax), %r8d                                #602.7
        cmpl      %r13d, %r8d                                   #602.7
        jae       ..B4.130      # Prob 10%                      #602.7
                                # LOE rdx rcx rsi rdi r10 r11 eax ebx ebp r12d r14d
..B4.119:                       # Preds ..B4.118
                                # Execution count [2.25e+00]
        imull     %ebx, %r14d                                   #602.7
        movslq    %eax, %rax                                    #602.7
        movslq    %r14d, %r14                                   #602.7
        addq      %rax, %r14                                    #602.7
        movq      32(%r10), %r10                                #602.7
        movl      -4(%r10,%r14,4), %r8d                         #602.7
        imull     %r8d, %r12d                                   #602.7
        movslq    %r12d, %r12                                   #602.7
        addq      %r12, %r11                                    #602.7
        movq      %r11, -8(%rcx,%rax,8)                         #602.7
        jmp       ..B4.130      # Prob 100%                     #602.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B4.120:                       # Preds ..B4.112
                                # Execution count [2.50e+00]
        movups    288(%rsp), %xmm0                              #604.7
        movups    304(%rsp), %xmm1                              #604.7
        movups    320(%rsp), %xmm2                              #604.7
        movups    336(%rsp), %xmm3                              #604.7
        movups    352(%rsp), %xmm4                              #604.7
        movups    368(%rsp), %xmm5                              #604.7
        movq      328(%rsp), %rcx                               #604.7
        movups    %xmm0, 864(%rsp)                              #604.7
        movups    %xmm1, 880(%rsp)                              #604.7
        movups    %xmm2, 896(%rsp)                              #604.7
        movups    %xmm3, 912(%rsp)                              #604.7
        movups    %xmm4, 928(%rsp)                              #604.7
        movups    %xmm5, 944(%rsp)                              #604.7
        cmpl      $0, 372(%rsp)                                 #604.7
        jne       ..B4.126      # Prob 50%                      #604.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B4.121:                       # Preds ..B4.120
                                # Execution count [1.25e+00]
        testl     %ebp, %ebp                                    #604.7
        je        ..B4.124      # Prob 50%                      #604.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B4.122:                       # Preds ..B4.121
                                # Execution count [6.25e-01]
        cmpl      $0, 368(%rsp)                                 #604.7
        je        ..B4.124      # Prob 50%                      #604.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B4.123:                       # Preds ..B4.122
                                # Execution count [3.12e-01]
        movq      blank_args(%rip), %rcx                        #604.7
        movq      %rcx, 1176(%rsp)                              #604.7
        jmp       ..B4.130      # Prob 100%                     #604.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B4.124:                       # Preds ..B4.121 ..B4.122
                                # Execution count [3.12e-01]
        movq      %rcx, 1176(%rsp)                              #604.7
        jmp       ..B4.130      # Prob 100%                     #604.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B4.126:                       # Preds ..B4.120
                                # Execution count [1.25e+00]
        movq      880(%rsp), %r9                                #604.7
        testq     %r9, %r9                                      #604.7
        je        ..B4.128      # Prob 12%                      #604.7
                                # LOE rdx rcx rsi rdi r9 ebx ebp
..B4.127:                       # Preds ..B4.126
                                # Execution count [1.10e+00]
        cmpl      $0, 956(%rsp)                                 #604.7
        jne       ..B4.129      # Prob 50%                      #604.7
                                # LOE rdx rcx rsi rdi r9 ebx ebp
..B4.128:                       # Preds ..B4.127 ..B4.126
                                # Execution count [7.00e-01]
        movslq    %ebx, %rbx                                    #604.7
        movslq    896(%rsp), %r8                                #604.7
        imulq     %rbx, %r8                                     #604.7
        addq      %r8, %rcx                                     #604.7
        movq      %rcx, 1176(%rsp)                              #604.7
        jmp       ..B4.130      # Prob 100%                     #604.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B4.129:                       # Preds ..B4.127
                                # Execution count [5.50e-01]
        movl      %ebx, %r8d                                    #604.7
        imull     24(%r9), %r8d                                 #604.7
        addl      892(%rsp), %r8d                               #604.7
        movslq    %r8d, %r8                                     #604.7
        movq      32(%r9), %r9                                  #604.7
        movl      896(%rsp), %r10d                              #604.7
        imull     (%r9,%r8,4), %r10d                            #604.7
        movslq    %r10d, %r10                                   #604.7
        addq      %r10, %rcx                                    #604.7
        movq      %rcx, 1176(%rsp)                              #604.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B4.130:                       # Preds ..B4.119 ..B4.118 ..B4.113 ..B4.123 ..B4.124
                                #       ..B4.128 ..B4.129
                                # Execution count [5.00e+00]
        cmpl      $-1, 412(%rsp)                                #605.23
        jge       ..B4.138      # Prob 50%                      #605.23
                                # LOE rdx rcx rsi rdi ebx ebp
..B4.131:                       # Preds ..B4.130
                                # Execution count [2.50e+00]
        movl      412(%rsp), %r9d                               #606.7
        movl      %r9d, 1264(%rsp)                              #606.7[spill]
        negl      %r9d                                          #606.7
        movq      400(%rsp), %r11                               #606.7
        movq      424(%rsp), %r12                               #606.7
        movl      416(%rsp), %r13d                              #606.7
        movq      1184(%rsp), %r8                               #606.43
        testl     %r9d, %r9d                                    #606.7
        jle       ..B4.148      # Prob 50%                      #606.7
                                # LOE rdx rcx rsi rdi r8 r11 r12 ebx ebp r9d r13d
..B4.132:                       # Preds ..B4.131
                                # Execution count [2.50e+00]
        movl      %r9d, %r15d                                   #606.7
        movl      $1, %eax                                      #606.7
        shrl      $31, %r15d                                    #606.7
        xorl      %r10d, %r10d                                  #606.7
        subl      1264(%rsp), %r15d                             #606.7[spill]
        sarl      $1, %r15d                                     #606.7
        movl      24(%r11), %r14d                               #606.7
        testl     %r15d, %r15d                                  #606.7
        jbe       ..B4.136      # Prob 10%                      #606.7
                                # LOE rdx rcx rsi rdi r8 r11 r12 eax ebx ebp r9d r10d r13d r14d r15d
..B4.133:                       # Preds ..B4.132
                                # Execution count [2.25e+00]
        movl      %ebx, %eax                                    #606.7
        movq      %rdi, 1248(%rsp)                              #606.7[spill]
        imull     %r14d, %eax                                   #606.7
        movl      %ebx, 1240(%rsp)                              #606.7[spill]
        movl      %ebp, 1232(%rsp)                              #606.7[spill]
        movl      %r15d, %edi                                   #606.7
        .align    16,0x90
                                # LOE rdx rcx rsi r8 r11 r12 eax edi r9d r10d r13d r14d
..B4.134:                       # Preds ..B4.134 ..B4.133
                                # Execution count [6.25e+00]
        movq      32(%r11), %r15                                #606.7
        lea       (%rax,%r10,2), %ebx                           #606.7
        movslq    %ebx, %rbx                                    #606.7
        lea       (%r10,%r10), %ebp                             #606.7
        movslq    %ebp, %rbp                                    #606.7
        incl      %r10d                                         #606.7
        movl      (%r15,%rbx,4), %r15d                          #606.7
        imull     %r13d, %r15d                                  #606.7
        movslq    %r15d, %r15                                   #606.7
        addq      %r12, %r15                                    #606.7
        movq      %r15, (%r8,%rbp,8)                            #606.7
        movq      32(%r11), %r15                                #606.7
        movl      4(%r15,%rbx,4), %ebx                          #606.7
        imull     %r13d, %ebx                                   #606.7
        movslq    %ebx, %rbx                                    #606.7
        addq      %r12, %rbx                                    #606.7
        movq      %rbx, 8(%r8,%rbp,8)                           #606.7
        cmpl      %edi, %r10d                                   #606.7
        jb        ..B4.134      # Prob 64%                      #606.7
                                # LOE rdx rcx rsi r8 r11 r12 eax edi r9d r10d r13d r14d
..B4.135:                       # Preds ..B4.134
                                # Execution count [2.25e+00]
        movq      1248(%rsp), %rdi                              #[spill]
        lea       1(%r10,%r10), %eax                            #606.7
        movl      1240(%rsp), %ebx                              #[spill]
        movl      1232(%rsp), %ebp                              #[spill]
                                # LOE rdx rcx rsi rdi r8 r11 r12 eax ebx ebp r9d r13d r14d
..B4.136:                       # Preds ..B4.135 ..B4.132
                                # Execution count [2.50e+00]
        lea       -1(%rax), %r10d                               #606.7
        cmpl      %r9d, %r10d                                   #606.7
        jae       ..B4.148      # Prob 10%                      #606.7
                                # LOE rdx rcx rsi rdi r8 r11 r12 eax ebx ebp r13d r14d
..B4.137:                       # Preds ..B4.136
                                # Execution count [2.25e+00]
        imull     %ebx, %r14d                                   #606.7
        movslq    %eax, %rax                                    #606.7
        movslq    %r14d, %r14                                   #606.7
        addq      %rax, %r14                                    #606.7
        movq      32(%r11), %r11                                #606.7
        movl      -4(%r11,%r14,4), %r9d                         #606.7
        imull     %r9d, %r13d                                   #606.7
        movslq    %r13d, %r13                                   #606.7
        addq      %r13, %r12                                    #606.7
        movq      %r12, -8(%r8,%rax,8)                          #606.7
        jmp       ..B4.148      # Prob 100%                     #606.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B4.138:                       # Preds ..B4.130
                                # Execution count [2.50e+00]
        movups    384(%rsp), %xmm0                              #608.7
        movups    400(%rsp), %xmm1                              #608.7
        movups    416(%rsp), %xmm2                              #608.7
        movups    432(%rsp), %xmm3                              #608.7
        movups    448(%rsp), %xmm4                              #608.7
        movups    464(%rsp), %xmm5                              #608.7
        movq      424(%rsp), %r8                                #608.7
        movups    %xmm0, 960(%rsp)                              #608.7
        movups    %xmm1, 976(%rsp)                              #608.7
        movups    %xmm2, 992(%rsp)                              #608.7
        movups    %xmm3, 1008(%rsp)                             #608.7
        movups    %xmm4, 1024(%rsp)                             #608.7
        movups    %xmm5, 1040(%rsp)                             #608.7
        cmpl      $0, 468(%rsp)                                 #608.7
        jne       ..B4.144      # Prob 50%                      #608.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B4.139:                       # Preds ..B4.138
                                # Execution count [1.25e+00]
        testl     %ebp, %ebp                                    #608.7
        je        ..B4.142      # Prob 50%                      #608.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B4.140:                       # Preds ..B4.139
                                # Execution count [6.25e-01]
        cmpl      $0, 464(%rsp)                                 #608.7
        je        ..B4.142      # Prob 50%                      #608.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B4.141:                       # Preds ..B4.140
                                # Execution count [3.12e-01]
        movq      blank_args(%rip), %r8                         #608.7
        movq      %r8, 1184(%rsp)                               #608.7
        jmp       ..B4.148      # Prob 100%                     #608.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B4.142:                       # Preds ..B4.139 ..B4.140
                                # Execution count [3.12e-01]
        movq      %r8, 1184(%rsp)                               #608.7
        jmp       ..B4.148      # Prob 100%                     #608.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B4.144:                       # Preds ..B4.138
                                # Execution count [1.25e+00]
        movq      976(%rsp), %r10                               #608.7
        testq     %r10, %r10                                    #608.7
        je        ..B4.146      # Prob 12%                      #608.7
                                # LOE rdx rcx rsi rdi r8 r10 ebx ebp
..B4.145:                       # Preds ..B4.144
                                # Execution count [1.10e+00]
        cmpl      $0, 1052(%rsp)                                #608.7
        jne       ..B4.147      # Prob 50%                      #608.7
                                # LOE rdx rcx rsi rdi r8 r10 ebx ebp
..B4.146:                       # Preds ..B4.145 ..B4.144
                                # Execution count [7.00e-01]
        movslq    %ebx, %rbx                                    #608.7
        movslq    992(%rsp), %r9                                #608.7
        imulq     %rbx, %r9                                     #608.7
        addq      %r9, %r8                                      #608.7
        movq      %r8, 1184(%rsp)                               #608.7
        jmp       ..B4.148      # Prob 100%                     #608.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B4.147:                       # Preds ..B4.145
                                # Execution count [5.50e-01]
        movl      %ebx, %r9d                                    #608.7
        imull     24(%r10), %r9d                                #608.7
        addl      988(%rsp), %r9d                               #608.7
        movslq    %r9d, %r9                                     #608.7
        movq      32(%r10), %r10                                #608.7
        movl      992(%rsp), %r11d                              #608.7
        imull     (%r10,%r9,4), %r11d                           #608.7
        movslq    %r11d, %r11                                   #608.7
        addq      %r11, %r8                                     #608.7
        movq      %r8, 1184(%rsp)                               #608.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B4.148:                       # Preds ..B4.137 ..B4.136 ..B4.131 ..B4.141 ..B4.142
                                #       ..B4.146 ..B4.147
                                # Execution count [5.00e+00]
        cmpl      $-1, 508(%rsp)                                #609.23
        jge       ..B4.156      # Prob 50%                      #609.23
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B4.149:                       # Preds ..B4.148
                                # Execution count [2.50e+00]
        movl      508(%rsp), %r10d                              #610.7
        movl      %r10d, 1256(%rsp)                             #610.7[spill]
        negl      %r10d                                         #610.7
        movq      496(%rsp), %r11                               #610.7
        movq      520(%rsp), %r12                               #610.7
        movl      512(%rsp), %r13d                              #610.7
        movq      1192(%rsp), %r9                               #610.43
        movl      %r10d, 1272(%rsp)                             #610.7[spill]
        testl     %r10d, %r10d                                  #610.7
        jle       ..B4.166      # Prob 50%                      #610.7
                                # LOE rdx rcx rsi rdi r8 r9 r11 r12 ebx ebp r10d r13d
..B4.150:                       # Preds ..B4.149
                                # Execution count [2.50e+00]
        movl      %r10d, %r15d                                  #610.7
        movl      $1, %eax                                      #610.7
        shrl      $31, %r15d                                    #610.7
        xorl      %r10d, %r10d                                  #610.7
        subl      1256(%rsp), %r15d                             #610.7[spill]
        sarl      $1, %r15d                                     #610.7
        movl      24(%r11), %r14d                               #610.7
        testl     %r15d, %r15d                                  #610.7
        jbe       ..B4.154      # Prob 10%                      #610.7
                                # LOE rdx rcx rsi rdi r8 r9 r11 r12 eax ebx ebp r10d r13d r14d r15d
..B4.151:                       # Preds ..B4.150
                                # Execution count [2.25e+00]
        movl      %ebx, %eax                                    #610.7
        movq      %rdi, 1248(%rsp)                              #610.7[spill]
        imull     %r14d, %eax                                   #610.7
        movl      %ebx, 1240(%rsp)                              #610.7[spill]
        movl      %ebp, 1232(%rsp)                              #610.7[spill]
        movl      %r15d, %edi                                   #610.7
        .align    16,0x90
                                # LOE rdx rcx rsi r8 r9 r11 r12 eax edi r10d r13d r14d
..B4.152:                       # Preds ..B4.152 ..B4.151
                                # Execution count [6.25e+00]
        movq      32(%r11), %r15                                #610.7
        lea       (%rax,%r10,2), %ebp                           #610.7
        movslq    %ebp, %rbp                                    #610.7
        lea       (%r10,%r10), %ebx                             #610.7
        movslq    %ebx, %rbx                                    #610.7
        incl      %r10d                                         #610.7
        movl      (%r15,%rbp,4), %r15d                          #610.7
        imull     %r13d, %r15d                                  #610.7
        movslq    %r15d, %r15                                   #610.7
        addq      %r12, %r15                                    #610.7
        movq      %r15, (%r9,%rbx,8)                            #610.7
        movq      32(%r11), %r15                                #610.7
        movl      4(%r15,%rbp,4), %ebp                          #610.7
        imull     %r13d, %ebp                                   #610.7
        movslq    %ebp, %rbp                                    #610.7
        addq      %r12, %rbp                                    #610.7
        movq      %rbp, 8(%r9,%rbx,8)                           #610.7
        cmpl      %edi, %r10d                                   #610.7
        jb        ..B4.152      # Prob 64%                      #610.7
                                # LOE rdx rcx rsi r8 r9 r11 r12 eax edi r10d r13d r14d
..B4.153:                       # Preds ..B4.152
                                # Execution count [2.25e+00]
        movq      1248(%rsp), %rdi                              #[spill]
        lea       1(%r10,%r10), %eax                            #610.7
        movl      1240(%rsp), %ebx                              #[spill]
        movl      1232(%rsp), %ebp                              #[spill]
                                # LOE rdx rcx rsi rdi r8 r9 r11 r12 eax ebx ebp r13d r14d
..B4.154:                       # Preds ..B4.153 ..B4.150
                                # Execution count [2.50e+00]
        lea       -1(%rax), %r10d                               #610.7
        cmpl      1272(%rsp), %r10d                             #610.7[spill]
        jae       ..B4.166      # Prob 10%                      #610.7
                                # LOE rdx rcx rsi rdi r8 r9 r11 r12 eax ebx ebp r13d r14d
..B4.155:                       # Preds ..B4.154
                                # Execution count [2.25e+00]
        imull     %ebx, %r14d                                   #610.7
        movslq    %eax, %rax                                    #610.7
        movslq    %r14d, %r14                                   #610.7
        addq      %rax, %r14                                    #610.7
        movq      32(%r11), %r11                                #610.7
        movl      -4(%r11,%r14,4), %r10d                        #610.7
        imull     %r10d, %r13d                                  #610.7
        movslq    %r13d, %r13                                   #610.7
        addq      %r13, %r12                                    #610.7
        movq      %r12, -8(%r9,%rax,8)                          #610.7
        jmp       ..B4.166      # Prob 100%                     #610.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B4.156:                       # Preds ..B4.148
                                # Execution count [2.50e+00]
        movups    480(%rsp), %xmm0                              #612.7
        movups    496(%rsp), %xmm1                              #612.7
        movups    512(%rsp), %xmm2                              #612.7
        movups    528(%rsp), %xmm3                              #612.7
        movups    544(%rsp), %xmm4                              #612.7
        movups    560(%rsp), %xmm5                              #612.7
        movq      520(%rsp), %r9                                #612.7
        movups    %xmm0, 1056(%rsp)                             #612.7
        movups    %xmm1, 1072(%rsp)                             #612.7
        movups    %xmm2, 1088(%rsp)                             #612.7
        movups    %xmm3, 1104(%rsp)                             #612.7
        movups    %xmm4, 1120(%rsp)                             #612.7
        movups    %xmm5, 1136(%rsp)                             #612.7
        cmpl      $0, 564(%rsp)                                 #612.7
        jne       ..B4.162      # Prob 50%                      #612.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B4.157:                       # Preds ..B4.156
                                # Execution count [1.25e+00]
        testl     %ebp, %ebp                                    #612.7
        je        ..B4.160      # Prob 50%                      #612.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B4.158:                       # Preds ..B4.157
                                # Execution count [6.25e-01]
        cmpl      $0, 560(%rsp)                                 #612.7
        je        ..B4.160      # Prob 50%                      #612.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B4.159:                       # Preds ..B4.158
                                # Execution count [3.12e-01]
        movq      blank_args(%rip), %r9                         #612.7
        movq      %r9, 1192(%rsp)                               #612.7
        jmp       ..B4.166      # Prob 100%                     #612.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B4.160:                       # Preds ..B4.157 ..B4.158
                                # Execution count [3.12e-01]
        movq      %r9, 1192(%rsp)                               #612.7
        jmp       ..B4.166      # Prob 100%                     #612.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B4.162:                       # Preds ..B4.156
                                # Execution count [1.25e+00]
        movq      1072(%rsp), %r11                              #612.7
        testq     %r11, %r11                                    #612.7
        je        ..B4.164      # Prob 12%                      #612.7
                                # LOE rdx rcx rsi rdi r8 r9 r11 ebx ebp
..B4.163:                       # Preds ..B4.162
                                # Execution count [1.10e+00]
        cmpl      $0, 1148(%rsp)                                #612.7
        jne       ..B4.165      # Prob 50%                      #612.7
                                # LOE rdx rcx rsi rdi r8 r9 r11 ebx ebp
..B4.164:                       # Preds ..B4.163 ..B4.162
                                # Execution count [7.00e-01]
        movslq    %ebx, %rbx                                    #612.7
        movslq    1088(%rsp), %r10                              #612.7
        imulq     %rbx, %r10                                    #612.7
        addq      %r10, %r9                                     #612.7
        movq      %r9, 1192(%rsp)                               #612.7
        jmp       ..B4.166      # Prob 100%                     #612.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B4.165:                       # Preds ..B4.163
                                # Execution count [5.50e-01]
        movl      %ebx, %r10d                                   #612.7
        imull     24(%r11), %r10d                               #612.7
        addl      1084(%rsp), %r10d                             #612.7
        movslq    %r10d, %r10                                   #612.7
        movq      32(%r11), %r11                                #612.7
        movl      1088(%rsp), %r12d                             #612.7
        imull     (%r11,%r10,4), %r12d                          #612.7
        movslq    %r12d, %r12                                   #612.7
        addq      %r12, %r9                                     #612.7
        movq      %r9, 1192(%rsp)                               #612.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B4.166:                       # Preds ..B4.155 ..B4.154 ..B4.149 ..B4.159 ..B4.160
                                #       ..B4.164 ..B4.165
                                # Execution count [5.00e+00]
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.326:
        call      *1288(%rsp)                                   #614.5[spill]
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.327:
                                # LOE ebx ebp
..B4.167:                       # Preds ..B4.166
                                # Execution count [5.00e+00]
        incl      %ebx                                          #584.32
        cmpl      1280(%rsp), %ebx                              #584.23[spill]
        jl        ..B4.56       # Prob 82%                      #584.23
                                # LOE ebx ebp
..B4.168:                       # Preds ..B4.167
                                # Execution count [9.00e-01]
        movl      1280(%rsp), %r9d                              #[spill]
        movq      1296(%rsp), %rbx                              #[spill]
        movq      1208(%rsp), %rbp                              #[spill]
        movq      1200(%rsp), %r12                              #[spill]
	.cfi_restore 12
        cmpl      16(%rbx), %r9d                                #617.18
        je        ..B4.170      # Prob 50%                      #617.18
                                # LOE rbp r12 r13 r14 r9d
..B4.169:                       # Preds ..B4.188 ..B4.168
                                # Execution count [5.00e-01]
        testl     %r9d, %r9d                                    #617.47
        jne       ..B4.171      # Prob 78%                      #617.47
                                # LOE rbp r12 r13 r14
..B4.170:                       # Preds ..B4.169 ..B4.188 ..B4.168
                                # Execution count [6.10e-01]
        movl      $6, %edi                                      #618.5
        lea       (%rsp), %rsi                                  #618.5
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.329:
#       op_mpi_wait_all(int, op_arg *)
        call      op_mpi_wait_all                               #618.5
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.330:
                                # LOE rbp r12 r13 r14
..B4.171:                       # Preds ..B4.170 ..B4.169
                                # Execution count [1.00e+00]
        movl      $6, %edi                                      #621.3
        lea       (%rsp), %rsi                                  #621.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.331:
#       op_mpi_set_dirtybit(int, op_arg *)
        call      op_mpi_set_dirtybit                           #621.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.332:
                                # LOE rbp r12 r13 r14
..B4.172:                       # Preds ..B4.171
                                # Execution count [1.00e+00]
        movq      1184(%rsp), %rsi                              #629.3
        lea       1744(%rsp), %rdi                              #629.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.333:
#       op_mpi_reduce_double(op_arg *, double *)
        call      op_mpi_reduce_double                          #629.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.334:
                                # LOE rbp r12 r13 r14
..B4.173:                       # Preds ..B4.172
                                # Execution count [1.00e+00]
        lea       576(%rsp), %rdi                               #633.3
        lea       584(%rsp), %rsi                               #633.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.335:
#       op_timers_core(double *, double *)
        call      op_timers_core                                #633.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.336:
                                # LOE rbp r12 r13 r14
..B4.174:                       # Preds ..B4.173
                                # Execution count [1.00e+00]
        movsd     584(%rsp), %xmm0                              #638.3
        movq      %rbp, %rdi                                    #638.3
        subsd     1224(%rsp), %xmm0                             #638.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.337:
#       op_mpi_perf_time(const char *, double)
        call      op_mpi_perf_time                              #638.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_.338:
                                # LOE r12 r13 r14
..B4.175:                       # Preds ..B4.174
                                # Execution count [1.00e+00]
        cmpl      $-1, 1388(%rsp)                               #641.18
        jge       ..B4.177      # Prob 78%                      #641.18
                                # LOE r12 r13 r14
..B4.176:                       # Preds ..B4.175
                                # Execution count [2.20e-01]
        movq      1152(%rsp), %rdi                              #642.5
#       free(void *)
        call      free                                          #642.5
                                # LOE r12 r13 r14
..B4.177:                       # Preds ..B4.176 ..B4.175
                                # Execution count [1.00e+00]
        cmpl      $-1, 1484(%rsp)                               #644.18
        jge       ..B4.179      # Prob 78%                      #644.18
                                # LOE r12 r13 r14
..B4.178:                       # Preds ..B4.177
                                # Execution count [2.20e-01]
        movq      1160(%rsp), %rdi                              #645.5
#       free(void *)
        call      free                                          #645.5
                                # LOE r12 r13 r14
..B4.179:                       # Preds ..B4.178 ..B4.177
                                # Execution count [1.00e+00]
        cmpl      $-1, 1580(%rsp)                               #647.18
        jge       ..B4.181      # Prob 78%                      #647.18
                                # LOE r12 r13 r14
..B4.180:                       # Preds ..B4.179
                                # Execution count [2.20e-01]
        movq      1168(%rsp), %rdi                              #648.5
#       free(void *)
        call      free                                          #648.5
                                # LOE r12 r13 r14
..B4.181:                       # Preds ..B4.180 ..B4.179
                                # Execution count [1.00e+00]
        cmpl      $-1, 1676(%rsp)                               #650.18
        jge       ..B4.183      # Prob 78%                      #650.18
                                # LOE r12 r13 r14
..B4.182:                       # Preds ..B4.181
                                # Execution count [2.20e-01]
        movq      1176(%rsp), %rdi                              #651.5
#       free(void *)
        call      free                                          #651.5
                                # LOE r12 r13 r14
..B4.183:                       # Preds ..B4.182 ..B4.181
                                # Execution count [1.00e+00]
        cmpl      $-1, 1772(%rsp)                               #653.18
        jge       ..B4.185      # Prob 78%                      #653.18
                                # LOE r12 r13 r14
..B4.184:                       # Preds ..B4.183
                                # Execution count [2.20e-01]
        movq      1184(%rsp), %rdi                              #654.5
#       free(void *)
        call      free                                          #654.5
                                # LOE r12 r13 r14
..B4.185:                       # Preds ..B4.184 ..B4.183
                                # Execution count [1.00e+00]
        cmpl      $-1, 1868(%rsp)                               #656.18
        jge       ..B4.187      # Prob 78%                      #656.18
                                # LOE r12 r13 r14
..B4.186:                       # Preds ..B4.185
                                # Execution count [2.20e-01]
        movq      1192(%rsp), %rdi                              #657.5
#       free(void *)
        call      free                                          #657.5
                                # LOE r12 r13 r14
..B4.187:                       # Preds ..B4.186 ..B4.185
                                # Execution count [1.00e+00]
        addq      $1312, %rsp                                   #659.1
	.cfi_def_cfa_offset 48
	.cfi_restore 6
        popq      %rbp                                          #659.1
	.cfi_def_cfa_offset 40
	.cfi_restore 3
        popq      %rbx                                          #659.1
	.cfi_def_cfa_offset 32
	.cfi_restore 15
        popq      %r15                                          #659.1
	.cfi_def_cfa_offset 24
	.cfi_restore 14
        popq      %r14                                          #659.1
	.cfi_def_cfa_offset 16
	.cfi_restore 13
        popq      %r13                                          #659.1
	.cfi_def_cfa_offset 8
        ret                                                     #659.1
	.cfi_def_cfa_offset 1360
	.cfi_offset 3, -40
	.cfi_offset 6, -48
	.cfi_offset 13, -16
	.cfi_offset 14, -24
	.cfi_offset 15, -32
                                # LOE
..B4.188:                       # Preds ..B4.54
                                # Execution count [1.00e-01]: Infreq
        cmpl      16(%rbx), %r9d                                #617.18
        je        ..B4.170      # Prob 50%                      #617.18
        jmp       ..B4.169      # Prob 100%                     #617.18
        .align    16,0x90
                                # LOE rbp r12 r13 r14 r9d
	.cfi_endproc
# mark_end;
	.type	_Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_,@function
	.size	_Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_,.-_Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_
	.data
# -- End  _Z11op_par_loopIKdS0_S0_S0_dKiEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSK_SK_SK_SK_SK_
	.section .text._Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_, "xaG",@progbits,_Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_,comdat
..TXTST4:
# -- Begin  _Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_
	.section .text._Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_, "xaG",@progbits,_Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_,comdat
# mark_begin;
       .align    16,0x90
	.weak _Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_
# --- op_par_loop<const double, const double, const double, const double, const double, const double, double, double>(void (*)(const double *, const double *, const double *, const double *, const double *, const double *, double *, double *), const char *, op_set, op_arg, op_arg, op_arg, op_arg, op_arg, op_arg, op_arg, op_arg)
_Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_:
# parameter 1: %rdi
# parameter 2: %rsi
# parameter 3: %rdx
# parameter 4: 1856 + %rsp
# parameter 5: 1952 + %rsp
# parameter 6: 2048 + %rsp
# parameter 7: 2144 + %rsp
# parameter 8: 2240 + %rsp
# parameter 9: 2336 + %rsp
# parameter 10: 2432 + %rsp
# parameter 11: 2528 + %rsp
..B5.1:                         # Preds ..B5.0
                                # Execution count [1.00e+00]
	.cfi_startproc
	.cfi_personality 0x3,__gxx_personality_v0
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.357:
..L358:
                                                        #812.44
        pushq     %r13                                          #812.44
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
        pushq     %r14                                          #812.44
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
        pushq     %r15                                          #812.44
	.cfi_def_cfa_offset 32
	.cfi_offset 15, -32
        pushq     %rbx                                          #812.44
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
        pushq     %rbp                                          #812.44
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
        subq      $1808, %rsp                                   #812.44
	.cfi_def_cfa_offset 1856
        movq      %rdx, %rbx                                    #812.44
        pxor      %xmm0, %xmm0                                  #814.16
        movq      %rsi, %rbp                                    #812.44
        movups    %xmm0, 1536(%rsp)                             #814.16
        movq      %rdi, %r15                                    #812.44
        movups    %xmm0, 1552(%rsp)                             #814.16
        movups    %xmm0, 1568(%rsp)                             #814.16
        movups    %xmm0, 1584(%rsp)                             #814.16
                                # LOE rbx rbp r12 r13 r14 r15
..B5.2:                         # Preds ..B5.1
                                # Execution count [1.00e+00]
        movups    1856(%rsp), %xmm0                             #815.21
        movups    1872(%rsp), %xmm1                             #815.21
        movups    1888(%rsp), %xmm2                             #815.21
        movups    1904(%rsp), %xmm3                             #815.21
        movups    1920(%rsp), %xmm4                             #815.21
        movups    1936(%rsp), %xmm5                             #815.21
        movups    1952(%rsp), %xmm6                             #815.27
        movups    1968(%rsp), %xmm7                             #815.27
        movups    1984(%rsp), %xmm8                             #815.27
        movups    2000(%rsp), %xmm9                             #815.27
        movups    2016(%rsp), %xmm10                            #815.27
        movups    2032(%rsp), %xmm11                            #815.27
        movups    2048(%rsp), %xmm12                            #815.33
        movups    2064(%rsp), %xmm13                            #815.33
        movups    2080(%rsp), %xmm14                            #815.33
        movups    2096(%rsp), %xmm15                            #815.33
        movups    %xmm0, (%rsp)                                 #815.21
        movups    %xmm1, 16(%rsp)                               #815.21
        movups    %xmm2, 32(%rsp)                               #815.21
        movups    %xmm3, 48(%rsp)                               #815.21
        movups    %xmm4, 64(%rsp)                               #815.21
        movups    %xmm5, 80(%rsp)                               #815.21
        movups    %xmm6, 96(%rsp)                               #815.27
        movups    %xmm7, 112(%rsp)                              #815.27
        movups    %xmm8, 128(%rsp)                              #815.27
        movups    %xmm9, 144(%rsp)                              #815.27
        movups    %xmm10, 160(%rsp)                             #815.27
        movups    %xmm11, 176(%rsp)                             #815.27
        movups    %xmm12, 192(%rsp)                             #815.33
        movups    %xmm13, 208(%rsp)                             #815.33
        movups    %xmm14, 224(%rsp)                             #815.33
        movups    %xmm15, 240(%rsp)                             #815.33
        movups    2112(%rsp), %xmm0                             #815.33
        movups    2128(%rsp), %xmm1                             #815.33
        movups    2144(%rsp), %xmm2                             #815.39
        movups    2160(%rsp), %xmm3                             #815.39
        movups    2176(%rsp), %xmm4                             #815.39
        movups    2192(%rsp), %xmm5                             #815.39
        movups    2208(%rsp), %xmm6                             #815.39
        movups    2224(%rsp), %xmm7                             #815.39
        movups    2240(%rsp), %xmm8                             #815.45
        movups    2256(%rsp), %xmm9                             #815.45
        movups    2272(%rsp), %xmm10                            #815.45
        movups    2288(%rsp), %xmm11                            #815.45
        movups    2304(%rsp), %xmm12                            #815.45
        movups    2320(%rsp), %xmm13                            #815.45
        movups    2336(%rsp), %xmm14                            #815.51
        movups    2352(%rsp), %xmm15                            #815.51
        movups    %xmm0, 256(%rsp)                              #815.33
        movups    %xmm1, 272(%rsp)                              #815.33
        movups    %xmm2, 288(%rsp)                              #815.39
        movups    %xmm3, 304(%rsp)                              #815.39
        movups    %xmm4, 320(%rsp)                              #815.39
        movups    %xmm5, 336(%rsp)                              #815.39
        movups    %xmm6, 352(%rsp)                              #815.39
        movups    %xmm7, 368(%rsp)                              #815.39
        movups    %xmm8, 384(%rsp)                              #815.45
        movups    %xmm9, 400(%rsp)                              #815.45
        movups    %xmm10, 416(%rsp)                             #815.45
        movups    %xmm11, 432(%rsp)                             #815.45
        movups    %xmm12, 448(%rsp)                             #815.45
        movups    %xmm13, 464(%rsp)                             #815.45
        movups    %xmm14, 480(%rsp)                             #815.51
        movups    %xmm15, 496(%rsp)                             #815.51
        movups    2368(%rsp), %xmm0                             #815.51
        movups    2384(%rsp), %xmm1                             #815.51
        movups    2400(%rsp), %xmm2                             #815.51
        movups    2416(%rsp), %xmm3                             #815.51
        movups    2432(%rsp), %xmm4                             #815.57
        movups    2448(%rsp), %xmm5                             #815.57
        movups    2464(%rsp), %xmm6                             #815.57
        movups    2480(%rsp), %xmm7                             #815.57
        movups    2496(%rsp), %xmm8                             #815.57
        movups    2512(%rsp), %xmm9                             #815.57
        movups    2528(%rsp), %xmm10                            #815.63
        movups    2544(%rsp), %xmm11                            #815.63
        movups    2560(%rsp), %xmm12                            #815.63
        movups    2576(%rsp), %xmm13                            #815.63
        movups    2592(%rsp), %xmm14                            #815.63
        movups    2608(%rsp), %xmm15                            #815.63
        movups    %xmm0, 512(%rsp)                              #815.51
        movups    %xmm1, 528(%rsp)                              #815.51
        movups    %xmm2, 544(%rsp)                              #815.51
        movups    %xmm3, 560(%rsp)                              #815.51
        movups    %xmm4, 576(%rsp)                              #815.57
        movups    %xmm5, 592(%rsp)                              #815.57
        movups    %xmm6, 608(%rsp)                              #815.57
        movups    %xmm7, 624(%rsp)                              #815.57
        movups    %xmm8, 640(%rsp)                              #815.57
        movups    %xmm9, 656(%rsp)                              #815.57
        movups    %xmm10, 672(%rsp)                             #815.63
        movups    %xmm11, 688(%rsp)                             #815.63
        movups    %xmm12, 704(%rsp)                             #815.63
        movups    %xmm13, 720(%rsp)                             #815.63
        movups    %xmm14, 736(%rsp)                             #815.63
        movups    %xmm15, 752(%rsp)                             #815.63
        cmpl      $-1, 1884(%rsp)                               #816.18
        jge       ..B5.5        # Prob 78%                      #816.18
                                # LOE rbx rbp r12 r13 r14 r15
..B5.3:                         # Preds ..B5.2
                                # Execution count [2.20e-01]
        movl      28(%rsp), %edi                                #817.22
        negl      %edi                                          #817.22
        movslq    %edi, %rdi                                    #817.22
        shlq      $3, %rdi                                      #817.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.370:
#       op_malloc(size_t)
        call      op_malloc                                     #817.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.371:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B5.4:                         # Preds ..B5.3
                                # Execution count [2.20e-01]
        movq      %rax, 1536(%rsp)                              #817.5
                                # LOE rbx rbp r12 r13 r14 r15
..B5.5:                         # Preds ..B5.2 ..B5.4
                                # Execution count [1.00e+00]
        cmpl      $-1, 1980(%rsp)                               #819.18
        jge       ..B5.8        # Prob 78%                      #819.18
                                # LOE rbx rbp r12 r13 r14 r15
..B5.6:                         # Preds ..B5.5
                                # Execution count [2.20e-01]
        movl      124(%rsp), %edi                               #820.22
        negl      %edi                                          #820.22
        movslq    %edi, %rdi                                    #820.22
        shlq      $3, %rdi                                      #820.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.372:
#       op_malloc(size_t)
        call      op_malloc                                     #820.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.373:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B5.7:                         # Preds ..B5.6
                                # Execution count [2.20e-01]
        movq      %rax, 1544(%rsp)                              #820.5
                                # LOE rbx rbp r12 r13 r14 r15
..B5.8:                         # Preds ..B5.5 ..B5.7
                                # Execution count [1.00e+00]
        cmpl      $-1, 2076(%rsp)                               #822.18
        jge       ..B5.11       # Prob 78%                      #822.18
                                # LOE rbx rbp r12 r13 r14 r15
..B5.9:                         # Preds ..B5.8
                                # Execution count [2.20e-01]
        movl      220(%rsp), %edi                               #823.22
        negl      %edi                                          #823.22
        movslq    %edi, %rdi                                    #823.22
        shlq      $3, %rdi                                      #823.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.374:
#       op_malloc(size_t)
        call      op_malloc                                     #823.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.375:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B5.10:                        # Preds ..B5.9
                                # Execution count [2.20e-01]
        movq      %rax, 1552(%rsp)                              #823.5
                                # LOE rbx rbp r12 r13 r14 r15
..B5.11:                        # Preds ..B5.8 ..B5.10
                                # Execution count [1.00e+00]
        cmpl      $-1, 2172(%rsp)                               #825.18
        jge       ..B5.14       # Prob 78%                      #825.18
                                # LOE rbx rbp r12 r13 r14 r15
..B5.12:                        # Preds ..B5.11
                                # Execution count [2.20e-01]
        movl      316(%rsp), %edi                               #826.22
        negl      %edi                                          #826.22
        movslq    %edi, %rdi                                    #826.22
        shlq      $3, %rdi                                      #826.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.376:
#       op_malloc(size_t)
        call      op_malloc                                     #826.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.377:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B5.13:                        # Preds ..B5.12
                                # Execution count [2.20e-01]
        movq      %rax, 1560(%rsp)                              #826.5
                                # LOE rbx rbp r12 r13 r14 r15
..B5.14:                        # Preds ..B5.11 ..B5.13
                                # Execution count [1.00e+00]
        cmpl      $-1, 2268(%rsp)                               #828.18
        jge       ..B5.17       # Prob 78%                      #828.18
                                # LOE rbx rbp r12 r13 r14 r15
..B5.15:                        # Preds ..B5.14
                                # Execution count [2.20e-01]
        movl      412(%rsp), %edi                               #829.22
        negl      %edi                                          #829.22
        movslq    %edi, %rdi                                    #829.22
        shlq      $3, %rdi                                      #829.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.378:
#       op_malloc(size_t)
        call      op_malloc                                     #829.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.379:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B5.16:                        # Preds ..B5.15
                                # Execution count [2.20e-01]
        movq      %rax, 1568(%rsp)                              #829.5
                                # LOE rbx rbp r12 r13 r14 r15
..B5.17:                        # Preds ..B5.14 ..B5.16
                                # Execution count [1.00e+00]
        cmpl      $-1, 2364(%rsp)                               #831.18
        jge       ..B5.20       # Prob 78%                      #831.18
                                # LOE rbx rbp r12 r13 r14 r15
..B5.18:                        # Preds ..B5.17
                                # Execution count [2.20e-01]
        movl      508(%rsp), %edi                               #832.22
        negl      %edi                                          #832.22
        movslq    %edi, %rdi                                    #832.22
        shlq      $3, %rdi                                      #832.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.380:
#       op_malloc(size_t)
        call      op_malloc                                     #832.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.381:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B5.19:                        # Preds ..B5.18
                                # Execution count [2.20e-01]
        movq      %rax, 1576(%rsp)                              #832.5
                                # LOE rbx rbp r12 r13 r14 r15
..B5.20:                        # Preds ..B5.17 ..B5.19
                                # Execution count [1.00e+00]
        cmpl      $-1, 2460(%rsp)                               #834.18
        jge       ..B5.23       # Prob 78%                      #834.18
                                # LOE rbx rbp r12 r13 r14 r15
..B5.21:                        # Preds ..B5.20
                                # Execution count [2.20e-01]
        movl      604(%rsp), %edi                               #835.22
        negl      %edi                                          #835.22
        movslq    %edi, %rdi                                    #835.22
        shlq      $3, %rdi                                      #835.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.382:
#       op_malloc(size_t)
        call      op_malloc                                     #835.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.383:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B5.22:                        # Preds ..B5.21
                                # Execution count [2.20e-01]
        movq      %rax, 1584(%rsp)                              #835.5
                                # LOE rbx rbp r12 r13 r14 r15
..B5.23:                        # Preds ..B5.20 ..B5.22
                                # Execution count [1.00e+00]
        cmpl      $-1, 2556(%rsp)                               #837.18
        jge       ..B5.26       # Prob 78%                      #837.18
                                # LOE rbx rbp r12 r13 r14 r15
..B5.24:                        # Preds ..B5.23
                                # Execution count [2.20e-01]
        movl      700(%rsp), %edi                               #838.22
        negl      %edi                                          #838.22
        movslq    %edi, %rdi                                    #838.22
        shlq      $3, %rdi                                      #838.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.384:
#       op_malloc(size_t)
        call      op_malloc                                     #838.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.385:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B5.25:                        # Preds ..B5.24
                                # Execution count [2.20e-01]
        movq      %rax, 1592(%rsp)                              #838.5
                                # LOE rbx rbp r12 r13 r14 r15
..B5.26:                        # Preds ..B5.23 ..B5.25
                                # Execution count [1.00e+00]
        cmpl      $0, 84(%rsp)                                  #843.28
        jne       ..B5.30       # Prob 50%                      #843.28
                                # LOE rbx rbp r12 r13 r14 r15
..B5.27:                        # Preds ..B5.26
                                # Execution count [5.00e-01]
        movl      32(%rsp), %edi                                #843.42
        cmpl      blank_args_size(%rip), %edi                   #843.57
        jle       ..B5.30       # Prob 68%                      #843.57
                                # LOE rbx rbp r12 r13 r14 r15 edi
..B5.28:                        # Preds ..B5.27
                                # Execution count [1.58e-01]
        movl      %edi, blank_args_size(%rip)                   #844.7
        movslq    %edi, %rdi                                    #845.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.386:
#       op_malloc(size_t)
        call      op_malloc                                     #845.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.387:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B5.29:                        # Preds ..B5.28
                                # Execution count [1.58e-01]
        movq      %rax, blank_args(%rip)                        #845.7
                                # LOE rbx rbp r12 r13 r14 r15
..B5.30:                        # Preds ..B5.27 ..B5.26 ..B5.29
                                # Execution count [1.00e+00]
        cmpl      $0, 180(%rsp)                                 #843.28
        jne       ..B5.34       # Prob 50%                      #843.28
                                # LOE rbx rbp r12 r13 r14 r15
..B5.31:                        # Preds ..B5.30
                                # Execution count [5.00e-01]
        movl      128(%rsp), %edi                               #843.42
        cmpl      blank_args_size(%rip), %edi                   #843.57
        jle       ..B5.34       # Prob 68%                      #843.57
                                # LOE rbx rbp r12 r13 r14 r15 edi
..B5.32:                        # Preds ..B5.31
                                # Execution count [1.58e-01]
        movl      %edi, blank_args_size(%rip)                   #844.7
        movslq    %edi, %rdi                                    #845.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.388:
#       op_malloc(size_t)
        call      op_malloc                                     #845.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.389:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B5.33:                        # Preds ..B5.32
                                # Execution count [1.58e-01]
        movq      %rax, blank_args(%rip)                        #845.7
                                # LOE rbx rbp r12 r13 r14 r15
..B5.34:                        # Preds ..B5.30 ..B5.31 ..B5.33
                                # Execution count [1.00e+00]
        cmpl      $0, 276(%rsp)                                 #843.28
        jne       ..B5.38       # Prob 50%                      #843.28
                                # LOE rbx rbp r12 r13 r14 r15
..B5.35:                        # Preds ..B5.34
                                # Execution count [5.00e-01]
        movl      224(%rsp), %edi                               #843.42
        cmpl      blank_args_size(%rip), %edi                   #843.57
        jle       ..B5.38       # Prob 68%                      #843.57
                                # LOE rbx rbp r12 r13 r14 r15 edi
..B5.36:                        # Preds ..B5.35
                                # Execution count [1.58e-01]
        movl      %edi, blank_args_size(%rip)                   #844.7
        movslq    %edi, %rdi                                    #845.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.390:
#       op_malloc(size_t)
        call      op_malloc                                     #845.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.391:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B5.37:                        # Preds ..B5.36
                                # Execution count [1.58e-01]
        movq      %rax, blank_args(%rip)                        #845.7
                                # LOE rbx rbp r12 r13 r14 r15
..B5.38:                        # Preds ..B5.34 ..B5.35 ..B5.37
                                # Execution count [1.00e+00]
        cmpl      $0, 372(%rsp)                                 #843.28
        jne       ..B5.42       # Prob 50%                      #843.28
                                # LOE rbx rbp r12 r13 r14 r15
..B5.39:                        # Preds ..B5.38
                                # Execution count [5.00e-01]
        movl      320(%rsp), %edi                               #843.42
        cmpl      blank_args_size(%rip), %edi                   #843.57
        jle       ..B5.42       # Prob 68%                      #843.57
                                # LOE rbx rbp r12 r13 r14 r15 edi
..B5.40:                        # Preds ..B5.39
                                # Execution count [1.58e-01]
        movl      %edi, blank_args_size(%rip)                   #844.7
        movslq    %edi, %rdi                                    #845.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.392:
#       op_malloc(size_t)
        call      op_malloc                                     #845.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.393:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B5.41:                        # Preds ..B5.40
                                # Execution count [1.58e-01]
        movq      %rax, blank_args(%rip)                        #845.7
                                # LOE rbx rbp r12 r13 r14 r15
..B5.42:                        # Preds ..B5.38 ..B5.39 ..B5.41
                                # Execution count [1.00e+00]
        cmpl      $0, 468(%rsp)                                 #843.28
        jne       ..B5.46       # Prob 50%                      #843.28
                                # LOE rbx rbp r12 r13 r14 r15
..B5.43:                        # Preds ..B5.42
                                # Execution count [5.00e-01]
        movl      416(%rsp), %edi                               #843.42
        cmpl      blank_args_size(%rip), %edi                   #843.57
        jle       ..B5.46       # Prob 68%                      #843.57
                                # LOE rbx rbp r12 r13 r14 r15 edi
..B5.44:                        # Preds ..B5.43
                                # Execution count [1.58e-01]
        movl      %edi, blank_args_size(%rip)                   #844.7
        movslq    %edi, %rdi                                    #845.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.394:
#       op_malloc(size_t)
        call      op_malloc                                     #845.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.395:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B5.45:                        # Preds ..B5.44
                                # Execution count [1.58e-01]
        movq      %rax, blank_args(%rip)                        #845.7
                                # LOE rbx rbp r12 r13 r14 r15
..B5.46:                        # Preds ..B5.42 ..B5.43 ..B5.45
                                # Execution count [1.00e+00]
        cmpl      $0, 564(%rsp)                                 #843.28
        jne       ..B5.50       # Prob 50%                      #843.28
                                # LOE rbx rbp r12 r13 r14 r15
..B5.47:                        # Preds ..B5.46
                                # Execution count [5.00e-01]
        movl      512(%rsp), %edi                               #843.42
        cmpl      blank_args_size(%rip), %edi                   #843.57
        jle       ..B5.50       # Prob 68%                      #843.57
                                # LOE rbx rbp r12 r13 r14 r15 edi
..B5.48:                        # Preds ..B5.47
                                # Execution count [1.58e-01]
        movl      %edi, blank_args_size(%rip)                   #844.7
        movslq    %edi, %rdi                                    #845.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.396:
#       op_malloc(size_t)
        call      op_malloc                                     #845.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.397:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B5.49:                        # Preds ..B5.48
                                # Execution count [1.58e-01]
        movq      %rax, blank_args(%rip)                        #845.7
                                # LOE rbx rbp r12 r13 r14 r15
..B5.50:                        # Preds ..B5.46 ..B5.47 ..B5.49
                                # Execution count [1.00e+00]
        cmpl      $0, 660(%rsp)                                 #843.28
        jne       ..B5.54       # Prob 50%                      #843.28
                                # LOE rbx rbp r12 r13 r14 r15
..B5.51:                        # Preds ..B5.50
                                # Execution count [5.00e-01]
        movl      608(%rsp), %edi                               #843.42
        cmpl      blank_args_size(%rip), %edi                   #843.57
        jle       ..B5.54       # Prob 68%                      #843.57
                                # LOE rbx rbp r12 r13 r14 r15 edi
..B5.52:                        # Preds ..B5.51
                                # Execution count [1.58e-01]
        movl      %edi, blank_args_size(%rip)                   #844.7
        movslq    %edi, %rdi                                    #845.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.398:
#       op_malloc(size_t)
        call      op_malloc                                     #845.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.399:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B5.53:                        # Preds ..B5.52
                                # Execution count [1.58e-01]
        movq      %rax, blank_args(%rip)                        #845.7
                                # LOE rbx rbp r12 r13 r14 r15
..B5.54:                        # Preds ..B5.50 ..B5.51 ..B5.53
                                # Execution count [1.00e+00]
        cmpl      $0, 756(%rsp)                                 #843.28
        jne       ..B5.58       # Prob 50%                      #843.28
                                # LOE rbx rbp r12 r13 r14 r15
..B5.55:                        # Preds ..B5.54
                                # Execution count [5.00e-01]
        movl      704(%rsp), %edi                               #843.42
        cmpl      blank_args_size(%rip), %edi                   #843.57
        jle       ..B5.58       # Prob 68%                      #843.57
                                # LOE rbx rbp r12 r13 r14 r15 edi
..B5.56:                        # Preds ..B5.55
                                # Execution count [1.58e-01]
        movl      %edi, blank_args_size(%rip)                   #844.7
        movslq    %edi, %rdi                                    #845.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.400:
#       op_malloc(size_t)
        call      op_malloc                                     #845.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.401:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B5.57:                        # Preds ..B5.56
                                # Execution count [1.58e-01]
        movq      %rax, blank_args(%rip)                        #845.7
                                # LOE rbx rbp r12 r13 r14 r15
..B5.58:                        # Preds ..B5.54 ..B5.55 ..B5.57
                                # Execution count [1.00e+00]
        movl      $0, 1792(%rsp)                                #848.13
        cmpl      $0, OP_diags(%rip)                            #849.18
        jle       ..B5.66       # Prob 40%                      #849.18
                                # LOE rbx rbp r12 r13 r14 r15
..B5.59:                        # Preds ..B5.58
                                # Execution count [5.37e-01]
        xorl      %esi, %esi                                    #850.5
        xorl      %eax, %eax                                    #850.5
        movq      %rax, %r14                                    #850.5
        movl      %esi, %r13d                                   #850.5
                                # LOE rbx rbp r12 r14 r15 r13d
..B5.60:                        # Preds ..B5.61 ..B5.59
                                # Execution count [2.98e+00]
        addq      $-96, %rsp                                    #850.5
	.cfi_def_cfa_offset 1952
        movq      %rbx, %rdi                                    #850.5
        movq      %rsp, %r8                                     #850.5
        movl      %r13d, %esi                                   #850.5
        movups    96(%rsp,%r14), %xmm0                          #850.5
        lea       1888(%rsp), %rdx                              #850.5
        movups    112(%rsp,%r14), %xmm1                         #850.5
        movq      %rbp, %rcx                                    #850.5
        movups    128(%rsp,%r14), %xmm2                         #850.5
        movups    144(%rsp,%r14), %xmm3                         #850.5
        movups    160(%rsp,%r14), %xmm4                         #850.5
        movups    176(%rsp,%r14), %xmm5                         #850.5
        movups    %xmm0, (%r8)                                  #850.5
        movups    %xmm1, 16(%r8)                                #850.5
        movups    %xmm2, 32(%r8)                                #850.5
        movups    %xmm3, 48(%r8)                                #850.5
        movups    %xmm4, 64(%r8)                                #850.5
        movups    %xmm5, 80(%r8)                                #850.5
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.403:
#       op_arg_check(op_set, int, op_arg, int *, const char *)
        call      op_arg_check                                  #850.5
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.404:
                                # LOE rbx rbp r12 r14 r15 r13d
..B5.262:                       # Preds ..B5.60
                                # Execution count [2.98e+00]
        addq      $96, %rsp                                     #850.5
	.cfi_def_cfa_offset 1856
                                # LOE rbx rbp r12 r14 r15 r13d
..B5.61:                        # Preds ..B5.262
                                # Execution count [2.98e+00]
        incl      %r13d                                         #850.5
        addq      $96, %r14                                     #850.5
        cmpl      $8, %r13d                                     #850.5
        jl        ..B5.60       # Prob 82%                      #850.5
                                # LOE rbx rbp r12 r14 r15 r13d
..B5.62:                        # Preds ..B5.61
                                # Execution count [5.37e-01]
        cmpl      $2, OP_diags(%rip)                            #852.18
        jle       ..B5.66       # Prob 50%                      #852.18
                                # LOE rbx rbp r12 r13 r14 r15
..B5.63:                        # Preds ..B5.62
                                # Execution count [5.00e-01]
        cmpl      $0, 1792(%rsp)                                #853.18
        jne       ..B5.65       # Prob 50%                      #853.18
                                # LOE rbx rbp r12 r13 r14 r15
..B5.64:                        # Preds ..B5.63
                                # Execution count [2.50e-01]
        movl      $.L_2__STRING.3, %edi                         #854.7
        movq      %rbp, %rsi                                    #854.7
        xorl      %eax, %eax                                    #854.7
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.406:
#       printf(const char *, ...)
        call      printf                                        #854.7
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.407:
        jmp       ..B5.66       # Prob 100%                     #854.7
                                # LOE rbx rbp r12 r13 r14 r15
..B5.65:                        # Preds ..B5.63
                                # Execution count [2.50e-01]
        movl      $.L_2__STRING.4, %edi                         #856.7
        movq      %rbp, %rsi                                    #856.7
        xorl      %eax, %eax                                    #856.7
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.408:
#       printf(const char *, ...)
        call      printf                                        #856.7
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.409:
                                # LOE rbx rbp r12 r13 r14 r15
..B5.66:                        # Preds ..B5.58 ..B5.64 ..B5.65 ..B5.62
                                # Execution count [1.00e+00]
        lea       1616(%rsp), %rdi                              #860.3
        lea       1624(%rsp), %rsi                              #860.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.410:
#       op_timers_core(double *, double *)
        call      op_timers_core                                #860.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.411:
                                # LOE rbx rbp r12 r13 r14 r15
..B5.67:                        # Preds ..B5.66
                                # Execution count [1.00e+00]
        movq      %rbx, %rdi                                    #863.17
        movl      $8, %esi                                      #863.17
        lea       (%rsp), %rdx                                  #863.17
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.412:
#       op_mpi_halo_exchanges(op_set, int, op_arg *)
        call      op_mpi_halo_exchanges                         #863.17
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.413:
                                # LOE rbx rbp r12 r13 r14 r15 eax
..B5.263:                       # Preds ..B5.67
                                # Execution count [1.00e+00]
        movl      %eax, %r10d                                   #863.17
                                # LOE rbx rbp r12 r13 r14 r15 r10d
..B5.68:                        # Preds ..B5.263
                                # Execution count [1.00e+00]
        xorl      %edx, %edx                                    #866.12
        xorl      %r9d, %r9d                                    #868.14
        xorl      %eax, %eax                                    #868.14
        testl     %r10d, %r10d                                  #868.23
        jle       ..B5.243      # Prob 10%                      #868.23
                                # LOE rbx rbp r9 r12 r13 r14 r15 eax edx r10d
..B5.69:                        # Preds ..B5.68
                                # Execution count [9.00e-01]
        movq      %r9, 1760(%rsp)                               #872.7[spill]
        movl      %r10d, 1768(%rsp)                             #872.7[spill]
        movq      %r15, 1776(%rsp)                              #872.7[spill]
        movq      %rbp, 1608(%rsp)                              #872.7[spill]
        movl      %edx, %ebp                                    #872.7
        movq      %rbx, 1784(%rsp)                              #872.7[spill]
        movl      %eax, %ebx                                    #872.7
        movq      %r12, 1600(%rsp)                              #872.7[spill]
	.cfi_offset 12, -256
                                # LOE ebx ebp
..B5.70:                        # Preds ..B5.217 ..B5.69
                                # Execution count [5.00e+00]
        movq      1784(%rsp), %rcx                              #869.14[spill]
        cmpl      16(%rcx), %ebx                                #869.14
        jne       ..B5.72       # Prob 78%                      #869.14
                                # LOE ebx ebp
..B5.71:                        # Preds ..B5.70
                                # Execution count [1.10e+00]
        movl      $8, %edi                                      #870.7
        lea       (%rsp), %rsi                                  #870.7
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.415:
#       op_mpi_wait_all(int, op_arg *)
        call      op_mpi_wait_all                               #870.7
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.416:
                                # LOE ebx ebp
..B5.72:                        # Preds ..B5.71 ..B5.70
                                # Execution count [5.00e+00]
        movq      1784(%rsp), %rcx                              #872.7[spill]
        movl      $1, %esi                                      #872.7
        cmpl      4(%rcx), %ebx                                 #872.7
        cmove     %esi, %ebp                                    #872.7
        cmpl      $-1, 28(%rsp)                                 #873.23
        jge       ..B5.80       # Prob 50%                      #873.23
                                # LOE ebx ebp
..B5.73:                        # Preds ..B5.72
                                # Execution count [2.50e+00]
        movl      28(%rsp), %r14d                               #874.7
        movl      %r14d, %ecx                                   #874.7
        negl      %ecx                                          #874.7
        movq      16(%rsp), %r9                                 #874.7
        movq      40(%rsp), %r8                                 #874.7
        movl      32(%rsp), %r10d                               #874.7
        movq      1536(%rsp), %rdi                              #874.43
        testl     %ecx, %ecx                                    #874.7
        jle       ..B5.90       # Prob 50%                      #874.7
                                # LOE rdi r8 r9 ecx ebx ebp r10d r14d
..B5.74:                        # Preds ..B5.73
                                # Execution count [2.50e+00]
        movl      %ecx, %r11d                                   #874.7
        movl      $1, %r13d                                     #874.7
        shrl      $31, %r11d                                    #874.7
        xorl      %esi, %esi                                    #874.7
        subl      %r14d, %r11d                                  #874.7
        sarl      $1, %r11d                                     #874.7
        movl      24(%r9), %r12d                                #874.7
        testl     %r11d, %r11d                                  #874.7
        jbe       ..B5.78       # Prob 10%                      #874.7
                                # LOE rdi r8 r9 ecx ebx ebp esi r10d r11d r12d r13d
..B5.75:                        # Preds ..B5.74
                                # Execution count [2.25e+00]
        movl      %ebx, %r13d                                   #874.7
        imull     %r12d, %r13d                                  #874.7
        .align    16,0x90
                                # LOE rdi r8 r9 ecx ebx ebp esi r10d r11d r12d r13d
..B5.76:                        # Preds ..B5.76 ..B5.75
                                # Execution count [6.25e+00]
        movq      32(%r9), %r14                                 #874.7
        lea       (%r13,%rsi,2), %eax                           #874.7
        movslq    %eax, %rax                                    #874.7
        lea       (%rsi,%rsi), %edx                             #874.7
        movslq    %edx, %rdx                                    #874.7
        incl      %esi                                          #874.7
        movl      (%r14,%rax,4), %r15d                          #874.7
        imull     %r10d, %r15d                                  #874.7
        movslq    %r15d, %r15                                   #874.7
        addq      %r8, %r15                                     #874.7
        movq      %r15, (%rdi,%rdx,8)                           #874.7
        movq      32(%r9), %r14                                 #874.7
        movl      4(%r14,%rax,4), %r14d                         #874.7
        imull     %r10d, %r14d                                  #874.7
        movslq    %r14d, %r14                                   #874.7
        addq      %r8, %r14                                     #874.7
        movq      %r14, 8(%rdi,%rdx,8)                          #874.7
        cmpl      %r11d, %esi                                   #874.7
        jb        ..B5.76       # Prob 64%                      #874.7
                                # LOE rdi r8 r9 ecx ebx ebp esi r10d r11d r12d r13d
..B5.77:                        # Preds ..B5.76
                                # Execution count [2.25e+00]
        lea       1(%rsi,%rsi), %r13d                           #874.7
                                # LOE rdi r8 r9 ecx ebx ebp r10d r12d r13d
..B5.78:                        # Preds ..B5.77 ..B5.74
                                # Execution count [2.50e+00]
        lea       -1(%r13), %esi                                #874.7
        cmpl      %ecx, %esi                                    #874.7
        jae       ..B5.90       # Prob 10%                      #874.7
                                # LOE rdi r8 r9 ebx ebp r10d r12d r13d
..B5.79:                        # Preds ..B5.78
                                # Execution count [2.25e+00]
        imull     %ebx, %r12d                                   #874.7
        movslq    %r13d, %r13                                   #874.7
        movslq    %r12d, %r12                                   #874.7
        addq      %r13, %r12                                    #874.7
        movq      32(%r9), %r9                                  #874.7
        movl      -4(%r9,%r12,4), %ecx                          #874.7
        imull     %ecx, %r10d                                   #874.7
        movslq    %r10d, %r10                                   #874.7
        addq      %r10, %r8                                     #874.7
        movq      %r8, -8(%rdi,%r13,8)                          #874.7
        jmp       ..B5.90       # Prob 100%                     #874.7
                                # LOE rdi ebx ebp
..B5.80:                        # Preds ..B5.72
                                # Execution count [2.50e+00]
        movups    (%rsp), %xmm0                                 #876.7
        movups    16(%rsp), %xmm1                               #876.7
        movups    32(%rsp), %xmm2                               #876.7
        movups    48(%rsp), %xmm3                               #876.7
        movups    64(%rsp), %xmm4                               #876.7
        movups    80(%rsp), %xmm5                               #876.7
        movq      40(%rsp), %rdi                                #876.7
        movups    %xmm0, 768(%rsp)                              #876.7
        movups    %xmm1, 784(%rsp)                              #876.7
        movups    %xmm2, 800(%rsp)                              #876.7
        movups    %xmm3, 816(%rsp)                              #876.7
        movups    %xmm4, 832(%rsp)                              #876.7
        movups    %xmm5, 848(%rsp)                              #876.7
        cmpl      $0, 84(%rsp)                                  #876.7
        jne       ..B5.86       # Prob 50%                      #876.7
                                # LOE rdi ebx ebp
..B5.81:                        # Preds ..B5.80
                                # Execution count [1.25e+00]
        testl     %ebp, %ebp                                    #876.7
        je        ..B5.84       # Prob 50%                      #876.7
                                # LOE rdi ebx ebp
..B5.82:                        # Preds ..B5.81
                                # Execution count [6.25e-01]
        cmpl      $0, 80(%rsp)                                  #876.7
        je        ..B5.84       # Prob 50%                      #876.7
                                # LOE rdi ebx ebp
..B5.83:                        # Preds ..B5.82
                                # Execution count [3.12e-01]
        movq      blank_args(%rip), %rdi                        #876.7
        movq      %rdi, 1536(%rsp)                              #876.31
        jmp       ..B5.90       # Prob 100%                     #876.31
                                # LOE rdi ebx ebp
..B5.84:                        # Preds ..B5.81 ..B5.82
                                # Execution count [3.12e-01]
        movq      %rdi, 1536(%rsp)                              #876.31
        jmp       ..B5.90       # Prob 100%                     #876.31
                                # LOE rdi ebx ebp
..B5.86:                        # Preds ..B5.80
                                # Execution count [1.25e+00]
        movq      784(%rsp), %rsi                               #876.7
        testq     %rsi, %rsi                                    #876.7
        je        ..B5.88       # Prob 12%                      #876.7
                                # LOE rsi rdi ebx ebp
..B5.87:                        # Preds ..B5.86
                                # Execution count [1.10e+00]
        cmpl      $0, 860(%rsp)                                 #876.7
        jne       ..B5.89       # Prob 50%                      #876.7
                                # LOE rsi rdi ebx ebp
..B5.88:                        # Preds ..B5.86 ..B5.87
                                # Execution count [7.00e-01]
        movslq    800(%rsp), %rcx                               #876.7
        imulq     1760(%rsp), %rcx                              #876.7[spill]
        addq      %rcx, %rdi                                    #876.7
        movq      %rdi, 1536(%rsp)                              #876.31
        jmp       ..B5.90       # Prob 100%                     #876.31
                                # LOE rdi ebx ebp
..B5.89:                        # Preds ..B5.87
                                # Execution count [5.50e-01]
        movl      %ebx, %ecx                                    #876.7
        imull     24(%rsi), %ecx                                #876.7
        addl      796(%rsp), %ecx                               #876.7
        movslq    %ecx, %rcx                                    #876.7
        movq      32(%rsi), %rsi                                #876.7
        movl      800(%rsp), %r8d                               #876.7
        imull     (%rsi,%rcx,4), %r8d                           #876.7
        movslq    %r8d, %r8                                     #876.7
        addq      %r8, %rdi                                     #876.7
        movq      %rdi, 1536(%rsp)                              #876.31
                                # LOE rdi ebx ebp
..B5.90:                        # Preds ..B5.79 ..B5.78 ..B5.73 ..B5.83 ..B5.84
                                #       ..B5.88 ..B5.89
                                # Execution count [5.00e+00]
        cmpl      $-1, 124(%rsp)                                #877.23
        jge       ..B5.98       # Prob 50%                      #877.23
                                # LOE rdi ebx ebp
..B5.91:                        # Preds ..B5.90
                                # Execution count [2.50e+00]
        movl      124(%rsp), %r15d                              #878.7
        movl      %r15d, %r8d                                   #878.7
        negl      %r8d                                          #878.7
        movq      112(%rsp), %r9                                #878.7
        movq      136(%rsp), %r14                               #878.7
        movl      128(%rsp), %ecx                               #878.7
        movq      1544(%rsp), %rsi                              #878.43
        testl     %r8d, %r8d                                    #878.7
        jle       ..B5.108      # Prob 50%                      #878.7
                                # LOE rsi rdi r9 r14 ecx ebx ebp r8d r15d
..B5.92:                        # Preds ..B5.91
                                # Execution count [2.50e+00]
        movl      %r8d, %r12d                                   #878.7
        movl      $1, %r10d                                     #878.7
        shrl      $31, %r12d                                    #878.7
        xorl      %r11d, %r11d                                  #878.7
        subl      %r15d, %r12d                                  #878.7
        sarl      $1, %r12d                                     #878.7
        movl      24(%r9), %r13d                                #878.7
        testl     %r12d, %r12d                                  #878.7
        jbe       ..B5.96       # Prob 10%                      #878.7
                                # LOE rsi rdi r9 r14 ecx ebx ebp r8d r10d r11d r12d r13d
..B5.93:                        # Preds ..B5.92
                                # Execution count [2.25e+00]
        movl      %ebx, %r10d                                   #878.7
        imull     %r13d, %r10d                                  #878.7
        .align    16,0x90
                                # LOE rsi rdi r9 r14 ecx ebx ebp r8d r10d r11d r12d r13d
..B5.94:                        # Preds ..B5.94 ..B5.93
                                # Execution count [6.25e+00]
        movq      32(%r9), %r15                                 #878.7
        lea       (%r10,%r11,2), %eax                           #878.7
        movslq    %eax, %rax                                    #878.7
        lea       (%r11,%r11), %edx                             #878.7
        movslq    %edx, %rdx                                    #878.7
        incl      %r11d                                         #878.7
        movl      (%r15,%rax,4), %r15d                          #878.7
        imull     %ecx, %r15d                                   #878.7
        movslq    %r15d, %r15                                   #878.7
        addq      %r14, %r15                                    #878.7
        movq      %r15, (%rsi,%rdx,8)                           #878.7
        movq      32(%r9), %r15                                 #878.7
        movl      4(%r15,%rax,4), %r15d                         #878.7
        imull     %ecx, %r15d                                   #878.7
        movslq    %r15d, %r15                                   #878.7
        addq      %r14, %r15                                    #878.7
        movq      %r15, 8(%rsi,%rdx,8)                          #878.7
        cmpl      %r12d, %r11d                                  #878.7
        jb        ..B5.94       # Prob 64%                      #878.7
                                # LOE rsi rdi r9 r14 ecx ebx ebp r8d r10d r11d r12d r13d
..B5.95:                        # Preds ..B5.94
                                # Execution count [2.25e+00]
        lea       1(%r11,%r11), %r10d                           #878.7
                                # LOE rsi rdi r9 r14 ecx ebx ebp r8d r10d r13d
..B5.96:                        # Preds ..B5.95 ..B5.92
                                # Execution count [2.50e+00]
        lea       -1(%r10), %r11d                               #878.7
        cmpl      %r8d, %r11d                                   #878.7
        jae       ..B5.108      # Prob 10%                      #878.7
                                # LOE rsi rdi r9 r14 ecx ebx ebp r10d r13d
..B5.97:                        # Preds ..B5.96
                                # Execution count [2.25e+00]
        imull     %ebx, %r13d                                   #878.7
        movslq    %r10d, %r10                                   #878.7
        movslq    %r13d, %r13                                   #878.7
        addq      %r10, %r13                                    #878.7
        movq      32(%r9), %r9                                  #878.7
        movl      -4(%r9,%r13,4), %r8d                          #878.7
        imull     %r8d, %ecx                                    #878.7
        movslq    %ecx, %rcx                                    #878.7
        addq      %rcx, %r14                                    #878.7
        movq      %r14, -8(%rsi,%r10,8)                         #878.7
        jmp       ..B5.108      # Prob 100%                     #878.7
                                # LOE rsi rdi ebx ebp
..B5.98:                        # Preds ..B5.90
                                # Execution count [2.50e+00]
        movups    96(%rsp), %xmm0                               #880.7
        movups    112(%rsp), %xmm1                              #880.7
        movups    128(%rsp), %xmm2                              #880.7
        movups    144(%rsp), %xmm3                              #880.7
        movups    160(%rsp), %xmm4                              #880.7
        movups    176(%rsp), %xmm5                              #880.7
        movq      136(%rsp), %rsi                               #880.7
        movups    %xmm0, 864(%rsp)                              #880.7
        movups    %xmm1, 880(%rsp)                              #880.7
        movups    %xmm2, 896(%rsp)                              #880.7
        movups    %xmm3, 912(%rsp)                              #880.7
        movups    %xmm4, 928(%rsp)                              #880.7
        movups    %xmm5, 944(%rsp)                              #880.7
        cmpl      $0, 180(%rsp)                                 #880.7
        jne       ..B5.104      # Prob 50%                      #880.7
                                # LOE rsi rdi ebx ebp
..B5.99:                        # Preds ..B5.98
                                # Execution count [1.25e+00]
        testl     %ebp, %ebp                                    #880.7
        je        ..B5.102      # Prob 50%                      #880.7
                                # LOE rsi rdi ebx ebp
..B5.100:                       # Preds ..B5.99
                                # Execution count [6.25e-01]
        cmpl      $0, 176(%rsp)                                 #880.7
        je        ..B5.102      # Prob 50%                      #880.7
                                # LOE rsi rdi ebx ebp
..B5.101:                       # Preds ..B5.100
                                # Execution count [3.12e-01]
        movq      blank_args(%rip), %rsi                        #880.7
        movq      %rsi, 1544(%rsp)                              #880.7
        jmp       ..B5.108      # Prob 100%                     #880.7
                                # LOE rsi rdi ebx ebp
..B5.102:                       # Preds ..B5.99 ..B5.100
                                # Execution count [3.12e-01]
        movq      %rsi, 1544(%rsp)                              #880.7
        jmp       ..B5.108      # Prob 100%                     #880.7
                                # LOE rsi rdi ebx ebp
..B5.104:                       # Preds ..B5.98
                                # Execution count [1.25e+00]
        movq      880(%rsp), %r8                                #880.7
        testq     %r8, %r8                                      #880.7
        je        ..B5.106      # Prob 12%                      #880.7
                                # LOE rsi rdi r8 ebx ebp
..B5.105:                       # Preds ..B5.104
                                # Execution count [1.10e+00]
        cmpl      $0, 956(%rsp)                                 #880.7
        jne       ..B5.107      # Prob 50%                      #880.7
                                # LOE rsi rdi r8 ebx ebp
..B5.106:                       # Preds ..B5.104 ..B5.105
                                # Execution count [7.00e-01]
        movslq    896(%rsp), %rcx                               #880.7
        imulq     1760(%rsp), %rcx                              #880.7[spill]
        addq      %rcx, %rsi                                    #880.7
        movq      %rsi, 1544(%rsp)                              #880.7
        jmp       ..B5.108      # Prob 100%                     #880.7
                                # LOE rsi rdi ebx ebp
..B5.107:                       # Preds ..B5.105
                                # Execution count [5.50e-01]
        movl      %ebx, %ecx                                    #880.7
        imull     24(%r8), %ecx                                 #880.7
        addl      892(%rsp), %ecx                               #880.7
        movslq    %ecx, %rcx                                    #880.7
        movq      32(%r8), %r8                                  #880.7
        movl      896(%rsp), %r9d                               #880.7
        imull     (%r8,%rcx,4), %r9d                            #880.7
        movslq    %r9d, %r9                                     #880.7
        addq      %r9, %rsi                                     #880.7
        movq      %rsi, 1544(%rsp)                              #880.7
                                # LOE rsi rdi ebx ebp
..B5.108:                       # Preds ..B5.97 ..B5.96 ..B5.91 ..B5.101 ..B5.102
                                #       ..B5.106 ..B5.107
                                # Execution count [5.00e+00]
        cmpl      $-1, 220(%rsp)                                #881.23
        jge       ..B5.116      # Prob 50%                      #881.23
                                # LOE rsi rdi ebx ebp
..B5.109:                       # Preds ..B5.108
                                # Execution count [2.50e+00]
        movl      220(%rsp), %r10d                              #882.7
        movl      %r10d, %r9d                                   #882.7
        negl      %r9d                                          #882.7
        movq      208(%rsp), %r15                               #882.7
        movq      232(%rsp), %r11                               #882.7
        movl      224(%rsp), %r8d                               #882.7
        movq      1552(%rsp), %rdx                              #882.43
        testl     %r9d, %r9d                                    #882.7
        jle       ..B5.126      # Prob 50%                      #882.7
                                # LOE rdx rsi rdi r11 r15 ebx ebp r8d r9d r10d
..B5.110:                       # Preds ..B5.109
                                # Execution count [2.50e+00]
        movl      %r9d, %r13d                                   #882.7
        movl      $1, %ecx                                      #882.7
        shrl      $31, %r13d                                    #882.7
        xorl      %r12d, %r12d                                  #882.7
        subl      %r10d, %r13d                                  #882.7
        sarl      $1, %r13d                                     #882.7
        movl      24(%r15), %r14d                               #882.7
        testl     %r13d, %r13d                                  #882.7
        jbe       ..B5.114      # Prob 10%                      #882.7
                                # LOE rdx rsi rdi r11 r15 ecx ebx ebp r8d r9d r12d r13d r14d
..B5.111:                       # Preds ..B5.110
                                # Execution count [2.25e+00]
        movl      %ebx, %r10d                                   #882.7
        imull     %r14d, %r10d                                  #882.7
        movl      %ebp, 1632(%rsp)                              #882.7[spill]
        .align    16,0x90
                                # LOE rdx rsi rdi r11 r15 ebx r8d r9d r10d r12d r13d r14d
..B5.112:                       # Preds ..B5.112 ..B5.111
                                # Execution count [6.25e+00]
        movq      32(%r15), %rbp                                #882.7
        lea       (%r10,%r12,2), %eax                           #882.7
        movslq    %eax, %rax                                    #882.7
        lea       (%r12,%r12), %ecx                             #882.7
        movslq    %ecx, %rcx                                    #882.7
        incl      %r12d                                         #882.7
        movl      (%rbp,%rax,4), %ebp                           #882.7
        imull     %r8d, %ebp                                    #882.7
        movslq    %ebp, %rbp                                    #882.7
        addq      %r11, %rbp                                    #882.7
        movq      %rbp, (%rdx,%rcx,8)                           #882.7
        movq      32(%r15), %rbp                                #882.7
        movl      4(%rbp,%rax,4), %ebp                          #882.7
        imull     %r8d, %ebp                                    #882.7
        movslq    %ebp, %rbp                                    #882.7
        addq      %r11, %rbp                                    #882.7
        movq      %rbp, 8(%rdx,%rcx,8)                          #882.7
        cmpl      %r13d, %r12d                                  #882.7
        jb        ..B5.112      # Prob 64%                      #882.7
                                # LOE rdx rsi rdi r11 r15 ebx r8d r9d r10d r12d r13d r14d
..B5.113:                       # Preds ..B5.112
                                # Execution count [2.25e+00]
        movl      1632(%rsp), %ebp                              #[spill]
        lea       1(%r12,%r12), %ecx                            #882.7
                                # LOE rdx rsi rdi r11 r15 ecx ebx ebp r8d r9d r14d
..B5.114:                       # Preds ..B5.113 ..B5.110
                                # Execution count [2.50e+00]
        lea       -1(%rcx), %r10d                               #882.7
        cmpl      %r9d, %r10d                                   #882.7
        jae       ..B5.126      # Prob 10%                      #882.7
                                # LOE rdx rsi rdi r11 r15 ecx ebx ebp r8d r14d
..B5.115:                       # Preds ..B5.114
                                # Execution count [2.25e+00]
        imull     %ebx, %r14d                                   #882.7
        movslq    %ecx, %rcx                                    #882.7
        movslq    %r14d, %r14                                   #882.7
        addq      %rcx, %r14                                    #882.7
        movq      32(%r15), %r15                                #882.7
        movl      -4(%r15,%r14,4), %r9d                         #882.7
        imull     %r9d, %r8d                                    #882.7
        movslq    %r8d, %r8                                     #882.7
        addq      %r8, %r11                                     #882.7
        movq      %r11, -8(%rdx,%rcx,8)                         #882.7
        jmp       ..B5.126      # Prob 100%                     #882.7
                                # LOE rdx rsi rdi ebx ebp
..B5.116:                       # Preds ..B5.108
                                # Execution count [2.50e+00]
        movups    192(%rsp), %xmm0                              #884.7
        movups    208(%rsp), %xmm1                              #884.7
        movups    224(%rsp), %xmm2                              #884.7
        movups    240(%rsp), %xmm3                              #884.7
        movups    256(%rsp), %xmm4                              #884.7
        movups    272(%rsp), %xmm5                              #884.7
        movq      232(%rsp), %rdx                               #884.7
        movups    %xmm0, 960(%rsp)                              #884.7
        movups    %xmm1, 976(%rsp)                              #884.7
        movups    %xmm2, 992(%rsp)                              #884.7
        movups    %xmm3, 1008(%rsp)                             #884.7
        movups    %xmm4, 1024(%rsp)                             #884.7
        movups    %xmm5, 1040(%rsp)                             #884.7
        cmpl      $0, 276(%rsp)                                 #884.7
        jne       ..B5.122      # Prob 50%                      #884.7
                                # LOE rdx rsi rdi ebx ebp
..B5.117:                       # Preds ..B5.116
                                # Execution count [1.25e+00]
        testl     %ebp, %ebp                                    #884.7
        je        ..B5.120      # Prob 50%                      #884.7
                                # LOE rdx rsi rdi ebx ebp
..B5.118:                       # Preds ..B5.117
                                # Execution count [6.25e-01]
        cmpl      $0, 272(%rsp)                                 #884.7
        je        ..B5.120      # Prob 50%                      #884.7
                                # LOE rdx rsi rdi ebx ebp
..B5.119:                       # Preds ..B5.118
                                # Execution count [3.12e-01]
        movq      blank_args(%rip), %rdx                        #884.7
        movq      %rdx, 1552(%rsp)                              #884.7
        jmp       ..B5.126      # Prob 100%                     #884.7
                                # LOE rdx rsi rdi ebx ebp
..B5.120:                       # Preds ..B5.117 ..B5.118
                                # Execution count [3.12e-01]
        movq      %rdx, 1552(%rsp)                              #884.7
        jmp       ..B5.126      # Prob 100%                     #884.7
                                # LOE rdx rsi rdi ebx ebp
..B5.122:                       # Preds ..B5.116
                                # Execution count [1.25e+00]
        movq      976(%rsp), %r8                                #884.7
        testq     %r8, %r8                                      #884.7
        je        ..B5.124      # Prob 12%                      #884.7
                                # LOE rdx rsi rdi r8 ebx ebp
..B5.123:                       # Preds ..B5.122
                                # Execution count [1.10e+00]
        cmpl      $0, 1052(%rsp)                                #884.7
        jne       ..B5.125      # Prob 50%                      #884.7
                                # LOE rdx rsi rdi r8 ebx ebp
..B5.124:                       # Preds ..B5.123 ..B5.122
                                # Execution count [7.00e-01]
        movslq    992(%rsp), %rcx                               #884.7
        imulq     1760(%rsp), %rcx                              #884.7[spill]
        addq      %rcx, %rdx                                    #884.7
        movq      %rdx, 1552(%rsp)                              #884.7
        jmp       ..B5.126      # Prob 100%                     #884.7
                                # LOE rdx rsi rdi ebx ebp
..B5.125:                       # Preds ..B5.123
                                # Execution count [5.50e-01]
        movl      %ebx, %ecx                                    #884.7
        imull     24(%r8), %ecx                                 #884.7
        addl      988(%rsp), %ecx                               #884.7
        movslq    %ecx, %rcx                                    #884.7
        movq      32(%r8), %r8                                  #884.7
        movl      992(%rsp), %r9d                               #884.7
        imull     (%r8,%rcx,4), %r9d                            #884.7
        movslq    %r9d, %r9                                     #884.7
        addq      %r9, %rdx                                     #884.7
        movq      %rdx, 1552(%rsp)                              #884.7
                                # LOE rdx rsi rdi ebx ebp
..B5.126:                       # Preds ..B5.115 ..B5.114 ..B5.109 ..B5.119 ..B5.120
                                #       ..B5.124 ..B5.125
                                # Execution count [5.00e+00]
        cmpl      $-1, 316(%rsp)                                #885.23
        jge       ..B5.134      # Prob 50%                      #885.23
                                # LOE rdx rsi rdi ebx ebp
..B5.127:                       # Preds ..B5.126
                                # Execution count [2.50e+00]
        movl      316(%rsp), %r15d                              #886.7
        movl      %r15d, %r13d                                  #886.7
        negl      %r13d                                         #886.7
        movq      304(%rsp), %r10                               #886.7
        movq      328(%rsp), %r11                               #886.7
        movl      320(%rsp), %r12d                              #886.7
        movq      1560(%rsp), %rcx                              #886.43
        testl     %r13d, %r13d                                  #886.7
        jle       ..B5.144      # Prob 50%                      #886.7
                                # LOE rdx rcx rsi rdi r10 r11 ebx ebp r12d r13d r15d
..B5.128:                       # Preds ..B5.127
                                # Execution count [2.50e+00]
        movl      %r13d, %r8d                                   #886.7
        movl      $1, %eax                                      #886.7
        shrl      $31, %r8d                                     #886.7
        xorl      %r9d, %r9d                                    #886.7
        subl      %r15d, %r8d                                   #886.7
        sarl      $1, %r8d                                      #886.7
        movl      24(%r10), %r14d                               #886.7
        testl     %r8d, %r8d                                    #886.7
        jbe       ..B5.132      # Prob 10%                      #886.7
                                # LOE rdx rcx rsi rdi r10 r11 eax ebx ebp r8d r9d r12d r13d r14d
..B5.129:                       # Preds ..B5.128
                                # Execution count [2.25e+00]
        movl      %ebx, %eax                                    #886.7
        imull     %r14d, %eax                                   #886.7
        movl      %ebx, 1640(%rsp)                              #886.7[spill]
        movl      %ebp, 1632(%rsp)                              #886.7[spill]
        .align    16,0x90
                                # LOE rdx rcx rsi rdi r10 r11 eax r8d r9d r12d r13d r14d
..B5.130:                       # Preds ..B5.130 ..B5.129
                                # Execution count [6.25e+00]
        movq      32(%r10), %r15                                #886.7
        lea       (%rax,%r9,2), %ebx                            #886.7
        movslq    %ebx, %rbx                                    #886.7
        lea       (%r9,%r9), %ebp                               #886.7
        movslq    %ebp, %rbp                                    #886.7
        incl      %r9d                                          #886.7
        movl      (%r15,%rbx,4), %r15d                          #886.7
        imull     %r12d, %r15d                                  #886.7
        movslq    %r15d, %r15                                   #886.7
        addq      %r11, %r15                                    #886.7
        movq      %r15, (%rcx,%rbp,8)                           #886.7
        movq      32(%r10), %r15                                #886.7
        movl      4(%r15,%rbx,4), %ebx                          #886.7
        imull     %r12d, %ebx                                   #886.7
        movslq    %ebx, %rbx                                    #886.7
        addq      %r11, %rbx                                    #886.7
        movq      %rbx, 8(%rcx,%rbp,8)                          #886.7
        cmpl      %r8d, %r9d                                    #886.7
        jb        ..B5.130      # Prob 64%                      #886.7
                                # LOE rdx rcx rsi rdi r10 r11 eax r8d r9d r12d r13d r14d
..B5.131:                       # Preds ..B5.130
                                # Execution count [2.25e+00]
        movl      1640(%rsp), %ebx                              #[spill]
        lea       1(%r9,%r9), %eax                              #886.7
        movl      1632(%rsp), %ebp                              #[spill]
                                # LOE rdx rcx rsi rdi r10 r11 eax ebx ebp r12d r13d r14d
..B5.132:                       # Preds ..B5.131 ..B5.128
                                # Execution count [2.50e+00]
        lea       -1(%rax), %r8d                                #886.7
        cmpl      %r13d, %r8d                                   #886.7
        jae       ..B5.144      # Prob 10%                      #886.7
                                # LOE rdx rcx rsi rdi r10 r11 eax ebx ebp r12d r14d
..B5.133:                       # Preds ..B5.132
                                # Execution count [2.25e+00]
        imull     %ebx, %r14d                                   #886.7
        movslq    %eax, %rax                                    #886.7
        movslq    %r14d, %r14                                   #886.7
        addq      %rax, %r14                                    #886.7
        movq      32(%r10), %r10                                #886.7
        movl      -4(%r10,%r14,4), %r8d                         #886.7
        imull     %r8d, %r12d                                   #886.7
        movslq    %r12d, %r12                                   #886.7
        addq      %r12, %r11                                    #886.7
        movq      %r11, -8(%rcx,%rax,8)                         #886.7
        jmp       ..B5.144      # Prob 100%                     #886.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B5.134:                       # Preds ..B5.126
                                # Execution count [2.50e+00]
        movups    288(%rsp), %xmm0                              #888.7
        movups    304(%rsp), %xmm1                              #888.7
        movups    320(%rsp), %xmm2                              #888.7
        movups    336(%rsp), %xmm3                              #888.7
        movups    352(%rsp), %xmm4                              #888.7
        movups    368(%rsp), %xmm5                              #888.7
        movq      328(%rsp), %rcx                               #888.7
        movups    %xmm0, 1056(%rsp)                             #888.7
        movups    %xmm1, 1072(%rsp)                             #888.7
        movups    %xmm2, 1088(%rsp)                             #888.7
        movups    %xmm3, 1104(%rsp)                             #888.7
        movups    %xmm4, 1120(%rsp)                             #888.7
        movups    %xmm5, 1136(%rsp)                             #888.7
        cmpl      $0, 372(%rsp)                                 #888.7
        jne       ..B5.140      # Prob 50%                      #888.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B5.135:                       # Preds ..B5.134
                                # Execution count [1.25e+00]
        testl     %ebp, %ebp                                    #888.7
        je        ..B5.138      # Prob 50%                      #888.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B5.136:                       # Preds ..B5.135
                                # Execution count [6.25e-01]
        cmpl      $0, 368(%rsp)                                 #888.7
        je        ..B5.138      # Prob 50%                      #888.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B5.137:                       # Preds ..B5.136
                                # Execution count [3.12e-01]
        movq      blank_args(%rip), %rcx                        #888.7
        movq      %rcx, 1560(%rsp)                              #888.7
        jmp       ..B5.144      # Prob 100%                     #888.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B5.138:                       # Preds ..B5.135 ..B5.136
                                # Execution count [3.12e-01]
        movq      %rcx, 1560(%rsp)                              #888.7
        jmp       ..B5.144      # Prob 100%                     #888.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B5.140:                       # Preds ..B5.134
                                # Execution count [1.25e+00]
        movq      1072(%rsp), %r9                               #888.7
        testq     %r9, %r9                                      #888.7
        je        ..B5.142      # Prob 12%                      #888.7
                                # LOE rdx rcx rsi rdi r9 ebx ebp
..B5.141:                       # Preds ..B5.140
                                # Execution count [1.10e+00]
        cmpl      $0, 1148(%rsp)                                #888.7
        jne       ..B5.143      # Prob 50%                      #888.7
                                # LOE rdx rcx rsi rdi r9 ebx ebp
..B5.142:                       # Preds ..B5.141 ..B5.140
                                # Execution count [7.00e-01]
        movslq    1088(%rsp), %r8                               #888.7
        imulq     1760(%rsp), %r8                               #888.7[spill]
        addq      %r8, %rcx                                     #888.7
        movq      %rcx, 1560(%rsp)                              #888.7
        jmp       ..B5.144      # Prob 100%                     #888.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B5.143:                       # Preds ..B5.141
                                # Execution count [5.50e-01]
        movl      %ebx, %r8d                                    #888.7
        imull     24(%r9), %r8d                                 #888.7
        addl      1084(%rsp), %r8d                              #888.7
        movslq    %r8d, %r8                                     #888.7
        movq      32(%r9), %r9                                  #888.7
        movl      1088(%rsp), %r10d                             #888.7
        imull     (%r9,%r8,4), %r10d                            #888.7
        movslq    %r10d, %r10                                   #888.7
        addq      %r10, %rcx                                    #888.7
        movq      %rcx, 1560(%rsp)                              #888.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B5.144:                       # Preds ..B5.133 ..B5.132 ..B5.127 ..B5.137 ..B5.138
                                #       ..B5.142 ..B5.143
                                # Execution count [5.00e+00]
        cmpl      $-1, 412(%rsp)                                #889.23
        jge       ..B5.152      # Prob 50%                      #889.23
                                # LOE rdx rcx rsi rdi ebx ebp
..B5.145:                       # Preds ..B5.144
                                # Execution count [2.50e+00]
        movl      412(%rsp), %r9d                               #890.7
        movl      %r9d, 1728(%rsp)                              #890.7[spill]
        negl      %r9d                                          #890.7
        movq      400(%rsp), %r11                               #890.7
        movq      424(%rsp), %r12                               #890.7
        movl      416(%rsp), %r13d                              #890.7
        movq      1568(%rsp), %r8                               #890.43
        testl     %r9d, %r9d                                    #890.7
        jle       ..B5.162      # Prob 50%                      #890.7
                                # LOE rdx rcx rsi rdi r8 r11 r12 ebx ebp r9d r13d
..B5.146:                       # Preds ..B5.145
                                # Execution count [2.50e+00]
        movl      %r9d, %r15d                                   #890.7
        movl      $1, %eax                                      #890.7
        shrl      $31, %r15d                                    #890.7
        xorl      %r10d, %r10d                                  #890.7
        subl      1728(%rsp), %r15d                             #890.7[spill]
        sarl      $1, %r15d                                     #890.7
        movl      24(%r11), %r14d                               #890.7
        testl     %r15d, %r15d                                  #890.7
        jbe       ..B5.150      # Prob 10%                      #890.7
                                # LOE rdx rcx rsi rdi r8 r11 r12 eax ebx ebp r9d r10d r13d r14d r15d
..B5.147:                       # Preds ..B5.146
                                # Execution count [2.25e+00]
        movl      %ebx, %eax                                    #890.7
        movq      %rdi, 1648(%rsp)                              #890.7[spill]
        imull     %r14d, %eax                                   #890.7
        movl      %ebx, 1640(%rsp)                              #890.7[spill]
        movl      %ebp, 1632(%rsp)                              #890.7[spill]
        movl      %r15d, %edi                                   #890.7
        .align    16,0x90
                                # LOE rdx rcx rsi r8 r11 r12 eax edi r9d r10d r13d r14d
..B5.148:                       # Preds ..B5.148 ..B5.147
                                # Execution count [6.25e+00]
        movq      32(%r11), %r15                                #890.7
        lea       (%rax,%r10,2), %ebx                           #890.7
        movslq    %ebx, %rbx                                    #890.7
        lea       (%r10,%r10), %ebp                             #890.7
        movslq    %ebp, %rbp                                    #890.7
        incl      %r10d                                         #890.7
        movl      (%r15,%rbx,4), %r15d                          #890.7
        imull     %r13d, %r15d                                  #890.7
        movslq    %r15d, %r15                                   #890.7
        addq      %r12, %r15                                    #890.7
        movq      %r15, (%r8,%rbp,8)                            #890.7
        movq      32(%r11), %r15                                #890.7
        movl      4(%r15,%rbx,4), %ebx                          #890.7
        imull     %r13d, %ebx                                   #890.7
        movslq    %ebx, %rbx                                    #890.7
        addq      %r12, %rbx                                    #890.7
        movq      %rbx, 8(%r8,%rbp,8)                           #890.7
        cmpl      %edi, %r10d                                   #890.7
        jb        ..B5.148      # Prob 64%                      #890.7
                                # LOE rdx rcx rsi r8 r11 r12 eax edi r9d r10d r13d r14d
..B5.149:                       # Preds ..B5.148
                                # Execution count [2.25e+00]
        movq      1648(%rsp), %rdi                              #[spill]
        lea       1(%r10,%r10), %eax                            #890.7
        movl      1640(%rsp), %ebx                              #[spill]
        movl      1632(%rsp), %ebp                              #[spill]
                                # LOE rdx rcx rsi rdi r8 r11 r12 eax ebx ebp r9d r13d r14d
..B5.150:                       # Preds ..B5.149 ..B5.146
                                # Execution count [2.50e+00]
        lea       -1(%rax), %r10d                               #890.7
        cmpl      %r9d, %r10d                                   #890.7
        jae       ..B5.162      # Prob 10%                      #890.7
                                # LOE rdx rcx rsi rdi r8 r11 r12 eax ebx ebp r13d r14d
..B5.151:                       # Preds ..B5.150
                                # Execution count [2.25e+00]
        imull     %ebx, %r14d                                   #890.7
        movslq    %eax, %rax                                    #890.7
        movslq    %r14d, %r14                                   #890.7
        addq      %rax, %r14                                    #890.7
        movq      32(%r11), %r11                                #890.7
        movl      -4(%r11,%r14,4), %r9d                         #890.7
        imull     %r9d, %r13d                                   #890.7
        movslq    %r13d, %r13                                   #890.7
        addq      %r13, %r12                                    #890.7
        movq      %r12, -8(%r8,%rax,8)                          #890.7
        jmp       ..B5.162      # Prob 100%                     #890.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B5.152:                       # Preds ..B5.144
                                # Execution count [2.50e+00]
        movups    384(%rsp), %xmm0                              #892.7
        movups    400(%rsp), %xmm1                              #892.7
        movups    416(%rsp), %xmm2                              #892.7
        movups    432(%rsp), %xmm3                              #892.7
        movups    448(%rsp), %xmm4                              #892.7
        movups    464(%rsp), %xmm5                              #892.7
        movq      424(%rsp), %r8                                #892.7
        movups    %xmm0, 1152(%rsp)                             #892.7
        movups    %xmm1, 1168(%rsp)                             #892.7
        movups    %xmm2, 1184(%rsp)                             #892.7
        movups    %xmm3, 1200(%rsp)                             #892.7
        movups    %xmm4, 1216(%rsp)                             #892.7
        movups    %xmm5, 1232(%rsp)                             #892.7
        cmpl      $0, 468(%rsp)                                 #892.7
        jne       ..B5.158      # Prob 50%                      #892.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B5.153:                       # Preds ..B5.152
                                # Execution count [1.25e+00]
        testl     %ebp, %ebp                                    #892.7
        je        ..B5.156      # Prob 50%                      #892.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B5.154:                       # Preds ..B5.153
                                # Execution count [6.25e-01]
        cmpl      $0, 464(%rsp)                                 #892.7
        je        ..B5.156      # Prob 50%                      #892.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B5.155:                       # Preds ..B5.154
                                # Execution count [3.12e-01]
        movq      blank_args(%rip), %r8                         #892.7
        movq      %r8, 1568(%rsp)                               #892.7
        jmp       ..B5.162      # Prob 100%                     #892.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B5.156:                       # Preds ..B5.153 ..B5.154
                                # Execution count [3.12e-01]
        movq      %r8, 1568(%rsp)                               #892.7
        jmp       ..B5.162      # Prob 100%                     #892.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B5.158:                       # Preds ..B5.152
                                # Execution count [1.25e+00]
        movq      1168(%rsp), %r10                              #892.7
        testq     %r10, %r10                                    #892.7
        je        ..B5.160      # Prob 12%                      #892.7
                                # LOE rdx rcx rsi rdi r8 r10 ebx ebp
..B5.159:                       # Preds ..B5.158
                                # Execution count [1.10e+00]
        cmpl      $0, 1244(%rsp)                                #892.7
        jne       ..B5.161      # Prob 50%                      #892.7
                                # LOE rdx rcx rsi rdi r8 r10 ebx ebp
..B5.160:                       # Preds ..B5.159 ..B5.158
                                # Execution count [7.00e-01]
        movslq    1184(%rsp), %r9                               #892.7
        imulq     1760(%rsp), %r9                               #892.7[spill]
        addq      %r9, %r8                                      #892.7
        movq      %r8, 1568(%rsp)                               #892.7
        jmp       ..B5.162      # Prob 100%                     #892.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B5.161:                       # Preds ..B5.159
                                # Execution count [5.50e-01]
        movl      %ebx, %r9d                                    #892.7
        imull     24(%r10), %r9d                                #892.7
        addl      1180(%rsp), %r9d                              #892.7
        movslq    %r9d, %r9                                     #892.7
        movq      32(%r10), %r10                                #892.7
        movl      1184(%rsp), %r11d                             #892.7
        imull     (%r10,%r9,4), %r11d                           #892.7
        movslq    %r11d, %r11                                   #892.7
        addq      %r11, %r8                                     #892.7
        movq      %r8, 1568(%rsp)                               #892.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B5.162:                       # Preds ..B5.151 ..B5.150 ..B5.145 ..B5.155 ..B5.156
                                #       ..B5.160 ..B5.161
                                # Execution count [5.00e+00]
        cmpl      $-1, 508(%rsp)                                #893.23
        jge       ..B5.170      # Prob 50%                      #893.23
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B5.163:                       # Preds ..B5.162
                                # Execution count [2.50e+00]
        movl      508(%rsp), %r10d                              #894.7
        movl      %r10d, 1696(%rsp)                             #894.7[spill]
        negl      %r10d                                         #894.7
        movq      496(%rsp), %r12                               #894.7
        movq      520(%rsp), %r13                               #894.7
        movl      512(%rsp), %r14d                              #894.7
        movq      1576(%rsp), %r9                               #894.43
        testl     %r10d, %r10d                                  #894.7
        jle       ..B5.180      # Prob 50%                      #894.7
                                # LOE rdx rcx rsi rdi r8 r9 r12 r13 ebx ebp r10d r14d
..B5.164:                       # Preds ..B5.163
                                # Execution count [2.50e+00]
        movl      %r10d, %r15d                                  #894.7
        movl      $1, %eax                                      #894.7
        shrl      $31, %r15d                                    #894.7
        subl      1696(%rsp), %r15d                             #894.7[spill]
        sarl      $1, %r15d                                     #894.7
        movl      24(%r12), %r11d                               #894.7
        movl      $0, 1736(%rsp)                                #894.7[spill]
        testl     %r15d, %r15d                                  #894.7
        jbe       ..B5.168      # Prob 10%                      #894.7
                                # LOE rdx rcx rsi rdi r8 r9 r12 r13 eax ebx ebp r10d r11d r14d r15d
..B5.165:                       # Preds ..B5.164
                                # Execution count [2.25e+00]
        movl      %ebx, %eax                                    #894.7
        movq      %rsi, 1656(%rsp)                              #894.7[spill]
        movq      %rdi, 1648(%rsp)                              #894.7[spill]
        imull     %r11d, %eax                                   #894.7
        movl      %ebx, 1640(%rsp)                              #894.7[spill]
        movl      %ebp, 1632(%rsp)                              #894.7[spill]
        movl      %r15d, %esi                                   #894.7
        movl      1736(%rsp), %edi                              #894.7[spill]
        .align    16,0x90
                                # LOE rdx rcx r8 r9 r12 r13 eax esi edi r10d r11d r14d
..B5.166:                       # Preds ..B5.166 ..B5.165
                                # Execution count [6.25e+00]
        movq      32(%r12), %r15                                #894.7
        lea       (%rax,%rdi,2), %ebx                           #894.7
        movslq    %ebx, %rbx                                    #894.7
        lea       (%rdi,%rdi), %ebp                             #894.7
        movslq    %ebp, %rbp                                    #894.7
        incl      %edi                                          #894.7
        movl      (%r15,%rbx,4), %r15d                          #894.7
        imull     %r14d, %r15d                                  #894.7
        movslq    %r15d, %r15                                   #894.7
        addq      %r13, %r15                                    #894.7
        movq      %r15, (%r9,%rbp,8)                            #894.7
        movq      32(%r12), %r15                                #894.7
        movl      4(%r15,%rbx,4), %ebx                          #894.7
        imull     %r14d, %ebx                                   #894.7
        movslq    %ebx, %rbx                                    #894.7
        addq      %r13, %rbx                                    #894.7
        movq      %rbx, 8(%r9,%rbp,8)                           #894.7
        cmpl      %esi, %edi                                    #894.7
        jb        ..B5.166      # Prob 64%                      #894.7
                                # LOE rdx rcx r8 r9 r12 r13 eax esi edi r10d r11d r14d
..B5.167:                       # Preds ..B5.166
                                # Execution count [2.25e+00]
        movl      %edi, 1736(%rsp)                              #[spill]
        movl      %edi, %r15d                                   #894.7
        movq      1656(%rsp), %rsi                              #[spill]
        movq      1648(%rsp), %rdi                              #[spill]
        movl      1640(%rsp), %ebx                              #[spill]
        lea       1(%r15,%r15), %eax                            #894.7
        movl      1632(%rsp), %ebp                              #[spill]
                                # LOE rdx rcx rsi rdi r8 r9 r12 r13 eax ebx ebp r10d r11d r14d
..B5.168:                       # Preds ..B5.167 ..B5.164
                                # Execution count [2.50e+00]
        lea       -1(%rax), %r15d                               #894.7
        cmpl      %r10d, %r15d                                  #894.7
        jae       ..B5.180      # Prob 10%                      #894.7
                                # LOE rdx rcx rsi rdi r8 r9 r12 r13 eax ebx ebp r11d r14d
..B5.169:                       # Preds ..B5.168
                                # Execution count [2.25e+00]
        imull     %ebx, %r11d                                   #894.7
        movslq    %eax, %rax                                    #894.7
        movslq    %r11d, %r11                                   #894.7
        addq      %rax, %r11                                    #894.7
        movq      32(%r12), %r12                                #894.7
        movl      -4(%r12,%r11,4), %r10d                        #894.7
        imull     %r10d, %r14d                                  #894.7
        movslq    %r14d, %r14                                   #894.7
        addq      %r14, %r13                                    #894.7
        movq      %r13, -8(%r9,%rax,8)                          #894.7
        jmp       ..B5.180      # Prob 100%                     #894.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B5.170:                       # Preds ..B5.162
                                # Execution count [2.50e+00]
        movups    480(%rsp), %xmm0                              #896.7
        movups    496(%rsp), %xmm1                              #896.7
        movups    512(%rsp), %xmm2                              #896.7
        movups    528(%rsp), %xmm3                              #896.7
        movups    544(%rsp), %xmm4                              #896.7
        movups    560(%rsp), %xmm5                              #896.7
        movq      520(%rsp), %r9                                #896.7
        movups    %xmm0, 1248(%rsp)                             #896.7
        movups    %xmm1, 1264(%rsp)                             #896.7
        movups    %xmm2, 1280(%rsp)                             #896.7
        movups    %xmm3, 1296(%rsp)                             #896.7
        movups    %xmm4, 1312(%rsp)                             #896.7
        movups    %xmm5, 1328(%rsp)                             #896.7
        cmpl      $0, 564(%rsp)                                 #896.7
        jne       ..B5.176      # Prob 50%                      #896.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B5.171:                       # Preds ..B5.170
                                # Execution count [1.25e+00]
        testl     %ebp, %ebp                                    #896.7
        je        ..B5.174      # Prob 50%                      #896.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B5.172:                       # Preds ..B5.171
                                # Execution count [6.25e-01]
        cmpl      $0, 560(%rsp)                                 #896.7
        je        ..B5.174      # Prob 50%                      #896.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B5.173:                       # Preds ..B5.172
                                # Execution count [3.12e-01]
        movq      blank_args(%rip), %r9                         #896.7
        movq      %r9, 1576(%rsp)                               #896.7
        jmp       ..B5.180      # Prob 100%                     #896.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B5.174:                       # Preds ..B5.171 ..B5.172
                                # Execution count [3.12e-01]
        movq      %r9, 1576(%rsp)                               #896.7
        jmp       ..B5.180      # Prob 100%                     #896.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B5.176:                       # Preds ..B5.170
                                # Execution count [1.25e+00]
        movq      1264(%rsp), %r11                              #896.7
        testq     %r11, %r11                                    #896.7
        je        ..B5.178      # Prob 12%                      #896.7
                                # LOE rdx rcx rsi rdi r8 r9 r11 ebx ebp
..B5.177:                       # Preds ..B5.176
                                # Execution count [1.10e+00]
        cmpl      $0, 1340(%rsp)                                #896.7
        jne       ..B5.179      # Prob 50%                      #896.7
                                # LOE rdx rcx rsi rdi r8 r9 r11 ebx ebp
..B5.178:                       # Preds ..B5.177 ..B5.176
                                # Execution count [7.00e-01]
        movslq    1280(%rsp), %r10                              #896.7
        imulq     1760(%rsp), %r10                              #896.7[spill]
        addq      %r10, %r9                                     #896.7
        movq      %r9, 1576(%rsp)                               #896.7
        jmp       ..B5.180      # Prob 100%                     #896.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B5.179:                       # Preds ..B5.177
                                # Execution count [5.50e-01]
        movl      %ebx, %r10d                                   #896.7
        imull     24(%r11), %r10d                               #896.7
        addl      1276(%rsp), %r10d                             #896.7
        movslq    %r10d, %r10                                   #896.7
        movq      32(%r11), %r11                                #896.7
        movl      1280(%rsp), %r12d                             #896.7
        imull     (%r11,%r10,4), %r12d                          #896.7
        movslq    %r12d, %r12                                   #896.7
        addq      %r12, %r9                                     #896.7
        movq      %r9, 1576(%rsp)                               #896.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B5.180:                       # Preds ..B5.169 ..B5.168 ..B5.163 ..B5.173 ..B5.174
                                #       ..B5.178 ..B5.179
                                # Execution count [5.00e+00]
        cmpl      $-1, 604(%rsp)                                #897.23
        jge       ..B5.188      # Prob 50%                      #897.23
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B5.181:                       # Preds ..B5.180
                                # Execution count [2.50e+00]
        movl      604(%rsp), %r11d                              #898.7
        movl      %r11d, 1688(%rsp)                             #898.7[spill]
        negl      %r11d                                         #898.7
        movl      608(%rsp), %r10d                              #898.7
        movq      592(%rsp), %r13                               #898.7
        movq      616(%rsp), %r14                               #898.7
        movl      %r10d, 1744(%rsp)                             #898.7[spill]
        movq      1584(%rsp), %rax                              #898.43
        testl     %r11d, %r11d                                  #898.7
        jle       ..B5.198      # Prob 50%                      #898.7
                                # LOE rax rdx rcx rsi rdi r8 r9 r13 r14 ebx ebp r11d
..B5.182:                       # Preds ..B5.181
                                # Execution count [2.50e+00]
        movl      %r11d, %r15d                                  #898.7
        movl      $1, %r10d                                     #898.7
        shrl      $31, %r15d                                    #898.7
        subl      1688(%rsp), %r15d                             #898.7[spill]
        sarl      $1, %r15d                                     #898.7
        movl      24(%r13), %r12d                               #898.7
        movl      $0, 1712(%rsp)                                #898.7[spill]
        testl     %r15d, %r15d                                  #898.7
        jbe       ..B5.186      # Prob 10%                      #898.7
                                # LOE rax rdx rcx rsi rdi r8 r9 r13 r14 ebx ebp r10d r11d r12d r15d
..B5.183:                       # Preds ..B5.182
                                # Execution count [2.25e+00]
        movl      %ebx, %r10d                                   #898.7
        movq      %rsi, 1656(%rsp)                              #898.7[spill]
        movq      %rdi, 1648(%rsp)                              #898.7[spill]
        movl      %ebp, 1632(%rsp)                              #898.7[spill]
        imull     %r12d, %r10d                                  #898.7
        movq      %rdx, 1664(%rsp)                              #898.7[spill]
        movl      %ebx, 1640(%rsp)                              #898.7[spill]
        movl      %r15d, %ebp                                   #898.7
        movl      1712(%rsp), %esi                              #898.7[spill]
        movl      1744(%rsp), %edi                              #898.7[spill]
                                # LOE rax rcx r8 r9 r13 r14 ebp esi edi r10d r11d r12d
..B5.184:                       # Preds ..B5.184 ..B5.183
                                # Execution count [6.25e+00]
        movq      32(%r13), %r15                                #898.7
        lea       (%r10,%rsi,2), %edx                           #898.7
        movslq    %edx, %rdx                                    #898.7
        lea       (%rsi,%rsi), %ebx                             #898.7
        movslq    %ebx, %rbx                                    #898.7
        incl      %esi                                          #898.7
        movl      (%r15,%rdx,4), %r15d                          #898.7
        imull     %edi, %r15d                                   #898.7
        movslq    %r15d, %r15                                   #898.7
        addq      %r14, %r15                                    #898.7
        movq      %r15, (%rax,%rbx,8)                           #898.7
        movq      32(%r13), %r15                                #898.7
        movl      4(%r15,%rdx,4), %edx                          #898.7
        imull     %edi, %edx                                    #898.7
        movslq    %edx, %rdx                                    #898.7
        addq      %r14, %rdx                                    #898.7
        movq      %rdx, 8(%rax,%rbx,8)                          #898.7
        cmpl      %ebp, %esi                                    #898.7
        jb        ..B5.184      # Prob 64%                      #898.7
                                # LOE rax rcx r8 r9 r13 r14 ebp esi edi r10d r11d r12d
..B5.185:                       # Preds ..B5.184
                                # Execution count [2.25e+00]
        movl      %esi, 1712(%rsp)                              #[spill]
        movl      %esi, %r10d                                   #898.7
        movq      1664(%rsp), %rdx                              #[spill]
        movq      1656(%rsp), %rsi                              #[spill]
        movq      1648(%rsp), %rdi                              #[spill]
        lea       1(%r10,%r10), %r10d                           #898.7
        movl      1640(%rsp), %ebx                              #[spill]
        movl      1632(%rsp), %ebp                              #[spill]
                                # LOE rax rdx rcx rsi rdi r8 r9 r13 r14 ebx ebp r10d r11d r12d
..B5.186:                       # Preds ..B5.185 ..B5.182
                                # Execution count [2.50e+00]
        lea       -1(%r10), %r15d                               #898.7
        cmpl      %r11d, %r15d                                  #898.7
        jae       ..B5.198      # Prob 10%                      #898.7
                                # LOE rax rdx rcx rsi rdi r8 r9 r13 r14 ebx ebp r10d r12d
..B5.187:                       # Preds ..B5.186
                                # Execution count [2.25e+00]
        imull     %ebx, %r12d                                   #898.7
        movslq    %r10d, %r10                                   #898.7
        movslq    %r12d, %r12                                   #898.7
        addq      %r10, %r12                                    #898.7
        movq      32(%r13), %r13                                #898.7
        movl      1744(%rsp), %r11d                             #898.7[spill]
        imull     -4(%r13,%r12,4), %r11d                        #898.7
        movslq    %r11d, %r11                                   #898.7
        addq      %r11, %r14                                    #898.7
        movq      %r14, -8(%rax,%r10,8)                         #898.7
        jmp       ..B5.198      # Prob 100%                     #898.7
                                # LOE rax rdx rcx rsi rdi r8 r9 ebx ebp
..B5.188:                       # Preds ..B5.180
                                # Execution count [2.50e+00]
        movups    576(%rsp), %xmm0                              #900.7
        movups    592(%rsp), %xmm1                              #900.7
        movups    608(%rsp), %xmm2                              #900.7
        movups    624(%rsp), %xmm3                              #900.7
        movups    640(%rsp), %xmm4                              #900.7
        movups    656(%rsp), %xmm5                              #900.7
        movq      616(%rsp), %rax                               #900.7
        movups    %xmm0, 1344(%rsp)                             #900.7
        movups    %xmm1, 1360(%rsp)                             #900.7
        movups    %xmm2, 1376(%rsp)                             #900.7
        movups    %xmm3, 1392(%rsp)                             #900.7
        movups    %xmm4, 1408(%rsp)                             #900.7
        movups    %xmm5, 1424(%rsp)                             #900.7
        cmpl      $0, 660(%rsp)                                 #900.7
        jne       ..B5.194      # Prob 50%                      #900.7
                                # LOE rax rdx rcx rsi rdi r8 r9 ebx ebp
..B5.189:                       # Preds ..B5.188
                                # Execution count [1.25e+00]
        testl     %ebp, %ebp                                    #900.7
        je        ..B5.192      # Prob 50%                      #900.7
                                # LOE rax rdx rcx rsi rdi r8 r9 ebx ebp
..B5.190:                       # Preds ..B5.189
                                # Execution count [6.25e-01]
        cmpl      $0, 656(%rsp)                                 #900.7
        je        ..B5.192      # Prob 50%                      #900.7
                                # LOE rax rdx rcx rsi rdi r8 r9 ebx ebp
..B5.191:                       # Preds ..B5.190
                                # Execution count [3.12e-01]
        movq      blank_args(%rip), %rax                        #900.7
        movq      %rax, 1584(%rsp)                              #900.7
        jmp       ..B5.198      # Prob 100%                     #900.7
                                # LOE rax rdx rcx rsi rdi r8 r9 ebx ebp
..B5.192:                       # Preds ..B5.189 ..B5.190
                                # Execution count [3.12e-01]
        movq      %rax, 1584(%rsp)                              #900.7
        jmp       ..B5.198      # Prob 100%                     #900.7
                                # LOE rax rdx rcx rsi rdi r8 r9 ebx ebp
..B5.194:                       # Preds ..B5.188
                                # Execution count [1.25e+00]
        movq      1360(%rsp), %r11                              #900.7
        testq     %r11, %r11                                    #900.7
        je        ..B5.196      # Prob 12%                      #900.7
                                # LOE rax rdx rcx rsi rdi r8 r9 r11 ebx ebp
..B5.195:                       # Preds ..B5.194
                                # Execution count [1.10e+00]
        cmpl      $0, 1436(%rsp)                                #900.7
        jne       ..B5.197      # Prob 50%                      #900.7
                                # LOE rax rdx rcx rsi rdi r8 r9 r11 ebx ebp
..B5.196:                       # Preds ..B5.195 ..B5.194
                                # Execution count [7.00e-01]
        movslq    1376(%rsp), %r10                              #900.7
        imulq     1760(%rsp), %r10                              #900.7[spill]
        addq      %r10, %rax                                    #900.7
        movq      %rax, 1584(%rsp)                              #900.7
        jmp       ..B5.198      # Prob 100%                     #900.7
                                # LOE rax rdx rcx rsi rdi r8 r9 ebx ebp
..B5.197:                       # Preds ..B5.195
                                # Execution count [5.50e-01]
        movl      %ebx, %r10d                                   #900.7
        imull     24(%r11), %r10d                               #900.7
        addl      1372(%rsp), %r10d                             #900.7
        movslq    %r10d, %r10                                   #900.7
        movq      32(%r11), %r11                                #900.7
        movl      1376(%rsp), %r12d                             #900.7
        imull     (%r11,%r10,4), %r12d                          #900.7
        movslq    %r12d, %r12                                   #900.7
        addq      %r12, %rax                                    #900.7
        movq      %rax, 1584(%rsp)                              #900.7
                                # LOE rax rdx rcx rsi rdi r8 r9 ebx ebp
..B5.198:                       # Preds ..B5.187 ..B5.186 ..B5.181 ..B5.191 ..B5.192
                                #       ..B5.196 ..B5.197
                                # Execution count [5.00e+00]
        cmpl      $-1, 700(%rsp)                                #901.23
        jge       ..B5.206      # Prob 50%                      #901.23
                                # LOE rax rdx rcx rsi rdi r8 r9 ebx ebp
..B5.199:                       # Preds ..B5.198
                                # Execution count [2.50e+00]
        movl      700(%rsp), %r10d                              #902.7
        movl      %r10d, 1680(%rsp)                             #902.7[spill]
        negl      %r10d                                         #902.7
        movq      688(%rsp), %r14                               #902.7
        movq      712(%rsp), %r13                               #902.7
        movl      704(%rsp), %r12d                              #902.7
        movq      1592(%rsp), %r11                              #902.43
        movl      %r10d, 1704(%rsp)                             #902.7[spill]
        testl     %r10d, %r10d                                  #902.7
        jle       ..B5.216      # Prob 50%                      #902.7
                                # LOE rax rdx rcx rsi rdi r8 r9 r11 r13 r14 ebx ebp r10d r12d
..B5.200:                       # Preds ..B5.199
                                # Execution count [2.50e+00]
        shrl      $31, %r10d                                    #902.7
        subl      1680(%rsp), %r10d                             #902.7[spill]
        movl      24(%r14), %r15d                               #902.7
        sarl      $1, %r10d                                     #902.7
        movl      %r15d, 1720(%rsp)                             #902.7[spill]
        movl      $1, %r15d                                     #902.7
        movl      $0, 1752(%rsp)                                #902.7[spill]
        movl      %r10d, 1672(%rsp)                             #902.7[spill]
        testl     %r10d, %r10d                                  #902.7
        jbe       ..B5.204      # Prob 10%                      #902.7
                                # LOE rax rdx rcx rsi rdi r8 r9 r11 r13 r14 ebx ebp r12d r15d
..B5.201:                       # Preds ..B5.200
                                # Execution count [2.25e+00]
        movl      %ebx, %r10d                                   #902.7
        movq      %rsi, 1656(%rsp)                              #902.7[spill]
        movq      %rdi, 1648(%rsp)                              #902.7[spill]
        imull     1720(%rsp), %r10d                             #902.7[spill]
        movl      1672(%rsp), %esi                              #902.7[spill]
        movl      1752(%rsp), %edi                              #902.7[spill]
        movl      %ebx, 1640(%rsp)                              #902.7[spill]
        movl      %ebp, 1632(%rsp)                              #902.7[spill]
        .align    16,0x90
                                # LOE rax rdx rcx r8 r9 r11 r13 r14 esi edi r10d r12d
..B5.202:                       # Preds ..B5.202 ..B5.201
                                # Execution count [6.25e+00]
        movq      32(%r14), %r15                                #902.7
        lea       (%r10,%rdi,2), %ebp                           #902.7
        movslq    %ebp, %rbp                                    #902.7
        lea       (%rdi,%rdi), %ebx                             #902.7
        movslq    %ebx, %rbx                                    #902.7
        incl      %edi                                          #902.7
        movl      (%r15,%rbp,4), %r15d                          #902.7
        imull     %r12d, %r15d                                  #902.7
        movslq    %r15d, %r15                                   #902.7
        addq      %r13, %r15                                    #902.7
        movq      %r15, (%r11,%rbx,8)                           #902.7
        movq      32(%r14), %r15                                #902.7
        movl      4(%r15,%rbp,4), %ebp                          #902.7
        imull     %r12d, %ebp                                   #902.7
        movslq    %ebp, %rbp                                    #902.7
        addq      %r13, %rbp                                    #902.7
        movq      %rbp, 8(%r11,%rbx,8)                          #902.7
        cmpl      %esi, %edi                                    #902.7
        jb        ..B5.202      # Prob 64%                      #902.7
                                # LOE rax rdx rcx r8 r9 r11 r13 r14 esi edi r10d r12d
..B5.203:                       # Preds ..B5.202
                                # Execution count [2.25e+00]
        movl      %edi, 1752(%rsp)                              #[spill]
        movl      %edi, %r10d                                   #902.7
        movq      1656(%rsp), %rsi                              #[spill]
        movq      1648(%rsp), %rdi                              #[spill]
        movl      1640(%rsp), %ebx                              #[spill]
        lea       1(%r10,%r10), %r15d                           #902.7
        movl      1632(%rsp), %ebp                              #[spill]
                                # LOE rax rdx rcx rsi rdi r8 r9 r11 r13 r14 ebx ebp r12d r15d
..B5.204:                       # Preds ..B5.203 ..B5.200
                                # Execution count [2.50e+00]
        lea       -1(%r15), %r10d                               #902.7
        cmpl      1704(%rsp), %r10d                             #902.7[spill]
        jae       ..B5.216      # Prob 10%                      #902.7
                                # LOE rax rdx rcx rsi rdi r8 r9 r11 r13 r14 ebx ebp r12d r15d
..B5.205:                       # Preds ..B5.204
                                # Execution count [2.25e+00]
        movl      1720(%rsp), %r10d                             #902.7[spill]
        imull     %ebx, %r10d                                   #902.7
        movslq    %r15d, %r15                                   #902.7
        movslq    %r10d, %r10                                   #902.7
        addq      %r15, %r10                                    #902.7
        movq      32(%r14), %r14                                #902.7
        imull     -4(%r14,%r10,4), %r12d                        #902.7
        movslq    %r12d, %r12                                   #902.7
        addq      %r12, %r13                                    #902.7
        movq      %r13, -8(%r11,%r15,8)                         #902.7
        jmp       ..B5.216      # Prob 100%                     #902.7
                                # LOE rax rdx rcx rsi rdi r8 r9 r11 ebx ebp
..B5.206:                       # Preds ..B5.198
                                # Execution count [2.50e+00]
        movups    672(%rsp), %xmm0                              #904.7
        movups    688(%rsp), %xmm1                              #904.7
        movups    704(%rsp), %xmm2                              #904.7
        movups    720(%rsp), %xmm3                              #904.7
        movups    736(%rsp), %xmm4                              #904.7
        movups    752(%rsp), %xmm5                              #904.7
        movq      712(%rsp), %r11                               #904.7
        movups    %xmm0, 1440(%rsp)                             #904.7
        movups    %xmm1, 1456(%rsp)                             #904.7
        movups    %xmm2, 1472(%rsp)                             #904.7
        movups    %xmm3, 1488(%rsp)                             #904.7
        movups    %xmm4, 1504(%rsp)                             #904.7
        movups    %xmm5, 1520(%rsp)                             #904.7
        cmpl      $0, 756(%rsp)                                 #904.7
        jne       ..B5.212      # Prob 50%                      #904.7
                                # LOE rax rdx rcx rsi rdi r8 r9 r11 ebx ebp
..B5.207:                       # Preds ..B5.206
                                # Execution count [1.25e+00]
        testl     %ebp, %ebp                                    #904.7
        je        ..B5.210      # Prob 50%                      #904.7
                                # LOE rax rdx rcx rsi rdi r8 r9 r11 ebx ebp
..B5.208:                       # Preds ..B5.207
                                # Execution count [6.25e-01]
        cmpl      $0, 752(%rsp)                                 #904.7
        je        ..B5.210      # Prob 50%                      #904.7
                                # LOE rax rdx rcx rsi rdi r8 r9 r11 ebx ebp
..B5.209:                       # Preds ..B5.208
                                # Execution count [3.12e-01]
        movq      blank_args(%rip), %r11                        #904.7
        movq      %r11, 1592(%rsp)                              #904.7
        jmp       ..B5.216      # Prob 100%                     #904.7
                                # LOE rax rdx rcx rsi rdi r8 r9 r11 ebx ebp
..B5.210:                       # Preds ..B5.207 ..B5.208
                                # Execution count [3.12e-01]
        movq      %r11, 1592(%rsp)                              #904.7
        jmp       ..B5.216      # Prob 100%                     #904.7
                                # LOE rax rdx rcx rsi rdi r8 r9 r11 ebx ebp
..B5.212:                       # Preds ..B5.206
                                # Execution count [1.25e+00]
        movq      1456(%rsp), %r12                              #904.7
        testq     %r12, %r12                                    #904.7
        je        ..B5.214      # Prob 12%                      #904.7
                                # LOE rax rdx rcx rsi rdi r8 r9 r11 r12 ebx ebp
..B5.213:                       # Preds ..B5.212
                                # Execution count [1.10e+00]
        cmpl      $0, 1532(%rsp)                                #904.7
        jne       ..B5.215      # Prob 50%                      #904.7
                                # LOE rax rdx rcx rsi rdi r8 r9 r11 r12 ebx ebp
..B5.214:                       # Preds ..B5.213 ..B5.212
                                # Execution count [7.00e-01]
        movslq    1472(%rsp), %r10                              #904.7
        imulq     1760(%rsp), %r10                              #904.7[spill]
        addq      %r10, %r11                                    #904.7
        movq      %r11, 1592(%rsp)                              #904.7
        jmp       ..B5.216      # Prob 100%                     #904.7
                                # LOE rax rdx rcx rsi rdi r8 r9 r11 ebx ebp
..B5.215:                       # Preds ..B5.213
                                # Execution count [5.50e-01]
        movl      %ebx, %r10d                                   #904.7
        imull     24(%r12), %r10d                               #904.7
        addl      1468(%rsp), %r10d                             #904.7
        movslq    %r10d, %r10                                   #904.7
        movq      32(%r12), %r12                                #904.7
        movl      1472(%rsp), %r13d                             #904.7
        imull     (%r12,%r10,4), %r13d                          #904.7
        movslq    %r13d, %r13                                   #904.7
        addq      %r13, %r11                                    #904.7
        movq      %r11, 1592(%rsp)                              #904.7
                                # LOE rax rdx rcx rsi rdi r8 r9 r11 ebx ebp
..B5.216:                       # Preds ..B5.205 ..B5.204 ..B5.199 ..B5.209 ..B5.210
                                #       ..B5.214 ..B5.215
                                # Execution count [5.00e+00]
        pushq     %r11                                          #906.5
	.cfi_def_cfa_offset 1864
        pushq     %rax                                          #906.5
	.cfi_def_cfa_offset 1872
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.419:
        call      *1792(%rsp)                                   #906.5[spill]
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.420:
                                # LOE ebx ebp
..B5.264:                       # Preds ..B5.216
                                # Execution count [5.00e+00]
        addq      $16, %rsp                                     #906.5
	.cfi_def_cfa_offset 1856
                                # LOE ebx ebp
..B5.217:                       # Preds ..B5.264
                                # Execution count [5.00e+00]
        incl      %ebx                                          #868.32
        incq      1760(%rsp)                                    #868.32[spill]
        cmpl      1768(%rsp), %ebx                              #868.23[spill]
        jl        ..B5.70       # Prob 82%                      #868.23
                                # LOE ebx ebp
..B5.218:                       # Preds ..B5.217
                                # Execution count [9.00e-01]
        movl      1768(%rsp), %r10d                             #[spill]
        movq      1784(%rsp), %rbx                              #[spill]
        movq      1608(%rsp), %rbp                              #[spill]
        movq      1600(%rsp), %r12                              #[spill]
	.cfi_restore 12
        cmpl      16(%rbx), %r10d                               #909.18
        je        ..B5.220      # Prob 50%                      #909.18
                                # LOE rbp r12 r13 r14 r10d
..B5.219:                       # Preds ..B5.243 ..B5.218
                                # Execution count [5.00e-01]
        testl     %r10d, %r10d                                  #909.47
        jne       ..B5.221      # Prob 78%                      #909.47
                                # LOE rbp r12 r13 r14
..B5.220:                       # Preds ..B5.219 ..B5.243 ..B5.218
                                # Execution count [6.10e-01]
        movl      $8, %edi                                      #910.5
        lea       (%rsp), %rsi                                  #910.5
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.423:
#       op_mpi_wait_all(int, op_arg *)
        call      op_mpi_wait_all                               #910.5
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.424:
                                # LOE rbp r12 r13 r14
..B5.221:                       # Preds ..B5.220 ..B5.219
                                # Execution count [1.00e+00]
        movl      $8, %edi                                      #913.3
        lea       (%rsp), %rsi                                  #913.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.425:
#       op_mpi_set_dirtybit(int, op_arg *)
        call      op_mpi_set_dirtybit                           #913.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.426:
                                # LOE rbp r12 r13 r14
..B5.222:                       # Preds ..B5.221
                                # Execution count [1.00e+00]
        movq      1584(%rsp), %rsi                              #923.3
        lea       2432(%rsp), %rdi                              #923.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.427:
#       op_mpi_reduce_double(op_arg *, double *)
        call      op_mpi_reduce_double                          #923.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.428:
                                # LOE rbp r12 r13 r14
..B5.223:                       # Preds ..B5.222
                                # Execution count [1.00e+00]
        movq      1592(%rsp), %rsi                              #924.3
        lea       2528(%rsp), %rdi                              #924.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.429:
#       op_mpi_reduce_double(op_arg *, double *)
        call      op_mpi_reduce_double                          #924.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.430:
                                # LOE rbp r12 r13 r14
..B5.224:                       # Preds ..B5.223
                                # Execution count [1.00e+00]
        lea       768(%rsp), %rdi                               #927.3
        lea       776(%rsp), %rsi                               #927.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.431:
#       op_timers_core(double *, double *)
        call      op_timers_core                                #927.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.432:
                                # LOE rbp r12 r13 r14
..B5.225:                       # Preds ..B5.224
                                # Execution count [1.00e+00]
        movsd     776(%rsp), %xmm0                              #932.3
        movq      %rbp, %rdi                                    #932.3
        subsd     1624(%rsp), %xmm0                             #932.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.433:
#       op_mpi_perf_time(const char *, double)
        call      op_mpi_perf_time                              #932.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_.434:
                                # LOE r12 r13 r14
..B5.226:                       # Preds ..B5.225
                                # Execution count [1.00e+00]
        cmpl      $-1, 1884(%rsp)                               #935.18
        jge       ..B5.228      # Prob 78%                      #935.18
                                # LOE r12 r13 r14
..B5.227:                       # Preds ..B5.226
                                # Execution count [2.20e-01]
        movq      1536(%rsp), %rdi                              #936.5
#       free(void *)
        call      free                                          #936.5
                                # LOE r12 r13 r14
..B5.228:                       # Preds ..B5.227 ..B5.226
                                # Execution count [1.00e+00]
        cmpl      $-1, 1980(%rsp)                               #938.18
        jge       ..B5.230      # Prob 78%                      #938.18
                                # LOE r12 r13 r14
..B5.229:                       # Preds ..B5.228
                                # Execution count [2.20e-01]
        movq      1544(%rsp), %rdi                              #939.5
#       free(void *)
        call      free                                          #939.5
                                # LOE r12 r13 r14
..B5.230:                       # Preds ..B5.229 ..B5.228
                                # Execution count [1.00e+00]
        cmpl      $-1, 2076(%rsp)                               #941.18
        jge       ..B5.232      # Prob 78%                      #941.18
                                # LOE r12 r13 r14
..B5.231:                       # Preds ..B5.230
                                # Execution count [2.20e-01]
        movq      1552(%rsp), %rdi                              #942.5
#       free(void *)
        call      free                                          #942.5
                                # LOE r12 r13 r14
..B5.232:                       # Preds ..B5.231 ..B5.230
                                # Execution count [1.00e+00]
        cmpl      $-1, 2172(%rsp)                               #944.18
        jge       ..B5.234      # Prob 78%                      #944.18
                                # LOE r12 r13 r14
..B5.233:                       # Preds ..B5.232
                                # Execution count [2.20e-01]
        movq      1560(%rsp), %rdi                              #945.5
#       free(void *)
        call      free                                          #945.5
                                # LOE r12 r13 r14
..B5.234:                       # Preds ..B5.233 ..B5.232
                                # Execution count [1.00e+00]
        cmpl      $-1, 2268(%rsp)                               #947.18
        jge       ..B5.236      # Prob 78%                      #947.18
                                # LOE r12 r13 r14
..B5.235:                       # Preds ..B5.234
                                # Execution count [2.20e-01]
        movq      1568(%rsp), %rdi                              #948.5
#       free(void *)
        call      free                                          #948.5
                                # LOE r12 r13 r14
..B5.236:                       # Preds ..B5.235 ..B5.234
                                # Execution count [1.00e+00]
        cmpl      $-1, 2364(%rsp)                               #950.18
        jge       ..B5.238      # Prob 78%                      #950.18
                                # LOE r12 r13 r14
..B5.237:                       # Preds ..B5.236
                                # Execution count [2.20e-01]
        movq      1576(%rsp), %rdi                              #951.5
#       free(void *)
        call      free                                          #951.5
                                # LOE r12 r13 r14
..B5.238:                       # Preds ..B5.237 ..B5.236
                                # Execution count [1.00e+00]
        cmpl      $-1, 2460(%rsp)                               #953.18
        jge       ..B5.240      # Prob 78%                      #953.18
                                # LOE r12 r13 r14
..B5.239:                       # Preds ..B5.238
                                # Execution count [2.20e-01]
        movq      1584(%rsp), %rdi                              #954.5
#       free(void *)
        call      free                                          #954.5
                                # LOE r12 r13 r14
..B5.240:                       # Preds ..B5.239 ..B5.238
                                # Execution count [1.00e+00]
        cmpl      $-1, 2556(%rsp)                               #956.18
        jge       ..B5.242      # Prob 78%                      #956.18
                                # LOE r12 r13 r14
..B5.241:                       # Preds ..B5.240
                                # Execution count [2.20e-01]
        movq      1592(%rsp), %rdi                              #957.5
#       free(void *)
        call      free                                          #957.5
                                # LOE r12 r13 r14
..B5.242:                       # Preds ..B5.241 ..B5.240
                                # Execution count [1.00e+00]
        addq      $1808, %rsp                                   #959.1
	.cfi_def_cfa_offset 48
	.cfi_restore 6
        popq      %rbp                                          #959.1
	.cfi_def_cfa_offset 40
	.cfi_restore 3
        popq      %rbx                                          #959.1
	.cfi_def_cfa_offset 32
	.cfi_restore 15
        popq      %r15                                          #959.1
	.cfi_def_cfa_offset 24
	.cfi_restore 14
        popq      %r14                                          #959.1
	.cfi_def_cfa_offset 16
	.cfi_restore 13
        popq      %r13                                          #959.1
	.cfi_def_cfa_offset 8
        ret                                                     #959.1
	.cfi_def_cfa_offset 1856
	.cfi_offset 3, -40
	.cfi_offset 6, -48
	.cfi_offset 13, -16
	.cfi_offset 14, -24
	.cfi_offset 15, -32
                                # LOE
..B5.243:                       # Preds ..B5.68
                                # Execution count [1.00e-01]: Infreq
        cmpl      16(%rbx), %r10d                               #909.18
        je        ..B5.220      # Prob 50%                      #909.18
        jmp       ..B5.219      # Prob 100%                     #909.18
        .align    16,0x90
                                # LOE rbp r12 r13 r14 r10d
	.cfi_endproc
# mark_end;
	.type	_Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_,@function
	.size	_Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_,.-_Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_
	.data
# -- End  _Z11op_par_loopIKdS0_S0_S0_S0_S0_ddEvPFvPT_PT0_PT1_PT2_PT3_PT4_PT5_PT6_EPKcP11op_set_core6op_argSN_SN_SN_SN_SN_SN_SN_
	.section .text._Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_, "xaG",@progbits,_Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_,comdat
..TXTST5:
# -- Begin  _Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_
	.section .text._Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_, "xaG",@progbits,_Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_,comdat
# mark_begin;
       .align    16,0x90
	.weak _Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_
# --- op_par_loop<const double, const double, const double, const double, const double, double>(void (*)(const double *, const double *, const double *, const double *, const double *, double *), const char *, op_set, op_arg, op_arg, op_arg, op_arg, op_arg, op_arg)
_Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_:
# parameter 1: %rdi
# parameter 2: %rsi
# parameter 3: %rdx
# parameter 4: 1360 + %rsp
# parameter 5: 1456 + %rsp
# parameter 6: 1552 + %rsp
# parameter 7: 1648 + %rsp
# parameter 8: 1744 + %rsp
# parameter 9: 1840 + %rsp
..B6.1:                         # Preds ..B6.0
                                # Execution count [1.00e+00]
	.cfi_startproc
	.cfi_personality 0x3,__gxx_personality_v0
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.453:
..L454:
                                                        #534.70
        pushq     %r13                                          #534.70
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
        pushq     %r14                                          #534.70
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
        pushq     %r15                                          #534.70
	.cfi_def_cfa_offset 32
	.cfi_offset 15, -32
        pushq     %rbx                                          #534.70
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
        pushq     %rbp                                          #534.70
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
        subq      $1312, %rsp                                   #534.70
	.cfi_def_cfa_offset 1360
        movq      %rdx, %rbx                                    #534.70
        pxor      %xmm0, %xmm0                                  #536.16
        movq      %rsi, %rbp                                    #534.70
        movups    %xmm0, 1152(%rsp)                             #536.16
        movq      %rdi, %r15                                    #534.70
        movups    %xmm0, 1168(%rsp)                             #536.16
        movups    %xmm0, 1184(%rsp)                             #536.16
                                # LOE rbx rbp r12 r13 r14 r15
..B6.2:                         # Preds ..B6.1
                                # Execution count [1.00e+00]
        movups    1360(%rsp), %xmm0                             #537.21
        movups    1376(%rsp), %xmm1                             #537.21
        movups    1392(%rsp), %xmm2                             #537.21
        movups    1408(%rsp), %xmm3                             #537.21
        movups    1424(%rsp), %xmm4                             #537.21
        movups    1440(%rsp), %xmm5                             #537.21
        movups    1456(%rsp), %xmm6                             #537.27
        movups    1472(%rsp), %xmm7                             #537.27
        movups    1488(%rsp), %xmm8                             #537.27
        movups    1504(%rsp), %xmm9                             #537.27
        movups    1520(%rsp), %xmm10                            #537.27
        movups    1536(%rsp), %xmm11                            #537.27
        movups    1552(%rsp), %xmm12                            #537.33
        movups    1568(%rsp), %xmm13                            #537.33
        movups    1584(%rsp), %xmm14                            #537.33
        movups    1600(%rsp), %xmm15                            #537.33
        movups    %xmm0, (%rsp)                                 #537.21
        movups    %xmm1, 16(%rsp)                               #537.21
        movups    %xmm2, 32(%rsp)                               #537.21
        movups    %xmm3, 48(%rsp)                               #537.21
        movups    1616(%rsp), %xmm0                             #537.33
        movups    1632(%rsp), %xmm1                             #537.33
        movups    1648(%rsp), %xmm2                             #537.39
        movups    1664(%rsp), %xmm3                             #537.39
        movups    %xmm4, 64(%rsp)                               #537.21
        movups    %xmm5, 80(%rsp)                               #537.21
        movups    %xmm6, 96(%rsp)                               #537.27
        movups    %xmm7, 112(%rsp)                              #537.27
        movups    %xmm8, 128(%rsp)                              #537.27
        movups    %xmm9, 144(%rsp)                              #537.27
        movups    %xmm10, 160(%rsp)                             #537.27
        movups    %xmm11, 176(%rsp)                             #537.27
        movups    %xmm12, 192(%rsp)                             #537.33
        movups    %xmm13, 208(%rsp)                             #537.33
        movups    %xmm14, 224(%rsp)                             #537.33
        movups    %xmm15, 240(%rsp)                             #537.33
        movups    %xmm0, 256(%rsp)                              #537.33
        movups    %xmm1, 272(%rsp)                              #537.33
        movups    %xmm2, 288(%rsp)                              #537.39
        movups    %xmm3, 304(%rsp)                              #537.39
        movups    1680(%rsp), %xmm4                             #537.39
        movups    1696(%rsp), %xmm5                             #537.39
        movups    1712(%rsp), %xmm6                             #537.39
        movups    1728(%rsp), %xmm7                             #537.39
        movups    1744(%rsp), %xmm8                             #537.45
        movups    1760(%rsp), %xmm9                             #537.45
        movups    1776(%rsp), %xmm10                            #537.45
        movups    1792(%rsp), %xmm11                            #537.45
        movups    1808(%rsp), %xmm12                            #537.45
        movups    1824(%rsp), %xmm13                            #537.45
        movups    1840(%rsp), %xmm14                            #537.51
        movups    1856(%rsp), %xmm15                            #537.51
        movups    1872(%rsp), %xmm0                             #537.51
        movups    1888(%rsp), %xmm1                             #537.51
        movups    1904(%rsp), %xmm2                             #537.51
        movups    1920(%rsp), %xmm3                             #537.51
        movups    %xmm4, 320(%rsp)                              #537.39
        movups    %xmm5, 336(%rsp)                              #537.39
        movups    %xmm6, 352(%rsp)                              #537.39
        movups    %xmm7, 368(%rsp)                              #537.39
        movups    %xmm8, 384(%rsp)                              #537.45
        movups    %xmm9, 400(%rsp)                              #537.45
        movups    %xmm10, 416(%rsp)                             #537.45
        movups    %xmm11, 432(%rsp)                             #537.45
        movups    %xmm12, 448(%rsp)                             #537.45
        movups    %xmm13, 464(%rsp)                             #537.45
        movups    %xmm14, 480(%rsp)                             #537.51
        movups    %xmm15, 496(%rsp)                             #537.51
        movups    %xmm0, 512(%rsp)                              #537.51
        movups    %xmm1, 528(%rsp)                              #537.51
        movups    %xmm2, 544(%rsp)                              #537.51
        movups    %xmm3, 560(%rsp)                              #537.51
        cmpl      $-1, 1388(%rsp)                               #538.18
        jge       ..B6.5        # Prob 78%                      #538.18
                                # LOE rbx rbp r12 r13 r14 r15
..B6.3:                         # Preds ..B6.2
                                # Execution count [2.20e-01]
        movl      28(%rsp), %edi                                #539.22
        negl      %edi                                          #539.22
        movslq    %edi, %rdi                                    #539.22
        shlq      $3, %rdi                                      #539.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.466:
#       op_malloc(size_t)
        call      op_malloc                                     #539.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.467:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B6.4:                         # Preds ..B6.3
                                # Execution count [2.20e-01]
        movq      %rax, 1152(%rsp)                              #539.5
                                # LOE rbx rbp r12 r13 r14 r15
..B6.5:                         # Preds ..B6.2 ..B6.4
                                # Execution count [1.00e+00]
        cmpl      $-1, 1484(%rsp)                               #541.18
        jge       ..B6.8        # Prob 78%                      #541.18
                                # LOE rbx rbp r12 r13 r14 r15
..B6.6:                         # Preds ..B6.5
                                # Execution count [2.20e-01]
        movl      124(%rsp), %edi                               #542.22
        negl      %edi                                          #542.22
        movslq    %edi, %rdi                                    #542.22
        shlq      $3, %rdi                                      #542.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.468:
#       op_malloc(size_t)
        call      op_malloc                                     #542.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.469:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B6.7:                         # Preds ..B6.6
                                # Execution count [2.20e-01]
        movq      %rax, 1160(%rsp)                              #542.5
                                # LOE rbx rbp r12 r13 r14 r15
..B6.8:                         # Preds ..B6.5 ..B6.7
                                # Execution count [1.00e+00]
        cmpl      $-1, 1580(%rsp)                               #544.18
        jge       ..B6.11       # Prob 78%                      #544.18
                                # LOE rbx rbp r12 r13 r14 r15
..B6.9:                         # Preds ..B6.8
                                # Execution count [2.20e-01]
        movl      220(%rsp), %edi                               #545.22
        negl      %edi                                          #545.22
        movslq    %edi, %rdi                                    #545.22
        shlq      $3, %rdi                                      #545.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.470:
#       op_malloc(size_t)
        call      op_malloc                                     #545.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.471:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B6.10:                        # Preds ..B6.9
                                # Execution count [2.20e-01]
        movq      %rax, 1168(%rsp)                              #545.5
                                # LOE rbx rbp r12 r13 r14 r15
..B6.11:                        # Preds ..B6.8 ..B6.10
                                # Execution count [1.00e+00]
        cmpl      $-1, 1676(%rsp)                               #547.18
        jge       ..B6.14       # Prob 78%                      #547.18
                                # LOE rbx rbp r12 r13 r14 r15
..B6.12:                        # Preds ..B6.11
                                # Execution count [2.20e-01]
        movl      316(%rsp), %edi                               #548.22
        negl      %edi                                          #548.22
        movslq    %edi, %rdi                                    #548.22
        shlq      $3, %rdi                                      #548.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.472:
#       op_malloc(size_t)
        call      op_malloc                                     #548.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.473:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B6.13:                        # Preds ..B6.12
                                # Execution count [2.20e-01]
        movq      %rax, 1176(%rsp)                              #548.5
                                # LOE rbx rbp r12 r13 r14 r15
..B6.14:                        # Preds ..B6.11 ..B6.13
                                # Execution count [1.00e+00]
        cmpl      $-1, 1772(%rsp)                               #550.18
        jge       ..B6.17       # Prob 78%                      #550.18
                                # LOE rbx rbp r12 r13 r14 r15
..B6.15:                        # Preds ..B6.14
                                # Execution count [2.20e-01]
        movl      412(%rsp), %edi                               #551.22
        negl      %edi                                          #551.22
        movslq    %edi, %rdi                                    #551.22
        shlq      $3, %rdi                                      #551.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.474:
#       op_malloc(size_t)
        call      op_malloc                                     #551.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.475:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B6.16:                        # Preds ..B6.15
                                # Execution count [2.20e-01]
        movq      %rax, 1184(%rsp)                              #551.5
                                # LOE rbx rbp r12 r13 r14 r15
..B6.17:                        # Preds ..B6.14 ..B6.16
                                # Execution count [1.00e+00]
        cmpl      $-1, 1868(%rsp)                               #553.18
        jge       ..B6.20       # Prob 78%                      #553.18
                                # LOE rbx rbp r12 r13 r14 r15
..B6.18:                        # Preds ..B6.17
                                # Execution count [2.20e-01]
        movl      508(%rsp), %edi                               #554.22
        negl      %edi                                          #554.22
        movslq    %edi, %rdi                                    #554.22
        shlq      $3, %rdi                                      #554.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.476:
#       op_malloc(size_t)
        call      op_malloc                                     #554.22
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.477:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B6.19:                        # Preds ..B6.18
                                # Execution count [2.20e-01]
        movq      %rax, 1192(%rsp)                              #554.5
                                # LOE rbx rbp r12 r13 r14 r15
..B6.20:                        # Preds ..B6.17 ..B6.19
                                # Execution count [1.00e+00]
        cmpl      $0, 84(%rsp)                                  #559.28
        jne       ..B6.24       # Prob 50%                      #559.28
                                # LOE rbx rbp r12 r13 r14 r15
..B6.21:                        # Preds ..B6.20
                                # Execution count [5.00e-01]
        movl      32(%rsp), %edi                                #559.42
        cmpl      blank_args_size(%rip), %edi                   #559.57
        jle       ..B6.24       # Prob 68%                      #559.57
                                # LOE rbx rbp r12 r13 r14 r15 edi
..B6.22:                        # Preds ..B6.21
                                # Execution count [1.58e-01]
        movl      %edi, blank_args_size(%rip)                   #560.7
        movslq    %edi, %rdi                                    #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.478:
#       op_malloc(size_t)
        call      op_malloc                                     #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.479:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B6.23:                        # Preds ..B6.22
                                # Execution count [1.58e-01]
        movq      %rax, blank_args(%rip)                        #561.7
                                # LOE rbx rbp r12 r13 r14 r15
..B6.24:                        # Preds ..B6.21 ..B6.20 ..B6.23
                                # Execution count [1.00e+00]
        cmpl      $0, 180(%rsp)                                 #559.28
        jne       ..B6.28       # Prob 50%                      #559.28
                                # LOE rbx rbp r12 r13 r14 r15
..B6.25:                        # Preds ..B6.24
                                # Execution count [5.00e-01]
        movl      128(%rsp), %edi                               #559.42
        cmpl      blank_args_size(%rip), %edi                   #559.57
        jle       ..B6.28       # Prob 68%                      #559.57
                                # LOE rbx rbp r12 r13 r14 r15 edi
..B6.26:                        # Preds ..B6.25
                                # Execution count [1.58e-01]
        movl      %edi, blank_args_size(%rip)                   #560.7
        movslq    %edi, %rdi                                    #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.480:
#       op_malloc(size_t)
        call      op_malloc                                     #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.481:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B6.27:                        # Preds ..B6.26
                                # Execution count [1.58e-01]
        movq      %rax, blank_args(%rip)                        #561.7
                                # LOE rbx rbp r12 r13 r14 r15
..B6.28:                        # Preds ..B6.24 ..B6.25 ..B6.27
                                # Execution count [1.00e+00]
        cmpl      $0, 276(%rsp)                                 #559.28
        jne       ..B6.32       # Prob 50%                      #559.28
                                # LOE rbx rbp r12 r13 r14 r15
..B6.29:                        # Preds ..B6.28
                                # Execution count [5.00e-01]
        movl      224(%rsp), %edi                               #559.42
        cmpl      blank_args_size(%rip), %edi                   #559.57
        jle       ..B6.32       # Prob 68%                      #559.57
                                # LOE rbx rbp r12 r13 r14 r15 edi
..B6.30:                        # Preds ..B6.29
                                # Execution count [1.58e-01]
        movl      %edi, blank_args_size(%rip)                   #560.7
        movslq    %edi, %rdi                                    #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.482:
#       op_malloc(size_t)
        call      op_malloc                                     #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.483:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B6.31:                        # Preds ..B6.30
                                # Execution count [1.58e-01]
        movq      %rax, blank_args(%rip)                        #561.7
                                # LOE rbx rbp r12 r13 r14 r15
..B6.32:                        # Preds ..B6.28 ..B6.29 ..B6.31
                                # Execution count [1.00e+00]
        cmpl      $0, 372(%rsp)                                 #559.28
        jne       ..B6.36       # Prob 50%                      #559.28
                                # LOE rbx rbp r12 r13 r14 r15
..B6.33:                        # Preds ..B6.32
                                # Execution count [5.00e-01]
        movl      320(%rsp), %edi                               #559.42
        cmpl      blank_args_size(%rip), %edi                   #559.57
        jle       ..B6.36       # Prob 68%                      #559.57
                                # LOE rbx rbp r12 r13 r14 r15 edi
..B6.34:                        # Preds ..B6.33
                                # Execution count [1.58e-01]
        movl      %edi, blank_args_size(%rip)                   #560.7
        movslq    %edi, %rdi                                    #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.484:
#       op_malloc(size_t)
        call      op_malloc                                     #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.485:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B6.35:                        # Preds ..B6.34
                                # Execution count [1.58e-01]
        movq      %rax, blank_args(%rip)                        #561.7
                                # LOE rbx rbp r12 r13 r14 r15
..B6.36:                        # Preds ..B6.32 ..B6.33 ..B6.35
                                # Execution count [1.00e+00]
        cmpl      $0, 468(%rsp)                                 #559.28
        jne       ..B6.40       # Prob 50%                      #559.28
                                # LOE rbx rbp r12 r13 r14 r15
..B6.37:                        # Preds ..B6.36
                                # Execution count [5.00e-01]
        movl      416(%rsp), %edi                               #559.42
        cmpl      blank_args_size(%rip), %edi                   #559.57
        jle       ..B6.40       # Prob 68%                      #559.57
                                # LOE rbx rbp r12 r13 r14 r15 edi
..B6.38:                        # Preds ..B6.37
                                # Execution count [1.58e-01]
        movl      %edi, blank_args_size(%rip)                   #560.7
        movslq    %edi, %rdi                                    #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.486:
#       op_malloc(size_t)
        call      op_malloc                                     #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.487:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B6.39:                        # Preds ..B6.38
                                # Execution count [1.58e-01]
        movq      %rax, blank_args(%rip)                        #561.7
                                # LOE rbx rbp r12 r13 r14 r15
..B6.40:                        # Preds ..B6.36 ..B6.37 ..B6.39
                                # Execution count [1.00e+00]
        cmpl      $0, 564(%rsp)                                 #559.28
        jne       ..B6.44       # Prob 50%                      #559.28
                                # LOE rbx rbp r12 r13 r14 r15
..B6.41:                        # Preds ..B6.40
                                # Execution count [5.00e-01]
        movl      512(%rsp), %edi                               #559.42
        cmpl      blank_args_size(%rip), %edi                   #559.57
        jle       ..B6.44       # Prob 68%                      #559.57
                                # LOE rbx rbp r12 r13 r14 r15 edi
..B6.42:                        # Preds ..B6.41
                                # Execution count [1.58e-01]
        movl      %edi, blank_args_size(%rip)                   #560.7
        movslq    %edi, %rdi                                    #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.488:
#       op_malloc(size_t)
        call      op_malloc                                     #561.28
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.489:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B6.43:                        # Preds ..B6.42
                                # Execution count [1.58e-01]
        movq      %rax, blank_args(%rip)                        #561.7
                                # LOE rbx rbp r12 r13 r14 r15
..B6.44:                        # Preds ..B6.40 ..B6.41 ..B6.43
                                # Execution count [1.00e+00]
        movl      $0, 1304(%rsp)                                #564.13
        cmpl      $0, OP_diags(%rip)                            #565.18
        jle       ..B6.52       # Prob 40%                      #565.18
                                # LOE rbx rbp r12 r13 r14 r15
..B6.45:                        # Preds ..B6.44
                                # Execution count [5.37e-01]
        xorl      %esi, %esi                                    #566.5
        xorl      %eax, %eax                                    #566.5
        movq      %rax, %r14                                    #566.5
        movl      %esi, %r13d                                   #566.5
                                # LOE rbx rbp r12 r14 r15 r13d
..B6.46:                        # Preds ..B6.47 ..B6.45
                                # Execution count [2.98e+00]
        addq      $-96, %rsp                                    #566.5
	.cfi_def_cfa_offset 1456
        movq      %rbx, %rdi                                    #566.5
        movq      %rsp, %r8                                     #566.5
        movl      %r13d, %esi                                   #566.5
        movups    96(%rsp,%r14), %xmm0                          #566.5
        lea       1400(%rsp), %rdx                              #566.5
        movups    112(%rsp,%r14), %xmm1                         #566.5
        movq      %rbp, %rcx                                    #566.5
        movups    128(%rsp,%r14), %xmm2                         #566.5
        movups    144(%rsp,%r14), %xmm3                         #566.5
        movups    160(%rsp,%r14), %xmm4                         #566.5
        movups    176(%rsp,%r14), %xmm5                         #566.5
        movups    %xmm0, (%r8)                                  #566.5
        movups    %xmm1, 16(%r8)                                #566.5
        movups    %xmm2, 32(%r8)                                #566.5
        movups    %xmm3, 48(%r8)                                #566.5
        movups    %xmm4, 64(%r8)                                #566.5
        movups    %xmm5, 80(%r8)                                #566.5
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.491:
#       op_arg_check(op_set, int, op_arg, int *, const char *)
        call      op_arg_check                                  #566.5
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.492:
                                # LOE rbx rbp r12 r14 r15 r13d
..B6.203:                       # Preds ..B6.46
                                # Execution count [2.98e+00]
        addq      $96, %rsp                                     #566.5
	.cfi_def_cfa_offset 1360
                                # LOE rbx rbp r12 r14 r15 r13d
..B6.47:                        # Preds ..B6.203
                                # Execution count [2.98e+00]
        incl      %r13d                                         #566.5
        addq      $96, %r14                                     #566.5
        cmpl      $6, %r13d                                     #566.5
        jl        ..B6.46       # Prob 82%                      #566.5
                                # LOE rbx rbp r12 r14 r15 r13d
..B6.48:                        # Preds ..B6.47
                                # Execution count [5.37e-01]
        cmpl      $2, OP_diags(%rip)                            #568.18
        jle       ..B6.52       # Prob 50%                      #568.18
                                # LOE rbx rbp r12 r13 r14 r15
..B6.49:                        # Preds ..B6.48
                                # Execution count [5.00e-01]
        cmpl      $0, 1304(%rsp)                                #569.18
        jne       ..B6.51       # Prob 50%                      #569.18
                                # LOE rbx rbp r12 r13 r14 r15
..B6.50:                        # Preds ..B6.49
                                # Execution count [2.50e-01]
        movl      $.L_2__STRING.3, %edi                         #570.7
        movq      %rbp, %rsi                                    #570.7
        xorl      %eax, %eax                                    #570.7
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.494:
#       printf(const char *, ...)
        call      printf                                        #570.7
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.495:
        jmp       ..B6.52       # Prob 100%                     #570.7
                                # LOE rbx rbp r12 r13 r14 r15
..B6.51:                        # Preds ..B6.49
                                # Execution count [2.50e-01]
        movl      $.L_2__STRING.4, %edi                         #572.7
        movq      %rbp, %rsi                                    #572.7
        xorl      %eax, %eax                                    #572.7
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.496:
#       printf(const char *, ...)
        call      printf                                        #572.7
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.497:
                                # LOE rbx rbp r12 r13 r14 r15
..B6.52:                        # Preds ..B6.44 ..B6.50 ..B6.51 ..B6.48
                                # Execution count [1.00e+00]
        lea       1216(%rsp), %rdi                              #576.3
        lea       1224(%rsp), %rsi                              #576.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.498:
#       op_timers_core(double *, double *)
        call      op_timers_core                                #576.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.499:
                                # LOE rbx rbp r12 r13 r14 r15
..B6.53:                        # Preds ..B6.52
                                # Execution count [1.00e+00]
        movq      %rbx, %rdi                                    #579.17
        movl      $6, %esi                                      #579.17
        lea       (%rsp), %rdx                                  #579.17
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.500:
#       op_mpi_halo_exchanges(op_set, int, op_arg *)
        call      op_mpi_halo_exchanges                         #579.17
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.501:
                                # LOE rbx rbp r12 r13 r14 r15 eax
..B6.204:                       # Preds ..B6.53
                                # Execution count [1.00e+00]
        movl      %eax, %r9d                                    #579.17
                                # LOE rbx rbp r12 r13 r14 r15 r9d
..B6.54:                        # Preds ..B6.204
                                # Execution count [1.00e+00]
        xorl      %edx, %edx                                    #582.12
        xorl      %eax, %eax                                    #584.14
        testl     %r9d, %r9d                                    #584.23
        jle       ..B6.188      # Prob 10%                      #584.23
                                # LOE rbx rbp r12 r13 r14 r15 eax edx r9d
..B6.55:                        # Preds ..B6.54
                                # Execution count [9.00e-01]
        movl      %r9d, 1280(%rsp)                              #588.7[spill]
        movq      %r15, 1288(%rsp)                              #588.7[spill]
        movq      %rbp, 1208(%rsp)                              #588.7[spill]
        movl      %edx, %ebp                                    #588.7
        movq      %rbx, 1296(%rsp)                              #588.7[spill]
        movl      %eax, %ebx                                    #588.7
        movq      %r12, 1200(%rsp)                              #588.7[spill]
	.cfi_offset 12, -160
                                # LOE ebx ebp
..B6.56:                        # Preds ..B6.167 ..B6.55
                                # Execution count [5.00e+00]
        movq      1296(%rsp), %rcx                              #585.14[spill]
        cmpl      16(%rcx), %ebx                                #585.14
        jne       ..B6.58       # Prob 78%                      #585.14
                                # LOE ebx ebp
..B6.57:                        # Preds ..B6.56
                                # Execution count [1.10e+00]
        movl      $6, %edi                                      #586.7
        lea       (%rsp), %rsi                                  #586.7
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.503:
#       op_mpi_wait_all(int, op_arg *)
        call      op_mpi_wait_all                               #586.7
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.504:
                                # LOE ebx ebp
..B6.58:                        # Preds ..B6.57 ..B6.56
                                # Execution count [5.00e+00]
        movq      1296(%rsp), %rcx                              #588.7[spill]
        movl      $1, %esi                                      #588.7
        cmpl      4(%rcx), %ebx                                 #588.7
        cmove     %esi, %ebp                                    #588.7
        cmpl      $-1, 28(%rsp)                                 #589.23
        jge       ..B6.66       # Prob 50%                      #589.23
                                # LOE ebx ebp
..B6.59:                        # Preds ..B6.58
                                # Execution count [2.50e+00]
        movl      28(%rsp), %r14d                               #590.7
        movl      %r14d, %ecx                                   #590.7
        negl      %ecx                                          #590.7
        movq      16(%rsp), %r9                                 #590.7
        movq      40(%rsp), %r8                                 #590.7
        movl      32(%rsp), %r10d                               #590.7
        movq      1152(%rsp), %rdi                              #590.43
        testl     %ecx, %ecx                                    #590.7
        jle       ..B6.76       # Prob 50%                      #590.7
                                # LOE rdi r8 r9 ecx ebx ebp r10d r14d
..B6.60:                        # Preds ..B6.59
                                # Execution count [2.50e+00]
        movl      %ecx, %r11d                                   #590.7
        movl      $1, %r13d                                     #590.7
        shrl      $31, %r11d                                    #590.7
        xorl      %esi, %esi                                    #590.7
        subl      %r14d, %r11d                                  #590.7
        sarl      $1, %r11d                                     #590.7
        movl      24(%r9), %r12d                                #590.7
        testl     %r11d, %r11d                                  #590.7
        jbe       ..B6.64       # Prob 10%                      #590.7
                                # LOE rdi r8 r9 ecx ebx ebp esi r10d r11d r12d r13d
..B6.61:                        # Preds ..B6.60
                                # Execution count [2.25e+00]
        movl      %ebx, %r13d                                   #590.7
        imull     %r12d, %r13d                                  #590.7
        .align    16,0x90
                                # LOE rdi r8 r9 ecx ebx ebp esi r10d r11d r12d r13d
..B6.62:                        # Preds ..B6.62 ..B6.61
                                # Execution count [6.25e+00]
        movq      32(%r9), %r14                                 #590.7
        lea       (%r13,%rsi,2), %eax                           #590.7
        movslq    %eax, %rax                                    #590.7
        lea       (%rsi,%rsi), %edx                             #590.7
        movslq    %edx, %rdx                                    #590.7
        incl      %esi                                          #590.7
        movl      (%r14,%rax,4), %r15d                          #590.7
        imull     %r10d, %r15d                                  #590.7
        movslq    %r15d, %r15                                   #590.7
        addq      %r8, %r15                                     #590.7
        movq      %r15, (%rdi,%rdx,8)                           #590.7
        movq      32(%r9), %r14                                 #590.7
        movl      4(%r14,%rax,4), %r14d                         #590.7
        imull     %r10d, %r14d                                  #590.7
        movslq    %r14d, %r14                                   #590.7
        addq      %r8, %r14                                     #590.7
        movq      %r14, 8(%rdi,%rdx,8)                          #590.7
        cmpl      %r11d, %esi                                   #590.7
        jb        ..B6.62       # Prob 64%                      #590.7
                                # LOE rdi r8 r9 ecx ebx ebp esi r10d r11d r12d r13d
..B6.63:                        # Preds ..B6.62
                                # Execution count [2.25e+00]
        lea       1(%rsi,%rsi), %r13d                           #590.7
                                # LOE rdi r8 r9 ecx ebx ebp r10d r12d r13d
..B6.64:                        # Preds ..B6.63 ..B6.60
                                # Execution count [2.50e+00]
        lea       -1(%r13), %esi                                #590.7
        cmpl      %ecx, %esi                                    #590.7
        jae       ..B6.76       # Prob 10%                      #590.7
                                # LOE rdi r8 r9 ebx ebp r10d r12d r13d
..B6.65:                        # Preds ..B6.64
                                # Execution count [2.25e+00]
        imull     %ebx, %r12d                                   #590.7
        movslq    %r13d, %r13                                   #590.7
        movslq    %r12d, %r12                                   #590.7
        addq      %r13, %r12                                    #590.7
        movq      32(%r9), %r9                                  #590.7
        movl      -4(%r9,%r12,4), %ecx                          #590.7
        imull     %ecx, %r10d                                   #590.7
        movslq    %r10d, %r10                                   #590.7
        addq      %r10, %r8                                     #590.7
        movq      %r8, -8(%rdi,%r13,8)                          #590.7
        jmp       ..B6.76       # Prob 100%                     #590.7
                                # LOE rdi ebx ebp
..B6.66:                        # Preds ..B6.58
                                # Execution count [2.50e+00]
        movups    (%rsp), %xmm0                                 #592.7
        movups    16(%rsp), %xmm1                               #592.7
        movups    32(%rsp), %xmm2                               #592.7
        movups    48(%rsp), %xmm3                               #592.7
        movups    64(%rsp), %xmm4                               #592.7
        movups    80(%rsp), %xmm5                               #592.7
        movq      40(%rsp), %rdi                                #592.7
        movups    %xmm0, 576(%rsp)                              #592.7
        movups    %xmm1, 592(%rsp)                              #592.7
        movups    %xmm2, 608(%rsp)                              #592.7
        movups    %xmm3, 624(%rsp)                              #592.7
        movups    %xmm4, 640(%rsp)                              #592.7
        movups    %xmm5, 656(%rsp)                              #592.7
        cmpl      $0, 84(%rsp)                                  #592.7
        jne       ..B6.72       # Prob 50%                      #592.7
                                # LOE rdi ebx ebp
..B6.67:                        # Preds ..B6.66
                                # Execution count [1.25e+00]
        testl     %ebp, %ebp                                    #592.7
        je        ..B6.70       # Prob 50%                      #592.7
                                # LOE rdi ebx ebp
..B6.68:                        # Preds ..B6.67
                                # Execution count [6.25e-01]
        cmpl      $0, 80(%rsp)                                  #592.7
        je        ..B6.70       # Prob 50%                      #592.7
                                # LOE rdi ebx ebp
..B6.69:                        # Preds ..B6.68
                                # Execution count [3.12e-01]
        movq      blank_args(%rip), %rdi                        #592.7
        movq      %rdi, 1152(%rsp)                              #592.31
        jmp       ..B6.76       # Prob 100%                     #592.31
                                # LOE rdi ebx ebp
..B6.70:                        # Preds ..B6.67 ..B6.68
                                # Execution count [3.12e-01]
        movq      %rdi, 1152(%rsp)                              #592.31
        jmp       ..B6.76       # Prob 100%                     #592.31
                                # LOE rdi ebx ebp
..B6.72:                        # Preds ..B6.66
                                # Execution count [1.25e+00]
        movq      592(%rsp), %rsi                               #592.7
        testq     %rsi, %rsi                                    #592.7
        je        ..B6.74       # Prob 12%                      #592.7
                                # LOE rsi rdi ebx ebp
..B6.73:                        # Preds ..B6.72
                                # Execution count [1.10e+00]
        cmpl      $0, 668(%rsp)                                 #592.7
        jne       ..B6.75       # Prob 50%                      #592.7
                                # LOE rsi rdi ebx ebp
..B6.74:                        # Preds ..B6.72 ..B6.73
                                # Execution count [7.00e-01]
        movslq    %ebx, %rbx                                    #592.7
        movslq    608(%rsp), %rcx                               #592.7
        imulq     %rbx, %rcx                                    #592.7
        addq      %rcx, %rdi                                    #592.7
        movq      %rdi, 1152(%rsp)                              #592.31
        jmp       ..B6.76       # Prob 100%                     #592.31
                                # LOE rdi ebx ebp
..B6.75:                        # Preds ..B6.73
                                # Execution count [5.50e-01]
        movl      %ebx, %ecx                                    #592.7
        imull     24(%rsi), %ecx                                #592.7
        addl      604(%rsp), %ecx                               #592.7
        movslq    %ecx, %rcx                                    #592.7
        movq      32(%rsi), %rsi                                #592.7
        movl      608(%rsp), %r8d                               #592.7
        imull     (%rsi,%rcx,4), %r8d                           #592.7
        movslq    %r8d, %r8                                     #592.7
        addq      %r8, %rdi                                     #592.7
        movq      %rdi, 1152(%rsp)                              #592.31
                                # LOE rdi ebx ebp
..B6.76:                        # Preds ..B6.65 ..B6.64 ..B6.59 ..B6.69 ..B6.70
                                #       ..B6.74 ..B6.75
                                # Execution count [5.00e+00]
        cmpl      $-1, 124(%rsp)                                #593.23
        jge       ..B6.84       # Prob 50%                      #593.23
                                # LOE rdi ebx ebp
..B6.77:                        # Preds ..B6.76
                                # Execution count [2.50e+00]
        movl      124(%rsp), %r15d                              #594.7
        movl      %r15d, %r8d                                   #594.7
        negl      %r8d                                          #594.7
        movq      112(%rsp), %r9                                #594.7
        movq      136(%rsp), %r14                               #594.7
        movl      128(%rsp), %ecx                               #594.7
        movq      1160(%rsp), %rsi                              #594.43
        testl     %r8d, %r8d                                    #594.7
        jle       ..B6.94       # Prob 50%                      #594.7
                                # LOE rsi rdi r9 r14 ecx ebx ebp r8d r15d
..B6.78:                        # Preds ..B6.77
                                # Execution count [2.50e+00]
        movl      %r8d, %r12d                                   #594.7
        movl      $1, %r10d                                     #594.7
        shrl      $31, %r12d                                    #594.7
        xorl      %r11d, %r11d                                  #594.7
        subl      %r15d, %r12d                                  #594.7
        sarl      $1, %r12d                                     #594.7
        movl      24(%r9), %r13d                                #594.7
        testl     %r12d, %r12d                                  #594.7
        jbe       ..B6.82       # Prob 10%                      #594.7
                                # LOE rsi rdi r9 r14 ecx ebx ebp r8d r10d r11d r12d r13d
..B6.79:                        # Preds ..B6.78
                                # Execution count [2.25e+00]
        movl      %ebx, %r10d                                   #594.7
        imull     %r13d, %r10d                                  #594.7
        .align    16,0x90
                                # LOE rsi rdi r9 r14 ecx ebx ebp r8d r10d r11d r12d r13d
..B6.80:                        # Preds ..B6.80 ..B6.79
                                # Execution count [6.25e+00]
        movq      32(%r9), %r15                                 #594.7
        lea       (%r10,%r11,2), %eax                           #594.7
        movslq    %eax, %rax                                    #594.7
        lea       (%r11,%r11), %edx                             #594.7
        movslq    %edx, %rdx                                    #594.7
        incl      %r11d                                         #594.7
        movl      (%r15,%rax,4), %r15d                          #594.7
        imull     %ecx, %r15d                                   #594.7
        movslq    %r15d, %r15                                   #594.7
        addq      %r14, %r15                                    #594.7
        movq      %r15, (%rsi,%rdx,8)                           #594.7
        movq      32(%r9), %r15                                 #594.7
        movl      4(%r15,%rax,4), %r15d                         #594.7
        imull     %ecx, %r15d                                   #594.7
        movslq    %r15d, %r15                                   #594.7
        addq      %r14, %r15                                    #594.7
        movq      %r15, 8(%rsi,%rdx,8)                          #594.7
        cmpl      %r12d, %r11d                                  #594.7
        jb        ..B6.80       # Prob 64%                      #594.7
                                # LOE rsi rdi r9 r14 ecx ebx ebp r8d r10d r11d r12d r13d
..B6.81:                        # Preds ..B6.80
                                # Execution count [2.25e+00]
        lea       1(%r11,%r11), %r10d                           #594.7
                                # LOE rsi rdi r9 r14 ecx ebx ebp r8d r10d r13d
..B6.82:                        # Preds ..B6.81 ..B6.78
                                # Execution count [2.50e+00]
        lea       -1(%r10), %r11d                               #594.7
        cmpl      %r8d, %r11d                                   #594.7
        jae       ..B6.94       # Prob 10%                      #594.7
                                # LOE rsi rdi r9 r14 ecx ebx ebp r10d r13d
..B6.83:                        # Preds ..B6.82
                                # Execution count [2.25e+00]
        imull     %ebx, %r13d                                   #594.7
        movslq    %r10d, %r10                                   #594.7
        movslq    %r13d, %r13                                   #594.7
        addq      %r10, %r13                                    #594.7
        movq      32(%r9), %r9                                  #594.7
        movl      -4(%r9,%r13,4), %r8d                          #594.7
        imull     %r8d, %ecx                                    #594.7
        movslq    %ecx, %rcx                                    #594.7
        addq      %rcx, %r14                                    #594.7
        movq      %r14, -8(%rsi,%r10,8)                         #594.7
        jmp       ..B6.94       # Prob 100%                     #594.7
                                # LOE rsi rdi ebx ebp
..B6.84:                        # Preds ..B6.76
                                # Execution count [2.50e+00]
        movups    96(%rsp), %xmm0                               #596.7
        movups    112(%rsp), %xmm1                              #596.7
        movups    128(%rsp), %xmm2                              #596.7
        movups    144(%rsp), %xmm3                              #596.7
        movups    160(%rsp), %xmm4                              #596.7
        movups    176(%rsp), %xmm5                              #596.7
        movq      136(%rsp), %rsi                               #596.7
        movups    %xmm0, 672(%rsp)                              #596.7
        movups    %xmm1, 688(%rsp)                              #596.7
        movups    %xmm2, 704(%rsp)                              #596.7
        movups    %xmm3, 720(%rsp)                              #596.7
        movups    %xmm4, 736(%rsp)                              #596.7
        movups    %xmm5, 752(%rsp)                              #596.7
        cmpl      $0, 180(%rsp)                                 #596.7
        jne       ..B6.90       # Prob 50%                      #596.7
                                # LOE rsi rdi ebx ebp
..B6.85:                        # Preds ..B6.84
                                # Execution count [1.25e+00]
        testl     %ebp, %ebp                                    #596.7
        je        ..B6.88       # Prob 50%                      #596.7
                                # LOE rsi rdi ebx ebp
..B6.86:                        # Preds ..B6.85
                                # Execution count [6.25e-01]
        cmpl      $0, 176(%rsp)                                 #596.7
        je        ..B6.88       # Prob 50%                      #596.7
                                # LOE rsi rdi ebx ebp
..B6.87:                        # Preds ..B6.86
                                # Execution count [3.12e-01]
        movq      blank_args(%rip), %rsi                        #596.7
        movq      %rsi, 1160(%rsp)                              #596.7
        jmp       ..B6.94       # Prob 100%                     #596.7
                                # LOE rsi rdi ebx ebp
..B6.88:                        # Preds ..B6.85 ..B6.86
                                # Execution count [3.12e-01]
        movq      %rsi, 1160(%rsp)                              #596.7
        jmp       ..B6.94       # Prob 100%                     #596.7
                                # LOE rsi rdi ebx ebp
..B6.90:                        # Preds ..B6.84
                                # Execution count [1.25e+00]
        movq      688(%rsp), %r8                                #596.7
        testq     %r8, %r8                                      #596.7
        je        ..B6.92       # Prob 12%                      #596.7
                                # LOE rsi rdi r8 ebx ebp
..B6.91:                        # Preds ..B6.90
                                # Execution count [1.10e+00]
        cmpl      $0, 764(%rsp)                                 #596.7
        jne       ..B6.93       # Prob 50%                      #596.7
                                # LOE rsi rdi r8 ebx ebp
..B6.92:                        # Preds ..B6.91 ..B6.90
                                # Execution count [7.00e-01]
        movslq    %ebx, %rbx                                    #596.7
        movslq    704(%rsp), %rcx                               #596.7
        imulq     %rbx, %rcx                                    #596.7
        addq      %rcx, %rsi                                    #596.7
        movq      %rsi, 1160(%rsp)                              #596.7
        jmp       ..B6.94       # Prob 100%                     #596.7
                                # LOE rsi rdi ebx ebp
..B6.93:                        # Preds ..B6.91
                                # Execution count [5.50e-01]
        movl      %ebx, %ecx                                    #596.7
        imull     24(%r8), %ecx                                 #596.7
        addl      700(%rsp), %ecx                               #596.7
        movslq    %ecx, %rcx                                    #596.7
        movq      32(%r8), %r8                                  #596.7
        movl      704(%rsp), %r9d                               #596.7
        imull     (%r8,%rcx,4), %r9d                            #596.7
        movslq    %r9d, %r9                                     #596.7
        addq      %r9, %rsi                                     #596.7
        movq      %rsi, 1160(%rsp)                              #596.7
                                # LOE rsi rdi ebx ebp
..B6.94:                        # Preds ..B6.83 ..B6.82 ..B6.77 ..B6.87 ..B6.88
                                #       ..B6.92 ..B6.93
                                # Execution count [5.00e+00]
        cmpl      $-1, 220(%rsp)                                #597.23
        jge       ..B6.102      # Prob 50%                      #597.23
                                # LOE rsi rdi ebx ebp
..B6.95:                        # Preds ..B6.94
                                # Execution count [2.50e+00]
        movl      220(%rsp), %r10d                              #598.7
        movl      %r10d, %r9d                                   #598.7
        negl      %r9d                                          #598.7
        movq      208(%rsp), %r15                               #598.7
        movq      232(%rsp), %r11                               #598.7
        movl      224(%rsp), %r8d                               #598.7
        movq      1168(%rsp), %rdx                              #598.43
        testl     %r9d, %r9d                                    #598.7
        jle       ..B6.112      # Prob 50%                      #598.7
                                # LOE rdx rsi rdi r11 r15 ebx ebp r8d r9d r10d
..B6.96:                        # Preds ..B6.95
                                # Execution count [2.50e+00]
        movl      %r9d, %r13d                                   #598.7
        movl      $1, %ecx                                      #598.7
        shrl      $31, %r13d                                    #598.7
        xorl      %r12d, %r12d                                  #598.7
        subl      %r10d, %r13d                                  #598.7
        sarl      $1, %r13d                                     #598.7
        movl      24(%r15), %r14d                               #598.7
        testl     %r13d, %r13d                                  #598.7
        jbe       ..B6.100      # Prob 10%                      #598.7
                                # LOE rdx rsi rdi r11 r15 ecx ebx ebp r8d r9d r12d r13d r14d
..B6.97:                        # Preds ..B6.96
                                # Execution count [2.25e+00]
        movl      %ebx, %r10d                                   #598.7
        imull     %r14d, %r10d                                  #598.7
        movl      %ebp, 1232(%rsp)                              #598.7[spill]
        .align    16,0x90
                                # LOE rdx rsi rdi r11 r15 ebx r8d r9d r10d r12d r13d r14d
..B6.98:                        # Preds ..B6.98 ..B6.97
                                # Execution count [6.25e+00]
        movq      32(%r15), %rbp                                #598.7
        lea       (%r10,%r12,2), %eax                           #598.7
        movslq    %eax, %rax                                    #598.7
        lea       (%r12,%r12), %ecx                             #598.7
        movslq    %ecx, %rcx                                    #598.7
        incl      %r12d                                         #598.7
        movl      (%rbp,%rax,4), %ebp                           #598.7
        imull     %r8d, %ebp                                    #598.7
        movslq    %ebp, %rbp                                    #598.7
        addq      %r11, %rbp                                    #598.7
        movq      %rbp, (%rdx,%rcx,8)                           #598.7
        movq      32(%r15), %rbp                                #598.7
        movl      4(%rbp,%rax,4), %ebp                          #598.7
        imull     %r8d, %ebp                                    #598.7
        movslq    %ebp, %rbp                                    #598.7
        addq      %r11, %rbp                                    #598.7
        movq      %rbp, 8(%rdx,%rcx,8)                          #598.7
        cmpl      %r13d, %r12d                                  #598.7
        jb        ..B6.98       # Prob 64%                      #598.7
                                # LOE rdx rsi rdi r11 r15 ebx r8d r9d r10d r12d r13d r14d
..B6.99:                        # Preds ..B6.98
                                # Execution count [2.25e+00]
        movl      1232(%rsp), %ebp                              #[spill]
        lea       1(%r12,%r12), %ecx                            #598.7
                                # LOE rdx rsi rdi r11 r15 ecx ebx ebp r8d r9d r14d
..B6.100:                       # Preds ..B6.99 ..B6.96
                                # Execution count [2.50e+00]
        lea       -1(%rcx), %r10d                               #598.7
        cmpl      %r9d, %r10d                                   #598.7
        jae       ..B6.112      # Prob 10%                      #598.7
                                # LOE rdx rsi rdi r11 r15 ecx ebx ebp r8d r14d
..B6.101:                       # Preds ..B6.100
                                # Execution count [2.25e+00]
        imull     %ebx, %r14d                                   #598.7
        movslq    %ecx, %rcx                                    #598.7
        movslq    %r14d, %r14                                   #598.7
        addq      %rcx, %r14                                    #598.7
        movq      32(%r15), %r15                                #598.7
        movl      -4(%r15,%r14,4), %r9d                         #598.7
        imull     %r9d, %r8d                                    #598.7
        movslq    %r8d, %r8                                     #598.7
        addq      %r8, %r11                                     #598.7
        movq      %r11, -8(%rdx,%rcx,8)                         #598.7
        jmp       ..B6.112      # Prob 100%                     #598.7
                                # LOE rdx rsi rdi ebx ebp
..B6.102:                       # Preds ..B6.94
                                # Execution count [2.50e+00]
        movups    192(%rsp), %xmm0                              #600.7
        movups    208(%rsp), %xmm1                              #600.7
        movups    224(%rsp), %xmm2                              #600.7
        movups    240(%rsp), %xmm3                              #600.7
        movups    256(%rsp), %xmm4                              #600.7
        movups    272(%rsp), %xmm5                              #600.7
        movq      232(%rsp), %rdx                               #600.7
        movups    %xmm0, 768(%rsp)                              #600.7
        movups    %xmm1, 784(%rsp)                              #600.7
        movups    %xmm2, 800(%rsp)                              #600.7
        movups    %xmm3, 816(%rsp)                              #600.7
        movups    %xmm4, 832(%rsp)                              #600.7
        movups    %xmm5, 848(%rsp)                              #600.7
        cmpl      $0, 276(%rsp)                                 #600.7
        jne       ..B6.108      # Prob 50%                      #600.7
                                # LOE rdx rsi rdi ebx ebp
..B6.103:                       # Preds ..B6.102
                                # Execution count [1.25e+00]
        testl     %ebp, %ebp                                    #600.7
        je        ..B6.106      # Prob 50%                      #600.7
                                # LOE rdx rsi rdi ebx ebp
..B6.104:                       # Preds ..B6.103
                                # Execution count [6.25e-01]
        cmpl      $0, 272(%rsp)                                 #600.7
        je        ..B6.106      # Prob 50%                      #600.7
                                # LOE rdx rsi rdi ebx ebp
..B6.105:                       # Preds ..B6.104
                                # Execution count [3.12e-01]
        movq      blank_args(%rip), %rdx                        #600.7
        movq      %rdx, 1168(%rsp)                              #600.7
        jmp       ..B6.112      # Prob 100%                     #600.7
                                # LOE rdx rsi rdi ebx ebp
..B6.106:                       # Preds ..B6.103 ..B6.104
                                # Execution count [3.12e-01]
        movq      %rdx, 1168(%rsp)                              #600.7
        jmp       ..B6.112      # Prob 100%                     #600.7
                                # LOE rdx rsi rdi ebx ebp
..B6.108:                       # Preds ..B6.102
                                # Execution count [1.25e+00]
        movq      784(%rsp), %r8                                #600.7
        testq     %r8, %r8                                      #600.7
        je        ..B6.110      # Prob 12%                      #600.7
                                # LOE rdx rsi rdi r8 ebx ebp
..B6.109:                       # Preds ..B6.108
                                # Execution count [1.10e+00]
        cmpl      $0, 860(%rsp)                                 #600.7
        jne       ..B6.111      # Prob 50%                      #600.7
                                # LOE rdx rsi rdi r8 ebx ebp
..B6.110:                       # Preds ..B6.109 ..B6.108
                                # Execution count [7.00e-01]
        movslq    %ebx, %rbx                                    #600.7
        movslq    800(%rsp), %rcx                               #600.7
        imulq     %rbx, %rcx                                    #600.7
        addq      %rcx, %rdx                                    #600.7
        movq      %rdx, 1168(%rsp)                              #600.7
        jmp       ..B6.112      # Prob 100%                     #600.7
                                # LOE rdx rsi rdi ebx ebp
..B6.111:                       # Preds ..B6.109
                                # Execution count [5.50e-01]
        movl      %ebx, %ecx                                    #600.7
        imull     24(%r8), %ecx                                 #600.7
        addl      796(%rsp), %ecx                               #600.7
        movslq    %ecx, %rcx                                    #600.7
        movq      32(%r8), %r8                                  #600.7
        movl      800(%rsp), %r9d                               #600.7
        imull     (%r8,%rcx,4), %r9d                            #600.7
        movslq    %r9d, %r9                                     #600.7
        addq      %r9, %rdx                                     #600.7
        movq      %rdx, 1168(%rsp)                              #600.7
                                # LOE rdx rsi rdi ebx ebp
..B6.112:                       # Preds ..B6.101 ..B6.100 ..B6.95 ..B6.105 ..B6.106
                                #       ..B6.110 ..B6.111
                                # Execution count [5.00e+00]
        cmpl      $-1, 316(%rsp)                                #601.23
        jge       ..B6.120      # Prob 50%                      #601.23
                                # LOE rdx rsi rdi ebx ebp
..B6.113:                       # Preds ..B6.112
                                # Execution count [2.50e+00]
        movl      316(%rsp), %r15d                              #602.7
        movl      %r15d, %r13d                                  #602.7
        negl      %r13d                                         #602.7
        movq      304(%rsp), %r10                               #602.7
        movq      328(%rsp), %r11                               #602.7
        movl      320(%rsp), %r12d                              #602.7
        movq      1176(%rsp), %rcx                              #602.43
        testl     %r13d, %r13d                                  #602.7
        jle       ..B6.130      # Prob 50%                      #602.7
                                # LOE rdx rcx rsi rdi r10 r11 ebx ebp r12d r13d r15d
..B6.114:                       # Preds ..B6.113
                                # Execution count [2.50e+00]
        movl      %r13d, %r8d                                   #602.7
        movl      $1, %eax                                      #602.7
        shrl      $31, %r8d                                     #602.7
        xorl      %r9d, %r9d                                    #602.7
        subl      %r15d, %r8d                                   #602.7
        sarl      $1, %r8d                                      #602.7
        movl      24(%r10), %r14d                               #602.7
        testl     %r8d, %r8d                                    #602.7
        jbe       ..B6.118      # Prob 10%                      #602.7
                                # LOE rdx rcx rsi rdi r10 r11 eax ebx ebp r8d r9d r12d r13d r14d
..B6.115:                       # Preds ..B6.114
                                # Execution count [2.25e+00]
        movl      %ebx, %eax                                    #602.7
        imull     %r14d, %eax                                   #602.7
        movl      %ebx, 1240(%rsp)                              #602.7[spill]
        movl      %ebp, 1232(%rsp)                              #602.7[spill]
        .align    16,0x90
                                # LOE rdx rcx rsi rdi r10 r11 eax r8d r9d r12d r13d r14d
..B6.116:                       # Preds ..B6.116 ..B6.115
                                # Execution count [6.25e+00]
        movq      32(%r10), %r15                                #602.7
        lea       (%rax,%r9,2), %ebx                            #602.7
        movslq    %ebx, %rbx                                    #602.7
        lea       (%r9,%r9), %ebp                               #602.7
        movslq    %ebp, %rbp                                    #602.7
        incl      %r9d                                          #602.7
        movl      (%r15,%rbx,4), %r15d                          #602.7
        imull     %r12d, %r15d                                  #602.7
        movslq    %r15d, %r15                                   #602.7
        addq      %r11, %r15                                    #602.7
        movq      %r15, (%rcx,%rbp,8)                           #602.7
        movq      32(%r10), %r15                                #602.7
        movl      4(%r15,%rbx,4), %ebx                          #602.7
        imull     %r12d, %ebx                                   #602.7
        movslq    %ebx, %rbx                                    #602.7
        addq      %r11, %rbx                                    #602.7
        movq      %rbx, 8(%rcx,%rbp,8)                          #602.7
        cmpl      %r8d, %r9d                                    #602.7
        jb        ..B6.116      # Prob 64%                      #602.7
                                # LOE rdx rcx rsi rdi r10 r11 eax r8d r9d r12d r13d r14d
..B6.117:                       # Preds ..B6.116
                                # Execution count [2.25e+00]
        movl      1240(%rsp), %ebx                              #[spill]
        lea       1(%r9,%r9), %eax                              #602.7
        movl      1232(%rsp), %ebp                              #[spill]
                                # LOE rdx rcx rsi rdi r10 r11 eax ebx ebp r12d r13d r14d
..B6.118:                       # Preds ..B6.117 ..B6.114
                                # Execution count [2.50e+00]
        lea       -1(%rax), %r8d                                #602.7
        cmpl      %r13d, %r8d                                   #602.7
        jae       ..B6.130      # Prob 10%                      #602.7
                                # LOE rdx rcx rsi rdi r10 r11 eax ebx ebp r12d r14d
..B6.119:                       # Preds ..B6.118
                                # Execution count [2.25e+00]
        imull     %ebx, %r14d                                   #602.7
        movslq    %eax, %rax                                    #602.7
        movslq    %r14d, %r14                                   #602.7
        addq      %rax, %r14                                    #602.7
        movq      32(%r10), %r10                                #602.7
        movl      -4(%r10,%r14,4), %r8d                         #602.7
        imull     %r8d, %r12d                                   #602.7
        movslq    %r12d, %r12                                   #602.7
        addq      %r12, %r11                                    #602.7
        movq      %r11, -8(%rcx,%rax,8)                         #602.7
        jmp       ..B6.130      # Prob 100%                     #602.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B6.120:                       # Preds ..B6.112
                                # Execution count [2.50e+00]
        movups    288(%rsp), %xmm0                              #604.7
        movups    304(%rsp), %xmm1                              #604.7
        movups    320(%rsp), %xmm2                              #604.7
        movups    336(%rsp), %xmm3                              #604.7
        movups    352(%rsp), %xmm4                              #604.7
        movups    368(%rsp), %xmm5                              #604.7
        movq      328(%rsp), %rcx                               #604.7
        movups    %xmm0, 864(%rsp)                              #604.7
        movups    %xmm1, 880(%rsp)                              #604.7
        movups    %xmm2, 896(%rsp)                              #604.7
        movups    %xmm3, 912(%rsp)                              #604.7
        movups    %xmm4, 928(%rsp)                              #604.7
        movups    %xmm5, 944(%rsp)                              #604.7
        cmpl      $0, 372(%rsp)                                 #604.7
        jne       ..B6.126      # Prob 50%                      #604.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B6.121:                       # Preds ..B6.120
                                # Execution count [1.25e+00]
        testl     %ebp, %ebp                                    #604.7
        je        ..B6.124      # Prob 50%                      #604.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B6.122:                       # Preds ..B6.121
                                # Execution count [6.25e-01]
        cmpl      $0, 368(%rsp)                                 #604.7
        je        ..B6.124      # Prob 50%                      #604.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B6.123:                       # Preds ..B6.122
                                # Execution count [3.12e-01]
        movq      blank_args(%rip), %rcx                        #604.7
        movq      %rcx, 1176(%rsp)                              #604.7
        jmp       ..B6.130      # Prob 100%                     #604.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B6.124:                       # Preds ..B6.121 ..B6.122
                                # Execution count [3.12e-01]
        movq      %rcx, 1176(%rsp)                              #604.7
        jmp       ..B6.130      # Prob 100%                     #604.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B6.126:                       # Preds ..B6.120
                                # Execution count [1.25e+00]
        movq      880(%rsp), %r9                                #604.7
        testq     %r9, %r9                                      #604.7
        je        ..B6.128      # Prob 12%                      #604.7
                                # LOE rdx rcx rsi rdi r9 ebx ebp
..B6.127:                       # Preds ..B6.126
                                # Execution count [1.10e+00]
        cmpl      $0, 956(%rsp)                                 #604.7
        jne       ..B6.129      # Prob 50%                      #604.7
                                # LOE rdx rcx rsi rdi r9 ebx ebp
..B6.128:                       # Preds ..B6.127 ..B6.126
                                # Execution count [7.00e-01]
        movslq    %ebx, %rbx                                    #604.7
        movslq    896(%rsp), %r8                                #604.7
        imulq     %rbx, %r8                                     #604.7
        addq      %r8, %rcx                                     #604.7
        movq      %rcx, 1176(%rsp)                              #604.7
        jmp       ..B6.130      # Prob 100%                     #604.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B6.129:                       # Preds ..B6.127
                                # Execution count [5.50e-01]
        movl      %ebx, %r8d                                    #604.7
        imull     24(%r9), %r8d                                 #604.7
        addl      892(%rsp), %r8d                               #604.7
        movslq    %r8d, %r8                                     #604.7
        movq      32(%r9), %r9                                  #604.7
        movl      896(%rsp), %r10d                              #604.7
        imull     (%r9,%r8,4), %r10d                            #604.7
        movslq    %r10d, %r10                                   #604.7
        addq      %r10, %rcx                                    #604.7
        movq      %rcx, 1176(%rsp)                              #604.7
                                # LOE rdx rcx rsi rdi ebx ebp
..B6.130:                       # Preds ..B6.119 ..B6.118 ..B6.113 ..B6.123 ..B6.124
                                #       ..B6.128 ..B6.129
                                # Execution count [5.00e+00]
        cmpl      $-1, 412(%rsp)                                #605.23
        jge       ..B6.138      # Prob 50%                      #605.23
                                # LOE rdx rcx rsi rdi ebx ebp
..B6.131:                       # Preds ..B6.130
                                # Execution count [2.50e+00]
        movl      412(%rsp), %r9d                               #606.7
        movl      %r9d, 1264(%rsp)                              #606.7[spill]
        negl      %r9d                                          #606.7
        movq      400(%rsp), %r11                               #606.7
        movq      424(%rsp), %r12                               #606.7
        movl      416(%rsp), %r13d                              #606.7
        movq      1184(%rsp), %r8                               #606.43
        testl     %r9d, %r9d                                    #606.7
        jle       ..B6.148      # Prob 50%                      #606.7
                                # LOE rdx rcx rsi rdi r8 r11 r12 ebx ebp r9d r13d
..B6.132:                       # Preds ..B6.131
                                # Execution count [2.50e+00]
        movl      %r9d, %r15d                                   #606.7
        movl      $1, %eax                                      #606.7
        shrl      $31, %r15d                                    #606.7
        xorl      %r10d, %r10d                                  #606.7
        subl      1264(%rsp), %r15d                             #606.7[spill]
        sarl      $1, %r15d                                     #606.7
        movl      24(%r11), %r14d                               #606.7
        testl     %r15d, %r15d                                  #606.7
        jbe       ..B6.136      # Prob 10%                      #606.7
                                # LOE rdx rcx rsi rdi r8 r11 r12 eax ebx ebp r9d r10d r13d r14d r15d
..B6.133:                       # Preds ..B6.132
                                # Execution count [2.25e+00]
        movl      %ebx, %eax                                    #606.7
        movq      %rdi, 1248(%rsp)                              #606.7[spill]
        imull     %r14d, %eax                                   #606.7
        movl      %ebx, 1240(%rsp)                              #606.7[spill]
        movl      %ebp, 1232(%rsp)                              #606.7[spill]
        movl      %r15d, %edi                                   #606.7
        .align    16,0x90
                                # LOE rdx rcx rsi r8 r11 r12 eax edi r9d r10d r13d r14d
..B6.134:                       # Preds ..B6.134 ..B6.133
                                # Execution count [6.25e+00]
        movq      32(%r11), %r15                                #606.7
        lea       (%rax,%r10,2), %ebx                           #606.7
        movslq    %ebx, %rbx                                    #606.7
        lea       (%r10,%r10), %ebp                             #606.7
        movslq    %ebp, %rbp                                    #606.7
        incl      %r10d                                         #606.7
        movl      (%r15,%rbx,4), %r15d                          #606.7
        imull     %r13d, %r15d                                  #606.7
        movslq    %r15d, %r15                                   #606.7
        addq      %r12, %r15                                    #606.7
        movq      %r15, (%r8,%rbp,8)                            #606.7
        movq      32(%r11), %r15                                #606.7
        movl      4(%r15,%rbx,4), %ebx                          #606.7
        imull     %r13d, %ebx                                   #606.7
        movslq    %ebx, %rbx                                    #606.7
        addq      %r12, %rbx                                    #606.7
        movq      %rbx, 8(%r8,%rbp,8)                           #606.7
        cmpl      %edi, %r10d                                   #606.7
        jb        ..B6.134      # Prob 64%                      #606.7
                                # LOE rdx rcx rsi r8 r11 r12 eax edi r9d r10d r13d r14d
..B6.135:                       # Preds ..B6.134
                                # Execution count [2.25e+00]
        movq      1248(%rsp), %rdi                              #[spill]
        lea       1(%r10,%r10), %eax                            #606.7
        movl      1240(%rsp), %ebx                              #[spill]
        movl      1232(%rsp), %ebp                              #[spill]
                                # LOE rdx rcx rsi rdi r8 r11 r12 eax ebx ebp r9d r13d r14d
..B6.136:                       # Preds ..B6.135 ..B6.132
                                # Execution count [2.50e+00]
        lea       -1(%rax), %r10d                               #606.7
        cmpl      %r9d, %r10d                                   #606.7
        jae       ..B6.148      # Prob 10%                      #606.7
                                # LOE rdx rcx rsi rdi r8 r11 r12 eax ebx ebp r13d r14d
..B6.137:                       # Preds ..B6.136
                                # Execution count [2.25e+00]
        imull     %ebx, %r14d                                   #606.7
        movslq    %eax, %rax                                    #606.7
        movslq    %r14d, %r14                                   #606.7
        addq      %rax, %r14                                    #606.7
        movq      32(%r11), %r11                                #606.7
        movl      -4(%r11,%r14,4), %r9d                         #606.7
        imull     %r9d, %r13d                                   #606.7
        movslq    %r13d, %r13                                   #606.7
        addq      %r13, %r12                                    #606.7
        movq      %r12, -8(%r8,%rax,8)                          #606.7
        jmp       ..B6.148      # Prob 100%                     #606.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B6.138:                       # Preds ..B6.130
                                # Execution count [2.50e+00]
        movups    384(%rsp), %xmm0                              #608.7
        movups    400(%rsp), %xmm1                              #608.7
        movups    416(%rsp), %xmm2                              #608.7
        movups    432(%rsp), %xmm3                              #608.7
        movups    448(%rsp), %xmm4                              #608.7
        movups    464(%rsp), %xmm5                              #608.7
        movq      424(%rsp), %r8                                #608.7
        movups    %xmm0, 960(%rsp)                              #608.7
        movups    %xmm1, 976(%rsp)                              #608.7
        movups    %xmm2, 992(%rsp)                              #608.7
        movups    %xmm3, 1008(%rsp)                             #608.7
        movups    %xmm4, 1024(%rsp)                             #608.7
        movups    %xmm5, 1040(%rsp)                             #608.7
        cmpl      $0, 468(%rsp)                                 #608.7
        jne       ..B6.144      # Prob 50%                      #608.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B6.139:                       # Preds ..B6.138
                                # Execution count [1.25e+00]
        testl     %ebp, %ebp                                    #608.7
        je        ..B6.142      # Prob 50%                      #608.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B6.140:                       # Preds ..B6.139
                                # Execution count [6.25e-01]
        cmpl      $0, 464(%rsp)                                 #608.7
        je        ..B6.142      # Prob 50%                      #608.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B6.141:                       # Preds ..B6.140
                                # Execution count [3.12e-01]
        movq      blank_args(%rip), %r8                         #608.7
        movq      %r8, 1184(%rsp)                               #608.7
        jmp       ..B6.148      # Prob 100%                     #608.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B6.142:                       # Preds ..B6.139 ..B6.140
                                # Execution count [3.12e-01]
        movq      %r8, 1184(%rsp)                               #608.7
        jmp       ..B6.148      # Prob 100%                     #608.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B6.144:                       # Preds ..B6.138
                                # Execution count [1.25e+00]
        movq      976(%rsp), %r10                               #608.7
        testq     %r10, %r10                                    #608.7
        je        ..B6.146      # Prob 12%                      #608.7
                                # LOE rdx rcx rsi rdi r8 r10 ebx ebp
..B6.145:                       # Preds ..B6.144
                                # Execution count [1.10e+00]
        cmpl      $0, 1052(%rsp)                                #608.7
        jne       ..B6.147      # Prob 50%                      #608.7
                                # LOE rdx rcx rsi rdi r8 r10 ebx ebp
..B6.146:                       # Preds ..B6.145 ..B6.144
                                # Execution count [7.00e-01]
        movslq    %ebx, %rbx                                    #608.7
        movslq    992(%rsp), %r9                                #608.7
        imulq     %rbx, %r9                                     #608.7
        addq      %r9, %r8                                      #608.7
        movq      %r8, 1184(%rsp)                               #608.7
        jmp       ..B6.148      # Prob 100%                     #608.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B6.147:                       # Preds ..B6.145
                                # Execution count [5.50e-01]
        movl      %ebx, %r9d                                    #608.7
        imull     24(%r10), %r9d                                #608.7
        addl      988(%rsp), %r9d                               #608.7
        movslq    %r9d, %r9                                     #608.7
        movq      32(%r10), %r10                                #608.7
        movl      992(%rsp), %r11d                              #608.7
        imull     (%r10,%r9,4), %r11d                           #608.7
        movslq    %r11d, %r11                                   #608.7
        addq      %r11, %r8                                     #608.7
        movq      %r8, 1184(%rsp)                               #608.7
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B6.148:                       # Preds ..B6.137 ..B6.136 ..B6.131 ..B6.141 ..B6.142
                                #       ..B6.146 ..B6.147
                                # Execution count [5.00e+00]
        cmpl      $-1, 508(%rsp)                                #609.23
        jge       ..B6.156      # Prob 50%                      #609.23
                                # LOE rdx rcx rsi rdi r8 ebx ebp
..B6.149:                       # Preds ..B6.148
                                # Execution count [2.50e+00]
        movl      508(%rsp), %r10d                              #610.7
        movl      %r10d, 1256(%rsp)                             #610.7[spill]
        negl      %r10d                                         #610.7
        movq      496(%rsp), %r11                               #610.7
        movq      520(%rsp), %r12                               #610.7
        movl      512(%rsp), %r13d                              #610.7
        movq      1192(%rsp), %r9                               #610.43
        movl      %r10d, 1272(%rsp)                             #610.7[spill]
        testl     %r10d, %r10d                                  #610.7
        jle       ..B6.166      # Prob 50%                      #610.7
                                # LOE rdx rcx rsi rdi r8 r9 r11 r12 ebx ebp r10d r13d
..B6.150:                       # Preds ..B6.149
                                # Execution count [2.50e+00]
        movl      %r10d, %r15d                                  #610.7
        movl      $1, %eax                                      #610.7
        shrl      $31, %r15d                                    #610.7
        xorl      %r10d, %r10d                                  #610.7
        subl      1256(%rsp), %r15d                             #610.7[spill]
        sarl      $1, %r15d                                     #610.7
        movl      24(%r11), %r14d                               #610.7
        testl     %r15d, %r15d                                  #610.7
        jbe       ..B6.154      # Prob 10%                      #610.7
                                # LOE rdx rcx rsi rdi r8 r9 r11 r12 eax ebx ebp r10d r13d r14d r15d
..B6.151:                       # Preds ..B6.150
                                # Execution count [2.25e+00]
        movl      %ebx, %eax                                    #610.7
        movq      %rdi, 1248(%rsp)                              #610.7[spill]
        imull     %r14d, %eax                                   #610.7
        movl      %ebx, 1240(%rsp)                              #610.7[spill]
        movl      %ebp, 1232(%rsp)                              #610.7[spill]
        movl      %r15d, %edi                                   #610.7
        .align    16,0x90
                                # LOE rdx rcx rsi r8 r9 r11 r12 eax edi r10d r13d r14d
..B6.152:                       # Preds ..B6.152 ..B6.151
                                # Execution count [6.25e+00]
        movq      32(%r11), %r15                                #610.7
        lea       (%rax,%r10,2), %ebp                           #610.7
        movslq    %ebp, %rbp                                    #610.7
        lea       (%r10,%r10), %ebx                             #610.7
        movslq    %ebx, %rbx                                    #610.7
        incl      %r10d                                         #610.7
        movl      (%r15,%rbp,4), %r15d                          #610.7
        imull     %r13d, %r15d                                  #610.7
        movslq    %r15d, %r15                                   #610.7
        addq      %r12, %r15                                    #610.7
        movq      %r15, (%r9,%rbx,8)                            #610.7
        movq      32(%r11), %r15                                #610.7
        movl      4(%r15,%rbp,4), %ebp                          #610.7
        imull     %r13d, %ebp                                   #610.7
        movslq    %ebp, %rbp                                    #610.7
        addq      %r12, %rbp                                    #610.7
        movq      %rbp, 8(%r9,%rbx,8)                           #610.7
        cmpl      %edi, %r10d                                   #610.7
        jb        ..B6.152      # Prob 64%                      #610.7
                                # LOE rdx rcx rsi r8 r9 r11 r12 eax edi r10d r13d r14d
..B6.153:                       # Preds ..B6.152
                                # Execution count [2.25e+00]
        movq      1248(%rsp), %rdi                              #[spill]
        lea       1(%r10,%r10), %eax                            #610.7
        movl      1240(%rsp), %ebx                              #[spill]
        movl      1232(%rsp), %ebp                              #[spill]
                                # LOE rdx rcx rsi rdi r8 r9 r11 r12 eax ebx ebp r13d r14d
..B6.154:                       # Preds ..B6.153 ..B6.150
                                # Execution count [2.50e+00]
        lea       -1(%rax), %r10d                               #610.7
        cmpl      1272(%rsp), %r10d                             #610.7[spill]
        jae       ..B6.166      # Prob 10%                      #610.7
                                # LOE rdx rcx rsi rdi r8 r9 r11 r12 eax ebx ebp r13d r14d
..B6.155:                       # Preds ..B6.154
                                # Execution count [2.25e+00]
        imull     %ebx, %r14d                                   #610.7
        movslq    %eax, %rax                                    #610.7
        movslq    %r14d, %r14                                   #610.7
        addq      %rax, %r14                                    #610.7
        movq      32(%r11), %r11                                #610.7
        movl      -4(%r11,%r14,4), %r10d                        #610.7
        imull     %r10d, %r13d                                  #610.7
        movslq    %r13d, %r13                                   #610.7
        addq      %r13, %r12                                    #610.7
        movq      %r12, -8(%r9,%rax,8)                          #610.7
        jmp       ..B6.166      # Prob 100%                     #610.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B6.156:                       # Preds ..B6.148
                                # Execution count [2.50e+00]
        movups    480(%rsp), %xmm0                              #612.7
        movups    496(%rsp), %xmm1                              #612.7
        movups    512(%rsp), %xmm2                              #612.7
        movups    528(%rsp), %xmm3                              #612.7
        movups    544(%rsp), %xmm4                              #612.7
        movups    560(%rsp), %xmm5                              #612.7
        movq      520(%rsp), %r9                                #612.7
        movups    %xmm0, 1056(%rsp)                             #612.7
        movups    %xmm1, 1072(%rsp)                             #612.7
        movups    %xmm2, 1088(%rsp)                             #612.7
        movups    %xmm3, 1104(%rsp)                             #612.7
        movups    %xmm4, 1120(%rsp)                             #612.7
        movups    %xmm5, 1136(%rsp)                             #612.7
        cmpl      $0, 564(%rsp)                                 #612.7
        jne       ..B6.162      # Prob 50%                      #612.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B6.157:                       # Preds ..B6.156
                                # Execution count [1.25e+00]
        testl     %ebp, %ebp                                    #612.7
        je        ..B6.160      # Prob 50%                      #612.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B6.158:                       # Preds ..B6.157
                                # Execution count [6.25e-01]
        cmpl      $0, 560(%rsp)                                 #612.7
        je        ..B6.160      # Prob 50%                      #612.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B6.159:                       # Preds ..B6.158
                                # Execution count [3.12e-01]
        movq      blank_args(%rip), %r9                         #612.7
        movq      %r9, 1192(%rsp)                               #612.7
        jmp       ..B6.166      # Prob 100%                     #612.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B6.160:                       # Preds ..B6.157 ..B6.158
                                # Execution count [3.12e-01]
        movq      %r9, 1192(%rsp)                               #612.7
        jmp       ..B6.166      # Prob 100%                     #612.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B6.162:                       # Preds ..B6.156
                                # Execution count [1.25e+00]
        movq      1072(%rsp), %r11                              #612.7
        testq     %r11, %r11                                    #612.7
        je        ..B6.164      # Prob 12%                      #612.7
                                # LOE rdx rcx rsi rdi r8 r9 r11 ebx ebp
..B6.163:                       # Preds ..B6.162
                                # Execution count [1.10e+00]
        cmpl      $0, 1148(%rsp)                                #612.7
        jne       ..B6.165      # Prob 50%                      #612.7
                                # LOE rdx rcx rsi rdi r8 r9 r11 ebx ebp
..B6.164:                       # Preds ..B6.163 ..B6.162
                                # Execution count [7.00e-01]
        movslq    %ebx, %rbx                                    #612.7
        movslq    1088(%rsp), %r10                              #612.7
        imulq     %rbx, %r10                                    #612.7
        addq      %r10, %r9                                     #612.7
        movq      %r9, 1192(%rsp)                               #612.7
        jmp       ..B6.166      # Prob 100%                     #612.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B6.165:                       # Preds ..B6.163
                                # Execution count [5.50e-01]
        movl      %ebx, %r10d                                   #612.7
        imull     24(%r11), %r10d                               #612.7
        addl      1084(%rsp), %r10d                             #612.7
        movslq    %r10d, %r10                                   #612.7
        movq      32(%r11), %r11                                #612.7
        movl      1088(%rsp), %r12d                             #612.7
        imull     (%r11,%r10,4), %r12d                          #612.7
        movslq    %r12d, %r12                                   #612.7
        addq      %r12, %r9                                     #612.7
        movq      %r9, 1192(%rsp)                               #612.7
                                # LOE rdx rcx rsi rdi r8 r9 ebx ebp
..B6.166:                       # Preds ..B6.155 ..B6.154 ..B6.149 ..B6.159 ..B6.160
                                #       ..B6.164 ..B6.165
                                # Execution count [5.00e+00]
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.505:
        call      *1288(%rsp)                                   #614.5[spill]
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.506:
                                # LOE ebx ebp
..B6.167:                       # Preds ..B6.166
                                # Execution count [5.00e+00]
        incl      %ebx                                          #584.32
        cmpl      1280(%rsp), %ebx                              #584.23[spill]
        jl        ..B6.56       # Prob 82%                      #584.23
                                # LOE ebx ebp
..B6.168:                       # Preds ..B6.167
                                # Execution count [9.00e-01]
        movl      1280(%rsp), %r9d                              #[spill]
        movq      1296(%rsp), %rbx                              #[spill]
        movq      1208(%rsp), %rbp                              #[spill]
        movq      1200(%rsp), %r12                              #[spill]
	.cfi_restore 12
        cmpl      16(%rbx), %r9d                                #617.18
        je        ..B6.170      # Prob 50%                      #617.18
                                # LOE rbp r12 r13 r14 r9d
..B6.169:                       # Preds ..B6.188 ..B6.168
                                # Execution count [5.00e-01]
        testl     %r9d, %r9d                                    #617.47
        jne       ..B6.171      # Prob 78%                      #617.47
                                # LOE rbp r12 r13 r14
..B6.170:                       # Preds ..B6.169 ..B6.188 ..B6.168
                                # Execution count [6.10e-01]
        movl      $6, %edi                                      #618.5
        lea       (%rsp), %rsi                                  #618.5
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.508:
#       op_mpi_wait_all(int, op_arg *)
        call      op_mpi_wait_all                               #618.5
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.509:
                                # LOE rbp r12 r13 r14
..B6.171:                       # Preds ..B6.170 ..B6.169
                                # Execution count [1.00e+00]
        movl      $6, %edi                                      #621.3
        lea       (%rsp), %rsi                                  #621.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.510:
#       op_mpi_set_dirtybit(int, op_arg *)
        call      op_mpi_set_dirtybit                           #621.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.511:
                                # LOE rbp r12 r13 r14
..B6.172:                       # Preds ..B6.171
                                # Execution count [1.00e+00]
        movq      1192(%rsp), %rsi                              #630.3
        lea       1840(%rsp), %rdi                              #630.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.512:
#       op_mpi_reduce_double(op_arg *, double *)
        call      op_mpi_reduce_double                          #630.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.513:
                                # LOE rbp r12 r13 r14
..B6.173:                       # Preds ..B6.172
                                # Execution count [1.00e+00]
        lea       576(%rsp), %rdi                               #633.3
        lea       584(%rsp), %rsi                               #633.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.514:
#       op_timers_core(double *, double *)
        call      op_timers_core                                #633.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.515:
                                # LOE rbp r12 r13 r14
..B6.174:                       # Preds ..B6.173
                                # Execution count [1.00e+00]
        movsd     584(%rsp), %xmm0                              #638.3
        movq      %rbp, %rdi                                    #638.3
        subsd     1224(%rsp), %xmm0                             #638.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.516:
#       op_mpi_perf_time(const char *, double)
        call      op_mpi_perf_time                              #638.3
..___tag_value__Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_.517:
                                # LOE r12 r13 r14
..B6.175:                       # Preds ..B6.174
                                # Execution count [1.00e+00]
        cmpl      $-1, 1388(%rsp)                               #641.18
        jge       ..B6.177      # Prob 78%                      #641.18
                                # LOE r12 r13 r14
..B6.176:                       # Preds ..B6.175
                                # Execution count [2.20e-01]
        movq      1152(%rsp), %rdi                              #642.5
#       free(void *)
        call      free                                          #642.5
                                # LOE r12 r13 r14
..B6.177:                       # Preds ..B6.176 ..B6.175
                                # Execution count [1.00e+00]
        cmpl      $-1, 1484(%rsp)                               #644.18
        jge       ..B6.179      # Prob 78%                      #644.18
                                # LOE r12 r13 r14
..B6.178:                       # Preds ..B6.177
                                # Execution count [2.20e-01]
        movq      1160(%rsp), %rdi                              #645.5
#       free(void *)
        call      free                                          #645.5
                                # LOE r12 r13 r14
..B6.179:                       # Preds ..B6.178 ..B6.177
                                # Execution count [1.00e+00]
        cmpl      $-1, 1580(%rsp)                               #647.18
        jge       ..B6.181      # Prob 78%                      #647.18
                                # LOE r12 r13 r14
..B6.180:                       # Preds ..B6.179
                                # Execution count [2.20e-01]
        movq      1168(%rsp), %rdi                              #648.5
#       free(void *)
        call      free                                          #648.5
                                # LOE r12 r13 r14
..B6.181:                       # Preds ..B6.180 ..B6.179
                                # Execution count [1.00e+00]
        cmpl      $-1, 1676(%rsp)                               #650.18
        jge       ..B6.183      # Prob 78%                      #650.18
                                # LOE r12 r13 r14
..B6.182:                       # Preds ..B6.181
                                # Execution count [2.20e-01]
        movq      1176(%rsp), %rdi                              #651.5
#       free(void *)
        call      free                                          #651.5
                                # LOE r12 r13 r14
..B6.183:                       # Preds ..B6.182 ..B6.181
                                # Execution count [1.00e+00]
        cmpl      $-1, 1772(%rsp)                               #653.18
        jge       ..B6.185      # Prob 78%                      #653.18
                                # LOE r12 r13 r14
..B6.184:                       # Preds ..B6.183
                                # Execution count [2.20e-01]
        movq      1184(%rsp), %rdi                              #654.5
#       free(void *)
        call      free                                          #654.5
                                # LOE r12 r13 r14
..B6.185:                       # Preds ..B6.184 ..B6.183
                                # Execution count [1.00e+00]
        cmpl      $-1, 1868(%rsp)                               #656.18
        jge       ..B6.187      # Prob 78%                      #656.18
                                # LOE r12 r13 r14
..B6.186:                       # Preds ..B6.185
                                # Execution count [2.20e-01]
        movq      1192(%rsp), %rdi                              #657.5
#       free(void *)
        call      free                                          #657.5
                                # LOE r12 r13 r14
..B6.187:                       # Preds ..B6.186 ..B6.185
                                # Execution count [1.00e+00]
        addq      $1312, %rsp                                   #659.1
	.cfi_def_cfa_offset 48
	.cfi_restore 6
        popq      %rbp                                          #659.1
	.cfi_def_cfa_offset 40
	.cfi_restore 3
        popq      %rbx                                          #659.1
	.cfi_def_cfa_offset 32
	.cfi_restore 15
        popq      %r15                                          #659.1
	.cfi_def_cfa_offset 24
	.cfi_restore 14
        popq      %r14                                          #659.1
	.cfi_def_cfa_offset 16
	.cfi_restore 13
        popq      %r13                                          #659.1
	.cfi_def_cfa_offset 8
        ret                                                     #659.1
	.cfi_def_cfa_offset 1360
	.cfi_offset 3, -40
	.cfi_offset 6, -48
	.cfi_offset 13, -16
	.cfi_offset 14, -24
	.cfi_offset 15, -32
                                # LOE
..B6.188:                       # Preds ..B6.54
                                # Execution count [1.00e-01]: Infreq
        cmpl      16(%rbx), %r9d                                #617.18
        je        ..B6.170      # Prob 50%                      #617.18
        jmp       ..B6.169      # Prob 100%                     #617.18
        .align    16,0x90
                                # LOE rbp r12 r13 r14 r9d
	.cfi_endproc
# mark_end;
	.type	_Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_,@function
	.size	_Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_,.-_Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_
	.data
# -- End  _Z11op_par_loopIKdS0_S0_S0_S0_dEvPFvPT_PT0_PT1_PT2_PT3_PT4_EPKcP11op_set_core6op_argSJ_SJ_SJ_SJ_SJ_
	.section .text._Z8adt_calcPKdS0_S0_S0_S0_Pd, "xaG",@progbits,_Z8adt_calcPKdS0_S0_S0_S0_Pd,comdat
..TXTST6:
# -- Begin  _Z8adt_calcPKdS0_S0_S0_S0_Pd
	.section .text._Z8adt_calcPKdS0_S0_S0_S0_Pd, "xaG",@progbits,_Z8adt_calcPKdS0_S0_S0_S0_Pd,comdat
# mark_begin;
       .align    16,0x90
	.weak _Z8adt_calcPKdS0_S0_S0_S0_Pd
# --- adt_calc(const double *, const double *, const double *, const double *, const double *, double *)
_Z8adt_calcPKdS0_S0_S0_S0_Pd:
# parameter 1: %rdi
# parameter 2: %rsi
# parameter 3: %rdx
# parameter 4: %rcx
# parameter 5: %r8
# parameter 6: %r9
..B7.1:                         # Preds ..B7.0
                                # Execution count [1.00e+00]
	.cfi_startproc
	.cfi_personality 0x3,__gxx_personality_v0
..___tag_value__Z8adt_calcPKdS0_S0_S0_S0_Pd.536:
..L537:
                                                        #2.70
        movsd     .L_2il0floatpacket.66(%rip), %xmm7            #5.3
        divsd     (%r8), %xmm7                                  #5.15
        movsd     8(%r8), %xmm3                                 #6.12
        movsd     16(%r8), %xmm2                                #7.12
        mulsd     %xmm7, %xmm3                                  #6.12
        mulsd     %xmm7, %xmm2                                  #7.12
        movaps    %xmm3, %xmm6                                  #8.49
        movaps    %xmm2, %xmm5                                  #8.57
        mulsd     %xmm3, %xmm6                                  #8.49
        mulsd     %xmm2, %xmm5                                  #8.57
        movsd     24(%r8), %xmm4                                #8.30
        addsd     %xmm5, %xmm6                                  #8.57
        mulsd     %xmm4, %xmm7                                  #8.30
        mulsd     .L_2il0floatpacket.217(%rip), %xmm6           #8.57
        movsd     (%rsi), %xmm10                                #10.8
        subsd     %xmm6, %xmm7                                  #8.57
        subsd     (%rdi), %xmm10                                #10.16
        movsd     8(%rsi), %xmm9                                #11.8
        movaps    %xmm10, %xmm8                                 #12.28
        movsd     gam(%rip), %xmm1                              #8.12
        mulsd     gm1(%rip), %xmm1                              #8.18
        subsd     8(%rdi), %xmm9                                #11.16
        mulsd     %xmm10, %xmm10                                #12.48
        mulsd     %xmm2, %xmm8                                  #12.28
        mulsd     %xmm7, %xmm1                                  #8.57
        movaps    %xmm9, %xmm0                                  #12.19
        mulsd     %xmm9, %xmm9                                  #12.58
        sqrtsd    %xmm1, %xmm1                                  #8.7
        mulsd     %xmm3, %xmm0                                  #12.19
        addsd     %xmm9, %xmm10                                 #12.58
        sqrtsd    %xmm10, %xmm10                                #12.38
        subsd     %xmm8, %xmm0                                  #12.28
        mulsd     %xmm1, %xmm10                                 #12.38
        andps     .L_2il0floatpacket.65(%rip), %xmm0            #12.10
        addsd     %xmm10, %xmm0                                 #12.38
        movsd     %xmm0, (%r9)                                  #12.4
        movsd     (%rdx), %xmm13                                #14.8
        movsd     8(%rdx), %xmm12                               #15.8
        subsd     (%rsi), %xmm13                                #14.16
        subsd     8(%rsi), %xmm12                               #15.16
        movaps    %xmm12, %xmm14                                #16.20
        movaps    %xmm13, %xmm11                                #16.29
        mulsd     %xmm13, %xmm13                                #16.49
        mulsd     %xmm12, %xmm12                                #16.59
        mulsd     %xmm3, %xmm14                                 #16.20
        mulsd     %xmm2, %xmm11                                 #16.29
        addsd     %xmm12, %xmm13                                #16.59
        subsd     %xmm11, %xmm14                                #16.29
        sqrtsd    %xmm13, %xmm13                                #16.39
        mulsd     %xmm1, %xmm13                                 #16.39
        andps     .L_2il0floatpacket.65(%rip), %xmm14           #16.11
        addsd     %xmm13, %xmm14                                #16.39
        addsd     %xmm14, %xmm0                                 #16.4
        movsd     %xmm0, (%r9)                                  #16.4
        movsd     (%rcx), %xmm5                                 #18.8
        movsd     8(%rcx), %xmm4                                #19.8
        subsd     (%rdx), %xmm5                                 #18.16
        subsd     8(%rdx), %xmm4                                #19.16
        movaps    %xmm4, %xmm6                                  #20.20
        movaps    %xmm5, %xmm15                                 #20.29
        mulsd     %xmm5, %xmm5                                  #20.49
        mulsd     %xmm4, %xmm4                                  #20.59
        mulsd     %xmm3, %xmm6                                  #20.20
        mulsd     %xmm2, %xmm15                                 #20.29
        addsd     %xmm4, %xmm5                                  #20.59
        subsd     %xmm15, %xmm6                                 #20.29
        sqrtsd    %xmm5, %xmm5                                  #20.39
        mulsd     %xmm1, %xmm5                                  #20.39
        andps     .L_2il0floatpacket.65(%rip), %xmm6            #20.11
        addsd     %xmm5, %xmm6                                  #20.39
        addsd     %xmm6, %xmm0                                  #20.4
        movsd     %xmm0, (%r9)                                  #20.4
        movsd     (%rdi), %xmm8                                 #22.8
        movsd     8(%rdi), %xmm7                                #23.8
        subsd     (%rcx), %xmm8                                 #22.16
        subsd     8(%rcx), %xmm7                                #23.16
        mulsd     %xmm8, %xmm2                                  #24.29
        mulsd     %xmm7, %xmm3                                  #24.20
        mulsd     %xmm8, %xmm8                                  #24.49
        mulsd     %xmm7, %xmm7                                  #24.59
        subsd     %xmm2, %xmm3                                  #24.29
        addsd     %xmm7, %xmm8                                  #24.59
        sqrtsd    %xmm8, %xmm8                                  #24.39
        mulsd     %xmm8, %xmm1                                  #24.39
        andps     .L_2il0floatpacket.65(%rip), %xmm3            #24.11
        addsd     %xmm1, %xmm3                                  #24.39
        addsd     %xmm3, %xmm0                                  #24.4
        movsd     %xmm0, (%r9)                                  #24.4
        divsd     cfl(%rip), %xmm0                              #27.27
        movsd     %xmm0, (%r9)                                  #27.4
        ret                                                     #28.1
        .align    16,0x90
                                # LOE
	.cfi_endproc
# mark_end;
	.type	_Z8adt_calcPKdS0_S0_S0_S0_Pd,@function
	.size	_Z8adt_calcPKdS0_S0_S0_S0_Pd,.-_Z8adt_calcPKdS0_S0_S0_S0_Pd
	.data
# -- End  _Z8adt_calcPKdS0_S0_S0_S0_Pd
	.section .text._Z9bres_calcPKdS0_S0_S0_PdPKi, "xaG",@progbits,_Z9bres_calcPKdS0_S0_S0_PdPKi,comdat
..TXTST7:
# -- Begin  _Z9bres_calcPKdS0_S0_S0_PdPKi
	.section .text._Z9bres_calcPKdS0_S0_S0_PdPKi, "xaG",@progbits,_Z9bres_calcPKdS0_S0_S0_PdPKi,comdat
# mark_begin;
       .align    16,0x90
	.weak _Z9bres_calcPKdS0_S0_S0_PdPKi
# --- bres_calc(const double *, const double *, const double *, const double *, double *, const int *)
_Z9bres_calcPKdS0_S0_S0_PdPKi:
# parameter 1: %rdi
# parameter 2: %rsi
# parameter 3: %rdx
# parameter 4: %rcx
# parameter 5: %r8
# parameter 6: %r9
..B8.1:                         # Preds ..B8.0
                                # Execution count [1.00e+00]
	.cfi_startproc
	.cfi_personality 0x3,__gxx_personality_v0
..___tag_value__Z9bres_calcPKdS0_S0_S0_PdPKi.539:
..L540:
                                                        #2.75
        movsd     (%rdx), %xmm7                                 #8.15
        movsd     .L_2il0floatpacket.66(%rip), %xmm6            #8.15
        divsd     %xmm7, %xmm6                                  #8.15
        movsd     8(%rdx), %xmm10                               #9.44
        movsd     16(%rdx), %xmm2                               #9.60
        movaps    %xmm10, %xmm0                                 #9.44
        movaps    %xmm2, %xmm3                                  #9.60
        mulsd     %xmm10, %xmm0                                 #9.44
        mulsd     %xmm2, %xmm3                                  #9.60
        movsd     .L_2il0floatpacket.217(%rip), %xmm8           #9.23
        addsd     %xmm3, %xmm0                                  #9.60
        movaps    %xmm8, %xmm1                                  #9.30
        mulsd     %xmm6, %xmm1                                  #9.30
        mulsd     %xmm0, %xmm1                                  #9.60
        movsd     24(%rdx), %xmm3                               #9.15
        movsd     (%rdi), %xmm9                                 #5.8
        subsd     %xmm1, %xmm3                                  #9.60
        subsd     (%rsi), %xmm9                                 #5.16
        movsd     8(%rdi), %xmm5                                #6.8
        movsd     gm1(%rip), %xmm4                              #9.8
        mulsd     %xmm4, %xmm3                                  #9.60
        subsd     8(%rsi), %xmm5                                #6.16
        cmpl      $1, (%r9)                                     #11.17
        je        ..B8.4        # Prob 16%                      #11.17
                                # LOE rdx rcx rbx rbp r8 r12 r13 r14 r15 xmm2 xmm3 xmm4 xmm5 xmm6 xmm7 xmm8 xmm9 xmm10
..B8.2:                         # Preds ..B8.1
                                # Execution count [8.40e-01]
        mulsd     %xmm5, %xmm10                                 #15.26
        movaps    %xmm8, %xmm12                                 #18.34
        mulsd     %xmm9, %xmm2                                  #15.39
        movsd     qinf(%rip), %xmm1                             #17.17
        subsd     %xmm2, %xmm10                                 #15.39
        movsd     .L_2il0floatpacket.66(%rip), %xmm2            #17.17
        divsd     %xmm1, %xmm2                                  #17.17
        movsd     8+qinf(%rip), %xmm15                          #18.50
        movsd     16+qinf(%rip), %xmm14                         #18.70
        movaps    %xmm15, %xmm11                                #18.50
        mulsd     %xmm10, %xmm6                                 #15.39
        movaps    %xmm14, %xmm10                                #18.70
        mulsd     %xmm15, %xmm11                                #18.50
        mulsd     %xmm14, %xmm10                                #18.70
        mulsd     %xmm5, %xmm15                                 #19.28
        mulsd     %xmm9, %xmm14                                 #19.43
        addsd     %xmm10, %xmm11                                #18.70
        mulsd     %xmm2, %xmm12                                 #18.34
        subsd     %xmm14, %xmm15                                #19.43
        mulsd     %xmm11, %xmm12                                #18.70
        mulsd     %xmm15, %xmm2                                 #19.43
        movaps    %xmm7, %xmm10                                 #23.64
        mulsd     %xmm6, %xmm7                                  #23.24
        subsd     %xmm1, %xmm10                                 #23.64
        mulsd     %xmm2, %xmm1                                  #23.39
        movsd     (%rcx), %xmm0                                 #21.12
        addsd     %xmm1, %xmm7                                  #23.39
        mulsd     eps(%rip), %xmm0                              #21.20
        mulsd     %xmm8, %xmm7                                  #23.39
        mulsd     %xmm0, %xmm10                                 #23.64
        movsd     24+qinf(%rip), %xmm13                         #18.17
        addsd     %xmm7, %xmm10                                 #23.64
        subsd     %xmm12, %xmm13                                #18.70
        addsd     (%r8), %xmm10                                 #24.5
        mulsd     %xmm13, %xmm4                                 #18.70
        movsd     %xmm10, (%r8)                                 #24.5
        movaps    %xmm3, %xmm10                                 #25.64
        movsd     8(%rdx), %xmm1                                #25.24
        addsd     %xmm4, %xmm10                                 #25.64
        movsd     8+qinf(%rip), %xmm7                           #25.49
        movaps    %xmm1, %xmm11                                 #26.23
        mulsd     %xmm6, %xmm1                                  #25.24
        subsd     %xmm7, %xmm11                                 #26.23
        mulsd     %xmm2, %xmm7                                  #25.49
        mulsd     %xmm5, %xmm10                                 #25.64
        mulsd     %xmm0, %xmm11                                 #26.23
        addsd     %xmm7, %xmm1                                  #25.64
        movaps    %xmm3, %xmm7                                  #28.49
        addsd     %xmm1, %xmm10                                 #25.64
        mulsd     %xmm8, %xmm10                                 #25.64
        xorps     .L_2il0floatpacket.218(%rip), %xmm7           #28.49
        addsd     %xmm10, %xmm11                                #26.23
        subsd     %xmm4, %xmm7                                  #28.49
        addsd     8(%r8), %xmm11                                #27.5
        mulsd     %xmm9, %xmm7                                  #28.49
        movsd     %xmm11, 8(%r8)                                #27.5
        movsd     16(%rdx), %xmm1                               #28.24
        movsd     16+qinf(%rip), %xmm5                          #28.49
        movaps    %xmm1, %xmm10                                 #29.23
        mulsd     %xmm6, %xmm1                                  #28.24
        subsd     %xmm5, %xmm10                                 #29.23
        mulsd     %xmm2, %xmm5                                  #28.49
        mulsd     %xmm0, %xmm10                                 #29.23
        addsd     %xmm5, %xmm1                                  #28.49
        addsd     %xmm1, %xmm7                                  #28.49
        mulsd     %xmm8, %xmm7                                  #28.64
        addsd     %xmm7, %xmm10                                 #29.23
        addsd     16(%r8), %xmm10                               #30.5
        movsd     %xmm10, 16(%r8)                               #30.5
        movsd     24(%rdx), %xmm1                               #31.25
        movsd     24+qinf(%rip), %xmm5                          #31.47
        movaps    %xmm1, %xmm9                                  #32.23
        addsd     %xmm1, %xmm3                                  #31.33
        addsd     %xmm5, %xmm4                                  #31.57
        subsd     %xmm5, %xmm9                                  #32.23
        mulsd     %xmm3, %xmm6                                  #31.33
        mulsd     %xmm4, %xmm2                                  #31.57
        mulsd     %xmm9, %xmm0                                  #32.23
        addsd     %xmm2, %xmm6                                  #31.57
        mulsd     %xmm6, %xmm8                                  #31.57
        addsd     %xmm8, %xmm0                                  #32.23
        addsd     24(%r8), %xmm0                                #33.5
        movsd     %xmm0, 24(%r8)                                #33.5
                                # LOE rbx rbp r12 r13 r14 r15
..B8.3:                         # Preds ..B8.2
                                # Execution count [1.00e+00]
        ret                                                     #35.1
                                # LOE
..B8.4:                         # Preds ..B8.1
                                # Execution count [1.60e-01]: Infreq
        mulsd     %xmm3, %xmm5                                  #12.22
        mulsd     %xmm9, %xmm3                                  #13.22
        addsd     8(%r8), %xmm5                                 #12.5
        xorps     .L_2il0floatpacket.218(%rip), %xmm3           #13.22
        movsd     %xmm5, 8(%r8)                                 #12.5
        addsd     16(%r8), %xmm3                                #13.5
        movsd     %xmm3, 16(%r8)                                #13.5
        ret                                                     #13.5
        .align    16,0x90
                                # LOE rbx rbp r12 r13 r14 r15
	.cfi_endproc
# mark_end;
	.type	_Z9bres_calcPKdS0_S0_S0_PdPKi,@function
	.size	_Z9bres_calcPKdS0_S0_S0_PdPKi,.-_Z9bres_calcPKdS0_S0_S0_PdPKi
	.data
# -- End  _Z9bres_calcPKdS0_S0_S0_PdPKi
	.section .text._Z8res_calcPKdS0_S0_S0_S0_S0_PdS1_, "xaG",@progbits,_Z8res_calcPKdS0_S0_S0_S0_S0_PdS1_,comdat
..TXTST8:
# -- Begin  _Z8res_calcPKdS0_S0_S0_S0_S0_PdS1_
	.section .text._Z8res_calcPKdS0_S0_S0_S0_S0_PdS1_, "xaG",@progbits,_Z8res_calcPKdS0_S0_S0_S0_S0_PdS1_,comdat
# mark_begin;
       .align    16,0x90
	.weak _Z8res_calcPKdS0_S0_S0_S0_S0_PdS1_
# --- res_calc(const double *, const double *, const double *, const double *, const double *, const double *, double *, double *)
_Z8res_calcPKdS0_S0_S0_S0_S0_PdS1_:
# parameter 1: %rdi
# parameter 2: %rsi
# parameter 3: %rdx
# parameter 4: %rcx
# parameter 5: %r8
# parameter 6: %r9
# parameter 7: 8 + %rsp
# parameter 8: 16 + %rsp
..B9.1:                         # Preds ..B9.0
                                # Execution count [1.00e+00]
	.cfi_startproc
	.cfi_personality 0x3,__gxx_personality_v0
..___tag_value__Z8res_calcPKdS0_S0_S0_S0_S0_PdS1_.542:
..L543:
                                                        #3.50
        movsd     (%rdx), %xmm8                                 #9.15
        movsd     .L_2il0floatpacket.66(%rip), %xmm9            #9.15
        divsd     %xmm8, %xmm9                                  #9.15
        movsd     .L_2il0floatpacket.66(%rip), %xmm2            #13.15
        movsd     (%rcx), %xmm1                                 #13.15
        divsd     %xmm1, %xmm2                                  #13.15
        movsd     8(%rdx), %xmm14                               #10.44
        movsd     16(%rdx), %xmm13                              #10.60
        movaps    %xmm14, %xmm11                                #10.44
        movaps    %xmm13, %xmm10                                #10.60
        mulsd     %xmm14, %xmm11                                #10.44
        mulsd     %xmm13, %xmm10                                #10.60
        movsd     .L_2il0floatpacket.217(%rip), %xmm6           #10.23
        addsd     %xmm10, %xmm11                                #10.60
        movsd     (%rdi), %xmm7                                 #6.8
        movaps    %xmm6, %xmm12                                 #10.30
        movsd     8(%rdi), %xmm5                                #7.8
        movaps    %xmm6, %xmm10                                 #14.30
        mulsd     %xmm9, %xmm12                                 #10.30
        subsd     (%rsi), %xmm7                                 #6.16
        subsd     8(%rsi), %xmm5                                #7.16
        mulsd     %xmm2, %xmm10                                 #14.30
        mulsd     %xmm11, %xmm12                                #10.60
        mulsd     %xmm5, %xmm14                                 #11.24
        mulsd     %xmm7, %xmm13                                 #11.37
        movsd     24(%rdx), %xmm3                               #10.15
        subsd     %xmm13, %xmm14                                #11.37
        subsd     %xmm12, %xmm3                                 #10.60
        mulsd     %xmm14, %xmm9                                 #11.37
        movsd     8(%rcx), %xmm13                               #14.44
        movaps    %xmm1, %xmm14                                 #19.37
        movsd     16(%rcx), %xmm12                              #14.60
        movaps    %xmm13, %xmm0                                 #14.44
        movaps    %xmm12, %xmm15                                #14.60
        mulsd     %xmm13, %xmm0                                 #14.44
        mulsd     %xmm12, %xmm15                                #14.60
        mulsd     %xmm5, %xmm13                                 #15.24
        mulsd     %xmm7, %xmm12                                 #15.37
        addsd     %xmm15, %xmm0                                 #14.60
        subsd     %xmm12, %xmm13                                #15.37
        mulsd     %xmm0, %xmm10                                 #14.60
        mulsd     %xmm13, %xmm2                                 #15.37
        movsd     (%r8), %xmm0                                  #17.18
        movsd     24(%rcx), %xmm11                              #14.15
        mulsd     %xmm2, %xmm14                                 #19.37
        addsd     (%r9), %xmm0                                  #17.28
        subsd     %xmm10, %xmm11                                #14.60
        mulsd     %xmm6, %xmm0                                  #17.28
        movaps    %xmm8, %xmm10                                 #19.22
        subsd     %xmm1, %xmm8                                  #19.60
        mulsd     %xmm9, %xmm10                                 #19.22
        mulsd     eps(%rip), %xmm0                              #17.37
        addsd     %xmm14, %xmm10                                #19.37
        mulsd     %xmm0, %xmm8                                  #19.60
        mulsd     %xmm6, %xmm10                                 #19.37
        movq      8(%rsp), %rax                                 #3.50
        addsd     %xmm8, %xmm10                                 #19.60
        movsd     (%rax), %xmm1                                 #20.3
        movsd     gm1(%rip), %xmm4                              #10.8
        addsd     %xmm10, %xmm1                                 #20.3
        mulsd     %xmm4, %xmm3                                  #10.60
        mulsd     %xmm11, %xmm4                                 #14.60
        movq      16(%rsp), %r10                                #3.50
        movaps    %xmm3, %xmm11                                 #22.60
        movsd     %xmm1, (%rax)                                 #20.3
        movaps    %xmm3, %xmm14                                 #26.47
        xorps     .L_2il0floatpacket.218(%rip), %xmm14          #26.47
        addsd     %xmm4, %xmm11                                 #22.60
        subsd     %xmm4, %xmm14                                 #26.47
        mulsd     %xmm5, %xmm11                                 #22.60
        mulsd     %xmm7, %xmm14                                 #26.47
        movsd     (%r10), %xmm8                                 #21.3
        subsd     %xmm10, %xmm8                                 #21.3
        movsd     %xmm8, (%r10)                                 #21.3
        movsd     8(%rdx), %xmm10                               #23.13
        movsd     8(%rcx), %xmm15                               #23.21
        movaps    %xmm10, %xmm1                                 #22.22
        movaps    %xmm15, %xmm5                                 #22.47
        subsd     %xmm15, %xmm10                                #23.21
        mulsd     %xmm9, %xmm1                                  #22.22
        mulsd     %xmm2, %xmm5                                  #22.47
        mulsd     %xmm0, %xmm10                                 #23.21
        addsd     %xmm5, %xmm1                                  #22.60
        movsd     8(%rax), %xmm8                                #24.3
        addsd     %xmm1, %xmm11                                 #22.60
        mulsd     %xmm6, %xmm11                                 #22.60
        addsd     %xmm10, %xmm11                                #23.21
        addsd     %xmm11, %xmm8                                 #24.3
        movsd     %xmm8, 8(%rax)                                #24.3
        movsd     8(%r10), %xmm12                               #25.3
        subsd     %xmm11, %xmm12                                #25.3
        movsd     %xmm12, 8(%r10)                               #25.3
        movsd     16(%rdx), %xmm13                              #27.13
        movsd     16(%rcx), %xmm5                               #27.21
        movaps    %xmm13, %xmm1                                 #26.22
        movaps    %xmm5, %xmm7                                  #26.47
        subsd     %xmm5, %xmm13                                 #27.21
        mulsd     %xmm9, %xmm1                                  #26.22
        mulsd     %xmm2, %xmm7                                  #26.47
        mulsd     %xmm0, %xmm13                                 #27.21
        addsd     %xmm7, %xmm1                                  #26.47
        movsd     16(%rax), %xmm8                               #28.3
        addsd     %xmm1, %xmm14                                 #26.47
        mulsd     %xmm6, %xmm14                                 #26.60
        addsd     %xmm13, %xmm14                                #27.21
        addsd     %xmm14, %xmm8                                 #28.3
        movsd     %xmm8, 16(%rax)                               #28.3
        movsd     16(%r10), %xmm10                              #29.3
        subsd     %xmm14, %xmm10                                #29.3
        movsd     %xmm10, 16(%r10)                              #29.3
        movsd     24(%rdx), %xmm11                              #30.66
        movsd     24(%rcx), %xmm7                               #30.74
        addsd     %xmm11, %xmm3                                 #30.31
        addsd     %xmm7, %xmm4                                  #30.53
        subsd     %xmm7, %xmm11                                 #30.74
        mulsd     %xmm3, %xmm9                                  #30.31
        mulsd     %xmm4, %xmm2                                  #30.53
        mulsd     %xmm11, %xmm0                                 #30.74
        addsd     %xmm2, %xmm9                                  #30.53
        mulsd     %xmm9, %xmm6                                  #30.53
        movsd     24(%rax), %xmm9                               #31.3
        addsd     %xmm0, %xmm6                                  #30.74
        addsd     %xmm6, %xmm9                                  #31.3
        movsd     %xmm9, 24(%rax)                               #31.3
        movsd     24(%r10), %xmm2                               #32.3
        subsd     %xmm6, %xmm2                                  #32.3
        movsd     %xmm2, 24(%r10)                               #32.3
        ret                                                     #33.1
        .align    16,0x90
                                # LOE
	.cfi_endproc
# mark_end;
	.type	_Z8res_calcPKdS0_S0_S0_S0_S0_PdS1_,@function
	.size	_Z8res_calcPKdS0_S0_S0_S0_S0_PdS1_,.-_Z8res_calcPKdS0_S0_S0_S0_S0_PdS1_
	.data
# -- End  _Z8res_calcPKdS0_S0_S0_S0_S0_PdS1_
	.text
# -- Begin  __sti__$E
	.text
# mark_begin;
       .align    16,0x90
# --- __sti__$E()
__sti__$E:
..B10.1:                        # Preds ..B10.0
                                # Execution count [1.00e+00]
	.cfi_startproc
	.cfi_personality 0x3,__gxx_personality_v0
..___tag_value___sti__$E.545:
..L546:
                                                        #
        pushq     %rsi                                          #
	.cfi_def_cfa_offset 16
        movslq    blank_args_size(%rip), %rdi                   #13.35
..___tag_value___sti__$E.548:
#       op_malloc(size_t)
        call      op_malloc                                     #13.35
..___tag_value___sti__$E.549:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B10.2:                        # Preds ..B10.1
                                # Execution count [1.00e+00]
        movq      %rax, blank_args(%rip)                        #13.35
        popq      %rcx                                          #13.35
	.cfi_def_cfa_offset 8
        ret                                                     #13.35
        .align    16,0x90
                                # LOE
	.cfi_endproc
# mark_end;
	.type	__sti__$E,@function
	.size	__sti__$E,.-__sti__$E
	.data
# -- End  __sti__$E
	.bss
	.align 8
	.align 8
blank_args:
	.type	blank_args,@object
	.size	blank_args,8
	.space 8	# pad
	.align 8
	.globl gam
gam:
	.type	gam,@object
	.size	gam,8
	.space 8	# pad
	.align 8
	.globl gm1
gm1:
	.type	gm1,@object
	.size	gm1,8
	.space 8	# pad
	.align 8
	.globl cfl
cfl:
	.type	cfl,@object
	.size	cfl,8
	.space 8	# pad
	.align 8
	.globl eps
eps:
	.type	eps,@object
	.size	eps,8
	.space 8	# pad
	.align 8
	.globl mach
mach:
	.type	mach,@object
	.size	mach,8
	.space 8	# pad
	.align 8
	.globl alpha
alpha:
	.type	alpha,@object
	.size	alpha,8
	.space 8	# pad
	.align 8
	.globl qinf
qinf:
	.type	qinf,@object
	.size	qinf,32
	.space 32	# pad
	.data
	.align 4
	.align 4
blank_args_size:
	.long	512
	.type	blank_args_size,@object
	.size	blank_args_size,4
	.section .rodata, "a"
	.align 16
	.align 16
.L_2il0floatpacket.65:
	.long	0xffffffff,0x7fffffff,0x00000000,0x00000000
	.type	.L_2il0floatpacket.65,@object
	.size	.L_2il0floatpacket.65,16
	.align 16
.L_2il0floatpacket.218:
	.long	0x00000000,0x80000000,0x00000000,0x00000000
	.type	.L_2il0floatpacket.218,@object
	.size	.L_2il0floatpacket.218,16
	.align 8
.L_2il0floatpacket.62:
	.long	0x00000000,0x40590000
	.type	.L_2il0floatpacket.62,@object
	.size	.L_2il0floatpacket.62,8
	.align 8
.L_2il0floatpacket.63:
	.long	0x14512840,0x3f1bca4f
	.type	.L_2il0floatpacket.63,@object
	.size	.L_2il0floatpacket.63,8
	.align 8
.L_2il0floatpacket.64:
	.long	0x88e368f1,0x3ee4f8b5
	.type	.L_2il0floatpacket.64,@object
	.size	.L_2il0floatpacket.64,8
	.align 8
.L_2il0floatpacket.66:
	.long	0x00000000,0x3ff00000
	.type	.L_2il0floatpacket.66,@object
	.size	.L_2il0floatpacket.66,8
	.align 8
.L_2il0floatpacket.217:
	.long	0x00000000,0x3fe00000
	.type	.L_2il0floatpacket.217,@object
	.size	.L_2il0floatpacket.217,8
	.section .rodata.str1.4, "aMS",@progbits,1
	.space 3, 0x00 	# pad
	.align 4
.L_2__STRING.6:
	.long	1953066601
	.long	1768710505
	.long	1735289203
	.long	1869375008
	.long	1768300663
	.long	543452261
	.word	10
	.type	.L_2__STRING.6,@object
	.size	.L_2__STRING.6,26
	.space 2, 0x00 	# pad
	.align 4
.L_2__STRING.7:
	.long	1701080942
	.word	115
	.type	.L_2__STRING.7,@object
	.size	.L_2__STRING.7,6
	.space 2, 0x00 	# pad
	.align 4
.L_2__STRING.8:
	.long	1701274725
	.word	115
	.type	.L_2__STRING.8,@object
	.size	.L_2__STRING.8,6
	.space 2, 0x00 	# pad
	.align 4
.L_2__STRING.9:
	.long	1734632802
	.word	29541
	.byte	0
	.type	.L_2__STRING.9,@object
	.size	.L_2__STRING.9,7
	.space 1, 0x00 	# pad
	.align 4
.L_2__STRING.10:
	.long	1819043171
	.word	115
	.type	.L_2__STRING.10,@object
	.size	.L_2__STRING.10,6
	.space 2, 0x00 	# pad
	.align 4
.L_2__STRING.11:
	.long	1734632816
	.word	101
	.type	.L_2__STRING.11,@object
	.size	.L_2__STRING.11,6
	.space 2, 0x00 	# pad
	.align 4
.L_2__STRING.12:
	.long	1701012848
	.word	27756
	.byte	0
	.type	.L_2__STRING.12,@object
	.size	.L_2__STRING.12,7
	.space 1, 0x00 	# pad
	.align 4
.L_2__STRING.13:
	.long	1684365936
	.word	25959
	.byte	0
	.type	.L_2__STRING.13,@object
	.size	.L_2__STRING.13,7
	.space 1, 0x00 	# pad
	.align 4
.L_2__STRING.14:
	.long	1667588720
	.long	7105637
	.type	.L_2__STRING.14,@object
	.size	.L_2__STRING.14,8
	.align 4
.L_2__STRING.15:
	.long	1818583920
	.word	108
	.type	.L_2__STRING.15,@object
	.size	.L_2__STRING.15,6
	.space 2, 0x00 	# pad
	.align 4
.L_2__STRING.16:
	.long	1702125421
	.word	29811
	.byte	0
	.type	.L_2__STRING.16,@object
	.size	.L_2__STRING.16,7
	.space 1, 0x00 	# pad
	.align 4
.L_2__STRING.18:
	.long	7630441
	.type	.L_2__STRING.18,@object
	.size	.L_2__STRING.18,4
	.align 4
.L_2__STRING.19:
	.long	1868717936
	.long	6581877
	.type	.L_2__STRING.19,@object
	.size	.L_2__STRING.19,8
	.align 4
.L_2__STRING.1:
	.long	1651863396
	.word	25964
	.byte	0
	.type	.L_2__STRING.1,@object
	.size	.L_2__STRING.1,7
	.space 1, 0x00 	# pad
	.align 4
.L_2__STRING.20:
	.long	7888752
	.type	.L_2__STRING.20,@object
	.size	.L_2__STRING.20,4
	.align 4
.L_2__STRING.21:
	.long	7430000
	.type	.L_2__STRING.21,@object
	.size	.L_2__STRING.21,4
	.align 4
.L_2__STRING.22:
	.long	1869700976
	.word	25708
	.byte	0
	.type	.L_2__STRING.22,@object
	.size	.L_2__STRING.22,7
	.space 1, 0x00 	# pad
	.align 4
.L_2__STRING.23:
	.long	1684103024
	.word	116
	.type	.L_2__STRING.23,@object
	.size	.L_2__STRING.23,6
	.space 2, 0x00 	# pad
	.align 4
.L_2__STRING.24:
	.long	1701994352
	.word	115
	.type	.L_2__STRING.24,@object
	.size	.L_2__STRING.24,6
	.space 2, 0x00 	# pad
	.align 4
.L_2__STRING.25:
	.long	1702125424
	.word	29811
	.byte	0
	.type	.L_2__STRING.25,@object
	.size	.L_2__STRING.25,7
	.space 1, 0x00 	# pad
	.align 4
.L_2__STRING.27:
	.long	7168359
	.type	.L_2__STRING.27,@object
	.size	.L_2__STRING.27,4
	.align 4
.L_2__STRING.28:
	.long	1601660270
	.long	1684632167
	.long	3500078
	.type	.L_2__STRING.28,@object
	.size	.L_2__STRING.28,12
	.align 4
.L_2__STRING.29:
	.long	3239271
	.type	.L_2__STRING.29,@object
	.size	.L_2__STRING.29,4
	.align 4
.L_2__STRING.30:
	.long	7104099
	.type	.L_2__STRING.30,@object
	.size	.L_2__STRING.30,4
	.align 4
.L_2__STRING.31:
	.long	7565413
	.type	.L_2__STRING.31,@object
	.size	.L_2__STRING.31,4
	.align 4
.L_2__STRING.32:
	.long	1751343469
	.byte	0
	.type	.L_2__STRING.32,@object
	.size	.L_2__STRING.32,5
	.space 3, 0x00 	# pad
	.align 4
.L_2__STRING.33:
	.long	1752198241
	.word	97
	.type	.L_2__STRING.33,@object
	.size	.L_2__STRING.33,6
	.space 2, 0x00 	# pad
	.align 4
.L_2__STRING.34:
	.long	1718511985
	.byte	0
	.type	.L_2__STRING.34,@object
	.size	.L_2__STRING.34,5
	.space 3, 0x00 	# pad
	.align 4
.L_2__STRING.35:
	.long	1601660270
	.long	1684632167
	.long	1953853279
	.long	3500078
	.type	.L_2__STRING.35,@object
	.size	.L_2__STRING.35,16
	.align 4
.L_2__STRING.36:
	.long	1297236304
	.long	1397314629
	.byte	0
	.type	.L_2__STRING.36,@object
	.size	.L_2__STRING.36,9
	.space 3, 0x00 	# pad
	.align 4
.L_2__STRING.37:
	.long	1497454411
	.byte	0
	.type	.L_2__STRING.37,@object
	.size	.L_2__STRING.37,5
	.space 3, 0x00 	# pad
	.align 4
.L_2__STRING.38:
	.long	1702257011
	.long	1819243359
	.word	110
	.type	.L_2__STRING.38,@object
	.size	.L_2__STRING.38,10
	.space 2, 0x00 	# pad
	.align 4
.L_2__STRING.3:
	.long	1919249184
	.long	543974766
	.long	1953853298
	.long	543518313
	.long	544157559
	.long	1768189545
	.long	1952671090
	.long	980316009
	.long	1931812896
	.word	10
	.type	.L_2__STRING.3,@object
	.size	.L_2__STRING.3,38
	.space 2, 0x00 	# pad
	.align 4
.L_2__STRING.4:
	.long	1919249184
	.long	543974766
	.long	1953853298
	.long	543518313
	.long	1752459639
	.long	1684957472
	.long	1667592809
	.long	1852795252
	.long	1931812922
	.word	10
	.type	.L_2__STRING.4,@object
	.size	.L_2__STRING.4,38
	.space 2, 0x00 	# pad
	.align 4
.L_2__STRING.39:
	.long	1601463393
	.long	1668047203
	.byte	0
	.type	.L_2__STRING.39,@object
	.size	.L_2__STRING.39,9
	.space 3, 0x00 	# pad
	.align 4
.L_2__STRING.40:
	.long	1601398130
	.long	1668047203
	.byte	0
	.type	.L_2__STRING.40,@object
	.size	.L_2__STRING.40,9
	.space 3, 0x00 	# pad
	.align 4
.L_2__STRING.41:
	.long	1936028258
	.long	1818321759
	.word	99
	.type	.L_2__STRING.41,@object
	.size	.L_2__STRING.41,10
	.space 2, 0x00 	# pad
	.align 4
.L_2__STRING.42:
	.long	1633972341
	.word	25972
	.byte	0
	.type	.L_2__STRING.42,@object
	.size	.L_2__STRING.42,7
	.space 1, 0x00 	# pad
	.align 4
.L_2__STRING.43:
	.long	543434016
	.long	808527136
	.long	543503662
	.word	10
	.type	.L_2__STRING.43,@object
	.size	.L_2__STRING.43,14
	.space 2, 0x00 	# pad
	.align 4
.L_2__STRING.47:
	.long	1701603686
	.long	1835101791
	.long	896020069
	.byte	0
	.type	.L_2__STRING.47,@object
	.size	.L_2__STRING.47,13
	.space 3, 0x00 	# pad
	.align 4
.L_2__STRING.48:
	.long	544760141
	.long	1635020660
	.long	1970413676
	.long	1835627630
	.long	540876901
	.long	681509
	.type	.L_2__STRING.48,@object
	.size	.L_2__STRING.48,24
	.align 4
.L_2__STRING.45:
	.long	1936287828
	.long	1936028704
	.long	1936269428
	.long	1852793632
	.long	1701079411
	.long	543450482
	.long	1397965136
	.long	672837
	.type	.L_2__STRING.45,@object
	.size	.L_2__STRING.45,32
	.align 4
.L_2__STRING.46:
	.long	1936287828
	.long	1936028704
	.long	1936269428
	.long	1852793632
	.long	1701079411
	.long	543450482
	.long	1279869254
	.long	672837
	.type	.L_2__STRING.46,@object
	.size	.L_2__STRING.46,32
	.section .rodata.str1.32, "aMS",@progbits,1
	.align 32
	.align 32
.L_2__STRING.44:
	.long	1700006410
	.long	1881175155
	.long	1818390386
	.long	1998613861
	.long	543716457
	.long	1663067173
	.long	1936485477
	.long	544434464
	.long	1752459639
	.long	622882409
	.long	892415539
	.long	623190085
	.long	543584032
	.long	543516788
	.long	1701869669
	.long	1684370531
	.long	1819243296
	.long	1869182069
	.word	2670
	.byte	0
	.type	.L_2__STRING.44,@object
	.size	.L_2__STRING.44,75
	.section .ctors, "wa"
	.align 8
__init_0:
	.type	__init_0,@object
	.size	__init_0,8
	.quad	__sti__$E
	.data
# mark_proc_addr_taken __sti__$E;
	.section .note.GNU-stack, ""
// -- Begin DWARF2 SEGMENT .eh_frame
	.section .eh_frame,"a",@progbits
.eh_frame_seg:
	.align 8
# End
