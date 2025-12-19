    .data
    .align 2
rsqrt_table:
    .word 65536, 46341, 32768, 23170
    .word 16384, 11585, 8192, 5793
    .word 4096, 2896, 2048, 1448
    .word 1024, 724, 512, 362
    .word 256, 181, 128, 90
    .word 64, 45, 32, 23
    .word 16, 11, 8, 6
    .word 4, 3, 2, 1
      
    
.text    
.globl rsqrt_fast
.align 2




rsqrt_fast:

    
    #handle special cases
    beqz    a0, .Lx_zero          # if x == 0
    li      t0, -1
    beq     a0, t0, .Lx_max       # if x == UINT32_MAX
    
    mv      t6, a0          # init x
    mv      t0, a0          # work x
    li      t1, 0           # r = 0 

    # c = (x < 0x00010000) << 4;
    li      t2, 0x00010000
    sltu    t3, t0, t2      # t3 = (x < 0x00010000)
    slli    t3, t3, 4       # c = t3 << 4
    add     t1, t1, t3      # r += c
    sll     t0, t0, t3      # x <<= c

    # c = (x < 0x01000000) << 3;
    li      t2, 0x01000000
    sltu    t3, t0, t2
    slli    t3, t3, 3
    add     t1, t1, t3
    sll     t0, t0, t3

    # c = (x < 0x10000000) << 2;
    li      t2, 0x10000000
    sltu    t3, t0, t2
    slli    t3, t3, 2
    add     t1, t1, t3
    sll     t0, t0, t3

    # c = (x < 0x40000000) << 1;
    li      t2, 0x40000000
    sltu    t3, t0, t2
    slli    t3, t3, 1
    add     t1, t1, t3
    sll     t0, t0, t3

    # c = x < 0x80000000;
    li      t2, 0x80000000
    sltu    t3, t0, t2      # t3 = (x < 0x80000000) ? 1 : 0
    add     t1, t1, t3      # r += c
    sll     t0, t0, t3      # x <<= c

    # r += x == 0;
    seqz    t2, t0          # t2 = (x == 0) ? 1 : 0
    add     t1, t1, t2      # r += (x == 0)

    # t1 = clz(original x)

    # ----------------------------------------------------
    # exp = 31u - clz(x)
    # ----------------------------------------------------
    li      t2, 31
    sub     t0, t2, t1      # t0 = exp = 31 - clz(x)
    
    la      t3, rsqrt_table    # t3 = &rsqrt_table[0]

    # y_base
    slli    t2, t0, 2
    add     t4, t3, t2
    lw      t1, 0(t4)          # t1 = y_base

    # y_next
    li      t2, 31
    bgeu    t0, t2, .Lexp_is_31_or_more

    addi    t4, t0, 1
    slli    t4, t4, 2
    add     t4, t3, t4
    lw      t2, 0(t4)          # t2 = y_next
    j       .Ldone_y_next

    .Lexp_is_31_or_more:
        li  t2, 1

    .Ldone_y_next:

    
    mv t4,a0
    li t5,1
    sll t5,t5,t0
    
    sub t5,t4,t5
    
    li t4,16
    blt t0,t4,exp_less_16
    
    addi t4,t0,-16
    srl t5,t5,t4
    
    j fraction_done
    
    exp_less_16:
    sub t4,t4,t0
    sll t5,t5,t4
    
    
    fraction_done:
        
        
    addi    sp, sp, -16
    sw      s1, 12(sp)
    sw      s2, 8(sp)
        
    mv t3,t5 #t3 = fraction
    
    sub t5, t1,t2
    
        # product = diff * fraction
    li   t4, 0         # t4 = product = 0
    
    

    .Lmul_loop:
    beqz t3, .Lmul_done       # while (fraction != 0)

    andi t6, t3, 1            # if (fraction & 1)
    beqz t6, .Lno_add
    add  t4, t4, t5           #     product += diff
    .Lno_add:
    slli t5, t5, 1            # diff <<= 1
    srli t3, t3, 1            # fraction >>= 1
    bnez t3, .Lmul_loop

    .Lmul_done:
    # t4 = diff * original_fraction

    srli t4, t4, 16           # t4 = (diff * fraction) >> 16

    sub  s1, t1, t4           # s1 = y = y_base - t4
    
 
    mv s2,a0                  # s2 = x
    
    ########################
    #  y2 = (uint64_t) y * y
    ########################
    
    mv      t1, s1      # t1 = a = y
    mv      t2, s1      # t2 = b = y
    li      t0, 0       # t0 = product = 0

    .Lmul_yy_loop:
    beqz    t2, .Lmul_yy_done     # while (b != 0)

    andi    t3, t2, 1             # if (b & 1)
    beqz    t3, .Lmul_yy_skip_add
    add     t0, t0, t1            #   product += a
    .Lmul_yy_skip_add:
    slli    t1, t1, 1             # a <<= 1
    srli    t2, t2, 1             # b >>= 1
    bnez    t2, .Lmul_yy_loop

    .Lmul_yy_done:
    # t0 = y * y

    
    ########################
    #    t = (uint64_t) x * y2_q16
    #    tmp = (t >> 16)
    ########################
    mv      t1, t0          # b = y2_q16
    mv      t0, s2          # a = x
    
 li      t3, 0          # prod_lo = 0
    li      t4, 0          # prod_hi = 0

    mv      t5, t0         # a_lo = a
    li      t2, 0          # a_hi = 0
    mv      t6, t1         # b_work = b

.mul32_loop:
    beqz    t6, .mul32_done

    andi    t1, t6, 1      # LSB = b_work & 1
    beqz    t1, .mul32_skip_add

    # 64-bit addition: prod += (a_hi:a_lo)
    add     t1, t3, t5     # tmp_lo = prod_lo + a_lo
    sltu    t0, t1, t3     # carry = (tmp_lo < prod_lo)
    mv      t3, t1         # prod_lo = tmp_lo
    add     t4, t4, t2     # prod_hi += a_hi
    add     t4, t4, t0     # prod_hi += carry

.mul32_skip_add:
    # 64-bit left shift: (a_hi:a_lo) <<= 1
    srli    t0, t5, 31     # carry = a_lo >> 31
    slli    t5, t5, 1      # a_lo <<= 1
    slli    t2, t2, 1      # a_hi <<= 1
    or      t2, t2, t0     # a_hi |= carry

    # multiplier >>= 1
    srli    t6, t6, 1
    j       .mul32_loop

.mul32_done:
    # hi = t4
    # lo = t3
    

    slli t4,t4,16
    srli t3,t3,16
    or t0,t3,t4
    
    
    #######################
    # 3 term = (3<<16) - xy2
    ########################
    li      t1, 3
    slli t1,t1,16
    sub     t1, t1, t0      # t1 = term (Q16)
    

    
    
    ########################
    # 4 y_new = ( (uint64_t) y * term ) >> 17
    ########################
    mv      t0, s1              # a = y
    # t1 = term = b

    # 初始化 64-bit 乘法器
    li      t3, 0               # prod_lo = 0
    li      t4, 0               # prod_hi = 0
    li      t2, 0               # a_hi = 0
    mv      t5, t0              # a_lo = a = y
    mv      t6, t1              # b_work = term

.Lmul_yterm_loop:
    beqz    t6, .Lmul_yterm_done   # while (b_work != 0)

    andi    t0, t6, 1              # if (b_work & 1)
    beqz    t0, .Lmul_yterm_skip_add

    # prod += a (64-bit)
    add     t0, t3, t5             # tmp_lo = prod_lo + a_lo
    sltu    t1, t0, t3             # carry = (tmp_lo < old_lo)
    mv      t3, t0                 # prod_lo = tmp_lo
    add     t4, t4, t2             # prod_hi += a_hi
    add     t4, t4, t1             # prod_hi += carry

.Lmul_yterm_skip_add:
    # a <<= 1 (64-bit)
    srli    t0, t5, 31             # carry = a_lo >> 31
    slli    t5, t5, 1              # a_lo <<= 1
    slli    t2, t2, 1              # a_hi <<= 1
    or      t2, t2, t0             # a_hi |= carry

    # b >>= 1
    srli    t6, t6, 1
    bnez    t6, .Lmul_yterm_loop

.Lmul_yterm_done:
    # 現在 prod = (t4:t3) = (uint64_t) y * term

    # y_new = prod >> 17
    # result = (hi << (32-17)) | (lo >> 17)
    #        = (t4 << 15)      | (t3 >> 17)
    slli    t0, t4, 15           # hi << 15
    srli    t1, t3, 17           # lo >> 17
    or      t0, t0, t1           # t0 = y_new

    mv      s1, t0               # s1 = y_new (Q16)
    mv      a0, s1               # 回傳值
    
    lw      s2, 8(sp)
    lw      s1, 12(sp)
    addi    sp, sp, 16
    
      
    ret

    
    


.Lx_zero:
    li      a0, -1
    ret

.Lx_max:
    li      a0, 1
    ret

